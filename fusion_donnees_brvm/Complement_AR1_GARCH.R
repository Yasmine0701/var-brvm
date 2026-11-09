# ===============================================================================
# COMPLÉMENT : MODÈLE AR(1)-GARCH(1,1)-t (correction guidée par les diagnostics)
# ===============================================================================
# Justification : les tests Ljung-Box sur les résidus du GARCH(1,1) à moyenne
# constante détectent une autocorrélation (Q(10) p = 0.0059). Les rendements
# BRVM présentent une dépendance sérielle (marché peu liquide, échanges non
# synchrones). On ajoute donc un terme AR(1) dans l'équation de moyenne,
# puis on re-vérifie les diagnostics. C'est la démarche économétrique standard.
#
# À LANCER APRÈS Analyse_Complete_VaR_BRVM.R (dans la même session R)
# Sinon : décommente les 8 premières lignes.
# ===============================================================================

# setwd("C:/Users/HP/Desktop/MEMOIRE_M2_YASMINE/MEMOIREYAS/VaR_BRVM_Shiny")
# library(rugarch); library(moments); library(tseries)
# brvm <- read.csv("data/brvm_data.csv"); brvm$Date <- as.Date(brvm$Date)
# returns <- brvm$Return * 100; n <- length(returns)
# n_in <- floor(0.8 * n); r_in <- returns[1:n_in]; r_out <- returns[(n_in+1):n]
# r_in_dec <- r_in / 100
# arch_lm_test <- function(x, lag = 10) { nn <- length(x); x2 <- x^2
#   y <- x2[(lag+1):nn]; X <- sapply(1:lag, function(i) x2[(lag+1-i):(nn-i)])
#   f <- lm(y ~ X); s <- (nn-lag)*summary(f)$r.squared
#   list(statistic = s, p.value = 1 - pchisq(s, lag)) }

cat("\n===== COMPLÉMENT : AR(1)-GARCH(1,1)-t =====\n")

# -------------------------------------------------------------------------------
# 1. Vérification préalable : autocorrélation des rendements bruts
# -------------------------------------------------------------------------------
lb_ret <- Box.test(r_in, lag = 10, type = "Ljung-Box")
cat("Ljung-Box rendements bruts Q(10) =", round(lb_ret$statistic, 2),
    "| p =", round(lb_ret$p.value, 4),
    ifelse(lb_ret$p.value < 0.05, "-> autocorrelation confirmee", ""), "\n\n")

# -------------------------------------------------------------------------------
# 2. Estimation AR(1)-GARCH(1,1) avec distribution Student-t
#    (meilleur modele selon AIC + correction de l'autocorrelation)
# -------------------------------------------------------------------------------
spec_ar1 <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(1, 0), include.mean = TRUE),
  distribution.model = "std")

fit_ar1 <- ugarchfit(spec_ar1, r_in_dec, solver = "hybrid")

ll <- likelihood(fit_ar1)
k <- 6  # mu, ar1, omega, alpha1, beta1, shape
aic_ar1 <- round(-2 * ll + 2 * k, 2)
bic_ar1 <- round(-2 * ll + k * log(length(r_in_dec)), 2)

cf <- fit_ar1@fit$matcoef
cat("Parametres AR(1)-GARCH(1,1)-t :\n")
print(round(cf, 6))
cat("\nAIC =", aic_ar1, "| BIC =", bic_ar1, "\n")
cat("(a comparer au GARCH(1,1)-t sans AR : AIC = -1524.82)\n\n")

write.csv(data.frame(Parametre = rownames(cf), round(cf, 6)),
          "resultats/T13_parametres_AR1_GARCH_t.csv", row.names = FALSE)

# -------------------------------------------------------------------------------
# 3. RE-DIAGNOSTICS des résidus (le test décisif)
# -------------------------------------------------------------------------------
z2 <- as.numeric(residuals(fit_ar1, standardize = TRUE))

lb5   <- Box.test(z2,   lag = 5,  type = "Ljung-Box")
lb10  <- Box.test(z2,   lag = 10, type = "Ljung-Box")
lb20  <- Box.test(z2,   lag = 20, type = "Ljung-Box")
lb10s <- Box.test(z2^2, lag = 10, type = "Ljung-Box")
alm   <- arch_lm_test(z2, 10)

diag2 <- data.frame(
  Test = c("Ljung-Box Q(5)", "Ljung-Box Q(10)", "Ljung-Box Q(20)",
           "Ljung-Box Q2(10)", "ARCH-LM(10)"),
  Statistique = round(c(lb5$statistic, lb10$statistic, lb20$statistic,
                        lb10s$statistic, alm$statistic), 3),
  p_value = round(c(lb5$p.value, lb10$p.value, lb20$p.value,
                    lb10s$p.value, alm$p.value), 4)
)
diag2$Conclusion <- ifelse(diag2$p_value > 0.05,
                           "OK (pas de structure residuelle)",
                           "Structure residuelle detectee")
cat("----- DIAGNOSTICS RESIDUS AR(1)-GARCH(1,1)-t -----\n")
print(diag2)
write.csv(diag2, "resultats/T14_diagnostics_AR1_GARCH_t.csv", row.names = FALSE)

# Q-Q plot des nouveaux résidus
png("resultats/FIG8_qqplot_AR1_GARCH_t.png", width = 800, height = 600)
qqnorm(z2, main = "Q-Q Plot - Residus AR(1)-GARCH(1,1)-t", pch = 20, col = "steelblue")
qqline(z2, col = "red", lwd = 2); grid()
dev.off()
cat("-> FIG8_qqplot_AR1_GARCH_t.png exporte\n\n")

# -------------------------------------------------------------------------------
# 4. VaR 95% du modèle final + backtesting
# -------------------------------------------------------------------------------
fc <- ugarchforecast(fit_ar1, n.ahead = 1)
mu_fc    <- as.numeric(fitted(fc)) * 100
sigma_fc <- as.numeric(sigma(fc)) * 100
shape    <- coef(fit_ar1)["shape"]

# Quantile Student-t standardisé (variance unitaire)
q_t <- qt(0.05, df = shape) * sqrt((shape - 2) / shape)
var_ar1 <- -(mu_fc + q_t * sigma_fc)
cat("VaR 95% AR(1)-GARCH(1,1)-t (1 jour) =", round(var_ar1, 4), "%\n")

# Backtesting sur out-of-sample (VaR fixe pour comparabilite)
viol <- sum(r_out < -var_ar1)
taux <- viol / length(r_out)
p0 <- 0.05
if (viol == 0) {
  lr <- -2 * length(r_out) * log(1 - p0)
} else {
  lr <- -2 * (log((1-p0)^(length(r_out)-viol) * p0^viol) -
              log((1-taux)^(length(r_out)-viol) * taux^viol))
}
p_kupiec <- 1 - pchisq(lr, 1)
cat("Backtesting : ", viol, "violations /", length(r_out),
    "(", round(taux*100, 2), "%) | Kupiec p =", round(p_kupiec, 4), "\n")

var_final <- data.frame(
  Modele = "AR(1)-GARCH(1,1)-t",
  VaR_95 = round(var_ar1, 4),
  Violations = viol,
  Taux_pct = round(taux*100, 2),
  p_Kupiec = round(p_kupiec, 4)
)
write.csv(var_final, "resultats/T15_VaR_modele_final.csv", row.names = FALSE)

cat("\n===== SYNTHESE COMPLEMENT =====\n")
cat("1. Autocorrelation rendements bruts : Q(10) p =", round(lb_ret$p.value, 4), "\n")
cat("2. AR(1)-GARCH(1,1)-t : AIC =", aic_ar1, "(vs -1524.82 sans AR)\n")
cat("3. Diagnostics apres correction : voir T14\n")
cat("4. VaR 95% modele final =", round(var_ar1, 3), "% | Kupiec p =", round(p_kupiec, 4), "\n")
cat("Fichiers : T13-T15 (CSV) + FIG8 (PNG) dans resultats/\n")
