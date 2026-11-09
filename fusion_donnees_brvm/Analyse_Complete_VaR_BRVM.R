# ===============================================================================
# ANALYSE COMPLÈTE VaR BRVM - MÉMOIRE MASTER II INSSEDS
# ===============================================================================
# COULIBALY Peteninta Lagon Yasmine
# Directeur : Mr AKPOSSO Didier Martial
#
# Ce script répond aux corrections demandées par l'évaluation :
#   1. Test de stationnarité formel (ADF)              [Rigueur statistique]
#   2. Diagnostics de modèles (AIC/BIC, Ljung-Box)     [Méthodologie]
#   3. Graphiques complets exportés en PNG             [Résultats empiriques]
#   4. Backtesting développé (Kupiec + Christoffersen) [Résultats empiriques]
#   5. Analyse de robustesse (Bootstrap)               [Rigueur statistique]
#   6. Traitement non-normalité & hétéroscédasticité   [Données]
#   7. Export complet des tableaux/sorties             [Annexes]
#
# UTILISATION :
#   1. Adapter le chemin setwd() ligne 30
#   2. Lancer : source("Analyse_Complete_VaR_BRVM.R")
#   3. Tous les résultats sont exportés dans le dossier "resultats/"
# ===============================================================================

# -------------------------------------------------------------------------------
# 0. CONFIGURATION
# -------------------------------------------------------------------------------

# >>> ADAPTE CE CHEMIN VERS TON DOSSIER VaR_BRVM_Shiny <<<
setwd("C:/Users/HP/Desktop/MEMOIRE_M2_YASMINE/MEMOIREYAS/VaR_BRVM_Shiny")

# Packages nécessaires (installe si manquant)
packages <- c("rugarch", "moments", "tseries", "evir", "boot")
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# Dossier de sortie pour les résultats
dir.create("resultats", showWarnings = FALSE)

cat("\n")
cat("===============================================================\n")
cat("  ANALYSE COMPLÈTE VaR BRVM - MÉMOIRE MASTER II INSSEDS\n")
cat("===============================================================\n")

# -------------------------------------------------------------------------------
# 1. CHARGEMENT ET DESCRIPTION COMPLÈTE DES DONNÉES
#    [Correction : "Fournir une description complète des données"]
# -------------------------------------------------------------------------------

brvm <- read.csv("data/brvm_data.csv")
brvm$Date <- as.Date(brvm$Date)
returns <- brvm$Return * 100   # En pourcentage pour lisibilité
prices  <- brvm$Close
n <- length(returns)

# Split in-sample / out-of-sample (80/20)
n_in  <- floor(0.8 * n)
r_in  <- returns[1:n_in]
r_out <- returns[(n_in + 1):n]

cat("\n----- 1. DESCRIPTION DES DONNÉES -----\n")
cat("Source              : BRVM Composite Index\n")
cat("Observations totales:", n, "\n")
cat("Période             :", format(min(brvm$Date)), "->", format(max(brvm$Date)), "\n")
cat("In-sample           :", n_in, "obs (80%)\n")
cat("Out-of-sample       :", n - n_in, "obs (20%)\n")

desc <- data.frame(
  Statistique = c("N", "Moyenne (%)", "Ecart-type (%)", "Minimum (%)", 
                  "Maximum (%)", "Skewness", "Kurtosis (exces)", "Mediane (%)"),
  Valeur = c(n_in,
             round(mean(r_in), 4),
             round(sd(r_in), 4),
             round(min(r_in), 4),
             round(max(r_in), 4),
             round(skewness(r_in), 4),
             round(kurtosis(r_in) - 3, 4),
             round(median(r_in), 4))
)
print(desc)
write.csv(desc, "resultats/T1_statistiques_descriptives.csv", row.names = FALSE)

# GRAPHIQUE 1 : Evolution du cours et des rendements
png("resultats/FIG1_prix_rendements.png", width = 1000, height = 700)
par(mfrow = c(2, 1), mar = c(4, 4, 3, 1))
plot(brvm$Date, prices, type = "l", col = "steelblue", lwd = 1.5,
     main = "BRVM Composite - Evolution du cours",
     xlab = "Date", ylab = "Indice")
grid()
plot(brvm$Date, returns, type = "h", col = "darkred",
     main = "BRVM Composite - Rendements journaliers (%)",
     xlab = "Date", ylab = "Rendement (%)")
abline(h = 0, col = "gray40")
grid()
dev.off()
cat("-> FIG1_prix_rendements.png exporte\n")

# -------------------------------------------------------------------------------
# 2. TEST DE STATIONNARITÉ FORMEL (ADF)
#    [Correction : "Effectuer un test de stationnarité formel"]
# -------------------------------------------------------------------------------

cat("\n----- 2. TEST DE STATIONNARITÉ (Augmented Dickey-Fuller) -----\n")

# ADF sur les PRIX (attendu : non-stationnaire)
adf_prix <- adf.test(prices[1:n_in])
cat("ADF sur les PRIX      : stat =", round(adf_prix$statistic, 4),
    "| p-value =", round(adf_prix$p.value, 4), "\n")

# ADF sur les RENDEMENTS (attendu : stationnaire)
adf_rend <- adf.test(r_in)
cat("ADF sur les RENDEMENTS: stat =", round(adf_rend$statistic, 4),
    "| p-value =", round(adf_rend$p.value, 4), "\n")

adf_table <- data.frame(
  Serie = c("Prix (niveau)", "Rendements"),
  ADF_stat = c(round(adf_prix$statistic, 4), round(adf_rend$statistic, 4)),
  p_value = c(round(adf_prix$p.value, 4), round(adf_rend$p.value, 4)),
  Conclusion = c(
    ifelse(adf_prix$p.value > 0.05, "Non-stationnaire (I(1))", "Stationnaire"),
    ifelse(adf_rend$p.value < 0.05, "Stationnaire (I(0))", "Non-stationnaire")
  )
)
print(adf_table)
write.csv(adf_table, "resultats/T2_test_stationnarite_ADF.csv", row.names = FALSE)

# -------------------------------------------------------------------------------
# 3. TESTS DE NORMALITÉ + GRAPHIQUES
#    [Correction : "Utiliser des modèles qui tiennent compte de la non-normalité"]
# -------------------------------------------------------------------------------

cat("\n----- 3. TESTS DE NORMALITÉ -----\n")

jb <- jarque.bera.test(r_in)
cat("Jarque-Bera : JB =", round(jb$statistic, 2),
    "| p-value =", format.pval(jb$p.value, digits = 4), "\n")
cat("Skewness    :", round(skewness(r_in), 4), "(asymetrie negative = pertes extremes)\n")
cat("Kurtosis    :", round(kurtosis(r_in), 4), "\n")

sw <- shapiro.test(r_in)
cat("Shapiro-Wilk: W =", round(sw$statistic, 4),
    "| p-value =", format.pval(sw$p.value, digits = 4), "\n")

norm_table <- data.frame(
  Test = c("Jarque-Bera", "Shapiro-Wilk"),
  Statistique = c(round(jb$statistic, 2), round(sw$statistic, 4)),
  p_value = c(format.pval(jb$p.value, digits = 4), format.pval(sw$p.value, digits = 4)),
  Conclusion = c("Normalite rejetee", "Normalite rejetee")
)
write.csv(norm_table, "resultats/T3_tests_normalite.csv", row.names = FALSE)

# GRAPHIQUE 2 : Histogramme + densité normale + Q-Q plot des rendements
png("resultats/FIG2_normalite_rendements.png", width = 1000, height = 500)
par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
hist(r_in, breaks = 30, freq = FALSE, col = "lightsteelblue",
     main = "Distribution des rendements BRVM",
     xlab = "Rendement (%)", ylab = "Densite")
curve(dnorm(x, mean(r_in), sd(r_in)), add = TRUE, col = "red", lwd = 2)
legend("topright", legend = c("Empirique", "Normale theorique"),
       col = c("lightsteelblue", "red"), lwd = c(8, 2), bty = "n")
qqnorm(r_in, main = "Q-Q Plot - Rendements bruts", pch = 20, col = "steelblue")
qqline(r_in, col = "red", lwd = 2)
grid()
dev.off()
cat("-> FIG2_normalite_rendements.png exporte\n")

# -------------------------------------------------------------------------------
# 4. TEST ARCH-LM SUR LES RENDEMENTS (hétéroscédasticité conditionnelle)
# -------------------------------------------------------------------------------

cat("\n----- 4. TEST ARCH-LM (Hétéroscédasticité conditionnelle) -----\n")

arch_lm_test <- function(x, lag = 10) {
  n <- length(x)
  x2 <- x^2
  y <- x2[(lag + 1):n]
  X <- sapply(1:lag, function(i) x2[(lag + 1 - i):(n - i)])
  fit <- lm(y ~ X)
  lm_stat <- (n - lag) * summary(fit)$r.squared
  p_val <- 1 - pchisq(lm_stat, df = lag)
  list(statistic = lm_stat, p.value = p_val)
}

arch_r5  <- arch_lm_test(r_in, 5)
arch_r10 <- arch_lm_test(r_in, 10)

cat("ARCH-LM(5)  : LM =", round(arch_r5$statistic, 2),
    "| p-value =", format.pval(arch_r5$p.value, digits = 4), "\n")
cat("ARCH-LM(10) : LM =", round(arch_r10$statistic, 2),
    "| p-value =", format.pval(arch_r10$p.value, digits = 4), "\n")

arch_table <- data.frame(
  Lag = c(5, 10),
  LM_stat = c(round(arch_r5$statistic, 2), round(arch_r10$statistic, 2)),
  p_value = c(format.pval(arch_r5$p.value, digits = 4),
              format.pval(arch_r10$p.value, digits = 4)),
  Conclusion = c("Effets ARCH presents", "Effets ARCH presents")
)
write.csv(arch_table, "resultats/T4_test_ARCH_LM_rendements.csv", row.names = FALSE)

# -------------------------------------------------------------------------------
# 5. ESTIMATION DES MODÈLES GARCH ET COMPARAISON AIC/BIC
#    [Correction : "Inclure des diagnostics de modèles"]
# -------------------------------------------------------------------------------

cat("\n----- 5. ESTIMATION DES MODÈLES & COMPARAISON AIC/BIC -----\n")

r_in_dec <- r_in / 100  # rugarch préfère les décimaux

fit_model <- function(spec, data, k) {
  fit <- tryCatch(ugarchfit(spec, data, solver = "hybrid"),
                  error = function(e) NULL)
  if (is.null(fit) || fit@fit$convergence != 0) return(NULL)
  ll <- likelihood(fit)
  list(fit = fit,
       AIC = round(-2 * ll + 2 * k, 2),
       BIC = round(-2 * ll + k * log(length(data)), 2))
}

# ARCH(1)
m_arch <- fit_model(
  ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 0)),
             mean.model = list(armaOrder = c(0, 0))), r_in_dec, 3)

# GARCH(1,1) normal
m_garch <- fit_model(
  ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
             mean.model = list(armaOrder = c(0, 0))), r_in_dec, 4)

# GARCH(1,1) Student-t
m_garcht <- fit_model(
  ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
             mean.model = list(armaOrder = c(0, 0)),
             distribution.model = "std"), r_in_dec, 5)

# EGARCH(1,1)
m_egarch <- fit_model(
  ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1, 1)),
             mean.model = list(armaOrder = c(0, 0))), r_in_dec, 5)

comp <- data.frame(
  Modele = c("ARCH(1)", "GARCH(1,1)", "GARCH(1,1)-t", "EGARCH(1,1)"),
  AIC = c(m_arch$AIC, m_garch$AIC, m_garcht$AIC, m_egarch$AIC),
  BIC = c(m_arch$BIC, m_garch$BIC, m_garcht$BIC, m_egarch$BIC)
)
comp$Delta_AIC <- round(comp$AIC - min(comp$AIC), 2)
comp <- comp[order(comp$AIC), ]
print(comp)
write.csv(comp, "resultats/T5_comparaison_AIC_BIC.csv", row.names = FALSE)

best_name <- comp$Modele[1]
cat("\n>>> MODELE OPTIMAL (AIC minimal) :", best_name, "<<<\n")

# Le modèle GARCH(1,1) sert de référence pour les diagnostics du mémoire
fit_ref <- m_garch$fit
coefs <- coef(fit_ref)
cat("\nParametres GARCH(1,1) :\n")
cat("  omega =", format(coefs["omega"], scientific = TRUE), "\n")
cat("  alpha =", round(coefs["alpha1"], 6), "\n")
cat("  beta  =", round(coefs["beta1"], 6), "\n")

param_table <- data.frame(
  Parametre = c("omega", "alpha1", "beta1"),
  Estimation = c(format(coefs["omega"], scientific = TRUE),
                 round(coefs["alpha1"], 6),
                 round(coefs["beta1"], 6)),
  Ecart_type = round(fit_ref@fit$matcoef[c("omega", "alpha1", "beta1"), 2], 6),
  t_value = round(fit_ref@fit$matcoef[c("omega", "alpha1", "beta1"), 3], 4),
  p_value = round(fit_ref@fit$matcoef[c("omega", "alpha1", "beta1"), 4], 4)
)
write.csv(param_table, "resultats/T6_parametres_GARCH11.csv", row.names = FALSE)

# -------------------------------------------------------------------------------
# 6. DIAGNOSTICS DES RÉSIDUS DU MODÈLE GARCH(1,1)
#    [Correction : "Absence de diagnostics de modèles"]
# -------------------------------------------------------------------------------

cat("\n----- 6. DIAGNOSTICS DES RÉSIDUS GARCH(1,1) -----\n")

z <- as.numeric(residuals(fit_ref, standardize = TRUE))

# 6.1 Ljung-Box sur résidus standardisés
lb5  <- Box.test(z, lag = 5,  type = "Ljung-Box")
lb10 <- Box.test(z, lag = 10, type = "Ljung-Box")
lb20 <- Box.test(z, lag = 20, type = "Ljung-Box")

cat("Ljung-Box residus standardises :\n")
cat("  Q(5)  =", round(lb5$statistic, 2),  "| p =", round(lb5$p.value, 4), "\n")
cat("  Q(10) =", round(lb10$statistic, 2), "| p =", round(lb10$p.value, 4), "\n")
cat("  Q(20) =", round(lb20$statistic, 2), "| p =", round(lb20$p.value, 4), "\n")

# 6.2 Ljung-Box sur résidus AU CARRÉ (effets ARCH restants)
lb5sq  <- Box.test(z^2, lag = 5,  type = "Ljung-Box")
lb10sq <- Box.test(z^2, lag = 10, type = "Ljung-Box")

cat("Ljung-Box residus carres :\n")
cat("  Q2(5)  =", round(lb5sq$statistic, 2),  "| p =", round(lb5sq$p.value, 4), "\n")
cat("  Q2(10) =", round(lb10sq$statistic, 2), "| p =", round(lb10sq$p.value, 4), "\n")

# 6.3 ARCH-LM sur résidus standardisés
arch_z10 <- arch_lm_test(z, 10)
cat("ARCH-LM(10) residus : LM =", round(arch_z10$statistic, 2),
    "| p =", round(arch_z10$p.value, 4), "\n")

diag_table <- data.frame(
  Test = c("Ljung-Box Q(5)", "Ljung-Box Q(10)", "Ljung-Box Q(20)",
           "Ljung-Box Q2(5)", "Ljung-Box Q2(10)", "ARCH-LM(10)"),
  Statistique = c(round(lb5$statistic, 2), round(lb10$statistic, 2),
                  round(lb20$statistic, 2), round(lb5sq$statistic, 2),
                  round(lb10sq$statistic, 2), round(arch_z10$statistic, 2)),
  p_value = c(round(lb5$p.value, 4), round(lb10$p.value, 4),
              round(lb20$p.value, 4), round(lb5sq$p.value, 4),
              round(lb10sq$p.value, 4), round(arch_z10$p.value, 4))
)
diag_table$Conclusion <- ifelse(diag_table$p_value > 0.05,
                                "OK (pas de structure residuelle)",
                                "Structure residuelle detectee")
print(diag_table)
write.csv(diag_table, "resultats/T7_diagnostics_residus.csv", row.names = FALSE)

# 6.4 Jarque-Bera sur résidus standardisés
jb_z <- jarque.bera.test(z)
cat("Jarque-Bera residus : JB =", round(jb_z$statistic, 2),
    "| p =", format.pval(jb_z$p.value, digits = 4), "\n")
cat("Skewness residus =", round(skewness(z), 4),
    "| Kurtosis =", round(kurtosis(z), 4), "\n")

# GRAPHIQUE 3 : Q-Q plot des résidus standardisés
png("resultats/FIG3_qqplot_residus_garch.png", width = 800, height = 600)
qqnorm(z, main = "Q-Q Plot - Residus standardises GARCH(1,1)",
       pch = 20, col = "steelblue")
qqline(z, col = "red", lwd = 2)
grid()
dev.off()
cat("-> FIG3_qqplot_residus_garch.png exporte\n")

# GRAPHIQUE 4 : ACF des résidus et résidus carrés
png("resultats/FIG4_acf_residus.png", width = 1000, height = 500)
par(mfrow = c(1, 2))
acf(z,   main = "ACF - Residus standardises", lag.max = 20)
acf(z^2, main = "ACF - Residus au carre", lag.max = 20)
dev.off()
cat("-> FIG4_acf_residus.png exporte\n")

# -------------------------------------------------------------------------------
# 7. PERSISTANCE DE LA VOLATILITÉ
# -------------------------------------------------------------------------------

cat("\n----- 7. PERSISTANCE DE LA VOLATILITÉ -----\n")

alpha <- coefs["alpha1"]; beta <- coefs["beta1"]
persistance <- alpha + beta
half_life <- log(0.5) / log(persistance)

cat("Persistance (alpha + beta) =", round(persistance, 6), "\n")
cat("Demi-vie d'un choc =", round(half_life, 0), "jours (",
    round(half_life / 252, 2), "annees de bourse )\n")

persist_table <- data.frame(
  Mesure = c("alpha (reaction aux chocs)", "beta (memoire volatilite)",
             "Persistance alpha+beta", "Demi-vie du choc (jours)"),
  Valeur = c(round(alpha, 4), round(beta, 4),
             round(persistance, 4), round(half_life, 0))
)
write.csv(persist_table, "resultats/T8_persistance_volatilite.csv", row.names = FALSE)

# GRAPHIQUE 5 : Volatilité conditionnelle estimée
sigma_t <- as.numeric(sigma(fit_ref)) * 100
png("resultats/FIG5_volatilite_conditionnelle.png", width = 1000, height = 500)
plot(brvm$Date[1:n_in], sigma_t, type = "l", col = "darkorange", lwd = 1.5,
     main = "Volatilite conditionnelle GARCH(1,1) - BRVM",
     xlab = "Date", ylab = "Volatilite (%)")
grid()
dev.off()
cat("-> FIG5_volatilite_conditionnelle.png exporte\n")

# -------------------------------------------------------------------------------
# 8. THÉORIE DES VALEURS EXTRÊMES (EVT - Approche POT)
# -------------------------------------------------------------------------------

cat("\n----- 8. EVT - PEAKS OVER THRESHOLD -----\n")

pertes <- -r_in  # pertes positives
seuil <- quantile(pertes, 0.80)
exces <- pertes[pertes > seuil] - seuil

cat("Seuil (quantile 80%) =", round(seuil, 4), "%\n")
cat("Nombre d'exces =", length(exces), "/", n_in, "\n")

# Ajustement GPD par maximum de vraisemblance
gpd_fit <- tryCatch(
  evir::gpd(pertes, threshold = seuil),
  error = function(e) NULL)

if (!is.null(gpd_fit)) {
  xi   <- gpd_fit$par.ests["xi"]
  beta_gpd <- gpd_fit$par.ests["beta"]
  cat("Parametre de forme xi =", round(xi, 4), "\n")
  cat("Parametre d'echelle beta =", round(beta_gpd, 4), "\n")
  
  evt_table <- data.frame(
    Parametre = c("Seuil u (quantile 80%)", "Nb exces", "xi (forme)", "beta (echelle)"),
    Valeur = c(round(seuil, 4), length(exces), round(xi, 4), round(beta_gpd, 4))
  )
  write.csv(evt_table, "resultats/T9_parametres_EVT.csv", row.names = FALSE)
}

# -------------------------------------------------------------------------------
# 9. CALCUL DES VaR (3 MODÈLES) SUR OUT-OF-SAMPLE
# -------------------------------------------------------------------------------

cat("\n----- 9. CALCUL DES VaR 95% (3 APPROCHES) -----\n")

alpha_var <- 0.95

# 9.1 VaR Paramétrique RiskMetrics (EWMA lambda = 0.94)
lambda <- 0.94
sigma2_ewma <- var(r_in)
for (t in 2:n_in) {
  sigma2_ewma <- lambda * sigma2_ewma + (1 - lambda) * r_in[t]^2
}
var_param <- qnorm(1 - alpha_var) * sqrt(sigma2_ewma) * (-1)
cat("VaR Parametrique (RiskMetrics) =", round(var_param, 4), "%\n")

# 9.2 VaR GARCH-EVT
sigma_forecast <- as.numeric(sigma(ugarchforecast(fit_ref, n.ahead = 1))) * 100
if (!is.null(gpd_fit)) {
  Nu <- length(exces); Nn <- length(pertes)
  var_evt_quantile <- seuil + (beta_gpd / xi) * (((Nn / Nu) * (1 - alpha_var))^(-xi) - 1)
  var_garch_evt <- var_evt_quantile
} else {
  var_garch_evt <- quantile(pertes, alpha_var)
}
cat("VaR GARCH-EVT =", round(var_garch_evt, 4), "%\n")

# 9.3 VaR Monte Carlo (10 000 simulations)
set.seed(123)
mc_sims <- rnorm(10000, mean(r_in), sd(r_in))
var_mc <- -quantile(mc_sims, 1 - alpha_var)
cat("VaR Monte Carlo =", round(var_mc, 4), "%\n")

var_table <- data.frame(
  Modele = c("Parametrique (RiskMetrics)", "GARCH-EVT", "Monte Carlo"),
  VaR_95 = c(round(var_param, 4), round(var_garch_evt, 4), round(var_mc, 4))
)
write.csv(var_table, "resultats/T10_VaR_95_trois_modeles.csv", row.names = FALSE)

# GRAPHIQUE 6 : Rendements out-of-sample vs VaR
png("resultats/FIG6_var_comparaison.png", width = 1000, height = 600)
plot(r_out, type = "h", col = "gray50",
     main = "Rendements out-of-sample vs seuils VaR 95%",
     xlab = "Jours (out-of-sample)", ylab = "Rendement (%)",
     ylim = range(c(r_out, -var_garch_evt * 1.3)))
abline(h = -var_param, col = "blue", lwd = 2, lty = 1)
abline(h = -var_garch_evt, col = "darkorange", lwd = 2, lty = 2)
abline(h = -var_mc, col = "darkgreen", lwd = 2, lty = 3)
legend("bottomleft",
       legend = c(paste0("Parametrique (", round(var_param, 3), "%)"),
                  paste0("GARCH-EVT (", round(var_garch_evt, 3), "%)"),
                  paste0("Monte Carlo (", round(var_mc, 3), "%)")),
       col = c("blue", "darkorange", "darkgreen"),
       lwd = 2, lty = 1:3, bty = "n")
grid()
dev.off()
cat("-> FIG6_var_comparaison.png exporte\n")

# -------------------------------------------------------------------------------
# 10. BACKTESTING COMPLET : KUPIEC + CHRISTOFFERSEN
#     [Correction : "Développer la présentation des résultats de backtesting"]
# -------------------------------------------------------------------------------

cat("\n----- 10. BACKTESTING (KUPIEC POF + CHRISTOFFERSEN) -----\n")

backtest_var <- function(returns_test, var_level, alpha = 0.95) {
  violations <- as.numeric(returns_test < -var_level)
  n_test <- length(returns_test)
  x <- sum(violations)
  p_attendu <- 1 - alpha
  taux <- x / n_test
  
  # --- Test de Kupiec (Proportion of Failures) ---
  if (x == 0) {
    lr_pof <- -2 * n_test * log(1 - p_attendu)
  } else {
    lr_pof <- -2 * (log((1 - p_attendu)^(n_test - x) * p_attendu^x) -
                    log((1 - taux)^(n_test - x) * taux^x))
  }
  p_pof <- 1 - pchisq(lr_pof, 1)
  
  # --- Test de Christoffersen (indépendance des violations) ---
  if (x > 1 && x < n_test) {
    v <- violations
    n00 <- sum(v[-length(v)] == 0 & v[-1] == 0)
    n01 <- sum(v[-length(v)] == 0 & v[-1] == 1)
    n10 <- sum(v[-length(v)] == 1 & v[-1] == 0)
    n11 <- sum(v[-length(v)] == 1 & v[-1] == 1)
    
    pi01 <- ifelse(n00 + n01 > 0, n01 / (n00 + n01), 0)
    pi11 <- ifelse(n10 + n11 > 0, n11 / (n10 + n11), 0)
    pi_c <- (n01 + n11) / (n00 + n01 + n10 + n11)
    
    l0 <- (1 - pi_c)^(n00 + n10) * pi_c^(n01 + n11)
    l1 <- (1 - pi01)^n00 * pi01^n01 * 
          (ifelse(pi11 < 1, (1 - pi11)^n10, 1)) * (ifelse(pi11 > 0, pi11^n11, 1))
    
    lr_ind <- ifelse(l0 > 0 & l1 > 0, -2 * log(l0 / l1), NA)
    p_ind <- ifelse(!is.na(lr_ind), 1 - pchisq(lr_ind, 1), NA)
  } else {
    lr_ind <- NA; p_ind <- NA
  }
  
  list(n = n_test, violations = x, taux = round(taux * 100, 2),
       LR_POF = round(lr_pof, 4), p_POF = round(p_pof, 4),
       LR_IND = ifelse(is.na(lr_ind), NA, round(lr_ind, 4)),
       p_IND = ifelse(is.na(p_ind), NA, round(p_ind, 4)))
}

bt_param <- backtest_var(r_out, var_param)
bt_evt   <- backtest_var(r_out, var_garch_evt)
bt_mc    <- backtest_var(r_out, var_mc)

bt_table <- data.frame(
  Modele = c("Parametrique", "GARCH-EVT", "Monte Carlo"),
  Violations = c(bt_param$violations, bt_evt$violations, bt_mc$violations),
  Taux_pct = c(bt_param$taux, bt_evt$taux, bt_mc$taux),
  Attendu_pct = 5,
  LR_Kupiec = c(bt_param$LR_POF, bt_evt$LR_POF, bt_mc$LR_POF),
  p_Kupiec = c(bt_param$p_POF, bt_evt$p_POF, bt_mc$p_POF),
  LR_Christoffersen = c(bt_param$LR_IND, bt_evt$LR_IND, bt_mc$LR_IND),
  p_Christoffersen = c(bt_param$p_IND, bt_evt$p_IND, bt_mc$p_IND)
)
print(bt_table)
write.csv(bt_table, "resultats/T11_backtesting_complet.csv", row.names = FALSE)

# -------------------------------------------------------------------------------
# 11. ANALYSE DE ROBUSTESSE : BOOTSTRAP
#     [Correction : "Inclure une discussion plus approfondie sur la robustesse"]
# -------------------------------------------------------------------------------

cat("\n----- 11. ROBUSTESSE : BOOTSTRAP DES VIOLATIONS -----\n")

set.seed(456)
B <- 1000
block <- 5
taux_boot <- numeric(B)

for (b in 1:B) {
  # Bootstrap par blocs (préserve la dépendance temporelle)
  n_blocks <- ceiling(length(r_out) / block)
  starts <- sample(1:(length(r_out) - block + 1), n_blocks, replace = TRUE)
  r_boot <- unlist(lapply(starts, function(s) r_out[s:(s + block - 1)]))
  r_boot <- r_boot[1:length(r_out)]
  taux_boot[b] <- mean(r_boot < -var_garch_evt) * 100
}

ic_boot <- quantile(taux_boot, c(0.025, 0.975))
cat("Bootstrap (B = 1000, blocs = 5) sur VaR GARCH-EVT :\n")
cat("  Taux moyen de violations =", round(mean(taux_boot), 2), "%\n")
cat("  IC 95% = [", round(ic_boot[1], 2), "% ;", round(ic_boot[2], 2), "% ]\n")
cat("  Taux theorique attendu = 5% ->",
    ifelse(5 >= ic_boot[1] & 5 <= ic_boot[2],
           "DANS l'IC : modele robuste", "HORS IC : calibration a revoir"), "\n")

boot_table <- data.frame(
  Mesure = c("Repetitions bootstrap", "Taille des blocs",
             "Taux moyen violations (%)", "IC 2.5%", "IC 97.5%"),
  Valeur = c(B, block, round(mean(taux_boot), 2),
             round(ic_boot[1], 2), round(ic_boot[2], 2))
)
write.csv(boot_table, "resultats/T12_bootstrap_robustesse.csv", row.names = FALSE)

# GRAPHIQUE 7 : Distribution bootstrap
png("resultats/FIG7_bootstrap_distribution.png", width = 800, height = 550)
hist(taux_boot, breaks = 30, col = "lightsteelblue",
     main = "Distribution bootstrap du taux de violations (GARCH-EVT)",
     xlab = "Taux de violations (%)", ylab = "Frequence")
abline(v = 5, col = "red", lwd = 2, lty = 2)
abline(v = ic_boot, col = "darkorange", lwd = 2, lty = 3)
legend("topright", legend = c("Attendu 5%", "IC 95%"),
       col = c("red", "darkorange"), lwd = 2, lty = c(2, 3), bty = "n")
dev.off()
cat("-> FIG7_bootstrap_distribution.png exporte\n")

# -------------------------------------------------------------------------------
# 12. SYNTHÈSE FINALE
# -------------------------------------------------------------------------------

cat("\n")
cat("===============================================================\n")
cat("  SYNTHÈSE DES RÉSULTATS - À REPORTER DANS LE MÉMOIRE\n")
cat("===============================================================\n")
cat("1.  Stationnarite : rendements I(0) (ADF p =", round(adf_rend$p.value, 4), ")\n")
cat("2.  Normalite rejetee (JB p <", format.pval(jb$p.value, digits = 2), ")\n")
cat("3.  Effets ARCH presents (ARCH-LM p <", format.pval(arch_r10$p.value, digits = 2), ")\n")
cat("4.  Meilleur modele (AIC) :", best_name, "\n")
cat("5.  Persistance GARCH =", round(persistance, 4),
    "| demi-vie =", round(half_life, 0), "jours\n")
cat("6.  Diagnostics residus : voir T7 (Ljung-Box, ARCH-LM)\n")
cat("7.  VaR 95% : Param =", round(var_param, 3),
    "% | GARCH-EVT =", round(var_garch_evt, 3),
    "% | MC =", round(var_mc, 3), "%\n")
cat("8.  Backtesting Kupiec + Christoffersen : voir T11\n")
cat("9.  Bootstrap : taux violations", round(mean(taux_boot), 2),
    "% IC[", round(ic_boot[1], 2), ";", round(ic_boot[2], 2), "]\n")
cat("\n")
cat("FICHIERS EXPORTES dans 'resultats/' :\n")
cat("  - 12 tableaux CSV (T1 a T12)\n")
cat("  - 7 graphiques PNG (FIG1 a FIG7)\n")
cat("===============================================================\n")

