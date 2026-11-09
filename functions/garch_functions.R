# ===============================================================================
# GARCH_FUNCTIONS.R - Modèles GARCH et EGARCH
# ===============================================================================

# GARCH(1,1) - Estimation
estimate_garch <- function(returns, p = 1, q = 1) {
  spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(p, q)),
                     mean.model = list(armaOrder = c(0, 0)),
                     distribution.model = "std")
  fit <- tryCatch(ugarchfit(spec, returns, solver = "hybrid"), error = function(e) NULL)
  return(fit)
}

# EGARCH(1,1) - Estimation avec asymétrie
estimate_egarch <- function(returns, p = 1, q = 1) {
  spec <- ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(p, q)),
                     mean.model = list(armaOrder = c(0, 0)),
                     distribution.model = "std")
  fit <- tryCatch(ugarchfit(spec, returns, solver = "hybrid"), error = function(e) NULL)
  return(fit)
}

# Extraire paramètres
extract_garch_params <- function(fit) {
  coef <- coef(fit)
  list(omega = coef["omega"], alpha = coef["alpha1"], beta = coef["beta1"])
}

# Persistance des chocs
garch_persistence <- function(fit) {
  coef <- coef(fit)
  persistence <- coef["alpha1"] + coef["beta1"]
  halflife <- log(0.5) / log(persistence)
  list(persistence = persistence, halflife = halflife)
}

# Diagnostic résidus
ljung_box_test <- function(fit, lags = 10) {
  resid <- residuals(fit)
  resid_sq <- resid^2
  
  n <- length(resid)
  acf_sq <- acf(resid_sq, lag.max = lags, plot = FALSE)$acf[-1]
  
  q_stat <- n * (n + 2) * sum((acf_sq^2) / (n - (1:lags)))
  p_value <- 1 - pchisq(q_stat, lags)
  
  list(Q_statistic = q_stat, p_value = p_value, lags = lags)
}

cat("✅ garch_functions.R chargé\n")
