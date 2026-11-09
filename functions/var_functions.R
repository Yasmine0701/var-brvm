# ===============================================================================
# VAR_FUNCTIONS.R - Calculs de Value-at-Risk
# Trois approches : RiskMetrics, GARCH-EVT, Monte Carlo
# ===============================================================================

# 1. RISKMETRICS (EWMA) - Approche paramétrique simple
# ===============================================================================
calculate_var_riskmetrics <- function(returns, 
                                      confidence = 0.95, 
                                      lambda = 0.94,
                                      n_future = 1) {
  
  # Initialiser volatilité
  n <- length(returns)
  sigma_t <- numeric(n)
  sigma_t[1] <- sd(returns, na.rm = TRUE)
  
  # EWMA - Moyenne mobile pondérée exponentiellement
  for (i in 2:n) {
    sigma_t[i] <- sqrt(lambda * sigma_t[i-1]^2 + (1 - lambda) * returns[i-1]^2)
  }
  
  # VaR paramétrique avec normale
  z_alpha <- qnorm(1 - confidence)
  mu <- mean(returns, na.rm = TRUE)
  var_t <- mu + z_alpha * sigma_t[n] * sqrt(n_future)
  
  return(list(
    var = abs(var_t),
    volatility = sigma_t[n],
    method = "RiskMetrics (EWMA)",
    confidence = confidence,
    z_score = z_alpha,
    forecast_period = n_future
  ))
}

# 2. VaR PARAMÉTRIQUE - Avec distribution Student-t
# ===============================================================================
calculate_var_parametric <- function(returns, 
                                     confidence = 0.95,
                                     dist = "normal") {
  
  n <- length(returns)
  mu <- mean(returns, na.rm = TRUE)
  sigma <- sd(returns, na.rm = TRUE)
  
  if (dist == "normal") {
    z_alpha <- qnorm(1 - confidence)
    var_value <- mu + z_alpha * sigma
  } else if (dist == "student_t") {
    # Estimer degrés de liberté
    df <- estimate_df_student(returns)
    t_alpha <- qt(1 - confidence, df)
    var_value <- mu + t_alpha * sigma
  }
  
  return(list(
    var = abs(var_value),
    mean = mu,
    std = sigma,
    method = paste("Paramétrique -", dist),
    confidence = confidence
  ))
}

# 3. VaR GARCH - Semi-paramétrique avec volatilité dynamique
# ===============================================================================
calculate_var_garch <- function(returns,
                                confidence = 0.95,
                                p = 1, q = 1) {
  
  # Spécification GARCH
  spec <- ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(p, q)),
    mean.model = list(armaOrder = c(0, 0)),
    distribution.model = "std"
  )
  
  # Estimation
  fit <- tryCatch(
    ugarchfit(spec, returns, solver = "hybrid"),
    error = function(e) NULL
  )
  
  if (is.null(fit)) return(NULL)
  
  # Récupérer paramètres
  coef <- coef(fit)
  sigma_t <- tail(fit@fit$sigma, 1)
  df <- coef["shape"]
  
  # Quantile avec Student-t
  z_alpha <- qt(1 - confidence, df)
  var_value <- z_alpha * sigma_t
  
  return(list(
    var = abs(var_value),
    volatility = sigma_t,
    df = df,
    method = "GARCH(1,1)",
    confidence = confidence,
    fit_object = fit
  ))
}

# 4. VaR MONTE CARLO - Simulation stochastique
# ===============================================================================
calculate_var_monte_carlo <- function(returns,
                                      confidence = 0.95,
                                      n_simulations = 10000,
                                      n_periods = 1) {
  
  # Paramètres empiriques
  mu <- mean(returns, na.rm = TRUE)
  sigma <- sd(returns, na.rm = TRUE)
  
  # Générer trajectoires aléatoires
  set.seed(123)
  simulated_returns <- matrix(
    rnorm(n_simulations * n_periods, mean = mu, sd = sigma),
    nrow = n_simulations,
    ncol = n_periods
  )
  
  # Cumul sur la période
  final_returns <- rowSums(simulated_returns)
  
  # VaR = quantile empirique
  alpha <- 1 - confidence
  var_value <- quantile(final_returns, alpha)
  cvar_value <- mean(final_returns[final_returns <= var_value])
  
  return(list(
    var = abs(as.numeric(var_value)),
    cvar = abs(cvar_value),
    simulations = n_simulations,
    simulated_returns = final_returns,
    method = "Monte Carlo",
    confidence = confidence,
    mean = mu,
    std = sigma
  ))
}

# 5. BACKTESTING - Test Kupiec POF
# ===============================================================================
kupiec_pof_test <- function(returns_test, var_estimates, confidence = 0.95) {
  
  n <- length(returns_test)
  violations <- sum(returns_test < -var_estimates, na.rm = TRUE)
  
  # Nombre violations attendues
  alpha <- 1 - confidence
  expected_violations <- n * alpha
  
  # Test du rapport de vraisemblance
  if (violations > 0) {
    lr <- -2 * (n * log(alpha) + (n - violations) * log(1 - alpha) - 
                violations * log(violations / n) - 
                (n - violations) * log((n - violations) / n))
  } else {
    lr <- -2 * (n * log(alpha) + (n - violations) * log(1 - alpha))
  }
  
  # p-value (chi-squared avec 1 degré de liberté)
  p_value <- 1 - pchisq(lr, df = 1)
  
  return(list(
    violations_observed = violations,
    violations_expected = expected_violations,
    violation_rate = violations / n * 100,
    expected_rate = alpha * 100,
    lr_statistic = lr,
    p_value = p_value,
    test_result = ifelse(p_value > 0.05, "Accepté", "Rejeté"),
    conclusion = ifelse(p_value > 0.05, 
                       "Modèle VaR acceptable au seuil 5%",
                       "Modèle VaR non acceptable (violations excessives)")
  ))
}

# 6. CALCUL DES STATISTIQUES DESCRIPTIVES
# ===============================================================================
calculate_skewness <- function(x) {
  x <- na.omit(x)
  n <- length(x)
  m <- mean(x)
  s <- sd(x)
  
  skewness <- (1/n) * sum(((x - m) / s)^3)
  return(skewness)
}

calculate_kurtosis <- function(x) {
  x <- na.omit(x)
  n <- length(x)
  m <- mean(x)
  s <- sd(x)
  
  kurtosis <- (1/n) * sum(((x - m) / s)^4) - 3
  return(kurtosis)
}

# 7. ESTIMATION DEGRÉS DE LIBERTÉ STUDENT-T
# ===============================================================================
estimate_df_student <- function(returns) {
  
  # Estimation par maximum de vraisemblance
  spec <- ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
    mean.model = list(armaOrder = c(0, 0)),
    distribution.model = "std"
  )
  
  fit <- tryCatch(
    ugarchfit(spec, returns, solver = "hybrid"),
    error = function(e) NULL
  )
  
  if (is.null(fit)) {
    return(60)  # Défaut
  }
  
  df <- coef(fit)["shape"]
  return(as.numeric(df))
}

# 8. CALCUL CVaR (Expected Shortfall)
# ===============================================================================
calculate_cvar <- function(returns_test, var_estimate) {
  
  # Perte au-delà de la VaR
  losses_beyond_var <- returns_test[returns_test < -var_estimate]
  
  if (length(losses_beyond_var) == 0) {
    cvar <- var_estimate
  } else {
    cvar <- mean(losses_beyond_var)
  }
  
  return(list(
    cvar = abs(cvar),
    observations_beyond_var = length(losses_beyond_var),
    average_excess_loss = mean(losses_beyond_var, na.rm = TRUE)
  ))
}

# 9. RÉSUMÉ COMPARATIF DES 3 MODÈLES
# ===============================================================================
summary_var_comparison <- function(var_riskmetrics,
                                   var_garch_evt,
                                   var_monte_carlo) {
  
  comparison <- data.frame(
    Modèle = c("RiskMetrics", "GARCH-EVT", "Monte Carlo"),
    VaR = c(var_riskmetrics$var, 
            var_garch_evt$var, 
            var_monte_carlo$var),
    Volatilité = c(var_riskmetrics$volatility,
                   var_garch_evt$volatility,
                   var_monte_carlo$std),
    Approche = c("Paramétrique", "Semi-paramétrique", "Simulation"),
    Complexité = c("Basse", "Haute", "Moyenne")
  )
  
  return(comparison)
}

cat("✅ var_functions.R chargé\n")
