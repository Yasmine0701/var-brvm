# ===============================================================================
# EVT_FUNCTIONS.R - Théorie des Valeurs Extrêmes
# ===============================================================================

# Hill Estimator - Indice de queue
hill_estimator <- function(x, threshold_pct = 0.80) {
  x <- sort(x[x > 0], decreasing = TRUE)
  k <- ceiling(length(x) * (1 - threshold_pct))
  
  if (k < 2) return(NULL)
  
  X_k <- x[k]
  excesses <- x[1:k] - X_k
  xi <- mean(log(excesses + X_k) - log(X_k))
  
  list(xi = xi, threshold = X_k, n_excesses = k, domain = ifelse(xi > 0, "Fréchet", "Weibull"))
}

# VaR EVT - Quantile extrême
var_evt <- function(returns, confidence = 0.95, threshold_pct = 0.80) {
  # Losses positives
  losses <- -returns[returns < 0]
  
  hill <- hill_estimator(losses, threshold_pct)
  if (is.null(hill)) return(NULL)
  
  k <- hill$n_excesses
  n <- length(returns)
  alpha <- 1 - confidence
  
  # Quantile
  var <- hill$threshold * ((n / k) * alpha)^(-hill$xi)
  
  list(var = var, xi = hill$xi, threshold = hill$threshold, method = "EVT (Hill)")
}

cat("✅ evt_functions.R chargé\n")
