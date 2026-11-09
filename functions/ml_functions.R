# ===============================================================================
# ML_FUNCTIONS.R - Machine Learning prédictif
# ===============================================================================

# Random Forest - Prédiction VaR future
train_ml_var_model <- function(returns, features_lag = 5, ntree = 500) {
  
  # Créer features avec lags
  n <- length(returns)
  X <- matrix(NA, nrow = n - features_lag, ncol = features_lag)
  
  for (i in 1:features_lag) {
    X[, i] <- returns[i:(n - features_lag + i - 1)]
  }
  
  # Target : VaR future (perte maximale sur 5 jours)
  y <- rep(NA, nrow(X))
  for (i in 1:nrow(X)) {
    y[i] <- max(abs(returns[(n - features_lag + i):(n - features_lag + i + 4)]))
  }
  
  # Random Forest
  df <- data.frame(X, VaR = y)
  rf <- randomForest(VaR ~ ., data = df, ntree = ntree, importance = TRUE)
  
  return(rf)
}

# Prédiction
predict_var_ml <- function(model, recent_returns) {
  features <- as.data.frame(t(recent_returns))
  names(features) <- paste0("X", 1:length(recent_returns))
  
  prediction <- predict(model, features)
  return(as.numeric(prediction))
}

# Feature Importance
get_feature_importance <- function(model) {
  importance(model)[, "%IncMSE"]
}

cat("✅ ml_functions.R chargé\n")
