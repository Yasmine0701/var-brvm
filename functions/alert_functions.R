# ===============================================================================
# ALERT_FUNCTIONS.R - Système d'alerte macro
# ===============================================================================

# Évaluer alertes CDS
evaluate_cds_alerts <- function(cds_value, thresholds = list(low = 300, medium = 350, high = 400)) {
  if (cds_value < thresholds$low) {
    list(level = "🟢 NORMAL", color = "green", action = "Continuer monitoring")
  } else if (cds_value < thresholds$medium) {
    list(level = "🟡 VIGILANCE", color = "yellow", action = "Augmenter VaR +10%")
  } else if (cds_value < thresholds$high) {
    list(level = "🔴 ALERTE", color = "red", action = "Augmenter VaR +20%")
  } else {
    list(level = "⚫ CRITIQUE", color = "darkred", action = "Audit immédiat")
  }
}

# Évaluer alertes Taux
evaluate_taux_alerts <- function(taux_value, threshold = 6.5) {
  if (taux_value < threshold) {
    list(level = "✅ OK", color = "green", risk_adjustment = 0)
  } else {
    list(level = "⚠️ ATTENTION", color = "orange", risk_adjustment = 0.15)
  }
}

# Score risque global
calculate_risk_score <- function(cds, taux, volatility) {
  score <- 0
  if (cds > 350) score <- score + 30
  if (taux > 6.5) score <- score + 20
  if (volatility > 0.40) score <- score + 30
  
  list(score = min(score, 100), 
       level = ifelse(score < 30, "Faible", ifelse(score < 60, "Modéré", "Élevé")))
}

cat("✅ alert_functions.R chargé\n")
