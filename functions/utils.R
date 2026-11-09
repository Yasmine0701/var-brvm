# ===============================================================================
# UTILS.R - Fonctions utilitaires
# ===============================================================================

# Formater les nombres
format_number <- function(x, digits = 4) {
  format(round(x, digits), nsmall = digits, scientific = FALSE)
}

# Formater les pourcentages
format_percent <- function(x, digits = 2) {
  paste0(format(round(x * 100, digits), nsmall = digits), "%")
}

# Convertir date en chaîne lisible
format_date <- function(date) {
  format(as.Date(date), "%d %b %Y")
}

# Valider inputs
validate_var_inputs <- function(returns, confidence, horizon) {
  if (length(returns) < 30) return("Au minimum 30 observations requises")
  if (confidence <= 0 || confidence >= 1) return("Confidence entre 0 et 1")
  if (horizon < 1) return("Horizon minimum 1 jour")
  return(TRUE)
}

# Générer rapports texte
generate_summary_text <- function(var_rm, var_ge, var_mc) {
  paste(
    "=== RÉSUMÉ VaR BRVM ===\n",
    sprintf("RiskMetrics : %.4f%%\n", var_rm$var),
    sprintf("GARCH-EVT : %.4f%%\n", var_ge$var),
    sprintf("Monte Carlo : %.4f%%\n", var_mc$var),
    "\n✅ Modèle recommandé : Monte Carlo"
  )
}

cat("✅ utils.R chargé\n")
