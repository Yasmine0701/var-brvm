# ===============================================================================
# APP.R - Point d'entrée de l'application Shiny
# Estimation de la Value-at-Risk sur le marché actions de la BRVM
# Master II INSSEDS 2024-2025
# ===============================================================================

# Charger global.R (configuration + données)
source("global.R", encoding = "UTF-8")

# Charger UI et Server
source("ui.R", encoding = "UTF-8")
source("server.R", encoding = "UTF-8")

# ===============================================================================
# LANCER L'APPLICATION
# ===============================================================================

shinyApp(
  ui = ui,
  server = server,
  options = list(
    launch.browser = TRUE,
    shiny.reactlog = FALSE,
    shiny.maxRequestSize = 50 * 1024^2
  )
)

# ===============================================================================
# OPTIONS DE DÉPLOIEMENT
# ===============================================================================

# Pour déployer sur shinyapps.io :
# 1. install.packages("rsconnect")
# 2. rsconnect::setAccountInfo(name='YOUR_ACCOUNT', token='YOUR_TOKEN', secret='YOUR_SECRET')
# 3. rsconnect::deployApp(appDir = getwd())

# Pour exécution locale :
# shiny::runApp()

cat("\n✅ Application VaR BRVM lancée avec succès!\n")
cat("   URL: http://localhost:3838\n\n")
