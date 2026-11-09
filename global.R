# ===============================================================================
# GLOBAL.R - Configuration globale et chargement des données
# Application Shiny : Estimation VaR BRVM (Master II INSSEDS)
# ===============================================================================

# 1. LIBRAIRIES REQUISES
# ===============================================================================
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(shinyalert)
library(plotly)
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)
library(zoo)
library(xts)
library(TTR)
library(rugarch)
library(caret)
library(randomForest)
library(rmarkdown)
library(knitr)
library(DT)
library(rhandsontable)

# 2. OPTIONS GLOBALES
# ===============================================================================
options(
  shiny.maxRequestSize = 50 * 1024^2,  # Max upload 50MB
  scipen = 999,                         # Pas notation scientifique
  digits = 4                            # 4 décimales
)

# 3. SOURCER LES FICHIERS FONCTIONS
# ===============================================================================
source("functions/var_functions.R", encoding = "UTF-8")
source("functions/garch_functions.R", encoding = "UTF-8")
source("functions/evt_functions.R", encoding = "UTF-8")
source("functions/ml_functions.R", encoding = "UTF-8")
source("functions/alert_functions.R", encoding = "UTF-8")
source("functions/export_functions.R", encoding = "UTF-8")
source("functions/utils.R", encoding = "UTF-8")

# 4. SOURCER LES MODULES
# ===============================================================================
source("modules/mod_dashboard.R", encoding = "UTF-8")
source("modules/mod_calcul_var.R", encoding = "UTF-8")
source("modules/mod_backtesting.R", encoding = "UTF-8")
source("modules/mod_sensibilite.R", encoding = "UTF-8")
source("modules/mod_comparaison.R", encoding = "UTF-8")
source("modules/mod_macro_risque.R", encoding = "UTF-8")
source("modules/mod_ml_prediction.R", encoding = "UTF-8")
source("modules/mod_diagnostics.R", encoding = "UTF-8")

# 5. CHARGER LES DONNÉES
# ===============================================================================
# Données BRVM Composite - VRAIES données Sika Finance (juil 2025 - juin 2026)
# NOTE : le fichier data/brvm_data.csv doit être présent, contenant les 259 
# observations téléchargées depuis Sika Finance. Voir data/README_DONNEES.txt
if (!file.exists("data/brvm_data.csv")) {
  stop(paste("\n\nERREUR : le fichier data/brvm_data.csv est manquant.\n",
             "Copiez-le depuis votre projet d'analyse principal (259 obs BRVM",
             "Sika Finance).\nVoir data/README_DONNEES.txt pour les instructions.\n"))
}
brvm_data <- read.csv("data/brvm_data.csv", stringsAsFactors = FALSE)
brvm_data$Date <- as.Date(brvm_data$Date)
brvm_data <- brvm_data[order(brvm_data$Date), ]

cat("✅ Données BRVM chargées :", nrow(brvm_data), "observations",
    "(", format(min(brvm_data$Date)), "->", format(max(brvm_data$Date)), ")\n")

# Données macroéconomiques
macro_data <- read.csv("data/macro_data.csv", stringsAsFactors = FALSE)
macro_data$Date <- as.Date(macro_data$Date)

# Paramètres de calibration
calibration <- read.csv("data/calibration.csv", stringsAsFactors = FALSE)

# 6. PARAMÈTRES PAR DÉFAUT
# ===============================================================================
PARAMS <- list(
  # VaR parameters
  confidence_level = 0.95,
  horizon_days = 1,
  
  # GARCH parameters
  garch_p = 1,
  garch_q = 1,
  
  # EVT parameters
  evt_threshold_percentile = 0.80,
  
  # Monte Carlo parameters
  mc_simulations = 10000,
  mc_scenarios = 52,
  
  # Bootstrap parameters
  bootstrap_blocks = 192,
  bootstrap_simulations = 10000,
  
  # Machine Learning
  ml_train_ratio = 0.80,
  ml_ntree = 500,
  
  # Alert thresholds
  alert_cds_low = 300,
  alert_cds_medium = 350,
  alert_cds_high = 400,
  alert_taux_high = 6.5,
  
  # Display
  date_format = "%Y-%m-%d",
  decimal_places = 4
)

# 7. PALETTES DE COULEURS
# ===============================================================================
COLOR_PALETTE <- list(
  primary = "#1f77b4",      # Bleu
  secondary = "#ff7f0e",    # Orange
  success = "#2ca02c",      # Vert
  danger = "#d62728",       # Rouge
  warning = "#ff9900",      # Orange clair
  info = "#17a2b8",         # Cyan
  light = "#f8f9fa",        # Gris clair
  dark = "#343a40"          # Gris foncé
)

# 8. THÈME SHINY DASHBOARD
# ===============================================================================
# Theme géré via CSS personnalisé (www/style.css)
# freshTheme est optionnel - on utilise CSS à la place

# 9. STATISTIQUES DESCRIPTIVES PRÉ-CALCULÉES
# ===============================================================================
STATS_BRVM <- list(
  mean = mean(brvm_data$Return, na.rm = TRUE),
  std = sd(brvm_data$Return, na.rm = TRUE),
  skewness = calculate_skewness(brvm_data$Return),
  kurtosis = calculate_kurtosis(brvm_data$Return),
  min = min(brvm_data$Return, na.rm = TRUE),
  max = max(brvm_data$Return, na.rm = TRUE),
  n_obs = nrow(brvm_data)
)

# 10. MESSAGES ET TEXTES
# ===============================================================================
MESSAGES <- list(
  welcome = "Bienvenue dans l'application VaR BRVM - Master II INSSEDS",
  no_data = "Aucune donnée disponible",
  loading = "Chargement en cours...",
  error = "Une erreur s'est produite",
  success = "Opération réussie !",
  export_pdf = "Exporter en PDF",
  export_csv = "Exporter en CSV"
)

# 11. LABELS ET DESCRIPTIONS
# ===============================================================================
LABELS <- list(
  var = "Value-at-Risk",
  cvar = "Conditional VaR (Expected Shortfall)",
  brvm = "BRVM Composite Index",
  riskmetrics = "RiskMetrics (Paramétrique)",
  garch_evt = "GARCH-EVT (Semi-paramétrique)",
  monte_carlo = "Monte Carlo (Simulation)",
  volatility = "Volatilité conditionnelle",
  returns = "Rendements quotidiens",
  violations = "Violations VaR"
)

# 12. INFORMATION MÉTADONNÉES
# ===============================================================================
APP_INFO <- list(
  title = "Estimation de la Value-at-Risk (VaR) - BRVM",
  subtitle = "Comparaison des approches paramétrique, semi-paramétrique et Monte Carlo",
  version = "1.0.0",
  author = "Master II INSSEDS - 2024-2025",
  director = "Mr AKPOSSO Didier Martial",
  last_update = Sys.Date()
)

cat("\n✅ Configuration globale chargée avec succès\n")
cat(sprintf("  • Données BRVM : %d observations\n", nrow(brvm_data)))
cat(sprintf("  • Plage dates : %s à %s\n", min(brvm_data$Date), max(brvm_data$Date)))
cat(sprintf("  • Paramètres : %d configurés\n", length(PARAMS)))
cat("✅ Prêt à démarrer l'application!\n\n")
