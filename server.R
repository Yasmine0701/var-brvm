# ===============================================================================
# SERVER.R - Logique serveur principale
# ===============================================================================

server <- function(input, output, session) {
  
  # ========================================================================
  # DONNÉES RÉACTIVES
  # ========================================================================
  
  # Données BRVM enrichies
  brvm_enriched <- reactive({
    data <- brvm_data
    
    # Ajouter volatilité EGARCH
    fit_egarch <- estimate_egarch(data$Return)
    if (!is.null(fit_egarch)) {
      data$volatility <- fit_egarch@fit$sigma
    } else {
      data$volatility <- NA
    }
    
    # Ajouter VaR estimées
    data$var_estimate <- sapply(1:nrow(data), function(i) {
      if (i < 30) return(NA)
      calculate_var_monte_carlo(data$Return[1:i])$var
    })
    
    # Ajouter CVaR
    data$cvar <- sapply(1:nrow(data), function(i) {
      if (i < 30) return(NA)
      calculate_var_monte_carlo(data$Return[1:i])$cvar
    })
    
   
    
    return(data)
  })
  
  # Données macro
  macro_enriched <- reactive({
    macro_data
  })
  
  # ========================================================================
  # APPELS MODULES
  # ========================================================================
  
  # Dashboard
  mod_dashboard_server("dashboard_1", brvm_enriched)
  
  # Calcul VaR
  var_results <- mod_calcul_var_server("calcul_1", reactive(brvm_data$Return))
  
  # Backtesting
  mod_backtesting_server("backtest_1", reactive(brvm_data$Return))
  
  # Diagnostics GARCH (AIC/BIC, Ljung-Box, ARCH-LM, Q-Q plot)
  mod_diagnostics_server("diagnostics_1", reactive(brvm_data$Return))
  
  # Sensibilité
  mod_sensibilite_server("sensibilite_1", reactive(brvm_data$Return))
  
  # Comparaison
  mod_comparaison_server("comparaison_1", reactive(brvm_data$Return))
  
  # Macro-Risque
  mod_macro_risque_server("macro_1", macro_enriched)
  
  # Prédiction ML
  mod_ml_prediction_server("ml_1", reactive(brvm_data$Return))
  
  # ========================================================================
  # EXPORT PDF
  # ========================================================================
  
  output$download_pdf <- downloadHandler(
    filename = function() {
      paste0("VaR_BRVM_Report_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      # Créer rapport temporaire
      rmd_file <- tempfile(fileext = ".Rmd")
      
      cat("---\ntitle: 'Rapport VaR BRVM'\ndate: '", Sys.Date(), "'\noutput: pdf_document\n---\n\n", 
          "# Résumé Exécutif\n\n",
          "## VaR Estimée\n\n",
          "- **RiskMetrics** : ", 
          ifelse(is.null(var_results()), "N/A", sprintf("%.2f%%", 100 * var_results()$rm$var)), "\n",
          "- **GARCH-EVT** : ",
          ifelse(is.null(var_results()), "N/A", sprintf("%.2f%%", 100 * var_results()$garch$var)), "\n",
          "- **Monte Carlo** : ",
          ifelse(is.null(var_results()), "N/A", sprintf("%.2f%%", 100 * var_results()$mc$var)), "\n\n",
          "## Recommandation\n\n",
          "Adopter le modèle **Monte Carlo** pour la gestion du risque BRVM.\n",
          file = rmd_file)
      
      rmarkdown::render(rmd_file, output_file = file, 
                       output_format = rmarkdown::pdf_document())
    }
  )
  
  # ========================================================================
  # EXPORT CSV
  # ========================================================================
  
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("VaR_BRVM_Data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      data <- brvm_enriched()
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  # ========================================================================
  # MESSAGES D'INITIALISATION
  # ========================================================================
  
  observe({
    shinyalert(
      title = "✅ Bienvenue VaR BRVM",
      text = paste0(
        "Application Master II INSSEDS\n\n",
        "Directeur : Mr AKPOSSO Didier Martial\n",
        "Données : ", nrow(brvm_data), " observations BRVM\n",
        "Période : ", min(brvm_data$Date), " à ", max(brvm_data$Date)
      ),
      type = "info",
      closeOnClickOutside = TRUE,
      html = TRUE
    )
  })
  
  # ========================================================================
  # SESSION INFO
  # ========================================================================
  
  cat("\n✅ Session started at", format(Sys.time(), "%H:%M:%S"), "\n")
}
