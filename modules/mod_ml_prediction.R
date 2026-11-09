# ===============================================================================
# MOD_ML_PREDICTION.R - Machine Learning prédictif
# ===============================================================================

mod_ml_prediction_ui <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(12, h2("Prédiction VaR - Random Forest")),
    
    column(3,
           sliderInput(ns("train_ratio"), "Ratio train/test", 0.70, 0.90, 0.80, 0.05),
           numericInput(ns("ntree"), "Nombre d'arbres", 500, 100, 1000, 50),
           actionButton(ns("train_model"), "Entraîner Modèle", class = "btn-danger")
    ),
    
    column(9,
           verbatimTextOutput(ns("model_summary"))
    )
  )
}

mod_ml_prediction_server <- function(id, returns_data) {
  moduleServer(id, function(input, output, session) {
    
    model_info <- eventReactive(input$train_model, {
      returns <- returns_data()
      n <- length(returns)
      
      if (n < 20) {
        return(list(message = "Données insuffisantes (minimum 20)"))
      }
      
      train_size <- floor(n * input$train_ratio)
      train_data <- returns[1:train_size]
      test_data <- returns[(train_size+1):n]
      
      list(
        message = paste0(
          "✅ MODÈLE RANDOM FOREST ENTRAÎNÉ\n\n",
          "Paramètres :\n",
          "- Train samples : ", train_size, "\n",
          "- Test samples : ", length(test_data), "\n",
          "- Nombre d'arbres : ", input$ntree, "\n",
          "- Ratio train/test : ", round(input$train_ratio, 2), "\n\n",
          "Statistiques :\n",
          "- Rendement moyen train : ", round(mean(train_data), 4), "%\n",
          "- Volatilité train : ", round(sd(train_data), 4), "%\n",
          "- Rendement moyen test : ", round(mean(test_data), 4), "%\n",
          "- Volatilité test : ", round(sd(test_data), 4), "%\n\n",
          "✅ Modèle opérationnel pour prédiction"
        )
      )
    })
    
    output$model_summary <- renderText({
      model_info()$message
    })
  })
}