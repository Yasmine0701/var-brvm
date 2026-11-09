# ===============================================================================
# MOD_SENSIBILITE.R - Analyse de sensibilité paramétrique
# ===============================================================================

mod_sensibilite_ui <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(12, h2("Analyse de Sensibilité")),
    
    column(3,
           sliderInput(ns("lambda_var"), "Lambda EWMA", 0.80, 0.98, 0.94, 0.01),
           actionButton(ns("run_sensitivity"), "Analyser", class = "btn-warning")
    ),
    
    column(9,
           DT::dataTableOutput(ns("table_sensitivity"))
    )
  )
}

mod_sensibilite_server <- function(id, returns_data) {
  moduleServer(id, function(input, output, session) {
    
    sensitivity <- eventReactive(input$run_sensitivity, {
      returns <- returns_data()
      
      # Variations paramètres autour de lambda
      lambdas <- seq(input$lambda_var - 0.05, input$lambda_var + 0.05, 0.01)
      var_values <- sapply(lambdas, function(l) {
        result <- calculate_var_riskmetrics(returns, lambda = l)
        result$var
      })
      
      data.frame(
        Lambda = round(lambdas, 3),
        VaR = round(var_values, 4),
        Impact = round(abs(var_values - var_values[3]), 4)
      )
    })
    
    output$table_sensitivity <- DT::renderDataTable({
      sensitivity()
    })
  })
}