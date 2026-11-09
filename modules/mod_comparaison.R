# ===============================================================================
# MOD_COMPARAISON.R - Comparaison des 3 modèles
# ===============================================================================

mod_comparaison_ui <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(12, h2("Comparaison Modèles VaR")),
    
    column(6, plotlyOutput(ns("plot_var_comparison"))),
    column(6, plotlyOutput(ns("plot_volatility_comparison"))),
    
    column(12, DT::dataTableOutput(ns("table_comparison")))
  )
}

mod_comparaison_server <- function(id, returns_data) {
  moduleServer(id, function(input, output, session) {
    
    comparison <- reactive({
      returns <- returns_data()
      var_rm <- calculate_var_riskmetrics(returns)
      var_garch <- calculate_var_garch(returns)
      var_mc <- calculate_var_monte_carlo(returns)
      
      data.frame(
        Modèle = c("RiskMetrics", "GARCH-EVT", "Monte Carlo"),
        VaR = c(var_rm$var, var_garch$var, var_mc$var),
        Volatilité = c(var_rm$volatility, var_garch$volatility, var_mc$std),
        Approche = c("Paramétrique", "Semi-param", "Simulation"),
        Complexité = c("Basse", "Haute", "Moyenne")
      )
    })
    
    output$plot_var_comparison <- renderPlotly({
      comp <- comparison()
      plot_ly(x = comp$Modèle, y = comp$VaR, type = "bar",
              marker = list(color = c("blue", "orange", "green"))) %>%
        layout(title = "VaR par modèle",
               yaxis = list(title = "VaR (%)"))
    })
    
    output$plot_volatility_comparison <- renderPlotly({
      comp <- comparison()
      plot_ly(x = comp$Modèle, y = comp$Volatilité, type = "bar",
              marker = list(color = c("lightblue", "lightyellow", "lightgreen"))) %>%
        layout(title = "Volatilité par modèle",
               yaxis = list(title = "Volatilité (%)"))
    })
    
    output$table_comparison <- DT::renderDataTable({
      comparison()
    })
  })
}
