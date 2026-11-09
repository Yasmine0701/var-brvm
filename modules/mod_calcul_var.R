# ===============================================================================
# MOD_CALCUL_VAR.R - Calcul VaR (3 modèles interactifs)
# ===============================================================================

mod_calcul_var_ui <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(12, h2("Calcul Value-at-Risk - 3 Modèles"))
  )
  
  fluidRow(
    # INPUTS (gauche)
    column(6, 
           sliderInput(ns("confidence"), "Niveau de confiance", 0.90, 0.99, 0.95, 0.01),
           sliderInput(ns("horizon"), "Horizon (jours)", 1, 10, 1, 1),
           numericInput(ns("mc_sims"), "Simulations MC", 10000, 1000, 50000, 1000),
           actionButton(ns("calculate"), "Calculer VaR", class = "btn-success")
    ),
    
    # RÉSULTATS (droite)
    column(9,
           column(8, valueBoxOutput(ns("var_rm"))),
           column(8, valueBoxOutput(ns("var_garch"))),
           column(8, valueBoxOutput(ns("var_mc")))
    )
  )
}

mod_calcul_var_server <- function(id, returns_data) {
  moduleServer(id, function(input, output, session) {
    
    results <- eventReactive(input$calculate, {
      var_rm <- calculate_var_riskmetrics(returns_data(), input$confidence, n_future = input$horizon)
      var_garch <- calculate_var_garch(returns_data(), input$confidence)
      var_mc <- calculate_var_monte_carlo(returns_data(), input$confidence, input$mc_sims, input$horizon)
      
      list(rm = var_rm, garch = var_garch, mc = var_mc)
    })
    
    output$var_rm <- renderValueBox({
      if (is.null(results())) return(NULL)
      valueBox(sprintf("%.6f%%", results()$rm$var), "RiskMetrics", icon = icon("chart-bar"), "blue")
    })
    
    output$var_garch <- renderValueBox({
      if (is.null(results())) return(NULL)
      valueBox(sprintf("%.6f%%", results()$garch$var), "GARCH-EVT", icon = icon("wave-square"), "orange")
    })
    
    output$var_mc <- renderValueBox({
      if (is.null(results())) return(NULL)
      valueBox(sprintf("%.6f%%", results()$mc$var), "Monte Carlo", icon = icon("random"), "green")
    })
    
    return(results)
  })
}