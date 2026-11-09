# ===============================================================================
# MOD_BACKTESTING.R - Backtesting rolling windows
# ===============================================================================

mod_backtesting_ui <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(12, h2("Backtesting Rolling Windows")),
    
    column(3,
      sliderInput(ns("window_size"), "Taille fenêtre (jours)", 20, 50, 30, 5),
      actionButton(ns("run_backtest"), "Lancer Backtesting", class = "btn-info")
    ),
    
    column(9,
      plotlyOutput(ns("plot_backtest")),
      DT::dataTableOutput(ns("table_backtest"))
    )
  )
}

mod_backtesting_server <- function(id, returns_data) {
  moduleServer(id, function(input, output, session) {
    
    backtest_results <- eventReactive(input$run_backtest, {
      returns <- returns_data()
      n <- length(returns)
      window_size <- input$window_size
      
      if (n < window_size + 5) {
        return(list(
          windows = c(1),
          violations = c(0),
          message = "Données insuffisantes"
        ))
      }
      
      # Fenêtres roulantes simples
      violations <- c()
      windows <- c()
      
      i <- 1
      for (start_idx in seq(1, n - window_size - 5, by = 5)) {
        end_idx <- start_idx + window_size - 1
        test_idx_start <- end_idx + 1
        test_idx_end <- min(end_idx + 5, n)
        
        if (test_idx_end <= test_idx_start) break
        
        window_returns <- returns[start_idx:end_idx]
        var_val <- calculate_var_monte_carlo(window_returns, 0.95)$var
        
        test_returns <- returns[test_idx_start:test_idx_end]
        n_violations <- sum(test_returns < -var_val, na.rm = TRUE)
        
        violations <- c(violations, n_violations)
        windows <- c(windows, i)
        i <- i + 1
      }
      
      list(windows = windows, violations = violations)
    })
    
    output$plot_backtest <- renderPlotly({
      res <- backtest_results()
      if (is.null(res) || length(res$violations) == 0) return(NULL)
      
      plot_ly(x = res$windows, y = res$violations,
              type = "scatter", mode = "lines+markers",
              marker = list(size = 8, color = "steelblue")) %>%
        layout(title = "Violations - Fenêtres roulantes",
               xaxis = list(title = "Fenêtre"),
               yaxis = list(title = "Nombre violations"))
    })
    
    output$table_backtest <- DT::renderDataTable({
      res <- backtest_results()
      if (is.null(res) || length(res$violations) == 0) return(NULL)
      
      df <- data.frame(
        Fenêtre = res$windows,
        Violations = res$violations
      )
      DT::datatable(df, options = list(pageLength = 10))
    })
  })
}
