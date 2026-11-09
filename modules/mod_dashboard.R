# ===============================================================================
# MOD_DASHBOARD.R - Dashboard monitoring VaR temps réel
# Les rendements, la VaR, la CVaR et la volatilité sont stockés en décimal
# (ex. 0.0082) et convertis en pourcentage (×100) uniquement à l'affichage.
# ===============================================================================

# UI
mod_dashboard_ui <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    # KPI Cards
    column(6, valueBoxOutput(ns("kpi_var"), width = 12)),
    column(6, valueBoxOutput(ns("kpi_volatility"), width = 12)),
    column(6, valueBoxOutput(ns("kpi_cvar"), width = 12)),
    column(6, valueBoxOutput(ns("kpi_score"), width = 12)),
    
    # Graphiques
    column(8, plotlyOutput(ns("plot_returns"))),
    column(8, plotlyOutput(ns("plot_volatility"))),
    
    # Tableau résumé
    column(12, DT::dataTableOutput(ns("table_summary")))
  )
}

# Server
mod_dashboard_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    # Dernière valeur non manquante d'une série
    last_value <- function(x) {
      x <- x[!is.na(x)]
      if (length(x) == 0) return(NA_real_)
      tail(x, 1)
    }
    
    # Format pourcentage : 0.0082 -> "0.82%"
    fmt_pct <- function(x) {
      if (is.na(x)) return("N/A")
      sprintf("%.2f%%", 100 * x)
    }
    
    # KPI VaR
    output$kpi_var <- renderValueBox({
      var_value <- last_value(data()$var_estimate)
      valueBox(
        value = fmt_pct(var_value),
        subtitle = "VaR (95%) journalière",
        icon = icon("chart-line"),
        color = ifelse(!is.na(var_value) && 100 * var_value > 2, "red", "blue")
      )
    })
    
    # KPI Volatilité
    output$kpi_volatility <- renderValueBox({
      vol <- last_value(data()$volatility)
      valueBox(
        value = fmt_pct(vol),
        subtitle = "Volatilité EGARCH journalière",
        icon = icon("wave-square"),
        color = ifelse(!is.na(vol) && 100 * vol > 1.5, "orange", "green")
      )
    })
    
    # KPI CVaR
    output$kpi_cvar <- renderValueBox({
      cvar <- last_value(data()$cvar)
      valueBox(
        value = fmt_pct(cvar),
        subtitle = "CVaR (Expected Shortfall)",
        icon = icon("exclamation-triangle"),
        color = "purple"
      )
    })
    
    # KPI Score Risque :
    # rang percentile de la VaR actuelle dans l'historique des VaR estimées
    output$kpi_score <- renderValueBox({
      var_hist <- data()$var_estimate
      var_hist <- var_hist[!is.na(var_hist)]
      
      if (length(var_hist) < 2) {
        score <- NA_real_
      } else {
        score <- round(100 * ecdf(var_hist)(tail(var_hist, 1)))
      }
      
      valueBox(
        value = ifelse(is.na(score), "N/A", sprintf("%.0f/100", score)),
        subtitle = "Score Risque (percentile historique de la VaR)",
        icon = icon("tachometer-alt"),
        color = ifelse(!is.na(score) && score > 80, "red",
                       ifelse(!is.na(score) && score > 60, "orange", "green"))
      )
    })
    
    # Graphique Rendements
    output$plot_returns <- renderPlotly({
      d <- data()
      plot_ly(x = d$Date, y = 100 * d$Return, type = "scatter", mode = "lines",
              name = "Rendements") %>%
        layout(title = "Rendements BRVM Composite",
               xaxis = list(title = "Date"),
               yaxis = list(title = "Rendement (%)"),
               hovermode = "x unified")
    })
    
    # Graphique Volatilité
    output$plot_volatility <- renderPlotly({
      d <- data()
      plot_ly(x = d$Date, y = 100 * d$volatility, type = "scatter", mode = "lines",
              name = "Volatilité", fill = "tozeroy") %>%
        layout(title = "Volatilité Conditionnelle EGARCH",
               xaxis = list(title = "Date"),
               yaxis = list(title = "Volatilité (%)"))
    })
    
    # Tableau Résumé (valeurs en %)
    output$table_summary <- DT::renderDataTable({
      d <- data()
      summary_df <- data.frame(
        Date = tail(d$Date, 10),
        `Rendement (%)`  = round(100 * tail(d$Return, 10), 3),
        `Volatilité (%)` = round(100 * tail(d$volatility, 10), 3),
        `VaR (%)`        = round(100 * tail(d$var_estimate, 10), 3),
        `CVaR (%)`       = round(100 * tail(d$cvar, 10), 3),
        check.names = FALSE
      )
      DT::datatable(summary_df, rownames = FALSE, options = list(pageLength = 10))
    })
  })
}