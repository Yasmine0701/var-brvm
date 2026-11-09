# ===============================================================================
# MOD_MACRO_RISQUE.R - Impact variables macro (Taux, CDS, Inflation)
# ===============================================================================

mod_macro_risque_ui <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(12, h2("Analyse Macro-Risque")),
    
    # Alertes
    column(6, valueBoxOutput(ns("alert_cds"))),
    column(6, valueBoxOutput(ns("alert_taux"))),
    column(6, valueBoxOutput(ns("alert_inflation"))),
    column(6, valueBoxOutput(ns("score_risque"))),
    
    # Graphiques
    column(6, plotlyOutput(ns("plot_cds"))),
    column(6, plotlyOutput(ns("plot_taux"))),
    
    # Impact
    column(12, DT::dataTableOutput(ns("table_impact")))
  )
}

mod_macro_risque_server <- function(id, macro_data) {
  moduleServer(id, function(input, output, session) {
    
    cds_current <- reactive(tail(macro_data()$CDS, 1))
    taux_current <- reactive(tail(macro_data()$Taux, 1))
    
    output$alert_cds <- renderValueBox({
      alert <- evaluate_cds_alerts(cds_current())
      valueBox(sprintf("%.0f bps", cds_current()), 
               alert$level, icon = icon("exclamation"),
               color = alert$color)
    })
    
    output$alert_taux <- renderValueBox({
      alert <- evaluate_taux_alerts(taux_current())
      valueBox(sprintf("%.2f%%", taux_current()), 
               alert$level, icon = icon("percent"),
               color = alert$color)
    })
    
    output$alert_inflation <- renderValueBox({
      valueBox("2.3%", "Inflation Stable", 
               icon = icon("chart-line"), "green")
    })
    
    output$score_risque <- renderValueBox({
      score <- calculate_risk_score(cds_current(), taux_current(), 0.35)
      valueBox(sprintf("%.0f/100", score$score), 
               score$level, icon = icon("tachometer-alt"),
               color = ifelse(score$score > 60, "red", "green"))
    })
    
    output$plot_cds <- renderPlotly({
      d <- macro_data()
      plot_ly(x = d$Date, y = d$CDS, type = "scatter", mode = "lines",
              fill = "tozeroy") %>%
        layout(title = "Spread CDS Souverain",
               xaxis = list(title = "Date"),
               yaxis = list(title = "CDS (bps)"))
    })
    
    output$plot_taux <- renderPlotly({
      d <- macro_data()
      plot_ly(x = d$Date, y = d$Taux, type = "scatter", mode = "lines") %>%
        layout(title = "Taux Directeur BCEAO",
               xaxis = list(title = "Date"),
               yaxis = list(title = "Taux (%)"))
    })
    
    output$table_impact <- DT::renderDataTable({
      data.frame(
        Variable = c("CDS Spread", "Taux BCEAO", "Inflation"),
        Valeur = c(sprintf("%.0f bps", cds_current()),
                   sprintf("%.2f%%", taux_current()),
                   "2.3%"),
        Corrélation_VaR = c("-0.52", "-0.35", "+0.18"),
        Alerte = c(ifelse(cds_current() > 350, "🔴", "🟢"),
                   ifelse(taux_current() > 6.5, "⚠️", "✅"),
                   "✅")
      )
    })
  })
}
