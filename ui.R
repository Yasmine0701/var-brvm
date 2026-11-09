# ===============================================================================
# UI.R - Interface Utilisateur principale
# Application Shiny : VaR BRVM
# ===============================================================================

ui <- dashboardPage(
  # Header
  dashboardHeader(
    title = "VaR BRVM | Master II INSSEDS",
    titleWidth = 350,
    tags$li(class = "dropdown",
            tags$a(href = "javascript:void(0);", 
                   icon("bell"), 
                   class = "dropdown-toggle",
                   `data-toggle` = "dropdown")),
    tags$li(class = "dropdown",
            tags$a(href = "javascript:void(0);",
                   icon("user"),
                   span("Admin"),
                   class = "dropdown-toggle",
                   `data-toggle` = "dropdown"))
  ),
  
  # Sidebar
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      menuItem("📊 Dashboard", tabName = "dashboard", icon = icon("th-large")),
      menuItem("🧮 Calcul VaR", tabName = "calcul_var", icon = icon("calculator")),
      menuItem("🔬 Diagnostics", tabName = "diagnostics", icon = icon("stethoscope")),
      menuItem("📈 Backtesting", tabName = "backtesting", icon = icon("bar-chart")),
      menuItem("🎯 Sensibilité", tabName = "sensibilite", icon = icon("sliders")),
      menuItem("🔄 Comparaison", tabName = "comparaison", icon = icon("exchange")),
      menuItem("💼 Macro-Risque", tabName = "macro", icon = icon("warning")),
      menuItem("🤖 Prédiction ML", tabName = "ml", icon = icon("brain")),
      
      # Divider
      tags$hr(),
      
      # Export options
      h4("Export Résultats"),
      downloadButton("download_pdf", "📄 PDF", class = "btn-block"),
      downloadButton("download_csv", "📊 CSV", class = "btn-block"),
      
      # Info
      tags$hr(),
      h4("Informations"),
      p("Application VaR BRVM", class = "sidebar-footer"),
      p("Master II INSSEDS 2024-2025", class = "sidebar-footer"),
      p(sprintf("Maj: %s", format(Sys.Date(), "%d-%m-%Y")), class = "sidebar-footer")
    )
  ),
  
  # Body
  dashboardBody(
    # CSS personnalisé
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    
    tabItems(
      # TAB 1 : DASHBOARD
      tabItem(tabName = "dashboard",
              h1("📊 Dashboard Monitoring VaR"),
              mod_dashboard_ui("dashboard_1")),
      
      # TAB 2 : CALCUL VaR
      tabItem(tabName = "calcul_var",
              h1("🧮 Calcul Value-at-Risk"),
              mod_calcul_var_ui("calcul_1")),
      
      # TAB : DIAGNOSTICS GARCH
      tabItem(tabName = "diagnostics",
              h1("🔬 Diagnostics des Modèles"),
              mod_diagnostics_ui("diagnostics_1")),
      
      # TAB 3 : BACKTESTING
      tabItem(tabName = "backtesting",
              h1("📈 Backtesting Rolling Windows"),
              mod_backtesting_ui("backtest_1")),
      
      # TAB 4 : SENSIBILITÉ
      tabItem(tabName = "sensibilite",
              h1("🎯 Analyse de Sensibilité"),
              mod_sensibilite_ui("sensibilite_1")),
      
      # TAB 5 : COMPARAISON
      tabItem(tabName = "comparaison",
              h1("🔄 Comparaison Modèles"),
              mod_comparaison_ui("comparaison_1")),
      
      # TAB 6 : MACRO-RISQUE
      tabItem(tabName = "macro",
              h1("💼 Analyse Macro-Risque"),
              mod_macro_risque_ui("macro_1")),
      
      # TAB 7 : PRÉDICTION ML
      tabItem(tabName = "ml",
              h1("🤖 Prédiction Machine Learning"),
              mod_ml_prediction_ui("ml_1"))
    )
  )
)
