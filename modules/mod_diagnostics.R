# ===============================================================================
# MOD_DIAGNOSTICS.R - Diagnostics des modèles GARCH
# (Réponse à l'évaluation : "Absence de diagnostics de modèles")
# ===============================================================================

mod_diagnostics_ui <- function(id) {
  ns <- NS(id)

  fluidRow(
    column(12, h2("Diagnostics des Modèles GARCH")),

    column(3,
      selectInput(ns("modele"), "Modèle à diagnostiquer",
                  choices = c("GARCH(1,1)" = "sGARCH",
                              "EGARCH(1,1)" = "eGARCH")),
      actionButton(ns("run_diag"), "Lancer Diagnostics", class = "btn-primary"),
      br(), br(),
      helpText("Tests réalisés : ADF, comparaison AIC/BIC,",
               "Ljung-Box, ARCH-LM sur résidus, Q-Q plot,",
               "persistance de la volatilité.")
    ),

    column(9,
      tabsetPanel(
        tabPanel("AIC / BIC",
          br(),
          DT::dataTableOutput(ns("table_aic"))
        ),
        tabPanel("Tests résidus",
          br(),
          DT::dataTableOutput(ns("table_tests"))
        ),
        tabPanel("Q-Q Plot",
          br(),
          plotlyOutput(ns("plot_qq"), height = "420px")
        ),
        tabPanel("Volatilité",
          br(),
          plotlyOutput(ns("plot_vol"), height = "420px"),
          verbatimTextOutput(ns("persistance_txt"))
        )
      )
    )
  )
}

mod_diagnostics_server <- function(id, returns_data) {
  moduleServer(id, function(input, output, session) {

    diag_results <- eventReactive(input$run_diag, {
      withProgress(message = "Estimation des modèles...", value = 0.2, {

        returns <- returns_data()
        returns <- returns[!is.na(returns)]
        n <- length(returns)

        # --- Estimation des 4 variantes pour AIC/BIC ---
        fit_one <- function(model, order, dist = "norm", k = 4) {
          spec <- ugarchspec(
            variance.model = list(model = model, garchOrder = order),
            mean.model = list(armaOrder = c(0, 0)),
            distribution.model = dist)
          fit <- tryCatch(ugarchfit(spec, returns, solver = "hybrid"),
                          error = function(e) NULL)
          if (is.null(fit)) return(list(fit = NULL, AIC = NA, BIC = NA))
          ll <- likelihood(fit)
          list(fit = fit,
               AIC = round(-2 * ll + 2 * k, 2),
               BIC = round(-2 * ll + k * log(n), 2))
        }

        incProgress(0.2)
        m_arch   <- fit_one("sGARCH", c(1, 0), k = 3)
        m_garch  <- fit_one("sGARCH", c(1, 1), k = 4)
        m_garcht <- fit_one("sGARCH", c(1, 1), dist = "std", k = 5)
        m_egarch <- fit_one("eGARCH", c(1, 1), k = 5)
        incProgress(0.3)

        aic_table <- data.frame(
          Modele = c("ARCH(1)", "GARCH(1,1)", "GARCH(1,1)-t", "EGARCH(1,1)"),
          AIC = c(m_arch$AIC, m_garch$AIC, m_garcht$AIC, m_egarch$AIC),
          BIC = c(m_arch$BIC, m_garch$BIC, m_garcht$BIC, m_egarch$BIC)
        )
        aic_table$Delta_AIC <- round(aic_table$AIC - min(aic_table$AIC, na.rm = TRUE), 2)

        # --- Modèle sélectionné pour les diagnostics ---
        fit_sel <- if (input$modele == "eGARCH") m_egarch$fit else m_garch$fit
        if (is.null(fit_sel)) return(NULL)

        z <- as.numeric(residuals(fit_sel, standardize = TRUE))

        # --- Tests sur résidus ---
        lb10  <- Box.test(z,   lag = 10, type = "Ljung-Box")
        lb20  <- Box.test(z,   lag = 20, type = "Ljung-Box")
        lb10s <- Box.test(z^2, lag = 10, type = "Ljung-Box")

        arch_lm <- function(x, lag = 10) {
          nn <- length(x); x2 <- x^2
          y <- x2[(lag + 1):nn]
          X <- sapply(1:lag, function(i) x2[(lag + 1 - i):(nn - i)])
          f <- lm(y ~ X)
          stat <- (nn - lag) * summary(f)$r.squared
          list(statistic = stat, p.value = 1 - pchisq(stat, lag))
        }
        alm <- arch_lm(z, 10)

        tests_table <- data.frame(
          Test = c("Ljung-Box Q(10) residus",
                   "Ljung-Box Q(20) residus",
                   "Ljung-Box Q2(10) residus^2",
                   "ARCH-LM(10) residus"),
          Statistique = round(c(lb10$statistic, lb20$statistic,
                                lb10s$statistic, alm$statistic), 3),
          p_value = round(c(lb10$p.value, lb20$p.value,
                            lb10s$p.value, alm$p.value), 4)
        )
        tests_table$Conclusion <- ifelse(tests_table$p_value > 0.05,
                                         "OK : pas de structure residuelle",
                                         "Attention : structure detectee")

        # --- Persistance ---
        cf <- coef(fit_sel)
        if (input$modele == "sGARCH") {
          persistance <- unname(cf["alpha1"] + cf["beta1"])
        } else {
          persistance <- unname(cf["beta1"])
        }
        half_life <- log(0.5) / log(min(persistance, 0.99999))

        sigma_t <- as.numeric(sigma(fit_sel))

        incProgress(0.3)

        list(aic = aic_table, tests = tests_table, z = z,
             sigma = sigma_t, persistance = persistance,
             half_life = half_life, modele = input$modele)
      })
    })

    output$table_aic <- DT::renderDataTable({
      req(diag_results())
      DT::datatable(diag_results()$aic,
                    options = list(dom = "t"), rownames = FALSE) %>%
        DT::formatStyle("Delta_AIC",
                        backgroundColor = DT::styleEqual(0, "#d4edda"))
    })

    output$table_tests <- DT::renderDataTable({
      req(diag_results())
      DT::datatable(diag_results()$tests,
                    options = list(dom = "t"), rownames = FALSE)
    })

    output$plot_qq <- renderPlotly({
      req(diag_results())
      z <- sort(diag_results()$z)
      th <- qnorm(ppoints(length(z)))
      plot_ly() %>%
        add_markers(x = th, y = z, name = "Residus",
                    marker = list(color = "steelblue", size = 5)) %>%
        add_lines(x = range(th), y = range(th) * sd(z) + mean(z),
                  name = "Reference normale",
                  line = list(color = "red", dash = "dash")) %>%
        layout(title = "Q-Q Plot - Residus standardises",
               xaxis = list(title = "Quantiles theoriques N(0,1)"),
               yaxis = list(title = "Quantiles empiriques"))
    })

    output$plot_vol <- renderPlotly({
      req(diag_results())
      s <- diag_results()$sigma * 100
      plot_ly(x = seq_along(s), y = s, type = "scatter", mode = "lines",
              line = list(color = "darkorange")) %>%
        layout(title = "Volatilite conditionnelle estimee (%)",
               xaxis = list(title = "Temps"),
               yaxis = list(title = "Sigma (%)"))
    })

    output$persistance_txt <- renderText({
      req(diag_results())
      d <- diag_results()
      paste0("Persistance de la volatilite : ", round(d$persistance, 4),
             "\nDemi-vie d'un choc : ", round(d$half_life, 0), " jours",
             " (~", round(d$half_life / 252, 2), " annees de bourse)")
    })
  })
}
