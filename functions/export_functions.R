# ===============================================================================
# EXPORT_FUNCTIONS.R - Exportation résultats
# ===============================================================================

# Exporter en PDF
export_to_pdf <- function(content, filename) {
  rmarkdown::render(
    input = filename,
    output_format = "pdf_document",
    output_file = paste0(gsub(".Rmd", "", filename), ".pdf")
  )
}

# Exporter résumé CSV
export_summary_csv <- function(data, filename = "var_summary.csv") {
  write.csv(data, file = filename, row.names = FALSE)
}

# Créer rapport HTML
create_html_report <- function(var_results, macro_data) {
  html <- paste0(
    "<html><head><meta charset='UTF-8'><title>Rapport VaR BRVM</title></head><body>",
    "<h1>Rapport Value-at-Risk - BRVM</h1>",
    "<h2>Résultats VaR</h2>",
    "<table border='1'>",
    "<tr><th>Modèle</th><th>VaR</th><th>Confidence</th></tr>"
  )
  
  for (i in 1:nrow(var_results)) {
    html <- paste0(html, 
      "<tr><td>", var_results[i, 1], "</td>",
      "<td>", round(var_results[i, 2], 4), "</td>",
      "<td>95%</td></tr>"
    )
  }
  
  html <- paste0(html, "</table></body></html>")
  return(html)
}

cat("✅ export_functions.R chargé\n")
