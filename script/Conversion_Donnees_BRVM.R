# ===============================================================================
# CONVERSION DES DONNÉES BRVM RÉELLES -> FORMAT DU SCRIPT D'ANALYSE
# ===============================================================================
# Gère automatiquement les formats : Sika Finance, Investing.com, Ilboursa
# (séparateurs ; ou , | dates FR JJ/MM/AAAA | virgules décimales | espaces milliers)
#
# UTILISATION :
#   1. Place ton fichier téléchargé dans le dossier VaR_BRVM_Shiny
#   2. Adapte les 2 lignes ci-dessous
#   3. Lance : source("Conversion_Donnees_BRVM.R")
# ===============================================================================

# >>> ADAPTE CES 2 LIGNES <<<
setwd("C:/Users/HP/Desktop/MEMOIRE_M2_YASMINE/MEMOIREYAS/VaR_BRVM_Shiny")
fichier_telecharge <- "BRVMC_historique.csv"   # <- nom EXACT de ton fichier téléchargé

# -------------------------------------------------------------------------------
# Lecture robuste (détecte le séparateur automatiquement)
# -------------------------------------------------------------------------------
premiere_ligne <- readLines(fichier_telecharge, n = 1, warn = FALSE)
sep <- if (grepl(";", premiere_ligne)) ";" else ","

raw <- read.csv(fichier_telecharge, sep = sep, stringsAsFactors = FALSE,
                check.names = FALSE, fileEncoding = "UTF-8-BOM")
cat("Colonnes detectees :", paste(names(raw), collapse = " | "), "\n")
cat("Nombre de lignes   :", nrow(raw), "\n\n")

# -------------------------------------------------------------------------------
# Identification automatique des colonnes Date et Cours de clôture
# -------------------------------------------------------------------------------
noms <- tolower(names(raw))

col_date <- which(grepl("date|jour", noms))[1]
col_close <- which(grepl("cl.ture|dernier|close|cours", noms))[1]

if (is.na(col_date) || is.na(col_close)) {
  stop("Colonnes non reconnues. Renomme manuellement : Date et Cloture")
}
cat("Colonne Date   :", names(raw)[col_date], "\n")
cat("Colonne Cours  :", names(raw)[col_close], "\n\n")

dates_brutes <- as.character(raw[[col_date]])
close_brut   <- as.character(raw[[col_close]])

# -------------------------------------------------------------------------------
# Nettoyage des cours : espaces milliers, virgule décimale
# -------------------------------------------------------------------------------
close_num <- gsub("[[:space:]\u00A0]", "", close_brut)  # espaces + insécables
close_num <- gsub("\\.", "", ifelse(grepl(",", close_num) & grepl("\\..*,", close_num),
                                    close_num, close_num))  # cas 1.234,56
close_num <- gsub(",", ".", close_num)                   # virgule -> point
close_num <- suppressWarnings(as.numeric(close_num))

# -------------------------------------------------------------------------------
# Conversion des dates (essaie plusieurs formats)
# -------------------------------------------------------------------------------
essai_formats <- c("%d/%m/%Y", "%d-%m-%Y", "%Y-%m-%d", "%d/%m/%y", "%m/%d/%Y")
dates <- as.Date(NA)
for (f in essai_formats) {
  tentative <- as.Date(dates_brutes, format = f)
  if (sum(!is.na(tentative)) > 0.9 * length(tentative)) {
    dates <- tentative
    cat("Format de date detecte :", f, "\n")
    break
  }
}
if (all(is.na(dates))) stop("Format de date non reconnu - envoie-moi les 3 premieres lignes du fichier")

# -------------------------------------------------------------------------------
# Assemblage, tri chronologique, calcul des rendements
# -------------------------------------------------------------------------------
df <- data.frame(Date = dates, Close = close_num)
df <- df[!is.na(df$Date) & !is.na(df$Close), ]
df <- df[order(df$Date), ]                 # du plus ancien au plus récent
df <- df[!duplicated(df$Date), ]           # supprime les doublons

# Rendements logarithmiques (standard académique)
df$Return <- c(0, diff(log(df$Close)))

cat("\n----- RESULTAT DE LA CONVERSION -----\n")
cat("Observations :", nrow(df), "\n")
cat("Periode      :", format(min(df$Date)), "->", format(max(df$Date)), "\n")
cat("Cours min/max:", round(min(df$Close), 2), "/", round(max(df$Close), 2), "\n")
cat("Rendement moyen (%) :", round(mean(df$Return) * 100, 4), "\n")
cat("Volatilite (%)      :", round(sd(df$Return) * 100, 4), "\n\n")
print(head(df, 5))
print(tail(df, 5))

# -------------------------------------------------------------------------------
# Sauvegarde : remplace le fichier de données du projet (backup de l'ancien)
# -------------------------------------------------------------------------------
if (file.exists("data/brvm_data.csv")) {
  file.copy("data/brvm_data.csv", "data/brvm_data_SIMULE_backup.csv", overwrite = TRUE)
  cat("\nAncien fichier sauvegarde : data/brvm_data_SIMULE_backup.csv\n")
}
write.csv(df, "data/brvm_data.csv", row.names = FALSE)
cat("NOUVEAU fichier ecrit : data/brvm_data.csv\n")
cat("\n>>> Tu peux maintenant relancer Analyse_Complete_VaR_BRVM.R <<<\n")
