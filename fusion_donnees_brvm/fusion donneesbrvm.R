# ===============================================================================
# FUSION DES FICHIERS MENSUELS SIKA FINANCE -> data/brvm_data.csv
# ===============================================================================

setwd("C:/Users/HP/Desktop/MEMOIRE_M2_YASMINE/MEMOIREYAS/VaR_BRVM_Shiny")

# Lit TOUS les fichiers CSV du dossier donnees_brutes
fichiers <- list.files("donnees_brutes", pattern = "\\.csv$", full.names = TRUE)
cat("Fichiers trouves :", length(fichiers), "\n")
print(basename(fichiers))

if (length(fichiers) == 0) stop("Aucun CSV dans donnees_brutes/ !")

# Lecture et empilement
liste <- lapply(fichiers, function(f) {
  d <- read.csv(f, sep = ";", stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM")
  names(d) <- tolower(names(d))
  data.frame(
    Date  = as.Date(d$date, format = "%d/%m/%Y"),
    Close = as.numeric(gsub(",", ".", d$cloture))
  )
})
df <- do.call(rbind, liste)

# Nettoyage : NA, doublons, tri chronologique
df <- df[!is.na(df$Date) & !is.na(df$Close), ]
df <- df[!duplicated(df$Date), ]
df <- df[order(df$Date), ]

# Rendements logarithmiques
df$Return <- c(0, diff(log(df$Close)))

cat("\n----- FUSION TERMINEE -----\n")
cat("Observations totales :", nrow(df), "\n")
cat("Periode              :", format(min(df$Date)), "->", format(max(df$Date)), "\n")
cat("Indice min / max     :", min(df$Close), "/", max(df$Close), "\n")
cat("Rendement moyen (%)  :", round(mean(df$Return)*100, 4), "\n")
cat("Volatilite (%)       :", round(sd(df$Return)*100, 4), "\n\n")
print(head(df, 3)); print(tail(df, 3))

# Verification continuite (détecte les mois manquants)
ecarts <- diff(df$Date)
gros_trous <- which(ecarts > 7)
if (length(gros_trous) > 0) {
  cat("\n⚠ ATTENTION - trous de plus de 7 jours detectes apres :\n")
  print(df$Date[gros_trous])
  cat("-> Verifie qu'il ne manque pas un mois !\n")
} else {
  cat("\n✓ Serie continue, aucun mois manquant\n")
}

# Sauvegarde (backup de l'ancien fichier simule)
if (file.exists("data/brvm_data.csv")) {
  file.copy("data/brvm_data.csv", "data/brvm_data_SIMULE_backup.csv", overwrite = TRUE)
}
write.csv(df, "data/brvm_data.csv", row.names = FALSE)
cat("\n>>> data/brvm_data.csv ecrit - lance maintenant Analyse_Complete_VaR_BRVM.R <<<\n")
