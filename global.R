# ============================================================
# global.R
# ============================================================

library(tidyverse)

library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(ggiraph)
library(shinydashboard)
library(bslib)
library(lubridate)
library(readr)
library(tidyr)

# 1. Chargement des données
chemin_donnees <- "data/data_ulule_2025.csv"
data_ulule <- read_csv(chemin_donnees)

# ---- 1. Chargement des données --------------------------------------------
data_ulule <- read_csv("data/data_ulule_2025.csv")

# 2. Netoyage des données
# Filtre : campagnes non-annulées, date de début correcte et après 2020, devise uniquement euros (EUR)
data_ulule_clean <- data_ulule |> 
  mutate(date_start = as.Date(date_start), 
         date_end = as.Date(date_end), 
         annee = year(date_start), 
         trimestre = paste0("T", quarter(date_start))
  ) |> 
  filter(is_cancelled == FALSE, 
         !is.na(date_start), 
         date_start >= as.Date("2020-01-01"), 
         currency == "EUR"
  )

# 3. Recalcul des indicateurs
data_ulule_clean <- data_ulule_clean |> 
  mutate(nb_days_recal = as.double(difftime(date_end, date_start, units="days")))

# 4. Création de listes supplémentaires
liste_categories <- sort(unique(na.omit(data_ulule_clean$category)))

indicateurs_campagnes <- c(
  "Nombre de campagnes"          = "nb_campagnes",
  "Nombre de campagnes réussies" = "nb_reussies",
  "Ratio (%)"             = "combine"   # ou "Répartition réussite/échec (%)"
)