# ============================================================
# global.R
# ============================================================

library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(ggiraph)
library(shinydashboard)
library(bslib)
library(lubridate)


# ---- 1. Chargement des données --------------------------------------------
chemin_donnees <- "data/data_ulule_2025.csv"  

#Format date

data_ulule$date_start   <- as.Date(data_ulule$date_start)
data_ulule$date_end     <- as.Date(data_ulule$date_end)


data_ulule_clean <- data_ulule |>
  filter(is_cancelled == FALSE) |>
  filter(!is.na(date_start), date_start >= as.Date("2020-01-01"))


data_ulule_clean$annee <- year(data_ulule_clean$date_start)
data_ulule_clean$trimestre <- paste0("T", quarter(data_ulule_clean$date_start))

liste_categories <- sort(unique(na.omit(data_ulule_clean$category)))

indicateurs <- c(
  "Nombre de campagnes" = "nb_campagnes",
  "Nombre de campagnes réussies" = "nb_reussies",
  "Montant financé (€)" = "montant"
)