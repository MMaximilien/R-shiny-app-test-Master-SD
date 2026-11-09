# ============================================================
# global.R
# ============================================================


# ---- Librairies ----------------------------------------------------
library(tidyverse)       # dplyr, ggplot2, readr, tidyr, forcats...
library(shiny)           # Application web interactive
library(DT)              # Tableaux de données interactifs
library(ggiraph)         # Graphiques interactifs
library(bslib)           # Thème Bootstrap, cartes, value boxes, tooltips
library(bsicons)         # Icônes (value boxes, aide contextuelle)
library(lubridate)       # Manipulation des dates
library(shinyWidgets)    # pickerInput (sélection multiple avec recherche)


# ---- Chargement des données -----------------------------------------
chemin_donnees <- "data/data_ulule_2025.csv"
data_ulule <- read_csv(chemin_donnees)


# ---- Nettoyage des données -------------------------------------------
data_ulule_propre <- data_ulule |>
  mutate(
    # Conversion des colonnes de dates en vrai type Date
    date_start = as.Date(date_start),
    date_end   = as.Date(date_end),
    
    # Année et trimestre de lancement de la campagne
    annee     = year(date_start),
    trimestre = paste0("T", quarter(date_start)),
    
    # Durée réelle de la campagne en jours, recalculée à partir des dates
    nb_days = as.double(difftime(date_end, date_start, units = "days"))
  ) |>
  filter(
    is_cancelled == FALSE,                  # On écarte les campagnes annulées
    !is.na(date_start),                     # On écarte les lignes sans date de début
    date_start >= as.Date("2020-01-01"),    # Campagnes lancées à partir de 2020
    currency == "EUR"                       # Campagnes en euros uniquement
  )

# ---- Listes utilisées dans les filtres de l'UI -----------------------
liste_annees     <- sort(unique(data_ulule_propre$annee))
liste_trimestres <- sort(unique(data_ulule_propre$trimestre))
liste_categories <- sort(unique(na.omit(data_ulule_propre$category)))

# Correspondance "libellé affiché" -> "valeur technique" (input$indicateur)
indicateurs_campagnes <- c(
  "Nombre de campagnes"          = "nb_campagnes",
  "Nombre de campagnes réussies" = "nb_reussies",
  "Ratio (%)"                    = "combine"
)

# Choix de la vue sur la page financements (input$vue_montant)
vues_montant <- c(
  "Montant total"                               = "total",
  "Montant médian du financement des campagnes" = "mediane"
)

# Critère de classement du top 10 des pays (input$critere_pays)
criteres_pays <- c(
  "Montant total financé (€)" = "montant",
  "Nombre de campagnes"       = "nb"
)



# ---- Constantes de couleurs -------------------------------------------
COULEUR_PRIMAIRE    <- "#00A3E0"
COULEUR_REUSSIE     <- COULEUR_PRIMAIRE
COULEUR_NON_REUSSIE <- "gray83"
COULEUR_MEDIANE     <- "#27AE60"
COULEUR_CORAIL      <- "#FF6B57"
COULEUR_NOIR        <- "#2B2B2B"
COULEUR_VIOLET      <- "#8E44AD"
COULEUR_TEXTE_KPI   <- "#FFFFFF"



# ---- Textes d'aide (infobulles) ----------------------------------------
AIDE_PERIMETRE <- paste(
  "Sont prises en compte : les campagnes en euros, lancées depuis le",
  "1er janvier 2020 et non annulées. Tous les montants sont en euros."
)
AIDE_NB_CAMPAGNES <- "Nombre de campagnes correspondant aux filtres sélectionnés."
AIDE_REUSSIE <- paste(
  "Une campagne est dite réussie lorsqu'elle a atteint son objectif",
  "de financement."
)
AIDE_TAUX <- paste(
  "Part des campagnes réussies parmi les campagnes sélectionnées :",
  "campagnes réussies ÷ nombre total de campagnes × 100."
)
AIDE_EVOLUTION <- paste(
  "Évolution par trimestre de lancement. « Ratio (%) » montre la part de",
  "campagnes réussies et non réussies dans chaque période."
)
AIDE_DUREE <- paste(
  "Durée entre la date de début et la date de fin de la campagne,",
  "calculée uniquement sur les campagnes terminées."
)
AIDE_MONTANT_TOTAL <- paste(
  "Somme des montants collectés par les campagnes sélectionnées,",
  "qu'elles aient réussi ou non."
)
AIDE_MEDIAN <- paste(
  "La moitié des campagnes a collecté moins que ce montant, l'autre moitié",
  "davantage. Moins sensible aux très gros projets que la moyenne."
)
AIDE_TOP_PAYS <- "Pays dont les campagnes ont collecté le montant total le plus élevé."
AIDE_CLASSEMENT_PAYS <- paste(
  "Les 10 premiers pays selon le critère choisi. Les montants sont en euros",
  "(seules les campagnes en euros sont retenues)."
)

# Petite icône "i" avec infobulle (bslib::tooltip), à placer à côté d'un titre
icone_aide <- function(texte) {
  bslib::tooltip( #permet de faire une infobulle
    bsicons::bs_icon("info-circle", class = "ms-1"), #icone I avec un espace à gauche
    texte,
    placement = "top" # infobulle au dessus de l'icone
  )
}


# ---- Fonctions utilitaires --------------------------------------------

# Formate un nombre en euros : arrondi à l'entier, espace comme séparateur
fmt_eur <- function(x) {
  paste0(format(round(x), big.mark = " "), " €")
}

# Colonnes affichées dans l'onglet "Données" et exportées en CSV.
# Nom affiché (à gauche) = nom de la colonne dans les données (à droite).
COLONNES_EXPORT <- c(
  "Identifiant"                = "id",
  "Catégorie"                  = "category",
  "Pays"                       = "country",
  "Localisation"               = "location",
  "Date de début"              = "date_start",
  "Date de fin"                = "date_end",
  "Durée (jours)"              = "nb_days",
  "Statut"                     = "status",
  "Terminée"                   = "finished",
  "Objectif (€)"               = "goal",
  "Montant collecté (€)"       = "amount_raised",
  "Taux de financement (%)"    = "percent",
  "Objectif atteint"           = "goal_raised",
  "Lien"                       = "absolute_url"
)

# Ne garde que les colonnes principales, avec des noms lisibles
table_export <- function(df) {
  df |> select(all_of(COLONNES_EXPORT))
}

# Thème ggplot2 commun à tous les graphiques de l'appli
theme_ulule <- function() {
  theme_minimal() +
    theme(
      panel.grid      = element_blank(), #supprime le quadrillage
      legend.position = "bottom",
      axis.text.x     = element_text(angle = 45, hjust = 1)
    )
}

# Convertit un objet ggplot en graphique interactif girafe
to_girafe <- function(p) {
  girafe(
    ggobj = p, #objet qui va etre transformé
    width_svg  = 8,
    height_svg = 4.5,
    options = list(
      opts_sizing(rescale = TRUE, width = 1) #prend toutes la largeur disponible et redimensionner si on change d'écran 
    )
  )
}

# ---- Date de la dernière campagne -------------------------------------
date_derniere_maj <- max(data_ulule_clean$date_start)
texte_derniere_maj <- paste(
  "Date de lancement de la dernière campagne :",
  format(date_derniere_maj, "%d/%m/%Y")
)