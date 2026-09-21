# ============================================================
# ui.
# ============================================================
ui <- fluidPage(
  # applique le thème bootstrap 5 via le package bslib
  theme = bslib::bs_theme(version = 5),
  
  # Affiche le titre principal en haut de l'application
  titlePanel("Suivi trimestriel des campagnes Ulule"),
  
  # structure la page avec une barre latérale (filtres) et un panneau principal
  sidebarLayout(
    
    # barre latérale avec les filtres
    sidebarPanel(
      width = 3, # largeur de la barre latérale
      
      # filtre de sélection pour les années
      selectInput(
        inputId = "annee",
        label = "Année",
        choices = sort(unique(data_ulule_clean$annee)),
        selected = sort(unique(data_ulule_clean$annee)),
        multiple = TRUE
      ),
      # filtres pour les trimestres
      selectInput(
        inputId = "trimestre",
        label = "Trimestre",
        choices = sort(unique(data_ulule_clean$trimestre)),
        selected = sort(unique(data_ulule_clean$trimestre)),
        multiple = TRUE
      ),
      # filtres pour les catégories
      selectInput(
        inputId = "categories",
        label = "Catégorie(s) :",
        choices = liste_categories,
        selected = liste_categories,
        multiple = TRUE
      ),
      # Bouton pour le téléchargement du fichier csv
      downloadButton(
        "telecharger",
        "Télécharger les campagnes (CSV)"
      )
    ),
    
    # Panneau principal
    mainPanel(
      width = 9,
      
      # Création d'un système d'onglets pour naviguer
      tabsetPanel(
        
        # Onglet 1 : Analyse globale des campagnes
        tabPanel(
          "Analyse des campagnes",
          br(),
          
          
          # 3 indicateurs 
          bslib::layout_columns(
            value_box(
              title = "Nombre de campagnes",
              value = textOutput("kpi_nb_campagnes"),
              showcase = bsicons::bs_icon("megaphone"),
              theme = "primary"
            ),
            value_box(
              title = "Campagnes réussies",
              value = textOutput("kpi_nb_reussies"),
              showcase = bsicons::bs_icon("check-circle"),
              theme = "success"
            ),
            value_box(
              title = "Taux de réussite",
              value = textOutput("kpi_taux_reussite"),
              showcase = bsicons::bs_icon("percent"),
              theme = value_box_theme(bg = "#00A3E0", fg = "#FFFFFF")
            ),
            col_widths = c(4, 4, 4)
          ),
          
          # Carte pour le graphique
          card(
            card_header(
              h3("Évolution des campagnes", align = "center")
            ),
            div(
              radioButtons(
                inputId = "indicateur",
                label = NULL,
                choices = indicateurs_campagnes,
                selected = "nb_campagnes",
                inline = TRUE
              )
            ),
            girafeOutput(
              "graphique",
              height = "450px"
            )
          ),
          
          # Éléments sous la carte (inclus dans l'onglet)
          h4("Durée moyenne des campagnes (en jours)"),
          tableOutput("tableau_duree"),
          tags$p(tags$em("Calculé uniquement sur les campagnes terminées."))
        ), # Fin du premier tabPanel
        
        # Onglet 2
        tabPanel(
          "Analyse des financements",
          br(),
          
          layout_columns(
            value_box(
              title = "Montant total financé",
              value = textOutput("kpi_montant_total"),
              showcase = bsicons::bs_icon("cash-stack"),
              theme = value_box_theme(bg = "#00A3E0", fg = "#FFFFFF")
            ),
            value_box(
              title = "Montant médian par campagne",
              value = textOutput("kpi_montant_median"),
              showcase = bsicons::bs_icon("graph-up"),
              theme = value_box_theme(bg = "#E07B00", fg = "#FFFFFF")
            ),
            value_box(
              title = "Pays le plus financé",
              value = textOutput("kpi_top_pays"),
              showcase = bsicons::bs_icon("geo-alt"),
              theme = value_box_theme(bg = "#8E44AD", fg = "#FFFFFF")
            )
          ),
          card(
            card_header(
              h3("Analyse des financements", align = "center")
            ),
            div(
              radioButtons(
                inputId  = "vue_montant",
                label    = NULL,
                choices  = c(
                  "Montant total"                               = "total",
                  "Montant médian du financement des campagnes" = "mediane"
                ),
                selected = "total",
                inline   = TRUE
              )
            ), # <-- Fermeture du div
            girafeOutput("graphique_financement")
          )
          ), # <-- Fermeture de card()
        
        # Onglet 3
        tabPanel(
          "Données",
          br(),
          bslib::card(
            bslib::card_header(
              h3("Extrait des données des campagnes", align = "center")
            ),
            # Affiche la table dynamique
            DT::DTOutput("table_donnees")
          )
        )
      ) # Fin du tabsetPanel
    ) # Fin du mainPanel
  ) # Fin du sidebarLayout
)
# Fin de fluidPage