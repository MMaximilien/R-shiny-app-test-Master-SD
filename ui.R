
# ============================================================
# ui.
# ============================================================
ui <- fluidPage(
  # applique le thème boostrap 5 via le package bslib
  theme = bslib::bs_theme(version = 5),
  #Affiche le titre principal en haut de l'application
  titlePanel("Suivi trimestriel des campagnes Ulule"),
  #structure la page avec une barre latérale (filtres) et un panneau principal
  sidebarLayout(
    #barre latérale avec les filtres
    sidebarPanel(
      width = 3, #largeur de la barre latérale
      
      checkboxGroupInput(
        inputId = "indicateur",
        label = "Choisir un indicateur :",
        choices = indicateurs,
        selected = "nb_campagnes"
      ),
      
      #filtre de sélection pour les années
      selectInput(
        inputId = "annee",
        label = "Année",
        choices = sort(unique(data_ulule_clean$annee)),
        selected = sort(unique(data_ulule_clean$annee)),
        multiple = TRUE
      ),
      #filtres pour les trimestres
      selectInput(
        inputId = "trimestre",
        label = "Trimestre",
        choices = sort(unique(data_ulule_clean$trimestre)),
        selected = sort(unique(data_ulule_clean$trimestre)),
        multiple = TRUE
      ),
      #filtres pour les catégories
      selectInput(
        inputId = "categories",
        label = "Catégorie(s) :",
        choices = liste_categories,
        selected = liste_categories,
        multiple = TRUE
      ),
      #Bouton pour le téléchargment du fichier csv
      downloadButton(
        "telecharger",
        "Télécharger les campagnes (CSV)"
      )
    ),
    
    #Panneau principal
    mainPanel(
      width = 9,
      #Création d'un système d'onglets pour naviguer
      tabsetPanel(
        #Analyse globale des campagnes
        tabPanel(
          "Analyse des campagnes",
          
          #3indicateurs 
          layout_columns(
            
            value_box(
              title = "Nombre de campagnes", #libellé
              value = textOutput("kpi_nb_campagnes"), #valeur dynamique
              showcase = bsicons::bs_icon("megaphone"), #icône
              theme = "primary" #couleur du bloc
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
              theme = "info"
            ),
            
            col_widths = c(4, 4, 4) #découpage
            
          ),
          
          girafeOutput("graphique"), #zone d'affichage du graphique
          
          h4("Durée moyenne des campagnes (en jours)"), #titre
          tableOutput("tableau_duree"),
          
          tags$p(tags$em("Calculé uniquement sur les campagnes terminées.")
          )
        ),
        
        tabPanel(
          "Analyse des financements",
          DTOutput("tableau")
        ),
        
        tabPanel(
          "Données"
        )
      )
    )
  )
)


