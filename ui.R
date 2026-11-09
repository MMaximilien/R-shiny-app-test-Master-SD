# ============================================================
# ui.R
# ============================================================

ui <- fluidPage(
  
  # ---- Thème bslib -----------------------------------------------
  theme = bslib::bs_theme(
    version = 5,
    primary = COULEUR_PRIMAIRE, #Couleur principale : boutons, liens, bandeau
    "border-radius" = "0.6rem" # # Coins arrondis des cartes et boutons
  ),
  
  # ---- Bandeau de titre avec logo ---------------------------------
  div(
    #   text-bg-primary         : fond de la couleur primaire + texte lisible dessus
    #   p-4                     : espace intérieur (padding) de taille 4
    #   mb-4                    : marge en dessous (margin-bottom)
    #   d-flex                  : place les éléments enfants côte à côte (flexbox)
    #   align-items-center      : les centre verticalement
    #   gap-3                   : espace entre les éléments
    #   justify-content-between : pousse le 1er bloc à gauche et le dernier à droite
    class = "text-bg-primary p-4 mb-4 d-flex align-items-center gap-3 justify-content-between",
    
    # Bloc de gauche : logo + titres
    div(
      class = "d-flex gap-3",
      tags$img(src = "logo_ulule.png", height = "70"),
      div(
        h2("Suivi trimestriel des campagnes Ulule", class = "mb-0"), ## mb-0 : pas de marge basse
        p("Analyse des campagnes, financements par période & catégories")
      )
    ),
    
    p(texte_derniere_maj)   # Bloc de droite : date de la dernière campagne (texte préparé dans global.R)
  ),
  
  sidebarLayout(
    
    # ---- Barre latérale : filtres ----------------------------------
    sidebarPanel(
      width = 3,
      
      checkboxGroupInput(
        inputId  = "annee",
        label    = "Année",
        choices  = liste_annees,
        selected = liste_annees,
        inline   = TRUE #cases côte à côte
      ),
      
      checkboxGroupInput(
        inputId  = "trimestre",
        label    = "Trimestre",
        choices  = liste_trimestres,
        selected = liste_trimestres,
        inline   = TRUE
      ),
      # Liste déroulante à choix multiples avec recherche (input$categories).
      shinyWidgets::pickerInput(
        inputId  = "categories",
        label    = "Catégorie(s) :",
        choices  = liste_categories,
        selected = liste_categories,
        multiple = TRUE, # plusieurs choix possibles
        options  = shinyWidgets::pickerOptions(
          actionsBox         = TRUE, # affiche les boutons "Tout / Aucun"
          liveSearch         = TRUE,  # champ de recherche dans la liste
          selectAllText      = "Tout sélectionner",
          deselectAllText    = "Tout désélectionner",
          noneSelectedText   = "Aucune catégorie", # texte si rien n'est choisi
          selectedTextFormat = "count > 2",  # au-delà de 2 choix : affiche un compteur
          countSelectedText  = "{0} catégories sélectionnées"
        )
      ),
      
      hr(), # ligne horizontale de séparation
      
      actionButton(
        "reset_filtres",
        "Réinitialiser les filtres",
        icon  = icon("rotate-left"),
        class = "btn-outline-secondary w-100 mb-2" ## btn-outline-secondary = bouton à contour gris, w-100 = pleine largeur.
      ),
      
      downloadButton(
        "telecharger",
        "Télécharger les campagnes (CSV)",
        class = "w-100"
      )
    ),
    
    # ---- Panneau principal -------------------------------------------
    mainPanel(
      width = 9,
      
      # Résumé des filtres actifs, visible quel que soit l'onglet
      div(
        class = "text-muted small mb-2", #  # text-muted = texte grisé ; small = petite taille.
        icon("filter"), " ",
        textOutput("resume_filtres", inline = TRUE), #   # inline = TRUE : reste sur la même ligne que l'icône
        icone_aide(AIDE_PERIMETRE) # # petite icône "i" 
      ),
      
      tabsetPanel(
        # ---- Onglet 1 : Analyse des campagnes ----------------------
        tabPanel(
          "Analyse des campagnes",
          br(),
          
          bslib::layout_columns( #met plusieurs éléments en colonnes.
            value_box(
              title    = tags$span("Nombre de campagnes", icone_aide(AIDE_NB_CAMPAGNES)),
              value    = textOutput("kpi_nb_campagnes"),
              showcase = bsicons::bs_icon("megaphone"),
              theme    = value_box_theme(bg = COULEUR_PRIMAIRE, fg = COULEUR_TEXTE_KPI) # bg = fond, fg = couleur du texte
            ),
            value_box(
              title    = tags$span("Campagnes réussies", icone_aide(AIDE_REUSSIE)),
              value    = textOutput("kpi_nb_reussies"),
              showcase = bsicons::bs_icon("check-circle"),
              theme    = "success"
            ),
            value_box(
              title    = tags$span("Taux de réussite", icone_aide(AIDE_TAUX)),
              value    = textOutput("kpi_taux_reussite"),
              showcase = bsicons::bs_icon("percent"),
              theme    = value_box_theme(bg = COULEUR_CORAIL, fg = COULEUR_TEXTE_KPI)
            ),
            col_widths = c(4, 4, 4)
          ),
          
          card( # un cadre avec en-tête (card_header) et contenu.
            card_header(
              ##Centre le contenu de l'en-tête (titre + icône d'aide)
              class = "d-flex justify-content-center align-items-center",
              h3("Évolution des campagnes", class = "mb-0"),
              icone_aide(AIDE_EVOLUTION)
            ),
            radioButtons(
              inputId  = "indicateur",
              label    = NULL,
              choices  = indicateurs_campagnes,
              selected = "nb_campagnes",  # choix par défaut
              inline   = TRUE
            ),
            girafeOutput("graphique", height = "450px")
          ),
          
          h4(
            "Durée moyenne des campagnes (en jours)",
            icone_aide(AIDE_DUREE)
          ),
          tableOutput("tableau_duree"),
          tags$p(tags$em("Calculé uniquement sur les campagnes terminées.")) # paragraphe en italique (em = emphasis)
        ),
        
        # ---- Onglet 2 : Analyse des financements -------------------
        tabPanel(
          "Analyse des financements",
          br(),
          
          layout_columns(
            value_box(
              title    = tags$span("Montant total financé", icone_aide(AIDE_MONTANT_TOTAL)),
              value    = textOutput("kpi_montant_total"),
              showcase = bsicons::bs_icon("cash-stack"),
              theme    = value_box_theme(bg = COULEUR_CORAIL, fg = COULEUR_TEXTE_KPI)
            ),
            value_box(
              title    = tags$span("Montant médian par campagne", icone_aide(AIDE_MEDIAN)),
              value    = textOutput("kpi_montant_median"),
              showcase = bsicons::bs_icon("graph-up"),
              theme    = value_box_theme(bg = COULEUR_NOIR, fg = COULEUR_TEXTE_KPI)
            ),
            value_box(
              title    = tags$span("Pays le plus financé", icone_aide(AIDE_TOP_PAYS)),
              value    = textOutput("kpi_top_pays"),
              showcase = bsicons::bs_icon("geo-alt"),
              theme    = value_box_theme(bg = COULEUR_VIOLET, fg = COULEUR_TEXTE_KPI)
            )
          ),
          
          card(
            card_header(
              class = "d-flex justify-content-center",
              h3("Analyse des financements", class = "mb-0")
            ),
            radioButtons(
              inputId  = "vue_montant",
              label    = NULL,
              choices  = vues_montant,
              selected = "total",
              inline   = TRUE
            ),
            girafeOutput("graphique_financement")
          ),
          
          # Nouveau : top 10 des pays
          card(
            card_header(
              class = "d-flex justify-content-center align-items-center",
              h3("Top 10 des pays", class = "mb-0"),
              icone_aide(AIDE_CLASSEMENT_PAYS)
            ),
            radioButtons(
              inputId  = "critere_pays",
              label    = NULL,
              choices  = criteres_pays,
              selected = "montant",
              inline   = TRUE
            ),
            girafeOutput("graphique_pays")
          )
        ),
        
        # ---- Onglet 3 : Données --------------------------------------
        tabPanel(
          "Données",
          br(),
          bslib::card(
            bslib::card_header(
              h3("Extrait des données des campagnes", class = "text-center")
            ),
            DT::DTOutput("table_donnees")
          )
        )
      )     # fin de tabsetPanel
    )       # fin de mainPanel
  )         # fin de sidebarLayout
)           # fin de fluidPage