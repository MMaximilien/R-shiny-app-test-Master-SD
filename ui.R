# ============================================================
# ui.R
# ============================================================

ui <- fluidPage(
  
  # ---- Thème bslib -----------------------------------------------
  theme = bslib::bs_theme(
    version = 5,
    primary = COULEUR_PRIMAIRE,
    "border-radius" = "0.6rem"
  ),
  
  # ---- Bandeau de titre avec logo ---------------------------------
  div(
    class = "text-bg-primary p-4 mb-4 d-flex align-items-center gap-3 justify-content-between",
    
    div(
      class = "d-flex gap-3",
      tags$img(src = "logo_ulule.png", height = "70"),
      div(
        h2("Suivi trimestriel des campagnes Ulule", class = "mb-0"),
        p("Analyse des campagnes, financements par période & catégories")
      )
    ),
    
    p(texte_derniere_maj)
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
        inline   = TRUE
      ),
      
      checkboxGroupInput(
        inputId  = "trimestre",
        label    = "Trimestre",
        choices  = liste_trimestres,
        selected = liste_trimestres,
        inline   = TRUE
      ),
      
      shinyWidgets::pickerInput(
        inputId  = "categories",
        label    = "Catégorie(s) :",
        choices  = liste_categories,
        selected = liste_categories,
        multiple = TRUE,
        options  = shinyWidgets::pickerOptions(
          actionsBox         = TRUE,
          liveSearch         = TRUE,
          selectAllText      = "Tout sélectionner",
          deselectAllText    = "Tout désélectionner",
          noneSelectedText   = "Aucune catégorie",
          selectedTextFormat = "count > 2",
          countSelectedText  = "{0} catégories sélectionnées"
        )
      ),
      
      hr(),
      
      actionButton(
        "reset_filtres",
        "Réinitialiser les filtres",
        icon  = icon("rotate-left"),
        class = "btn-outline-secondary w-100 mb-2"
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
        class = "text-muted small mb-2",
        icon("filter"), " ",
        textOutput("resume_filtres", inline = TRUE),
        icone_aide(AIDE_PERIMETRE)
      ),
      
      tabsetPanel(
        # ---- Onglet 1 : Analyse des campagnes ----------------------
        tabPanel(
          "Analyse des campagnes",
          br(),
          
          bslib::layout_columns(
            value_box(
              title    = tags$span("Nombre de campagnes", icone_aide(AIDE_NB_CAMPAGNES)),
              value    = textOutput("kpi_nb_campagnes"),
              showcase = bsicons::bs_icon("megaphone"),
              theme    = value_box_theme(bg = COULEUR_PRIMAIRE, fg = COULEUR_TEXTE_KPI)
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
          
          card(
            card_header(
              class = "d-flex justify-content-center align-items-center",
              h3("Évolution des campagnes", class = "mb-0"),
              icone_aide(AIDE_EVOLUTION)
            ),
            radioButtons(
              inputId  = "indicateur",
              label    = NULL,
              choices  = indicateurs_campagnes,
              selected = "nb_campagnes",
              inline   = TRUE
            ),
            girafeOutput("graphique", height = "450px")
          ),
          
          h4(
            "Durée moyenne des campagnes (en jours)",
            icone_aide(AIDE_DUREE)
          ),
          tableOutput("tableau_duree"),
          tags$p(tags$em("Calculé uniquement sur les campagnes terminées."))
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
      )
    )
  )
)