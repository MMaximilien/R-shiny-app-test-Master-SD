# ============================================================
# server.R
# ============================================================

server <- function(input, output, session) {
  
  # ---- Données filtrées selon les choix de l'utilisateur ----
  donnees_filtrees <- reactive({
    req(input$categories, input$annee, input$trimestre) 
    
    data_ulule_clean   |> 
      filter(
        category %in% input$categories,
        annee %in% input$annee,
        trimestre %in% input$trimestre
      )
  })
  
  # ---- KPI : nombre de campagnes -------------------------------
  output$kpi_nb_campagnes <- renderText({
    nrow(donnees_filtrees())
  })
  
  
  # ---- KPI : nombre de campagnes réussies ----------------------
  output$kpi_nb_reussies <- renderText({
    sum(donnees_filtrees()$goal_raised == TRUE, na.rm = TRUE)
  })
  
  
  # ---- KPI : taux de réussite ----------------------------------
  output$kpi_taux_reussite <- renderText({
    
    nb_campagnes <- nrow(donnees_filtrees())
    nb_reussies <- sum(donnees_filtrees()$goal_raised == TRUE, na.rm = TRUE)
    
    if (nb_campagnes == 0) {
      return("0 %")
    }
    
    paste0(round(nb_reussies / nb_campagnes * 100, 1), " %")
  })
  
  
  # ---- Graphique : nombre de campagnes ----------------------
  output$graphique <- renderGirafe({
    
    df <- donnees_filtrees() |>
      #Compte le nombre total de campagnes par année et par trimstre
      count(annee, trimestre, name = "nombre_campagnes") |>
      #fusionne le comptage avec le comptage des campagnes réussies
      left_join(
        donnees_filtrees() |>
          filter(goal_raised == TRUE) |> # filtres uniquement sur les campagnes ayant atteint leur objectif
          count(annee, trimestre, name = "nombre_reussies"),
        by = c("annee", "trimestre") #clés de jointure entre les 2 tableaux
      ) |>
      mutate(
        taux_reussite = round(nombre_reussies / nombre_campagnes * 100, 1), #calcul du pourcentage
        periode = paste0(annee, "- ", trimestre) #libellé pour la période
      ) |>
      arrange(annee, trimestre) #trie par année puis trimestre
    
    # Garder l'ordre chronologique
    df$periode <- factor(df$periode, levels = unique(df$periode))
    
    # Graphique
    p <- ggplot(df, aes(x = periode)) +
      
      # ---- Campagnes totales ----
    #trace courbe nombre total de campagnes
    geom_line(
      aes(y = nombre_campagnes, group = 1, color = "Totales"), #Lie la ligne à la valeur et crée la catégorie "Totales"
      linewidth = 1 #épaisseur de la ligne
    ) +
      geom_point_interactive( #point intératifs 
        aes(
          y = nombre_campagnes,
          color = "Totales",
          tooltip = paste0( #infobulle
            "Période : ", periode,
            "\nCampagnes : ", nombre_campagnes,
            "\nRéussies : ", nombre_reussies,
            "\nTaux de réussite : ", taux_reussite, "%"
          )
        ),
        size = 2 #taille des points
      ) +
      
      # ---- Campagnes réussies ----
    geom_line(
      aes(y = nombre_reussies, group = 1, color = "Réussies"),
      linewidth = 1
    ) +
      geom_point_interactive(
        aes(
          y = nombre_reussies,
          color = "Réussies",
          tooltip = paste0(
            "Période : ", periode,
            "\nCampagnes : ", nombre_campagnes,
            "\nRéussies : ", nombre_reussies,
            "\nTaux de réussite : ", taux_reussite, "%"
          )
        ),
        size = 2
      ) +
      
      # Personnalisation des couleurs et de la légende
      scale_color_manual(
        values = c("Totales" = "#7E81AB", "Réussies" = "#2A9D8F")
      ) +
      # étiquettes des axes
      labs(
        x = "Période",
        y = "Nombre de campagnes",
        color = "Type de campagne",
        title = "Évolution des campagnes"
      ) +
      
      theme_minimal() +
      theme(
        legend.position = "bottom", #légende sous le graphique
        axis.text.x = element_text(angle = 45, hjust = 1)        # Inclinasion des étiquettes de l'axe X
        
      )
    
    # Rendu de l'élément interactif avec gestion des dimensions
    girafe(
      ggobj = p,                                                # Transmet le graphique ggplot "p"
      width_svg = 8,                                            # Largeur relative du rendu SVG
      height_svg = 4.5
    )
  })
  
  
  # Campagnes terminées, dans le périmètre filtré
  donnees_terminees <- reactive({
    donnees_filtrees() |> filter(finished == TRUE)
  })
  
  # Tableau croisé : durée moyenne (lignes = trimestre, colonnes = année)
  output$tableau_duree <- renderTable({
    
    df <- donnees_terminees() |>     
      mutate(annee = as.character(as.integer(annee)))
    # uniquement les campagnes terminées
    
    # Moyenne par année et trimestre (une ligne par combinaison)
    par_trimestre <- df |>
      group_by(annee, trimestre) |>
      summarise(duree_moyenne = round(mean(nb_days, na.rm = TRUE), 1), .groups = "drop") |>
      tidyr::pivot_wider(names_from = trimestre, values_from = duree_moyenne) 
    
    # Moyenne globale par année (tous trimestres confondus)
    par_annee <- df |>
      group_by(annee) |>
      summarise(Moyenne = round(mean(nb_days, na.rm = TRUE), 1), .groups = "drop") 
    
    # On colle les deux par année
    left_join(par_trimestre, par_annee, by = "annee") |>
      mutate(annee = as.character(as.integer(annee)))
    
  })
  
  # ---- Téléchargement CSV ------------------------------------
  output$telecharger <- downloadHandler(
    
    filename = function() {
      paste0("campagnes_ulule_", Sys.Date(), ".csv") #génère le nom du fichier CSV au moment du clic
    },
    
    # Exporte les données filtrées
    content = function(file) {
      # Exporte le tableau 'donnees_filtrees()' en CSV
      write.csv(
        donnees_filtrees(), # Données à exporter
        file,               # Chemin du fichier de destination transmis par Shiny
      )
      
    })
}