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
      #Nbre total de campagnes par année et par trimestre
      count(annee, trimestre, name = "nombre_campagnes") |>
      # fusion avec les campagnes réussies ( objectif atteint)
      left_join(
        donnees_filtrees() |>
          filter(goal_raised == TRUE) |>
          count(annee, trimestre, name = "nombre_reussies"),
        by = c("annee", "trimestre")
      ) |>
      mutate(
        nombre_reussies = replace_na(nombre_reussies, 0), # Sécurité NA
        taux_reussite   = round(nombre_reussies / nombre_campagnes * 100, 1),
        periode = paste0(annee, "- ", trimestre)
      ) |>
      arrange(annee, trimestre)
    
    df$periode <- factor(df$periode, levels = unique(df$periode)) # confersion pour avoir un ordre chronologique
    
 #Construction du graphique selon l'option choisie
    if (input$indicateur == "combine") {
      
      df_long <- df |>
        mutate(
          non_reussies = nombre_campagnes - nombre_reussies,
          tooltip = paste0(
            "Période : ", periode,
            "\nNombre total de campagnes : ", nombre_campagnes,
            "\nNombre de campagnes réussies : ", nombre_reussies,
            "\nTaux de réussite : ", taux_reussite, " %"
          )
        ) |>
        select(periode, tooltip, nombre_reussies, non_reussies) |>
        tidyr::pivot_longer(
          cols = c(nombre_reussies, non_reussies),
          names_to = "statut",
          values_to = "nombre"
        ) |>
        mutate(
          statut = dplyr::recode(
            statut,
            nombre_reussies = "Réussies",
            non_reussies = "Non réussies"
          )
        )
      
      p <- ggplot(
        df_long,
        aes(x = periode, y = nombre, fill = statut)
      ) +
        geom_col_interactive(
          aes(
            tooltip = tooltip,
            data_id = periode
          ),
          position = "fill",
          width = 0.7
        ) +
        scale_fill_manual(
          values = c(
            "Réussies" = "#00A3E0",
            "Non réussies" = "gray83"
          )
        ) +
        labs(
          y = "Nombre de campagnes",
          fill = NULL,
          title = "Nombre de campagnes par période"
        ) +
        scale_y_continuous(labels = scales::label_percent()) 
    }
    else if (input$indicateur == "nb_reussies") {
      
      # ---- Nombre de campagnes réussies (valeur brute) ----
      p <- ggplot(df, aes(x = periode, y = nombre_reussies)) +
        geom_col_interactive(
          aes(
            tooltip = paste0(
              "Période : ", periode,
              "\nNombre de campagnes réussies : ", nombre_reussies ),
            data_id = periode
          ),
          fill = "#00A3E0",
          width = 0.7
        ) + 
        labs(y = "Nombre de campagnes réussies", title = "Campagnes réussies par période")
      
    } else {
      
      # ---- Nombre total brut ----
      p <- ggplot(df, aes(x = periode, y = nombre_campagnes)) +
        geom_col_interactive(
          aes(
            tooltip = paste0("Période : ", periode, "\nCampagnes : ", nombre_campagnes),
            data_id = periode
          ),
          fill = "gray83",
          width = 0.7
        ) +
        labs(y = "Nombre de campagnes", title = "Nombre de campagnes par période")
    }
    
    p <- p +
      labs(x = "Période") +
      theme_minimal() +
      theme(
        panel.grid      = element_blank(), #enleve la grille
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    girafe(ggobj = p, width_svg = 8, height_svg = 4.5,
           options = list( opts_sizing(rescale = TRUE, width = 1)) )# Force le graphique à occuper 100 % de la largeur disponible)
  })
  
  
  ##### DURÉE DES CAMPAGNES ###############

  
  # Campagnes terminées, dans le périmètre filtré
  donnees_terminees <- reactive({
    donnees_filtrees() |> filter(finished == TRUE)
  })
  
  # Tableau croisé : durée moyenne (lignes = année, colonnes = trimestre)
  output$tableau_duree <- renderTable({
    
    req(nrow(donnees_terminees()) > 0)
    
    ordre_trim <- c("T1", "T2", "T3", "T4")
    
    df <- donnees_terminees() |>
      mutate(annee = as.character(as.integer(annee)))
    
    # Moyenne par année et trimestre (une ligne par combinaison)
    par_trimestre <- df |>
      group_by(annee, trimestre) |>
      summarise(duree_moyenne = round(mean(nb_days, na.rm = TRUE), 1), .groups = "drop") |>
      tidyr::pivot_wider(names_from = trimestre, values_from = duree_moyenne)
    
    # Réordonne les colonnes trimestre chronologiquement (T1 -> T4), en
    # conservant en fin toute valeur inattendue plutôt que de la perdre
    cols_connues   <- intersect(ordre_trim, names(par_trimestre))
    cols_inconnues <- setdiff(names(par_trimestre), c("annee", ordre_trim))
    par_trimestre  <- par_trimestre |> select(annee, all_of(cols_connues), all_of(cols_inconnues))
    
    # Moyenne globale par année (tous trimestres confondus)
    par_annee <- df |>
      group_by(annee) |>
      summarise(Moyenne = round(mean(nb_days, na.rm = TRUE), 1), .groups = "drop")
    
    tableau <- left_join(par_trimestre, par_annee, by = "annee") |>
      arrange(annee)
    
    # Ligne de synthèse : moyenne de chaque colonne, toutes années confondues
    ligne_ensemble <- tableau |>
      summarise(across(where(is.numeric), ~ round(mean(.x, na.rm = TRUE), 1))) |>
      mutate(annee = "Ensemble")
    
    tableau <- bind_rows(tableau, ligne_ensemble)
    
    # Mise en forme finale : suffixe "j", 1 décimale, tiret pour les valeurs manquantes
    tableau |>
      mutate(across(
        where(is.numeric),
        ~ ifelse(is.na(.x), "–", paste0(formatC(.x, format = "f", digits = 1, decimal.mark = ","), " j"))
      )) |>
      rename(Année = annee)
    
  }, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = "s", align = "c", width = "100%")
    
    ############Kpi page financement    #####################

  output$kpi_montant_total <- renderText({
    df <- donnees_filtrees()
    req(nrow(df) > 0)
    
    montant_total <- sum(df$amount_raised, na.rm = TRUE)
    paste0(format(round(montant_total), big.mark = " "), " €")
  })
  
  output$kpi_montant_median <- renderText({
    df <- donnees_filtrees()
    req(nrow(df) > 0)
    
    montant_median <- median(df$amount_raised, na.rm = TRUE)
    paste0(format(round(montant_median), big.mark = " "), " €")
  })
  
  output$kpi_top_pays <- renderText({
    df <- donnees_filtrees()
    req(nrow(df) > 0)
    
    top_pays <- df |>
      group_by(country) |>
      summarise(montant = sum(amount_raised, na.rm = TRUE), .groups = "drop") |>
      arrange(desc(montant)) |>
      slice(1)
    
    paste0(top_pays$country, " (", format(round(top_pays$montant), big.mark = " "), " €)")
  })
  
  # ---- ANALYSE DES FINANCEMENTS --------------------------------
  output$graphique_financement <- renderGirafe({
    
    req(nrow(donnees_filtrees()) > 0)
    req(input$vue_montant)
    
    fmt_eur <- function(x) {
      paste0(format(round(x), big.mark = " "), " €")
    }
    
    df <- donnees_filtrees() |>
      mutate(
        statut = ifelse(goal_raised, "Réussies", "Échouées"),
        periode = paste0(annee, " - ", trimestre)
      )
    
    ordre_periodes <- df |>
      distinct(annee, trimestre, periode) |>
      arrange(annee, trimestre) |>
      pull(periode)
    
    df$periode <- factor(df$periode, levels = ordre_periodes)
    
    
    # ============================================================
    # VUE MONTANT TOTAL
    # ============================================================
    
    if (input$vue_montant == "total") {
      
      df_plot <- df |>
        group_by(periode, statut) |>
        summarise(
          nombre_campagnes = n(),
          montant_total = sum(amount_raised, na.rm = TRUE),
          .groups = "drop"
        ) |>
        group_by(periode) |>
        mutate(
          montant_total_periode = sum(montant_total),
          pourcentage_montant = round(
            montant_total / montant_total_periode * 100,
            1
          )
        ) |>
        ungroup()
      
      p <- ggplot(
        df_plot,
        aes(periode, montant_total, fill = statut)
      ) +
        geom_col_interactive(
          aes(
            tooltip = paste0(
              "Période : ", periode,
              "\nStatut : ", statut,
              "\nMontant : ", fmt_eur(montant_total),
              " (", pourcentage_montant, " %)",
              "\nNombre de campagnes : ", nombre_campagnes,
              "\nMontant total période : ",
              fmt_eur(montant_total_periode)
            ),
            data_id = paste(periode, statut)
          ),
          position = "stack",
          width = 0.7
        ) +
        scale_fill_manual(
          values = c(
            "Réussies" = "#00A3E0",
            "Échouées" = "gray83"
          )
        ) +
        labs(
          x = "Période",
          y = "Montant total financé (€)",
          fill = "Statut",
          title = "Montant total financé par période, par statut"
        ) +
        scale_y_continuous(
          labels = scales::label_number(
            big.mark = " ",
            suffix = " €"
          )
        ) +
        theme_minimal() +
        theme(
          legend.position = "bottom",
          panel.grid = element_blank(),
          axis.text.x = element_text(
            angle = 45,
            hjust = 1
          )
        )
      
      
      # ============================================================
      # VUE MÉDIANE
      # ============================================================
      
    } else {
      
      resume <- function(data) {
        data |>
          summarise(
            nombre_campagnes = n(),
            montant_min = min(
              amount_raised,
              na.rm = TRUE
            ),
            montant_q1 = as.numeric(
              quantile(
                amount_raised,
                0.25,
                na.rm = TRUE
              )
            ),
            montant_median = median(
              amount_raised,
              na.rm = TRUE
            ),
            montant_q3 = as.numeric(
              quantile(
                amount_raised,
                0.75,
                na.rm = TRUE
              )
            ),
            montant_max = max(
              amount_raised,
              na.rm = TRUE
            ),
            .groups = "drop"
          )
      }
      
      # Résumé par période, sans séparer les statuts
      df_plot <- df |>
        group_by(periode) |>
        resume() |>
        mutate(
          statut = "Toutes campagnes (échouées & réussies)"
        )
      
      p <- ggplot(
        df_plot,
        aes(
          periode,
          montant_median,
          color = statut,
          group = statut
        )
      ) +
        geom_line_interactive(
          linewidth = 1
        ) +
        geom_point_interactive(
          aes(
            tooltip = paste0(
              "Période : ", periode,
              "\nStatut : ", statut,
              "\nMédiane : ", fmt_eur(montant_median),
              "\nMin : ", fmt_eur(montant_min),
              "\nQ1 : ", fmt_eur(montant_q1),
              "\nQ3 : ", fmt_eur(montant_q3),
              "\nMax : ", fmt_eur(montant_max),
              "\nNombre de campagnes : ", nombre_campagnes
            ),
            data_id = paste(periode, statut)
          ),
          size = 3
        ) +
        scale_color_manual(
          values = c(
            "Toutes campagnes (échouées & réussies)" = "#27AE60"
          )
        ) +
        labs(
          x = "Période",
          y = "Médiane par campagne (€)",
          title = "Médiane du montant financé par période"
        ) +
        scale_y_continuous(
          labels = scales::label_number(
            big.mark = " ",
            suffix = " €"
          )
        ) +
        theme_minimal() +
        theme(
          legend.position = "bottom",
          panel.grid = element_blank(),
          axis.text.x = element_text(
            angle = 45,
            hjust = 1
          )
        )
    }
    
    
    # ============================================================
    # TRANSFORMATION EN GIRAFE
    # ============================================================
    
    girafe(
      ggobj = p,
      width_svg = 8,
      height_svg = 4.5,
      options = list(
        opts_sizing(
          rescale = TRUE,
          width = 1
        )
      )
    )
  })
  ##############Données #######################
  
  
  # Génération de l'extrait de la table
  output$table_donnees <- DT::renderDT({

    extrait <- donnees_filtrees() |>
      dplyr::relocate(absolute_url, .after = dplyr::last_col()) #mettre la colonne absolute_url a la fin du dataframe
    
    DT::datatable(
      extrait,
      options = list(
        pageLength = 10,       # Nombre de lignes par page
        scrollX = TRUE       # Barre de défilement horizontal si beaucoup de colonnes
      ),
      rownames = FALSE
    )
  })

  # ---- Téléchargement CSV ------------------------------------
  output$telecharger <- downloadHandler(
    filename = function() {
      paste0("campagnes_ulule_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(donnees_filtrees(), file)
    }
  )
}
