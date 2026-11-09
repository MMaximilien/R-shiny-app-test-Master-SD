# ============================================================
# server.R
# ============================================================

server <- function(input, output, session) {
  
  # ---- Réinitialisation des filtres ---------------------------------
  observeEvent(input$reset_filtres, {
    updateCheckboxGroupInput(session, "annee",     selected = liste_annees)
    updateCheckboxGroupInput(session, "trimestre", selected = liste_trimestres)
    shinyWidgets::updatePickerInput(session, "categories", selected = liste_categories)
    updateRadioButtons(session, "indicateur",   selected = "nb_campagnes")
    updateRadioButtons(session, "vue_montant",  selected = "total")
    updateRadioButtons(session, "critere_pays", selected = "montant")
  })
  
  
  # ---- Résumé des filtres actifs ------------------------------------
  # Remplit textOutput("resume_filtres"). length() compte le nombre d'éléments
  output$resume_filtres <- renderText({
    paste0(
      length(input$annee), " année(s) · ",
      length(input$trimestre), " trimestre(s) · ",
      length(input$categories), " catégorie(s) sur ", length(liste_categories)
    )
  })
  
  
  # ---- Données filtrées selon les choix de l'utilisateur ------------
  donnees_filtrees <- reactive({
    df <- data_ulule_clean |>
      filter(
        category %in% input$categories,
        annee %in% input$annee,
        trimestre %in% input$trimestre
      )
    
    # validate(need(condition, "message")) : si la condition est fausse (ici, aucune
    # ligne après filtrage), Shiny stoppe le calcul et affiche le message à la place
    # du graphique / tableau, au lieu d'une erreur rouge.
    validate(need(nrow(df) > 0, "Aucune campagne ne correspond à ces filtres."))
    
    df
  })
  
  
  ##### KPI PAGE CAMPAGNES ###############
  
  stats_campagnes <- reactive({
    df <- donnees_filtrees()
    nb_campagnes  <- nrow(df)
    nb_reussies   <- sum(df$goal_raised == TRUE, na.rm = TRUE)
    taux_reussite <- round(nb_reussies / nb_campagnes * 100, 1)
    
    list(
      nb_campagnes  = nb_campagnes,
      nb_reussies   = nb_reussies,
      taux_reussite = taux_reussite
    )
  })
  
  output$kpi_nb_campagnes <- renderText({
    stats_campagnes()$nb_campagnes
  })
  
  output$kpi_nb_reussies <- renderText({
    stats_campagnes()$nb_reussies
  })
  
  output$kpi_taux_reussite <- renderText({
    paste0(stats_campagnes()$taux_reussite, " %")
  })
  
  
  # ---- Table agrégée par période ------------------------------------
  donnees_par_periode <- reactive({
    df <- donnees_filtrees()
    
    df |>
      count(annee, trimestre, name = "nombre_campagnes") |>   # count() compte les lignes par couple (annee, trimestre)
      left_join(  # left_join() ajoute à ce tableau le nombre de campagnes RÉUSSIES par période.
        df |>
          filter(goal_raised == TRUE) |>
          count(annee, trimestre, name = "nombre_reussies"),
        by = c("annee", "trimestre")
      ) |>
      mutate(
        # Une période sans réussite n'a pas de ligne à droite : le join met NA.
        # replace_na remplace les NA par 0.
        nombre_reussies = replace_na(nombre_reussies, 0),
        taux_reussite   = round(nombre_reussies / nombre_campagnes * 100, 1),
        periode         = paste0(annee, "- ", trimestre)
      ) |>
      arrange(annee, trimestre) |>
      mutate(periode = factor(periode, levels = unique(periode))) # fige l'ORDRE des période, sinon ordre alphabétique
  })
  
  
  # ---- Graphique : évolution des campagnes --------------------------
  output$graphique <- renderGirafe({
    
    df <- donnees_par_periode()
    
    p <- if (input$indicateur == "combine") {
      
      df_long <- df |>
        mutate(
          non_reussies = nombre_campagnes - nombre_reussies,
          tooltip = paste0( #texte de l'infobulle
            "Période : ", periode,
            "\nNombre total de campagnes : ", nombre_campagnes,
            "\nNombre de campagnes réussies : ", nombre_reussies,
            "\nTaux de réussite : ", taux_reussite, " %"
          )
        ) |>
        select(periode, tooltip, nombre_reussies, non_reussies) |>
        tidyr::pivot_longer(
          cols = c(nombre_reussies, non_reussies),
          names_to = "statut", # fusionne les noms dans la colonne statut -> une colonne avec les 2 statut par période
          values_to = "nombre" #valeur numérique 
        ) |>
        mutate(
          statut = dplyr::recode(
            statut,
            nombre_reussies = "Réussies",
            non_reussies    = "Non réussies"
          )
        )
      
      ggplot(df_long, aes(x = periode, y = nombre, fill = statut)) + #couleur par statut
        geom_col_interactive(
          aes(tooltip = tooltip, data_id = periode), #Permet d'afficher des infobulles et la période
          position = "fill", #diagramme en barres empilées proportionnelles
          width = 0.7 #largeur des barres
        ) +
        scale_fill_manual(values = c(
          setNames(COULEUR_REUSSIE,     "Réussies"),
          setNames(COULEUR_NON_REUSSIE, "Non réussies")
        )) +
        labs(
          y = "Part des campagnes (%)",
          fill = NULL, #supprime le titre de la légende
          title = "Taux de réussite par période"
        ) +
        scale_y_continuous(labels = scales::label_percent()) #Convertit l'axe Y en pourcentage
      
    } else if (input$indicateur == "nb_reussies") {
      
      ggplot(df, aes(x = periode, y = nombre_reussies)) +
        geom_col_interactive(
          aes(
            tooltip = paste0(
              "Période : ", periode,
              "\nNombre de campagnes réussies : ", nombre_reussies
            ),
            data_id = periode
          ),
          fill = COULEUR_REUSSIE,
          width = 0.7
        ) +
        labs(y = "Nombre de campagnes réussies", title = "Campagnes réussies par période")
      
    } else {
      
      ggplot(df, aes(x = periode, y = nombre_campagnes)) +
        geom_col_interactive(
          aes(
            tooltip = paste0("Période : ", periode, "\nCampagnes : ", nombre_campagnes),
            data_id = periode
          ),
          fill = COULEUR_NON_REUSSIE,
          width = 0.7
        ) +
        labs(y = "Nombre de campagnes", title = "Nombre de campagnes par période")
    }
    
    p <- p + labs(x = "Période") + theme_ulule()
    to_girafe(p)
  })
  
  
  ##### DURÉE DES CAMPAGNES ###############
  
  donnees_terminees <- reactive({
    donnees_filtrees() |> filter(finished == TRUE)
  })
  
  output$tableau_duree <- renderTable({
    
    ordre_trim <- c("T1", "T2", "T3", "T4") # Vecteur définissant l'ordre
    
    df <- donnees_terminees() |>
      mutate(annee = as.character(annee)) # Convertit l'année en texte
    
    validate(need(nrow(df) > 0, "Aucune campagne terminée pour ces filtres."))
    
    # Calcul de la durée moyenne par trimestre et pivot en colonnes
    par_trimestre <- df |>
      group_by(annee, trimestre) |>
      summarise(duree_moyenne = round(mean(nb_days, na.rm = TRUE), 1), .groups = "drop") |>
      tidyr::pivot_wider(names_from = trimestre, values_from = duree_moyenne) |> #transforme les trimestres en colonnes
      select(annee, any_of(ordre_trim))
    
    # Calcul de la moyenne annuelle et fusion avec les trimestres
    tableau <- df |>
      group_by(annee) |>
      summarise(
        Moyenne = round(mean(nb_days, na.rm = TRUE), 1),
        .groups = "drop"
      ) |>
      right_join(par_trimestre, by = "annee") |> # joint moyenne annuelle avec le tableau des trimestres
      relocate(Moyenne, .after = last_col()) |> # Déplace la colonne Moyenne tout à droite
      arrange(annee)
    
    
    # Formatage final (remplacement des NA par "–", ajout de " j" et renommage)
    tableau |>
      mutate(across(where(is.numeric), ~ ifelse(is.na(.x), "–", paste0(.x, " j")))) |>
      rename(Année = annee)
    
  }, striped = TRUE, #effet zebre
  hover    = TRUE, #change de couleur quand on passe la souris
  bordered = TRUE, # bordures 
  align    = "c", #centre le texte
  width    = "100%" #toute la largeur
  )
  
  
  ##### KPI PAGE FINANCEMENT #####################
  
  output$kpi_montant_total <- renderText({
    df <- donnees_filtrees()
    fmt_eur(sum(df$amount_raised, na.rm = TRUE))
  })
  
  output$kpi_montant_median <- renderText({
    df <- donnees_filtrees()
    fmt_eur(median(df$amount_raised, na.rm = TRUE))
  })
  
  output$kpi_top_pays <- renderText({
    df <- donnees_filtrees()
    
    top_pays <- df |>
      group_by(country) |>
      summarise(montant = sum(amount_raised, na.rm = TRUE), .groups = "drop") |>
      arrange(desc(montant)) |>
      slice(1)
    
    paste0(top_pays$country, " (", fmt_eur(top_pays$montant), ")")
  })
  
  
  # ---- ANALYSE DES FINANCEMENTS --------------------------------
  output$graphique_financement <- renderGirafe({
    
    
    df <- donnees_filtrees() |>
      mutate(
        statut  = ifelse(goal_raised, "Réussies", "Non réussies"),
        periode = paste0(annee, " - ", trimestre)
      )
    
    ordre_periodes <- df |>
      distinct(annee, trimestre, periode) |>
      arrange(annee, trimestre) |>
      pull(periode)
    
    df$periode <- factor(df$periode, levels = ordre_periodes)
    
    if (input$vue_montant == "total") {
      
      # ---- Montant total, par statut ----
      df_plot <- df |>
        group_by(periode, statut) |>
        summarise(
          nombre_campagnes = n(),
          montant_total    = sum(amount_raised, na.rm = TRUE),
          .groups = "drop"
        ) |>
        group_by(periode) |>
        mutate(
          montant_total_periode = sum(montant_total),
          pourcentage_montant   = round(montant_total / montant_total_periode * 100, 1)
        ) |>
        ungroup()
      
      p <- ggplot(df_plot, aes(periode, montant_total, fill = statut)) +
        geom_col_interactive(
          aes(
            tooltip = paste0(
              "Période : ", periode,
              "\nStatut : ", statut,
              "\nMontant : ", fmt_eur(montant_total), " (", pourcentage_montant, " %)",
              "\nNombre de campagnes : ", nombre_campagnes,
              "\nMontant total période : ", fmt_eur(montant_total_periode)
            ),
            data_id = paste(periode, statut)
          ),
          position = "stack", #Empile les barres des différents statuts les unes
          width = 0.7
        ) +
        scale_fill_manual(values = c(
          setNames(COULEUR_REUSSIE,     "Réussies"),
          setNames(COULEUR_NON_REUSSIE, "Non réussies")
        )) +
        labs(
          x = "Période", y = "Montant total financé (€)", fill = "Statut",
          title = "Montant total financé par période, par statut"
        ) +
        scale_y_continuous(labels = scales::label_number(big.mark = " ", suffix = " €"))
      
    } else {
      
      # ---- Médiane (avec min / Q1 / Q3 / max dans le tooltip) ----
      df_plot <- df |>
        group_by(periode) |>
        summarise(
          nombre_campagnes = n(),
          montant_min      = min(amount_raised, na.rm = TRUE),
          montant_q1       = as.numeric(quantile(amount_raised, 0.25, na.rm = TRUE)),
          montant_median   = median(amount_raised, na.rm = TRUE),
          montant_q3       = as.numeric(quantile(amount_raised, 0.75, na.rm = TRUE)),
          montant_max      = max(amount_raised, na.rm = TRUE),
          .groups = "drop"
        ) |>
        mutate(statut = "Toutes campagnes (réussies & non réussies)")
      
      p <- ggplot(df_plot, aes(periode, montant_median, color = statut, group = statut)) +
        geom_line_interactive(linewidth = 1) +
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
          values = c("Toutes campagnes (réussies & non réussies)" = COULEUR_MEDIANE)
        ) +
        labs(
          x = "Période", y = "Médiane par campagne (€)", color = NULL,
          title = "Médiane du montant financé par période"
        ) +
        scale_y_continuous(labels = scales::label_number(big.mark = " ", suffix = " €"))
    }
    
    p <- p + theme_ulule()
    to_girafe(p)
  })
  
  
  # ---- Top 10 des pays ------------------------------------------------
  output$graphique_pays <- renderGirafe({
    
    par_nb   <- input$critere_pays == "nb"
    
    df <- donnees_filtrees() |>
      filter(!is.na(country)) |>
      group_by(country) |>
      summarise(
        montant_total  = sum(amount_raised, na.rm = TRUE),
        montant_median = median(amount_raised, na.rm = TRUE),
        nb_campagnes   = n(),
        nb_reussies    = sum(goal_raised == TRUE, na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(taux_reussite = round(nb_reussies / nb_campagnes * 100, 1))
    
    validate(need(nrow(df) > 0, "Aucun pays renseigné pour ces filtres."))
    
    # Valeur utilisée pour classer et dessiner les barres
    df <- df |>
      mutate(valeur = if (par_nb) nb_campagnes else montant_total) |>
      slice_max(valeur, n = 10, with_ties = FALSE) |>
      mutate(country = forcats::fct_reorder(country, valeur))
    
    p <- ggplot(df, aes(x = valeur, y = country)) +
      geom_col_interactive(
        aes(
          tooltip = paste0(
            "Pays : ", country,
            "\nMontant total : ", fmt_eur(montant_total),
            "\nMontant médian : ", fmt_eur(montant_median),
            "\nNombre de campagnes : ", nb_campagnes,
            "\nTaux de réussite : ", taux_reussite, " %"
          ),
          data_id = country
        ),
        fill = COULEUR_PRIMAIRE,
        width = 0.7
      ) +
      scale_x_continuous(
        labels = if (par_nb) {
          scales::label_number(big.mark = " ")
        } else {
          scales::label_number(big.mark = " ", suffix = " €")
        },
        expand = expansion(mult = c(0, 0.05))
      ) +
      labs(
        x = if (par_nb) "Nombre de campagnes" else "Montant total financé (€)",
        y = NULL,
        title = if (par_nb) {
          "Top 10 des pays par nombre de campagnes"
        } else {
          "Top 10 des pays par montant total financé"
        }
      ) +
      theme_ulule() +
      theme(axis.text.x = element_text(angle = 0, hjust = 0.5))
    
    to_girafe(p)
  })
  
  
  ##### DONNÉES #######################
  
  output$table_donnees <- DT::renderDT({
    extrait <- table_export(donnees_filtrees()) |>
      select(-any_of("Lien"))
    
    DT::datatable(
      extrait,
      filter   = "top",
      options  = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    ) |>
      DT::formatRound(c("Objectif (€)", "Montant collecté (€)"), digits = 0, mark = " ")
  })
  
  # ---- Téléchargement CSV ------------------------------------
  output$telecharger <- downloadHandler(
    filename = function() {
      paste0("campagnes_ulule_", Sys.Date(), ".csv")
    },
    content = function(file) { #génère et écrit le contenu 
      readr::write_excel_csv2(table_export(donnees_filtrees()), file)
    } #le format _csv2 utilise ; comme séparateur
  )
}