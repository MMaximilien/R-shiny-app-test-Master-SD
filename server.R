
library(shiny)

# Define server logic required to draw a histogram
function(input, output, session) {
  
  # Graphique d'évolution du nombre de films par an
  output$plot_evo_nb_films <- renderPlot({
    
    df_allocine |> 
      filter(genre == input$choix_genre) |>  # Ne sélectionner que le genre choisi par l'utilisateur
      count(annee_sortie) |> 
      ggplot() +
      geom_line(
        aes(x = annee_sortie, y = n)
      ) +
      labs(
        title = "Evolution du nombre de films par an",
        subtitle = paste0("Genre ", input$choix_genre, " uniquement")
      ) +
      theme_classic()

    
  })
  
}
