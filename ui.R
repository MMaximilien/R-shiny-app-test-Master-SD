
library(shiny)

navbarPage("Application Allociné", 
           
           # Thème application
           theme = shinytheme("journal"),
       
           # Logo statique allociné
           img(src = "logo_allocine.png", width = "50"), 
           
           tabPanel("Page principale",
      
                    
                    sidebarLayout(
                      
                      # Barre latérale
                      sidebarPanel(
                        
                        # Bouton pour choisir le genre de films
                        selectInput( 
                          "choix_genre",
                          "Choix du genre de film : ",
                          choices = unique(df_allocine$genre) # Proposer tous les genres existants
                        )
                      ),
                      
                      # Ecran principal
                      mainPanel(
                        
                        
                        # Graphique d'évolution du nombre de films par an
                        plotOutput("plot_evo_nb_films"),
                        
                      )
                    )
           ),
           
           tabPanel("A propos",
                    # Texte statique en HTML
                    br(),
                    "Ceci est une mini-application ", strong("Shiny"),
                    " basée sur les données", em("Allociné"))
)

