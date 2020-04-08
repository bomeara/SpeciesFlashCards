#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rinat)

ui <- fluidPage(

    titlePanel("Species Flash Cards"),

    p("This will take several seconds on initial load. Then, click New image to get a new picture to identify. If you move your mouse over the image, you can see the common and species name for the pictured organism. All pictures are research-grade observations from iNaturalist."),

        mainPanel(
           actionButton("goButton", "New image!"),
           htmlOutput("picture"),
           htmlOutput("credit")
  #         textInput('speciesvector', 'Enter a vector (comma delimited) of species you want to learn', "Hyla avivoca,Hyla chrysoscelis,Hyla cinerea,Hyla gratiosa,Hyla versicolor,Pseudacris brachyphona,Pseudacris feriarum")
        )
    
)

server <- function(input, output) {
 
    #src = "https://static.inaturalist.org/photos/64996351/medium.jpg?1585516023"
    randspecies <- eventReactive(input$goButton,{
        i = sample(sequence(length(species)),1)
        local_obs <- image_list[[i]]
        
        return(local_obs[sample(sequence(nrow(local_obs)),1),])
    })
    #   i = sample(sequence(length(species)),1)
    # local_obs <- image_list[[i]]
    # 
    # src<-local_obs$image_url[sample(sequence(nrow(local_obs)),1)]
    # 
    
    output$picture<-renderText({c('<img src="',randspecies()$image_url,'" title="', paste0(randspecies()$common_name, ' (', randspecies()$scientific_name, ')'),'">')})
    output$credit<-renderText(paste0("Photo by user <a href='https://www.inaturalist.org/people/", randspecies()$user_login,"'>",randspecies()$user_login,"</a>",  " submitted to <a href='https://www.inaturalist.org/'>iNaturalist.org</a>"))
}


species <- c(
    bird_voiced_treefrog="Hyla avivoca",
    copes_gray_treefrog="Hyla chrysoscelis",
    green_treefrog="Hyla cinerea",
    barking_treefrog="Hyla gratiosa",
    gray_treefrog="Hyla versicolor",
    mountain_chorus_frog="Pseudacris brachyphona",
    upland_chorus_frog="Pseudacris feriarum"
)

#species <- species[1:3]

#species <- trimws(unlist(strsplit(input$speciesvector,",")))
image_list <- list()
for (i in seq_along(species)) {
    obs <- rinat::get_inat_obs(taxon_name=unname(species[i]))
    obs <- obs[nchar(obs$image_url)>5,]
    obs <- subset(obs, quality_grade=="research")
    image_list[[i]] <- obs
}

shinyApp(ui = ui, server = server)
