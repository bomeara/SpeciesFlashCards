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

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("Species Flash Cards"),
    
    HTML("Enter the species you want below, separated by commas. You can use common names, scientific names, or a mixture. Then click the 'Store image locations' button. It will take some time to get these locations: you should see a progress bar pop up. After that, 'New image' will give you an image to identify; to see if you are right, you can mouseover the image to see the species and common name. <br /><br />This is made possible due to <a href='https://www.inaturalist.org/'>iNaturalist.org</a> and all their users who make observations and help identify species, but it is no way affiliated with them; if this app is having issues, please contact <a href='mailto:omeara.brian@gmail.com'>Brian O'Meara</a>, not the good iNaturalist folks. You can see the source code to make this <a href='https://github.com/bomeara/SpeciesFlashCards'>here</a>, with most of this in <a href='https://github.com/bomeara/SpeciesFlashCards/blob/master/SpeciesFlashCards/app.R'>this file</a>. It uses the rinat library (from rOpenSci) and the shiny library, as well as iNaturalist's APIs.<br /><br />"),
    mainPanel(
        textAreaInput(inputId="speciesArea", label="Enter species, separating each by a comma.", value="Leopard frog, Hyla avivoca, Hyla chrysoscelis, Hyla gratiosa", width = "1000px"),
        actionButton("queryButton", "Store image locations"),
        actionButton("goButton", "New image!"),
        htmlOutput("picture"),
        htmlOutput("credit"),
        htmlOutput("loaded")
        #         textInput('speciesvector', 'Enter a vector (comma delimited) of species you want to learn', "Hyla avivoca,Hyla chrysoscelis,Hyla cinerea,Hyla gratiosa,Hyla versicolor,Pseudacris brachyphona,Pseudacris feriarum")
    )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    randspecies <- eventReactive(input$goButton,{
        i = sample(length(image_list),1)
        local_obs <- image_list[[i]]
        
        return(local_obs[sample(sequence(nrow(local_obs)),1),])
    })
  
    observeEvent(input$queryButton, {
        species <-trimws(unlist(strsplit(input$speciesArea,",")))
        bad_species <- c()
        #image_list <- list()
        withProgress(message = 'Getting picture URLs', value = 0, {
        for (i in seq_along(species)) {
            output$loaded <- renderPrint({paste0("Now looking up ", species[i])})
            obs <- data.frame(matrix(nrow=0, ncol=1))
            try(obs <- rinat::get_inat_obs(query=unname(species[i]), maxresults=50), silent=TRUE)
            if(nrow(obs)>0) {
                obs <- obs[nchar(obs$image_url)>5,]
                obs <- subset(obs, quality_grade=="research")
            }
            if(nrow(obs)>0) {
                image_list[[i]] <<- obs
            } else {
                bad_species <- c(bad_species, i)
            }
            incProgress(1/length(species), detail = paste(": ", species[i]))
        }
            if(length(bad_species)>0) {
                image_list <<- image_list[-bad_species]
            }
        })
        output$loaded <- renderText({paste("Loaded addresses of info for", length(image_list), "species", ifelse(length(bad_species)>0, paste(" but failed for", paste(species[bad_species], collapse=", ")),""))})
    })
    
    observeEvent(input$goButton, {
        #output$loaded <- renderText({paste("image_list", length(image_list))})
        output$picture<-renderText({c('<img src="',randspecies()$image_url,'" title="', paste0(randspecies()$common_name, ' (', randspecies()$scientific_name, ')'),'">')})
        output$credit<-renderText(paste0("Photo by user <a href='https://www.inaturalist.org/people/", randspecies()$user_login,"'>",randspecies()$user_login,"</a>",  " submitted to <a href='https://www.inaturalist.org/'>iNaturalist.org</a>"))
    })
}

image_list <- list()

# Run the application 
shinyApp(ui = ui, server = server)
