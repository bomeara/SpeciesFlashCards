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
library(jsonlite)

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("Species Flash Cards"),

    mainPanel(
        htmlOutput("intro"),
        textAreaInput(inputId="speciesArea", label="Enter multiple species (scientific and/or common names), separating each by a comma, OR enter a single URL. Then click Store image locations to get ready, then New image to try one.", value="Ipomoea purpurea, Chamaecrista nictitans, Desmodium paniculatum, Ipomoea coccinea, Toxicodendron radicans, Parthenocissus quinquefolia, Oxalis stricta, Trifolium pratense, Oenothera biennis, Fagopyrum esculentum, Clematis terniflora, Ipomoea pandurata, Impatiens capensis, Lobelia cardinalis,  Persicaria punctata, Cuscuta gronovii, Amphicarpaea bracteata, Agrimonia parviflora, Lycopus rubellus, Lespedeza cuneata, Acalypha rhomboidea, Phyla lanceolata, Daucus carota, Fallopia japonica, Rudbeckia fulgida, Helenium autumnale, Agalinas purpurea, Typha latifolia, Spiranthes cernua, Oxypolis rigidior, Iva annua, Chamaecrista fasciculata", width = "500px"),
        actionButton("queryButton", "Store image locations"),
        HTML("<br /><hr /><br />"),
        actionButton("goButton", "New image!"),
        htmlOutput("picture"),
        htmlOutput("credit"),
        htmlOutput("loaded"),
        HTML("<br /><br />This is made possible due to <a href='https://www.inaturalist.org/'>iNaturalist.org</a> and all their users who make observations and help identify species, but it is no way affiliated with them; if this app is having issues, please contact <a href='mailto:omeara.brian@gmail.com'>Brian O'Meara</a>, not the good iNaturalist folks. You can see the source code to make this <a href='https://github.com/bomeara/SpeciesFlashCards'>here</a>, with most of this in <a href='https://github.com/bomeara/SpeciesFlashCards/blob/master/SpeciesFlashCards/app.R'>this file</a>. This depends on the rinat (Barve and Hart, 2017), shiny (<a href='https://CRAN.R-project.org/package=shiny'>Chang, Cheng, Allaire,
  Xie and McPherson, 2020</a>), and jsonlite (<a href='https://arxiv.org/abs/1403.2805'>Ooms, 2014</a>) packages in R. Scientific name extraction from web pages is provided by <a href='http://globalnames.org/'>Global Names</a> using their <a href='https://gnrd.globalnames.org/api'>API</a> to their <a href='https://github.com/gnames/gnfinder'>gnfinder</a> tool. The first draft of this was done to help my daughter study for a species identification quiz in middle school.")
        #         textInput('speciesvector', 'Enter a vector (comma delimited) of species you want to learn', "Hyla avivoca,Hyla chrysoscelis,Hyla cinerea,Hyla gratiosa,Hyla versicolor,Pseudacris brachyphona,Pseudacris feriarum")
    )

)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$intro <- renderUI({HTML("Enter the species you want below, separated by commas. An example is <code>Leopard frog, Hyla avivoca, Hyla chrysoscelis, Hyla gratiosa</code>. You can use common names, scientific names, or a mixture. Or you can enter a single URL, such as <code><a href='https://www.nps.gov/grsm/learn/nature/fish-checklist.htm'>https://www.nps.gov/grsm/learn/nature/fish-checklist.htm</a></code>, and it will attempt to pull all species names (scientific names only) from that page. Then click the 'Store image locations' button. It will take some time to get these locations: you should see a progress bar pop up. After that, 'New image' will give you an image to identify; to see if you are right, you can mouseover the image to see the species and common name. <br /><br />")})
    if(length(image_list)!=0)  {
      i = sample(length(image_list),1)
      local_obs <- image_list[[i]]
      to_show <- local_obs[sample(sequence(nrow(local_obs)),1),]
      
      output$picture <- renderText({c('<img src="',to_show$image_url,'" title="', paste0(to_show$common_name, ' (', to_show$scientific_name, ')'),'">')})
      output$credit <- renderText(paste0("Photo by user <a href='https://www.inaturalist.org/people/", to_show$user_login,"'>",to_show$user_login,"</a>",  " submitted to <a href='https://www.inaturalist.org/'>iNaturalist.org</a>"))
      
    }

    randspecies <- eventReactive(input$goButton,{
        i = sample(length(image_list),1)
        local_obs <- image_list[[i]]

        return(local_obs[sample(sequence(nrow(local_obs)),1),])
    })

    urltospecies <- function(url) {
        mydata <- jsonlite::stream_in(url(paste0("https://gnrd.globalnames.org/name_finder.json?url=", url)))
        myspecies <- mydata$names[[1]]$scientificName
        countSpaces <- function(s) { sapply(gregexpr(" ", s), function(p) { sum(p>=0) } ) }
        myspecies <- myspecies[which(countSpaces(myspecies)==1)]
        myspecies <- unique(myspecies)
        return(myspecies)
    }

    observeEvent(input$queryButton, {
        species <-trimws(unlist(strsplit(input$speciesArea,",")))
        if(length(species)==1) { # must be a URL
            species <- urltospecies(trimws(input$speciesArea))
        }
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
        output$intro <- renderText({""})
        
        # now render the first one
        i = sample(length(image_list),1)
        local_obs <- image_list[[i]]
        to_show <- local_obs[sample(sequence(nrow(local_obs)),1),]
        
        output$picture <- renderText({c('<img src="',to_show$image_url,'" title="', paste0(to_show$common_name, ' (', to_show$scientific_name, ')'),'">')})
        output$credit <- renderText(paste0("Photo by user <a href='https://www.inaturalist.org/people/", to_show$user_login,"'>",to_show$user_login,"</a>",  " submitted to <a href='https://www.inaturalist.org/'>iNaturalist.org</a>"))
        
    })

    observeEvent(input$goButton, {
        output$loaded <- renderText({""})
        output$picture<-renderText({c('<img src="',randspecies()$image_url,'" title="', paste0(randspecies()$common_name, ' (', randspecies()$scientific_name, ')'),'">')})
        output$credit<-renderText(paste0("Photo by user <a href='https://www.inaturalist.org/people/", randspecies()$user_login,"'>",randspecies()$user_login,"</a>",  " submitted to <a href='https://www.inaturalist.org/'>iNaturalist.org</a>"))
    })
}

image_list <- list()
try(load(file="fieldbotany.rda"))

# Run the application
shinyApp(ui = ui, server = server)
