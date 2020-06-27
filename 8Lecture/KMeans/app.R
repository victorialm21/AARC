#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(tidyverse)
library(imager)




file <- "autumn_drawing_walking_82963_320x480.jpg"
im <- load.image(file)
bdf <- as.data.frame(im, wide="c")
set.seed(10)
kclust <- kmeans(bdf[c(3,4,5)], centers = 3)
show_col(rgb(kclust$centers), labels=FALSE, border = NA, ncol=1)


kmeans_colors <- function(im, centroides) {
    set.seed(20)
    tmp <- as.data.frame(im, wide="c")
    tmp <- kmeans(tmp[,c(3,4,5)], centers = centroides)
    return (rgb(tmp$centers))
}


# Define UI for application that draws a histogram
ui <- fluidPage(
    navbarPage(
        theme = shinytheme('slate'),  # <--- To use a theme, uncomment this
        #p("Tidymodels", style="color:firebrick"),
        a("NextJournal", style="color:grey", href="https://nextjournal.com/lazarus/extracting-dominant-colours-from-pictures"),
        tabPanel("KMeans - Dominant Colors",
                 fluidRow(
                     column(5, img(src = "https://nextjournal.com/data/QmcHxBuKUttKioaSDNZREyYRGtDbiRw6UEFwDERS6MtBAo?content-type=image/png", 
                                   hight=300, width = 300)),
                     column(3, 
                            plotOutput("distPlot")),
                     column(5,
                            h3("Extracting dominant colours from pictures"), br(),
                            p('We will use',strong('k-means'), 'in the', strong('RGB'), 'space  as a way  to 
                              find the most common colours in a picture. Clusters
                              with the largest amount of elements will correspond 
                              to the dominant colours. In Julia this is done as follows.'),
                            br(),
                            br(),
                            code('by @VictoriaLM')),
                            # br(),br(), code("install.packages(\"tidymodels\")"), br(),br(),
                            # p("Run", em("library(tidymodels)"), "to load the core packages and make 
                            #   them available in your current R session"))
                 )
                 
        ),
        tabPanel('Image Load',
                 # Copy the line below to make a text input box
                 column(6,
                        textInput(
                            inputId = 'image_url',
                            label = 'Enter Image URL',
                            value = "http://placehold.it/300x300"),
                        htmlOutput('image')),
                 column(6,
                        fileInput(
                            inputId = 'image_file',
                            label = 'Browse your image',
                            accept = c('image/png', 'image/jpeg','image/jpg')),
                        imageOutput('image_browse')
                       ),
                 plotOutput('kmean_plot')
        )

    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    tmp <- reactive({
        if (is.null(input$image_file$datapath)) {
            return (input$image_url)
        } else {
            return (gsub("\\\\", "/", input$image_file$datapath))
        }
    })
    
    
    bdf <- as.data.frame(im, wide="c")
    set.seed(10)
    kclust <- kmeans(bdf[c(3,4,5)], centers = 3)
    
    output$distPlot <- renderPlot({
        
        show_col(rgb(kclust$centers), labels=FALSE, border = NA, ncol=1)
        
    })
    
    output$image = renderUI({
        tags$img(src = input$image_url)  
    })
    
    output$image_browse = renderImage({
        list(src=gsub("\\\\", "/", input$image_file$datapath))
    }, deleteFile = FALSE)
    
    
    output$kmean_plot <- renderPlot({
        tmp=kmeans_colors(im=load.image(tmp()), 5)
        ggplot(as.data.frame(tmp), aes(x = tmp)) +
            geom_bar(size = 25,  fill = tmp)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
