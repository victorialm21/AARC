

# library(shiny)
# 
# # Define UI for application that draws a histogram
# ui <- fluidPage(
#         )
# 
# # Define server logic required to draw a histogram
# server <- function(input, output) {
# 
# }
# 
# # Run the application 
# shinyApp(ui = ui, server = server)


library(shiny)
library(colourpicker)

options(shiny.autoreload = TRUE)
# Define UI for application that draws a histogram
# ui <- fluidPage(
#     titlePanel('titulo del panel'),
#     sidebarLayout(position = 'right',
#         sidebarPanel('sidebar panel'),
#         mainPanel(
#             h1('Primer nivel', align= 'center'),
#             h2('segundo nivel'),
#             h3('tercer nivel'),
#             p('Este funciona para hacer nuevos parrafos...', 
#               strong('strong funciona para hacer negrita la letra'),
#               em('em() crea texto en italica'),
#               code('xalala')),
#             p('texto en color', style='color:red')
#         )
#     )
# )
# 
# # Define server logic required to draw a histogram
# server <- function(input, output) {
#     
# }
# 
# # Run the application 
# shinyApp(ui = ui, server = server)




# ui <- fluidPage(
#     titlePanel('titulo del panel'),
#     sidebarLayout(position = 'right',
#                   sidebarPanel('sidebar panel'),
#                   mainPanel(
#                       img(src='tidyverse.jpg')
#                   )
#     )
# )
# 
# # Define server logic required to draw a histogram
# server <- function(input, output) {
#     
# }

# Run the application 
# shinyApp(ui = ui, server = server)

## ACTIVIDAD 1 CLASE 5


# # Define UI for application that draws a histogram
# ui <- fluidPage(
#     titlePanel('Victoria López'),
#     sidebarLayout(position = 'left',
#         sidebarPanel(
#             h2('Data Scientist | DS'),
#             code('victoria.ds@xalala.com'),
#             img(src='tidyverse.png', height='60%', width='60%'),
#             br(),
#             br(),
#             p('Shiny Activity')
#             ),
#             
#             
#         mainPanel(
#             position= 'right',
#             h2('Work Experience', align= 'left'),
#             h4( 'BSC 1 | 2015'),
#             p(strong('Data Analyst'),
#               'interprets data and turns it into information which can offer 
#               ways to improve a business, thus affecting business decisions. 
#               Data Analysts gather information from various sources and
#               interpret patterns and trends',
#               # em('em() crea texto en italica'),
#             br(),
#             h2('Other Experience', align= 'left'),
#             p('A data scientist is someone who makes value out of data. 
#               Such a person proactively fetches information from various sources 
#               and analyzes it for better understanding about how the business performs, 
#               and builds AI tools that automate certain processes within the company.')
#             )
#         #     p('texto en color', style='color:red'),
#         # p()
#         )
#     )
# )
# 
# # Define server logic required to draw a histogram
# server <- function(input, output) {
# 
# }
# 
# # Run the application
# shinyApp(ui = ui, server = server)

library(shiny)
library(colourpicker)
library(tidyverse)
library(moderndive)
library(colourpicker)
library(patchwork)


## APP CON PLOTS

# Define UI for application that draws a histogram
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



library(shiny)
library(tidyverse)
library(moderndive)
library(colourpicker)
library(patchwork)



house_prices <- house_prices %>% mutate(
    log10_price = log10(price), log10_size = log10(sqft_living) )


# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/


house_prices <- house_prices %>% mutate(
    log10_price = log10(price), log10_size = log10(sqft_living) )



themes <- list("grey" = theme_grey(), "bw" = theme_bw(), "Light" = theme_light(),
               "Minimal" = theme_minimal(),
               "dark" = theme_dark())
# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Seattle house prices"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 20), 
            
            sliderInput("price_bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 20), 
            
            colourInput("color", "Select Fill colour", value = "orange"),
            colourInput("colorLine", "Select Line colour", value = "white"),
            selectInput("theme", label = h4("Select theme for plot"), choices = names(themes))
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot")
        )
    )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        bins <- input$bins
        colorfill <- input$color
        colorline <- input$colorLine
        # draw the histogram with the specified number of bins
        p1 <- ggplot(house_prices, aes(x = log10_size)) + 
            geom_histogram(bins = bins, color = colorline, fill = colorfill) +
            labs(x = "log10 living space (square feet)", title = "House size") + themes[[input$theme]]
        
        p2 <- ggplot(house_prices, aes(x = log10_price)) + 
            geom_histogram(bins = input$price_bins, color = colorline, fill = colorfill) +
            labs(x = "log10 price", title = "House price") 
        
        p1 + p2
    })
}



# Run the application 
shinyApp(ui = ui, server = server)


