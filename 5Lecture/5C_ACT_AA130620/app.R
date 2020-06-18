#
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

options(shiny.autoreload = TRUE)

glimpse(mpg)

# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/


compare_mean <- mpg %>%
    group_by(cyl) %>%
    summarise(Mean = mean(cty))
# 
# compare_mode <- mpg %>%
#     group_by(cyl) %>%
#     summarise(mode = mode(cty))


compare_median <- mpg %>%
    group_by(cyl) %>%
    summarise(Median = median(cty))


statistic_measures <- list('mean' = geom_vline(data = compare_mean, aes(xintercept = Mean, color = factor(cyl)),
                                               linetype = 3, size = 0.75),  
                           'median'= geom_vline(data = compare_median, aes(xintercept = Median, color = factor(cyl)),
                                                                                                 linetype = 3, size = 0.75))

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
            selectInput("theme", label = h4("Select theme for plot"), choices = names(themes)),
            selectInput("measures", label = h4("Select statistic measure for plot"), choices = names(statistic_measures))
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
        p1 <- g <- ggplot(mpg, aes(cty))+
            geom_density(aes(fill=factor(cyl)), alpha=0.35) + 
            labs(title="Density plot", 
                 subtitle="City Mileage Grouped by Number of cylinders",
                 caption="Source: mpg",
                 x="City Mileage",
                 fill="# Cylinders") +
            statistic_measures[[input$measures]]
        p1
        # p2 <- ggplot(house_prices, aes(x = log10_price)) + 
        #     geom_histogram(bins = input$price_bins, color = colorline, fill = colorfill) +
        #     labs(x = "log10 price", title = "House price") 
        # 
        # p1 + p2
    })
}



# Run the application 
shinyApp(ui = ui, server = server)