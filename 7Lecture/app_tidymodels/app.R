
# https://www.tidymodels.org/
#https://shiny.rstudio.com/images/shiny-cheatsheet.pdf
library(shiny)
library(tidymodels)
library(shinythemes)
library(ggplot2)
library(tidyr)

options(shiny.autoreload = TRUE)

d_broom <- "broom summarizes key information about models in tidy tibble()s. 
broom provides three verbs to make it convenient to 
interact with model objects:
- tidy() summarizes information about model components
- glance() reports information about the entire model
- augment() adds informations about observations to a dataset
For a detailed introduction, please see vignette(\"broom\").

broom tidies 100+ models from popular modelling packages and almost all of the model
objects in the stats package that comes with base R. vignette(\"available-methods\")
lists method availability.

If you aren’t familiar with tidy data structures and want to know how they
can make your life easier, we highly recommend reading Hadley Wickham’s Tidy Data.
"

d_rsample <- 'rsample contains a set of functions to create different types of resamples and 
corresponding classes for their analysis. The goal is to have a modular set of methods that 
can be used across different R packages for:
traditional resampling techniques for estimating the sampling distribution of a statistic and
estimating model performance using a holdout set
The scope of rsample is to provide the basic building blocks for creating and analyzing resamples
of a data set but does not include code for modeling or calculating statistics. The “Working with
Resample Sets” vignette gives demonstrations of how rsample tools can be used.'




# images <- list('rsample'='https://github.com/rstudio/hex-stickers/blob/master/PNG/rsample.png?raw=true', 'parsnip'=, 'recipes'=,'workflows'=,'tune'=,'yardstick'=,
#                'broom'=,'dials'=)

overviews_keys <- c("broom",'rsample')
overviews_descriptions <- c(d_broom, d_rsample)
names(overviews_descriptions) <- overviews_keys

# Define UI for application
ui <- fluidPage(
    navbarPage(
        theme = shinytheme('slate'),  # <--- To use a theme, uncomment this
        #p("Tidymodels", style="color:firebrick"),
        a("Tidymodels", style="color:firebrick", href="https://www.tidymodels.org/"),
        tabPanel("Installation",
                 fluidRow(
                     column(5, img(src = "tidymodels.png", hight=300, width = 300)),
                     column(3,
                            h3("TIDYMODELS"), br(),
                            p("The tidymodels framework is a collection of packages for modeling 
                            and machine learning using", strong("tidyverse"),"principles."), 
                            p("Install tidymodels with:"),
                            br(),br(), code("install.packages(\"tidymodels\")"), br(),br(),
                            p("Run", em("library(tidymodels)"), "to load the core packages and make 
                              them available in your current R session"))
                 ),
                 br(),
                 br(),
                 code('by @VictoriaLM')
                ),
        tabPanel("Packages", 
                 h3("CORE TIDYMODELS"),
                 uiOutput(outputId = 'logo'),
                 p("The core tidymodels packages work together to enable a wide variety of modeling approaches."),
                 selectInput("state", "Choose a tidymodel library:",
                             list('rsample', 'parsnip', 'recipes','workflows','tune','yardstick',
                                                'broom','dials')
                             # list('rsample'='https://raw.githubusercontent.com/rstudio/hex-stickers/master/SVG/rsample.svg', 
                             #      'parsnip'='https://raw.githubusercontent.com/rstudio/hex-stickers/master/SVG/parsnip.svg', 
                             #      'recipes'='https://raw.githubusercontent.com/rstudio/hex-stickers/master/SVG/recipes.svg',
                             #      'workflows'= 'https://raw.githubusercontent.com/rstudio/hex-stickers/master/SVG/rsample.svg', 
                             #      'tune','yardstick'='https://raw.githubusercontent.com/rstudio/hex-stickers/master/SVG/rsample.svg', 
                             #      'broom'='https://raw.githubusercontent.com/rstudio/hex-stickers/master/SVG/rsample.svg',
                             #      'dials'= 'https://raw.githubusercontent.com/rstudio/hex-stickers/master/SVG/rsample.svg')
                             # list(`package` = tidymodels_packages()) # hacer una lista limpia con los paquetes principales
                             ),
                 # verbatimTextOutput("result")
                 htmlOutput("descr")
                 
                 ),
        tabPanel("Learn", "This panel is intentionally left blank",
                     tabsetPanel(
                         tabPanel("PERFORM STATISTICAL ANALYSIS",
                                  tabsetPanel(
                                      tabPanel('Correlation & regression',
                                               h5('LEARNING OBJECTIVE'),
                                               br(),
                                               p(em('Analyze the results of correlation tests and simple regression models
                                                    for many data sets at once.')),
                                               br(),
                                               h4('INTRODUCTION'),
                                               br(),
                                               p('To use the code in this article, you will need to install the following
                                                 packages: tidymodels and tidyr.'),
                                               br(),
                                               p('While the tidymodels package',code('broom '),'is useful for summarizing the result
                                                 of a single analysis in a consistent format, it is really designed for high-throughput
                                                 applications, where you must combine results from multiple analyses. These could be 
                                                 subgroups of data, analyses using different models, bootstrap replicates, permutations,
                                                 and so on. In particular, it plays well with the `nest()/unnest()` functions from', code('tidyr'), 'and
                                                 the map() function in', code('purrr'),'.'), 
                                               h4('CORRELATION ANALYSIS'),
                                               br(),
                                               p('Let’s demonstrate this with a simple data set, the built-in', strong('Orange'), '. We start by
                                                 coercing', strong('Orange'), 'to a tibble. This gives a nicer print method that will be especially
                                                 useful later on when we start working with list-columns.'),
                                               sidebarPanel(width=12,
                                                   p('library(tidymodels)
                                                   data(Orange)
                                                    Orange <- as_tibble(Orange)
                                                    Orange
                                                    #> # A tibble: 35 x 3
                                                    #>    Tree    age circumference
                                                    #>    <ord> <dbl>         <dbl>
                                                    #>  1 1       118            30
                                                    #>  2 1       484            58
                                                    #>  3 1       664            87
                                                    #>  4 1      1004           115
                                                    #>  5 1      1231           120
                                                    #>  6 1      1372           142
                                                    #>  7 1      1582           145
                                                    #>  8 2       118            33
                                                    #>  9 2       484            69
                                                    #> 10 2       664           111
                                                    #> # … with 25 more rows')
                                               ),
                                               plotOutput("linear_plot")
                                             
                                               
                                      ),
                                      tabPanel('K-means clustering',
                                               h5('LEARNING OBJECTIVE'),
                                               br(),
                                               p(em('Summarize clustering characteristics and estimate the best number
                                               of clusters for a data set.')),
                                               br(),
                                               h4('INTRODUCTION'),
                                               br(),
                                               p('To use the code in this article, you will need to install the following
                                                 packages: tidymodels and tidyr.'),
                                               br(),
                                               p('K-means clustering serves as a useful example of applying tidy data 
                                                 principles to statistical analysis, and especially the distinction between
                                                 the three tidying functions:'),
                                               uiOutput("Klist"),
                                               p('Let’s start by generating some random two-dimensional data with three clusters.
                                                 Data in each cluster will come from a multivariate gaussian distribution, with
                                                 different means for each cluster:'),
                                               sidebarPanel(width=12,
                                                            p('library(tidymodels)', br(),
                                                                    'library(tidyr)',br(),
                                                                    'set.seed(27)',br(),
                                                                    'centers <- tibble(
                                                                      cluster = factor(1:3), 
                                                                      num_points = c(100, 150, 50),  # number points in each cluster
                                                                      x1 = c(5, 0, -3),              # x1 coordinate of cluster center
                                                                      x2 = c(-1, 1, -2)              # x2 coordinate of cluster center
                                                                    )',
                                                                    br(),
                                                                    br(),
                                                                    'labelled_points <- 
                                                                      centers %>%
                                                                      mutate(
                                                                        x1 = map2(num_points, x1, rnorm),
                                                                        x2 = map2(num_points, x2, rnorm)
                                                                      ) %>% 
                                                                      select(-num_points) %>% 
                                                                      unnest(cols = c(x1, x2))',br(),
                                                                    
                                                                    'ggplot(labelled_points, aes(x1, x2, color = cluster)) +
                                                                      geom_point(alpha = 0.3)')
                                               ),
                                               plotOutput("kplot")
                                               
                                               
                                               
                                      ),
                                      tabPanel('Bootstrap resampling',
                                               
                                               
                                      ),
                                      tabPanel(' Hypothesis testing'
                                               
                                      ),
                                      tabPanel('Statistical analysis'
                                               
                                      )
                                      
                                  )
                         ),
                         tabPanel("CREATE ROBUST MODELS",
                                  tabsetPanel(
                                      tabPanel(' Regression models'
                                          
                                      ),
                                      tabPanel(' Classification models | NN'
                                               
                                      ),
                                      tabPanel('Subsampling'
                                               
                                      ),
                                      tabPanel('Modeling time series'
                                               
                                      ),
                                      tabPanel('Multivariate analysis'
                                               
                                      )
                                      
                                  )
                                  ),
                         tabPanel("TUNE, COMPARE, AND WORK WITH YOUR MODELS", "This panel is intentionally left blank",
                                  tabsetPanel(
                                      tabPanel('Model tuning'
                                               
                                      ),
                                      tabPanel('Nested resampling'
                                               
                                      ),
                                      tabPanel('Iterative Bayesian optimization'
                                               
                                      )
                                  )
                                  ),
                         tabPanel('DEVELOP CUSTOM MODELING TOOLS',
                                  tabsetPanel(
                                      tabPanel('Create your own recipe'
                                               
                                      ),
                                      tabPanel('How to build a parsnip model'
                                               
                                      ),
                                      tabPanel('Custom performance metrics'
                                               
                                      ),
                                      tabPanel('Tuning parameter function'
                                               
                                      )
                                      
                                  )
                                  )
                     )
                 
                 )
    )

)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    output$txtout <- renderText({
        paste(input$txt, input$slider, format(input$date), sep = ", ")
    })
    output$table <- renderTable({
        head(cars, 4)
    })
    output$result <- renderText({
        paste(overviews_descriptions[input$state])
        #HTML(overviews_descriptions[input$state])
    })
    
    output$logo <- renderUI({
        tags$img(src=paste0("./logos/", input$state, ".png"), hight=100, width=100)
        # tags$img(src=names(input$state), hight=100, width=100)
    })
    
    output$descr <- renderUI({
        my_test <- tags$iframe(src=paste0("https://", input$state,".tidymodels.org"), height=600, width=1500)
        print(my_test)
    })
    
    data(Orange)
    Orange <- as_tibble(Orange)

    output$linear_plot <- renderPlot({
        ggplot(Orange, aes(age, circumference, color = Tree)) +
            geom_line()
    })
    
    output$Klist <- renderUI(HTML("<ul><li>tidy()</li><li>augment()</li><li>glance()</li></ul>"))
    
    
    set.seed(27)
    
    centers <- tibble(
        cluster = factor(1:3), 
        num_points = c(100, 150, 50),  # number points in each cluster
        x1 = c(5, 0, -3),              # x1 coordinate of cluster center
        x2 = c(-1, 1, -2)              # x2 coordinate of cluster center
    )
    
    labelled_points <- 
        centers %>%
        mutate(
            x1 = map2(num_points, x1, rnorm),
            x2 = map2(num_points, x2, rnorm)
        ) %>% 
        select(-num_points) %>% 
        unnest(cols = c(x1, x2))
    
    
    output$kplot <- renderPlot({
        ggplot(labelled_points, aes(x1, x2, color = cluster)) +
            geom_point(alpha = 0.3)
    })
    
    
}

# Run the application 
shinyApp(ui, server)
