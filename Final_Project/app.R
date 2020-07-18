#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(shinythemes)
library(shiny)
library(baguette)


data <- read.csv('~/Documents/Master_4Sem/AARC/Project/winequality-red.csv', sep=";" )
data_model <- data[, c("fixed.acidity", "citric.acid",'density','pH')]


cors <- function(df) { 
    M <- Hmisc::rcorr(as.matrix(df))
    Mdf <- map(M, ~data.frame(.x))
}

formatted_cors <- function(df){
    cors(df) %>%
        map(~rownames_to_column(.x, var="measure1")) %>%
        map(~pivot_longer(.x, -measure1, "measure2")) %>% 
        bind_rows(.id = "id") %>%
        pivot_wider(names_from = id, values_from = value) %>%
        mutate(sig_p = ifelse(P < .05, T, F), p_if_sig = ifelse(P <.05, P, NA), r_if_sig = ifelse(P <.05, r, NA)) 
}

tree_opt <- function(data, splt, y, engine2){
    # engine 
    if (engine2 == 'classification') {
        engine <- c('C5.0','classification')
    } else {
        engine <- c('MARS', 'regression')
    }
    # formula 
    form <- reformulate(termlabels = c('.'), response = c(y))
    # training split
    train <- sample(1:nrow(data), nrow(data)*splt)
    # Bagger
    set.seed(20)
    ctrl <- control_bag(var_imp = TRUE)
    bagg <- bagger(form, data = data[train,], base_model = engine[1])
    # boosted tree model
    set.seed(20)
    bt_model <-
        boost_tree(
            learn_rate = 0.3,
            trees = 100,
            tree_depth = 6,
            min_n = 1,
            sample_size = 1,
            mode = engine[2]
        ) %>% set_engine("xgboost", verbose = 2) %>%  fit(form, data = data[train,])
    # randomForest
    set.seed(20)
    rf_model <-
        rand_forest(trees = 100, mtry = 5, mode = engine[2]) %>% set_engine("randomForest",
                                                                             # importance = T to have permutation score calculated
                                                                             importance = T,
                                                                             # localImp=T for randomForestExplainer(next post)
                                                                             localImp = T,) %>% fit(form, data = data[train,])
    # predictions 
    info.bag <- predict(bagg, data[-train,])
    info.bt <- predict(bt_model, data[-train,])
    info.rf <- predict(rf_model, data[-train,])
    # Metrics
    if (engine2 == 'classification'){
        acc.bag <- confusionMatrix(info.bag$.pred_class, (data[-train,]$grade))
        acc.bt <- confusionMatrix(info.bt$.pred_class, (data[-train,]$grade))
        acc.rf <- confusionMatrix(info.rf$.pred_class, (data[-train,]$grade))
        accuracy <- c(acc.bag$overall[1], acc.bt$overall[1], acc.rf$overall[1])
    } else {
        acc.bag <- sd(info.bag$.pred - data[-train,]$fixed.acidity)
        acc.bt <- sd(info.bt$.pred - data[-train,]$fixed.acidity)
        acc.rf <- sd(info.rf$.pred - data[-train,]$fixed.acidity)
        accuracy <- c(acc.bag, acc.bt, acc.rf)
    }
    
    # Result

    model <- c('Bagger', 'Boosted', 'RandomForest')
    data.frame(model, accuracy)
}



# Bagging MODEL

ctrl <- control_bag(var_imp = TRUE)
set.seed(7687)
wine_bag <- bagger(fixed.acidity ~ ., data = data_model, base_model = "MARS", times = 5,
                   control = ctrl)
predictions <- predict(wine_bag, new_data = data_model)
data_model$predictions <- predictions$.pred



# Define UI for application that draws a histogram
ui <- fluidPage(
    # shinythemes::themeSelector(),
    theme = shinytheme('slate'),
    titlePanel('Final Project | Wine Quality'),
    br(),
    sidebarLayout(
        sidebarPanel(p('The data was provided from UCI Machine Learning Repository.
        The dataset is related to red variants of the Portuguese "Vinho Verde" wine.
        Due to privacy and logistic issues, only physicochemical (inputs) and sensory (the output) variables are available.'),
                     p('These datasets can be viewed as classification or regression tasks.'),
                     br(),
                     h5('Analytics based on Classification & Regression'),
                     img(src= 'https://www.thespanishhamper.co.uk/wp-content/uploads/2018/03/Red-Rioja-Bujanda.jpg',heigth = 400, width = 200 ),
                     br(),
                     br(),
                     code('@by Victoria López')
        ),
        mainPanel(
            tabsetPanel(
                tabPanel('Project Goal',
                         br(),
                         br(),
                         h4('Kaggle dataset implementation for prediction:'),
                         p(strong('Model Selected'),
                           'How frequently a word occurs in a document.'),
                         p(strong('Prediction:')),
                         'In this project will be predicting the fixed acidity in red wine,
                         base on citric acid, Ph and density characteristics.',
                         p('The inverse document frequency for any given term
                            is defined as:'),
                         br(),
                         htmlOutput("link"),
                ),
                tabPanel('Data',
                         h4('Data Description'),
                         img(src= 'summary.png', heigth = 300, width = 550),
                         br(),
                         br(),
                         fluidRow(
                             column(7,
                                    h4('Table'),
                                    tableOutput('Table')),
                            column(3,
                                   br(),
                                   p(h5('Measure Units:'), strong('Fixed acidity:'),
                                     '(tartaric acid - g / dm^3).', br(),
                                     strong('Citric acid:'), '(g / dm^3).', br(),
                                     strong('Density:'), '(g / cm^3).',br(),
                                     strong('PH:'), 'most wines are between 3-4 on the pH scale.')),
                         ),
                         h4('Variables Correlation'),
                         plotOutput('plot_tf')
                         
                ),
                tabPanel('Modeling',
                         h4('Bagging (regression)'),
                         p('Stands for bootstrap aggregating. It consists of building multiple different
                           decisin tree models from a single training data set by repeatedly using multiple
                           bootstraped subsets of the data and averaging the models. Here, each tree is build
                           independently to the others.'),
                         br(),
                         column(7,
                             h4('Tree Graphic Example'),
                             img(src= 'tree.png', heigth = 200, width = 400),
                             br()),
                         column(3,
                                h4('Table Comparison'),
                                tableOutput('Table_comparison'))
                         ),
                
                #Actividad
                tabPanel("Results",
                         tableOutput("table_pred"),
                         br(),
                         h4('Visual Prediction Fit'),
                         plotOutput('plot_pred')
                         
                )
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
 
    output$Table <- renderTable({
        head(data[, c("fixed.acidity", "citric.acid",'density','pH')])
    })
    
    output$plot_tf <- renderPlot({
        formatted_cors(data[, c("fixed.acidity", "citric.acid",'density','pH')]) %>%
            ggplot(aes(measure1, measure2, fill=r, label=round(r_if_sig,2))) +
            geom_tile() +
            labs(x = NULL, y = NULL, fill = "Pearson's\nCorrelation", title="Correlations in Wine data", subtitle="Only significant Pearson's correlation coefficients shown") + 
            scale_fill_gradient2(mid="#FBFEF9",low="#0C6291",high="#A63446", limits=c(-1,1)) +
            geom_text() +
            theme_classic() +
            scale_x_discrete(expand=c(0,0)) +
            scale_y_discrete(expand=c(0,0)) 
    })
    
    output$link <- renderUI({
        my_test <- tags$iframe(src='https://www.vinhoverde.pt/en/statistics', height=400, width=700)
        print(my_test)
    })
    
    output$Table_comparison <- renderTable({
        tree_opt(data_model, .8, 'fixed.acidity', 'regression')
    })
    
    output$plot_pred <- renderPlot({
        ggplot(data_model, aes(x = fixed.acidity, y = predictions, color= predictions)) +
            geom_point() +
            scale_color_gradient2(low = '#75180C', high = '#75180C' , mid ='#C22813' ) +
            geom_line(aes(x = fixed.acidity, y = fixed.acidity))
            
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
