#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(janeaustenr)
library(tidytext)
library(tidyverse)
library(shinythemes)
library(shiny)

book_words <- austen_books() %>%
    unnest_tokens(word, text) %>%
    count(book, word, sort = TRUE)

total_words <- book_words %>% 
    group_by(book) %>% 
    summarize(total = sum(n))

book_words <- left_join(book_words, total_words)

freq_by_rank <- book_words %>% 
    group_by(book) %>% 
    mutate(rank = row_number(), 
           `term frequency` = n/total)

freq_by_rank


# Define UI for application that draws a histogram
ui <- fluidPage(
    # shinythemes::themeSelector(),
    theme = shinytheme('slate'),
    titlePanel('Analyzing word and document frequency'),
    sidebarLayout(
        sidebarPanel(p('A central question in text mining and NLP is how
                     to quantify what a document is about'),
                     p('Here, as an a example we will be considering'),
                     h3('Jane Austen novels'),
                     img(src= 'https://ep01.epimg.net/cultura/imagenes/2017/07/14/babelia/1500041594_163366_1500042008_noticia_normal_recorte1.jpg',heigth = 200, width = 150 ),
                     br(),
                     br(),
                     code('@by Victoria')
        ),
        mainPanel(
            tabsetPanel(
                tabPanel('Goal',
                         br(),
                         br(),
                         h4('Some other options are:'),
                         p(strong('term frequency : '),
                           'How frequently a word occurs in a document.'),
                         p(strong('Inverse document frequency:')),
                         'Decreases the weight for commonly used words and
                            increases those not soo much used.',
                         p('The statistic ', em('tf-idf'), 'is an word 
                           importance measure'),
                         p('The inverse document frequency for any given term
                            is defined as:'),
                         withMathJax("$$ idf(\\text{term})=
                                     \\ln{\\left(\\frac{n_{\\text{documents}}}
                                     {n_{\\text{documents containing term}}}\\right)}  $$")
                ),
                tabPanel('Data',
                         h4('Summary'),
                         verbatimTextOutput('Summary'),
                         fluidRow(
                             column(7,
                                    h4('Table'),
                                    tableOutput('Table')),
                            column(3,
                                   br(),
                                   br(),
                                   br(),
                                   p(strong('n'), 'is the number of times that word is used in that book',
                                     strong('total'), 'is the total number ')),
                         )
                ),
                tabPanel('TF Plots',
                         plotOutput('plot_tf')
                         ),
                #Actividad
                tabPanel("Zipf's law",
                         tableOutput("table_rank"),
                         
                         fluidRow(
                             column(8,plotOutput("plot_rank"),
                                    p("Zipf’s law states that the frequency that a word
                                      appears is inversely proportional to its rank.")),
                             column(3,sliderInput("range_rank1",
                                                  "Rank's range:",
                                                  min = 1,
                                                  max = 10000,
                                                  value = c(10,1000)),
                                    withMathJax("$$\\text{frequency} \\propto \\frac{1}{\\text{rank}}$$")
                             )
                             
                         )
                         
                ),
                #Actividad
                tabPanel('tf_idf',
                         fluidRow(
                             column(8,
                                    plotOutput('tf_idf')
                                    ),
                             column(3,
                                    sliderInput(
                                        'nw',
                                        'Number of words',
                                        min =2,
                                        max=26,
                                        value=6
                                    )
                                )
                             )
                         )
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$Summary <- renderPrint({
        summary(austen_books())
    })
    output$Table <- renderTable({
        head(book_words,7)
    })
    
    output$plot_tf <- renderPlot({
        ggplot(book_words, aes(n/total, fill=book, alpha=0.85)) + 
            scale_fill_brewer(palette = 'Spectral') + 
            geom_histogram(show.legend = FALSE) +
            xlim(NA, 0.0009) +
            facet_wrap(~book, ncol=2, scales = 'free_y') +
            theme_minimal()
    })
    
    #Actividad
    output$table_rank <- renderTable({
        head(freq_by_rank,4)
    })
    
    rank_subset <- reactive({
        freq_by_rank %>%
            filter(rank < input$range_rank1[2],
                   rank > input$range_rank1[1])})
    
    output$plot_rank <- renderPlot({
        fitvals = lm(log10(`term frequency`) ~ log10(rank), data = rank_subset() )
        freq_by_rank %>%
            ggplot(aes(rank, `term frequency`, color = book)) +
            geom_line(size = 0.5, alpha = 0.8, show.legend = FALSE) +
            geom_abline(intercept = fitvals$coefficients[1],
                        slope = fitvals$coefficients[2], color = "gray50", linetype = 2) +
            geom_vline(xintercept = c(input$range_rank1[1],input$range_rank1[2]),
                       color = c("blue", "red")) +
            scale_x_log10() +
            scale_y_log10()
    })
    
    book_words <- book_words %>%
        bind_tf_idf(word, book, n)
    
    book_words %>%
        select(-total) %>%
        arrange(desc(tf_idf))
    
    
    output$tf_idf <- renderPlot({
        book_words %>%
            arrange(desc(tf_idf)) %>%
            mutate(word = factor(word, levels = rev(unique(word)))) %>% 
            group_by(book) %>% 
            top_n(input$nw) %>% 
            ungroup() %>% 
            ggplot(aes(word, tf_idf, fill = book)) + 
            scale_fill_brewer(palette = 'Set2')+
            geom_col(show.legend = FALSE) +
            labs(x = NULL, y = "tf-idf") +
            facet_wrap(~book, ncol = 2, scales = "free") +
            coord_flip() +
            theme_minimal()
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
