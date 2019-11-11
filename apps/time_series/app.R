#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(readr)
library(ggplot2)
library(forcats)
library(stringr)

# Custom yardstick metric implementation of root mean squared log error (rmsle)
rmsle_vec <- function(truth, estimate, na_rm = TRUE, ...) {
    
    rmsle_impl <- function(truth, estimate) {
        sqrt(mean((log1p(truth) - log1p(estimate))^2))
    }
    
    yardstick::metric_vec_template(
        metric_impl = rmsle_impl,
        truth = truth, 
        estimate = estimate,
        na_rm = na_rm,
        cls = "numeric",
        ...
    )
}

test_predictions <- read_csv("test_predictions.csv")

dimensoes <- test_predictions %>% select(estado, regiao) %>% names()

rmsle_geral <- rmsle_vec(test_predictions$preco_medio_revenda, 
                         test_predictions$lag_preco_medio_revenda)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Root Mean Squared Log Error"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("modelo", 
                        "Modelo:", 
                        choices = c("Baseline", "Random Forest")),
            
            selectInput("variavel",
                        "Dimensão:",
                        choices = dimensoes)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("figplot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$figplot <- renderPlot({
            
        var <- rlang::sym(input$variavel)
        
        test_predictions %>% 
            group_by(!!var) %>% 
            summarise(rmsle = rmsle_vec(preco_medio_revenda, lag_preco_medio_revenda)) %>% 
            ungroup() %>% 
            mutate(!!var := fct_reorder(!!var, rmsle)) %>% 
            ggplot(aes(!!var, rmsle)) +
            geom_col(fill = "#42a5f5") +
            labs(title = paste("RMSLE por", input$variavel %>% str_to_title()),
                 subtitle = "A linha vermelha representa o RMSLE geral",
                 x = "",
                 y = "RMSLE") +
            geom_hline(yintercept = rmsle_geral, color = "red", size = 1.1) +
            coord_flip() +
            theme_light()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
