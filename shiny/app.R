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
library(shinyWidgets)

df <- read.csv("dunkest.csv", sep = ";")
cols = c("PDK_mean", "PDK_sd", "PDK_last", "PDK_n",
         "CR", "PDK_CR", "PLUS", "PCT_GAIN")

# Define UI for application that renders a data table with some options
ui <- fluidPage(

    # Application title
    titlePanel("Dunkest Data Analysis"),

    # Dropdown with several options for data rendering
    dropdown(
        
        # Numerical input for N of last days to be evaluated
        numericInput("n_days",
                     "Last N days to take into account:",
                     value = 5,
                     min = 0, 
                     max = max(df$Day)),
        
        # Numerical input for minimum N of game days to be included in analysis
        numericInput("min_days",
                     "Minimum of days for a player to be considered:",
                     value = 3,
                     min = 0,
                     max = max(df$Day)),
        
        # Numerical input for number of guards to be included in the table
        numericInput("n_g",
                     "Number of Guards",
                     value = 0,
                     min = 0,
                     max = 20),
        
        # Numerical input for number of forwards to be included in the table
        numericInput("n_f",
                     "Number of Forwards",
                     value = 0,
                     min = 0,max = 20),
        
        # Numerical input for number of centers to be included in the table
        numericInput("n_c",
                     "Number of Centers",
                     value = 0,
                     min = 0,
                     max = 20),
        
        # Whether to sort the table in ascending (T) or descending (F) order
        radioGroupButtons("asc",
                          "Sort in ascending order?",
                          c(TRUE, FALSE),
                          selected = FALSE),
        
        # The property to be optimized in the output data table
        radioGroupButtons("optimize",
                          "Which property do you want to optimize?",
                          cols,
                          direction = "vertical"),
        
        ), 
    
    br(),
    
        # Show a plot of the generated distribution
    dataTableOutput("distPlot")
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderDataTable({
        
        
        big_df <- df %>%
            filter(Day > (max(.$Day) - input$n_days)) %>%
            group_by(Player, Team) %>%
            summarize(Team = tail(Team, 1),
                      Pos = tail(Pos, 1),
                      PDK_mean = mean(PDK),
                      PDK_sd = sd(PDK),
                      PDK_last = tail(PDK, 1),
                      PDK_n = n(),
                      CR = tail(CR, 1),
                      PDK_CR = PDK_mean / CR,
                      PLUS = tail(PLUS, 1),
                      PCT_GAIN = PLUS/CR * 100,
            ) %>%
            filter(PDK_n >= input$min_days,
                   PDK_n <= input$n_days) %>% 
            mutate_if(is.numeric, round, 2)
        
        if (input$asc) {
            
            big_df <- big_df %>% 
                arrange(.[, input$optimize])
            
        } else {
            
            big_df <- big_df %>% 
                arrange(desc(.[, input$optimize]))
            
        }
        
        if (!input$n_g & !input$n_f & !input$n_c){
            
            big_df
            
        } else {
        
        g <- big_df %>%
            filter(Pos == "G") %>%
            head(input$n_g)
        
        f <- big_df %>%
            filter(Pos == "F") %>%
            head(input$n_f)
        
        c <- big_df %>%
            filter(Pos == "C") %>%
            head(input$n_c)
        
        rbind(g,f,c)
        
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
