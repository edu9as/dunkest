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
names(df) <- c(("DayRank"), names(df)[2:(length(df)-2)], c("PlusMinus", "Day"))
cols = c("PDK_mean", "PDK_sd", "PDK_last", "PDK_n",
         "CR", "PDK_CR", "PLUS", "PCT_GAIN")

# Define UI for application that renders a data table with some options
ui <- fluidPage(

    # Application title
    titlePanel("Dunkest Data Analysis"),
    tabsetPanel(type = "tabs",
                tabPanel("All data", dataTableOutput("simpleDf")),
                tabPanel("FindTeam",
                         
                         # Dropdown with several options for data rendering
                         dropdown(
                             
                             # Numerical input for N last days to be evaluated
                             numericInput("n_days",
                                          "Last N days to take into account:",
                                          value = 7,
                                          min = 0, 
                                          max = max(df$Day)),
                             
                             # Numerical input for minimum N of game days to be
                             # included in analysis
                             numericInput("min_days",
                                          paste("Minimum of days for a player",
                                                "to be considered:"),
                                          value = 0,
                                          min = 0,
                                          max = max(df$Day)),
                             
                             # Numerical input for number of guards to be
                             # included in the table
                             numericInput("n_g",
                                          "Number of Guards",
                                          value = 0,
                                          min = 0,
                                          max = 20),
                             
                             # Numerical input for number of forwards to be
                             # included in the table
                             numericInput("n_f",
                                          "Number of Forwards",
                                          value = 0,
                                          min = 0,max = 20),
                             
                             # Numerical input for number of centers to be
                             # included in the table
                             numericInput("n_c",
                                          "Number of Centers",
                                          value = 0,
                                          min = 0,
                                          max = 20),
                             
                             # Whether to sort the table in ascending (T) or
                             # descending (F) order
                             radioGroupButtons("asc",
                                               "Sort in ascending order?",
                                               c(TRUE, FALSE),
                                               selected = FALSE),
                             
                             # The property to be optimized in the output data
                             # table
                             radioGroupButtons("optimize",
                                               paste("Which property do you"
                                                     "want to optimize?"),
                                               cols,
                                               direction = "vertical"),
                             
                         ), 
                         
                         br(),
                         
                         # Show a plot of the generated distribution
                         dataTableOutput("groupDf")
                ),
                tabPanel("EvaluateTeam",
                         
                         # Dropdown with several options for data rendering
                         dropdown(
                             
                             # Numerical input for N of last days to be
                             # evaluated
                             numericInput("team_n_days",
                                          "Last N days to take into account:",
                                          value = 5,
                                          min = 0, 
                                          max = max(df$Day)),
                             
                             textAreaInput("team",
                                           "Introduce your team",
                                           "L. Doncic"),
                             
                         ), 
                         
                         br(),
                         dataTableOutput("evaluateTeam")
                )))

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$simpleDf <- renderDataTable({
        df %>% 
            select(Player, Day, everything())
    })
    big_df <- reactive({
        the_df <- df %>%
            filter(Day > (max(.$Day) - input$n_days)) %>%
            group_by(Player, Team) %>%
            summarize(Team = tail(Team, 1),
                      Pos = tail(Pos, 1),
                      PDK_mean = mean(PDK),
                      PDK_sd = sd(PDK),
                      PDK_last = tail(PDK, 1),
                      PDK_n = n(),
                      PCT_GAIN = tail(PLUS, 1)/tail(CR, 1) * 100,
                      CR = tail(CR, 1),
                      PDK_CR = PDK_mean / CR,
                      PLUS = tail(PLUS, 1),
                      
            ) %>%
            filter(PDK_n >= input$min_days,
                   PDK_n <= input$n_days) %>% 
            mutate_if(is.numeric, round, 2)
        
        if (input$asc) {
            
            the_df <- the_df %>% 
                arrange(.[, input$optimize])
            
        } else {
            
            the_df <- the_df %>% 
                arrange(desc(.[, input$optimize]))
            
        }
        the_df %>% 
            select(Player, Team, Pos, PDK_CR, CR, PDK_mean, PDK_sd, PDK_n,
                   PDK_last, everything())
        
    })
    
    best <- reactive({
        the_df <- big_df()
        g <- the_df %>%
            filter(Pos == "G") %>%
            head(input$n_g)
        
        f <- the_df %>%
            filter(Pos == "F") %>%
            head(input$n_f)
        
        c <- the_df %>%
            filter(Pos == "C") %>%
            head(input$n_c)
        
        rbind(g,f,c) %>% 
            select(Player, Team, Pos, PDK_CR, CR, PDK_mean, PDK_sd, PDK_n,
                   PDK_last, everything())})
    
    players_in_df <- reactive({
        the_df <- df %>%
            filter(Day > (max(.$Day) - input$team_n_days)) %>%
            group_by(Player, Team) %>%
            summarize(Team = tail(Team, 1),
                      Pos = tail(Pos, 1),
                      PDK_mean = mean(PDK),
                      PDK_sd = sd(PDK),
                      PDK_last = tail(PDK, 1),
                      PDK_n = n(),
                      PCT_GAIN = tail(PLUS, 1)/tail(CR, 1) * 100,
                      CR = tail(CR, 1),
                      PDK_CR = PDK_mean / CR,
                      PLUS = tail(PLUS, 1),
                      
            ) %>%
            filter(PDK_n >= input$min_days,
                   PDK_n <= input$n_days) %>% 
            mutate_if(is.numeric, round, 2)
        
        the_df$PlayerTeam <- paste0(the_df$Player, "+", the_df$Team)
        the_df %>% 
            filter(PlayerTeam %in% str_split(input$team, "\n")[[1]] | 
                       Player %in% str_split(input$team, "\n")[[1]]) %>% 
            select(-PlayerTeam) %>% 
            select(Player, Team, Pos, PDK_CR, CR, PDK_mean, PDK_sd, PDK_n,
                   PDK_last, everything())
        
    })
    
    
    output$groupDf <- renderDataTable({
        
        if (!input$n_g & !input$n_f & !input$n_c){
            
            big_df()
            
        } else {
        
        best()
        
        }
    })
    output$evaluateTeam <- renderDataTable({
        players_in_df()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
