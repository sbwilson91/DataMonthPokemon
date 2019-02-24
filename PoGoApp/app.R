#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# PoGo app

library(shiny)
library(tidyverse)
library(ggplot2)

base_stats <- read_csv("../pogo_base_stats.csv", col_names = T)
cpmtable <- read_csv("../CPM.csv")
overall <- tibble(
  Mystic = c("Overall, your pokemon is a wonder! What a breathtaking Pokemon!",
              "Overall, your pokemon has certainly caught my attention.",
              "Overall, your pokemon is above average.",
              "Overall, your pokemon is not likely to make much headway in battle."),
  Instinct = c("Overall, your pokemon looks like it can really battle with the best of them!",
              "Overall, your pokemon is really strong!",
              "Overall, your pokemon is pretty decent!",
              "Overall, your pokemon has room for improvement as far as battling goes."),
  Valor = c(
              "Overall, your pokemon simply amazes me. It can accomplish anything!",
              "Overall, your pokemon is a strong Pokemon. You should be proud!",
              "Overall, your pokemon is a decent Pokemon",
              "Overall, your pokemon may not be great in battle, but I still like it!"),
  TotalIV = c(list(39:48), list(32:38), list(25:31), list(0:24)
            ))

individual <- tibble(
  Mystic = c("Its stats exceed my calculations. It’s incredible!",
                "I am certainly impressed by its stats, I must say.",
                "Its stats are noticeably trending to the positive.",
                "Its stats are not out of the norm, in my opinion."),
  Instinct = c(
                "Its stats are the best I’ve ever seen! No doubt about it!",
                "Its stats are really strong! Impressive.",
                "It’s definitely got some good stats. Definitely!",
                "Its stats are all right, but kinda basic, as far as I can see."),
  Valor = c(
                "I’m blown away by its stats. WOW!",
                "It’s got excellent stats! How exciting!",
                "Its stats indicate that in battle, it’ll get the job done.",
                "Its stats don’t point to greatness in battle."),
                
  IV = c(list(15), list(13:14), list(8:12), list(0:7))
         
)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Pokemon IV calculator PokemonGo"),
  
  # Sidebar  
  sidebarLayout(
    sidebarPanel(
      radioButtons("team",
                   label = "What team are you in? ",
                   choices = c("Mystic", "Instinct", "Valor"),
                   selected = "Instinct", inline = T),
      selectInput(inputId = "pkmn",
                  label = "Pokemon: ",
                  choices = base_stats$Pokemon
      ),
      
      sliderInput("level",
                  "Pokemon Level:",
                  min = 1,
                  max = 40,
                  value = 25),
      
      uiOutput("hp"),
      uiOutput("cp"),
      uiOutput("overall"),
      checkboxGroupInput("ivstat",inline = T,
                         "Stats appraised:",
                         choices = c("Attack", "Defense", "HP")),
      uiOutput("ivapp")
    ),
    #mainPanel(
    #  tabsetPanel(type = "pills",
    #              tabPanel("Gen 1",
    #                       fluidRow(
    #                         tableOutput("BASEstats"),
    #                         tableOutput("IVstats"),
    #                         plotOutput("bar"))),
    #              tabPanel("Gen 2")
    #              
    #  ))
    
    # Show a plot of the generated distribution
    mainPanel(
       tableOutput("BASEstats"),
       tableOutput("IVstats"),
       plotOutput("bar")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output){
  
  
  output$ivapp <- renderUI({

    selectInput("ivapp",
                "Stat appraisal:",
                choices = individual[,input$team])
    
  })
 
  output$hp <- renderUI({
    poke <- base_stats %>% filter(Pokemon == input$pkmn)
    sliderInput("hp",
                "HP:",
                step = 1,
                min = as.integer((poke$HP + 0) * cpmtable[input$level,2]),
                max = as.integer((poke$HP + 15) * cpmtable[input$level,2]),
                value = as.integer((poke$HP + 0) * cpmtable[input$level,2]))
    
  })
  
  output$overall <- renderUI({
    selectInput("overall",
                "Overall Appraisal:",
                choices = as.vector(overall[,input$team]))
    
  })
  
  output$cp <- renderUI({
    poke <- base_stats %>% filter(Pokemon == input$pkmn)
    sliderInput("cp",
                "CP:",
                step = 1,
                min = 10,
                max = poke$MAX_CP,
                value = as.integer(10))
    
  })
  
  output$BASEstats <- renderTable({
    poke <- base_stats %>% filter(Pokemon == input$pkmn)
    poke[,1:6]
    
    
  })
  # perform alignment. might redo whole section.
  output$IVstats <- renderTable({
    # generate table of possible IVs from input values
    iv.app <- individual %>% transmute(Mystic, IV) %>% column_to_rownames(var = "Mystic")
   
    ivs <- data.frame(Stat = c("ATT", "DEF", "STA"),
            Min = c(0,0,0),
            Max = c(15,15,15))
    ivstat <- c("Attack", "Defense", "HP")
    if (grep("Attack", ivstat) > 0) {
      ivs[1,2] <- min(iv.app[["I am certainly impressed by its stats, I must say.",1]])
      ivs[1,3] <- max(iv.app[["I am certainly impressed by its stats, I must say.",1]])
    } 
    if (grep("Defense", ivstat) > 0) {
      ivs[2,2] <- min(iv.app[["I am certainly impressed by its stats, I must say.",1]])
      ivs[2,3] <- max(iv.app[["I am certainly impressed by its stats, I must say.",1]])
    } 
    if (grep("HP", ivstat) > 0) {
      ivs[3,2] <- min(iv.app[["I am certainly impressed by its stats, I must say.",1]])
      ivs[3,3] <- max(iv.app[["I am certainly impressed by its stats, I must say.",1]])
    } 
    
    ivs
      
      
    
    
  })
  output$bar <- renderPlot({
    iv.app <- individual %>% transmute(Mystic, IV) %>% column_to_rownames(var = "Mystic")
    
    ivs <- data.frame(Stat = c("ATT", "DEF", "STA"),
                      Min = c(0,0,0),
                      Max = c(15,15,15))
    ivstat <- c("Attack", "Defense", "HP")
    if (grep("Attack", ivstat) > 0) {
      ivs[1,2] <- min(iv.app[["I am certainly impressed by its stats, I must say.",1]])
      ivs[1,3] <- max(iv.app[["I am certainly impressed by its stats, I must say.",1]])
    } 
    if (grep("Defense", ivstat) > 0) {
      ivs[2,2] <- min(iv.app[["I am certainly impressed by its stats, I must say.",1]])
      ivs[2,3] <- max(iv.app[["I am certainly impressed by its stats, I must say.",1]])
    } 
    if (grep("HP", ivstat) > 0) {
      ivs[3,2] <- min(iv.app[["I am certainly impressed by its stats, I must say.",1]])
      ivs[3,3] <- max(iv.app[["I am certainly impressed by its stats, I must say.",1]])
    } 
    
    ggplot(ivs, aes(x = Stat)) + 
      geom_boxplot(aes(ymin = Min, middle = Min, lower = Min, upper = Max, ymax = Max), stat = "identity") +
      #geom_line(size = 2) +
      #geom_point(size = 5)+
      coord_cartesian(ylim = c(0,15)) 
      #labs(title = paste0("Percentage is "))
    
    
    #data.frame(Type = c("HP", "ATT", "DEF", "SPD", "SPC", "PCT"),
    #           DV = as.vector(stat[2:7]))
    
    
  })
  
}
# Run the application 
shinyApp(ui = ui, server = server)
