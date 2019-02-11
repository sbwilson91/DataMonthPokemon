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
library(ggplot2)

gen1stats <- read_csv("../Gen1Stats.csv", col_names = T)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Pokemon DV/IV calculator Generation 1"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "pkmn",
                    label = "Pokemon: ",
                    choices = gen1stats$Name
                    ),
        
         sliderInput("level",
                     "Pokemon Level:",
                     min = 1,
                     max = 100,
                     value = 25),
        uiOutput("health"),
        uiOutput("attack"),
        uiOutput("defense"),
        uiOutput("speed"),
        uiOutput("special")
        

        
      ),
      mainPanel(
      tabsetPanel(type = "pills",
        tabPanel("Gen 1",
                 fluidRow(
                 tableOutput("BASEstats"),
                 tableOutput("DVstats"),
                 plotOutput("bar"))),
        tabPanel("Gen 2")
        
      ))
      
      # Show a plot of the generated distribution
      #mainPanel(
      #   tableOutput("BASEstats"),
      #   tableOutput("DVstats"),
      #   plotOutput("bar")
      #)
   )
)

# Define server logic required to draw a histogram
server <- function(input, output){
  

  output$attack <- renderUI({
    poke <- gen1stats %>% filter(Name == input$pkmn)
    sliderInput("att",
                "Attack:",
                step = 1,
                min = as.integer(poke$ATT * input$level / 50 + 5 + 0 * input$level / 50),
                max = as.integer(poke$ATT * input$level / 50 + 5 + 15 * input$level / 50),
                value = as.integer(poke$ATT * input$level / 50 + 5 + 0 * input$level / 50))
    
  })
  output$defense <- renderUI({
    poke <- gen1stats %>% filter(Name == input$pkmn)
    sliderInput("def",
                "Defense:",
                step = 1,
                min = as.integer(poke$DEF * input$level / 50 + 5 + 0 * input$level / 50),
                max = as.integer(poke$DEF * input$level / 50 + 5 + 15 * input$level / 50),
                value = as.integer(poke$DEF * input$level / 50 + 5 + 0 * input$level / 50))
    
  })
  output$speed <- renderUI({
    poke <- gen1stats %>% filter(Name == input$pkmn)
    sliderInput("spd",
                "Speed:",
                step = 1,
                min = as.integer(poke$SPD * input$level / 50 + 5 + 0 * input$level / 50),
                max = as.integer(poke$SPD * input$level / 50 + 5 + 15 * input$level / 50),
                value = as.integer(poke$SPD * input$level / 50 + 5 + 0 * input$level / 50))
    
  })
  output$special <- renderUI({
    poke <- gen1stats %>% filter(Name == input$pkmn)
    sliderInput("spc",
                "Special:",
                step = 1,
                min = as.integer(poke$SPC * input$level / 50 + 5 + 0 * input$level / 50),
                max = as.integer(poke$SPC * input$level / 50 + 5 + 15 * input$level / 50),
                value = as.integer(poke$SPC * input$level / 50 + 5 + 0 * input$level / 50))
                
    
  })
  output$health <- renderUI({
    poke <- gen1stats %>% filter(Name == input$pkmn)
    sliderInput("hp",
                "HP:",
                step = 1,
                min = as.integer(poke$HP * input$level / 50 + (input$level+10) + 0 * input$level / 50),
                max = as.integer(poke$HP * input$level / 50 + (input$level+10) + 15 * input$level / 50),
                value = as.integer(poke$HP * input$level / 50 + (input$level+10) + 0 * input$level / 50))
    
  })
   
   output$BASEstats <- renderTable({
       poke <- gen1stats %>% filter(Name == input$pkmn)
       poke

      
   })
   output$DVstats <- renderTable({
     poke <- gen1stats %>% filter(Name == input$pkmn)
     a <- c("HP", "ATT", "DEF", "SPD", "SPC")
     min.max <- data.frame(row.names = c("MIN", "MAX"))
     inputs <- data.frame(pkmn = input$pkmn, 
                          hp = input$hp,
                          att = input$att,
                          def = input$def,
                          spd = input$spd,
                          spc = input$spc,
                          level = input$level)
     inputs$pkmn <- as.character(inputs$pkmn)
     poke$Name <- as.character(poke$Name)
     stats <- full_join(x = inputs, y = poke, by = c("pkmn" = "Name"))
     for(j in 1:length(a)){
       if (a[j] == "HP") {
         #hp calculation
         stat.range <- c()
         fla <- function(stats, i = i) {
           as.integer(stats$HP * stats$level / 50 + (stats$level+10) + i * stats$level / 50)
           
         }
         
       } else {
         stat.range <- c()
         fla <- function(stats, i = i) {
           as.integer(stats[,(7+j)] * stats$level / 50 + 5 + i * stats$level / 50)
           
         }
         #other stat calculation
       }
       
       for(i in 0:15){
         temp <- fla(stats, i)
         if (temp == inputs[j+1]){
           stat.range <- c(stat.range, i)
         }
         
       }
       min.max <- cbind(min.max, c(min(stat.range), max(stat.range)))

     }
     colnames(min.max) <- a
     min.max %>% mutate(PCT = (min.max$ATT+min.max$DEF+min.max$SPD+min.max$SPC+min.max$HP)/75*100)
     #min.max %>% mutate(PCT = sum(min.max[,1:5])/75*100)
     
     
   })
   output$bar <- renderPlot({
     poke <- gen1stats %>% filter(Name == input$pkmn)
     a <- c("HP", "ATT", "DEF", "SPD", "SPC")
     min.max <- data.frame(row.names = c("MIN", "MAX"))
     inputs <- data.frame(pkmn = input$pkmn, 
                          hp = input$hp,
                          att = input$att,
                          def = input$def,
                          spd = input$spd,
                          spc = input$spc,
                          level = input$level)
     stats <- full_join(x = inputs, y = poke, by = c("pkmn" = "Name"))
     for(j in 1:length(a)){
       if (a[j] == "HP") {
         #hp calculation
         stat.range <- c()
         fla <- function(stats, i = i) {
           as.integer(stats$HP * stats$level / 50 + (stats$level+10) + i * stats$level / 50)
           
         }
         
       } else {
         stat.range <- c()
         fla <- function(stats, i = i) {
           as.integer(stats[,(7+j)] * stats$level / 50 + 5 + i * stats$level / 50)
           
         }
         #other stat calculation
       }
       
       for(i in 0:15){
         temp <- fla(stats, i)
         if (temp == inputs[j+1]){
           stat.range <- c(stat.range, i)
         }
         
       }
       min.max <- cbind(min.max, c(min(stat.range), max(stat.range)))
       
     }
     colnames(min.max) <- a
     
     c <- t(min.max)
     c <- data.frame(STAT = c(a,a), VALUE = c(c[,1], c[,2]))

     ggplot(c, aes(x = STAT, y = VALUE)) + 
       geom_line(size = 2) +
       geom_point(size = 5)+
       coord_cartesian(ylim = c(0,15)) +
       labs(title = paste0("Percentage is "))
     
     
     #data.frame(Type = c("HP", "ATT", "DEF", "SPD", "SPC", "PCT"),
     #           DV = as.vector(stat[2:7]))
     
     
   })

}
# Run the application 
shinyApp(ui = ui, server = server)
