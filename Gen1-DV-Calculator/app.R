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
   titlePanel("Pokemon DV calculator Generation 1"),
   
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
                     value = 100),
        uiOutput("health"),
        uiOutput("attack"),
        uiOutput("defense"),
        uiOutput("speed"),
        uiOutput("special")
        

        
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         tableOutput("BASEstats"),
         tableOutput("DVstats"),
         plotOutput("bar")
      )
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
     #min.max <- data.frame(row.names = c("MIN", "MAX"))# %>% rownames_to_column("DV")
     #HP <- c()
     #for(i in 0:15){
     #  as.integer(poke$HP * input$level / 50 + 5 + i * input$level / 50) -> a
     #  if (a == input$hp){
     #    HP <- c(HP, a)
     #  }
     #  
     #}
     #data.frame(HP = HP)

     stat <- data.frame(name = input$pkmn)
     stat$HP <-  as.integer(100*(input$hp-(input$level+10))/(2*input$level)-poke$HP)
     stat$ATT <- as.integer(100*(input$att-5)/(2*input$level)-poke$ATT)
     stat$DEF <- as.integer(100*(input$def-5)/(2*input$level)-poke$DEF)
     stat$SPD <- as.integer(100*(input$spd-5)/(2*input$level)-poke$SPD)
     stat$SPC <- as.integer(100*(input$spc-5)/(2*input$level)-poke$SPC)
     stat$Pct <- sum(stat[2:6])/75*100
     stat
   })
   output$bar <- renderPlot({
     poke <- gen1stats %>% filter(Name == input$pkmn)
     a <- c("HP", "ATT", "DEF", "SPD", "SPC")
     b <- c(as.integer(100*(input$hp-(input$level+10))/(2*input$level)-poke$HP),
            as.integer(100*(input$att-5)/(2*input$level)-poke$ATT),
            as.integer(100*(input$def-5)/(2*input$level)-poke$DEF),
            as.integer(100*(input$spd-5)/(2*input$level)-poke$SPD),
            as.integer(100*(input$spc-5)/(2*input$level)-poke$SPC)
            )
     c <- data.frame(STAT = a, VALUE = b)

     ggplot(c, aes(x = STAT, y = VALUE)) + geom_col() + 
       coord_cartesian(ylim = c(0,15)) +
       labs(title = paste0("Percentage: ", sum(b[1:5])/75*100))
     
     
     #data.frame(Type = c("HP", "ATT", "DEF", "SPD", "SPC", "PCT"),
     #           DV = as.vector(stat[2:7]))
     
     
   })
#})
}
# Run the application 
shinyApp(ui = ui, server = server)
