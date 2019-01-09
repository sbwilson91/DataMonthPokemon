# producing a data table of min/max DV values
gen1stats <- read_csv("Gen1Stats.csv", col_names = T)
input <- data.frame(pkmn = "Machoke", 
                    hp = 82,
                    att = 62,
                    def = 44,
                    spd = 30,
                    spc = 32,
                    level = 25)
poke <- gen1stats %>% filter(Name == input$pkmn)
a <- c("HP", "ATT", "DEF", "SPD", "SPC")
b <- c(as.integer(100*(input$hp-(input$level+10))/(2*input$level)-poke$HP),
       as.integer(100*(input$att-5)/(2*input$level)-poke$ATT),
       as.integer(100*(input$def-5)/(2*input$level)-poke$DEF),
       as.integer(100*(input$spd-5)/(2*input$level)-poke$SPD),
       as.integer(100*(input$spc-5)/(2*input$level)-poke$SPC)
)
c <- data.frame(STAT = a, VALUE = b)



# work out def range

min.max <- data.frame(row.names = c("MIN", "MAX"))# %>% rownames_to_column("DV")

stats <- full_join(x = input, y = poke, by = c("pkmn" = "Name"))
for(j in 1:length(a)){
  if (a[j] == "HP") {
    #hp calculation
    stat.range <- c()
    print("as.integer(stats$HP * stats$level / 50 + (stats$level+10) + i * stats$level / 50)")
    fla <- function(stats, i = i) {
      as.integer(stats$HP * stats$level / 50 + (stats$level+10) + i * stats$level / 50)
      
      }
    
  } else {
    stat.range <- c()
    print("as.integer(stats[,(1+j)] * stats$level / 50 + 5 + i * stats$level / 50)")
    fla <- function(stats, i = i) {
      as.integer(stats[,(7+j)] * stats$level / 50 + 5 + i * stats$level / 50)
      
      }
    #other stat calculation
  }

  for(i in 0:15){
    print(paste0("Stat= ", a[j], " dv = ", i, " base= ", stats[1,(7+j)], " stat= ", stats[1,(1+j)]))
    temp <- fla(stats, i)
    print(temp)
    if (temp == input[j+1]){
      print(paste0("dv = ", i, " matches stat"))
      stat.range <- c(stat.range, i)
      print(stat.range)
    }
    
  }
  min.max <- cbind(min.max, c(min(stat.range), max(stat.range)))
  print(paste0("Possible ", a[j], " is ",  min(stat.range), "-", max(stat.range)))
  
}
colnames(min.max) <- a

