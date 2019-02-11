#As such, the only way to get Attack and Defense IVs is to just try values
#until they are legal. By taking the original CP formula:
#  
#  CP = ((Base_Attack + Individual_Attack) * 
#          ((Base_Defense + Individual_Defense)^0.5 * 
#             (Base_Stamina + Individual_Stamina)^0.5 * CPM^2) / 10
#        
#We can plug in values for everything except for Attack and Defense IVs, 
#and then rearrange the equations:
#  
#Individual_Attack = (CP / ((Base_Defense + Individual_Defense)^0.5 * 
#                             (Base_Stamina + Individual_Stamina)^(0.5) * 
#                             CPM^2 / 10)) - Base_Attack
#Individual_Defense = (CP / ((Base_Attack + Individual_Attack) * 
#                              (Base_Stamina + Individual_Stamina)^(0.5) *
#                              CPM^2 / 10))^2 - Base_Defense
### NEW STAT FORMULAS
#BaseAttack=Round(ScaledAttack∗SpeedMod)
#ScaledAttack=Round(2∗(7Higher/8 + 1Lower/8))
#SpeedMod=1+((Speed−75)/500)
#
#BaseDefense=Round(ScaledDefense∗SpeedMod)
#ScaledDefense=Round(2∗(7Higher/8 + 1Lower/8))
#

library(readr)
library(tidyverse)

stats <- read.csv("./pogo_base.csv")
head(stats)
colnames(stats) <- c("Pokemon", "HP", "ATT", "DEF", "MAX_CP", "RATING")
head(stats)
stats <- stats %>% filter(DEF != "Def")
head(stats)
names <- strsplit(as.character(stats[,1]), split = "\\s")
newnames <- NULL
for (i in 1:nrow(stats)){
  newnames <- c(newnames, (names[[i]][2]))
}

stats[,1] <- newnames
head(stats)

CP = ((Base_Attack + Individual_Attack) * 
       ((Base_Defense + Individual_Defense)^0.5 * 
          (Base_Stamina + Individual_Stamina)^0.5) * CPM^2) / 10

CPMtable <- read.csv("CPM.csv")
head(CPMtable)      





      
