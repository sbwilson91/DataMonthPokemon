library(tidyverse)

pokestats <- read_csv("Gen1Stats.csv")
head(pokestats)[,1:3]
str(pokestats)
basestat <- data.frame(name = pokestats$name,
                       attack = pokestats$attack,
                       defense = pokestats$defense)




STAT = int(((BaseStat + DV)*2+StatPoint)*Level/100)+E
(((35+15)*2+0)*26/100)+5

where E = Level + 10 for HP
E = 5 for any other stat.
StatPoint = int((SQRT(StatExp-1)+1)/4)

stat + E = int(((basestat + dv)*2+statpoint)*level/100)

(stat + E)/(level/100) = int((basestat + dv)*2+statpoint)

((stat + E)/(level/100) - statpoint)/2 = basestat + dv

dv = int(((stat + E)/(level/100) - statpoint)/2) - basestat

b.att <- 35

dv <- (((58+5)/(22/100)-0)/2)-b.att
dv

(((35+15)*2+0)*22/100)+5       





W = round(B * L / 50 + 5 + D * L / 50)

35 * 22 / 50 + 5 + 15 * 22 / 50

round( 50 * ( W – 5) / L ) – B

round( 50 * (49 - 5) / 22) - 100
round( 50 * (20 - 5) / 22) - 35


b.hp <- 30
b.att <- 35
b.def <- 30
b.spd <- 80
b.spc <- 100

gastly <- list(as.integer(b.hp), b.att, b.def, b.spd, b.spc)

stat <- function(B, L, W) {
   # print0("HP: ", round( 50 * (  W[[1]] - 10 ) / L ) – B[[1]] – 50)
    round( 50 * (W - 5) / L) - B
    #print0("DEF: ", round( 50 * ( W[[3]] – 5) / L ) – B[[3]])
    #print0("SPD: ", round( 50 * ( W[[4]] – 5) / L ) – B[[4]])
    #print0("SPC: ", round( 50 * ( W[[5]] – 5) / L ) – B[[5]])
 
}

stat(35, 26, 39)


round(50*(W[[1]]–5)/L)–B[[1]]
round(50*(W[[1]]-10)/L)-B[[1]]-50



L <- 22
W <- gastly
B <- list(58, 34, 31, 53, 62)

stat(30, 22, 0)
stat(30, 22, 15)

w[1]-1
W[1]-1
W[1]
-1
1-W[1]
class(gastly)

class(W[1])
class(as.integer(gastly[2]))


100(S-C)/(2*L)-B

((35+15)*2)*22/100
((35+seq(0,15,1))*2)*22/100+5

(100*(25-5))/(2*22)-35

pkmn.base <- c(b.hp, b.att, b.def, b.spd, b.spc)
pkmn.stats <- c(49, 25, 18, 43, 54)

dv <- function(pokemon, S, L) {
  poke <- gen1stats %>% filter(Name == pokemon)
  
  stat <- c((100*(S[1]-(L+10))/(2*L)-poke$HP),
  (100*(S[2]-5)/(2*L)-poke$ATT),
  (100*(S[3]-5)/(2*L)-poke$DEF),
  (100*(S[4]-5)/(2*L)-poke$SPD),
  (100*(S[5]-5)/(2*L)-poke$SPC))
  print(round(stat))

}
dv("Gastly", c(47,24,24,45,54), 22)


data.frame(pokestats$sp_attack, pokestats$sp_defense)[1:151,]

gen1stats <- read_csv("Gen1Stats.csv", col_names = T)


head(gen1stats)





# HP stat
# ODD: att = 8, def = 4, speed = 2, spc = 1

hpdv <- function(x) {
  i <- 0
  if (as.integer(x[1]) %% 2 != 0){
    i <- i + 8
  }
  if (as.integer(x[2]) %% 2 != 0){
    i <- i + 4
  }
  if (as.integer(x[3]) %% 2 != 0){
    i <- i + 2
  }
  if (as.integer(x[4]) %% 2 != 0){
    i <- i + 1
  }
  print(i)
}
hpdv(x = c(10,1,8,1))

dv.range <- function(pokemon,stat, L) {
  poke <- pokestats %>% filter(Name == pokemon)
  
  for(i in 0:15){
    as.integer(poke$DEF * L / 50 + 5 + i * L / 50) -> a
    if (a == stat){
      print(i)
    }
    
    
  }

  
}
  
dv.range("Magikarp", 18,10)

stat <- cbind(1, (100*(S[1]-(L+10))/(2*L)-poke$HP),
            (100*(S[2]-5)/(2*L)-poke$ATT),
            (100*(S[3]-5)/(2*L)-poke$DEF),
            (100*(S[4]-5)/(2*L)-poke$SPD),
            (100*(S[5]-5)/(2*L)-poke$SPC))
  print(round(stat))  

W <- as.integer(B * L / 50 + 5 + D * L / 50)


