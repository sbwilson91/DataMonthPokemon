library(tidyverse)
library(NMF)

pokemon <- read_csv("pokemon.csv")

head(pokemon)
str(pokemon)

(pokemon %>% filter(is_legendary > 0) %>% 
    transmute(name = name, attack = attack, defense = defense, speed = speed) %>% 
    arrange(-attack))
# pull out measurable statistics on battle stats as well as size metrics and legendary status
poke.stats <- pokemon %>% 
  transmute(name = name,
            hp = hp,
            attack = attack,
            defense = defense,
            speed = speed,
            sp_attack = sp_attack,
            sp_defense = sp_defense,
            height_m = height_m,
            weight_kg = weight_kg,
            type1 = type1,
            type2 = type2,
            is_legendary = is_legendary)
head(poke.stats)

poke.stats <- as.data.frame(poke.stats)
poke.stats <- (poke.stats %>% column_to_rownames("name"))
png("figs/legendary_stats_heatmap.png", res = 300, width = 300, height = 300, units = "mm")
aheatmap(x = as.matrix(poke.stats), scale = "col"
        )
dev.off()

library(ggplot2)

ggplot(data = poke.stats, aes(attack, defense)) +
  geom_point() +
  facet_grid(type1~.~type2)

summary(poke.stats$type1)
str(poke.stats)

type.combo <- poke.stats %>% 
  transmute(type = paste(type1, type2, sep = "."), type1 = type1, type2 = type2)
head(type.combo)

table(type.combo$type)
as.data.frame(table(type.combo$type))
type.combo.sum <- as.data.frame(table(type.combo$type)) %>% arrange(-Freq)
head(type.combo.sum)