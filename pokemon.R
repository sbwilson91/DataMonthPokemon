library(tidyverse)
library(NMF)
library(gganimate)


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


# What are the most commonly abilities

str(pokemon)
head(pokemon$abilities)
table(pokemon$abilities)
class(pokemon$abilities[1])
grep(pattern = "Absorb", x = pokemon$abilities,ignore.case = T,value = T )
abilities <- unlist(str_split(pokemon$abilities, pattern = "'"))
table(abilities)
abilities <- data.frame(ability = abilities)
head(abilities)
abilities <- as.data.frame(table(abilities))
head(abilities)

abilities <- abilities %>% filter(Freq < 100) %>% arrange(-Freq)

ggplot(abilities, aes(abilities, Freq)) +
  geom_col()

head(abilities)

# the following table is a summary of abilities and how common they are
head(abilities, 10)

# The most common is "Sturdy". which pokemon have this ability?

pokemon[grep(pattern = "Sturdy", x = pokemon$abilities), ]$name

# Which pokemon have an ability that can "absorb"?
pokemon[grep(pattern = "Absorb", x = pokemon$abilities),]$name

str(pokemon)
str(poke.stats)
ggplot(poke.stats, aes(height_m, weight_kg, fill = hp)) +
  geom_point()
)

str(poke.stats)

# look at primary type distribution by height for all non-legendary pokemon

ggplot((pokemon %>% filter(pokemon$is_legendary < 1)), aes(type1, height_m, fill = type1))+
  geom_boxplot()

# do the same for legendary pokemon, but sort by generation
ggplot((pokemon %>% filter(is_legendary > 0)), aes(generation, height_m, 
                                                   group = generation, fill = as.factor(generation)))+
  geom_boxplot()

# possibly a scatterplot might better demonstrate these metrics. Lets look at height vs attack
ggplot((pokemon %>% filter(is_legendary > 0)),
       aes(attack, height_m,
           group = generation, colour = as.factor(generation)))+
  geom_point(size = 3)+
  labs(title = 'Year: {frame_time}', x = 'Attack', y = 'Defense') +
  transition_time(generation) +
  ease_aes('linear')

# transition of attack and defense across generation sorted by primary type

ggplot(pokemon, 
       aes(type1, attack,
       fill = type1)) +
  geom_boxplot() +

  labs(title = 'Generation: {frame_time}') +
  transition_time(generation) +
  ease_aes('linear')
