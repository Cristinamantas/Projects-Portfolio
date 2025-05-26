install.packages("tidyverse")
library(tidyverse)

#Load data 
 fish_raw2 <- read.csv("Work_experience_fish_RAW.csv")
 fish_cleaned <- read.csv("Work_experience_fish_CLEANED.csv")

#INSPECT DATA 
view(fish_raw2)
head(fish_raw) 

#CORRELATION BETWEEN LENGTH AND BIOMASS IN HEALTH IN FISH? 

ggplot(data = fish_raw, mapping = aes(x = Length.mm., y = Biomass.g.)) +
geom_point(mapping = aes(colour = Site), size = 2) + 
  geom_smooth()+
labs(title = "Relationship between Length and Biomass", 
     subtitle = "Between different sites", x = "Length(%)", y = "Biomass(g)", 
     colour = "Site") + 
       theme_bw()

cor.test(fish_raw$Length.mm., fish_raw$Biomass.g., method = "pearson")


