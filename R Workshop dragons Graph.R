#CHECK PACKAGES 

#load data 
dragons <- read_csv("dragons_data.csv")

#INSPECT DATA
view(dragons)
head(dragons)

#IS THERE A REALTIONSHIP BETWEEN SIZE OF DRAGON AND ITS INTELLIGENCE?
#ARE LARGER DRAGONS MORE INTELLIGENT?
#SCATTER PLOT - GOOD FOR RELATIONSHIP/VS
#alpha is shade and colour is colour 
#we use mapping aes when we need ggplot to look back at the data and use it

ggplot(data = dragons, mapping = aes(x = bodyLength, y = testScore)) +
  geom_point(mapping = aes( colour = mountainRange), size = 3) + 
  geom_smooth() + 
  labs(title = "Relationship between dragon size and intelligence",
subtitle = "Between different mountain ranges", x = "Body length (m)",
y = "Test score (%)", 
colour = "Mountain range") + 
  theme_bw()

#IS THERE A DIFFERENCE IN DRAGON INTELLIGENCE BETWEEN MOUNTAIN RANGES?
#WHICH MOUNTAINRANGE HAS THE MOST INTELLIGENT DRAGONS?
#BAR GRAPH

#CREATE A SUMMARY DATA SET
dragons_grouped <- group_by(dragons, mountainRange)
dragons_summary <- summarise(dragons_grouped, average_dragon = mean(testScore), 
                             sd_dragon = sd(testScore), 
                             se_dragon = sd(testScore)/sqrt(n()))

#Create our bar graph 
ggplot(data = dragons_summary, mapping = aes(x = mountainRange, y = average_dragon)) + 
  geom_bar(stat = "identity", colour = "purple", fill = "pink") +
  geom_errorbar(mapping = aes(ymin = average_dragon - se_dragon, ymax = average_dragon + se_dragon), width = 0.6) +
  theme_bw()+
  labs(title = "Difference in dragon intelligence between mountain ranges", 
       x = "Mountain range",
       y = "Test score (%)") 
       
  


