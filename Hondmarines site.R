#INSTALL PACKAGES 
install.packages("tidyverse")
install.packages("sf")

#LOAD PACKAGES 
library(tidyverse)
library(sf)

#LOAD DATA 

sites_fish <- read_csv("Hondmarines.csv")

sites_fish$Site <- as.factor(sites_fish$Site)

#BUILD MAPPING 

ggplot() + 
  geom_point(data = sites_fish, aes(x = Longitude, y = Latitude), size = 1, 
             shape = 21, colour = "black")+  
  labs(title = "Hondmarines Survey")+  
  coord_sf()

#LOAD SHAPEFILE

Honduras_shallow <- st_read(file.choose())

#BUILD ON MAP WITH SHAPEFILE 

ggplot()+
  geom_sf(data = Honduras_shallow, size = 1, colour = "purple", fill = "pink") + 
  geom_point(data = sites_fish, aes(x = Longitude, y = Latitude), size = 1, 
             shape = 21, colour = "black")+
  geom_text(data = sites_fish, aes(x = Longitude, y = Latitude, label = sites_fish$Site), hjust = 0.1, vjust = 1.5)+
  labs(title = "Hondmarines Survey") +
  coord_sf() 

#ADD DATA 

Honduras_utila <- st_read(file.choose())

ggplot()+
  geom_sf(data = Honduras_shallow, size = 1, colour = "purple", fill = "pink") +
  geom_sf(data = Honduras_utila, size = 1, colour = "orange")+
  geom_point(data = sites_fish, aes(x = Longitude, y = Latitude), size = 1, 
             shape = 21, colour = "black") +
  geom_text(data = sites_fish, aes(x = Longitude, y = Latitude, label = sites_fish$Site), hjust = 0.1, vjust = 1.5)+
  labs(title = "Honduras Marine Survey") +
  coord_sf() 

#INSTALL PACKAGES

install.packages("ggspatial")
library(ggspatial)

#ADD NORTH ARROW 
#BL MEANS BOTTOM LEFT, YOU CAN HAVE TL TOP LEFT ETC

ggplot()+
  geom_sf(data = Honduras_shallow, size = 1, colour = "purple", fill = "pink") +
  geom_sf(data = Honduras_utila, size = 1, colour = "orange")+
  geom_point(data = sites_fish, aes(x = Longitude, y = Latitude), size = 1, 
             shape = 21, colour = "black") +
  geom_text(data = sites_fish, aes(x = Longitude, y = Latitude, label = sites_fish$Site), hjust = 0.1, vjust = 1.5)+
  annotation_scale(location = "br", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", pad_x = unit(0.2, "in"), pad_y = unit(2, "in"),
                         style = north_arrow_fancy_orienteering) + 
  labs(title = "Honduras Marine Survey") +
  coord_sf()   



