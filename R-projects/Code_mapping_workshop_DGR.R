
## These lines are just making sure your packages are installed.
## If you already have these packages installed you can ignore these two lines

install.packages("tidyverse")
install.packages("sf")

## These lines make the packages available to R in the current session
## You only need to run these lines once each time you open R

library(sf)
library(tidyverse)

## This function will import your csv file containing XY point coordinates in DD.dddd
## Load the Dinokeng_Survey_Sites.csv file

sites <- read_csv("Dinokeng_Survey_Sites.csv")

## R automatically treats all numbers as a continuous variable.
## However in this instance the numbers assigned to label the sites are acting as categories
## We can therefore use this line of code to tell R to treat these numbers as categorical rather than continuous

sites$Site <- as.factor(sites$Site)

## Now we can use ggplot to plot XY coordinates from Dinokeng_Survey_Sites. 
## coord_sf() sets the boundaries of the plot to the first dataset plotted (relevant later!)

ggplot() + 
  geom_point(data = sites, aes(x = Longitude, y = Latitude), size = 1,
             shape = 21, color = "black") +
  labs(title = "Dinokeng Survey Sites") + 
  coord_sf()

## Polygons are more of a challenge as they have a specific format
## We are using shapefiles (used in GIS software)
## Use st_read to import the file - select DGR_boundary.shp by navigating to the correct folder

DGR_boundary <- st_read(file.choose())


## Create new plot including DGR boundary using geom_sf for plotting the polygon in the shapefile
## Each line that starts with geom_* plots a different dataset
## It plots each dataset in turn and on top of each other

ggplot() + 
  geom_sf(data = DGR_boundary, size = 1, color = "black", fill = "darkolivegreen3") + 
  geom_point(data = sites, aes(x = Longitude, y = Latitude), size = 1,
             shape = 21, color = "black") +
  labs(title = "Dinokeng Survey Sites") + 
  coord_sf()


## By adding "colour =" into the geom_point() line, we can plot different colours for different herds

ggplot() + 
  geom_sf(data = DGR_boundary, size = 1, color = "black", fill = "darkolivegreen3") + 
  geom_point(data = sites, aes(x = Longitude, y = Latitude, color = Site), size = 1,
             shape = 21) +
  labs(title = "Dinokeng Survey Sites") + 
  coord_sf()


## This looks quite messy, so instead we can label the points themselves
## We use the geom_text() function for this

ggplot() + 
  geom_sf(data = DGR_boundary, size = 1, color = "black", fill = "darkolivegreen3") + 
  geom_point(data = sites, aes(x = Longitude, y = Latitude), size = 1,
             shape = 21) +
  geom_text(data = sites, aes(x = Longitude, y = Latitude, label = sites$Site), hjust=0,vjust=0) +
  labs(title = "Dinokeng Survey Sites") + 
  coord_sf()



## Add extra shapefiles to add detail of rivers within the reserve
## Run these lines separately to ensure you're selecting the correct .shp file each time
## N.B. always plot shapefiles in order of size - largest to smallest
## Large shapefiles will overlap smaller ones and they will be lost to view

DGR_rivers <- st_read(file.choose())


## Plot all together

ggplot() + 
  geom_sf(data = DGR_boundary, size = 1, color = "black", fill = "darkolivegreen3") + 
  geom_sf(data = DGR_rivers, size = 1, color = "cadetblue1") +
  geom_point(data = sites, aes(x = Longitude, y = Latitude), size = 1,
             shape = 21) +
  geom_text(data = sites, aes(x = Longitude, y = Latitude, label = sites$Site), hjust=0,vjust=0) +
  labs(title = "Dinokeng Survey Sites") + 
  coord_sf()


## Adding a north arrow and scale bar - need to install a new package for this

install.packages("ggspatial")
library(ggspatial)


## Replot data with North arrow and scale bar
## Edit the location of the North arrow by editing the units of pad_x and pad_y

ggplot() + 
  geom_sf(data = DGR_boundary, size = 1, color = "black", fill = "darkolivegreen3") + 
  geom_sf(data = DGR_rivers, size = 1, color = "cadetblue1") +
  geom_point(data = sites, aes(x = Longitude, y = Latitude), size = 1,
             shape = 21) +
  geom_text(data = sites, aes(x = Longitude, y = Latitude, label = sites$Site), hjust=0,vjust=0) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(1.5, "in"), pad_y = unit(1.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  labs(title = "Dinokeng Survey Sites") + 
  coord_sf()

