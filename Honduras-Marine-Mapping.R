
## Install packages needed for locator map

install.packages("tidyverse")
install.packages("sf")
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")
install.packages("ggspatial")
install.packages("rgeos")


## Load packages from library

library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(rgeos)

## Set theme (background) to black and white

theme_set(theme_bw())

## Create a new shapefile with all the country boundaries of the world in it

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)


## Plot the world

ggplot(data = world) +
  geom_sf()


## Plot the world with labels and colours
ggplot(data = world) +
  geom_sf(color = "black", fill = "lightgreen") +
  labs(title = "World Map", subtitle = "My subtitle",
       x = "Longitude", y = "Latitude")


## Add the north arrow and scale bar
## Use pad_x and pad_y to position north arrow and scale bar

ggplot(data = world) +
  geom_sf(color = "black", fill = "lightgreen") +
  labs(title = "World Map", subtitle = "My subtitle",
       x = "Longitude", y = "Latitude") +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)


## Adding point where research location is
## First you need to load a .csv file with one coordinate for your location
## You can create this yourself for any given location!

location <- read_csv(file.choose())
  

## Add point for the study location

ggplot(data = world) +
  geom_sf(color = "black", fill = "lightgreen") +
  geom_point(data = location, aes(x = Longitude, y = Latitude), size = 5) +
  labs(title = "World Map", subtitle = "My subtitle",
       x = "Longitude", y = "Latitude") +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)

## Edit your locator map and add labels as required
## Export map