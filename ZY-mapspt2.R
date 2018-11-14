# load libraries
library(raster)
install.packages('rgdal')
library(rgdal)
library(ggplot2)
#install.packages('broom')
library(broom)
library(RColorBrewer)
install.packages('rgeos')
library(rgeos)
library(dplyr)
# note that you don't need to call maptools to run the code below but it needs to be installed.
library(maptools)
# to add a north arrow and a scale bar to the map
install.packages('ggsn')
library(ggsn)
# set factors to false
options(stringsAsFactors = FALSE)
##import data##
##roads##
sjer_roads <- readOGR("C:/Users/zippy/OneDrive/NDSU/advanced_ecology/earthanalyticswk4/california/madera-county-roads/tl_2013_06039_roads.shp")
##view the original class of the TYPE column##
class(sjer_roads$RTTYP)
##unique is a cool function##
unique(sjer_roads$RTTYP)
##quick plot using base plot (sigh)##
plot(sjer_roads,
     main = "Quick plot of roads data")
##want to visualize NAs, change to unknowns##
sjer_roads$RTTYP[is.na(sjer_roads$RTTYP)] <- "Unknown"
class(sjer_roads$RTTYP)
unique(sjer_roads$RTTYP)
plot(sjer_roads,
     main = "Quick plot of roads data")
##tidying to make df so we can use ggplotmasterrace##
sjer_roads.df <- tidy(sjer_roads, region = "id")
sjer_roads$id <- rownames(sjer_roads@data)
##join the attribute table from the spatial object to the new data frame#
sjer_roads.df <- left_join(sjer_roads.df,
                           sjer_roads@data,
                           by = "id")
##ggplot##
gg <- ggplot() +
  geom_path(data = sjer_roads.df, aes(x = long, y = lat, group = group)) +
  labs(title = "ggplot map of roads")
gg

##NOW WITH COLOR!##
ggc <- ggplot() +
  geom_path(data = sjer_roads.df, aes(x = long, y = lat,
                                      group = group, color = factor(RTTYP))) +
labs(color = 'Road Types', # change the legend type
     title = "Roads colored by the RTTP attribute")
ggc

##count the number of unique values or levels##
length(levels(sjer_roads$RTTYP))
road_palette <- c("C" = "green",
                  "M" = "grey40",
                  "S" = "purple",
                  "Unknown" = "grey")
road_palette

##custom color and linie width##
ggplot() +
  geom_path(data = sjer_roads.df, aes(x = long, y = lat,
                                      group = group, color=factor(RTTYP))) +
      scale_colour_manual(values = road_palette) +
  labs(title = "Madera County Roads ",
       subtitle = "Colored by road type")
