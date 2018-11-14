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
library(ggsn)
# set factors to false
options(stringsAsFactors = FALSE)