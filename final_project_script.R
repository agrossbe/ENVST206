#install mapproj to be used to map the location of the weather stations
#install.packages("mapproj")
#install ggmap to be used to map the location of the weather stations
#install.packages("ggmap")
#install tidyverse to be used to map loaction of stations
#install.packages("tidyverse")
#load packages for data analysis
library(ggplot2)
library(mapproj)
library(ggmap)
library(tidyverse)

#read in station info csv for reference for what stations to read in later on
station_info <- read.csv("C:\\Users\\Hephzibah\\Documents\\EnvDS\\station_info.csv")


get_spots <- get_map(lat=42.396863, lon=-74.424662, zoom=10)
