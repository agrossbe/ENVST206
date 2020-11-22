#install packages to use ggplot2 to make a map
#install.packages(c("cowplot", "googleway", "ggrepel", "ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata"))
#install.packages("lwgeom")

library(ggplot2)
theme_set(theme_bw())
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(maps)
library(tools)

#read in station info
station_info <- read.csv("C:\\Users\\Hephzibah\\Documents\\EnvDS\\station_info.csv")

#make a data frame of just the long, lat and station ID
sites <- data.frame(longitude = station_info$long, latitude = station_info$lat, ID = station_info$station_id)

#make a dataframe world to be used to make a map
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

#convert polygons into sf for state and county polygons
states <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
counties <- st_as_sf(maps::map("county", plot = FALSE, fill = TRUE))
#make a data frame of just the NY counties
ny_counties <- subset(counties, grepl("new york", counties$ID))
ny_counties$area <- as.numeric(st_area(ny_counties))

#make a map focused on NY
ggplot(data=world)+
  geom_sf()+
  geom_sf(data = states, fill = NA)+ #adds the state outlines
  geom_sf(data = wc_bounds, fill = NA, color = "tomato3")+
  coord_sf(xlim = c(-74.5, -73.5), ylim = c(40.8, 42), expand = FALSE)+ # limits the map to just be NYS
  geom_point(data = sites, aes(x=longitude, y = latitude), size = 1, shape = 19, fill = "tomato3") #adds weather stations as points

#make data set of sites as sf to combine lat and long as a vector
sites_sf <- st_as_sf(sites, coords = c("longitude", "latitude"), crs = 4326)

#make a data frame of the station names in Westchester County
wc_bounds <- counties[counties$ID == "new york,westchester", ]

#get stations that are in Westchester, %>% converts from sgbp to list of logicals
in_wc <- sf::st_intersects(sites_sf, wc_bounds$geom, sparce = FALSE) %>%lengths>0

#make list of station numbers that are in westchester
wc_station_numb <- which(in_wc)
print(wc_station_numb)

#read in the new csv of just the stations in westchester from excel creating using the list of stations found above
west_stations <- read.csv("C:\\Users\\Hephzibah\\Documents\\EnvDS\\westchester_stations.csv")
west_stations_reform <- data.frame(longitude = west_stations$elev, latitude = west_stations$long, ID = west_stations$lat)

#make a new map with just the westchester stations
ggplot(data=world)+
  geom_sf()+
  geom_sf(data = states, fill = NA)+ #adds the state outlines
  geom_sf(data = wc_bounds, fill = NA, color = "tomato3")+
  coord_sf(xlim = c(-74.5, -73.5), ylim = c(40.8, 42), expand = FALSE)+ # limits the map to just be NYS
  geom_point(data = west_stations_reform, aes(x=longitude, y = latitude), size = 1, shape = 19, fill = "tomato3") #adds weather stations in Westchester

#read in precip data for all the stations in Westchester
wc_ap_prcp_all <- read.csv("C:\\Users\\Hephzibah\\Documents\\EnvDS\\stations\\USW00094745_prcp.csv")

#make data set of all the wc prcp data combine
wc_ap_prcp <- na.omit(data.frame(date=wc_ap_prcp$date,
                                 prcp=wc_ap_prcp$prcp/10))

#remove all zeros
wc_ap_nonzero <- wc_ap_prcp[wc_ap_prcp$prcp >0, ]

#make old and new line at 1980
wc_ap_old <- wc_ap_nonzero[wc_ap_nonzero$date <= 1982, ]
wc_ap_new <- wc_ap_nonzero[wc_ap_nonzero$date >1982, ]

#get a quartile for old
quantile(wc_ap_old$prcp, c(0.99))

#data sets of how many greater than 99 precentile in old and new
wc_ap_old_ext <- wc_ap_old[wc_ap_old$prcp >68.175, ]
wc_ap_new_ext <- wc_ap_new[wc_ap_new$prcp >68.175, ]

#calculate means of extremes
mean_old_ext <- mean(wc_ap_old_ext$prcp)
mean_new_ext <- mean(wc_ap_new_ext$prcp)

#total prcp of extremes
total_old_ext <- sum(wc_ap_old_ext$prcp)
total_new_ext <- sum(wc_ap_new_ext$prcp)

#check for normality
shapiro.test(wc_ap_old_ext$prcp) 
shapiro.test(wc_ap_new_ext$prcp)

#make extreme data sets just vector of prcp data without date
old_ext_vec <- sort(wc_ap_old_ext$prcp)
new_ext_vec <- sort(wc_ap_new_ext$prcp)

#make extreme with name and prcp
new_ext_name <- data.frame(Year="1982-2018",
                           prcp=new_ext_vec,
                           Observation=1:53)
old_ext_name <- data.frame(Year="1946-1981",
                           prcp=old_ext_vec,
                           Observation=1:42)

#join old and new into one data set
all_ext <- full_join(new_ext_name, old_ext_name)

ggplot(data=all_ext, aes(x=Observation, y=prcp, fill=Year))+
  geom_bar(stat="identity", position = "dodge")+
  labs(x="Observation", y="Precipitation (mm)")+
  scale_fill_manual(values=c("mediumseagreen", "lightsalmon4"))

  