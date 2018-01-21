##===============================================================================##
##                      ROAD ACCIDENTS IN SOUTH AUSTRALIA                        ##
##===============================================================================##


## R version 3.3.1 (2016-06-21)


#-------#
# Setup #
#-------#

# Install and load pacman if not already installed
if (!require("pacman")) install.packages("pacman")
library(pacman)

# Load packages
p_load(ggmap, ggplot2, ggthemes, magrittr, maps, mapdata, maptools, proj4, raster, rgdal, sp, stringr, tidyverse)


#---------------#
# Download data #
#---------------#

# Download, unzip and import data on road crashes (2016)
temp <- tempfile()
download.file("https://data.sa.gov.au/data/dataset/21386a53-56a1-4edf-bd0b-61ed15f10acf/resource/446afe5b-4e01-4cdf-a281-edd25aaf3802/download/road-crashes-in-sa-2014-16.zip", temp, mode = "w")
unzip(temp)
accidents <- read_csv("2016_DATA_SA_Crash.csv")
unlink(temp)


#----------------#
# Data wrangling #
#----------------#

# Clean column names
colnames(accidents) <- gsub(" ", "_", str_trim(tolower(names(accidents))), fixed = TRUE)

# Add column for fatality of crash
accidents$fatality <- NA # prevents error message for tibble
accidents$fatality[accidents$csef_severity == "4: Fatal"] <- "Fatal"
accidents$fatality[accidents$csef_severity != "4: Fatal"] <- "Non-fatal"

# Add column for total injured
accidents$total_in <- accidents$total_si + accidents$total_mi

# Recode type of crash
accidents$crash_type[accidents$crash_type == "Left Road - Out of Control"] <- "Left Road"


#------------------------#
# Spatial data wrangling #
#------------------------#

# Projection for accident coordinates (EPSG:3107)
proj <- "+proj=lcc +lat_1=-28 +lat_2=-36 +lat_0=-32 +lon_0=135 +x_0=1000000 +y_0=2000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

# Code spatial points and transform to data frame
points <- proj4::project(accidents[, c("accloc_x", "accloc_y")], proj = proj, inverse = TRUE)
accidents$longitude <- points$x
accidents$latitude <- points$y
names(accidents)[names(accidents) == "longitude"] <- "long"
names(accidents)[names(accidents) == "latitude"] <- "lat"
accidents_df <- as.data.frame(accidents)

# Download, unzip and import Australian shapefiles
temp <- tempfile()
download.file("http://data.daff.gov.au/data/warehouse/nsaasr9nnd_022/nsaasr9nnd_02211a04es_geo___.zip", temp, mode = "w")
unzip(temp)
aus_shp <- readShapeSpatial("aust_cd66states.shp", proj4string = CRS("+proj=longlat +ellps=WGS84"))
unlink(temp)

# Subset South Australia
sa_shp <- subset(aus_shp, STE == 4)


#-------------------#
# Mapping accidents #
#-------------------#

# Set theme for maps
## Theme adapted from #http://ellisp.github.io/blog/2017/10/15/traffic-crashes
map_theme <- theme_map(base_family = "Avenir") +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = rel(1), face = "bold"),
        plot.caption = element_text(colour = "grey50")) 

# Plot road accidents (full Australian map)
ggplot() + 
  geom_polygon(data = aus_shp, aes(x = long, y = lat, group = group)) +
  geom_point(data = accidents_df, aes(x = long, y = lat, colour = fatality), size = 1) +
  labs(x = "", y = "", title = "Road crashes in South Australia, 2016", subtitle = "(fatal and non-fatal crashes)", 
       caption = "Data source: South Australian Government Data Directory") + map_theme +
  scale_color_manual(name = "Severity", values = c("Fatal" = "red2", "Non-fatal" = "grey")) +
  coord_equal() 

# Plot road accidents (South Australia only)
## Note: Point size represents total number of casualties (divided by 1.5 for aesthetic reasons)
ggplot() + 
  geom_polygon(data = sa_shp, aes(x = long, y = lat, group = group)) +
  geom_point(data = accidents_df, aes(x = long, y = lat, colour = fatality, size = total_cas/1.5)) +
  labs(x = "", y = "", title = "Road crashes in South Australia, 2016", subtitle = "(by severity and number of casualties)", 
       caption = "Data source: South Australian Government Data Directory") + map_theme +
  scale_color_manual(name = "Severity", values = c("Fatal" = "red2", "Non-fatal" = "grey")) +
  scale_size_identity() + coord_equal() 

# Plot accidents in Adelaide using ggmap
adelaide <- get_map("Adelaide, Australia", maptype = "roadmap", zoom = 11)

ggmap(adelaide) +
  geom_point(data = accidents_df, aes(x = long, y = lat), alpha = 0.2) +
  map_theme + labs(title ="Road crashes in Adelaide, Australia", subtitle = "Total crashes in 2016", 
                   caption = "Data source: South Australian Government Data Directory") +
  theme(legend.position = "none") + coord_map()


#------------------------------------------#
# Visualizing no. of crashes by crash type #
#------------------------------------------#

# Set theme for visualizations
viz_theme <- theme(
  strip.background = element_blank(),
  strip.text = element_text(size = rel(1), face = "bold"),
  plot.caption = element_text(colour = "grey50"),
  axis.text.x = element_text(angle = 65, vjust = 0.5),
  text = element_text(family = "Avenir"))

# Plot number of crashes by type of crash
ggplot(accidents_df, aes(crash_type)) + 
  geom_bar(color = "red", fill = "black", width = 0.8) +
  labs(x = "", y = "", title = "Number of crashes by crash type", subtitle = "South Australia, 2016",
       caption = "Data source: South Australian Government Data Directory") + 
  viz_theme + ylim(0, 5000)

# Plot number of crashes by crash type and severity
ggplot(accidents_df, aes(crash_type)) + 
  geom_bar(aes(fill = fatality), width = 0.8) +
  labs(x = "", y = "", title = "Number of crashes by crash type and severity", subtitle = "South Australia, 2016",
       caption = "Data source: South Australian Government Data Directory") + 
  scale_fill_manual(name = "Severity", values = c("Fatal" = "red2", "Non-fatal" = "grey35")) +
  viz_theme + ylim(0, 5000) # + theme(legend.position = "bottom")


#--------------------------------------#
# Visualizing casualties by crash type #
#--------------------------------------#

# Calculate total number of casualties by crash type
accidents_cas <- accidents_df[, c("crash_type", "total_cas")]
accidents_cas %<>% 
  group_by(crash_type) %>% 
  summarise(cas_sum = sum(total_cas))
names(accidents_cas) <- c("crash_type", "total_cas")

# Order crash types by number of casualties
accidents_cas_ordered <- accidents_cas %>%
  arrange(total_cas, crash_type) %>%              
  mutate(crash_type = factor(crash_type, unique(crash_type)))

# Plot number of casualties by crash type (ordered)
ggplot(accidents_cas_ordered, aes(crash_type, total_cas)) + 
  geom_bar(stat = "identity", width = 0.8, color = "red", fill = "black") +
  labs(x = "", y = "", title = "Number of casualties (fatal and non-fatal) by crash type", subtitle = "South Australia, 2016",
       caption = "Data source: South Australian Government Data Directory") +
  viz_theme + ylim(0, 2000)


#--------------------------------------#
# Visualizing fatalities by crash type #
#--------------------------------------#

# Calculate total number of fatalities by crash type
accidents_fats <- accidents_df[, c("crash_type", "total_fats")]
accidents_fats %<>% 
  group_by(crash_type) %>% 
  summarise(cas_sum = sum(total_fats))
names(accidents_fats) <- c("crash_type", "total_fats")

# Order crash types by number of casualties
accidents_fats_ordered <- accidents_fats %>%
  arrange(total_fats, crash_type) %>%              
  mutate(crash_type = factor(crash_type, unique(crash_type)))

# Plot number of fatalities by crash type (ordered)
ggplot(accidents_fats_ordered, aes(crash_type, total_fats)) + 
  geom_bar(stat = "identity", width = 0.8, color = "red", fill = "black") +
  labs(x = "", y = "", title = "Number of fatalities by crash type", subtitle = "South Australia, 2016",
       caption = "Data source: South Australian Government Data Directory") +
  viz_theme + ylim(0, 30)


#-----------------------#
# Modern visualizations #
#-----------------------#

## Inspired by: http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html#Bar%20Chart

# Lollipop chart of number of casualties by crash type 
ggplot(accidents_cas, aes(x = crash_type, y = total_cas)) + 
  geom_point(size = 3, color = "red", fill = "grey35") + 
  geom_segment(aes(x = crash_type, 
                   xend = crash_type, 
                   y = 0, 
                   yend = total_cas)) + 
  labs(x = "", y = "", title = "Number of casualties (fatal and non-fatal) by crash type", subtitle = "South Australia, 2016",
       caption = "Data source: South Australian Government Data Directory") +
  viz_theme + ylim(0, 2000)

# Dot plot of number of casualties by crash type (ordered)
ggplot(accidents_cas_ordered, aes(x = crash_type, y = total_cas)) + 
  geom_point(color = "red", size = 3) + 
  geom_segment(aes(x = crash_type, 
                   xend = crash_type, 
                   y = min(total_cas), 
                   yend = max(total_cas)), 
               linetype = "dashed", 
               size = 0.1) +
  labs(x = "", y = "", title = "Number of casualties (fatal and non-fatal) by crash type", subtitle = "South Australia, 2016",
     caption = "Data source: South Australian Government Data Directory") +
  theme_bw() + ylim(0, 2000) + coord_flip()