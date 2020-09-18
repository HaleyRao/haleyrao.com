# load libraries
library(tidyverse)
library(leaflet)

# read in data
all_tornadoes <- read.csv("1950-2018_actual_tornadoes.csv")

# filter out tornadoes with no known end location
map_tornadoes <- filter(all_tornadoes, elat != 0 & elon != 0)
summary(map_tornadoes$elat)
summary(map_tornadoes$elon)

# filter out tornadoes not located in the continental United States, those occurring 2014 and earlier, and tornado paths
# less than 2 miles long
map_tornadoes <- filter(map_tornadoes, elat > 24.5465 & elon < 124.4094 & yr > 2014 & len > 2)


# interactive map, line represents tornado path, can zoom in and out, pop up window with some info (location, strength)
# color of line on a scale to show strength

tornado_interactive_map <- leaflet() %>%
  addTiles()

pal <- colorNumeric(
  palette = "YlOrRd",
  domain = map_tornadoes$mag)

# add lines to show each tornado path and color the paths according to the tornado strength
for(i in 1:nrow(map_tornadoes)){
  tornado_interactive_map <- addPolylines(tornado_interactive_map, lat = as.numeric(map_tornadoes[i, c(16, 18)]), 
                       lng = as.numeric(map_tornadoes[i, c(17, 19)]), color = pal(map_tornadoes$mag)[i])
}

# make a content column with date, magnitude, length
test <- paste(map_tornadoes$date[1], map_tornadoes$st[1], map_tornadoes$mag[1], sep = "\n")
cat(test)

dates <- paste("Date: ", as.character(map_tornadoes$date))
lens <- paste("Length: ", as.character(map_tornadoes$len), " miles")
mags <- paste("Magnitude: EF", as.character(map_tornadoes$mag), sep = "")

content <- paste(dates, lens, mags, sep = ", ")
map_tornadoes$content <- content


# add colored circles at starting point, label with content
tornado_interactive_map <- tornado_interactive_map %>% addCircleMarkers(lng = map_tornadoes$slon, lat = map_tornadoes$slat, popup = map_tornadoes$content, 
                                             color = pal(map_tornadoes$mag), stroke = FALSE, fillOpacity = 1, radius = 5)

tornado_interactive_map

library(htmlwidgets)
saveWidget(tornado_interactive_map, file="tornado_interactive_map.html")
#######################
