############################################################################ 
## Install or load package

pkg = c("tidyverse","dplyr","ggplot2","readxl","maps", "mapproj", "shiny","cluster","Rtsne","rsconnect","geojsonio","broom","geojsonR","viridis","zipcodeR")


## If not installed, install and load all packages
package.check <- lapply(
  pkg,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

############################################################################ 
data <- read_excel("data/Telco_customer_churn.xlsx")
head(data)

## Add helpers
source("helpers.r")
source("ggplotMap.r")
spdf <- geojson_read("https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/communes.geojson",  what = "sp")
# Se queda con sólo los ZIP CODEs que empiecen con "03"
spdf <- spdf[ substr(spdf@data$code,1,2)  %in% c("03") , ]
spdf_fortified <- tidy(spdf, region = "code")
ggplot() +
  geom_polygon(data = spdf_fortified, aes( x = long, y = lat, group = group), fill="white", color="grey") +
  theme_void() +
  coord_map()

# read data
dataNEw <- read.table("https://raw.githubusercontent.com/holtzy/R-graph-gallery/master/DATA/data_on_french_states.csv", header=T, sep=";")
head(dataNEw)
# Make the merge
spdf_fortified = spdf_fortified %>%
  left_join(. , dataNEw, by=c("id"="depcom"))
# Note that if the number of restaurant is NA, it is in fact 0
spdf_fortified$nb_equip[ is.na(spdf_fortified$nb_equip)] = 0.001
ggplot() +
  geom_polygon(data = spdf_fortified, aes(fill = nb_equip, x = long, y = lat, group = group)) +
  theme_void() +
  coord_map()


url = "data/county_ca.geojson"
california <- geojson_read(url,  what = "sp")
california_fortified <- tidy(california, region = "NAME")
# Make the merge
head(data)

ciudades <- aggregate(Count ~ City, data, sum)

california_fortified = california_fortified %>%
  left_join(. , ciudades, by=c("id"="City"))
# Note that if the number of restaurant is NA, it is in fact 0
california_fortified$Count[ is.na(california_fortified$Count)] = 0.001

ggplotMap(california_fortified)

