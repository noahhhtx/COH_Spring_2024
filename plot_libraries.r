library(readr)
library(sf)
library(tmap)
library(tidyverse)
library(dplyr)
library(stringr)
library(RColorBrewer)
library(tidygeocoder)

harris_zip = st_read("harris_map.geojson")
tx_hwys = st_read("./geographic_data/TxDOT_National_Highway_System.geojson")
library_data = read.csv("./geographic_data/houston_libraries.csv")

library_data = library_data %>% geocode(Address, method="arcgis", lat=latitude, long=longitude)
library_data_shape = st_as_sf(library_data, coords=c("longitude","latitude"))
st_write(library_data_shape, "libraries.geojson")

lib_plot = tm_shape(harris_zip) + tm_polygons(col="white", border.alpha=0.5) + tm_shape(tx_hwys) + tm_lines() + tm_shape(library_data_shape) + tm_dots(size=0.03, col="System", palette=c("red","blue"), title="Houston Libraries") + tm_legend(bg.color="white")
tmap_save(lib_plot, "./plots/libraries.png")