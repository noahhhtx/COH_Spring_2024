library(readr)
library(sf)
library(tmap)
library(tidyverse)
library(dplyr)
library(stringr)
library(RColorBrewer)

harris_zip = st_read("harris_map.geojson")
tx_hwys = st_read("./geographic_data/TxDOT_National_Highway_System.geojson")
libraries_schools = st_read("lib_school_shape.geojson")

plot = tm_shape(harris_zip) + tm_polygons(col="white", border.alpha=0.5) + tm_shape(tx_hwys) + tm_lines() + tm_shape(libraries_schools) + tm_dots(size=0.01, col="Type", palette=c("Library"="red", "Early Education"="blue", "Elementary School"="darkgreen", "Middle School" = "yellow3", "High School"="orange", "Elementary-Middle School" = "blueviolet", "Elementary-High School"="aquamarine4", "Middle-High School"="deeppink","K-12 School"="orangered4"), title = "Libraries and Schools") + tm_legend(outside=TRUE, bg.color="lightgray",outside.size=0.15)
tmap_save(plot,"./plots/libraries_and_schools.png")

for(x in unique(libraries_schools$Type)) {
  
  if(x=="Library") {next}
  temp = libraries_schools[libraries_schools$Type == x,]
  filepath = paste("./plots/",gsub(" ", "_", x),".png",sep="")
  temp_plot = tm_shape(harris_zip) + tm_polygons(col="white", border.alpha=0.5) + tm_shape(tx_hwys) + tm_lines() + tm_shape(temp) + tm_dots(size=0.01, col="red", title =x) + tm_layout(title=x)
  tmap_save(temp_plot, filepath)
  
}