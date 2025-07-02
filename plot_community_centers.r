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
community_centers = read.csv("./geographic_data/community_centers.csv")

community_centers = community_centers %>% geocode(Address, method="arcgis", lat=latitude, long=longitude)
community_centers_shape = st_as_sf(community_centers, coords=c("longitude","latitude"))
st_write(community_centers_shape, "community_centers.geojson")

msc_shape = community_centers_shape[community_centers_shape$Type=="Multi-Service Center",]
hpard_shape = community_centers_shape[community_centers_shape$Type=="HPARD Community Center",]
hccc_shape = community_centers_shape[community_centers_shape$Type=="Harris County Community Center",]

all_centers = tm_shape(harris_zip) + tm_polygons(col="white", border.alpha=0.5) + tm_shape(tx_hwys) + tm_lines() + tm_shape(community_centers_shape) + tm_dots(size=0.03, col="Type", palette=c("red","blue","green3"), title="Type") + tm_legend(bg.color="white") + tm_layout(main.title="Community Centers", main.title.position = "center")
tmap_save(all_centers, "./plots/community_centers.png")

ms_centers = tm_shape(harris_zip) + tm_polygons(col="white", border.alpha=0.5) + tm_shape(tx_hwys) + tm_lines() + tm_shape(msc_shape) + tm_dots(size=0.03, col="red") + tm_layout(main.title="Houston Multi-Service Centers", main.title.position = "center")
tmap_save(ms_centers, "./plots/ms_centers.png")

hpard_centers = tm_shape(harris_zip) + tm_polygons(col="white", border.alpha=0.5) + tm_shape(tx_hwys) + tm_lines() + tm_shape(hpard_shape) + tm_dots(size=0.03, col="Specification", palette=c("red","blue","green3","yellow3"), title="Location") + tm_legend(bg.color="white") + tm_layout(main.title="HPARD Community Centers", main.title.position = "center")
tmap_save(hpard_centers, "./plots/hpard_centers.png")

hccc_centers = tm_shape(harris_zip) + tm_polygons(col="white", border.alpha=0.5) + tm_shape(tx_hwys) + tm_lines() + tm_shape(hccc_shape) + tm_dots(size=0.03, col="Specification", palette=c("red","blue","green3","yellow3"), title="Precinct") + tm_legend(bg.color="white") + tm_layout(main.title="Harris County Community Centers", main.title.position = "center")
tmap_save(hccc_centers, "./plots/hccc_centers.png")