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
park_data = read.csv("./geographic_data/houston_parks_inventory_braeswood_omitted.csv")

park_data$count = 1
acres_by_zip = aggregate(park_data$TOTAL_ACRES, by=list(Name=park_data$ZIP_CODE), FUN=sum)
parks_by_zip = aggregate(park_data$count, by=list(Name=park_data$ZIP_CODE), FUN=sum)
acres_by_zip = acres_by_zip %>% rename(park_acres = x)
parks_by_zip = parks_by_zip %>% rename(parks = x)
acres_by_zip$Name = paste("<at><openparen>",acres_by_zip$Name,"<closeparen>",sep="")
parks_by_zip$Name = paste("<at><openparen>",parks_by_zip$Name,"<closeparen>",sep="")
harris_zip = merge(harris_zip, acres_by_zip, by.y="Name")
harris_zip = merge(harris_zip, parks_by_zip, by.y="Name")
harris_zip$acrage_rate = 100 * harris_zip$park_acres / harris_zip$total_area_acre
harris_zip$parks_per_capita = 10000 *harris_zip$parks / harris_zip$pop_2020
harris_zip$parkland_per_capita = 10000 * harris_zip$park_acres / harris_zip$pop_2020

pal = c(brewer.pal(9,"Greens"))
acrage_plot = (tm_shape(harris_zip) + tm_polygons(col="acrage_rate", border.alpha=0.25, palette=pal, style="fixed",breaks=c(0,1,2,3,4,5,6,7,8,9,10,Inf), title = "Percent of Area\nThat Is HPARD\nParkland") + tm_legend(bg.color="white", width=0.5) + tm_shape(tx_hwys) + tm_lines(col="black"))
tmap_save(acrage_plot,"./plots/acrage_plot.png")

park_data$FULL_ADDR = paste(park_data$ADDRESS, " ", park_data$STREET, ", ", park_data$ZIP_CODE, sep="")
lat_longs = data.frame(park_data$PARK.NAME, park_data$FULL_ADDR)

#certain addresses need to be cleaned...
lat_longs$park_data.FULL_ADDR[78] = "5901 Main, 77030"
lat_longs$park_data.FULL_ADDR[87] = "12414 Highway 6 S, 77498"
lat_longs$park_data.FULL_ADDR[16] = "911 Shepherd, 77019"
lat_longs$park_data.FULL_ADDR[19] = "11903 Pia Drive, 77044"
lat_longs$park_data.FULL_ADDR[21] = "6900 Banyan, 77028"
lat_longs$park_data.FULL_ADDR[35] = "2345 Maroneal, 77030"
lat_longs$park_data.FULL_ADDR[47] = "3600 Allen Pkwy, 77002"
lat_longs$park_data.FULL_ADDR[54] = "6401 Coppage, 77007"
lat_longs$park_data.FULL_ADDR[72] = "9680 Club Creek Drive, 77036"
lat_longs$park_data.FULL_ADDR[81] = "400 Westmont, 77015"
lat_longs$park_data.FULL_ADDR[83] = "19008 Saums, 77084"
lat_longs$park_data.FULL_ADDR[120] = "6818 Shadyvilla Ln, 77055"
lat_longs$park_data.FULL_ADDR[135] = "8800 Grandriver Dr, 77078"
lat_longs$park_data.FULL_ADDR[175] = "4201 W T C Jester Blvd, 77018"
lat_longs$park_data.FULL_ADDR[316] = "2701 E T C Jester Blvd, 77018"
lat_longs$park_data.FULL_ADDR[333] = "1400 W T C Jester Blvd, 77008"
lat_longs$park_data.FULL_ADDR[367] = "14850 White Heather, 77053"

lat_longs = lat_longs %>% geocode(park_data.FULL_ADDR, method="osm", lat=latitude, long=longitude)
lat_longs_2 = lat_longs[complete.cases(lat_longs),]
lat_longs_shape = st_as_sf(lat_longs_2, coords=c("longitude","latitude"))
acrage_plot_points = acrage_plot + tm_shape(lat_longs_shape) + tm_dots(size=0.01, col="red")
tmap_save(acrage_plot_points,"./plots/acrage_plot_points.png")

parks_per_capita = (tm_shape(harris_zip) + tm_polygons(col="parks_per_capita", border.alpha=0.25, palette=pal, title = "Parks per 10,000 People") + tm_legend(bg.color="white", width=0.5) + tm_shape(tx_hwys) + tm_lines(col="black"))
tmap_save(parks_per_capita,"./plots/parks_per_capita.png")

parks_per_capita_points = (tm_shape(harris_zip) + tm_polygons(col="parks_per_capita", border.alpha=0.25, palette=pal, title = "Parks per 10,000 People") + tm_legend(bg.color="white", width=0.5) + tm_shape(tx_hwys) + tm_lines(col="black")) + tm_shape(lat_longs_shape) + tm_dots(size=0.01, col="red")
tmap_save(parks_per_capita_points,"./plots/parks_per_capita_points.png")

parkland_per_capita = (tm_shape(harris_zip) + tm_polygons(col="parkland_per_capita", border.alpha=0.25, palette=pal, title = "Parkland per 10,000 People") + tm_legend(bg.color="white", width=0.5) + tm_shape(tx_hwys) + tm_lines(col="black"))
tmap_save(parkland_per_capita,"./plots/parkland_per_capita.png")

parkland_per_capita_points = (tm_shape(harris_zip) + tm_polygons(col="parkland_per_capita", border.alpha=0.25, palette=pal, title = "Parkland per 10,000 People") + tm_legend(bg.color="white", width=0.5) + tm_shape(tx_hwys) + tm_lines(col="black")) + tm_shape(lat_longs_shape) + tm_dots(size=0.01, col="red")
tmap_save(parkland_per_capita_points,"./plots/parkland_per_capita_points.png")

st_write(harris_zip, "park_shape.geojson")