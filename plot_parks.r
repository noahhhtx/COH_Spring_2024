library(tmap)
library(sf)
library(tidyr)
library(tidyverse)
library(dplyr)

get_min_distance <- function(center, park_list) {
  min = NA
  
  center_coords = st_coordinates(center)
  
  for(p in 1:nrow(park_list)) {
    
    park = park_list[p, "geometry"]
    park_coords = st_coordinates(park)
    x_diff = abs(center_coords[1] - park_coords[1])
    y_diff = abs(center_coords[2] - park_coords[2])
    dist = sqrt((x_diff^2) + (y_diff^2))
    if(is.na(min) | dist < min) { min = dist }
    
  }
  
  return(min)
}

houston_zip = st_read("composite_harris_map.geojson")
houston_zip = houston_zip %>% rename(ZIP = all_houston_zips)
tx_hwys = st_read("./geographic_data/TxDOT_National_Highway_System.geojson")
parks = st_read("./geographic_data/parks/ParkServe_Parks.shp")
library_data_shape = st_read("libraries.geojson")
community_centers = st_read("community_centers.geojson")
schools = st_read("lib_school_shape.geojson")
dumping_complaints = st_read("all_dumping_points_2023.geojson")
schools = schools[schools$Type != "Library",]
schools$Type = ifelse(schools$Type %in% c("K-12 School", "Elementary School", "Middle School", "High School", "Early Education"), schools$Type, "Other")
parks = parks[st_is_valid(parks),]
parks = parks[,!(names(parks) %in% "Park_DateA")]
parks = parks[!is.na(parks$ParkID),]
parks_houston = parks[parks$Park_Urban=="Houston, TX", ]
parks_houston = parks_houston[!is.na(parks_houston$ParkID),]
parks_in_zip = parks[parks$Park_Zip %in% houston_zip$ZIP, ]
parks_in_zip = parks_in_zip[!is.na(parks_in_zip$ParkID),]
hou_parkland = tm_shape(houston_zip) + tm_polygons() + tm_shape(parks_in_zip) + tm_polygons(border.col = "green4", col="green4", alpha=0.5, border.alpha=0.5) + tm_shape(tx_hwys) + tm_lines()
tmap_save(hou_parkland, "Parkland_in_ZIP.png", width=20, height=20)

mental_map = (tm_shape(houston_zip) + tm_polygons(col="poor_mental_health_14_days_2021", border.alpha=0.1, palette="Reds", title="Poor Mental Health,\n14+ Days (Percent)", colorNA=NULL) + tm_legend(bg.color="white") + tm_shape(tx_hwys) + tm_lines(col="black")) + tm_shape(parks_in_zip) + tm_polygons(border.col = "green4", col="green4", alpha=0.5, border.alpha=0.5) + tm_layout(main.title = "Mental Health and Parkland (2021)", main.title.position = "center")
tmap_save(mental_map, "./plots/mental_health_parkland.png")

vc_2023_map = (tm_shape(houston_zip) + tm_polygons(col="vc_pc_2023", border.alpha=0.1, palette="Reds", breaks=c(-Inf,250,500,750,1000,1250,1500,1750,2000,2250,2500,2750,3000,3250,3500,3750,4000,4250,4500,4750,5000,5250,5500,5750,6000,6250,6500,6750,7000,Inf), colorNA="gray", textNA="Out of Jurisdiction", title="Violent Crime Rate", colorNA=NULL) + tm_legend(bg.color="white", outside=TRUE,outside.size=0.15) + tm_shape(tx_hwys) + tm_lines(col="black")) + tm_shape(parks_in_zip) + tm_polygons(border.col = "green4", col="green4", alpha=0.5, border.alpha=0.5) + tm_layout(main.title = "Violent Crimes and Parkland (2023)", main.title.position = "center")
tmap_save(vc_2023_map, "./plots/violent_crime_parkland.png")

lib_plot = tm_shape(houston_zip) + tm_polygons(col="white", border.alpha=0.5) + tm_shape(tx_hwys) + tm_lines() + tm_shape(parks_in_zip) + tm_polygons(border.col = "green4", col="green4", alpha=0.5, border.alpha=0.5) + tm_shape(library_data_shape) + tm_dots(size=0.03, col="System", palette=c("red","blue"), title="Library Ststem") + tm_legend(bg.color="white") + tm_layout(main.title = "Libraries and Parkland", main.title.position = "center")
tmap_save(lib_plot, "./plots/libraries_and_parks.png")

cc_plot = tm_shape(houston_zip) + tm_polygons(col="white", border.alpha=0.5) + tm_shape(tx_hwys) + tm_lines() + tm_shape(parks_in_zip) + tm_polygons(border.col = "green4", col="green4", alpha=0.5, border.alpha=0.5) + tm_shape(community_centers) + tm_dots(size = 0.03, col="Type", palette=c("red","blue","purple3"), title="Community Center Type") + tm_legend(bg.color="white") + tm_layout(main.title = "Community Centers and Parkland", main.title.position='center')
tmap_save(cc_plot, "./plots/community_centers_parkland.png")

schools_plot = tm_shape(houston_zip) + tm_polygons(col="white", border.alpha=0.5) + tm_shape(tx_hwys) + tm_lines() + tm_shape(parks_in_zip) + tm_polygons(border.col = "green4", col="green4", alpha=0.5, border.alpha=0.5) + tm_shape(schools) + tm_dots(size = 0.03, col="Type", palette=c("red","blue","purple3", "brown4", "yellow4", "orange"), title="School Type") + tm_legend(bg.color="white") + tm_layout(main.title = "Schools and Parkland", main.title.position='center')
tmap_save(schools_plot, "./plots/schools_parkland.png")

violent_codes = c("120","13A", "13B", "13C", "09A", "09B", "100", "11A", "11B", "11C", "11D", "36A", "36B")
crime_2023 = read.csv("./crime_data/NIBRSPublicView2023.csv")
#crime_2023 = crime_2023[crime_2023$Beat!=""&crime_2023$Beat!="OOJ",]
crime_2023$ZIPCode = substring(crime_2023$ZIPCode,1,5)
violence_2023 = crime_2023[crime_2023$NIBRSClass %in% violent_codes,]
violence_2023 = violence_2023[!is.na(violence_2023$MapLongitude) & !is.na(violence_2023$MapLatitude),]
violence_2023 = st_as_sf(violence_2023, coords=c("MapLongitude","MapLatitude"))

crime_parkland = tm_shape(houston_zip) + tm_polygons(alpha=0) + tm_shape(parks_in_zip) + tm_polygons(border.col = "green4", col="green4", alpha=0.5, border.alpha=0.5) + tm_shape(tx_hwys) + tm_lines() + tm_shape(violence_2023) + tm_dots(size=0.0005, col="red") + tm_layout(main.title = "Violent Crimes and Parkland (2023)", main.title.position = "center")
tmap_save(crime_parkland, "./plots/Violent_Crime_Points_Parkland.png", width=35, height=35)
tmap_save(crime_parkland, "./plots/Violent_Crime_Points_Parkland_Smaller.png", width=20, height=20)

dumping_parkland = tm_shape(houston_zip) + tm_polygons(alpha=0) + tm_shape(parks_in_zip) + tm_polygons(border.col = "green4", col="green4", alpha=0.5, border.alpha=0.5) + tm_shape(tx_hwys) + tm_lines() + tm_shape(dumping_complaints) + tm_dots(size=0.0005, col="red") + tm_layout(main.title = "Illegal Dumping Complaints and Parkland (2023)", main.title.position = "center")
tmap_save(dumping_parkland, "./plots/Illegal_Dumping_Points_Parkland.png", width=35, height=35)
tmap_save(dumping_parkland, "./plots/Illegal_Dumping_Points_Parkland_Smaller.png", width=20, height=20)

parks_split = st_cast(parks_houston, "POLYGON")
park_centroids = st_centroid(parks_split)
library_data_shape = library_data_shape[,names(library_data_shape) %in% c("Name", "ZIP", "geometry")]
community_centers = community_centers[,names(community_centers) %in% c("Name", "ZIP", "geometry")]
bound_centers = rbind(library_data_shape, community_centers)
bound_centers = st_transform(bound_centers, crs=32615)
park_centroids = st_transform(park_centroids, crs=32615)

x = list()
for(row in 1:nrow(bound_centers)) {
  x = append(x, get_min_distance(bound_centers[row,]$geometry, park_centroids))
}
min_dists = unlist(x)
bound_centers = cbind(bound_centers, min_dists)
avg_dist_zip = aggregate(bound_centers$min_dists, by=list(ZIP=bound_centers$ZIP), FUN=mean)
avg_dist_zip = avg_dist_zip %>% rename(avg_dist = x)
avg_dist_zip = avg_dist_zip[avg_dist_zip$ZIP %in% houston_zip$ZIP,]
houston_zip = merge(houston_zip, avg_dist_zip, by.x="ZIP", all.x=TRUE)

avg_dist_map = tm_shape(houston_zip) + tm_polygons(col="avg_dist", palette="-RdBu", breaks=c(0,50,100,150,200,250,300,350,400,450,500,550,600,650,700,750,800,850,900,950,1000,Inf), midpoint=200, colorNA="gray", textNA="No Location", title="Distance (Meters)", border.alpha = 0.5) + tm_legend(bg.color="white", outside=TRUE,outside.size=0.15)  + tm_shape(tx_hwys) + tm_lines() + tm_layout(main.title = "Average Distance from Library or Community Center to Park", main.title.position = "center")
tmap_save(avg_dist_map, "avg_dist.png")