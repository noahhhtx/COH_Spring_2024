library(readr)
library(sf)
library(tmap)
library(tidyverse)
library(dplyr)
library(stringr)
library(RColorBrewer)
library(tidygeocoder)

harris_zip = st_read("harris_map.geojson")
roads = st_read("./geographic_data/TxDOT_roadways.geojson")
roads = roads[(is.na(roads$COUNTY) | roads$COUNTY == "Harris"),] #filter out highways

illegal_dumping_data <- read_csv("geographic_data/illegal_dumping_data.csv")
illegal_dumping_data$year = substr(illegal_dumping_data$`Created On`, nchar(illegal_dumping_data$`Created On`)-4+1, nchar(illegal_dumping_data$`Created On`))
illegal_dumping_data = illegal_dumping_data[illegal_dumping_data$year == 2023,]
illegal_dumping_data$Addr = paste(illegal_dumping_data$`Street Address`,illegal_dumping_data$City,illegal_dumping_data$`State/Province`,illegal_dumping_data$`Zip Code`)
crime_2023 = read.csv("./crime_data/NIBRSPublicView2023.csv")
crime_2023$ZIPCode = substring(crime_2023$ZIPCode,1,5)
crime_2023$Addr = paste(crime_2023$StreetNo, crime_2023$StreetName, crime_2023$StreetType, crime_2023$City, "TX", crime_2023$ZIPCode, sep=" ")

zips_of_interest = c(77026, 77028, 77016, 77004, 77021, 77051, 77033, 77048)
homicide = c("09A", "09B")
crime_2023 = crime_2023[(crime_2023$ZIPCode %in% zips_of_interest) & (crime_2023$NIBRSClass %in% homicide),]
illegal_dumping_data = illegal_dumping_data[illegal_dumping_data$`Zip Code` %in% zips_of_interest,]

illegal_dumping_coords = data.frame()
homicide_coords = data.frame()

for(zip in zips_of_interest) {
  homicides = crime_2023[crime_2023$ZIPCode == zip,]
  dumping_complaints = illegal_dumping_data[illegal_dumping_data$`Zip Code` == zip,]
  homicides_shape = st_as_sf(homicides, coords=c("MapLongitude","MapLatitude"))
  homicide_coords = rbind(homicide_coords, homicides_shape)
  dumping_complaints = dumping_complaints %>% geocode(Addr, method="arcgis", lat=latitude, long=longitude)
  complaints_shape= st_as_sf(dumping_complaints, coords=c("longitude","latitude"))
  illegal_dumping_coords = rbind(illegal_dumping_coords, complaints_shape)
  homicides_plot = tm_shape(harris_zip[harris_zip$all_houston_zips == zip,]) + tm_polygons(border.alpha=0.5) + tm_shape(roads) + tm_lines() + tm_shape(homicides_shape) + tm_dots(size=0.1, col="red") + tm_layout(main.title=paste("Homicides in", zip, "(2023)"), main.title.position = "center")
  complaints_plot = tm_shape(harris_zip[harris_zip$all_houston_zips == zip,]) + tm_polygons(border.alpha=0.5) + tm_shape(roads) + tm_lines() + tm_shape(complaints_shape) + tm_dots(size=0.05, col="red") + tm_layout(main.title=paste("Illegal Dumping Complaints in", zip, "(2023)"), main.title.position = "center")
  tmap_save(homicides_plot, paste("./plots/homicides_",zip,"_2023.png",sep=""))
  tmap_save(complaints_plot, paste("./plots/complaints_",zip,"_2023.png",sep=""))
}

illegal_dumping_coords = st_as_sf(illegal_dumping_coords)
homicide_coords = st_as_sf(homicide_coords)
st_write(illegal_dumping_coords, "illegal_dumping_coords.geojson")
st_write(homicide_coords, "homicide_coords.geojson")