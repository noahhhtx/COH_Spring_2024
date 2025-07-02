library(readr)
library(sf)
library(dplyr)
library(stringr)

us_zip = st_read("./geographic_data/cb_2020_us_zcta520_500k.kml")
harris_zipcodes = read.csv("./geographic_data/harris_zipcodes.csv")
zcta_area=read.csv("./geographic_data/zcta_area.csv")
population_data=read.csv("./geographic_data/population_2020.csv")
other_zip_codes = c(77336, 77338, 77339, 77345, 77346, 77357, 77365, 77373, 77375, 77379, 77388, 77396, 77401, 77429, 77433, 77449, 77450, 77477, 77489, 77493, 77494, 77503, 77504, 77506, 77520, 77530, 77532, 77536, 77545, 77546, 77547, 77571, 77598)
all_houston_zips = c(harris_zipcodes[harris_zipcodes$POSTAL=="HOUSTON",]$ZIP, other_zip_codes)
htx = data.frame(all_houston_zips)
htx$Name = paste("<at><openparen>",htx$all_houston_zips,"<closeparen>",sep="")
zcta_area$Name = paste("<at><openparen>",zcta_area$GEOID_ZCTA5_20,"<closeparen>",sep="")
population_data$Name = paste("<at><openparen>",population_data$ZIP_CODE,"<closeparen>",sep="")
drop=c("ZIP_CODE","zcta")
population_data = population_data[,!(names(population_data) %in% drop)]
harris_zip = merge(us_zip,htx,by.x="Name")
harris_zip = merge(harris_zip,zcta_area,by.x="Name")
harris_zip = merge(harris_zip,population_data,by.x="Name")
drop=c("EDITOR","Shape__Area","Shape__Length", "STATE", "ZIP_TYPE", "code", "GEOID_ZCTA5_20", "DATE_MOD")
harris_zip = harris_zip[,!(names(harris_zip) %in% drop)]
harris_zip = harris_zip[!duplicated(harris_zip$Name),]
harris_zip$aland_acre = harris_zip$AREALAND_ZCTA5_20 / 4047
harris_zip$awater_acre = harris_zip$AREAWATER_ZCTA5_20 / 4047
harris_zip$total_area = harris_zip$AREALAND_ZCTA5_20 + harris_zip$AREAWATER_ZCTA5_20
harris_zip$total_area_acre = harris_zip$total_area / 4047
st_write(harris_zip, "harris_map.geojson",append=FALSE)
st_write(harris_zip, "harris_map.kml",append=FALSE)
st_write(harris_zip, "harris_map.csv",append=FALSE)