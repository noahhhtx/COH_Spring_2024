library(sf)
library(dplyr)

harris_zip = st_read("harris_map.geojson")
cols=colnames(harris_zip)
cols = cols[2:11]
crime_map = st_read("crime_shape.geojson")
crime_map = st_drop_geometry(crime_map)
crime_map = crime_map[,!(colnames(crime_map) %in% cols)]
illegal_dumping_map = st_read("illegal_dumping_shape.geojson")
illegal_dumping_map = st_drop_geometry(illegal_dumping_map)
illegal_dumping_map = illegal_dumping_map[,!(colnames(illegal_dumping_map) %in% cols)]
health_map = st_read("health_map.geojson")
health_map = st_drop_geometry(health_map)
health_map = health_map[,!(colnames(health_map) %in% cols)]

harris_zip = merge(harris_zip,crime_map,by="Name",all.x=TRUE)
harris_zip = merge(harris_zip,illegal_dumping_map,by="Name",all.x=TRUE)
harris_zip = merge(harris_zip,health_map,by="Name",all.x=TRUE)

st_write(harris_zip, "composite_harris_map.geojson")
st_write(harris_zip, "composite_harris_map.csv")