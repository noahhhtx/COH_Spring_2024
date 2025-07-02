library(sf)
library(tidyr)
library(tmap)

sf_use_s2(FALSE)
hou_zip = st_read("composite_harris_map.geojson")
coh = st_read("./geographic_data/coh_limits.geojson")
coh_zip = st_intersection(hou_zip, coh)
coh_zip = coh_zip[,names(coh_zip) %in% names(hou_zip)]
coh_zip = coh_zip %>% drop_na()
st_write(coh_zip, "coh_map.geojson")