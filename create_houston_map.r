library(sf)

harris_zip = st_read("harris_map.geojson")
other_zip_codes = c(77336, 77338, 77339, 77345, 77346, 77357, 77365, 77373, 77375, 77379, 77388, 77389, 77396, 77401, 77429, 77433, 77449, 77450, 77459, 77469, 77477, 77478, 77489, 77493, 77494, 77502, 77503, 77504, 77505, 77506, 77520, 77530, 77532, 77536, 77545, 77546, 77547, 77571, 77587, 77598)
houston_zips = harris_zip[harris_zip$POSTAL=="HOUSTON" | harris_zip$ZIP %in% other_zip_codes,]
st_write(houston_zips,"houston_map.geojson",append=FALSE)