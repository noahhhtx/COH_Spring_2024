library(tmap)
library(sf)

houston_zip = st_read("harris_map.geojson")
tx_hwys = st_read("./geographic_data/TxDOT_National_Highway_System.geojson")
hou_zips = tm_shape(houston_zip) + tm_polygons() + tm_shape(tx_hwys) + tm_lines() + tm_shape(houston_zip) + tm_text("all_houston_zips",size=0.25,bg.color="white")
tmap_save(hou_zips, "houston_zip_map_label.png")
hou_zips2 = tm_shape(houston_zip) + tm_polygons() + tm_text("all_houston_zips",size=0.25)
tmap_save(hou_zips2, "houston_zip_map_label_no_roads.png")

