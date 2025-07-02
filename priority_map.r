library(tmap)
library(sf)
library(tidyr)
library(tidyverse)
library(dplyr)

tx_hwys = st_read("./geographic_data/TxDOT_National_Highway_System.geojson")

houston_zip = st_read("composite_harris_map.geojson")
top_violent_crime = quantile(houston_zip$vc_pc_2023, 0.8, na.rm=TRUE)
top_illegal_dumping = quantile(houston_zip$complaints_2023_pc, 0.8)
top_mental_health = quantile(houston_zip$poor_mental_health_14_days_2021, 0.8)
houston_zip$priority = 0
houston_zip$priority = ifelse(!(is.na(houston_zip$vc_pc_2023)) & houston_zip$vc_pc_2023 >= top_violent_crime, houston_zip$priority + 1, houston_zip$priority)
houston_zip$priority = ifelse(houston_zip$complaints_2023_pc >= top_illegal_dumping, houston_zip$priority + 1, houston_zip$priority)
houston_zip$priority = ifelse(houston_zip$poor_mental_health_14_days_2021 >= top_mental_health, houston_zip$priority + 1, houston_zip$priority)
houston_zip$priority = as.factor(houston_zip$priority)

st_write(houston_zip, "priority_map.geojson")

priority_map = tm_shape(houston_zip) + tm_polygons(col="priority", palette = c("green3", "yellow", "orange", "red3"), title="Priority Level") + tm_legend(bg.color="white") + tm_shape(tx_hwys) + tm_lines(col="black")
tmap_save(priority_map, "./plots/priority_map.png")