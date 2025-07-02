library(sf)
library(dplyr)
library(tmap)

remove_outliers <- function(x) {
  
  lower_bound = quantile(x, 0.25) - (1.5 * IQR(x))
  upper_bound = quantile(x, 0.75) + (1.5 * IQR(x))
  
  for(i in 1:length(x)) {
    if(x[i] > upper_bound) { x[i] = upper_bound }
    if(x[i] < lower_bound) { x[i] = lower_bound }
  }
  
  return(x)
  
}

normalize <- function(x) {
  
  mi = min(x)
  ma = max(x)
  
  for(i in 1:length(x)) {
    x[i] = 100 * (x[i] - mi) / (ma - mi)
  }
  
  return(x)
  
}

harris_zip = st_read("composite_harris_map.geojson")
cols_of_interest = c("vc_pc_2023", "theft_pc_2023", "complaints_2023_pc", "asthma_2021", "sedentary_2021", "obesity_2021", "poor_mental_health_14_days_2021")
harris_zip = harris_zip[,names(harris_zip) %in% c(cols_of_interest, "geometry", "zip_code")]
harris_zip_fixed = na.omit(harris_zip)
harris_zip_fixed = harris_zip_fixed %>% mutate_at(cols_of_interest, ~ remove_outliers(.))
harris_zip_fixed = harris_zip_fixed %>% mutate_at(cols_of_interest, ~ normalize(.))
harris_zip_fixed$health_score = (0.3 * harris_zip_fixed$asthma_2021) + (0.35 * harris_zip_fixed$obesity_2021) + (0.35 * harris_zip_fixed$poor_mental_health_14_days_2021)
harris_zip_fixed$safety_score = (0.5 * harris_zip_fixed$vc_pc_2023 + 0.5*harris_zip_fixed$complaints_2023_pc)
harris_zip_fixed$composite_score = (0.5 * harris_zip_fixed$health_score) + (0.5 * harris_zip_fixed$safety_score)

b = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100)

tx_hwys = st_read("./geographic_data/TxDOT_National_Highway_System.geojson")
harris_index = tm_shape(harris_zip_fixed) + tm_polygons(col="composite_score", palette = "-RdBu", title="Score", breaks=b, midpoint = median(harris_zip_fixed$composite_score)) + tm_legend(bg.color="white",outside=TRUE,outside.size=0.12) + tm_shape(tx_hwys) + tm_lines(col="black") + tm_layout(main.title = "Priority Index", main.title.position = "center")
tmap_save(harris_index, "./plots/index_map.png")

harris_zip_fixed$p_80 = ifelse(harris_zip_fixed$composite_score > quantile(harris_zip_fixed$composite_score, 0.8), "Y", "N")
harris_index_p_80 = tm_shape(harris_zip_fixed) + tm_polygons(col="p_80", palette = c("green3", "red2"), title="In 80th Percentile?") + tm_legend(bg.color="white") + tm_shape(tx_hwys) + tm_lines(col="black") + tm_layout(main.title = "In 80th Percentile of Priority Index", main.title.position = "center")
tmap_save(harris_index_p_80, "./plots/index_map_80p.png")