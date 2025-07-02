library(readr)
library(sf)
library(tmap)
library(tidyverse)
library(dplyr)
library(stringr)
library(RColorBrewer)
library(tidygeocoder)

"/" <- function(x,y) ifelse(y==0,base:::"/"(x,y+1),base:::"/"(x,y))
harris_zip = st_read("harris_map.geojson")
tx_hwys = st_read("./geographic_data/TxDOT_National_Highway_System.geojson")
illegal_dumping_data <- read_csv("geographic_data/illegal_dumping_data.csv")
illegal_dumping_data$year = substr(illegal_dumping_data$`Created On`, nchar(illegal_dumping_data$`Created On`)-4+1, nchar(illegal_dumping_data$`Created On`))
illegal_dumping_data$count = 1

illegal_dumping_data_2019 = illegal_dumping_data[illegal_dumping_data$year == "2019",]
complaints_by_zip_2019 = aggregate(illegal_dumping_data_2019$count, by=list(ZIP=illegal_dumping_data_2019$`Zip Code`), FUN=sum)
complaints_by_zip_2019 = complaints_by_zip_2019 %>% rename(complaints_2019 = x)

illegal_dumping_data_2022 = illegal_dumping_data[illegal_dumping_data$year == "2022",]
complaints_by_zip_2022 = aggregate(illegal_dumping_data_2022$count, by=list(ZIP=illegal_dumping_data_2022$`Zip Code`), FUN=sum)
complaints_by_zip_2022 = complaints_by_zip_2022 %>% rename(complaints_2022 = x)

illegal_dumping_data_2023 = illegal_dumping_data[illegal_dumping_data$year == "2023",]
complaints_by_zip_2023 = aggregate(illegal_dumping_data_2023$count, by=list(ZIP=illegal_dumping_data_2023$`Zip Code`), FUN=sum)
complaints_by_zip_2023 = complaints_by_zip_2023 %>% rename(complaints_2023 = x)

complaints_by_zip_all = merge(merge(complaints_by_zip_2019,complaints_by_zip_2022,by="ZIP",all=TRUE),complaints_by_zip_2023,by="ZIP",all=TRUE) 
complaints_by_zip_all[is.na(complaints_by_zip_all)] <- 0
complaints_by_zip_all = complaints_by_zip_all[complaints_by_zip_all$ZIP > 0, ]
complaints_by_zip_all$Name = paste("<at><openparen>",complaints_by_zip_all$ZIP,"<closeparen>",sep="")

harris_zip = merge(harris_zip, complaints_by_zip_all, by.x="Name", all.x=TRUE)
harris_zip = harris_zip[,!(names(harris_zip) %in% c("ZIP"))]
harris_zip[is.na(harris_zip)] <- 0

harris_zip$complaints_2019_pc = 100000 * (harris_zip$complaints_2019) / harris_zip$pop_2020
harris_zip$complaints_2022_pc = 100000 * (harris_zip$complaints_2022) / harris_zip$pop_2020
harris_zip$complaints_2023_pc = 100000 * (harris_zip$complaints_2023) / harris_zip$pop_2020

illegal_dumping_data_2023$Addr = paste(illegal_dumping_data_2023$`Street Address`,illegal_dumping_data_2023$City,illegal_dumping_data_2023$`State/Province`,illegal_dumping_data_2023$`Zip Code`)
dumping_complaints_2023_coords = illegal_dumping_data_2023 %>% geocode(Addr, method="arcgis", lat=latitude, long=longitude)
dumping_complaints_2023_shape = st_as_sf(dumping_complaints_2023_coords, coords=c("longitude","latitude"))

splits = c()
x = 0
while(x <= 400) {
  
  splits = c(splits,x)
  x = x + 25
  
}
splits = c(splits,Inf)

splits_pc = c()
x = 0
while(x <= 1800) {
  
  splits_pc = c(splits_pc,x)
  x = x + 25
  
}
splits_pc = c(splits_pc,Inf)

harris_zip$complaints_trend_2019_2023 = 100 * ((harris_zip$complaints_2023 - harris_zip$complaints_2019)/harris_zip$complaints_2019)
harris_zip$complaints_pc_trend_2019_2023 = 100 * ((harris_zip$complaints_2023_pc - harris_zip$complaints_2019_pc)/harris_zip$complaints_2019_pc)
harris_zip$complaints_trend_2022_2023 = 100 * ((harris_zip$complaints_2023 - harris_zip$complaints_2022)/harris_zip$complaints_2022)
harris_zip$complaints_pc_trend_2022_2023 = 100 * ((harris_zip$complaints_2023_pc - harris_zip$complaints_2022_pc)/harris_zip$complaints_2022_pc)

splits_trend = c(-Inf, -100, -90, -80, -70, -60, -50, -40, -30, -20, -10, -0.0000001,0.0000001, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, Inf)

complaints_2019_plot = tm_shape(harris_zip) + tm_polygons(col = "complaints_2019", border.alpha = 0.1, palette = "Reds", breaks=splits, title = "Illegal Dumping\nComplaints, 2019") + tm_legend(bg.color="white", outside=TRUE,outside.size=0.15) + tm_layout(title.size=5) + tm_shape(tx_hwys) + tm_lines(col="black")
tmap_save(complaints_2019_plot, "./plots/complaints_2019_plot.png")

complaints_2022_plot = tm_shape(harris_zip) + tm_polygons(col = "complaints_2022", border.alpha = 0.1, palette = "Reds", breaks=splits, title = "Illegal Dumping\nComplaints, 2022") + tm_legend(bg.color="white", outside=TRUE,outside.size=0.15) + tm_layout(title.size=5) + tm_shape(tx_hwys) + tm_lines(col="black")
tmap_save(complaints_2022_plot, "./plots/complaints_2022_plot.png")

complaints_2023_plot = tm_shape(harris_zip) + tm_polygons(col = "complaints_2023", border.alpha = 0.1, palette = "Reds", breaks=splits, title = "Illegal Dumping\nComplaints, 2023") + tm_legend(bg.color="white", outside=TRUE,outside.size=0.15) + tm_layout(title.size=5) + tm_shape(tx_hwys) + tm_lines(col="black")
tmap_save(complaints_2023_plot, "./plots/complaints_2023_plot.png")

complaints_2019_pc_plot = tm_shape(harris_zip) + tm_polygons(col = "complaints_2019_pc", border.alpha = 0.1, palette = "Reds", breaks=splits_pc, title = "Illegal Dumping\nComplaints per\n100,000, 2019") + tm_legend(bg.color="white", outside=TRUE,outside.size=0.15) + tm_layout(title.size=5) + tm_shape(tx_hwys) + tm_lines(col="black")
tmap_save(complaints_2019_pc_plot, "./plots/complaints_2019_pc_plot.png")

complaints_2022_pc_plot = tm_shape(harris_zip) + tm_polygons(col = "complaints_2022_pc", border.alpha = 0.1, palette = "Reds", breaks=splits_pc, title = "Illegal Dumping\nComplaints per\n100,000, 2022") + tm_legend(bg.color="white", outside=TRUE,outside.size=0.15) + tm_layout(title.size=5) + tm_shape(tx_hwys) + tm_lines(col="black")
tmap_save(complaints_2022_pc_plot, "./plots/complaints_2022_pc_plot.png")

complaints_2023_pc_plot = tm_shape(harris_zip) + tm_polygons(col = "complaints_2023_pc", border.alpha = 0.1, palette = "Reds", breaks=splits_pc, title = "Illegal Dumping\nComplaints per\n100,000, 2023") + tm_legend(bg.color="white", outside=TRUE,outside.size=0.15) + tm_layout(title.size=5) + tm_shape(tx_hwys) + tm_lines(col="black")
tmap_save(complaints_2023_pc_plot, "./plots/complaints_2023_pc_plot.png")

complaints_trend_2019_2023_plot = tm_shape(harris_zip) + tm_polygons(col = "complaints_trend_2019_2023", border.alpha = 0.1, palette = "-RdBu", breaks=splits_trend, title = "Change in Illegal\nDumping Complaints\n2019 to 2023\n(Percent)") + tm_legend(bg.color="white", outside=TRUE,outside.size=0.15) + tm_layout(title.size=5) + tm_shape(tx_hwys) + tm_lines(col="black")
tmap_save(complaints_trend_2019_2023_plot, "./plots/complaints_trend_2019_2023_plot.png")

complaints_trend_2022_2023_plot = tm_shape(harris_zip) + tm_polygons(col = "complaints_trend_2022_2023", border.alpha = 0.1, palette = "-RdBu", breaks=splits_trend, title = "Change in Illegal\nDumping Complaints\n2022 to 2023\n(Percent)") + tm_legend(bg.color="white", outside=TRUE,outside.size=0.15) + tm_layout(title.size=5) + tm_shape(tx_hwys) + tm_lines(col="black")
tmap_save(complaints_trend_2022_2023_plot, "./plots/complaints_trend_2022_2023_plot.png")

st_write(harris_zip, "illegal_dumping_shape.geojson")
st_write(dumping_complaints_2023_shape, "all_dumping_points_2023.geojson")