library(readr)
library(sf)
library(tmap)
library(tidyverse)
library(dplyr)
library(stringr)
library(RColorBrewer)
library(tidygeocoder)
library(readxl)

"/" <- function(x,y) ifelse(y==0,base:::"/"(x,y+1),base:::"/"(x,y))
houston_zip = st_read("harris_map.geojson")
tx_hwys = st_read("./geographic_data/TxDOT_National_Highway_System.geojson")

crime_2019 = read.csv("./crime_data/NIBRSPublicView2019.csv")
crime_2019 = crime_2019[crime_2019$Beat!=""&crime_2019$Beat!="OOJ",]
crime_2019$ZIPCode = substring(crime_2019$ZIPCode,1,5)
crime_2022 = read.csv("./crime_data/NIBRSPublicView2022.csv")
crime_2022 = crime_2022[crime_2022$Beat!=""&crime_2022$Beat!="OOJ",]
crime_2022$ZIPCode = substring(crime_2022$ZIPCode,1,5)
crime_2023 = read.csv("./crime_data/NIBRSPublicView2023.csv")
crime_2023 = crime_2023[crime_2023$Beat!=""&crime_2023$Beat!="OOJ",]
crime_2023$ZIPCode = substring(crime_2023$ZIPCode,1,5)

violent_codes = c("120","13A", "13B", "13C", "09A", "09B", "100", "11A", "11B", "11C", "11D", "36A", "36B")
homicide = c("09A", "09B")
theft = c("23A", "23B", "23C", "23D", "23E", "23F", "23G", "23H", "240")

violence_2019 = crime_2019[crime_2019$NIBRSClass %in% violent_codes,]
violent_crime_zip_2019 = aggregate(violence_2019$OffenseCount, by=list(Name=violence_2019$ZIPCode), FUN=sum)
violent_crime_zip_2019 = violent_crime_zip_2019 %>% rename(violent_offenses_2019 = x)

hom_2019 = crime_2019[crime_2019$NIBRSClass %in% homicide,]
hom_2019_zip = aggregate(hom_2019$OffenseCount, by=list(Name=hom_2019$ZIPCode), FUN=sum)
hom_2019_zip = hom_2019_zip %>% rename(homicide_2019 = x)

theft_2019 = crime_2019[crime_2019$NIBRSClass %in% theft,]
theft_zip_2019 = aggregate(theft_2019$OffenseCount, by=list(Name=theft_2019$ZIPCode), FUN=sum)
theft_zip_2019 = theft_zip_2019 %>% rename(theft_offenses_2019 = x)

violence_2022 = crime_2022[crime_2022$NIBRSClass %in% violent_codes,]
violent_crime_zip_2022 = aggregate(violence_2022$OffenseCount, by=list(Name=violence_2022$ZIPCode), FUN=sum)
violent_crime_zip_2022 = violent_crime_zip_2022 %>% rename(violent_offenses_2022 = x)

hom_2022 = crime_2022[crime_2022$NIBRSClass %in% homicide,]
hom_2022_zip = aggregate(hom_2022$OffenseCount, by=list(Name=hom_2022$ZIPCode), FUN=sum)
hom_2022_zip = hom_2022_zip %>% rename(homicide_2022 = x)

theft_2022 = crime_2022[crime_2022$NIBRSClass %in% theft,]
theft_zip_2022 = aggregate(theft_2022$OffenseCount, by=list(Name=theft_2022$ZIPCode), FUN=sum)
theft_zip_2022 = theft_zip_2022 %>% rename(theft_offenses_2022 = x)

violence_2023 = crime_2023[crime_2023$NIBRSClass %in% violent_codes,]
violent_crime_zip_2023 = aggregate(violence_2023$OffenseCount, by=list(Name=violence_2023$ZIPCode), FUN=sum)
violent_crime_zip_2023 = violent_crime_zip_2023 %>% rename(violent_offenses_2023 = x)

hom_2023 = crime_2023[crime_2023$NIBRSClass %in% homicide,]
hom_2023_zip = aggregate(hom_2023$OffenseCount, by=list(Name=hom_2023$ZIPCode), FUN=sum)
hom_2023_zip = hom_2023_zip %>% rename(homicide_2023 = x)

theft_2023 = crime_2023[crime_2023$NIBRSClass %in% theft,]
theft_zip_2023 = aggregate(theft_2023$OffenseCount, by=list(Name=theft_2023$ZIPCode), FUN=sum)
theft_zip_2023 = theft_zip_2023 %>% rename(theft_offenses_2023 = x)

crime_sheet = merge(violent_crime_zip_2019, violent_crime_zip_2022, by="Name", all=TRUE)
crime_sheet = merge(crime_sheet, violent_crime_zip_2023, by="Name", all=TRUE)
crime_sheet[is.na(crime_sheet)] <- 0
crime_sheet$Name = paste("<at><openparen>",crime_sheet$Name,"<closeparen>",sep="")
crime_sheet = crime_sheet[crime_sheet$Name %in% houston_zip$Name,]

hom_sheet = merge(hom_2019_zip,hom_2022_zip, by="Name", all=TRUE)
hom_sheet = merge(hom_sheet,hom_2023_zip, by="Name", all=TRUE)
hom_sheet[is.na(hom_sheet)] <- 0
hom_sheet$Name = paste("<at><openparen>",hom_sheet$Name,"<closeparen>",sep="")
hom_sheet = hom_sheet[hom_sheet$Name %in% houston_zip$Name,]

theft_sheet = merge(theft_zip_2019, theft_zip_2022, by="Name", all=TRUE)
theft_sheet = merge(theft_sheet, theft_zip_2023, by="Name", all=TRUE)
theft_sheet[is.na(theft_sheet)] <- 0
theft_sheet$Name = paste("<at><openparen>",theft_sheet$Name,"<closeparen>",sep="")
theft_sheet = theft_sheet[theft_sheet$Name %in% houston_zip$Name,]

houston_zip = merge(houston_zip, crime_sheet, by.x="Name")
houston_zip = merge(houston_zip, theft_sheet, by.x="Name")
houston_zip = merge(houston_zip, hom_sheet, by.x="Name", all=TRUE)
houston_zip[is.na(houston_zip)] <- 0


houston_zip$vc_pc_2019 = 100000 * houston_zip$violent_offenses_2019 / houston_zip$pop_2020
houston_zip$hom_pc_2019 = 100000 * houston_zip$homicide_2019 / houston_zip$pop_2020
houston_zip$theft_pc_2019 = 100000 * houston_zip$theft_offenses_2019 / houston_zip$pop_2020

houston_zip$vc_pc_2022 = 100000 * houston_zip$violent_offenses_2022 / houston_zip$pop_2020
houston_zip$hom_pc_2022 = 100000 * houston_zip$homicide_2022 / houston_zip$pop_2020
houston_zip$theft_pc_2022 = 100000 * houston_zip$theft_offenses_2022 / houston_zip$pop_2020

houston_zip$vc_pc_2023 = 100000 * houston_zip$violent_offenses_2023 / houston_zip$pop_2020
houston_zip$hom_pc_2023 = 100000 * houston_zip$homicide_2023 / houston_zip$pop_2020
houston_zip$theft_pc_2023 = 100000 * houston_zip$theft_offenses_2023 / houston_zip$pop_2020

houston_zip$vc_change_2019_2023 = 100 * (houston_zip$vc_pc_2023 - houston_zip$vc_pc_2019) / houston_zip$vc_pc_2019
houston_zip$hom_change_2019_2023 = 100 * (houston_zip$hom_pc_2023 - houston_zip$hom_pc_2019) / houston_zip$hom_pc_2019
houston_zip$theft_change_2019_2023 = 100 * (houston_zip$theft_pc_2023 - houston_zip$theft_pc_2019) / houston_zip$theft_pc_2019

houston_zip$vc_change_2022_2023 = 100 * (houston_zip$vc_pc_2023 - houston_zip$vc_pc_2022) / houston_zip$vc_pc_2022
houston_zip$hom_change_2022_2023 = 100 * (houston_zip$hom_pc_2023 - houston_zip$hom_pc_2022) / houston_zip$hom_pc_2022
houston_zip$theft_change_2022_2023 = 100 * (houston_zip$theft_pc_2023 - houston_zip$theft_pc_2022) / houston_zip$theft_pc_2022

houston_zip$vc_trend_2019_2023 = ifelse(houston_zip$vc_change_2019_2023 > 0 & houston_zip$vc_change_2022_2023 > 0,
                                        "Increase 2019 to 2023, Increase 2022 to 2023", ifelse(
                                          houston_zip$vc_change_2019_2023 > 0 & houston_zip$vc_change_2022_2023 < 0,
                                          "Increase 2019 to 2023, Decrease 2022 to 2023", ifelse(
                                            houston_zip$vc_change_2019_2023 < 0 & houston_zip$vc_change_2022_2023 < 0,
                                            "Decrease 2019 to 2023, Decrease 2022 to 2023", ifelse(
                                              houston_zip$vc_change_2019_2023 < 0 & houston_zip$vc_change_2022_2023 > 0,
                                              "Decrease 2019 to 2023, Increase 2022 to 2023", "Other"
                                            )
                                          )
                                        ))

splits_trend = c(-Inf, -50, -40, -30, -20, -10, -0.0000001,0.0000001, 10, 20, 30, 40, 50, Inf)

vc_2023_map = (tm_shape(houston_zip) + tm_polygons(col="vc_pc_2023", border.alpha=0.1, palette="Reds", breaks=c(-Inf,250,500,750,1000,1250,1500,1750,2000,2250,2500,2750,3000,3250,3500,3750,4000,4250,4500,4750,5000,5250,5500,5750,6000,6250,6500,6750,7000,Inf), title = "Violent Crimes\nPer 100,000,\n2023",colorNA=NULL) + tm_legend(bg.color="white", outside=TRUE,outside.size=0.15) + tm_shape(tx_hwys) + tm_lines(col="black"))
tmap_save(vc_2023_map,"./plots/violent_crime_2023.png")

hom_2023_map = (tm_shape(houston_zip) + tm_polygons(col="hom_pc_2023", border.alpha=0.1, palette=c("Reds"), breaks=c(0,5,10,15,20,25,30,35,40,45,50,Inf), title = "Homicides\nPer 100,000,\n2023",colorNA=NULL) + tm_legend(bg.color="white", outside=TRUE,outside.size=0.15) + tm_shape(tx_hwys) + tm_lines(col="black"))
tmap_save(hom_2023_map,"./plots/homicide_2023.png")

theft_2023_map = (tm_shape(houston_zip) + tm_polygons(col="theft_pc_2023", border.alpha=0.1, palette="Reds", breaks=c(-Inf,500,1000,1500,2000,2500,3000,3500,4000,4500,5000,5500,6000,6500,7000,7500,8000,8500,9000,9500,10000,Inf), title = "Theft Offenses\nPer 100,000,\n2023",colorNA=NULL) + tm_legend(bg.color="white", outside=TRUE,outside.size=0.15) + tm_shape(tx_hwys) + tm_lines(col="black"))
tmap_save(theft_2023_map,"./plots/theft_2023.png")

trend_2019_2023 = (tm_shape(houston_zip) + tm_polygons(col="vc_change_2019_2023", border.alpha=0.1, palette="-RdBu", style="fixed",breaks=splits_trend, title = "Change in Violent Crime Rate,\n2019 to 2023 (Percent)",colorNA=NULL) + tm_legend(bg.color="white", outside=TRUE,outside.size=0.15) + tm_shape(tx_hwys) + tm_lines(col="black"))
tmap_save(trend_2019_2023,"./plots/violent_crime_trend_2019_2023.png")

hom_trend_2019_2023 = (tm_shape(houston_zip) + tm_polygons(col="hom_change_2019_2023", border.alpha=0.1, palette="-RdBu", style="fixed",breaks=splits_trend, title = "Change in Homicide Rate,\n2019 to 2023 (Percent)", colorNA=NULL) + tm_legend(bg.color="white", outside=TRUE,outside.size=0.15) + tm_shape(tx_hwys) + tm_lines(col="black"))
tmap_save(hom_trend_2019_2023,"./plots/homicide_trend_2019_2023.png")

theft_trend_2019_2023 = (tm_shape(houston_zip) + tm_polygons(col="theft_change_2019_2023", border.alpha=0.1, palette="-RdBu", style="fixed",breaks=splits_trend, title = "Change in Theft Rate,\n2019 to 2023 (Percent)", colorNA=NULL) + tm_legend(bg.color="white", outside=TRUE,outside.size=0.15) + tm_shape(tx_hwys) + tm_lines(col="black"))
tmap_save(theft_trend_2019_2023,"./plots/theft_trend_2019_2023.png")

trend_2022_2023 = (tm_shape(houston_zip) + tm_polygons(col="vc_change_2022_2023", border.alpha=0.1, palette="-RdBu", style="fixed",breaks=splits_trend, title = "Change in Violent Crime Rate,\n2022 to 2023 (Percent)",colorNA=NULL) + tm_legend(bg.color="white", outside=TRUE,outside.size=0.15) + tm_shape(tx_hwys) + tm_lines(col="black"))
tmap_save(trend_2022_2023,"./plots/violent_crime_trend_2022_2023.png")

hom_trend_2022_2023 = (tm_shape(houston_zip) + tm_polygons(col="hom_change_2022_2023", border.alpha=0.1, palette="-RdBu", style="fixed",breaks=splits_trend, title = "Change in Homicide Rate,\n2022 to 2023 (Percent)",colorNA=NULL) + tm_legend(bg.color="white", outside=TRUE,outside.size=0.15) + tm_shape(tx_hwys) + tm_lines(col="black"))
tmap_save(hom_trend_2022_2023,"./plots/homicide_trend_2022_2023.png")

theft_trend_2022_2023 = (tm_shape(houston_zip) + tm_polygons(col="theft_change_2022_2023", border.alpha=0.1, palette="-RdBu", style="fixed",breaks=splits_trend, title = "Change in Theft Rate,\n2022 to 2023 (Percent)",colorNA=NULL) + tm_legend(bg.color="white", outside=TRUE,outside.size=0.15) + tm_shape(tx_hwys) + tm_lines(col="black"))
tmap_save(theft_trend_2022_2023,"./plots/Theft_trend_2022_2023.png")

trend_directions = (tm_shape(houston_zip) + tm_polygons(col="vc_trend_2019_2023", border.alpha=0.1, palette="Set2",title = "Violent Crime Trend Directions",colorNA=NULL) + tm_legend(bg.color="white") + tm_shape(tx_hwys) + tm_lines(col="black"))
tmap_save(trend_directions,"./plots/vc_trend_directions.png")

houston_zip_local = houston_zip[grepl("770",houston_zip$Name),]

vc_2023_map_local = (tm_shape(houston_zip_local) + tm_polygons(col="vc_pc_2023", border.alpha=0.1, palette="Reds", breaks=c(-Inf,250,500,750,1000,1250,1500,1750,2000,2250,2500,2750,3000,3250,3500,3750,4000,4250,4500,4750,5000,5250,5500,5750,6000,6250,6500,6750,7000,Inf), title = "Violent Crimes\nPer 100,000,\n2023",colorNA=NULL) + tm_legend(bg.color="white", outside=TRUE,outside.size=0.15) + tm_shape(tx_hwys) + tm_lines(col="black"))
tmap_save(vc_2023_map_local,"./plots/violent_crime_2023_local.png")

hom_2023_map_local = (tm_shape(houston_zip_local) + tm_polygons(col="hom_pc_2023", border.alpha=0.1, palette="Reds", breaks=c(0,5,10,15,20,25,30,35,40,45,50,Inf), title = "Homicides\nPer 100,000,\n2023",colorNA=NULL) + tm_legend(bg.color="white", outside=TRUE,outside.size=0.15) + tm_shape(tx_hwys) + tm_lines(col="black"))
tmap_save(hom_2023_map_local,"./plots/homicide_2023_local.png")

theft_2023_map_local = (tm_shape(houston_zip_local) + tm_polygons(col="theft_pc_2023", border.alpha=0.1, palette="Reds", breaks=c(-Inf,500,1000,1500,2000,2500,3000,3500,4000,4500,5000,5500,6000,6500,7000,7500,8000,8500,9000,9500,10000,Inf), title = "Thefts\nPer 100,000,\n2023",colorNA=NULL) + tm_legend(bg.color="white", outside=TRUE,outside.size=0.15) + tm_shape(tx_hwys) + tm_lines(col="black"))
tmap_save(theft_2023_map_local,"./plots/Theft_2023_local.png")

trend_2019_2023_local = (tm_shape(houston_zip_local) + tm_polygons(col="vc_change_2019_2023", border.alpha=0.1, palette="-RdBu", style="fixed",breaks=splits_trend, title = "Change in Violent Crime Rate,\n2019 to 2023 (Percent)",colorNA=NULL) + tm_legend(bg.color="white", outside=TRUE,outside.size=0.15) + tm_shape(tx_hwys) + tm_lines(col="black"))
tmap_save(trend_2019_2023_local,"./plots/violent_crime_trend_2019_2023_local.png")

hom_trend_2019_2023_local = (tm_shape(houston_zip_local) + tm_polygons(col="hom_change_2019_2023", border.alpha=0.1, palette="-RdBu", style="fixed",breaks=splits_trend, title = "Change in Homicide Rate,\n2019 to 2023 (Percent)",colorNA=NULL) + tm_legend(bg.color="white", outside=TRUE,outside.size=0.15) + tm_shape(tx_hwys) + tm_lines(col="black"))
tmap_save(hom_trend_2019_2023_local,"./plots/homicide_trend_2019_2023_local.png")

theft_trend_2019_2023_local = (tm_shape(houston_zip_local) + tm_polygons(col="theft_change_2019_2023", border.alpha=0.1, palette="-RdBu", style="fixed",breaks=splits_trend, title = "Change in Theft Rate,\n2019 to 2023 (Percent)",colorNA=NULL) + tm_legend(bg.color="white", outside=TRUE,outside.size=0.15) + tm_shape(tx_hwys) + tm_lines(col="black"))
tmap_save(theft_trend_2019_2023_local,"./plots/Theft_trend_2019_2023_local.png")

trend_2022_2023_local = (tm_shape(houston_zip_local) + tm_polygons(col="vc_change_2022_2023", border.alpha=0.1, palette="-RdBu", style="fixed",breaks=splits_trend, title = "Change in Violent Crime Rate,\n2022 to 2023 (Percent)",colorNA=NULL) + tm_legend(bg.color="white", outside=TRUE,outside.size=0.15) + tm_shape(tx_hwys) + tm_lines(col="black"))
tmap_save(trend_2022_2023_local,"./plots/violent_crime_trend_2022_2023_local.png")

hom_trend_2022_2023_local = (tm_shape(houston_zip_local) + tm_polygons(col="hom_change_2022_2023", border.alpha=0.1, palette="-RdBu", style="fixed",breaks=splits_trend, title = "Change in Homicide Rate,\n2022 to 2023 (Percent)",colorNA=NULL) + tm_legend(bg.color="white", outside=TRUE,outside.size=0.15) + tm_shape(tx_hwys) + tm_lines(col="black"))
tmap_save(hom_trend_2022_2023_local,"./plots/homicide_trend_2022_2023_local.png")

theft_trend_2022_2023_local = (tm_shape(houston_zip_local) + tm_polygons(col="theft_change_2022_2023", border.alpha=0.1, palette="-RdBu", style="fixed",breaks=splits_trend, title = "Change in Theft Rate,\n2022 to 2023 (Percent)",colorNA=NULL) + tm_legend(bg.color="white", outside=TRUE,outside.size=0.15) + tm_shape(tx_hwys) + tm_lines(col="black"))
tmap_save(theft_trend_2022_2023_local,"./plots/Theft_trend_2022_2023_local.png")

st_write(houston_zip, "crime_shape.geojson")