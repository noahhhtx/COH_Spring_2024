library(readr)
library(sf)
library(tidyverse)
library(dplyr)
library(stringr)

harris_zip = st_read("harris_map.geojson")

indicators_of_interest = c("Current asthma among adults aged >=18 years", "Obesity among adults aged >=18 years", "Mental health not good for >=14 days among adults aged >=18 years", "No leisure-time physical activity among adults aged >=18 years")

cols_of_interest = c("LocationName", "Measure", "Data_Value")

places_2021 <- read_csv("./health_data/places_2021.csv")
places_2019 = places_2021[places_2021$Year==2019 & places_2021$LocationName %in% harris_zip$all_houston_zips,]
places_2019 = places_2019[,names(places_2019) %in% cols_of_interest]
places_2019 = places_2019[places_2019$Measure %in% indicators_of_interest,]
asthma_2019 = places_2019[places_2019$Measure == indicators_of_interest[1],]
asthma_2019 = asthma_2019 %>% rename(asthma_2019 = Data_Value)
asthma_2019 = asthma_2019[,!(names(asthma_2019) %in% c("Measure"))]
obesity_2019 = places_2019[places_2019$Measure == indicators_of_interest[2],]
obesity_2019 = obesity_2019 %>% rename(obesity_2019 = Data_Value)
obesity_2019 = obesity_2019[,!(names(obesity_2019) %in% c("Measure"))]
poor_mental_health_14_days_2019 = places_2019[places_2019$Measure == indicators_of_interest[3],]
poor_mental_health_14_days_2019 = poor_mental_health_14_days_2019 %>% rename(poor_mental_health_14_days_2019 = Data_Value)
poor_mental_health_14_days_2019 = poor_mental_health_14_days_2019[,!(names(poor_mental_health_14_days_2019) %in% c("Measure"))]
sedentary_2019 = places_2019[places_2019$Measure == indicators_of_interest[4],]
sedentary_2019 = sedentary_2019 %>% rename(sedentary_2019 = Data_Value)
sedentary_2019 = sedentary_2019[,!(names(sedentary_2019) %in% c("Measure"))]
measures_2019 = merge(merge(merge(asthma_2019,obesity_2019,by.x="LocationName"),poor_mental_health_14_days_2019,by.x="LocationName"),sedentary_2019,by.x="LocationName")

places_2023 <- read_csv("./health_data/places_2023.csv")
places_2021 = places_2023[places_2023$Year==2021 & places_2023$LocationName %in% harris_zip$all_houston_zips,]
places_2021 = places_2021[,names(places_2021) %in% cols_of_interest]
places_2021 = places_2021[places_2021$Measure %in% indicators_of_interest,]
asthma_2021 = places_2021[places_2021$Measure == indicators_of_interest[1],]
asthma_2021 = asthma_2021 %>% rename(asthma_2021 = Data_Value)
asthma_2021 = asthma_2021[,!(names(asthma_2021) %in% c("Measure"))]
obesity_2021 = places_2021[places_2021$Measure == indicators_of_interest[2],]
obesity_2021 = obesity_2021 %>% rename(obesity_2021 = Data_Value)
obesity_2021 = obesity_2021[,!(names(obesity_2021) %in% c("Measure"))]
poor_mental_health_14_days_2021 = places_2021[places_2021$Measure == indicators_of_interest[3],]
poor_mental_health_14_days_2021 = poor_mental_health_14_days_2021 %>% rename(poor_mental_health_14_days_2021 = Data_Value)
poor_mental_health_14_days_2021 = poor_mental_health_14_days_2021[,!(names(poor_mental_health_14_days_2021) %in% c("Measure"))]
sedentary_2021 = places_2021[places_2021$Measure == indicators_of_interest[4],]
sedentary_2021 = sedentary_2021 %>% rename(sedentary_2021 = Data_Value)
sedentary_2021 = sedentary_2021[,!(names(sedentary_2021) %in% c("Measure"))]
measures_2021 = merge(merge(merge(asthma_2021,obesity_2021,by.x="LocationName"),poor_mental_health_14_days_2021,by.x="LocationName"),sedentary_2021,by.x="LocationName")

all_measures = merge(measures_2019,measures_2021,by.x="LocationName")
all_measures$asthma_trend = 100 * (all_measures$asthma_2021 - all_measures$asthma_2019) / all_measures$asthma_2019
all_measures$obesity_trend = 100 * (all_measures$obesity_2021 - all_measures$obesity_2019) / all_measures$obesity_2019
all_measures$poor_mental_health_14_days_trend = 100 * (all_measures$poor_mental_health_14_days_2021 - all_measures$poor_mental_health_14_days_2019) / all_measures$poor_mental_health_14_days_2019
all_measures$sedentary_trend = 100 * (all_measures$sedentary_2021 - all_measures$sedentary_2019) / all_measures$sedentary_2019

all_measures = all_measures %>% rename(zip_code = LocationName)
harris_zip = harris_zip %>% rename(zip_code = all_houston_zips)
harris_zip = merge(harris_zip, all_measures, by.x="zip_code")

st_write(harris_zip, "health_map.geojson",append=FALSE)
