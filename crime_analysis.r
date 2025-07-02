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

days_of_the_week <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")

crime_2019 = read.csv("./crime_data/NIBRSPublicView2019.csv")
crime_2019 = crime_2019[crime_2019$Beat!=""&crime_2019$Beat!="OOJ",]
crime_2019$weekday = weekdays(as.Date(crime_2019$RMSOccurrenceDate))
crime_2019$ZIPCode = substring(crime_2019$ZIPCode,1,5)
crime_2022 = read.csv("./crime_data/NIBRSPublicView2022.csv")
crime_2022 = crime_2022[crime_2022$Beat!=""&crime_2022$Beat!="OOJ",]
crime_2022$weekday = weekdays(as.Date(crime_2022$RMSOccurrenceDate))
crime_2022$ZIPCode = substring(crime_2022$ZIPCode,1,5)
crime_2023 = read.csv("./crime_data/NIBRSPublicView2023.csv")
crime_2023 = crime_2023[crime_2023$Beat!=""&crime_2023$Beat!="OOJ",]
crime_2023$weekday = weekdays(as.Date(crime_2023$RMSOccurrenceDate))
crime_2023$ZIPCode = substring(crime_2023$ZIPCode,1,5)

violent_codes = c("120","13A", "13B", "13C", "09A", "09B", "100", "11A", "11B", "11C", "11D", "36A", "36B")
homicide = c("09A", "09B")
theft = c("23A", "23B", "23C", "23D", "23E", "23F", "23G", "23H", "240")

violence_2019 = crime_2019[crime_2019$NIBRSClass %in% violent_codes,]
violence_2019$NIBRSDescription = str_to_title(violence_2019$NIBRSDescription)

violent_crime_zip_2019 = aggregate(violence_2019$OffenseCount, by=list(Name=violence_2019$ZIPCode), FUN=sum)
violent_crime_zip_2019 = violent_crime_zip_2019 %>% rename(violent_offenses_2019 = x)

violence_2019_premise = aggregate(violence_2019$OffenseCount, by=list(Name=violence_2019$NIBRSDescription), FUN=sum)
violence_2019_premise = violence_2019_premise %>% rename(count_2019 = x)

hom_2019 = crime_2019[crime_2019$NIBRSClass %in% homicide,]

hom_2019_zip = aggregate(hom_2019$OffenseCount, by=list(Name=hom_2019$ZIPCode), FUN=sum)
hom_2019_zip = hom_2019_zip %>% rename(homicide_2019 = x)

hom_2019_premise = aggregate(hom_2019$OffenseCount, by=list(Name=hom_2019$Premise), FUN=sum)
hom_2019_premise = hom_2019_premise %>% rename(homicide_2019 = x)

theft_2019 = crime_2019[crime_2019$NIBRSClass %in% theft,]
theft_zip_2019 = aggregate(theft_2019$OffenseCount, by=list(Name=theft_2019$ZIPCode), FUN=sum)
theft_zip_2019 = theft_zip_2019 %>% rename(theft_offenses_2019 = x)

violence_2022 = crime_2022[crime_2022$NIBRSClass %in% violent_codes,]
violence_2022$NIBRSDescription = str_to_title(violence_2022$NIBRSDescription)

violent_crime_zip_2022 = aggregate(violence_2022$OffenseCount, by=list(Name=violence_2022$ZIPCode), FUN=sum)
violent_crime_zip_2022 = violent_crime_zip_2022 %>% rename(violent_offenses_2022 = x)

violence_2022_premise = aggregate(violence_2022$OffenseCount, by=list(Name=violence_2022$NIBRSDescription), FUN=sum)
violence_2022_premise = violence_2022_premise %>% rename(count_2022 = x)

hom_2022 = crime_2022[crime_2022$NIBRSClass %in% homicide,]

hom_2022_zip = aggregate(hom_2022$OffenseCount, by=list(Name=hom_2022$ZIPCode), FUN=sum)
hom_2022_zip = hom_2022_zip %>% rename(homicide_2022 = x)

hom_2022_premise = aggregate(hom_2022$OffenseCount, by=list(Name=hom_2022$Premise), FUN=sum)
hom_2022_premise = hom_2022_premise %>% rename(homicide_2022 = x)

theft_2022 = crime_2022[crime_2022$NIBRSClass %in% theft,]
theft_zip_2022 = aggregate(theft_2022$OffenseCount, by=list(Name=theft_2022$ZIPCode), FUN=sum)
theft_zip_2022 = theft_zip_2022 %>% rename(theft_offenses_2022 = x)

violence_2023 = crime_2023[crime_2023$NIBRSClass %in% violent_codes,]
violence_2023$NIBRSDescription = str_to_title(violence_2023$NIBRSDescription)

violent_crime_zip_2023 = aggregate(violence_2023$OffenseCount, by=list(Name=violence_2023$ZIPCode), FUN=sum)
violent_crime_zip_2023 = violent_crime_zip_2023 %>% rename(violent_offenses_2023 = x)

violence_2023_premise = aggregate(violence_2023$OffenseCount, by=list(Name=violence_2023$NIBRSDescription), FUN=sum)
violence_2023_premise = violence_2023_premise %>% rename(count_2023 = x)

hom_2023 = crime_2023[crime_2023$NIBRSClass %in% homicide,]

hom_2023_zip = aggregate(hom_2023$OffenseCount, by=list(Name=hom_2023$ZIPCode), FUN=sum)
hom_2023_zip = hom_2023_zip %>% rename(homicide_2023 = x)

hom_2023_premise = aggregate(hom_2023$OffenseCount, by=list(Name=hom_2023$Premise), FUN=sum)
hom_2023_premise = hom_2023_premise %>% rename(homicide_2023 = x)

theft_2023 = crime_2023[crime_2023$NIBRSClass %in% theft,]
theft_zip_2023 = aggregate(theft_2023$OffenseCount, by=list(Name=theft_2023$ZIPCode), FUN=sum)
theft_zip_2023 = theft_zip_2023 %>% rename(theft_offenses_2023 = x)

theft_2019_type = aggregate(theft_2019$OffenseCount, by=list(Name=theft_2019$NIBRSDescription), FUN=sum)
theft_2019_type = theft_2019_type %>% rename(count_2019 = x)
theft_2022_type = aggregate(theft_2022$OffenseCount, by=list(Name=theft_2022$NIBRSDescription), FUN=sum)
theft_2022_type = theft_2022_type %>% rename(count_2022 = x)
theft_2023_type = aggregate(theft_2023$OffenseCount, by=list(Name=theft_2023$NIBRSDescription), FUN=sum)
theft_2023_type = theft_2023_type %>% rename(count_2023 = x)
theft_count = merge(theft_2019_type, merge(theft_2022_type, theft_2023_type, by="Name", all=TRUE),by="Name", all=TRUE)
theft_count[is.na(theft_count)] <- 0
theft_count$change_2019_2023 = (theft_count$count_2023 - theft_count$count_2019)
theft_count$change_2022_2023 = (theft_count$count_2023 - theft_count$count_2022)
theft_count$change_2019_2023_pct = 100 * (theft_count$count_2023 - theft_count$count_2019) / theft_count$count_2019
theft_count$change_2022_2023_pct = 100 * (theft_count$count_2023 - theft_count$count_2022) / theft_count$count_2022

violence_count = merge(violence_2019_premise, merge(violence_2022_premise, violence_2023_premise, by="Name", all=TRUE),by="Name", all=TRUE)
violence_count[is.na(violence_count)] <- 0
violence_count$change_2019_2023 = (violence_count$count_2023 - violence_count$count_2019)
violence_count$change_2022_2023 = (violence_count$count_2023 - violence_count$count_2022)
violence_count$change_2019_2023_pct = 100 * (violence_count$count_2023 - violence_count$count_2019) / violence_count$count_2019
violence_count$change_2022_2023_pct = 100 * (violence_count$count_2023 - violence_count$count_2022) / violence_count$count_2022

small = violence_count[violence_count$count_2023 < 150,]
medium = violence_count[violence_count$count_2023 <= 1000 & violence_count$count_2023 >= 150,]
large = violence_count[violence_count$count_2023 > 1000,]

hom_premise = merge(hom_2019_premise, merge(hom_2022_premise, hom_2023_premise, by="Name", all=TRUE),by="Name", all=TRUE)
hom_premise[is.na(hom_premise)] <- 0
hom_premise$change_2019_2023 = (hom_premise$homicide_2023 - hom_premise$homicide_2019)
hom_premise$change_2022_2023 = (hom_premise$homicide_2023 - hom_premise$homicide_2022)
hom_premise$change_2019_2023_pct = 100 * (hom_premise$homicide_2023 - hom_premise$homicide_2019) / hom_premise$homicide_2019
hom_premise$change_2022_2023_pct = 100 * (hom_premise$homicide_2023 - hom_premise$homicide_2022) / hom_premise$homicide_2022

set.seed(100)

violence_2023_Types = ggplot(violence_count, aes(Name,count_2023,fill=Name)) + geom_bar(stat="identity") + ggtitle("Violent Crimes (2023)") + xlab("Type") + ylab("Count") + theme(axis.text.x = element_text(angle = 90, size=8), plot.title = element_text(hjust = 0.5)) + theme(legend.position = "none")
ggsave("./analysis_plots/violence_2023_Types.png", violence_2023_Types,width=2500, height=2500,units="px")
violence_2023_Types_small = ggplot(small, aes(Name,count_2023,fill=Name)) + geom_bar(stat="identity") + ggtitle("Violent Crimes (2023)") + xlab("Type") + ylab("Count") + ylim(0,150) + theme(axis.text.x = element_text(angle = 90, size=8), plot.title = element_text(hjust = 0.5)) + theme(legend.position = "none")
ggsave("./analysis_plots/violence_2023_Types_small.png", violence_2023_Types_small,width=2500, height=2500,units="px")
violence_2023_Types_medium = ggplot(medium, aes(Name,count_2023,fill=Name)) + geom_bar(stat="identity") + ggtitle("Violent Crimes (2023)") + xlab("Type") + ylab("Count") + ylim(0,1000) + theme(axis.text.x = element_text(angle = 90, size=8), plot.title = element_text(hjust = 0.5)) + theme(legend.position = "none")
ggsave("./analysis_plots/violence_2023_Types_medium.png", violence_2023_Types_medium,width=2500, height=2500,units="px")
violence_2023_Types_large = ggplot(large, aes(Name,count_2023,fill=Name)) + geom_bar(stat="identity") + ggtitle("Violent Crimes (2023)") + xlab("Type") + ylab("Count") + theme(axis.text.x = element_text(angle = 90, size=8), plot.title = element_text(hjust = 0.5)) + theme(legend.position = "none")
ggsave("./analysis_plots/violence_2023_Types_large.png", violence_2023_Types_large,width=2500, height=2500,units="px")

violence_2022_Types = ggplot(violence_count, aes(Name,count_2022,fill=Name)) + geom_bar(stat="identity") + ggtitle("Violent Crimes (2022)") + xlab("Type") + ylab("Count") + theme(axis.text.x = element_text(angle = 90, size=8), plot.title = element_text(hjust = 0.5)) + theme(legend.position = "none")
ggsave("./analysis_plots/violence_2022_Types.png", violence_2022_Types,width=2500, height=2500,units="px")
violence_2022_Types_small = ggplot(small, aes(Name,count_2022,fill=Name)) + geom_bar(stat="identity") + ggtitle("Violent Crimes (2022)") + xlab("Type") + ylab("Count") + ylim(0,150) + theme(axis.text.x = element_text(angle = 90, size=8), plot.title = element_text(hjust = 0.5)) + theme(legend.position = "none")
ggsave("./analysis_plots/violence_2022_Types_small.png", violence_2022_Types_small,width=2500, height=2500,units="px")
violence_2022_Types_medium = ggplot(medium, aes(Name,count_2022,fill=Name)) + geom_bar(stat="identity") + ggtitle("Violent Crimes (2022)") + xlab("Type") + ylab("Count") + ylim(0,1000) + theme(axis.text.x = element_text(angle = 90, size=8), plot.title = element_text(hjust = 0.5)) + theme(legend.position = "none")
ggsave("./analysis_plots/violence_2022_Types_medium.png", violence_2022_Types_medium,width=2500, height=2500,units="px")
violence_2022_Types_large = ggplot(large, aes(Name,count_2022,fill=Name)) + geom_bar(stat="identity") + ggtitle("Violent Crimes (2022)") + xlab("Type") + ylab("Count") + theme(axis.text.x = element_text(angle = 90, size=8), plot.title = element_text(hjust = 0.5)) + theme(legend.position = "none")
ggsave("./analysis_plots/violence_2022_Types_large.png", violence_2022_Types_large,width=2500, height=2500,units="px")

violence_2019_Types = ggplot(violence_count, aes(Name,count_2019,fill=Name)) + geom_bar(stat="identity") + ggtitle("Violent Crimes (2019)") + xlab("Type") + ylab("Count") + theme(axis.text.x = element_text(angle = 90, size=8), plot.title = element_text(hjust = 0.5)) + theme(legend.position = "none")
ggsave("./analysis_plots/violence_2019_Types.png", violence_2019_Types,width=2500, height=2500,units="px")
violence_2019_Types_small = ggplot(small, aes(Name,count_2019,fill=Name)) + geom_bar(stat="identity") + ggtitle("Violent Crimes (2019)") + xlab("Type") + ylab("Count") + ylim(0,150) + theme(axis.text.x = element_text(angle = 90, size=8), plot.title = element_text(hjust = 0.5)) + theme(legend.position = "none")
ggsave("./analysis_plots/violence_2019_Types_small.png", violence_2019_Types_small,width=2500, height=2500,units="px")
violence_2019_Types_medium = ggplot(medium, aes(Name,count_2019,fill=Name)) + geom_bar(stat="identity") + ggtitle("Violent Crimes (2019)") + xlab("Type") + ylab("Count") + ylim(0,1000) + theme(axis.text.x = element_text(angle = 90, size=8), plot.title = element_text(hjust = 0.5)) + theme(legend.position = "none")
ggsave("./analysis_plots/violence_2019_Types_medium.png", violence_2019_Types_medium,width=2500, height=2500,units="px")
violence_2019_Types_large = ggplot(large, aes(Name,count_2019,fill=Name)) + geom_bar(stat="identity") + ggtitle("Violent Crimes (2019)") + xlab("Type") + ylab("Count") + theme(axis.text.x = element_text(angle = 90, size=8), plot.title = element_text(hjust = 0.5)) + theme(legend.position = "none")
ggsave("./analysis_plots/violence_2019_Types_large.png", violence_2019_Types_large,width=2500, height=2500,units="px")

violence_2019_2023_change_small = ggplot(small, aes(Name,change_2019_2023,fill=change_2019_2023 < 0)) + geom_bar(stat="identity") + ggtitle("Change in Violent Crimes by Type (2019 to 2023)") + xlab("Type") + ylab("Count") + theme(axis.text.x = element_text(angle = 90, size=8), plot.title = element_text(hjust = 0.5)) + theme(legend.position = "none") + scale_fill_manual(guide = FALSE, breaks = c(TRUE, FALSE), values=c("green3", "red"))
ggsave("./analysis_plots/violence_2019_2023_change_small.png", violence_2019_2023_change_small, width=2500, height=2500,units="px")
violence_2019_2023_change_medium = ggplot(medium, aes(Name,change_2019_2023,fill=change_2019_2023 < 0)) + geom_bar(stat="identity") + ggtitle("Change in Violent Crimes by Type (2019 to 2023)") + xlab("Type") + ylab("Count") + theme(axis.text.x = element_text(angle = 90, size=8), plot.title = element_text(hjust = 0.5)) + theme(legend.position = "none") + scale_fill_manual(guide = FALSE, breaks = c(TRUE, FALSE), values=c("green3", "red"))
ggsave("./analysis_plots/violence_2019_2023_change_medium.png", violence_2019_2023_change_medium, width=2500, height=2500,units="px")
violence_2019_2023_change_large = ggplot(large, aes(Name,change_2019_2023,fill=change_2019_2023 < 0)) + geom_bar(stat="identity") + ggtitle("Change in Violent Crimes by Type (2019 to 2023)") + xlab("Type") + ylab("Count") + theme(axis.text.x = element_text(angle = 90, size=8), plot.title = element_text(hjust = 0.5)) + theme(legend.position = "none") + scale_fill_manual(guide = FALSE, breaks = c(TRUE, FALSE), values=c("green3", "red"))
ggsave("./analysis_plots/violence_2019_2023_change_large.png", violence_2019_2023_change_large, width=2500, height=2500,units="px")

violence_2022_2023_change_small = ggplot(small, aes(Name,change_2022_2023,fill=change_2022_2023 < 0)) + geom_bar(stat="identity") + ggtitle("Change in Violent Crimes by Type (2022 to 2023)") + xlab("Type") + ylab("Count") + theme(axis.text.x = element_text(angle = 90, size=8), plot.title = element_text(hjust = 0.5)) + theme(legend.position = "none") + scale_fill_manual(guide = FALSE, breaks = c(TRUE, FALSE), values=c("green3", "red"))
ggsave("./analysis_plots/violence_2022_2023_change_small.png", violence_2022_2023_change_small, width=2500, height=2500,units="px")
violence_2022_2023_change_medium = ggplot(medium, aes(Name,change_2022_2023,fill=change_2022_2023 < 0)) + geom_bar(stat="identity") + ggtitle("Change in Violent Crimes by Type (2022 to 2023)") + xlab("Type") + ylab("Count") + theme(axis.text.x = element_text(angle = 90, size=8), plot.title = element_text(hjust = 0.5)) + theme(legend.position = "none") + scale_fill_manual(guide = FALSE, breaks = c(TRUE, FALSE), values=c("green3", "red"))
ggsave("./analysis_plots/violence_2022_2023_change_medium.png", violence_2022_2023_change_medium, width=2500, height=2500,units="px")
violence_2022_2023_change_large = ggplot(large, aes(Name,change_2022_2023,fill=change_2022_2023 < 0)) + geom_bar(stat="identity") + ggtitle("Change in Violent Crimes by Type (2022 to 2023)") + xlab("Type") + ylab("Count") + theme(axis.text.x = element_text(angle = 90, size=8), plot.title = element_text(hjust = 0.5)) + theme(legend.position = "none") + scale_fill_manual(guide = FALSE, breaks = c(TRUE, FALSE), values=c("green3", "red"))
ggsave("./analysis_plots/violence_2022_2023_change_large.png", violence_2022_2023_change_large, width=2500, height=2500,units="px")

violence_2019_2023_change_pct = ggplot(violence_count, aes(Name,change_2019_2023_pct,fill=change_2019_2023_pct < 0)) + geom_bar(stat="identity") + ggtitle("Change in Violent Crimes by Type, Percent (2019 to 2023)") + xlab("Type") + ylab("Percent") + coord_cartesian(ylim = c(-200, 200)) + theme(axis.text.x = element_text(angle = 90, size=8), plot.title = element_text(hjust = 0.5)) + theme(legend.position = "none") + scale_fill_manual(guide = FALSE, breaks = c(TRUE, FALSE), values=c("green3", "red"))
ggsave("./analysis_plots/violence_2019_2023_change_pct.png", violence_2019_2023_change_pct, width=2500, height=2500,units="px")
violence_2022_2023_change_pct = ggplot(violence_count, aes(Name,change_2022_2023_pct,fill=change_2022_2023_pct < 0)) + geom_bar(stat="identity") + ggtitle("Change in Violent Crimes by Type, Percent (2022 to 2023)") + xlab("Type") + ylab("Percent") + coord_cartesian(ylim = c(-200, 200)) + theme(axis.text.x = element_text(angle = 90, size=8), plot.title = element_text(hjust = 0.5)) + theme(legend.position = "none") + scale_fill_manual(guide = FALSE, breaks = c(TRUE, FALSE), values=c("green3", "red"))
ggsave("./analysis_plots/violence_2022_2023_change_pct.png", violence_2022_2023_change_pct, width=2500, height=2500,units="px")

theft_2023_Types = ggplot(theft_count, aes(Name,count_2023,fill=Name)) + geom_bar(stat="identity") + ggtitle("Thefts (2023)") + xlab("Type") + ylab("Count") + theme(axis.text.x = element_text(angle = 90, size=8), plot.title = element_text(hjust = 0.5)) + theme(legend.position = "none")
ggsave("./analysis_plots/theft_2023_Types.png", theft_2023_Types,width=2500, height=2500,units="px")
theft_2019_2023_change_pct = ggplot(theft_count, aes(Name,change_2019_2023_pct,fill=change_2019_2023_pct < 0)) + geom_bar(stat="identity") + ggtitle("Change in Thefts by Type, Percent (2019 to 2023)") + xlab("Type") + ylab("Percent") + coord_cartesian(ylim = c(-200, 200)) + theme(axis.text.x = element_text(angle = 90, size=8), plot.title = element_text(hjust = 0.5)) + theme(legend.position = "none") + scale_fill_manual(guide = FALSE, breaks = c(TRUE, FALSE), values=c("green3", "red"))
ggsave("./analysis_plots/theft_2019_2023_change_pct.png", theft_2019_2023_change_pct, width=2500, height=2500,units="px")
theft_2022_2023_change_pct = ggplot(theft_count, aes(Name,change_2022_2023_pct,fill=change_2022_2023_pct < 0)) + geom_bar(stat="identity") + ggtitle("Change in Thefts by Type, Percent (2022 to 2023)") + xlab("Type") + ylab("Percent") + coord_cartesian(ylim = c(-200, 200)) + theme(axis.text.x = element_text(angle = 90, size=8), plot.title = element_text(hjust = 0.5)) + theme(legend.position = "none") + scale_fill_manual(guide = FALSE, breaks = c(TRUE, FALSE), values=c("green3", "red"))
ggsave("./analysis_plots/theft_2022_2023_change_pct.png", theft_2022_2023_change_pct, width=2500, height=2500,units="px")

hom_2023_locations = ggplot(hom_premise, aes(Name,homicide_2023,fill=Name)) + geom_bar(stat="identity") + ggtitle("Homicide Locations (2023)") + xlab("Location") + ylab("Count") + ylim(0,140) + theme(axis.text.x = element_text(angle = 90, size=8), plot.title = element_text(hjust = 0.5)) + theme(legend.position = "none")
ggsave("./analysis_plots/hom_2023_locations.png", hom_2023_locations,width=2500, height=2500,units="px")
hom_2019_locations = ggplot(hom_premise, aes(Name,homicide_2019,fill=Name)) + geom_bar(stat="identity") + ggtitle("Homicide Locations (2019)") + xlab("Location") + ylab("Count") + ylim(0,140) + theme(axis.text.x = element_text(angle = 90, size=8), plot.title = element_text(hjust = 0.5)) + theme(legend.position = "none")
ggsave("./analysis_plots/hom_2019_locations.png", hom_2019_locations, width=2500, height=2500,units="px")
hom_2022_locations = ggplot(hom_premise, aes(Name,homicide_2022,fill=Name)) + geom_bar(stat="identity") + ggtitle("Homicide Locations (2022)") + xlab("Location") + ylab("Count") + ylim(0,170) + theme(axis.text.x = element_text(angle = 90, size=8), plot.title = element_text(hjust = 0.5)) + theme(legend.position = "none")
ggsave("./analysis_plots/hom_2022_locations.png", hom_2022_locations, width=2500, height=2500,units="px")
hom_2019_2023_change = ggplot(hom_premise, aes(Name,change_2019_2023,fill=change_2019_2023 < 0)) + geom_bar(stat="identity") + ggtitle("Change in Homicides by Location (2019 to 2023)") + xlab("Location") + ylab("Count") + theme(axis.text.x = element_text(angle = 90, size=8), plot.title = element_text(hjust = 0.5)) + theme(legend.position = "none") + scale_fill_manual(guide = FALSE, breaks = c(TRUE, FALSE), values=c("green3", "red"))
ggsave("./analysis_plots/hom_2019_2023_change.png", hom_2019_2023_change, width=2500, height=2500,units="px")
hom_2019_2023_change_pct = ggplot(hom_premise, aes(Name,change_2019_2023_pct,fill=change_2019_2023_pct < 0)) + geom_bar(stat="identity") + ggtitle("Change in Homicides by Location, Percent (2019 to 2023)") + xlab("Location") + ylab("Count") + coord_cartesian(ylim = c(-200, 200)) + theme(axis.text.x = element_text(angle = 90, size=8), plot.title = element_text(hjust = 0.5)) + theme(legend.position = "none") + scale_fill_manual(guide = FALSE, breaks = c(TRUE, FALSE), values=c("green3", "red"))
ggsave("./analysis_plots/hom_2019_2023_change_pct.png", hom_2019_2023_change_pct, width=2500, height=2500,units="px")
hom_2022_2023_change = ggplot(hom_premise, aes(Name,change_2022_2023,fill=change_2022_2023 < 0)) + geom_bar(stat="identity") + ggtitle("Change in Homicides by Location (2022 to 2023)") + xlab("Location") + ylab("Count") + theme(axis.text.x = element_text(angle = 90, size=8), plot.title = element_text(hjust = 0.5)) + theme(legend.position = "none") + scale_fill_manual(guide = FALSE, breaks = c(TRUE, FALSE), values=c("green3", "red"))
ggsave("./analysis_plots/hom_2022_2023_change.png", hom_2022_2023_change, width=2500, height=2500,units="px")
hom_2022_2023_change_pct = ggplot(hom_premise, aes(Name,change_2022_2023_pct,fill=change_2022_2023_pct < 0)) + geom_bar(stat="identity") + ggtitle("Change in Homicides by Location, Percent (2022 to 2023)") + xlab("Location") + ylab("Count") + coord_cartesian(ylim = c(-200, 200)) + theme(axis.text.x = element_text(angle = 90, size=8), plot.title = element_text(hjust = 0.5)) + theme(legend.position = "none") + scale_fill_manual(guide = FALSE, breaks = c(TRUE, FALSE), values=c("green3", "red"))
ggsave("./analysis_plots/hom_2022_2023_change_pct.png", hom_2022_2023_change_pct, width=2500, height=2500,units="px")

violence_time = aggregate(violence_2023$OffenseCount, by=list(Name=violence_2023$RMSOccurrenceHour), FUN=sum)
violence_time = violence_time %>% rename(offenses = x, hour = Name)

violence_date = aggregate(violence_2023$OffenseCount, by=list(Name=violence_2023$weekday), FUN=sum)
violence_date = violence_date %>% rename(offenses = x, weekday = Name)
violence_date$weekday = factor(violence_date$weekday, levels=days_of_the_week)

agg_ass_2023 = violence_2023[violence_2023$NIBRSDescription=="Aggravated Assault",]
agg_ass_time = aggregate(agg_ass_2023$OffenseCount, by=list(Name=agg_ass_2023$RMSOccurrenceHour), FUN=sum)
agg_ass_time = agg_ass_time %>% rename(offenses = x, hour = Name)

intimidation_2023 = violence_2023[violence_2023$NIBRSDescription=="Intimidation",]
intimidation_time = aggregate(intimidation_2023$OffenseCount, by=list(Name=intimidation_2023$RMSOccurrenceHour), FUN=sum)
intimidation_time = intimidation_time %>% rename(offenses = x, hour = Name)

simple_ass_2023 = violence_2023[violence_2023$NIBRSDescription=="Simple Assault",]
simple_ass_time = aggregate(simple_ass_2023$OffenseCount, by=list(Name=simple_ass_2023$RMSOccurrenceHour), FUN=sum)
simple_ass_time = simple_ass_time %>% rename(offenses = x, hour = Name)

