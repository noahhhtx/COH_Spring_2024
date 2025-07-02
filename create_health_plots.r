library(tmap)
library(sf)

healthmap=st_read("health_map.geojson")
tx_hwys = st_read("./geographic_data/TxDOT_National_Highway_System.geojson")

asthma_rate = (tm_shape(healthmap) + tm_polygons(col="asthma_2021", border.alpha=0.25, palette="Reds", title = "Asthma Rate in Adults (2021)") + tm_legend(bg.color="white", width=0.5) + tm_shape(tx_hwys) + tm_lines(col="black"))
tmap_save(asthma_rate, "./plots/asthma_plot.png")

asthma_trend = (tm_shape(healthmap) + tm_polygons(col="asthma_trend", border.alpha=0.25, palette="Reds", title = "Asthma Trend\n(2019 to 2021)") + tm_legend(bg.color="white", width=0.5) + tm_shape(tx_hwys) + tm_lines(col="black"))
tmap_save(asthma_trend,"./plots/asthma_trend_plot.png")

obesity_rate = (tm_shape(healthmap) + tm_polygons(col="obesity_2021", border.alpha=0.25, palette="Reds", title = "Obesity Rate in Adults (2021)") + tm_legend(bg.color="white", width=0.5) + tm_shape(tx_hwys) + tm_lines(col="black"))
tmap_save(obesity_rate, "./plots/obese_plot.png")

obese_trend = (tm_shape(healthmap) + tm_polygons(col="obesity_trend", border.alpha=0.25, palette="-RdYlGn", title = "Obesity Trend\n(2019 to 2021)") + tm_legend(bg.color="white", width=0.5) + tm_shape(tx_hwys) + tm_lines(col="black"))
tmap_save(obese_trend,"./plots/obese_trend_plot.png")

sedentary_rate = (tm_shape(healthmap) + tm_polygons(col="sedentary_2021", border.alpha=0.25, palette="Reds", title = "Sedentary Adults (2021)") + tm_legend(bg.color="white", width=0.5) + tm_shape(tx_hwys) + tm_lines(col="black"))
tmap_save(sedentary_rate, "./plots/sedentary_plot.png")

sedentary_trend = (tm_shape(healthmap) + tm_polygons(col="sedentary_trend", border.alpha=0.25, palette="-RdYlGn", title = "Sedentary Trend\n(2019 to 2021)") + tm_legend(bg.color="white", width=0.5) + tm_shape(tx_hwys) + tm_lines(col="black"))
tmap_save(sedentary_trend,"./plots/sedentary_trend_plot.png")

mental_rate = (tm_shape(healthmap) + tm_polygons(col="poor_mental_health_14_days_2021", border.alpha=0.25, palette="Reds", title = "Poor Mental Health\nin Adults, 14+\nDays (2021)") + tm_legend(bg.color="white", width=0.5) + tm_shape(tx_hwys) + tm_lines(col="black"))
tmap_save(mental_rate, "./plots/mental_plot.png")

mental_trend = (tm_shape(healthmap) + tm_polygons(col="poor_mental_health_14_days_trend", border.alpha=0.25, palette="Reds", title = "Mental Health Trend\n(2019 to 2021)") + tm_legend(bg.color="white", width=0.5) + tm_shape(tx_hwys) + tm_lines(col="black"))
tmap_save(mental_trend,"./plots/mental_trend_plot.png")