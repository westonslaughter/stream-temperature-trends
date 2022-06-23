# addressing basic questions and exploring trends in air and stream water temps``
library(dplyr)
library(data.table)
library(ggplot2)
library(viridis)

# read in data
air_wtr.raw <- fread("data/compiled/air_wtr_compiled.csv")

# add attributes for plotting and analysis later
# TEMPORAL attributes
floor_decade = function(value){ return(value - value %% 10) }

air_wtr <- air_wtr.raw %>%
  mutate(
    year = format(as.Date(date), "%Y"),
    month = factor(format(as.Date(date), "%m")),
    decade = as.character(floor_decade(as.numeric(year))),
    air_mean = (air_max + air_min)/2
  )

# SPATIAL/hydrogeo attributes
usgs_attr.raw <- read.csv("data/sites/info_compiled.csv")
usgs_attr <- usgs_attr.raw %>%
  select(
    site_code = site_no,
    lat = dec_lat_va,
    long = dec_long_va,
    ws_area = drain_area_va
  )


air_wtr <- air_wtr %>%
  merge(usgs_attr, by = 'site_code') %>%
  mutate(
    ws_area_log10 = log10(ws_area)
  )

# STATISTICAL attributes
# standard dev
air_wtr_stats <- air_wtr %>%
  group_by(site_code) %>%
  summarize(
    total_air_max = max(air_max),
    total_air_min = min(air_min),
    total_air_mean = mean(air_mean),
    total_wtr_max = max(wtr_max),
    total_wtr_min = min(wtr_min),
    total_wtr_mean = mean(wtr_mean),
    total_vp_max = max(vapor_pressure),
    total_vp_min = min(vapor_pressure),
    total_vp_mean = mean(vapor_pressure),
    total_dl_max = max(day_length),
    total_dl_min = min(day_length),
    total_dl_mean = mean(day_length),
    total_ppt_max = max(ppt),
    total_ppt_min = min(ppt),
    total_ppt_mean = mean(ppt),
    total_radiation_max = max(radiation),
    total_radiation_min = min(radiation),
    total_radiation_mean = mean(radiation),
    total_snow_max = max(snow),
    total_snow_min = min(snow),
    total_snow_mean = mean(snow)
  )
## fwrite(air_wtr_stats, 'data/compiled/air_wtr_thermoclimactic_summarystats.csv')

air_wtr <- air_wtr %>%
  merge(air_wtr_stats, by = 'site_code')

### DAILY
## MEANS
# scatter plots
scatter_func <- function(data, x, y, attr,  discrete = TRUE) {

  plt <- ggplot(data,
         aes_string(x = x,
             y = y)
             ) +
  geom_point(
    aes_string(color = attr)
  ) +
  geom_abline(slope=1) +
  scale_color_viridis(discrete = discrete, option = "plasma") +
  ylim(-15, 40) +
  xlab("Daily Mean Air Temperature") +
  ylab("Daily Mean Water Temperature") +
  ggtitle("Daily Mean Water Temp vs Daily Mean Air Temp, USGS 1980-2022") +
  theme_minimal() +
  theme(text = element_text(size = 30))

  return(plt)
}

# TEMPORAL DISTRIBUTION
# colored by year
scatter_func(air_wtr, 'air_mean', 'wtr_mean', 'year')
ggsave("data/img/scatter/air_wtr_mean_dv_year.png")

# colored by decade
scatter_func(air_wtr, 'air_mean', 'wtr_mean', 'decade')
ggsave("data/img/scatter/air_wtr_mean_dv_decade.png")

# colored by month
scatter_func(air_wtr, 'air_mean', 'wtr_mean', 'month')
ggsave("data/img/scatter/air_wtr_mean_dv_month.png")

# STATISTICAL/CLIMATIC DISTRIBUTIONS
# means
# ppt
scatter_func(air_wtr, 'air_mean', 'wtr_mean', 'total_ppt_mean', discrete = FALSE)
ggsave("data/img/scatter/air_wtr_mean_dv_ppt_mean.png")

# radiation
scatter_func(air_wtr, 'air_mean', 'wtr_mean', 'total_radiation_mean', discrete = FALSE)
ggsave("data/img/scatter/air_wtr_mean_dv_total_radiation_mean.png")

# day length
scatter_func(air_wtr, 'air_mean', 'wtr_mean', 'total_vp_mean', discrete = FALSE)
ggsave("data/img/scatter/air_wtr_mean_dv_total_vp_mean.png")

# snow
scatter_func(air_wtr, 'air_mean', 'wtr_mean', 'total_snow_mean', discrete = FALSE)
ggsave("data/img/scatter/air_wtr_mean_dv_total_snow_mean.png")

# day length
scatter_func(air_wtr, 'air_mean', 'wtr_mean', 'total_dl_mean', discrete = FALSE)
ggsave("data/img/scatter/air_wtr_mean_dv_total_dl_mean.png")

# SPATIAL / HYDROGEOLOGICAL DISTRIBUTIONS
# watershed area
scatter_func(air_wtr, 'air_mean', 'wtr_mean', 'ws_area_log10', discrete = FALSE)
ggsave("data/img/scatter/air_wtr_mean_dv_wsarealog10.png")

# latitude
scatter_func(air_wtr, 'air_mean', 'wtr_mean', 'lat', discrete = FALSE)
ggsave("data/img/scatter/air_wtr_mean_dv_latitude.png")

# longitude
scatter_func(air_wtr, 'air_mean', 'wtr_mean', 'long', discrete = FALSE)
ggsave("data/img/scatter/air_wtr_mean_dv_longitude.png")

# ecoregion
# hydraulic 'peak' season
