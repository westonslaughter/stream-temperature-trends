# addressing basic questions and exploring trends in air and stream water temps``
library(dplyr)
library(data.table)
library(ggplot2)
library(viridis)
library(ggpubr)

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
# watershed area
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

# ecoregion
usgs_eco.raw <- read.csv("data/sites/sites_eco.csv")
usgs_eco <- usgs_eco.raw %>%
  select(site_code, ecoregion)

air_wtr <- air_wtr %>%
  merge(usgs_eco, by = 'site_code')

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


## fwrite(air_wtr, 'data/compiled/air_wtr_all_attributes.csv')

### DAILY
## MEANS
# scatter plots
scatter_func <- function(data, x, y, attr,  discrete = TRUE, stat = "Mean") {
  title <- paste0("Daily ", stat, " Water Temp vs Daily ", stat," Air Temp, USGS 1980-2022")
  x_txt <- paste0('Daily ', stat, ' Air Temperature')
  y_txt <- paste0('Daily ', stat, ' Water Temperature')

  plt <- ggplot(data,
         aes_string(x = x,
                    y = y,
                    alpha = 0.5
                    )) +
  geom_point(
    aes_string(color = attr)
  ) +
  geom_abline(slope=1) +
  scale_color_viridis(discrete = discrete, option = "plasma") +
  ylim(-15, 40) +
  xlab(x_txt) +
  ylab(y_txt) +
  ## ggtitle(title) +
  theme_minimal() +
  theme(text = element_text(size = 30))

  return(plt)
}

scatter_minimalist_func <- function(data, x, y, attr,  discrete = TRUE, stat = "Mean") {
  title <- paste0("Daily ", stat, " Water Temp vs Daily ", stat," Air Temp, USGS 1980-2022")
  x_txt <- paste0('Daily ', stat, ' Air Temperature')
  y_txt <- paste0('Daily ', stat, ' Water Temperature')

  plt <- ggplot(data,
         aes_string(x = x,
                    y = y,
                    alpha = 0.5
                    )) +
  geom_point(
    aes_string(color = attr)
  ) +
  geom_abline(slope=1) +
  scale_color_viridis(discrete = discrete, option = "plasma") +
  ylim(-15, 40) +
  ## xlab(x_txt) +
  ## ylab(y_txt) +
  ## ggtitle(title) +
  theme_minimal() +
  theme(text = element_text(size = 30)) +
  theme(legend.position="none")

  return(plt)
}

# TEMPORAL DISTRIBUTION
# AIR/WTR MEANS
# colored by year
gg_yr_mn <- scatter_minimalist_func(air_wtr, 'air_mean', 'wtr_mean', 'year')
ggsave("data/img/scatter/air_wtr_mean_dv_year.png")

# colored by decade
gg_dec_mn <- scatter_minimalist_func(air_wtr, 'air_mean', 'wtr_mean', 'decade')
ggsave("data/img/scatter/air_wtr_mean_dv_decade.png")

# colored by month
gg_mo_mn <- scatter_minimalist_func(air_wtr, 'air_mean', 'wtr_mean', 'month')
ggsave("data/img/scatter/air_wtr_mean_dv_month.png")

ggarrange(gg_mo, gg_yr, gg_dec,
          ncol = 2, nrow = 2,
          widths = c(0.5, 0.5),
          heights = c(0.5, 0.5)
          )
ggsave("data/img/scatter/arranged/temporal_attributes_mean.png")

# AIR?WTR MAXS
# colored by year
gg_yr_mx <- scatter_minimalist_func(air_wtr, 'air_max', 'wtr_max', 'year', stat = 'Max')
ggsave("data/img/scatter/air_wtr_max_dv_year.png")

# colored by decade
gg_dec_mx <- scatter_minimalist_func(air_wtr, 'air_max', 'wtr_max', 'decade', stat = 'Max')
ggsave("data/img/scatter/air_wtr_max_dv_decade.png")

# colored by month
gg_mo_mx <- scatter_minimalist_func(air_wtr, 'air_max', 'wtr_max', 'month', stat = 'Max')
ggsave("data/img/scatter/air_wtr_max_dv_month.png")

ggarrange(gg_mo, gg_yr, gg_dec,
          ncol = 2, nrow = 2,
          widths = c(0.5, 0.5),
          heights = c(0.5, 0.5)
          )
ggsave("data/img/scatter/arranged/temporal_attributes_max.png")

# AIR?WTR MINS
# colored by year
gg_yr_min <- scatter_minimalist_func(air_wtr, 'air_min', 'wtr_min', 'year', stat = 'Min')
ggsave("data/img/scatter/air_wtr_min_dv_year.png")

# colored by decade
gg_dec_min <- scatter_minimalist_func(air_wtr, 'air_min', 'wtr_min', 'decade', stat = 'Min')
ggsave("data/img/scatter/air_wtr_min_dv_decade.png")

# colored by month
gg_mo_min <- scatter_minimalist_func(air_wtr, 'air_min', 'wtr_min', 'month', stat = 'Min')
ggsave("data/img/scatter/air_wtr_min_dv_month.png")

ggarrange(gg_mo, gg_yr, gg_dec,
          ncol = 2, nrow = 2,
          widths = c(0.5, 0.5),
          heights = c(0.5, 0.5)
          )
ggsave("data/img/scatter/arranged/temporal_attributes_min.png")

# all tmeporal
# months
months <- ggarrange(gg_mo_mn, gg_mo_mx, gg_mo_min, ncol = 3, nrow = 1,
                    labels = c("Means", "Maximums", "Minimums"),
                    font.label = list(size = 26),
                    vjust = 0.4,
                    common.legend = TRUE, legend = "right")
# years
years <- ggarrange(gg_yr_mn, gg_yr_mx, gg_yr_min, ncol = 3, nrow = 1,
                   common.legend = TRUE, legend = "right")
# decades
decades <- ggarrange(gg_dec_mn, gg_dec_mx, gg_dec_min,
                     ncol = 3, nrow = 1,
                     common.legend = TRUE, legend = "right")

ggarrange(months,
          years,
          decades,
          ncol = 1, nrow = 3
          )
ggsave("data/img/scatter/arranged/temporal_attributes_allstats.png")


# SPATIAL / HYDROGEOLOGICAL DISTRIBUTIONS
# MEAN
# watershed area
gg_wa <- scatter_func(air_wtr, 'air_mean', 'wtr_mean', 'ws_area_log10', discrete = FALSE)
ggsave("data/img/scatter/air_wtr_mean_dv_wsarealog10.png")

# latitude
gg_lat <- scatter_func(air_wtr, 'air_mean', 'wtr_mean', 'lat', discrete = FALSE)
ggsave("data/img/scatter/air_wtr_mean_dv_latitude.png")

# longitude
gg_long <- scatter_func(air_wtr, 'air_mean', 'wtr_mean', 'long', discrete = FALSE)
ggsave("data/img/scatter/air_wtr_mean_dv_longitude.png")

# ecoregion
gg_eco <- scatter_func(air_wtr, 'air_mean', 'wtr_mean', 'ecoregion')
ggsave("data/img/scatter/air_wtr_mean_dv_ecoregion.png")
# hydraulic 'peak' season

ggarrange(gg_wa, gg_lat, gg_long, gg_eco,
          ncol = 2, nrow = 2,
          widths = c(0.5, 0.5),
          heights = c(0.5, 0.5)
          )
ggsave("data/img/scatter/arranged/spatial_attributes_mean.png")

# all satst
gg_wa_mn <-  scatter_minimalist_func(air_wtr, 'air_mean', 'wtr_mean', 'ws_area_log10', discrete = FALSE)
gg_lat_mn <- scatter_minimalist_func(air_wtr, 'air_mean', 'wtr_mean', 'lat', discrete = FALSE)
gg_long_mn <-scatter_minimalist_func(air_wtr, 'air_mean', 'wtr_mean', 'long', discrete = FALSE)
gg_eco_mn <- scatter_minimalist_func(air_wtr, 'air_mean', 'wtr_mean', 'ecoregion')
gg_wa_min <-  scatter_minimalist_func(air_wtr, 'air_min', 'wtr_min', 'ws_area_log10', discrete = FALSE)
gg_lat_min <- scatter_minimalist_func(air_wtr, 'air_min', 'wtr_min', 'lat', discrete = FALSE)
gg_long_min <-scatter_minimalist_func(air_wtr, 'air_min', 'wtr_min', 'long', discrete = FALSE)
gg_eco_min <- scatter_minimalist_func(air_wtr, 'air_min', 'wtr_min', 'ecoregion')
gg_wa_max <-  scatter_minimalist_func(air_wtr, 'air_max', 'wtr_max', 'ws_area_log10', discrete = FALSE)
gg_lat_max <- scatter_minimalist_func(air_wtr, 'air_max', 'wtr_max', 'lat', discrete = FALSE)
gg_long_max <-scatter_minimalist_func(air_wtr, 'air_max', 'wtr_max', 'long', discrete = FALSE)
gg_eco_max <- scatter_minimalist_func(air_wtr, 'air_max', 'wtr_max', 'ecoregion')

# watershed area
wa <- ggarrange(gg_wa_mn, gg_wa_max, gg_wa_min, ncol = 3, nrow = 1,
                    labels = c("Means", "Maximums", "Minimums"),
                    font.label = list(size = 26),
                    vjust = 0.4,
                    common.legend = TRUE, legend = "right")

# er
eco <- ggarrange(gg_eco_mn, gg_eco_max, gg_eco_min, ncol = 3, nrow = 1,
                    common.legend = TRUE, legend = "right")

# latiudes
lat <- ggarrange(gg_lat_mn, gg_lat_max, gg_lat_min, ncol = 3, nrow = 1,
                    common.legend = TRUE, legend = "right")
# longitudes
long <- ggarrange(gg_long_mn, gg_long_max, gg_long_min, ncol = 3, nrow = 1,
                    common.legend = TRUE, legend = "right")

ggarrange(wa,
          eco,
          lat,
          long,
          ncol = 1, nrow = 4
          )
ggsave("data/img/scatter/arranged/spatial_attributes_allstats.png")


# STATISTICAL/CLIMATIC DISTRIBUTIONS
# MEAN
# ppt
gg_ppt_mean <- scatter_func(air_wtr, 'air_mean', 'wtr_mean', 'total_ppt_mean', discrete = FALSE)
ggsave("data/img/scatter/air_wtr_mean_dv_ppt_mean.png")

# radiation
gg_rad_mean <- scatter_func(air_wtr, 'air_mean', 'wtr_mean', 'total_radiation_mean', discrete = FALSE)
ggsave("data/img/scatter/air_wtr_mean_dv_total_radiation_mean.png")

# vapor pressure
gg_vp_mean <- scatter_func(air_wtr, 'air_mean', 'wtr_mean', 'total_vp_mean', discrete = FALSE)
ggsave("data/img/scatter/air_wtr_mean_dv_total_vp_mean.png")

# snow
gg_snow_mean <- scatter_func(air_wtr, 'air_mean', 'wtr_mean', 'total_snow_mean', discrete = FALSE)
ggsave("data/img/scatter/air_wtr_mean_dv_total_snow_mean.png")

# day length
gg_dl_mean <- scatter_func(air_wtr, 'air_mean', 'wtr_mean', 'total_dl_mean', discrete = FALSE)
ggsave("data/img/scatter/air_wtr_mean_dv_total_dl_mean.png")

ggarrange(gg_ppt_mean, gg_rad_mean, gg_vp_mean, gg_snow_mean, gg_dl_mean,
          ncol = 3, nrow = 2,
          widths = c(0.5, 0.5),
          heights = c(0.5, 0.5)
          )
ggsave("data/img/scatter/arranged/climate_attributes_mean.png")



gg_ppt_mean <- scatter_minimalist_func(air_wtr, 'air_mean', 'wtr_mean', 'total_ppt_mean', discrete = FALSE)
gg_rad_mean <- scatter_minimalist_func(air_wtr, 'air_mean', 'wtr_mean', 'total_radiation_mean', discrete = FALSE)
gg_vp_mean <-  scatter_minimalist_func(air_wtr, 'air_mean', 'wtr_mean', 'total_vp_mean', discrete = FALSE)
gg_snow_mean <-scatter_minimalist_func(air_wtr, 'air_mean', 'wtr_mean', 'total_snow_mean', discrete = FALSE)
gg_dl_mean <-  scatter_minimalist_func(air_wtr, 'air_mean', 'wtr_mean', 'total_dl_mean', discrete = FALSE)
gg_ppt_max <-  scatter_minimalist_func(air_wtr, 'air_mean', 'wtr_mean', 'total_ppt_max', discrete = FALSE)
gg_rad_max <-  scatter_minimalist_func(air_wtr, 'air_mean', 'wtr_mean', 'total_radiation_max', discrete = FALSE)
gg_vp_max <-   scatter_minimalist_func(air_wtr, 'air_mean', 'wtr_mean', 'total_vp_max', discrete = FALSE)
gg_snow_max <- scatter_minimalist_func(air_wtr, 'air_mean', 'wtr_mean', 'total_snow_max', discrete = FALSE)
gg_dl_max <-   scatter_minimalist_func(air_wtr, 'air_mean', 'wtr_mean', 'total_dl_max', discrete = FALSE)
gg_ppt_min <-  scatter_minimalist_func(air_wtr, 'air_mean', 'wtr_mean', 'total_ppt_min', discrete = FALSE)
gg_rad_min <-  scatter_minimalist_func(air_wtr, 'air_mean', 'wtr_mean', 'total_radiation_min', discrete = FALSE)
gg_vp_min <-   scatter_minimalist_func(air_wtr, 'air_mean', 'wtr_mean', 'total_vp_min', discrete = FALSE)
gg_snow_min <- scatter_minimalist_func(air_wtr, 'air_mean', 'wtr_mean', 'total_snow_min', discrete = FALSE)
gg_dl_min <-   scatter_minimalist_func(air_wtr, 'air_mean', 'wtr_mean', 'total_dl_min', discrete = FALSE)

# ppt
ppt <- ggarrange(gg_ppt_mean, gg_ppt_max, gg_ppt_min,
                 ncols = 3, nrow= 1,
                 labels = c("Mean", "Max", "Min"),
                 common.legend = TRUE, legend = "right")
# rad
rad <- ggarrange(gg_rad_mean, gg_rad_max, gg_rad_min,
                 ncols = 3, nrow= 1,
                 common.legend = TRUE, legend = "right")
# vp
vp <- ggarrange(gg_vp_mean, gg_vp_max, gg_vp_min,
                 ncols = 3, nrow= 1,
                 common.legend = TRUE, legend = "right")
# snow
snow <- ggarrange(gg_snow_mean, gg_snow_max, gg_snow_min,
                 ncols = 3, nrow= 1,
                 common.legend = TRUE, legend = "right")
# dl
dl <- ggarrange(gg_dl_mean, gg_dl_max, gg_dl_min,
                 ncols = 3, nrow= 1,
                 common.legend = TRUE, legend = "right")

# all stats
ggarrange(ppt, rad, vp, snow, dl,
          ncol = 1, nrow = 5)
ggsave("data/img/scatter/arranged/climate_attributes_max.png")


# facets
# TEMPORAL DISTRIBUTION
# AIR/WTR MEANS

scatter_minimalist_func(air_wtr, 'air_mean', 'wtr_mean', 'month') +
  facet_wrap( ~ year, nrow = 5)
ggsave("data/img/scatter/facet/grid_year_col_month_mean.png")

scatter_minimalist_func(air_wtr, 'air_mean', 'wtr_mean', 'year') +
  facet_wrap( ~ month, nrow = 3)
ggsave("data/img/scatter/facet/grid_month_col_year_mean.png")

scatter_minimalist_func(air_wtr, 'air_mean', 'wtr_mean', 'decade') +
  facet_wrap( ~ month, nrow = 3)
ggsave("data/img/scatter/facet/grid_month_col_decade_mean.png")

scatter_minimalist_func(air_wtr, 'air_mean', 'wtr_mean', 'year') +
  facet_wrap( ~ ecoregion, nrow = 3)

ggsave("data/img/scatter/facet/grid_eco_col_year_mean.png")

# Trend Analysis

# Seasonal Mann Kendall of Daily Data (per site)
site <- arrange(air_wtr[air_wtr$site_code == "10168000"], date)


# Mann Kendall of Mean Annual Data (per site)
# Mann Kendall of Mean Annual Fall/Winter/Spring/Summer Temp
