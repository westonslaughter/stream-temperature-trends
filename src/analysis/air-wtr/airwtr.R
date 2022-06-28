# addressing basic questions and exploring trends in air and stream water temps``
library(dplyr)
library(data.table)
library(feather)
library(ggplot2)
library(viridis)
library(ggpubr)

# focal daily data
dv <- read_feather('data/munged/dv/combined/focal_air_wtr.feather')

# check out sites by meta-stats
dv.st <- dv %>%
  group_by(site_code) %>%
  summarise(
    min_yr = min(year),
    max_yr = max(year),
    span = length(unique(year))
  ) %>%
  arrange(desc(span))

# TEMPORAL attributes
floor_decade = function(value){ return(value - value %% 10) }
dv <- dv %>%
  mutate(
    year = format(as.Date(date), "%Y"),
    month = factor(format(as.Date(date), "%m")),
    decade = as.character(floor_decade(as.numeric(year))),
    wtr.tmean = as.numeric(wtr.tmean),
    wtr.tmin  = as.numeric(wtr.tmin),
    wtr.tmax  = as.numeric(wtr.tmax),
    air.tmean = as.numeric(air.tmean),
    air.tmin  = as.numeric(air.tmin),
    air.tmax  = as.numeric(air.tmax)
  )

# SITE and SPATIAL attributes
site_info <- read.csv('data/munged/sites/focal_sites_compiled.csv', colClasses = 'character')
site_info <- site_info %>%
  select(
    site_code,
    lat,
    long,
    ws_area
  ) %>%
  mutate(
    paralell = as.character(round(as.numeric(lat)))
  )

# combine all data
dv_info <- dv %>%
  merge(site_info, by = c('site_code')) %>%
  filter(year > 1979,
         year < 2022)

scatter_simple <- function(data, x, y, attr,  discrete = TRUE, stat = "Mean") {
  title <- paste0("Daily ", stat, " Water Temp vs Daily ", stat," Air Temp, USGS 1980-2022")
  x_txt <- paste0('Daily ', stat, ' Air Temperature')
  y_txt <- paste0('Daily ', stat, ' Water Temperature')

  plt <- ggplot(data,
         aes_string(x = x,
                    y = y,
                    alpha = 0.1
                    )) +
  geom_point(
    aes_string(color = attr)
  ) +
  geom_abline(slope=1) +
  scale_color_viridis(discrete = discrete, option = "magma") +
  ylim(-15, 40) +
  ## xlab(x_txt) +
  ## ylab(y_txt) +
  ## ggtitle(title) +
  theme_minimal() +
  theme(text = element_text(size = 30))
  ## theme(legend.position="none")

  return(plt)
}

scatter_simple(dv_info, 'air.tmin', 'wtr.tmin', 'radiation', stat = "Mean", discrete = FALSE) + facet_wrap(~ year)
ggsave('doc/fig/scatter/airwtr_mean_facetyr_colvapor.png')
