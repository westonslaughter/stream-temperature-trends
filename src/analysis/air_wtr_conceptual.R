# script for conceptual figure conveying the idea behind this study:
# that air and water temperature in rivers is a sigmoid in its ideal form,
# and that real river data will reflect linear, exp. log, and sigmoidal relations
# that could be associated with drivers
                                        #
library(dplyr)
library(data.table)
library(feather)
library(ggplot2)
library(viridis)
library(ggpubr)
library(ggpmisc)
source("src/helpers.R")

# load in data
dv <- read_feather("data/dv/munged/ms_usgs_airwtr.feather") %>%
  mutate(
    year = format(as.Date(date), "%Y"),
    month = factor(format(as.Date(date), "%m")),
    decade = as.character(floor_decade(as.numeric(year))),
    half_decade = as.character(floor_timestep(as.numeric(year), 5)),
    season = factor(quarters(as.Date(date)), levels = c("Q1", "Q2", "Q3", "Q4"),
                    labels = c("winter", "spring", "summer", "fall")),
    wtr.tmean = as.numeric(wtr.tmean),
    air.tmean = as.numeric(air.tmean)
  ) %>%
  filter(year < 2022)


scatter_simple <- function(data, x, y, attr,  discrete = TRUE, stat = "Mean") {
  title <- paste0("Daily ", stat, " Water Temp vs Daily ", stat," Air Temp, USGS 1980-2022")
  x_txt <- paste0('Daily ', stat, ' Air Temperature')
  y_txt <- paste0('Daily ', stat, ' Water Temperature')

  data <- data %>%
    ## arrange(desc(year))
    arrange(year)

  plt <- ggplot(data,
         aes_string(x = x,
                    y = y
                    )) +
  geom_hline(yintercept=0, color='darkgrey', linetype='dashed') +
  geom_vline(xintercept=0, color='darkgrey', linetype='dashed') +
  geom_point(
    aes_string(color = attr)
  ) +
  geom_abline(slope=1,    color='#D22B2B', linetype='dashed') +
  scale_color_viridis(discrete = discrete, option = "magma", alpha = 0.4) +
  ylim(-15, 40) +
  ## xlab(x_txt) +
  ## ylab(y_txt) +
  theme_minimal() +
  theme(text = element_text(size = 30))
  ## theme(legend.position="none")

  return(plt)
}

# AIR:WTR
scatter_simple(dv, 'air.tmean', 'wtr.tmean', 'half_decade', stat = "Mean", discrete = TRUE) +
  facet_wrap(~ site_code) +
  xlab('\n Air Temperature (C)') +
  ylab(' Water Temperature (C)\n') +
  ggtitle('Water to Air Temperature Ratio at USGS Gauges\n1980-2021')
