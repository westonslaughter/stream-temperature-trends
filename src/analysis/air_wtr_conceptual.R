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
dv <- read_feather("data/dv/munged/ms_usgs_cmb_ssi_airwtr.feather") %>%
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

# make a small summary of the start and end years for each site
# we will use this to isolate only begin/end of site visually
site_start_end_years <- dv %>%
  group_by(site_code) %>%
  summarize(
    # diff year metrics
    n_yrs = length(unique(year)),
    begin_year = min(year),
    end_year = max(year)
  )
# carve up dv to just edge year
edge_n <- 5
dv_edge <- dv %>%
  group_by(site_code) %>%
  left_join(site_start_end_years, by = "site_code") %>%
  filter(
    as.numeric(year) < as.numeric(begin_year)+edge_n | as.numeric(year) > as.numeric(end_year)-edge_n
  )



scatter_concept <- function(data, x, y, attr,  discrete = TRUE, stat = "Mean") {
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

# AIR:WTR All Sites
scatter_concept(dv, 'air.tmean', 'wtr.tmean', 'decade', stat = "Mean", discrete = TRUE) +
  facet_wrap(~ site_code) +
  xlab('\n Air Temperature (C)') +
  ylab(' Water Temperature (C)\n') +
  ggtitle('Water to Air Temperature Ratio at USGS Gauges\n1980-2021')

# AIR:WTR Chesapeake Monitoring
cmb_dv <- dv %>%
  filter(!dataset %in% c("USGS", "macrosheds"))
scatter_concept(cmb_dv, 'air.tmean', 'wtr.tmean', 'year', stat = "Mean", discrete = TRUE) +
  facet_wrap(~ site_code) +
  xlab('\n Air Temperature (C)') +
  ylab(' Water Temperature (C)\n') +
  ggtitle('Water to Air Temperature Ratio at USGS Gauges\n1980-2021')

# AIR:WTR SSI Data
ssi_dv <- dv %>%
  filter(dataset %in% c("ssi"))
scatter_concept(ssi_dv, 'air.tmean', 'wtr.tmean', 'year', stat = "Mean", discrete = TRUE) +
  facet_wrap(~ site_code) +
  xlab('\n Air Temperature (C)') +
  ylab(' Water Temperature (C)\n') +
  ggtitle('Water to Air Temperature Ratio at USGS Gauges\n1980-2021')

## Case Studies, Narrative Exploration
## delaware strong sigmoid in 1999-2000 strong linear in 2020-2021
east_delaware_dv <- dv %>%
  filter(site_code %in% c("01417500"))
scatter_concept(east_delaware_dv, 'air.tmean', 'wtr.tmean', 'month', stat = "Mean", discrete = TRUE) +
  facet_wrap(~ year)

west_delaware_dv <- dv %>%
  filter(site_code %in% c("01426500", '01425000'))
scatter_concept(west_delaware_dv, 'air.tmean', 'wtr.tmean', 'site_code', stat = "Mean", discrete = TRUE) +
  facet_wrap(~ decade)

## Konza fire history changing thermal regime? Seems to ocmplicatedf for concept, pass for now
konza_dv <- dv %>%
  filter(site_code %in% c("N01B"))
scatter_concept(konza_dv, 'air.tmean', 'wtr.tmean', 'month', stat = "Mean", discrete = TRUE) +
  facet_wrap(~ year)

## Baltimore, urban lnear
bltmr_dv <- dv %>%
  filter(site_code %in% c("GFCP"))
scatter_concept(bltmr_dv, 'air.tmean', 'wtr.tmean', 'month', stat = "Mean", discrete = TRUE) +
  facet_wrap(~ half_decade)

## Bigelow, high sigmoid?
bigelow_dv <- dv %>%
  filter(site_code %in% c("Bigelow"))
scatter_concept(bigelow_dv, 'air.tmean', 'wtr.tmean', 'month', stat = "Mean", discrete = TRUE) +
  facet_wrap(~ year)

## Flathead River Montana
flathead_dv <- dv %>%
  filter(site_code %in% c("12363000"))
scatter_concept(flathead_dv, 'air.tmean', 'wtr.tmean', 'month', stat = "Mean", discrete = TRUE) +
  facet_wrap(~ decade)

## ??
flathead_dv <- dv %>%
  filter(site_code %in% c("02397530"))
scatter_concept(flathead_dv, 'air.tmean', 'wtr.tmean', 'month', stat = "Mean", discrete = TRUE) +
  facet_wrap(~ decade)

## ??
usgs_dv <- dv %>%
  filter(site_code %in% c("14187200"))
scatter_concept(usgs_dv, 'air.tmean', 'wtr.tmean', 'month', stat = "Mean", discrete = TRUE) +
  facet_wrap(~ decade)

## Cart Creek
cart_dv <- dv %>%
  filter(site_code %in% c("cart_creek"))
scatter_concept(cart_dv, 'air.tmean', 'wtr.tmean', 'month', stat = "Mean", discrete = TRUE) +
  facet_wrap(~ decade)

## saw mill brook
saw_dv <- dv %>%
  filter(site_code %in% c("saw_mill_brook"))
scatter_concept(saw_dv, 'air.tmean', 'wtr.tmean', 'month', stat = "Mean", discrete = TRUE) +
  facet_wrap(~ decade)

blackearth_dv <- dv %>%
  filter(site_code %in% c("black_earth_creek"))
scatter_concept(blackearth_dv, 'air.tmean', 'wtr.tmean', 'month', stat = "Mean", discrete = TRUE) +
  facet_wrap(~ decade)

hbef_dv <- dv %>%
  filter(site_code %in% c("w6"))
scatter_concept(hbef_dv, 'air.tmean', 'wtr.tmean', 'month', stat = "Mean", discrete = TRUE) +
  facet_wrap(~ year)

oracle_dv <- dv %>%
  filter(site_code %in% c("OracleRidge"))
scatter_concept(oracle_dv, 'air.tmean', 'wtr.tmean', 'month', stat = "Mean", discrete = TRUE) +
  facet_wrap(~ year)

## ??
flathead_falls_dv <- dv %>%
  filter(site_code %in% c("12363000"))
scatter_concept(flathead_falls_dv, 'air.tmean', 'wtr.tmean', 'month', stat = "Mean", discrete = TRUE) +
  facet_wrap(~ decade)


## seems lightly expionential > linear
usgs_dv <- dv %>%
  filter(site_code %in% c("02337170"))
scatter_concept(usgs_dv, 'air.tmean', 'wtr.tmean', 'month', stat = "Mean", discrete = TRUE) +
  facet_wrap(~ decade)

usgs_dv <- dv %>%
  filter(site_code %in% c("06041000"))
scatter_concept(usgs_dv, 'air.tmean', 'wtr.tmean', 'month', stat = "Mean", discrete = TRUE) +
  facet_wrap(~ decade)

# flat boi
usgs_dv <- dv %>%
  filter(site_code %in% c("11289650"))
scatter_concept(usgs_dv, 'air.tmean', 'wtr.tmean', 'month', stat = "Mean", discrete = TRUE) +
  facet_wrap(~ decade)

usgs_dv <- dv %>%
  filter(site_code %in% c("14139800"))
scatter_concept(usgs_dv, 'air.tmean', 'wtr.tmean', 'month', stat = "Mean", discrete = TRUE) +
  facet_wrap(~ decade)

# sigmoid > log ... Applegate River, OR
usgs_dv <- dv %>%
  filter(site_code %in% c("14366000"))
scatter_concept(usgs_dv, 'air.tmean', 'wtr.tmean', 'month', stat = "Mean", discrete = TRUE) +
  facet_wrap(~ decade)

# Kuparukl River, seems like Blob > Lin roughly
usgs_dv <- dv %>%
  filter(site_code %in% c("Kuparuk_River_0.56"))
scatter_concept(usgs_dv, 'air.tmean', 'wtr.tmean', 'month', stat = "Mean", discrete = TRUE) +
  facet_wrap(~ decade)

# Kuparukl River, seems like Blob > Lin roughly
usgs_dv <- dv %>%
  filter(site_code %in% c("GSMACK"))
scatter_concept(usgs_dv, 'air.tmean', 'wtr.tmean', 'month', stat = "Mean", discrete = TRUE) +
  facet_wrap(~ half_decade)

# NOTE: seems some sites (some USGS some MS) have below zero. Will not tolerate anything more than 1C belo 0.
dv <- dv %>%
  filter(wtr.tmean > -1)

# custom colors for sites
site_colors <- dv %>%
  mutate(
    color = case_when(site_code == '01426500' ~ 'red', TRUE ~ '00000000')
  ) %>%
  pull(color)

# actual figure func and drafting
# Delaware Case Study
west_delaware_dv_1999 <- west_delaware_dv %>%
  filter(
    site_code == "01426500",
    year %in% c("1980", "1981")
  )

west_delaware_dv_2020 <- west_delaware_dv %>%
  filter(
    site_code == "01426500",
    year %in% c("2020", "2019")
         )

scatter_figure <- function(data, x, y, attr,  discrete = TRUE, stat = "Mean") {
  title <- paste0("Daily ", stat, " Water Temp vs Daily ", stat," Air Temp\nUSGS and MacroSheds River Monitoring Sites")
  x_txt <- paste0('Daily ', stat, ' Air Temperature (C)')
  y_txt <- paste0('Daily ', stat, ' Water Temperature (C)')

  data <- data %>%
    ## arrange(desc(year))
    arrange(year)

  plt <- ggplot(data,
         ) +
  geom_hline(yintercept=0, color='darkgrey', linetype='dashed') +
  geom_vline(xintercept=0, color='darkgrey', linetype='dashed') +
  geom_point(
    aes_string(x=x, y=y),
    color = 'darkgrey',
    alpha = 0.005,
        size = 0.5
  ) +
    geom_point(
      data = west_delaware_dv_1999,
      color = "blue",
      aes_string(
          x=x,
          y=y,
        alpha = 0.7
        )) +
    geom_point(
      data = west_delaware_dv_2020,
      color = "red",
      aes_string(
          x=x,
          y=y,
          alpha = 0.7)) +
  geom_abline(slope=1,    color='#D22B2B', linetype='dashed') +
  ylim(-5, 40) +
  xlab(x_txt) +
  ylab(y_txt) +
  theme_minimal() +
  theme(text = element_text(size = 30))
  ## theme(legend.position="none")

  return(plt)
}


scatter_figure(dv, 'air.tmean', 'wtr.tmean', 'month', stat = "Mean", discrete = TRUE)
