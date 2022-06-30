# addressing basic questions and exploring trends in air and stream water temps``
library(dplyr)
library(data.table)
library(feather)
library(ggplot2)
library(viridis)
library(ggpubr)
library(ggpmisc)

# focal daily data
## dv <- read_feather('data/munged/dv/combined/focal_air_wtr.feather')

# dv longterm data
dv <- read_feather('data/munged/dv/combined/temp_only__air_wtr.feather')

# TEMPORAL attributes
floor_decade = function(value){ return(value - value %% 10) }
dv <- dv %>%
  mutate(
    year = format(as.Date(date), "%Y"),
    month = factor(format(as.Date(date), "%m")),
    decade = as.character(floor_decade(as.numeric(year))),
    season = factor(quarters(as.Date(date)), levels = c("Q1", "Q2", "Q3", "Q4"),
                    labels = c("winter", "spring", "summer", "fall")),
    wtr.tmean = as.numeric(wtr.tmean),
    wtr.tmin  = as.numeric(wtr.tmin),
    wtr.tmax  = as.numeric(wtr.tmax),
    air.tmean = as.numeric(air.tmean),
    air.tmin  = as.numeric(air.tmin),
    air.tmax  = as.numeric(air.tmax),
    wtrair.tmean = wtr.tmean/air.tmean,
    wtrair.tmin = wtr.tmin/air.tmin,
    wtrair.tmax = wtr.tmax/air.tmax
  )

# SITE and SPATIAL attributes
## site_info <- read.csv('data/munged/sites/focal_sites_compiled.csv', colClasses = 'character')
site_info <- read.csv('data/munged/sites/dv_temp_sites_compiled.csv', colClasses = 'character')
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

# check out sites by meta-stats
dv.st <- dv %>%
  group_by(site_code) %>%
  summarise(
    min_yr = min(year),
    max_yr = max(year),
    span = length(unique(year))
  ) %>%
  arrange(span)

# only sites with full data from 1980-2021
sites <- dv.st %>%
  filter(max_yr > 2020,
         min_yr < 1981,
         span > 40)

dv.plot <- dv_info %>%
  filter(site_code %in% unique(sites$site_code))

# manual region
site_states <- c(
  'new york'=c("01417500", "01421000", "01425000", "01426500", "01428500"),
  'montana'=c('06041000', '12363000'),
  'texas'=c('08049500', '08062500', '08065350')
  )
## magmonths <- magma(12, alpha = 1, begin = 0, end = 1, direction = 1)
scatter_simple <- function(data, x, y, attr,  discrete = TRUE, stat = "Mean") {
  title <- paste0("Daily ", stat, " Water Temp vs Daily ", stat," Air Temp, USGS 1980-2022")
  x_txt <- paste0('Daily ', stat, ' Air Temperature')
  y_txt <- paste0('Daily ', stat, ' Water Temperature')

  data <- data %>%
    arrange(desc(year))

  plt <- ggplot(data,
         aes_string(x = x,
                    y = y
                    )) +
  geom_point(
    aes_string(color = attr)
  ) +
  geom_abline(slope=1) +
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
# show air-wtr distribution shrinking over years (and unique/shapestable air-temp dist signatures at sites)
scatter_simple(dv.plot, 'air.tmin', 'wtr.tmin', 'year', stat = "Mean", discrete = TRUE) +
  facet_wrap(~ site_code) +
  xlab('\nDaily Minimum Air Temperature (C)') +
  ylab('Daily Minimum Water Temperature (C)\n') +
  ggtitle('Water to Air Temperature Ratio at 10 USGS Gauges\n1980-2021')

ggsave('doc/fig/scatter/dv_longterm_airwtr_max_facetsite_colyr.png')

scatter_simple(dv.plot, 'air.tmin', 'wtr.tmin', 'month', stat = "Mean", discrete = TRUE) +
  facet_wrap(~ year) +
  xlab('\nDaily Minimum Air Temperature (C)') +
  ylab('Daily Minimum Water Temperature (C)\n') +
  ggtitle('Water to Air Temperature Ratio at 10 USGS Gauges\n1980-2021')

scatter_simple(dv.plot, 'air.tmax', 'wtr.tmax', 'month', stat = "Mean", discrete = TRUE) +
  facet_wrap(~ year) +
  xlab('\nDaily Maximum Air Temperature (C)') +
  ylab('Daily Maximum Water Temperature (C)\n') +
  ggtitle('Water to Air Temperature Ratio at 10 USGS Gauges\n1980-2021')

scatter_simple(dv.plot, 'air.tmean', 'wtr.tmean', 'month', stat = "Mean", discrete = TRUE) +
  facet_wrap(~ year) +
  xlab('\nDaily Mean Air Temperature (C)') +
  ylab('Daily Mean Water Temperature (C)\n') +
  ggtitle('Water to Air Temperature Ratio at 10 USGS Gauges\n1980-2021')

scatter_simple(dv.plot, 'air.tmean', 'wtr.tmean', 'month', stat = "Mean", discrete = TRUE) +
  facet_wrap(~ site_code)
ggsave('doc/fig/scatter/dv_longterm_airwtr_mean_facetsite_colmonth.png')

scatter_simple(dv.plot, 'air.tmean', 'wtr.tmean', 'season', stat = "Mean", discrete = TRUE) +
  facet_wrap(~ site_code)
ggsave('doc/fig/scatter/dv_longterm_airwtr_mean_facetsite_colseason.png')

scatter_simple(dv.plot, 'air.tmean', 'wtr.tmean', 'vapor', stat = "Mean", discrete = FALSE) +
  facet_wrap(~ site_code)
ggsave('doc/fig/scatter/dv_longterm_airwtr_mean_facetsite_colvapor.png')

# show air-water time series at each site
# remove air/wtr ratios greater than 30
dv.ratio <- dv.plot %>%
  filter(wtrair.tmean < 3,
         wtrair.tmean > -1)

formula <- y ~ x
ggplot(dv.ratio, aes(x = date, y = wtrair.tmean)) +
  geom_point() +
  facet_wrap(~ site_code) +
  geom_smooth(method='lm', se = FALSE) +
       stat_poly_eq(aes(label = paste(..rr.label..)),
       label.x.npc = "right", label.y.npc = 0.15,
       formula = formula, parse = TRUE, size = 3)+
       stat_fit_glance(method = 'lm',
                       method.args = list(formula = formula),
                       geom = 'text',
                       aes(label = paste("P-value = ", signif(..p.value.., digits = 4), sep = "")),
       label.x.npc = 'right', label.y.npc = 0.35, size = 3)
ggsave('doc/fig/ts/airwtr/dv_longterm_airwtr_mean_facetsite.png')

# annual TS
dv.annual <- dv.plot %>%
  group_by(site_code, year) %>%
  summarize(
    wtr.tmean = mean(wtr.tmean),
    wtr.tmin  = mean(wtr.tmin),
    wtr.tmax  = mean(wtr.tmax),
    air.tmean = mean(air.tmean),
    air.tmin  = mean(air.tmin),
    air.tmax  = mean(air.tmax),
    wtrair.tmean = mean(wtrair.tmean),
    wtrair.tmin = mean(wtrair.tmin),
    wtrair.tmax = mean(wtrair.tmax)
  )

ggplot(dv.annual, aes(x = as.numeric(year), y = wtr.tmax)) +
  geom_point() +
  facet_wrap(~ site_code) +
  geom_smooth(method='lm') +
       stat_poly_eq(aes(label = paste(..rr.label..)),
       label.x.npc = "right", label.y.npc = 0.15,
       formula = formula, parse = TRUE, size = 3)+
       stat_fit_glance(method = 'lm',
                       method.args = list(formula = formula),
                       geom = 'text',
                       aes(label = paste("P-value = ", signif(..p.value.., digits = 4), sep = "")),
       label.x.npc = 'right', label.y.npc = 0.35, size = 3)
ggsave('doc/fig/ts/airwtr/dv_longterm_wtr_annualtmax_facetsite.png')

# seasonal TS
dv.season <- dv.plot %>%
  group_by(site_code, year, season) %>%
  summarize(
    wtr.tmean = as.numeric(wtr.tmean),
    wtr.tmin  = as.numeric(wtr.tmin),
    wtr.tmax  = as.numeric(wtr.tmax),
    air.tmean = as.numeric(air.tmean),
    air.tmin  = as.numeric(air.tmin),
    air.tmax  = as.numeric(air.tmax),
    wtrair.tmean = mean(wtrair.tmean),
    wtrair.tmin = mean(wtrair.tmin),
    wtrair.tmax = mean(wtrair.tmax)
  )

ggplot(dv.season, aes(x = year, y = wtrair.tmax)) +
  geom_point() +
  facet_wrap(~ season) +
  geom_smooth(method='lm', se = FALSE) +
       stat_poly_eq(aes(label = paste(..rr.label..)),
       label.x.npc = "right", label.y.npc = 0.15,
       formula = formula, parse = TRUE, size = 3)+
       stat_fit_glance(method = 'lm',
                       method.args = list(formula = formula),
                       geom = 'text',
                       aes(label = paste("P-value = ", signif(..p.value.., digits = 4), sep = "")),
       label.x.npc = 'right', label.y.npc = 0.35, size = 3)
ggsave('doc/fig/ts/airwtr/dv_longterm_airwtr_seasonmax_facetseason.png')

# time series analysis of mean water temp
dv.site <- dv.plot[dv.plot$site_code == dv.plot$site_code[1],] %>%
  distinct()


begin <- min(dv.site$date)
end <- max(dv.site$date)

dv.ts <- ts(dv.site$wtr.tmean,
            start = as.Date(begin),
            end = as.Date(end),
            frequency = 365)
dv.annual <- aggregate(dv.ts, FUN = mean)
plot(dv.annual)

# moving average
library(TTR)
ts.sma <- SMA(dv.ts, n = 10)
plot.ts(ts.sma)

# lag difference
library(stats)
ts_lag <- stats::lag(dv.ts, 1)
ts_lead <- stats::lag(dv.ts, -1)
ts_diff <- diff(dv.ts, 1)

# decompose
dv.ts.stl <- stl(dv.ts, s.window="periodic")
plot(dv.ts.stl)
