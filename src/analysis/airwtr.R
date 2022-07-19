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
## dv <- read_feather('data/munged/dv/combined/temp_only__air_wtr.feather')

dv <- read_feather("data/dv/munged/airwtr/airwtr.feather")

# TEMPORAL attributes
floor_decade = function(value){ return(value - value %% 10) }

dv <- dv %>%
  mutate(
    year = format(as.Date(date), "%Y"),
    month = factor(format(as.Date(date), "%m")),
    decade = as.character(floor_decade(as.numeric(year))),
    season = factor(quarters(as.Date(date)), levels = c("Q1", "Q2", "Q3", "Q4"),
                    labels = c("winter", "spring", "summer", "fall")),
    ## wtr.tmean = as.numeric(wtr.tmean),
    ## wtr.tmin  = as.numeric(wtr.tmin),
    ## wtr.tmax  = as.numeric(wtr.tmax),
    ## air.tmean = as.numeric(air.tmean),
    ## air.tmin  = as.numeric(air.tmin),
    ## air.tmax  = as.numeric(air.tmax),
    ## wtrair.tmean = wtr.tmean/air.tmean,
    ## wtrair.tmin = wtr.tmin/air.tmin,
    ## wtrair.tmax = wtr.tmax/air.tmax
    wtrair.tmean = mean/air.tmean,
    wtrair.tmin = min/air.tmin,
    wtrair.tmax = max/air.tmax
  )

# SITE and SPATIAL attributes
## site_info <- read.csv('data/munged/sites/focal_sites_compiled.csv', colClasses = 'character')
site_info <- read_feather('data/dv//sites/sites.feather')

site_info <- site_info %>%
  select(
    site_code,
    lat,
    long,
    ws_area,
    begin_year,
    end_year,
    state
  ) %>%
  mutate(
    paralell = as.character(round(as.numeric(lat))),
    span = as.integer(end_year) - as.integer(begin_year)
  )

# combine all data
dv.plot <- dv %>%
  merge(site_info, by = c('site_code')) %>%
  filter(year < 2022)

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
  scale_color_viridis(discrete = discrete, option = "magma", alpha = 0.1) +
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
scatter_simple(dv.plot, 'air.tmean', 'mean', 'decade', stat = "Mean", discrete = TRUE) +
  facet_wrap(~ site_code) +
  xlab('\n Air Temperature (C)') +
  ylab(' Water Temperature (C)\n') +
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
library(zoo)
library(xts)
library(gridExtra)
library(ggeffects)
library(sjPlot)
library(ggpubr)

head(dv.plot)
head(site_info)

## dv.site <- dv.plot[dv.plot$site_code == unique(dv.plot$site_code)[14],]

# order dy plot by state
dv.plot <- dv.plot %>%
  arrange(desc(ws_area))


zcalc <- function(x) {
  z <- (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)

  return(z)
}

# z scores, air, wtr, q
dv.z <- dv.plot %>%
  select(dataset, state, ws_area, site_code, datetime, max, mean, min, air.tmax, air.tmin, air.tmean)

# El Nino
nao_yrs <- c(1897,1900,1903,1906,1915,1919,1926,1931,1941,1942,1958,1966,1973,
             1978,1979, 1980,1982, 1983,1986, 1987,1988,1991, 1992,1995,1997, 1998,2002, 2003, 2004, 2005, 2006, 2007,2009, 2010, 2014, 2015, 2016, 2018, 2019)

nao_index <- c()

for(i in nao_yrs) {
  index = i - 1980

  nao_index <- c(nao_index, index)
}

nao1980 <- nao_index[nao_index >= 0]

# for all sites
sig.ts <- list()
insig.ts <- list()
index <- 1

lm.df <- data.frame(matrix(ncol = 7, nrow = 0))
colnames(lm.df) <- c('site_code', 'wtrslope', 'wtrp', 'wtrR2', 'airslope', 'airp', 'airR2')

for(site in unique(dv.z$site_code)) {
  ## site <- '12322000'
  dv.site <- dv.z[dv.z$site_code == site,]
  state <- unique(dv.site$state)
  ws_area <- unique(dv.site$ws_area)

  # NAs for this stat?
  na_sum <- sum(is.na(dv.site$min))
  n_sum <- length(dv.site$min)
  na_share <- na_sum/n_sum

  if(na_share < 0.2) {
    # get lm results

    # real TS zone
    # water
    dv.xts <- xts(zcalc(dv.site$min), dv.site$date)
    dv.xts <- na.locf(dv.xts)

    # air
    air.xts <- xts(zcalc(dv.site$air.tmin), dv.site$date)
    air.xts <- na.locf(air.xts)

    dv.app <- apply.yearly(dv.xts, mean)
    air.app <- apply.yearly(air.xts, mean)

    # linear model (wtr)
    dv.ts <- ts(dv.app, frequency = 1)
    mod = lm(coredata(dv.ts) ~ index(dv.ts))
    modsum = summary(mod)
    my.p = modsum$coefficients[2,4]
    r2 = modsum$adj.r.squared
    mylabel = bquote(italic(R)^2 == .(format(r2, digits = 3)))
    rp = vector()
    rp[1] = paste("r^2 ==", r2)
    rp[2] = paste("p   ==", my.p)
    wtrslope <- modsum$coefficients[2]

    # linear model (air)
    air.ts <- ts(air.app, frequency = 1)
    amod = lm(coredata(air.ts) ~ index(air.ts))
    amodsum = summary(amod)
    amy.p = amodsum$coefficients[2,4]
    ar2 = amodsum$adj.r.squared
    amylabel = bquote(italic(R)^2 == .(format(ar2, digits = 3)))
    arp = vector()
    arp[1] = paste("r^2 ==", ar2)
    arp[2] = paste("p   ==",  amy.p)
    airslope <- amodsum$coefficients[2]

    # save results
    lm.this <- c(site, wtrslope, my.p, r2, airslope, amy.p, ar2)
    lm.df <- rbind(lm.df, lm.this)

    dv.plt <- ggplot(dv.ts) +
      # nao
      ## geom_vline(xintercept = nao1980, lwd = 5, color = 'grey90') +
      # base plot
      ylim(-0.75, 0.75) +
      geom_point(color = 'blue') +
      geom_smooth(aes(y = coredata(dv.ts), x = index(dv.ts)), method = 'lm', se = FALSE) +
      # water stats
      stat_regline_equation(aes(y = coredata(dv.ts), x = index(dv.ts)),
                            ## label.x = 10.2,
                            label.y = 1.0,
                            size = 4, color = 'blue') +
      annotate('text', x = 2.6, y = 0.8, parse = TRUE,   size = 4, label = rp[1], color = 'blue') +
      annotate('text', x = 2.6, y = 0.6, parse = TRUE, size = 4, label = rp[2], color = 'blue') +
      ylab('Water Temp') +
      xlab('Year') +
      # plot stuff
      ggtitle('Mean Annual Minimum Daily Water and Air Temperatures, Z-Score',
              subtitle = paste(state, ',', site, ', area:', ws_area)) +
      theme_blank() +
      geom_hline(yintercept = 0, color = 'lightgrey') +
      theme(plot.title = element_text(hjust = 0.5),
            axis.line = element_blank())

    air.plt <- ggplot(air.ts) +
      # nao
      ## geom_vline(xintercept = nao1980, lwd = 5, color = 'grey90') +
      # base plot
      ylim(-0.75, 0.75) +
      geom_point(color = 'red') +
      geom_smooth(aes(y = coredata(air.ts), x = index(air.ts)), method = 'lm', se = FALSE, color = 'red') +
      # air stats
      stat_regline_equation(aes(y = coredata(air.ts), x = index(air.ts)),
                            ## label.x = 4.2,
                            label.y = 1.0,
                            size = 4, color = 'red') +
      annotate('text', x = 2.6, y = 0.8, parse = TRUE, size = 4, label = arp[1], color = 'red') +
      annotate('text', x = 2.6, y = 0.6, parse = TRUE, size = 4, label = arp[2], color = 'red') +
      ylab('Air Temp') +
      xlab('Year') +
      # plot stuff
      theme_blank() +
      geom_hline(yintercept = 0, color = 'lightgrey') +
      theme(plot.title = element_text(hjust = 0.5))

    both.plt <- ggarrange(dv.plt, air.plt, ncol = 1, nrow = 2)

    if(my.p > 0.1) {
      insig.ts[[site]] <- both.plt
    } else {
      sig.ts[[site]] <- both.plt
    }

    index <- index + 1
  } else {
    print(paste('WARNING: site', site, 'omitted'))
  }
}

colnames(lm.df) <- c('site_code', 'wtrslope', 'wtrp', 'wtrR2', 'airslope', 'airp', 'airR2')
lm.df <- lm.df %>%
  arrange(desc(wtrslope))

sig.lm <- lm.df[lm.df$wtrp <= 0.1,]

sig.ts.sloped <- sig.ts[c(sig.lm$site_code)]
plot_grid(purrr::compact(sig.ts.sloped))

plot_grid(purrr::compact(sig.ts))
plot_grid(purrr::compact(insig.ts))

# real TS zone
dv.xts <- xts(dv.site$mean, dv.site$date)
dv.xts <- na.locf(dv.xts)

dv.app <- diff(dv.xts)
dv.app <- rollapply(dv.xts, 7, sd)
dv.app <- apply.yearly(dv.xts, mean)

dv.ts <- ts(dv.app, frequency = 1)
plot(dv.ts)

dcm <- decompose(dv.ts)
plot(dcm)

begin <- min(dv.site$date)
end <- max(dv.site$date)

dv.zoo <- zoo(dv.site$mean, dv.site$date)
dv.xts <- xts(dv.site$wtrair.tmean, dv.site$date)
dv.yr <- to.yearly(dv.xts)

plot(dv.yr)

plot(diff(dv.xts))
plot(rollapply(dv.xts, 30, mean))

# test fr autocorrelation
Box.test(dv.xts)

# fit model
m <- lm(coredata(dv.ts) ~ index(dv.ts))
plot(m)

# decomp
dv.ts <- ts(dv.xts, frequency = 365)
plot(dv.ts)

# fill in NAs
dv.ts <- na.locf(dv.ts)

dcm <- decompose(dv.ts)

dv.annual <- aggregate.ts(dv.ts, FUN = mean)
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
