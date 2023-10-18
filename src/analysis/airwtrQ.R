# addressing basic questions and exploring trends in air and stream water temps and discharge
library(dplyr)
library(data.table)
library(feather)
library(ggplot2)
library(viridis)
library(ggpubr)
library(ggpmisc)

library(dataRetrieval)

# dv longterm data
dv <- read_feather("data/dv/munged/airwtr/airwtrQ.feather")

# TEMPORAL attributes
floor_decade = function(value){ return(value - value %% 10) }

dv <- dv %>%
  mutate(
    site_code = site_no,
    year = format(as.Date(date), "%Y"),
    month = factor(format(as.Date(date), "%m")),
    decade = as.character(floor_decade(as.numeric(year))),
    season = factor(quarters(as.Date(date)), levels = c("Q1", "Q2", "Q3", "Q4"),
                    labels = c("winter", "spring", "summer", "fall")),
    wtr.tmean = as.numeric(mean),
    wtr.tmin  = as.numeric(min),
    wtr.tmax  = as.numeric(max),
    q.mean = as.numeric(q.mean),
    q.min  = as.numeric(q.min),
    q.max  = as.numeric(q.max),
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
## site_more <-  read_feather('data/dv/sites/awq/sites.feather') %>%
##   mutate(
##     state = substr(station_nm, nchar(station_nm)-1, nchar(station_nm)),
##   ) %>%
##   rename(
##     site_code = site_no
##   )

## site_extra <- readNWISsite(unique(site_info$site_code)) %>%
##   rename(site_code = site_no)
site_info <- read_feather('data/dv/sites/awq/sites_info.feather') %>%
  mutate(
    site_code = site_no,
    lat = dec_lat_va,
    long = dec_long_va,
    begin_year = lubridate::year(begin_date),
    end_year = lubridate::year(end_date),
    state = gsub('[. /),]', '', substr(station_nm, nchar(station_nm)-2, nchar(station_nm)))
  )

## %>%
##   full_join(site_extra, by = "site_code", .keep_all = TRUE)


## site_info <- site_info %>%
##   select(
##     site_code,
##     lat,
##     long,
##     ws_area,
##     begin_year,
##     end_year,
##     state
##   ) %>%
##   mutate(
##     paralell = as.character(round(as.numeric(lat))),
##     span = as.integer(end_year) - as.integer(begin_year)
##   )


# combine all data
dv.plot <- dv %>%
  merge(site_info, by = c('site_code')) %>%
  filter(year < 2005, year > 1995) %>%
  distinct(site_code, datetime, .keep_all = TRUE)

# site metadat
site_meta <- dv.plot %>%
  ## filter(!is.na(max),
  ##        !is.na(mean),
  ##        !is.na(min)) %>%
  group_by(site_code) %>%
  summarize(n=n(),
            n_yrs = n_distinct(lubridate::year(datetime)))

# filter out spotty data
## sites_enough_yrs <- unique(site_meta[site_meta$n_yrs >= 30,]$site_code)
## dv.plot <- dv.plot %>%
##   filter(site_code %in% sites_enough_yrs)

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
# show air-wtr distribution shrinking over years (and unique/shapestable air-temp dist signatures at sites)
scatter_simple(dv.plot, 'air.tmean', 'mean', 'month', stat = "Mean", discrete = TRUE) +
  facet_wrap(~ site_code) +
  xlab('\n Air Temperature (C)') +
  ylab(' Water Temperature (C)\n') +
  ggtitle('Water to Air Temperature Ratio at USGS Gauges\n1980-2021')

ggsave('doc/fig/scatter/dv_longterm_airwtr_max_facetsite_colyr.png')

scatter_simple(dv.plot, 'air.tmin', 'min', 'month', stat = "Min", discrete = TRUE) +
  facet_wrap(~ site_code) +
  xlab('\nDaily Minimum Air Temperature (C)') +
  ylab('Daily Minimum Water Temperature (C)\n') +
  ggtitle('Water to Air Temperature Ratio at USGS Gauges\n1980-2021')

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
  filter(wtrair.tmean < 4,
         wtrair.tmean > -2)

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
## ggsave('doc/fig/ts/airwtr/dv_longterm_airwtr_mean_facetsite.png')
library(stringr)
url = "https://climatedataguide.ucar.edu/sites/default/files/nao_station_annual.txt"
nao_noaa = read.table(url, sep="\t",fileEncoding = "UTF-8") %>%
  mutate(
    year = str_extract(V1, '.+?(?= )'),
    nao_index = as.numeric(str_extract(V1, ' (.+)'))
  ) %>%
  select(year, nao_index)
nao_noaa <- nao_noaa[-1,]


# annual TS
dv.annual <- dv.plot %>%
  group_by(site_code, year) %>%
  summarize(
    state = state,
    wtr.tmean = mean(wtr.tmean, na.rm = TRUE),
    wtr.tmin   = mean(wtr.tmin, na.rm = TRUE),
    wtr.tmax   = mean(wtr.tmax, na.rm = TRUE),
    air.tmean = mean(air.tmean, na.rm = TRUE),
    air.tmin   = mean(air.tmin, na.rm = TRUE),
    air.tmax   = mean(air.tmax, na.rm = TRUE),
    wtrair.tmean = mean(wtrair.tmean, na.rm = TRUE),
    wtrair.tmin =   mean(wtrair.tmin, na.rm = TRUE),
    wtrair.tmax =   mean(wtrair.tmax, na.rm = TRUE),
    q.yield = sum(q.mean, na.rm = TRUE)*86400,
    q.mean = mean(q.mean, na.rm = TRUE),
    q.min =   mean(q.min, na.rm = TRUE),
    q.max =   mean(q.max, na.rm = TRUE),
    airwtr.ratio = air.tmean/wtr.tmean,
    vapor = mean(vapor, na.rm = TRUE),
    ppt = mean(ppt, na.rm = TRUE),
    snow = mean(snow, na.rm = TRUE),
    radiation = mean(radiation, na.rm = TRUE)
  ) %>%
  full_join(nao_noaa, by=c('year')) %>%
  ungroup() %>%
  group_by(site_code) %>%
  mutate(
    airz = zcalc(air.tmean),
    wtrz = zcalc(wtr.tmean)
  )

ggplot(dv.annual, aes(x = ppt, y = nao_index)) +
  geom_point() +
  facet_wrap(~ state) +
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

zcalc <- function(x) {
  z <- (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)

  return(z)
}

# z scores, air, wtr, q
dv.z <- dv.plot %>%
  select(dataset, site_code, station_nm, datetime, max, mean, min, q.max, q.mean, q.min, air.tmax, air.tmin, air.tmean) %>%
  mutate(
    q.yield = q.mean * 86400
  )

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

wtr_temp_nonsig_q_non_sig <- list()
wtr_temp_up_q_up <- list()
wtr_temp_down_q_down <- list()
wtr_temp_up_q_down <- list()
wtr_temp_down_q_up <- list()

supporting <- list()
nonsupporting <- list()

wtr_temp_all <- list()

index <- 1

lm.df <- data.frame(matrix(ncol = 10, nrow = 0))
colnames(lm.df) <- c('site_code', 'wtrslope', 'wtrp', 'wtrR2',
                     'airslope', 'airp', 'airR2',
                     'qslope', 'qp', 'qR2')

for(site in unique(dv.z$site_code)) {
  ## site <- '12322000'
  ## site <- '06054500' # Montana
  ## site <- dv.z$site_code[1]
  dv.site <- dv.z[dv.z$site_code == site,]

  station <- unique(dv.site$station_nm)
  ## state <- unique(dv.site$state)
  ## ws_area <- unique(dv.site$ws_area)

  # NAs for this stat?
  na_sum <- sum(is.na(dv.site$min))
  n_sum <- length(dv.site$min)
  na_share <- na_sum/n_sum

  if(na_share < 0.5) {
    # get lm results

    # real TS zone
    # water
    dv.xts <- xts(zcalc(dv.site$mean), dv.site$date)
    dv.xts <- na.locf(dv.xts)

    # air
    air.xts <- xts(zcalc(dv.site$air.tmin), dv.site$date)
    air.xts <- na.locf(air.xts)

    # q
    q.xts <- xts(zcalc(dv.site$q.mean), dv.site$date)
    q.xts <- na.locf(q.xts)

    dv.app <- apply.yearly(dv.xts, mean)
    air.app <- apply.yearly(air.xts, mean)
    q.app <- apply.yearly(q.xts, mean)

    # linear model (wtr)
    dv.ts <- ts(dv.app, frequency = 1)
    mod = lm(coredata(dv.ts) ~ index(dv.ts))
    modsum = summary(mod)
    my.p = modsum$coefficients[2,4]
    r2 = modsum$adj.r.squared
    mylabel = bquote(italic(R)^2 == .(format(r2, digits = 3)))
    rp = vector()
    rp[1] = paste("r^2 ==", sprintf("%.3f",  r2))
    rp[2] = paste("p   ==", sprintf("%.3f",  my.p))
    wtrslope <- modsum$coefficients[2]
    wtr.m = paste("m   ==",  sprintf("%.3f",  wtrslope))

    # linear model (air)
    air.ts <- ts(air.app, frequency = 1)
    amod = lm(coredata(air.ts) ~ index(air.ts))
    amodsum = summary(amod)
    amy.p = amodsum$coefficients[2,4]
    ar2 = amodsum$adj.r.squared
    amylabel = bquote(italic(R)^2 == .(format(ar2, digits = 3)))
    arp = vector()
    arp[1] = paste("r^2 ==", sprintf("%.3f",  ar2))
    arp[2] = paste("p   ==",  sprintf("%.3f",  amy.p))
    airslope <- amodsum$coefficients[2]
    air.m = paste("m   ==",  sprintf("%.3f",  airslope))

    # linear model (q)
    q.ts <- ts(q.app, frequency = 1)
    qmod = lm(coredata(q.ts) ~ index(q.ts))
    qmodsum = summary(qmod)
    qmy.p = qmodsum$coefficients[2,4]
    qr2 = qmodsum$adj.r.squared
    qmylabel = bquote(italic(R)^2 == .(format(qr2, digits = 3)))
    qrp = vector()
    qrp[1] = paste("r^2 ==", sprintf("%.3f",  qr2))
    qrp[2] = paste("p   ==",  sprintf("%.3f",  qmy.p))
    qslope <- qmodsum$coefficients[2]
    q.m = paste("m   ==",  sprintf("%.3f",  qslope))

    # save results
    lm.this <- c(site, wtrslope, my.p, r2, airslope, amy.p, ar2, qslope, qmy.p, qr2)
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
      annotate('text', x = 3.6, y = 0.55, parse = TRUE,   size = 4, label = rp[1], color = 'blue') +
      annotate('text', x = 14.6, y = 0.55, parse = TRUE,   size = 4, label = rp[2], color = 'blue') +
      annotate('text', x = 26.6, y = 0.55, parse = TRUE,   size = 4, label = wtr.m, color = 'blue') +
      ylab('Water Temp') +
      xlab('Year') +
      theme_blank() +
      geom_hline(yintercept = 0, color = 'lightgrey') +
      theme(plot.title = element_text(hjust = 0.5))
            ## axis.line = element_blank())

    air.plt <- ggplot(air.ts) +
      # nao
      ## geom_vline(xintercept = nao1980, lwd = 4, color = 'grey90') +
      # base plot
      ylim(-0.75, 0.75) +
      geom_point(color = 'red') +
      geom_smooth(aes(y = coredata(air.ts), x = index(air.ts)), method = 'lm', se = FALSE, color = 'red') +
      # air stats
      stat_regline_equation(aes(y = coredata(air.ts), x = index(air.ts)),
                            ## label.x = 4.2,
                            label.y = 1.0,
                            size = 4, color = 'red') +
      annotate('text', x = 3.6, y = 0.55, parse = TRUE, size = 4, label = arp[1], color = 'red') +
      annotate('text', x = 14.6, y = 0.55, parse = TRUE, size = 4, label = arp[2], color = 'red') +
      annotate('text', x = 26.6, y = 0.55, parse = TRUE, size = 4, label = air.m, color = 'red') +
      ylab('Air Temp') +
      xlab('Year') +
      # plot stuff
      ggtitle(paste(site, ',', station)) +
      ## ggtitle('Mean Annual Minimum Daily Water and Air Temperatures, Z-Score',
      ##         subtitle = paste(site, ',', station)) +
      theme_blank() +
      geom_hline(yintercept = 0, color = 'lightgrey') +
      theme(plot.title = element_text(hjust = 0.5), axis.line = element_blank())

    q.plt <- ggplot(q.ts) +
      # nao
      ## geom_vline(xintercept = nao1980, lwd = 4, color = 'grey90') +
      # base plot
      ylim(-0.75, 0.75) +
      geom_point(color = 'black') +
      geom_smooth(aes(y = coredata(q.ts), x = index(q.ts)), method = 'lm', se = FALSE, color = 'black') +
      # q stats
      stat_regline_equation(aes(y = coredata(q.ts), x = index(q.ts)),
                            ## label.x = 4.2,
                            label.y = 1.0,
                            size = 4, color = 'black') +
      annotate('text', x = 3.6, y = 0.55, parse = TRUE, size = 4, label = qrp[1], color = 'black') +
      annotate('text', x = 14.6, y = 0.55, parse = TRUE, size = 4, label = qrp[2], color = 'black') +
      annotate('text', x = 26.6, y = 0.55, parse = TRUE, size = 4, label = q.m, color = 'black') +
      ylab('Mean Q Yield') +
      xlab('Year') +
      # plot stuff
      theme_blank() +
      geom_hline(yintercept = 0, color = 'lightgrey') +
      theme(plot.title = element_text(hjust = 0.5))

    both.plt <- ggarrange(air.plt, dv.plt, q.plt, ncol = 1, nrow = 3)
    wtr_temp_all[[site]] <- both.plt

    # hpyothesis buckets
    # supporting
    # in sites where air temp rising
    if(amy.p < 0.05) {
      # Q is going down (sig)
      if(qslope < 0 & qmy.p < 0.05) {
        supporting[[site]] <- both.plt
      # or, water tmep is going up (sig)
      } else if(wtrslope > 0 & my.p < 0.05) {
        supporting[[site]] <- both.plt
      } else {
        nonsupporting[[site]] <- both.plt
      }
    } else {
        nonsupporting[[site]] <- both.plt
    }

    if(my.p > 0.05 & qmy.p > 0.05) {
      wtr_temp_nonsig_q_non_sig[[site]] <- both.plt
    } else if(wtrslope > 0 & qslope > 0 ){
      wtr_temp_up_q_up[[site]] <- both.plt
    } else if(wtrslope <0 & qslope < 0) {
      wtr_temp_down_q_down[[site]] <- both.plt
    } else if(wtrslope > 0 & qslope < 0) {
      wtr_temp_up_q_down[[site]] <- both.plt
    } else if(wtrslope < 0 & qslope > 0) {
      wtr_temp_down_q_up[[site]] <- both.plt
    } else {
      print(paste(site, 'did not get assigned'))
    }

    index <- index + 1
  } else {
    print(paste('WARNING: site', site, 'omitted'))
  }
}

# what supports our hypothesis? two ends of the spectrum: in sites where air is significant increasing,
# Q is going down, water temp is going up, or, both
plot_grid(purrr::compact(supporting))
plot_grid(purrr::compact(nonsupporting))


plot_grid(purrr::compact(wtr_temp_nonsig_q_non_sig))
plot_grid(purrr::compact(wtr_temp_up_q_up))
plot_grid(purrr::compact(wtr_temp_down_q_down))
plot_grid(purrr::compact(wtr_temp_up_q_down))
plot_grid(purrr::compact(wtr_temp_down_q_up))

plot_grid(purrr::compact(wtr_temp_up))
plot_grid(purrr::compact(wtr_temp_down))
plot_grid(purrr::compact(wtr_temp_nonsig))
plot_grid(purrr::compact(wtr_temp_all))

## plot_grid(purrr::compact(sig.ts))
## plot_grid(purrr::compact(insig.ts))

## colnames(lm.df) <- c('site_code', 'wtrslope', 'wtrp', 'wtrR2', 'airslope', 'airp', 'airR2', 'qslope', 'qp', 'qR2')
## lm.df <- lm.df %>%arrange(desc(wtrslope))
## sig.lm <- lm.df[lm.df$wtrp <= 0.1,]
## sig.ts.sloped <- sig.ts[c(sig.lm$site_code)]
## plot_grid(purrr::compact(sig.ts.sloped))


# dv annual scatters
ggplot(dv.annual, aes(x = wtr.tmean, y = vapor)) +
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
