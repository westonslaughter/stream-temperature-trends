# script for in depth look at selected sites
# objective: investigate how small streams and their watersheds respond to
# increased solar energy input, looking at
# San Juan River Utah, 09379500
# Sagehen Creek, CA
# Hubbard Brook
# HJ Andres
# Walker Branch
library(macrosheds)
library(tidyr)
library(glue)
library(feather)
library(stringi)
library(ggplot2)
library(ggthemes)
library(dplyr)                                        #
library(ggpmisc)
library(ggpubr)
library(sjPlot)

### DATA PREP
## MacroSheds
ms_root = '~/science/macrosheds/data/ms/'

# check out vairablke names
ms_vars <- ms_download_variables()
temp_vars <- ms_vars[grep('Temp', ms_vars$variable_name),]

# check out domain names
ms_site_data <- ms_download_site_data()
case_sites_info <- ms_site_data[grep('walk|hbef|hjandrew', ms_site_data$domain),]
ms_cases <- unique(case_sites_info$domain)


ms_temp <- ms_load_product(macrosheds_root =  ms_root,
                prodname = 'stream_chemistry',
                domains = ms_cases,
                filter_vars = 'temp',
                warn = FALSE)
ms_q <- ms_load_product(macrosheds_root =  ms_root,
                prodname = 'discharge',
                domains = ms_cases,
                warn = FALSE)

case_data <- rbind(ms_temp, ms_q) %>%
  mutate(
    var = ms_drop_var_prefix(var)
  ) %>%
  pivot_wider(id_cols = c(datetime, site_code),
              names_from = var,
              values_from = val) %>%
  filter(!is.na(temp),
         !is.na(discharge)) %>%
  mutate(datetime = lubridate::date(datetime)) %>%
  rename(date = datetime)


## sites <- case_sites_info
# get DayMet air data for sites
## for(i in 1:nrow(sites)){
##     site_file <- glue('data/dv/raw/ms/air/{s}.feather', s = sites[i,]$site_code)

##     if(file.exists('site_file')) {
##       print('site daymet file already downloaded')
##     } else {
##         url_request <- glue('https://daymet.ornl.gov/single-pixel/api/data?lat={lat}&lon={long}&vars=tmax,tmin,srad,vp,swe,prcp,dayl&start=1980-01-01&end=2021-12-31',
##                             lat = sites$latitude[i],
##                             long = sites$longitude[i])

##         temp_file <- tempfile(fileext = '.csv')
##         download.file(url = url_request,
##                       destfile = temp_file,
##                       cacheOK = FALSE,
##                       method = 'libcurl')

##         d <- read.csv(temp_file, colClasses = 'numeric', skip = 7) %>%
##             mutate(date = as.Date(yday, origin = paste0(year, '-01-01'))) %>%
##             mutate(site_code = sites$site_code[i],
##                    data_source = 'daymet') %>%
##             select(date, site_code, dayl..s., prcp..mm.day., srad..W.m.2., swe..kg.m.2.,
##                    tmax..deg.c., tmin..deg.c., vp..Pa.) %>%
##             mutate(data_source = 'daymet') %>%
##             pivot_longer(cols = c('dayl..s.', 'prcp..mm.day.', 'srad..W.m.2.', 'swe..kg.m.2.',
##                                   'tmax..deg.c.', 'tmin..deg.c.', 'vp..Pa.'),
##                          names_to = 'var',
##                          values_to = 'val')

##         write_feather(d, site_file)

##         file.remove(temp_file)
##     }
## }

source('src/etl/etl_helpers.R')
ms_daymet <- featherCompiler('data/dv/raw/ms/air', site_filter = sites$site_code)
ms_air <- ms_daymet %>%
  pivot_wider(id_cols = c(date, site_code),
              names_from = var,
              values_from = val)
ms_case_data <- left_join(case_data, ms_air, by = c('date', 'site_code')) %>%
  mutate(year = lubridate::year(date),
         month = lubridate::month(date),
         airmean = (tmax..deg.c. + tmin..deg.c.)/2
         ) %>%
  filter(year > 1980)


zcalc <- function(x) {
  z <- (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
  return(z)
}



### DATA ANALYSIS
# let's look at mean annual summer temperatures at all domains
# get z score for means of air and water temp and Q

ms_summer_annual_data <- ms_case_data %>%
  # summer
  ## filter(month %in% c(6,7,8)) %>%
  # winter
  filter(month %in% c(1,11,12)) %>%
  filter(site_code %in% c('w3', 'w6', 'east_fork', 'west_fork', 'GSWS02', 'GSWS08', 'GSWS09')) %>%
  group_by(site_code, year) %>%
  summarize(
    wtrmean = mean(temp),
    qmean =   mean(discharge),
    airmean = mean(airmean),
  ) %>%
  mutate(
    wtrz = scale(wtrmean),
    qz = scale(qmean),
    airz = scale(airmean)
  ) %>%
  merge(case_sites_info, by = 'site_code')

library(ggpubr)
# plot
## air.plt <- ggplot(ms_summer_annual_data, aes(x = year)) +
##   geom_point(aes(y = airz, color = site_code)) +
##   ylim(3,-3) +
##   facet_wrap(~domain) +
##   theme_minimal()
## wtr.plt <- ggplot(ms_summer_annual_data, aes(x = year)) +
##   geom_point(aes(y = wtrz, color = site_code)) +
##   ylim(3,-3) +
##   facet_wrap(~domain) +
##   theme_minimal()
## q.plt <- ggplot(ms_summer_annual_data, aes(x = year)) +
##   geom_point(aes(y = qz, color = site_code)) +
##   ylim(3,-3) +
##   facet_wrap(~domain) +
##   theme_minimal()

## all.plt <- ggarrange(air.plt, wtr.plt, q.plt, ncol = 1, nrow = 3)

# time series version
library(xts)
run_lm <- function(x) {
    mod = lm(coredata(x) ~ index(x))
    modsum = summary(mod)
    rps = vector()
    pv = modsum$coefficients[2,4]
    r2 = modsum$adj.r.squared
    slope <- modsum$coefficients[2]

    rps['r2'] = paste("r^2 ==", sprintf("%.3f",  r2))
    rps['pv'] = paste("p   ==", sprintf("%.3f",  pv))
    rps['slope'] = paste("m   ==",  sprintf("%.3f",  slope))
    return(rps)
}

wtr_temp_all <- list()

# plots
for(domain in unique(ms_summer_annual_data$domain)) {
  for(site in unique(ms_summer_annual_data[ms_summer_annual_data$domain == domain,]$site_code)) {
    ms_case <- ms_case_data %>%
      # summer
      ## filter(month %in% c(6,7,8)) %>%
      # winter
      ## filter(month %in% c(1,11,12)) %>%
      filter(site_code == site)

    wtr.ts <- na.locf(xts(scale(ms_case$temp), ms_case$date))
    air.ts <- na.locf(xts(scale(ms_case$airmean), ms_case$date))
    q.ts   <- na.locf(xts(scale(ms_case$discharge), ms_case$date))

    ## total variability version
    ## wtr.ts <- na.locf(xts(abs(scale(ms_case$temp)), ms_case$date))
    ## air.ts <- na.locf(xts(abs(scale(ms_case$airmean)), ms_case$date))
    ## q.ts   <- na.locf(xts(abs(scale(ms_case$discharge)), ms_case$date))

    wtr.annual <- ts(apply.yearly(wtr.ts, mean), frequency = 1)
    air.annual <- ts(apply.yearly(air.ts, mean), frequency = 1)
    q.annual   <- ts(apply.yearly(q.ts, mean), frequency = 1)

    wtr.lm <- run_lm(wtr.annual)
    air.lm <- run_lm(air.annual)
    q.lm <- run_lm(q.annual)

    # plots
    wtr.plt <- ggplot(wtr.annual) +
      ylim(-0.75, 0.75) +
      geom_point(color = 'blue') +
      geom_smooth(aes(y=coredata(wtr.annual), x = index(wtr.annual)), method = 'lm', se = FALSE) +
      # water stats
      stat_regline_equation(aes(y=coredata(wtr.annual), x = index(wtr.annual)),
                            label.y = 1.0,
                            size = 4, color = 'blue') +
      annotate('text', x = 3.6, y = 0.55, parse = TRUE,   size = 4, label = rp[1], color = 'blue') +
      annotate('text', x = 14.6, y = 0.55, parse = TRUE,   size = 4, label = rp[2], color = 'blue') +
      annotate('text', x = 26.6, y = 0.55, parse = TRUE,   size = 4, label = wtr.m, color = 'blue') +
      ylab('Water Temp') +
      xlab('Year') +
      theme_minimal() +
      geom_hline(yintercept = 0, color = 'lightgrey') +
      theme(plot.title = element_text(hjust = 0.5))

    air.plt <- ggplot(air.annual) +
      ylim(-0.75, 0.75) +
      geom_point(color = 'red') +
      geom_smooth(aes(y = coredata(air.annual), x = index(air.annual)), method = 'lm', se = FALSE, color = 'red') +
      stat_regline_equation(aes(y = coredata(air.annual), x = index(air.annual)),
                            label.y = 1.0,
                            size = 4, color = 'red') +
      annotate('text', x = 3.6, y = 0.55, parse = TRUE, size = 4, label = arp[1], color = 'red') +
      annotate('text', x = 14.6, y = 0.55, parse = TRUE, size = 4, label = arp[2], color = 'red') +
      annotate('text', x = 26.6, y = 0.55, parse = TRUE, size = 4, label = air.m, color = 'red') +
      ylab('Air Temp') +
      xlab('Year') +
      ggtitle(paste(domain, ',', site)) +
      theme_minimal() +
      geom_hline(yintercept = 0, color = 'lightgrey') +
      theme(plot.title = element_text(hjust = 0.5), axis.line = element_blank())

    q.plt <- ggplot(q.annual) +
      ylim(-0.75, 0.75) +
      geom_point(color = 'black') +
      geom_smooth(aes(y = coredata(q.annual), x = index(q.annual)), method = 'lm', se = FALSE, color = 'black') +
      stat_regline_equation(aes(y = coredata(q.annual), x = index(q.annual)),
                            label.y = 1.0,
                            size = 4, color = 'black') +
      annotate('text', x = 3.6, y = 0.55, parse = TRUE, size = 4, label = qrp[1], color = 'black') +
      annotate('text', x = 14.6, y = 0.55, parse = TRUE, size = 4, label = qrp[2], color = 'black') +
      annotate('text', x = 26.6, y = 0.55, parse = TRUE, size = 4, label = q.m, color = 'black') +
      ylab('Mean Q') +
      xlab('Year') +
      theme_minimal() +
      geom_hline(yintercept = 0, color = 'lightgrey') +
      theme(plot.title = element_text(hjust = 0.5))

    # save to list
    all.plt <- ggarrange(air.plt, wtr.plt, q.plt, ncol = 1, nrow = 3)
    wtr_temp_all[[site]] <- all.plt
  }
}

plot_grid(purrr::compact(wtr_temp_all))

scatter_simple <- function(data, x, y, attr,  discrete = TRUE, stat = "Mean") {
  title <- paste0("Daily ", stat, " Water Temp vs Daily ", stat," Air Temp, Selected MacroSheds 1980-2022")
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

ms_case_scatter <- ms_case_data %>%
  mutate(month = as.character(month))
scatter_simple(ms_case_scatter, 'airmean', 'temp', 'month', stat = "Mean", discrete = TRUE) +
  facet_wrap(~ site_code) +
  xlab('\n Air Temperature (C)') +
  ylab(' Water Temperature (C)\n') +
  ggtitle('Water to Air Temperature Ratio at Selected MacroSheds Sites\n1980-2021')
