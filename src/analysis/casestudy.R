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


sites <- case_sites_info
# get DayMet air data for sites
for(i in 1:nrow(sites)){
    site_file <- glue('data/dv/raw/ms/air/{s}.feather', s = sites[i,]$site_code)

    if(file.exists('site_file')) {
      print('site daymet file already downloaded')
    } else {
        url_request <- glue('https://daymet.ornl.gov/single-pixel/api/data?lat={lat}&lon={long}&vars=tmax,tmin,srad,vp,swe,prcp,dayl&start=1980-01-01&end=2021-12-31',
                            lat = sites$latitude[i],
                            long = sites$longitude[i])

        temp_file <- tempfile(fileext = '.csv')
        download.file(url = url_request,
                      destfile = temp_file,
                      cacheOK = FALSE,
                      method = 'libcurl')

        d <- read.csv(temp_file, colClasses = 'numeric', skip = 7) %>%
            mutate(date = as.Date(yday, origin = paste0(year, '-01-01'))) %>%
            mutate(site_code = sites$site_code[i],
                   data_source = 'daymet') %>%
            select(date, site_code, dayl..s., prcp..mm.day., srad..W.m.2., swe..kg.m.2.,
                   tmax..deg.c., tmin..deg.c., vp..Pa.) %>%
            mutate(data_source = 'daymet') %>%
            pivot_longer(cols = c('dayl..s.', 'prcp..mm.day.', 'srad..W.m.2.', 'swe..kg.m.2.',
                                  'tmax..deg.c.', 'tmin..deg.c.', 'vp..Pa.'),
                         names_to = 'var',
                         values_to = 'val')

        write_feather(d, site_file)

        file.remove(temp_file)
    }
}

source('src/etl/etl_helpers.R')
ms_daymet <- featherCompiler('data/dv/raw/ms/air', site_filter = sites$site_code)
ms_air <- ms_daymet %>%
  pivot_wider(id_cols = c(date, site_code),
              names_from = var,
              values_from = val)
ms_case_data <- left_join(case_data, ms_air, by = c('date', 'site_code')) %>%
  mutate(year = lubridate::year(date),
         month = lubridate::month(date)) %>%
  filter(year > 1980)


zcalc <- function(x) {
  z <- (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
  return(z)
}



### DATA ANALYSIS
# let's look at mean annual summer temperatures at all domains
# get z score for means of air and water temp and Q

ms_summer_annual_data <- ms_case_data %>%
  filter(month %in% c(6,7,8)) %>%
  filter(site_code %in% c('w3', 'w6', 'east_fork', 'west_fork', 'GSWS02', 'GSWS08', 'GSWS09')) %>%
  mutate(year = as.character(year),
         airmean = (tmax..deg.c..y + tmax..deg.c..y)/2
         ) %>%
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
air.plt <- ggplot(ms_summer_annual_data, aes(x = year)) +
  geom_point(aes(y = airz, color = site_code)) +
  ylim(3,-3) +
  facet_wrap(~domain) +
  theme_minimal()
wtr.plt <- ggplot(ms_summer_annual_data, aes(x = year)) +
  geom_point(aes(y = wtrz, color = site_code)) +
  ylim(3,-3) +
  facet_wrap(~domain) +
  theme_minimal()
q.plt <- ggplot(ms_summer_annual_data, aes(x = year)) +
  geom_point(aes(y = qz, color = site_code)) +
  ylim(3,-3) +
  facet_wrap(~domain) +
  theme_minimal()

all.plt <- ggarrange(air.plt, wtr.plt, q.plt, ncol = 1, nrow = 3)
all.plt

# time series version
library(xts)

wtr.ts <- na.locf(xts(scale(ms_case_data$temp), ms_case_data$date))
wtr.annual <- apply.yearly(wtr.ts, mean)



# plots
for(domain in unique(ms_summer_annual_data$domain)) {
  for(site in unique(ms[ms$domain == domain,]$site)) {
    ms <- ms_summer_annual_data[ms_summer_annual_data$site_code == site,]
    ms_yrs <- ms$year
    air <- ms$airz
    wtr <- ms$wtrz
    q <- ms$qz


    mod = lm(wtr ~ ms_yrs)
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
    amod = lm(air ~ ms_yrs)
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
    qmod = lm(q ~ ms_yrs)
    qmodsum = summary(qmod)
    qmy.p = qmodsum$coefficients[2,4]
    qr2 = qmodsum$adj.r.squared
    qmylabel = bquote(italic(R)^2 == .(format(qr2, digits = 3)))
    qrp = vector()
    qrp[1] = paste("r^2 ==", sprintf("%.3f",  qr2))
    qrp[2] = paste("p   ==",  sprintf("%.3f",  qmy.p))
    qslope <- qmodsum$coefficients[2]
    q.m = paste("m   ==",  sprintf("%.3f",  qslope))

    # plots
    dv.plt <- ggplot(ms, aes(x=year, y = wtrz)) +
      ylim(-0.75, 0.75) +
      geom_point(color = 'blue') +
      geom_smooth(aes(y = wtrz, x = year), method = 'lm', se = FALSE) +
      # water stats
      stat_regline_equation(aes(y = wtr, x = ms_yrs),
                            ## label.x = 10.2,
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
  }
}


    both.plt <- ggarrange(air.plt, dv.plt, q.plt, ncol = 1, nrow = 3)
