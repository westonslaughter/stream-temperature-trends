#### Script Info ####
# This script will download climate data using the daymet api (info here: 
# https://daymet.ornl.gov/web_services#single) for all sites in a given csv.


#### Script ####
library(sf)
library(glue)
library(feather)
library(tidyverse)
source("src/etl/etl_helpers.R")

site <- read_feather('data/dv/sites/awq/sites.feather')

# Read in usgs
sites <- read_feather('data/dv/sites/awq/sites_info.feather') %>%
  rename(lat=dec_lat_va, long=dec_long_va) %>%
  select(site_no, lat , long) %>%
  filter(site_no %in% site$site_no) %>%
  distinct()

# retrieve daymet data for all sites
for(i in 1:nrow(sites)){
    site_file <- glue('data/dv/raw/air/daymet/{s}.feather', s = sites[i,1])

    if(file.exists('site_file')) {
      print('site daymet file already downloaded')
    } else {
        url_request <- glue('https://daymet.ornl.gov/single-pixel/api/data?lat={lat}&lon={long}&vars=tmax,tmin,srad,vp,swe,prcp,dayl&start=1980-01-01&end=2021-12-31',
                            lat = sites$lat[i],
                            long = sites$long[i])

        temp_file <- tempfile(fileext = '.csv')
        download.file(url = url_request,
                      destfile = temp_file,
                      cacheOK = FALSE,
                      method = 'libcurl')

        d <- read.csv(temp_file, colClasses = 'numeric', skip = 7) %>%
            mutate(date = as.Date(yday, origin = paste0(year, '-01-01'))) %>%
            mutate(site_no = sites$site_no[i],
                   data_source = 'daymet') %>%
            select(date, site_no, dayl..s., prcp..mm.day., srad..W.m.2., swe..kg.m.2.,
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

# check things
## length(list.files('data/dv/raw/air/daymet/'))
