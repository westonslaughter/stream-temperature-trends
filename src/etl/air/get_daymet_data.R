#### Script Info ####
# This script will download climate data using the daymet api (info here: 
# https://daymet.ornl.gov/web_services#single) for all sites in a given csv.


#### Script ####
library(sf)
library(glue)
library(feather)
library(tidyverse)

# Read in usgs
usgs_info <- read.csv('data/sites/sites_filtered_compiled.csv', colClasses = 'character')

site_correct <- c()
for(index in 1:nrow(usgs_info)) {
  site <- usgs_info[index,]$site_code

  if(nchar(site) == 7) {
    site <- paste0("0", as.character(site))
  }

  site_correct <- c(site_correct, site)
}

usgs_info$site_code <- site_correct

# retrieve daymet data for all sites
for(i in 1:nrow(usgs_info)){
    
    url_request <- glue('https://daymet.ornl.gov/single-pixel/api/data?lat={lat}&lon={long}&vars=tmax,tmin,srad,vp,swe,prcp,dayl&start=1980-01-01&end=2021-12-31',
                        lat = usgs_info$lat[i],
                        long = usgs_info$long[i])
    
    temp_file <- tempfile(fileext = '.csv')
    download.file(url = url_request,
                  destfile = temp_file,
                  cacheOK = FALSE,
                  method = 'libcurl')
    
    d <- read.csv(temp_file, colClasses = 'numeric', skip = 7) %>%
        mutate(date = as.Date(yday, origin = paste0(year, '-01-01'))) %>%
        mutate(site_code = usgs_info[i,1],
               data_source = 'daymet') %>%
        select(date, site_code, dayl..s., prcp..mm.day., srad..W.m.2., swe..kg.m.2.,
               tmax..deg.c., tmin..deg.c., vp..Pa.) %>%
        mutate(data_source = 'daymet') %>%
        pivot_longer(cols = c('dayl..s.', 'prcp..mm.day.', 'srad..W.m.2.', 'swe..kg.m.2.',
                              'tmax..deg.c.', 'tmin..deg.c.', 'vp..Pa.'),
                     names_to = 'var',
                     values_to = 'val')
    
    write_feather(d, glue('data/daymet/{s}.feather',
                          s = usgs_info[i,1]))
    
    file.remove(temp_file)
    
}
