#### Script Info ####
# This script will download climate data using the daymet api (info here: 
# https://daymet.ornl.gov/web_services#single) for all sites in a given csv.


#### Script ####
library(sf)
library(glue)
library(feather)
library(tidyverse)
source("src/etl/etl_helpers.R")

# Read in usgs

sitecodeCompiler <- function(fp) {
  site_matrix <- data.frame(matrix(ncol = 3, nrow = 1))
  colnames(site_matrix) <- c('site_code', 'lat', 'long')
  fp_list <- list.files(fp, full.names = TRUE)

  for(file in fp_list) {
    file_data <- read.csv(file, colClasses = 'character')

    file_matrix <- data.frame(matrix(ncol = 3, nrow = nrow(file_data)))
    colnames(file_matrix) <- c('site_code', 'lat', 'long')

    file_matrix$site_code <- file_data$site_no
    file_matrix$lat <- file_data$dec_lat_va
    file_matrix$long <- file_data$dec_long_va

    site_matrix <- rbind(site_matrix, file_matrix)
    print(paste('loaded in site codes from', file))
  }

  print(paste("total sites:", nrow(site_matrix)))
  return(site_matrix)
}

usgs_info <- sitecodeCompiler('data/munged/sites/codes/')[-1,]
## usgs_info <- sites

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
        mutate(site_code = usgs_info$site_code[i],
               data_source = 'daymet') %>%
        select(date, site_code, dayl..s., prcp..mm.day., srad..W.m.2., swe..kg.m.2.,
               tmax..deg.c., tmin..deg.c., vp..Pa.) %>%
        mutate(data_source = 'daymet') %>%
        pivot_longer(cols = c('dayl..s.', 'prcp..mm.day.', 'srad..W.m.2.', 'swe..kg.m.2.',
                              'tmax..deg.c.', 'tmin..deg.c.', 'vp..Pa.'),
                     names_to = 'var',
                     values_to = 'val')
    
    write_feather(d, glue('data/raw/dv/air/daymet/{s}.feather',
                          s = usgs_info[i,1]))
    
    file.remove(temp_file)
    
}

# check things
## length(list.files('data/raw/dv/air/daymet/'))
