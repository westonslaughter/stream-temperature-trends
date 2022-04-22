#### Script Info ####
# This scrip will search and download NOAA weather data from the Global Historical 
# Climate Network- daily database using rnoaa. 

#### Script ####

library(sf)
library(tidyverse)
library(lubridate)
library(feather)
library(glue)

#### Helper Functions ####
# Function to locate and download NOAA site near stream site
search_near_stations <- function(site, noaa_sites, buffer_km, start_year, end_year) {
    
    # Site = an sf object with a minimum of the columns: site_code and geometry
    # noaa_sites = an sf object with NOAA site info, made above 
    # buffer_km = the maximum distance from a stream gauge that will be searched 
    #     for NOAA site
    # start_year = numeric of when NOAA data should start (set same as stream start date)
    # end_year = numeric of when NOAA data should have data through (set same as stream end date)
    buffer_m <- buffer_km*1000
    site_buf <- site %>%
        st_buffer(buffer_m)
    
    near_noaa <- st_filter(noaa_station_sf, site_buf)
    
    
    near_noaa_time <- near_noaa %>%
        filter(first_year <= start_year,
               last_year >= end_year)
    
    if(nrow(near_noaa_time) == 0){
        print(paste0('No station was found within ', buffer_km, ' km of site: ', site$site_code))
        return(list(data = tibble(),
                    relation = tibble(site_code = site$site_code,
                                      NOAA_id = NA,
                                      distance_km = NA)))
    }
    
    
    near_noaa_time <- near_noaa_time %>%
        mutate(distance = as.numeric(st_distance(geometry, site$geometry)))
    
    nearest_station <- near_noaa_time %>%
        filter(distance == min(near_noaa_time$distance))
    
    if(nrow(nearest_station) > 1){
        nearest_station <- nearest_station %>%
            ungroup() %>%
            mutate(range = last_year-first_year)
        
        nearest_station <- nearest_station %>%
            filter(range == max(nearest_station$range)) %>%
            select(-range)
    }
    
    
    noaa_data <- rnoaa::ghcnd(stationid = nearest_station$id)
    
    col_names <- names(noaa_data)
    
    col_names[5:128] <- paste0(substr(col_names[5:128], 0, 5), '_', substr(col_names[5:128], 6, 7))
    
    colnames(noaa_data) <- col_names
    
    noaa_data_clean <- noaa_data %>%
        filter(element %in% c('TMAX', 'TMIN', 'PRCP'))
    
    noaa_data_clean_VALUE <- noaa_data_clean %>%
        select(id, year, month, element, grep('VALUE', col_names[5:128], value = T)) %>%
        pivot_longer(cols = grep('VALUE', col_names[5:128], value = T), values_to = 'VALUE') %>%
        mutate(day = str_split_fixed(name, '_', n = Inf)[,2]) %>%
        select(-name)
    
    noaa_data_clean_MFLAG <- noaa_data_clean %>%
        select(id, year, month, element, grep('MFLAG', col_names[5:128], value = T)) %>%
        pivot_longer(cols = grep('MFLAG', col_names[5:128], value = T), values_to = 'MFLAG') %>%
        mutate(day = str_split_fixed(name, '_', n = Inf)[,2]) %>%
        select(-name)
    
    noaa_data_clean_QFLAG <- noaa_data_clean %>%
        select(id, year, month, element, grep('QFLAG', col_names[5:128], value = T)) %>%
        pivot_longer(cols = grep('QFLAG', col_names[5:128], value = T), values_to = 'QFLAG') %>%
        mutate(day = str_split_fixed(name, '_', n = Inf)[,2]) %>%
        select(-name)
    
    noaa_data_clean_SFLAG <- noaa_data_clean %>%
        select(id, year, month, element, grep('SFLAG', col_names[5:128], value = T)) %>%
        pivot_longer(cols = grep('SFLAG', col_names[5:128], value = T), values_to = 'SFLAG') %>%
        mutate(day = str_split_fixed(name, '_', n = Inf)[,2]) %>%
        select(-name)
    
    all_noaa <- full_join(noaa_data_clean_VALUE, noaa_data_clean_MFLAG, by = c('id', 'year', 'month', 'day', 'element')) %>%
        full_join(., noaa_data_clean_QFLAG, by = c('id', 'year', 'month', 'day', 'element')) %>%
        full_join(., noaa_data_clean_SFLAG, by = c('id', 'year', 'month', 'day', 'element')) %>%
        filter(!is.na(VALUE)) %>%
        mutate(date = lubridate::ymd(paste0(year, '-', month, '-', day))) %>%
        mutate(VALUE = VALUE/10) %>%
        select(NOAA_id = id, date, element, VALUE, MFLAG, QFLAG, SFLAG) 
    
    relation_tib <- tibble(site_code = site$site_code,
                           NOAA_id = nearest_station$id,
                           distance_km = nearest_station$distance/1000)
    
    return(list(data = all_noaa,
                relation = relation_tib))
    
    
}


#### Read in USGS data ####
# Read in USGS Site Info
usgs_info <- read.csv('temperature/data/usgs/site_info.csv', colClasses = 'character')

# Convert to sf object
usgs_info <- usgs_info %>%
    select(site_no, station_nm, dec_lat_va, dec_long_va) %>%
    st_as_sf(., coords = c('dec_long_va', 'dec_lat_va'), crs = 4326) 

# Read in stream temperature 
usgs_temporal_info <- tibble()
usgs_temp_files <- list.files('temperature/data/usgs/temp/', full.names = T)
for(i in 1:length(usgs_temp_files)){
    temp_file <- read.csv(usgs_temp_files[i], colClasses = 'character')
    temp_summary <- temp_file %>%
        mutate(year = lubridate::year(ymd(dateTime))) %>%
        group_by(site_no) %>%
        summarise(start_year = min(year),
                  end_year = max(year))
    
    usgs_temporal_info <- rbind(usgs_temporal_info, temp_summary)
}

all_usgs_info <- full_join(usgs_info, usgs_temporal_info) %>%
    filter(!is.na(start_year)) %>%
    rename(site_code = site_no)

#### Set up NOAA data ####
# Get NOAA site info
noaa_station <- rnoaa::ghcnd_stations()

# Only want sites that have temperature measurements
noaa_station_all_vars <- noaa_station %>%
    group_by(id) %>%
    filter(element %in% c('TMAX', 'TMIN'))

# Conver to sf object
noaa_station_sf <- noaa_station %>% 
    filter(id %in% !!unique(noaa_station_all_vars$id)) %>%
    distinct(id, .keep_all = T) %>% 
    sf::st_as_sf(., coords = c('longitude', 'latitude'), crs = 4326)

#### Loop though usgs sites ####
relationsip_table <- tibble()
buffer_km <- 30
for(i in 1:nrow(all_usgs_info)){
    site <- all_usgs_info[i,]
    start_year <- all_usgs_info$start_year[i]
    end_year <- all_usgs_info$end_year[i]
    
    noaa_results <- search_near_stations(site = site, 
                                         noaa_sites = noaa_station_sf,
                                         buffer_km = buffer_km,
                                         start_year = start_year,
                                         end_year = end_year)
    
    if(nrow(noaa_results$data) == 0) next
    noaa_data <- noaa_results$data %>%
        rename(site_code = NOAA_id,
               var = element,
               val = VALUE) %>%
        mutate(date_source = 'NOAA')
    
    relationship <- noaa_results$relation %>%
        mutate(data_source = 'USGS')
    
    relationsip_table <- rbind(relationsip_table, relationship)
    
    write_feather(noaa_data, glue('temperature/data/noaa/climate_data/{s}.feather',
                                  s = relationship$NOAA_id))
}


relationsip_table <- relationsip_table %>%
    rename(usgs_site_no = site_code)

write_feather(relationsip_table, 'data/noaa/usgs_noaa_key.feather')







#### Compare trends ####
all_relation_tables <- list.files('temperature/data/usgs/noaa/relation/', 
                                  full.names = T)

relation_table <- map_dfr(all_relation_tables, read_feather)

usgs_temporal_info <- usgs_temporal_info %>%
    mutate(range = end_year-start_year)

usgs_site <- '02423130' 
noaa_site <- relation_table %>%
    filter(site_code == usgs_site) %>%
    pull(NOAA_id)

noaa_data <- read_feather(glue('temperature/data/usgs/noaa/climate_data/{s}.feather',
                              s = noaa_site)) %>%
    filter(element %in% c('TMIN', 'TMAX')) %>%
    pivot_wider(names_from = 'element', values_from = 'VALUE') %>%
    mutate(air_temp_mean = (TMAX+TMIN)/2) %>%
    mutate(year = year(date)) %>%
    filter(!is.na(air_temp_mean)) %>%
    group_by(NOAA_id, year) %>%
    summarise(annual_air_temp_mean = mean(air_temp_mean, na.rm = T),
              n = n()) %>%
    filter(n > 300)


usgs_data <- read_feather(glue('temperature/data/usgs/usgs_stream_temp/{s}.feather',
                              s = usgs_site)) %>%
    mutate(year = year(Date)) %>%
    filter(!is.na(X_00010_00003)) %>%
    group_by(site_no, year) %>%
    summarise(annual_stream_temp_mean = mean(X_00010_00003, na.rm = T),
              n = n()) %>%
    filter(n > 300)

joined_data <- full_join(noaa_data, usgs_data, by = 'year') %>%
    filter(!is.na(annual_stream_temp_mean))

ggplot(joined_data) +
    geom_point(aes(year, annual_stream_temp_mean), col = 'blue') +
    stat_smooth(aes(year, annual_stream_temp_mean), method = 'lm', col = 'blue') +
    geom_point(aes(year, annual_air_temp_mean), col = 'red') +
    stat_smooth(aes(year, annual_air_temp_mean), method = 'lm', col = 'red')
    

#### old code ####
# lat_longs <- read_csv('../data_processing/data/general/site_data.csv') %>%
#     filter(site_code %in% c('w6', 'west_fork', 'Oksrukuyik_Creek_2.7', 'GSLOOK',
#                             'Q3', 'Site1')) %>%
#     select(site_code, latitude, longitude) %>%
#     st_as_sf(., coords = c('longitude', 'latitude'), crs = 4326)




result <- search_near_stations(site, noaa_sites = noaa_station_sf, buffer_km = 50, 
                               start_year = 1980,
                               end_year = 2000)


noaa_data <- result$data
relation <- result$relation

