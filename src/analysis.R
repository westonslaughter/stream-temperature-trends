### ---------------------------
##
## Title: analysis.R
##
## Purpose: this script performs basic analysis of stream and air temperatue data.
##
## Author: WS
##
## Date Created: 2022-04-29
##
## ---------------------------
library(dplyr)
library(ggplot2)
library(feather)

source("./src/helpers.R")

## Data
# USGS stream temp and discharge
raw_stream_temp <- read.csv("./data/dv/raw_temp_dv_compiled.csv")
raw_stream_disch <- read.csv("./data/dv/raw_disch_dv_compiled.csv")

# NOAA to USGS site code key
usgs_noaa_key <- read_feather("./data/air/air_temp/usgs_noaa_key.feather")

# TODO: implement multiprocessing ('featherCompiler' takes a long time)
daymet_files <- list.files("./data/air/air_temp/daymet", full.names = TRUE)
daymet_data <- featherCompiler(daymet_files)

# clean USGS data
# temp
stream_temp <- raw_stream_temp %>%
#   remove provisional data
    filter(X_00010_00001_cd == "A") %>%
    mutate(var = "wtr_temp") %>%
#   select desired columns
    select(dateTime, site_no, agency_cd, var, X_00010_00001) %>%
#   rename columns
    rename(
      date = dateTime,
      site_code = site_no,
      data_source = agency_cd,
      val = X_00010_00001
    ) %>%
  mutate(
    site_code = stringr::str_pad(string = site_code, width = 8, side = 'left', pad = 0)
  )

# Q
stream_disch <- raw_stream_disch %>%
#   remove provisional data
    filter(X_00060_00003_cd == "A") %>%
    mutate(var = "disch") %>%
#   select desired columns
    select(dateTime, site_no, agency_cd, var, X_00060_00003) %>%
#   rename columns
    rename(
      date = dateTime,
      site_code = site_no,
      data_source = agency_cd,
      val = X_00060_00003
    ) %>%
    mutate(
      site_code = stringr::str_pad(string = site_code, width = 8, side = 'left', pad = 0)
    )

# combine
stream_long <- rbind(stream_temp, stream_disch)
temp_long <- rbind(stream_data, daymet_data)

# annual
# stream data
annual_stream_data <- stream_data %>%
  tidyr::drop_na() %>%
  tidyr::pivot_wider(
           names_from = var,
           values_from = val,
           ) %>%
  mutate(year = format(as.Date(date), "%Y")) %>%
  group_by(year, site_code, data_source) %>%
  summarise(
    disch_mean = mean(disch_temp),
    disch_min = min(disch_temp),
    disch_max = max(disch_temp)
    wtr_mean = mean(wtr_temp),
    wtr_min = min(wtr_temp),
    wtr_max = max(wtr_temp)
  )

#   mean
#   max
#   min

# air temperatures
annual_daymet_data <- raw_daymet_data %>%
  tidyr::pivot_wider(
           names_from = var,
           values_from = val,
           ) %>%
  rename(
    daylight = dayl..s.,
    ppt = prcp..mm.day.,
    shortwave = srad..W.m.2.,
    swe = swe..kg.m.2.,
    tmax = tmax..deg.c.,
    tmin = tmin..deg.c.,
    vapor = vp..Pa.
  ) %>%
  mutate(year = format(as.Date(date), "%Y")) %>%
  select(-date) %>%
  group_by(site_code, year) %>%
  summarise(
    across(daylight:vapor, mean, .names = "mean_{.col}"),
    across(daylight:vapor, min, .names = "min_{.col}"),
    across(daylight:vapor, max, .names = "max_{.col}")
  )

#   max
#   min

# combine
all_data_wide <- alldata %>%
  tidyr::pivot_wider(
           names_from = var,
           values_from = val,
           ) %>%
  rename(
    daylight = dayl..s.,
    ppt = prcp..mm.day.,
    shortwave = srad..W.m.2.,
    swe = swe..kg.m.2.,
    tmax = tmax..deg.c.,
    tmin = tmin..deg.c.,
    vapor = vp..Pa.
  )

annual_data <- all_data_wide %>%
  mutate(year = format(as.Date(date), "%Y")) %>%
  select(-date) %>%
  group_by(site_code, year) %>%
  summarise(
    across(wtr_temp:vapor, mean, .names = "mean_{.col}"),
    across(wtr_temp:vapor, min, .names = "min_{.col}"),
    across(wtr_temp:vapor, max, .names = "max_{.col}")
  )
test <- annual_data %>%
  tidyr::drop_na()

## combine annuals
annual_df <- merge(
  annual_stream_data, daymet_data, by = c('site_code', 'year')) %>%
  tidyr::drop_na()

library(ggplot2)
annual.f <- annual_df %>%
  filter(wtr_min < 50)

p <- ggplot(annual.f, aes(x=year, y=wtr_min, group=site_code)) +
  geom_line(aes(color = site_code)) +
  xlab("Year") +
  ggtitle("Annual Min Water Temperatures Across USGS Sites") +
  theme_minimal() +
  theme(legend.position="none") +
  theme(text = element_text(size = 34))


p <- ggplot(annual.f, aes(x=year, y=mean_tmin, group=site_code)) +
  geom_line(aes(color = site_code)) +
  xlab("Year") +
  ggtitle("Annual Min Air Temperatures Across USGS Sites") +
  theme_minimal() +
  theme(legend.position="none") +
  theme(text = element_text(size = 34))

## Analysis
