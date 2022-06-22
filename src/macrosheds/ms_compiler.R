# Script Compiling MacroSheds data
library(macrosheds)
library(dplyr)
library(tidyr)
library(lubridate)

# inspect in MS sites for those w long term discharge and wtr temp
ms_site_info <- ms_download_site_data()
ms_catalog <- ms_catalog()

# filter catalog also for period > 30 yr and > 80% days observed day
ms_targets <- ms_catalog %>%
  filter(variable_name %in% c("Discharge", "Water Temperature")) %>%
  # length of period of records in years
  mutate(period = as.numeric(last_record_utc - first_record_utc)/86400/365) %>%
  # more than 30 years
  filter(period > 30) %>%
  # more than 80% coverage
  mutate(annual_obs = observations/period) %>%
  filter(annual_obs > 292)

# sites with both Q and Temp
ms_targets_sum <- ms_targets %>%
  group_by(site_code) %>%
  summarise(n_vars = length(unique(variable_name))) %>%
  filter(n_vars == 2)

# filter by summary
qt_sites <- ms_targets %>%
  filter(site_code %in% ms_targets_sum$site_code)

# load in data
ms_dir <- "../../macrosheds/data/ms/"

q_data <- ms_load_product(
  ms_dir,
  prodname = "discharge",
  site_codes = qt_sites$site_code,
  warn = FALSE
)

# temperature
t_data <- ms_load_product(
  ms_dir,
  prodname = "stream_chemistry",
  filter_vars = "temp",
  site_codes = unique(qt_sites$site_code),
  warn = FALSE
)

# merge
qt_data <- rbind(q_data, t_data)

qt_df <- qt_data %>%
  filter(ms_status == 1) %>%
  pivot_wider(id_cols = c('datetime', 'site_code'),
              names_from = var,
              values_from = val) %>%
  rename(qls = IS_discharge) %>%
  mutate(date = date(as.Date(datetime))) %>%
  group_by(site_code, date) %>%
  summarise(d_q = mean(qls),
            d_gn_t = mean(GN_temp),
            d_is_t = mean(IS_temp))

# extremely sad amount of q-temp data?
qt <- qt_df %>%
  filter(!is.na(d_q),
         !is.na(d_is_t))
