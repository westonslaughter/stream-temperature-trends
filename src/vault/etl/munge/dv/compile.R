library(dplyr)
library(tidyr)
library(data.table)
library(feather)
library(stringi)

source('src/etl/etl_helpers.R')

# bring in focal site info
# larger focal group
## sites <- fread("data/munged/sites/focal_sites_compiled.csv", colClasses = 'character')
## small, long term temp and q dv focal group (no crossover with uv temp)
sites <- fread("data/munged/sites/dv_temp_sites_compiled.csv", colClasses = 'character')
site_list <- unique(sites$site_code)

# load in water data
focal_sites_wtr <- fileCompiler('data/raw/dv/wtr', site_filter = site_list)

# load in air data
focal_sites_daymet <- featherCompiler(fp = 'data/raw/dv/air/daymet/', site_filter = site_list)

# wide form, by var
focal_sites_daymet <- focal_sites_daymet %>%
  pivot_wider(id_cols = c("site_code", "date"),
              names_from = var,
              values_from = val)


# write to compiled
write_feather(focal_sites_wtr, "data/munged/dv/wtr/temp_only_usgs_compiled.feather")
write_feather(focal_sites_daymet, "data/munged/dv/air/temp_only__daymet_compiled.feather")
