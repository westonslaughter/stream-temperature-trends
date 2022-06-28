library(dplyr)
library(data.table)
library(feather)
library(stringi)


# bring in focal site info
# larger focal group
## sites <- fread("data/munged/sites/focal_sites_compiled.csv", colClasses = 'character')
## small, long term temp and q dv focal group (no crossover with uv temp)
sites <- fread("data/munged/sites/sites_dv_filtered_compiled.csv", colClasses = 'character')
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
write_feather(focal_sites_wtr, "data/munged/dv/wtr/dv_longterm_usgs_compiled.feather")
write_feather(focal_sites_daymet, "data/munged/dv/air/dv_longterm_daymet_compiled.feather")


# check out
## read_feather(focal_sites_daymet, "data/munged/dv/air/focal_daymet_compiled.feather")
## wtr <- read_feather("data/munged/dv/wtr/focal_usgs_compiled.feather")

## wtr.sy <- wtr %>%
##   mutate(
##   year = format(as.Date(dateTime), "%Y")
##   ) %>%
##   group_by(site_no) %>%
##   summarise(
##     n = n()
##   )


# one site with reporting discrepancies
## test_site <- sites[sites$site_code == '01379000',]
## test_data <- wtr[wtr$site_no == '01379000',]

## test_min <- test_site[test_site$data_type == 'dv',]$begin_date
## test_max <- test_site[test_site$data_type == 'dv',]$end_date

## test_info <- readNWISdata(
##               sites = '01379000',
##               parameterCd = '00010',
##               service = 'dv',
##               startDate = test_min,
##               endDate = test_max)
