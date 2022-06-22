### ---------------------------
##
## Title: site_filter.R
##
## Purpose: this script will filter USGS sites by the requirements of our investigation
##
## Author: WS
##
## Date Created: 2022-06-21
##
## ---------------------------

# bring in usgs site info
usgs_info <- read.csv("data/sites/info_compiled.csv", colClasses = 'character')
usgs_sites <- read.csv("data/sites/sites_compiled.csv", colClasses = 'character')

# merge
usgs_i <- merge(usgs_info, usgs_sites, by = "site_no")

# filter to desired columns
usgs <- usgs_i %>%
  select(site_code = site_no,
         station = station_nm.x,
         lat = dec_lat_va.x,
         long = dec_long_va.x,
         datum = dec_coord_datum_cd.x,
         huc = huc_cd.x,
         ws_area = drain_area_va,
         contrib_ws_area = contrib_drain_area_va,
         tz = tz_cd,
         site_type = site_tp_cd.y,
         data_type = data_type_cd,
         parm_cd,
         begin_date,
         end_date,
         count_nu,
         period)

## fwrite(usgs, "./data/sites/sites_select_compiled.csv")

# filter by:
# 30 yrs
yr_days <- 30 * 365
# >80% coverage
yr80p <- 365 * 0.8

usgs_f <- usgs %>%
  mutate(
    period = as.integer(period),
    count_nu = as.integer(count_nu),
    mean_n = count_nu/period
  ) %>%
  filter(
    site_type == "ST",
    period > yr_days,
    mean_n > 0.8
  )

## fwrite(usgs_f, "./data/sites/sites_filtered_compiled.csv")

# get uv sites
usgs_uv <- usgs_f %>%
  filter(data_type == "uv")
fwrite(usgs_uv, "./data/sites/sites_filtered_uv.csv")

# get dv sites
usgs_dv <- usgs_f %>%
  filter(data_type == "dv")
fwrite(usgs, "./data/sites/sites_filtered_dv.csv")
