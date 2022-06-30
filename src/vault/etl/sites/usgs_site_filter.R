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
library(feather)

# bring in usgs site info
usgs_info <- read_feather("data/munged/sites/info_compiled.feather")
usgs_sites <- read_feather("data/munged/sites/sites_compiled.feather")

## usgs_temps <- usgs_sites %>%
##   group_by(site_no) %>%
##   filter(parm_cd == '00010')

## usgs_q <- usgs_sites %>%
##   group_by(site_no) %>%
##   filter(parm_cd == '00060')

## cross_filter <- usgs_temps$site_no %in% unique(usgs_q$site_no)

## usgs_cross <- usgs_temps[cross_filter,]

## length(unique(usgs_cross$site_no))

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

## fwrite(usgs, "./data/munged/sites/sites_select_compiled.csv")

# filter by:
# 40 yrs
min_yrs <- 40 * 365
# >80% coverage
min_obs <- 0.8

# we have both TEMP and DISCH site records - must filter
## for sites w *both*  records passig muster
## usgs_temp <- usgs %>%
##   filter(parm_cd == "00010") %>%
##   mutate(
##     period = as.integer(period),
##     count_nu = as.integer(count_nu),
##     mean_n = count_nu/period
##   ) %>%
##   filter(
##     site_type == "ST",
##     period > min_yrs,
##     mean_n > min_obs
##   )

## usgs_q <- usgs %>%
##   filter(parm_cd == "00060") %>%
##   mutate(
##     period = as.integer(period),
##     count_nu = as.integer(count_nu),
##     mean_n = count_nu/period
##   ) %>%
##   filter(
##     site_type == "ST",
##     period > min_yrs,
##     mean_n > min_obs
##   )

usgs_f <- usgs_temp %>%
  filter(site_code %in% usgs_q$site_code)

# focal sites for long term daily value analysis
## fwrite(usgs_f, "./data/munged/sites/sites_dv_filtered_compiled.csv")

## # get uv sites
## usgs_uv <- usgs_f %>%
##   filter(data_type == "uv")
## fwrite(usgs_uv, "./data/sites/sites_filtered_uv.csv")

## # get dv sites
## usgs_dv <- usgs_f %>%
##   filter(data_type == "dv")
## fwrite(usgs, "./data/sites/sites_filtered_dv.csv")

## # site overlap
## uv_sites <- unique(usgs_uv$site_code)
## dv_sites <- unique(usgs_dv$site_code)

## cross_sites <- uv_sites[uv_sites %in% dv_sites]

## # focal sites
## usgs_cross <- usgs_f %>%
##   filter(site_code %in% cross_sites)

## fwrite(usgs_cross, "./data/munged/sites/focal_sites_compiled.csv")


# just temp dv sites!
usgs_temp <- usgs %>%
  filter(parm_cd == "00010",
         data_type == 'dv') %>%
  mutate(
    period = as.integer(period),
    count_nu = as.integer(count_nu),
    mean_n = count_nu/period,
  ) %>%
  filter(
    site_type == "ST",
    period > min_yrs,
    mean_n > min_obs,
    begin_date < as.Date('1980-01-02'),
    end_date > as.Date('2020-12-31')
  )

write.csv(usgs_temp, "./data/munged/sites/dv_temp_sites_compiled.csv")
