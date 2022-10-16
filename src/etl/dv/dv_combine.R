## all USGS water sites in Daymet?
air_files <- list.files("./data/dv/raw/air/daymet/")
list.files("./data/dv/raw/wtr/") %in% list.files("./data/dv/raw/air/daymet/")

# read in USGS and MacroSheds and Other data
# water
usgs_wtr <- featherCompiler("./data/dv/raw/wtr/")
ms_wtr <- read_feather("./data/dv/raw/ms/wtr/ms.feather")
cmb <- read_feather("./data/dv/raw/cbm/cbm.feather")

# sites (water sites is limiting factor)
usgs_sites <- unique(usgs_wtr$site_code)
ms_sites <- unique(ms_wtr$site_code)
cmb_sites <- read_feather("./data/dv/sites/cmb_sites.feather") %>%
  pull(site_code)
sites <- c(usgs_sites, ms_sites, cmb_sites)

# air
air <- featherCompiler("./data/dv/raw/air/daymet/", site_filter = sites)

## Reformat all data, then make a combined air-water df
## Daymet
daymet_long <- air %>%
  group_by(site_no, date, var) %>%
  summarize(val = mean(val))

daymet <- daymet_long %>%
  pivot_wider(id_cols = c("site_no", "date"),
              names_from = var,
              values_from = val) %>%
  rename(
    site_code = site_no,
    daylength = dayl..s.,
    ppt = prcp..mm.day.,
    radiation = srad..W.m.2.,
    snow = swe..kg.m.2.,
    air.tmax = tmax..deg.c.,
    air.tmin = tmin..deg.c.,
    vapor = vp..Pa.,
    site_code = site_no
  ) %>%
  mutate(
    air.tmean = (air.tmax + air.tmin)/2
  )

# USGS
# NOTE: very important! USGS is all daily sum data, macrosheds mostly grab samps...
usgs <- usgs_wtr %>%
  filter(
    X_00010_00003_cd == "A"
  ) %>%
  mutate(
    date = date(datetime)
        ) %>%
  select(
    dataset,
    site_code,
    date,
    ## wtr.tmax = X_00010_00001,
    ## wtr.tmin = X_00010_00002,
    wtr.tmean = X_00010_00003,
    ## wtr.grab =  X_00010_00011
    ) %>%
  filter(!is.na(wtr.tmean))

# MacroSheds
ms <- ms_wtr %>%
  filter(!is.na(val),
         ms_status == 0,
         ms_interp == 0) %>%
  mutate(date = date(datetime)) %>%
  group_by(site_code, date) %>%
  # NOTE: this move makes some data daily means, and the rest mostly still single grab samps
  # then, we remove the distinction info from data. long term unacceptable
  summarize(val = mean(val)) %>%
  rename(
    wtr.tmean = val
  ) %>%
  mutate(dataset = "macrosheds", .before = site_code) %>%
  ungroup()

## Merge
temp <- rbind(usgs, ms) %>%
  left_join(daymet, by = c('site_code', 'date')) %>%
  filter(!is.na(wtr.tmean),
         !is.na(air.tmean))

write_feather(temp, "./data/dv/munged/ms_usgs_airwtr_approved.feather")
