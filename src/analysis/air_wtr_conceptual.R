# script for conceptual figure conveying the idea behind this study:
# that air and water temperature in rivers is a sigmoid in its ideal form,
# and that real river data will reflect linear, exp. log, and sigmoidal relations
# that could be associated with drivers
                                        #
library(dplyr)
library(data.table)
library(feather)
library(ggplot2)
library(viridis)
library(ggpubr)
library(ggpmisc)
source("src/helpers.R")

# load in data
dv <- read_feather("data/dv/munged/ms_usgs_cmb_ssi_snp_airwtr.feather") %>%
  mutate(
    year = format(as.Date(date), "%Y"),
    month = factor(format(as.Date(date), "%m")),
    decade = as.character(floor_decade(as.numeric(year))),
    half_decade = as.character(floor_timestep(as.numeric(year), 5)),
    season = factor(quarters(as.Date(date)), levels = c("Q1", "Q2", "Q3", "Q4"),
                    labels = c("winter", "spring", "summer", "fall")),
    wtr.tmean = as.numeric(wtr.tmean),
    air.tmean = as.numeric(air.tmean)
  ) %>%
  filter(year < 2022)

# make a small summary of the start and end years for each site
# we will use this to isolate only begin/end of site visually
site_start_end_years <- dv %>%
  group_by(site_code) %>%
  summarize(
    # diff year metrics
    n_yrs = length(unique(year)),
    begin_year = min(year),
    end_year = max(year)
  )
# carve up dv to just edge year
edge_n <- 5
dv_edge <- dv %>%
  group_by(site_code) %>%
  left_join(site_start_end_years, by = "site_code") %>%
  filter(
    as.numeric(year) < as.numeric(begin_year)+edge_n | as.numeric(year) > as.numeric(end_year)-edge_n
  )



scatter_concept <- function(data, x, y, attr,  discrete = TRUE, stat = "Mean") {
  title <- paste0("Daily ", stat, " Water Temp vs Daily ", stat," Air Temp, USGS 1980-2022")
  x_txt <- paste0('Daily ', stat, ' Air Temperature')
  y_txt <- paste0('Daily ', stat, ' Water Temperature')

  data <- data %>%
    ## arrange(desc(year))
    arrange(year)

  plt <- ggplot(data,
         aes_string(x = x,
                    y = y
                    )) +
  geom_hline(yintercept=0, color='darkgrey', linetype='dashed') +
  geom_vline(xintercept=0, color='darkgrey', linetype='dashed') +
  geom_point(
    aes_string(color = attr)
  ) +
  geom_abline(slope=1,    color='#D22B2B', linetype='dashed') +
  scale_color_viridis(discrete = discrete, option = "magma", alpha = 0.4) +
  ylim(-15, 40) +
  ## xlab(x_txt) +
  ## ylab(y_txt) +
  theme_minimal() +
  theme(text = element_text(size = 30))
  ## theme(legend.position="none")

  return(plt)
}


# AIR:WTR All Sites
scatter_concept(dv, 'air.tmean', 'wtr.tmean', 'decade', stat = "Mean", discrete = TRUE) +
  facet_wrap(~ site_code) +
  xlab('\n Air Temperature (C)') +
  ylab(' Water Temperature (C)\n') +
  ggtitle('Water to Air Temperature Ratio at USGS Gauges\n1980-2021')

# AIR:WTR Chesapeake Monitoring
cmb_dv <- dv %>%
  filter(!dataset %in% c("USGS", "macrosheds"))
scatter_concept(cmb_dv, 'air.tmean', 'wtr.tmean', 'year', stat = "Mean", discrete = TRUE) +
  facet_wrap(~ site_code) +
  xlab('\n Air Temperature (C)') +
  ylab(' Water Temperature (C)\n') +
  ggtitle('Water to Air Temperature Ratio at USGS Gauges\n1980-2021')

# AIR:WTR SSI Data
ssi_dv <- dv %>%
  filter(dataset %in% c("ssi"))
scatter_concept(ssi_dv, 'air.tmean', 'wtr.tmean', 'year', stat = "Mean", discrete = TRUE) +
  facet_wrap(~ site_code) +
  xlab('\n Air Temperature (C)') +
  ylab(' Water Temperature (C)\n') +
  ggtitle('Water to Air Temperature Ratio at USGS Gauges\n1980-2021')

# AIR:WTR SNP Data
snp_dv <- dv %>%
  filter(dataset %in% c("snp")) %>%
  mutate(
    site_code = "snp"
  )
# Groundwater story!
scatter_concept(snp_dv, 'air.tmean', 'wtr.tmean', 'year', stat = "Mean", discrete = TRUE) +
  facet_wrap(~ decade) +
  xlab('\n Air Temperature (C)') +
  ylab(' Water Temperature (C)\n') +
  ggtitle('Water to Air Temperature Ratio at USGS Gauges\n1980-2021')

## Quashnet? https://www.sciencebase.gov/catalog/item/5ae37225e4b0e2c2dd3207e5
## Shendoah https://swas.evsc.virginia.edu/POST/scripts/overview.php

## Case Studies, Narrative Exploration
## east delaware
## exponential > linear
east_delaware_dv <- dv %>%
  filter(site_code %in% c("01417500"))
scatter_concept(east_delaware_dv, 'air.tmean', 'wtr.tmean', 'month', stat = "Mean", discrete = TRUE) +
  facet_wrap(~ decade)

## delaware strong sigmoid in 1999-2000 strong linear in 2020-2021
west_delaware_dv <- dv %>%
  ## filter(site_code %in% c("01426500", '01425000'))
  filter(site_code %in% c("01426500"))
scatter_concept(west_delaware_dv, 'air.tmean', 'wtr.tmean', 'month', stat = "Mean", discrete = TRUE) +
  facet_wrap(~ year)

## Konza fire history changing thermal regime? Seems to ocmplicatedf for concept, pass for now
konza_dv <- dv %>%
  filter(site_code %in% c("N01B"))
scatter_concept(konza_dv, 'air.tmean', 'wtr.tmean', 'month', stat = "Mean", discrete = TRUE) +
  facet_wrap(~ year)

## Baltimore, urban lnear
bltmr_dv <- dv %>%
  filter(site_code %in% c("GFCP"))
scatter_concept(bltmr_dv, 'air.tmean', 'wtr.tmean', 'month', stat = "Mean", discrete = TRUE) +
  facet_wrap(~ half_decade)

## Bigelow, linear > sigmoig??? (other CA data?)``
bigelow_dv <- dv %>%
  filter(site_code %in% c("Bigelow"))
scatter_concept(bigelow_dv, 'air.tmean', 'wtr.tmean', 'month', stat = "Mean", discrete = TRUE) +
  facet_wrap(~ year)

## Flathead River Montana
flathead_dv <- dv %>%
  filter(site_code %in% c("12363000"))
scatter_concept(flathead_dv, 'air.tmean', 'wtr.tmean', 'month', stat = "Mean", discrete = TRUE) +
  facet_wrap(~ decade)

## Chatahooga exponential > Linear?
chat_dv <- dv %>%
  filter(site_code %in% c("02397530"))
scatter_concept(chat_dv, 'air.tmean', 'wtr.tmean', 'month', stat = "Mean", discrete = TRUE) +
  facet_wrap(~ year)

## ??
usgs_dv <- dv %>%
  filter(site_code %in% c("14187200"))
scatter_concept(usgs_dv, 'air.tmean', 'wtr.tmean', 'month', stat = "Mean", discrete = TRUE) +
  facet_wrap(~ decade)

## Cart Creek
cart_dv <- dv %>%
  filter(site_code %in% c("cart_creek"))
scatter_concept(cart_dv, 'air.tmean', 'wtr.tmean', 'month', stat = "Mean", discrete = TRUE) +
  facet_wrap(~ decade)

## saw mill brook
saw_dv <- dv %>%
  filter(site_code %in% c("saw_mill_brook"))
scatter_concept(saw_dv, 'air.tmean', 'wtr.tmean', 'month', stat = "Mean", discrete = TRUE) +
  facet_wrap(~ decade)

blackearth_dv <- dv %>%
  filter(site_code %in% c("black_earth_creek"))
scatter_concept(blackearth_dv, 'air.tmean', 'wtr.tmean', 'month', stat = "Mean", discrete = TRUE) +
  facet_wrap(~ year)

hbef_dv <- dv %>%
  filter(site_code %in% c("w6"))
scatter_concept(hbef_dv, 'air.tmean', 'wtr.tmean', 'month', stat = "Mean", discrete = TRUE) +
  facet_wrap(~ decade)

oracle_dv <- dv %>%
  filter(site_code %in% c("OracleRidge"))
scatter_concept(oracle_dv, 'air.tmean', 'wtr.tmean', 'month', stat = "Mean", discrete = TRUE) +
  facet_wrap(~ year)

## ??
flathead_falls_dv <- dv %>%
  filter(site_code %in% c("12363000"))
scatter_concept(flathead_falls_dv, 'air.tmean', 'wtr.tmean', 'month', stat = "Mean", discrete = TRUE) +
  facet_wrap(~ half_decade)


## seems lightly expionential > linear
usgs_dv <- dv %>%
  filter(site_code %in% c("02337170"))
scatter_concept(usgs_dv, 'air.tmean', 'wtr.tmean', 'month', stat = "Mean", discrete = TRUE) +
  facet_wrap(~ decade)

usgs_dv <- dv %>%
  filter(site_code %in% c("06041000"))
scatter_concept(usgs_dv, 'air.tmean', 'wtr.tmean', 'month', stat = "Mean", discrete = TRUE) +
  facet_wrap(~ decade)

# flat boi (Touloumne California, Below Dam)
usgs_dv <- dv %>%
  filter(site_code %in% c("11289650"))
scatter_concept(usgs_dv, 'air.tmean', 'wtr.tmean', 'month', stat = "Mean", discrete = TRUE) +
  facet_wrap(~ half_decade)

usgs_dv <- dv %>%
  filter(site_code %in% c("14139800"))
scatter_concept(usgs_dv, 'air.tmean', 'wtr.tmean', 'month', stat = "Mean", discrete = TRUE) +
  facet_wrap(~ decade)

# sigmoid/linear > log ... Applegate River, OR
apple_dv <- dv %>%
  filter(site_code %in% c("14366000"))
scatter_concept(apple_dv, 'air.tmean', 'wtr.tmean', 'month', stat = "Mean", discrete = TRUE) +
  facet_wrap(~ year)

# Kuparukl River, seems like Blob > Lin roughly
usgs_dv <- dv %>%
  filter(site_code %in% c("Kuparuk_River_0.56"))
scatter_concept(usgs_dv, 'air.tmean', 'wtr.tmean', 'month', stat = "Mean", discrete = TRUE) +
  facet_wrap(~ decade)

hj_dv <- dv %>%
  filter(site_code %in% c("GSMACK"))
scatter_concept(hj_dv, 'air.tmean', 'wtr.tmean', 'month', stat = "Mean", discrete = TRUE) +
  facet_wrap(~ year)

# NOTE: seems some sites (some USGS some MS) have below zero. Will not tolerate anything more than 1C belo 0.
dv <- dv %>%
  filter(wtr.tmean > -1)

# custom colors for sites
site_colors <- dv %>%
  mutate(
    color = case_when(site_code == '01426500' ~ 'red', TRUE ~ '00000000')
  ) %>%
  pull(color)

# actual figure func and drafting
# Delaware Case Study
west_delaware_dv_1999 <- west_delaware_dv %>%
  filter(
    site_code == "01426500",
    year %in% c("1980", "1981")
  )

west_delaware_dv_2020 <- west_delaware_dv %>%
  filter(
    site_code == "01426500",
    year %in% c("2020", "2019")
         )

apple_dv_1999 <- apple_dv %>%
  filter(
    site_code == "14366000",
    year %in% c("1985", "1986", "1987")
  )

apple_dv_2020 <- apple_dv %>%
  filter(
    site_code == "14366000",
    year %in% c("2019", "2020", "2021")
         )

scatter_figure <- function(data, x, y, attr,  discrete = TRUE, stat = "Mean") {
  title <- paste0("Daily ", stat, " Water Temp vs Daily ", stat," Air Temp\nUSGS and MacroSheds River Monitoring Sites")
  x_txt <- paste0('Daily ', stat, ' Air Temperature (C)')
  y_txt <- paste0('Daily ', stat, ' Water Temperature (C)')

  data <- data %>%
    ## arrange(desc(year))
    arrange(year)

  plt <- ggplot(data,
         ) +
  geom_hline(yintercept=0, color='darkgrey', linetype='dashed') +
  geom_vline(xintercept=0, color='darkgrey', linetype='dashed') +
  geom_point(
    aes_string(x=x, y=y),
    color = 'darkgrey',
    alpha = 0.005,
        size = 0.5
  ) +
    # Delaware
    ## geom_point(
    ##   data = west_delaware_dv_1999,
    ##   color = "blue",
    ##   aes_string(
    ##       x=x,
    ##       y=y,
    ##     alpha = 0.7
    ##     )) +
    ## geom_point(
    ##   data = west_delaware_dv_2020,
    ##   color = "red",
    ##   aes_string(
    ##       x=x,
    ##       y=y,
    ##       alpha = 0.7)) +

    # Applegate, OR
    geom_point(
      data = apple_dv_1999,
      color = "blue",
      aes_string(
          x=x,
          y=y,
        alpha = 0.7
        )) +
    geom_point(
      data = apple_dv_2020,
      color = "red",
      aes_string(
          x=x,
          y=y,
          alpha = 0.7)) +
  geom_abline(slope=1,    color='#D22B2B', linetype='dashed') +
  ylim(-5, 40) +
  xlab(x_txt) +
  ylab(y_txt) +
  log_x_
  theme_minimal() +
  theme(text = element_text(size = 30))
  ## theme(legend.position="none")

  return(plt)
}


scatter_figure(dv, 'air.tmean', 'wtr.tmean', 'month', stat = "Mean", discrete = TRUE)


# Quarterd Figure Comparing Site Year  Year
# Chatahoochie
chat_dv_1999 <- chat_dv %>%
  filter(
    year %in% c("1980", "1981")
  )

chat_dv_2020 <- chat_dv %>%
  filter(
    year %in% c("2020", "2019")
         )


## Chatahooga exponential > Linear?
chat_dv <- dv %>%
  filter(site_code %in% c("02397530"))
scatter_concept(chat_dv, 'air.tmean', 'wtr.tmean', 'month', stat = "Mean", discrete = TRUE) +
  facet_wrap(~ year)
# sigmoid/linear > log ... Applegate River, OR
apple_dv <- dv %>%
  filter(site_code %in% c("14366000"))
scatter_concept(apple_dv, 'air.tmean', 'wtr.tmean', 'month', stat = "Mean", discrete = TRUE) +
  facet_wrap(~ year)

# make new dv filtered to case study sites with color ctegories for this case study:
case_dv <- dv %>%
  mutate(
    site_code = case_when(
      dataset == "snp" ~ "snp",
      TRUE ~ as.character(site_code)
    )) %>%
  filter(
    # Applegate, OR
    site_code == "14366000" |
    # Chatahooga, GA/AL
    site_code == "02397530" |
    # Shenendoah, VA |
    site_code == "snp"
  ) %>%
  mutate(
#   Chatahooga and Applegate "before" is BLUE and "after" is RED
    timing = case_when(
      site_code == "14366000" & year %in% c("1986") ~ "historical", # before (applegate)
      site_code == "02397530" & year %in% c("1985") ~ "historical", # before (chatahooga)
      year %in% c("2021") ~ "modern", # after
      site_code == "snp" & grepl("199", year) ~ "historical", # before (SNP)
      site_code == "snp" & grepl("201|202", year) ~ "modern", # before (SNP)
      TRUE ~ "gray"),
#   all data points are labelled as case: "chatahooga", "applegate"
    case = case_when(
      site_code == "14366000" ~ "Applegate, OR",
      site_code == "02397530" ~ "Chatahooga, GA/AL",
      site_code == "snp" ~ "Shenendoah NP, VA",
      TRUE ~ "gray"
    )
  ) %>%
  filter(
    case != "gray",
    timing != "gray"
  )

# arrange w models draft
scatter_grid <- function(data, x, y, attr,  discrete = TRUE, stat = "Mean", model = "") {
  ## title <- paste0("Daily ", stat, " Water Temp vs Daily ", stat," Air Temp\nUSGS and MacroSheds River Monitoring Sites")

  data <- data %>%
    ## arrange(desc(year))
    arrange(year)

  ## air_min <- min(data$air.tmean)


  plt <- ggplot(data) +
  geom_hline(yintercept=0, color='darkgrey', linetype='solid') +
  geom_vline(xintercept=0, color='darkgrey', linetype='solid') +
  ## geom_vline(xintercept=air_min, color='purple', linetype='dashed') +
  geom_point(
    aes_string(x=x, y=y, color = attr),
    alpha = 0.75,
    size = 0.75
  ) +
    ## {if(model == "logistic") stat_smooth(aes_string(x=x,y=y), formula=y~tan(x), se=FALSE, method.args = list(family=binomial))} +
  scale_color_manual(breaks = c("historical", "modern"), values = c("blue", "red")) +
  geom_abline(slope=1, color='#D22B2B', linetype='dashed') +
  ylim(-5, 40) +
  xlim(-10, 35) +
  theme_minimal() +
    theme(
      text = element_text(size = 20),
      legend.position= "none"
      ) +
    xlab('') +
    ylab('')
  return(plt)
}

library(ggpubr)
library(grid)
library(gridExtra)

chat_before_dv <- case_dv %>% filter(site_code == "02397530" & timing == "historical")
chat_after_dv <- case_dv %>% filter(site_code == "02397530" & timing == "modern")

apple_before_dv <- case_dv %>% filter(site_code == "14366000" & timing == "historical")
apple_after_dv <- case_dv %>% filter(site_code ==  "14366000" & timing == "modern")

snp_before_dv <- case_dv %>% filter(site_code == "snp" & timing == "historical")
snp_after_dv <- case_dv %>% filter(site_code ==  "snp" & timing == "modern")

# individual plots
cb <- scatter_grid(chat_before_dv,  'air.tmean', 'wtr.tmean', 'timing', stat = "Mean", discrete = TRUE) +
  ggtitle("1985")
ca <- scatter_grid(chat_after_dv,   'air.tmean', 'wtr.tmean', 'timing', stat = "Mean", discrete = TRUE) +
  ggtitle("2021")

ab <- scatter_grid(apple_before_dv, 'air.tmean', 'wtr.tmean', 'timing', stat = "Mean", discrete = TRUE) +
  ggtitle("1985")
aa <- scatter_grid(apple_after_dv, 'air.tmean', 'wtr.tmean', 'timing', stat = "Mean", discrete = TRUE) +
  ggtitle("2021")

sb <- scatter_grid(snp_before_dv, 'air.tmean', 'wtr.tmean', 'timing', stat = "Mean", discrete = TRUE) +
  ggtitle("1990's")
sa <- scatter_grid(snp_after_dv, 'air.tmean', 'wtr.tmean', 'timing', stat = "Mean", discrete = TRUE) +
  ggtitle("2010s")

blank <- c()
# rows
crow <- grid.arrange(cb, blank, ca, nrow =1, ncol =3, top = textGrob("Chatahooga, GA/AL", gp=gpar(fontsize=26,font=3))  , widths = c(1,0.25,1))
brow <- grid.arrange(blank, blank, blank, nrow =1, ncol =3, widths = c(1,0.25,1), heights = c(0.25))
arow <- grid.arrange(ab, blank, aa, nrow =1, ncol =3, top = textGrob("Applegate River, OR", gp=gpar(fontsize=26,font=3)), widths = c(1,0.25,1))
brow <- grid.arrange(blank, blank, blank, nrow =1, ncol =3, widths = c(1,0.25,1), heights = c(0.25))
srow <- grid.arrange(sb, blank, sa, nrow =1, ncol =3, top = textGrob("Shenendoah NP, VA", gp=gpar(fontsize=26,font=3))  , widths = c(1,0.25,1))
brow <- grid.arrange(blank, blank, blank, nrow =1, ncol =3, widths = c(1,0.25,1), heights = c(0.25))


grid.arrange(
  top =    textGrob("\nDaily Water vs Air Temperature\n", gp=gpar(fontsize=30,font=3)),
  left =   textGrob("\nWater Temperature (C)", rot = 90,  gp=gpar(fontsize=26,font=3)),
  right =   textGrob("\n", rot = 90,                      gp=gpar(fontsize=26,font=3)),
  bottom = textGrob("Air Temperature (C)\n",              gp=gpar(fontsize=26,font=3)),
  arrangeGrob(blank, nrow = 1, ncol =1),
  # chatahoga
  crow,
  brow,
  # applegate
  arow,
  brow,
  # shenenodah aggregate
  srow,
  brow,
             nrow = 7, ncol = 1,
             heights = c(0.1,2,0.5,2,0.5,2,0.5))

# FACETING SOLUTION DRAFT:
scatter_case <- function(data, x, y, attr,  discrete = TRUE, stat = "Mean") {
  title <- paste0("Daily ", stat, " Water Temp vs Daily ", stat," Air Temp\nUSGS and MacroSheds River Monitoring Sites")
  x_txt <- paste0('Daily ', stat, ' Air Temperature (C)')
  y_txt <- paste0('Daily ', stat, ' Water Temperature (C)')

  data <- data %>%
    ## arrange(desc(year))
    arrange(year)

  plt <- ggplot(data,
         ) +
  geom_hline(yintercept=0, color='darkgrey', linetype='dashed') +
  geom_vline(xintercept=0, color='darkgrey', linetype='dashed') +
  geom_point(
    aes_string(x=x, y=y, color = attr),
    alpha = 0.75,
    size = 0.75
  ) +
  scale_color_manual( breaks = c("historical", "modern"), values = c("blue", "red")) +
  geom_abline(slope=1,    color='#D22B2B', linetype='dashed') +
  ylim(-5, 40) +
  xlab(x_txt) +
  ylab(y_txt) +
  theme_minimal() +
  theme(text = element_text(size = 30))
  ## theme(legend.position="none") +

  return(plt)
}

scatter_case(case_dv, 'air.tmean', 'wtr.tmean', 'timing', stat = "Mean", discrete = TRUE) +
  facet_wrap(~case+timing)
