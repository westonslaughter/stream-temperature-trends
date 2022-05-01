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
library(plotly)
source("./src/helpers.R")

# keep only records > certain period of years
low_n <- annual.plot %>%
  group_by(site_code) %>%
  summarise(
    n = n()
  ) %>%
  filter(n > 40)

annual.plot <- annual.df.z %>%
  filter(year < 2022) %>%
  filter(site_code %in% low_n$site_code)

# Determine p-values of regression
p.vals = sapply(unique(annual.plot$site_code), function(i) {
  coef(summary(lm(year ~ z_min_wtr_temp, data=annual.plot[annual.plot$site_code==i, ])))[2,4]
})

ggplotly(
  ggplot(annual.plot, aes(x=year, y=z_min_wtr_temp, group=site_code)) +
  ## geom_point(aes(color = site_code)) +
  geom_smooth(
    data=annual.plot[annual.plot$site_code %in% names(p.vals)[p.vals < 0.05],],
    aes(year, z_min_wtr_temp, colour=site_code),
    method='lm',
    se = FALSE
  ) +
  xlab("Year") +
  ggtitle("Annual Min Water Temperatures Across USGS Sites") +
  theme_minimal() +
  theme(text = element_text(size = 30)))

ggplotly(
  ggplot(annual.plot, aes(x=year, y=z_min_wtr_temp, group=site_code)) +
  ## geom_point(aes(color = site_code)) +
  geom_smooth(
    data=annual.plot[annual.plot$site_code %in% names(p.vals)[p.vals > 0.05],],
    aes(year, z_min_wtr_temp, colour=site_code),
    method='lm',
    se = FALSE
  ) +
  xlab("Year") +
  ggtitle("Annual Min Water Temperatures Across USGS Sites") +
  theme_minimal() +
  theme(text = element_text(size = 30)))

ggplotly(
  ggplot(annual.plot, aes(x=year, y=z_mean_tmin..deg.c., group=site_code)) +
  geom_line(aes(color = site_code)) +
  xlab("Year") +
  ggtitle("Annual Min Air Temperatures Across USGS Sites") +
  theme_minimal() +
  theme(legend.position="none") +
  theme(text = element_text(size = 30)))

