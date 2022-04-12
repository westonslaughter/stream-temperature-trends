library(tidyverse)
library(stringr)
library(multidplyr)
library(data.table)

# multiprocessing
cluster <- new_cluster(8)

# import water temp and Q data
df_disch <- fread("./data/vars/dv/compiled/disch.csv")
df_wtr <- fread("./data/vars/dv/compiled/wtr.csv")

# clean disch df columns and names
df_disch <- df_disch %>%
  select(c("site_no", "dateTime", "X_00060_00003", "X_00060_00003_cd")) %>%
  filter(X_00060_00003_cd == "A") %>%
  select(-X_00060_00003_cd)
colnames(df_disch) <- c("site", "datetime", "disch")

# clear out all provisional data"X_00010_00001_cd"
df_wtr <- df_wtr %>%
  filter(V4 == "A") %>%
  select(-V4)

df_wtr <- as.data.table(df_wtr)
df_disch <- as.data.table(df_disch)
df_wtr <- na.omit(df_wtr)
df_disch <- na.omit(df_disch)

wq <- merge(df_disch, df_wtr, by = c("site", "datetime"))

wq <- wq %>%
  filter(site > 1)

# filters to make sure the "years" are all at least 75% complete

df <- wq %>%
  mutate(year = format(as.Date(datetime), "%Y")) %>%
  mutate(day = format(as.Date(datetime), "%j")) %>%
  mutate(site_year = paste(site, year)) %>%
  group_by(site_year) %>%
  summarise(days_n = n_distinct(day)) %>%
  filter(days_n > 273) %>%
  partition(cluster) %>%
  collect()

wq_filter <- wq %>%
  mutate(year = format(as.Date(datetime), "%Y")) %>%
  mutate(site_year = paste(site, year)) %>%
  filter(site_year %in% df$site_year) %>%
  partition(cluster) %>%
  collect()

write.csv(wq_filter, "data/wq_A.csv")


# combine with site info
info <- fread("./data/sites/info/CA.csv")
wq <- fread("./data/wq/wq.csv")

# season stuff later
# label samples per seasons,
## getSeason <- function(DATES) {
##     WS <- as.Date("2012-12-15", format = "%Y-%m-%d") # Winter Solstice
##     SE <- as.Date("2012-3-15",  format = "%Y-%m-%d") # Spring Equinox
##     SS <- as.Date("2012-6-15",  format = "%Y-%m-%d") # Summer Solstice
##     FE <- as.Date("2012-9-15",  format = "%Y-%m-%d") # Fall Equinox

##     # Convert dates from any year to 2012 dates
##     d <- as.Date(strftime(DATES, format="2012-%m-%d"))

##     ifelse (d >= WS | d < SE, "Winter",
##       ifelse (d >= SE & d < SS, "Spring",
##         ifelse (d >= SS & d < FE, "Summer", "Fall")))
## }

# eplore
library(ggplot2)

wq_gg <- wq %>%
  filter(wtr < 100)  %>%
  mutate(year = format(as.Date(datetime), "%Y")) %>%
  group_by(site, year) %>%
  summarise(
    annual_temp_mean = mean(wtr),
    annual_temp_min = min(wtr),
    annual_temp_max = max(wtr)
  )
begin <- min(wq_gg$year)
end <- max(wq_gg$year)
wq_gg$year <- as.numeric(wq_gg$year)
wq_gg$site <- as.character(wq_gg$site)

p <- ggplot(wq_gg, aes(x=year, y=annual_temp_min, group=site)) +
  geom_line(aes(color = site)) +
  ## xlab("Year") +
  ggtitle("Annual Min Temperatures Across USGS Sites \n 1908 to 2022") +
  theme_few() +
  theme(legend.position="none") +
  theme(text = element_text(size = 34))
  ## scale_x_discrete(limits = c(begin, end))
  ## scale_x_continuous(breaks=seq(0,40,5))
