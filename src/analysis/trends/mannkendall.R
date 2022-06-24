# Seasonal Mann-Kendall and Mann-Kendall
library(feather)
library(Kendall)

# bring in air-wtr data
air.wtr <- read_feather('data/munged/dv/combined/air_wtr.feather')

# Seasonal Mann Kendall
# test on site one
site_one <- air.wtr[air.wtr$site_code == "01010000",]
wtr.vec <- site_one$wtr.tmean
start <- min(site_one$date)
end <- max(site_one$date)

site.ts <- ts(wtr.vec, start = start, end = end)

site_one.smk <- SeasonalMannKendall(site.ts)

# loop thru all sites
wtr.smk <- data.frame(matrix(ncol = 8, nrow = 1))
colnames(wtr.smk) <- c("site_code", "start", "end", "tau", "S", "sl", "D", "varS")

sites <- unique(air.wtr$site_code)
for(site in (sites)) {

  site.data <- air.wtr[air.wtr$site_code == site,]
  wtr.vec <- site.data$wtr.tmean
  start <- min(site.data$date)
  end <- max(site.data$date)

  site.ts <- ts(wtr.vec, start = start, end = end)

  site.smk <- SeasonalMannKendall(site.ts)
  site.smk.row <- c(site, start, end, site.smk$tau[1], site.smk$S[1], site.smk$sl[1], site.smk$D[1], site.smk$varS)
  wtr.smk <- rbind(wtr.smk, site.smk.row)
}

wtr.smk <- wtr.smk[-1,]
write.csv(wtr.smk, "data/derived/stats/mannkendall/smk_results.csv")
