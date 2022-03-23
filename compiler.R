library(zoo)
library(tidyverse)
library(stringr)
library(plyr)
library(data.table)

# load sites and list target variables
site_files <- list.files(path = "./data/temp/", pattern = ".rds", full.names = TRUE)

vars <- c("wtr", "disch")

# loop through variables, writing compiled csv
for(var in vars){
  print(paste("PULLING:", var))

  fp <- site_files[grep(var, site_files)]
  names(fp) <- paste(fp)

  data <- ldply(fp, readRDS)
  data$site <- str_match(data$.id, "//(nwis_[0-9]+)-")[,2]
  data <- subset(data, select=-c(.id))

  csv_fp <- paste0("./data/sites/", var, ".csv")
  fwrite(data, csv_fp)

  print(paste("SUCCESS:", var))
  print(paste("file saved to", csv_fp))
}
