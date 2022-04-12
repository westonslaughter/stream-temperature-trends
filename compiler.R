library(tidyverse)
library(stringr)
library(plyr)
library(data.table)

mode <- "dv"
vars <- c("wtr")

# loop through variables, writing compiled csv
for(var in vars){
  # load sites and list target variables
  fp <- paste0("./data/vars/", "dv", "/", "wtr")

  site_files <- list.files(path = fp, pattern = ".csv", full.names = TRUE)

  ## fp <- site_files[grep(var, site_files)]
  ## names(site_files) <- paste(site_files)

  q_data <- ldply(
    .data = site_files,
    .fun = function(x){
      fread(file=x,  select = c("site_no", "dateTime", "X_00010_00001", "X_00010_00001_cd"))
    },
    .parallel = TRUE)

  ## data$site <- str_match(data$.id, "(?<![a-z]{2})[0-9]+")[1]
  ## data <- subset(data, select=-c(.id))

  csv_fp <- paste0("./data/vars/", mode, "/compiled/", "wtr", ".csv")
  colnames(q_data) <- c("site", "datetime", "wtr")
  fwrite(q_data, csv_fp)

  print(paste("SUCCESS:", var))
  print(paste("file saved to", csv_fp))
}


# have to compile info as well
