library(dataRetrieval)
library(dplyr)
library(mda.streams)

# names of target varibales for mda.streams function
var_codes <- c("00010")
fails <- c()

for (var_code in var_codes) {
  # loop thru target variables

  for (state in state.abb) {
    # start list to append failed sites (if any)
    failed_sites <- c()

    csv_fp <- paste0("./data/sites/dv/", state, ".csv")
    site_data <- read.csv(csv_fp)
    site_list <- unique(site_data$site_no)

    print(paste("__________", state, "__________"))

    # initialize the counter (i) and  progress bar
    if(length(site_list) > 0) {

      i <- 0
      pb <- txtProgressBar(min = 0, max = length(site_list), style = 3, width = 50, char = "=")

      # loop thru sites and pull data
      for(site_code in site_list) {
        i <- i + 1

        tryCatch(
          expr = {

            ## get dates for site
            begin <- min(site_data[site_data$site_no == site_code,]$begin_date)
            end <- max(site_data[site_data$site_no == site_code,]$end_date)

            # data fp
            if(var_code == "00010") {
              var_name <- "wtr"
            } else {
              var_name <- "disch"
            }


            if(nchar(site_code) >= 8) {
              # actual data pull, for target site and variable
              q_data <- readNWISdata(sites = site_code,
                             parameterCd = var_code,
                             service = "dv",
                             startDate = begin,
                             endDate = end)

              data_fp <- paste0("./data/vars/dv/", var_name, "/", state, "_", site_code, ".csv")
              write.csv(q_data, data_fp)
            } else {
              print(paste("---- warning: first try failed, retrying with leading zero", "", sep="\n"))
              site_code <- paste0("0", site_code)

              q_data <- readNWISdata(sites = site_code,
                             parameterCd = var_code,
                             service = "dv",
                             startDate = begin,
                             endDate = end)

              data_fp <- paste0("./data/vars/dv/", var_name, "/", state, "_", site_code, ".csv")
              write.csv(q_data, data_fp)
            }
          },
          error = function(e) {
            print(paste("---- error:", state, var_name, site_code))
            print("     query failed")
          },

          finally = {
            if(nrow(q_data) > 0) {
              print(paste("pulled", state, var_name, site_code))
            } else {
              print(paste("---- failed:", state, var_name, site_code))
              print("     dataframe empty")
              fails <- c(fails, site_code)
            }
            setTxtProgressBar(pb, i)
          }
        )
      }
    }
  }
}

print(paste("_____ failed sites:", fails, sep = "\n"))
  # USGS parameter codes       CrossReference URL: https://help.waterdata.usgs.gov/parameter_cd?group_cd=PHY
## p_codes <- c("temp"="00010", "Q"="00060", "DO"="00300", "mean_depth"="00064")
