library(dplyr)
library(dataRetrieval)


for (state in state.abb) {
  tryCatch(
    expr = {
      fp <- paste0("./data/sites/dv/", state, ".csv")
      site_data <- read.csv(fp, colClass="character")


      if(nrow(site_data) == 0) {
        print(paste("WARNING:",
                    state,
                    "no sites from this state")
              )
      } else {
        info <- readNWISsite(unique(site_data$site_no))
        write.csv(info, paste0("./data/sites/info/", state, ".csv"))

        if(!exists("site_info")) {
          site_info <- info
        } else {
          site_info <- rbind(site_info, info)
        }
      }


      print(paste("DONE:", state))
    },
    error = function(e) {
      print("ERROR", state)
    }
  )
}

write.csv(site_info, "dv_sites_info.csv")
