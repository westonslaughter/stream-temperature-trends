### ---------------------------
##
## Title: helpers.R
##
## Purpose: this script contains functions of general use for this project
##
## Author: WS
##
## Date Created: 2022-04-11
##
## ---------------------------

stateRetrievalLoop <- function(readpath, writepath, varid = "00010", svc = "dv", type="ST") {
  print(paste("USGS data retrieval:", varid))

  for (state in state.abb) {
    print(paste("-- attempting data pull", state))

    tryCatch(
      expr = {
        rp <- paste0(readpath, state, ".csv")
        wp <- paste0(writepath, state, "_", varid, "_", svc, ".csv")

        # read in CSV
        df <- fread(rp,
                    colClasses=c("character"))

        df <- df %>%
          filter(data_type_cd %in% svc) %>%
          filter(site_tp_cd %in% type)


        if(nrow(df) == 0) {
          print(paste("---- WARNING:", state, "input CSV is empty"))
        } else {
          codes <- unique(df$site_no)

          # get dates for site
          begin <- min(df$begin_date)
          end <- max(df$end_date)

          # apply supplied function to the list of sites
          info <- readNWISdata(
            sites = codes,
            parameterCd = varid,
            service = svc,
            startDate = begin,
            endDate = end
          )

          # save to wrtiepath
          write.csv(info, wp)
        }
        print(paste0("---- ", state, ": DONE"))
      },
      error = function(e) {
        print(paste("---- ERROR:", state))
      },
      finally = {
        # if df empty
        if(nrow(info) < 1) {
          print(paste("------ results empty:", state, "    ", varid))
        }
      }
    )
  }
}
