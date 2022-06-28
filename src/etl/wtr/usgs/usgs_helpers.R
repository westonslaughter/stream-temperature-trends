### ---------------------------
##
## Title: usgs_helpers.R
##
## Purpose: this script contains functions of  use for USGS data retrieval and munging
##
## Author: WS
##
## Date Created: 2022-04-11
##
## ---------------------------


stateRetrievalLoop <- function(readpath, writepath, varid = "00010",
                               svc = "dv", type="ST", site_filter = c()) {
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

          if(length(site_filter) > 0) {
            codes <- intersect(codes, site_filter)

            if(length(codes) == 0) {
              print('no sites from user-specified filter present in this record')
              next
            }
          }


          # get dates for site
          begin <- min(df$begin_date)
          end <- max(df$end_date)

          if(svc == "qw") {
            qw_codes <- paste0("USGS-", codes)
            # get grab sample data
            info <- readWQPqw(
              siteNumbers = qw_codes,
              parameterCd = varid,
              startDate = begin,
              endDate = end
            )
          } else {
            # get daily mean data
            info <- readNWISdata(
              sites = codes,
              parameterCd = varid,
              service = svc,
              startDate = begin,
              endDate = end
            )
          }

          if(!exists('info')) {
            print('sites not found in this state')
          } else if(nrow(info) < 1) {
            print(paste("------ results empty:", state, "    ", varid))
          } else {
            # save to wrtiepath
            write.csv(info, wp)
          }
        }
        print(paste0("---- ", state, ": DONE"))
      },
      error = function(e) {
        print(paste("---- ERROR:", state))
      }
    )
  }
}

stateInfoRetrievalLoop <- function(readpath, writepath) {
  print("USGS site info retrieval")

  for (state in state.abb) {
    print(paste("-- attempting info pull", state))

    tryCatch(
      expr = {
        rp <- paste0(readpath, state, ".csv")
        wp <- paste0(writepath, state, "_info.csv")

        # read in CSV
        df <- fread(rp,
                    colClasses=c("character")
                    )

        if(nrow(df) == 0) {
          print(paste("---- WARNING:", state, "input CSV is empty"))
        } else {
          codes <- unique(df$site_no)

          site_info <- readNWISsite(
            siteNumbers = codes
          )

          # save to wrtiepath
          write.csv(site_info, wp)
        }
        print(paste0("---- ", state, ": DONE"))
      },
      error = function(e) {
        print(paste("---- ERROR:", state))
      },
      finally = {
        # if df empty
        if(nrow(site_info) < 1) {
          print(paste("------ results empty:", state, "    ", varid))
        }
      }
    )
  }
}
