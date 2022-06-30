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

fileCompiler <- function(fp, site_filter = c()) {
  flist <- list.files(fp, full.names = TRUE)
  for (file in flist) {
    tryCatch(
      expr = {
          file_data <- fread(file, colClasses = "character")

          if(length(site_filter) > 0) {
            tryCatch(
              expr = {
                file_data <- file_data %>%
                  filter(site_no %in% site_filter)
              },
              error = function(e) {
                print(paste(file, '/n----- ERROR'))
              }
            )
          }

          if(!exists("all_df")) {
            all_df <- file_data
            print("__ CREATING DATAFRAME __")
            print(paste("DONE:", file))
          } else {
            all_df <- rbind(all_df, file_data, fill = TRUE)
            print(paste("DONE:", file))
          }

          ## print('______ overwriting feather with new state data')
          ## write_feather(all_df, f)
        },
      error = function(e) {
        print(paste("---- ERROR:", file))
        }
    )
  }
  return(all_df)
}


featherCompiler <- function(fp, site_filter = c()) {
  flist <- list.files(fp, full.names = TRUE)

  for (file in flist) {
    filter_test <- stri_detect_fixed(file, site_filter)

    if(TRUE %in% filter_test) {
      tryCatch(
        expr = {
            file_data <- read_feather(file)

            if(!exists("all_df")) {
              all_df <- file_data
              print("__ CREATING DATAFRAME __")
              print(paste("DONE:", file))
            } else {
              all_df <- rbind(all_df, file_data)
              print(paste("DONE:", file))
            }
          },
        error = function(e) {
          print(paste("---- ERROR:", file))
          }
      )
    }
  }
  return(all_df)
}

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

## stateInfoRetrievalLoop <- function(readpath, writepath, site_filter = c()) {
##   print("USGS site info retrieval")

##   for (state in state.abb) {
##     print(paste("-- attempting info pull", state))

##     tryCatch(
##       expr = {
##         rp <- paste0(readpath, state, ".csv")
##         wp <- paste0(writepath, state, "_info.csv")

##         # read in CSV
##         df <- fread(rp, colClasses=c("character"))

##         if(nrow(df) == 0) {
##           print(paste("---- WARNING:", state, "input CSV is empty"))
##         } else {
##           codes <- unique(df$site_no)

##           if(length(site_filter) < 1) {
##             site_filter <- codes
##           }

##           if(TRUE %in% (codes %in% site_filter)) {
##             site_info <- readNWISsite(
##               siteNumbers = codes
##             )

##             # save to writepath
##             write.csv(site_info, wp)
##           }
##         }
##         print(paste0("---- ", state, ": DONE"))
##       },
##       error = function(e) {
##         print(paste("---- ERROR:", state))
##       },
##       finally = {
##         # if df empty
##         if(nrow(site_info) < 1) {
##           print(paste("------ results empty:", state, "    ", varid))
##         }
##       }
##     )
##   }
## }
