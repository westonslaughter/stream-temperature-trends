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


fileCompiler <- function(listfiles) {
  for (file in listfiles) {
    tryCatch(
      expr = {
          file_data <- fread(file, colClasses = "character")

          if(!exists("all_df")) {
            all_df <- file_data
            print("__ CREATING DATAFRAME __")
            print(paste("DONE:", file))
          } else {
            all_df <- rbind(all_df, file_data, fill = TRUE)
            print(paste("DONE:", file))
          }
        },
      error = function(e) {
        print(paste("---- ERROR:", file))
        }
    )
  }
  return(all_df)
}

featherCompiler <- function(listfiles) {
  for (file in listfiles) {
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
  return(all_df)
}
