source("src/helpers.R")

daymet_files <- list.files("./data/daymet", full.names = TRUE)
daymet_data <- featherCompiler(daymet_files)
