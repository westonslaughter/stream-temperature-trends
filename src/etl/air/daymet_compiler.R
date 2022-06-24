source("src/helpers.R")
library(dplyr)
library(feather)

daymet_files <- list.files("data/daymet", full.names = TRUE)
daymet_data <- featherCompiler(daymet_files)

# wide form, by var
daymet_df <- daymet_data %>%
  pivot_wider(id_cols = c("site_code", "date"),
              names_from = var,
              values_from = val)

# save wide df
write_feather(daymet_df, "data/compiled/daymet.feather")
