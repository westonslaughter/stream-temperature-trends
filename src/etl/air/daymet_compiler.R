source("src/etl/etl_helpers.R")
library(dplyr)
library(tidyr)
library(feather)

daymet_files <- list.files("data/raw/dv/air/daymet", full.names = TRUE)

# bar careful- this would be an enormous dataframe
daymet_data <- featherCompiler(daymet_files)
write_feather(daymet_data, "data/munged/dv/air/daymet_compiled_long.feather")

# wide form, by var
daymet_df <- daymet_data %>%
  pivot_wider(id_cols = c("site_code", "date"),
              names_from = var,
              values_from = val)

# save wide df
write_feather(daymet_df, "data/munged/dv/air/daymet_compiled.feather")
check <- read_feather("data/munged/dv/air/daymet_compiled.feather")
