library(macrosheds)
library(feather)

## MacroSheds
ms_root = '~/science/macrosheds/data/ms/'

# check out vairablke names
ms_vars <- ms_download_variables()
temp_vars <- ms_vars[grep('Temp', ms_vars$variable_name),]

# check cataliog
ms_catalog <- ms_catalog()

temp_sites <- ms_catalog[grepl('temp', ms_catalog$variable_code),] %>%
  filter(observations > (365 * 4),
         mean_obs_per_day > 0.9) %>%
  pull(site_code)

ms_temp <- ms_load_product(macrosheds_root =  ms_root,
                prodname = 'stream_chemistry',
                site_codes = temp_sites,
                filter_vars = 'temp',
                warn = FALSE)

write_feather(ms_temp, "data/dv/raw/ms/wtr/ms.feather")
