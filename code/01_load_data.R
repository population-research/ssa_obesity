# Place holder script to allow renv setup

library(tidyverse)
library(here)
library(haven)

rename_to_lower_snake <- function(df) {
  df %>%
    rename_with( ~gsub("([a-z])([A-Z])", "\\1_\\2", .x) ) %>%  # Adds _ to camel case var names
    rename_with( ~tolower(gsub("[ ]+", "_", .x)) )  # Converts to lower and substitutes _ for spaces
}


# Unzip the data
unzip(here("raw_data", "dhs"),
      exdir = here("data", "unzipped_dhs"),
      junkpaths = TRUE,
      unzip = "unzip"
)


# Get a list of files for tz
tz_dhs_data <- tibble(
  tz = list.files(
    here("data", "unzipped_dhs"),
    pattern = "^TZ.*",
    full.names = TRUE
  )
)

unzipped_tz <- unzip(tz_dhs_data,
                     exdir = here("data", "unzipped_tz_dhs"),
                     junkpaths = TRUE,
                     unzip = "unzip"
)

# Unzip all .zip files in the directory

zip_dir_path <- here("data", "unzipped_dhs")
tz_zip_files <- list.files(zip_dir_path, pattern = "*.zip", full.names = TRUE)
sapply(tz_zip_files, unzip, exdir = here("data", "unzipped_tz_dhs"))

# Get a list of all .dta files in the directory
tz_dta_files <- list.files(here("data", "unzipped_tz_dhs"), pattern = "*.dta", full.names = TRUE)

# Load each .dta file as a tibble and store in a list
tz_dta_list <- lapply(tz_dta_files, read_dta) %>%
  map(as_tibble)

# Combine all tibbles in the list into a single tibble
all_tz_dta_data <- bind_rows(tz_dta_list)









