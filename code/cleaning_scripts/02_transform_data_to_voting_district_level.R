library(tidyverse)
library(here)

rm(list=ls())

# BRING CLEANING FUNCTIONS INTO OUR GLOBAL ENVIRONMENT ---------------------
source("code/cleaning_scripts/01_electoral_returns_cleaning_functions.R")

# CREATE PATH TO CSV FILES WE WISH TO IMPORT -------------------
my_csv_files <- list.files(here("data", "raw_data", "electoral_returns"))

#READ IN FILES --------------------
for(i in 1:length(my_csv_files)) {
  assign(my_csv_files[i], 
         read.csv(paste0(here("data", "raw_data", "electoral_returns"), sep = "/", my_csv_files[i])))
}

data_2006_clean <- clean_data(lge_2006.csv)
data_2011_clean <- clean_data(lge_2011.csv)


voting_district_unit_of_analysis <- function(data) {
  data <- data %>% pivot_wider(id_cols = c("province", "local_municipality_id", "local_municipality_name", "ward_id",
                                           "voting_district", "registered_voters", "total_votes_cast", "total_valid_votes_cast", "spoilt_votes_cast"),
                               names_from = "party_name",
                               names_prefix = "for_",
                               values_from = "total_valid_votes_cast")
  return(data)
}


data_2006_final <- voting_district_unit_of_analysis(data_2006_clean)
data_2011_final <- voting_district_unit_of_analysis(data_2011_clean)

saveRDS(data_2006_final, here("data", "processed_data", "data_2006_final.RDS"))
saveRDS(data_2011_final, here("data", "processed_data", "data_2011_final.RDS"))
