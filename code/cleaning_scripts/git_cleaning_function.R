library(tidyverse)
library(stringi)
library(here)

rm(list=ls())

# READ IN DATA USING HERE PACKAGE
data <- read.csv(here("data", "raw_data", "electoral_returns", "lge_2006.csv"))

glimpse(data)


# FUNCTION FOR RENAMING COLUMNS ------------------
rename_columns <- function(data) {
  data <- data %>% rename(province = Province,
                          municipality = Municipality, 
                          ward_id = Ward, 
                          voting_district = Voting..District,
                          party_name = Party,
                          ballot_type = Ballot..Type,
                          registered_voters = Registered.Voters, 
                          total_valid_votes_cast = Valid.Votes..Cast,
                          total_votes_cast = Total.Votes..Cast,
                          spoilt_votes_cast = Spoilt.Votes)
  return(data)
}


# FUNCTION FOR TIDYING DATA ------------------
tidy_data <- function(data) {
  data <- data %>% mutate(electoral_cycle = 2006,
                          election_type = "lge") %>% 
    separate(municipality, c("local_municipality_id", "local_municipality_name"), sep = "-", 
             extra = "merge", fill = "right") %>%
    select(-c(Electoral.Event, X..Voter..Turnout, MEC7.Votes)) %>% 
    mutate_if(is.character, str_trim) #remove any whitespace in character vectors
  return(data)
}



# FUNCTION FOR CONVERTING VARIABLE CLASS ------------------
convert_class <- function(data) {
  data <- data %>% mutate(province = as.factor(province),
                          local_municipality_id = as.factor(local_municipality_id),
                          local_municipality_name = as.factor(local_municipality_name),
                          ward_id = as.factor(ward_id),
                          voting_district = as.factor(voting_district), 
                          party_name = as.factor(party_name),
                          ballot_type = as.factor(ballot_type),
                          electoral_cycle = as.factor(electoral_cycle)
  )
  return(data)
}


# FUNCTION TO SELECT THE "WARD" RESULTS (AS OPPOSED TO PR OR DC40%)
subset_to_ward <- function(data) {
  data <- data %>% 
    filter(ballot_type == "WARD")
  return(data)
}


# FUNCTION TO CLEAN UP STRINGS 
clean_strings <- function(data) {
  data <- data %>% 
    mutate(party_name = str_replace_all(party_name, " ", "_")) %>% 
    mutate(across(.cols = everything(), .fns = ~stringi::stri_replace_all_regex(.x, "'", ""))) %>% 
    mutate(across(.cols = everything(), .fns = ~stringi::stri_replace_all_regex(.x, "/", ""))) %>% 
    mutate(across(.cols = everything(), .fns = ~stringi::stri_replace_all_regex(.x, "-", ""))) %>% 
    mutate(local_municipality_id = str_replace_all(local_municipality_id, "KZ", "KZN")) %>% 
    mutate(local_municipality_id = str_replace_all(local_municipality_id, "NP", "LIM")) %>% 
    mutate(local_municipality_id = str_replace_all(local_municipality_id, "Durban", "ETH")) %>% 
    mutate(local_municipality_id = str_replace_all(local_municipality_id, "Cape Town", "CPT")) %>% 
    mutate(local_municipality_id = str_replace_all(local_municipality_id, "Johannesburg", "JHB")) %>% 
    mutate(local_municipality_id = str_replace_all(local_municipality_id, "Port Elizabeth", "NMB")) %>% 
    mutate(local_municipality_name = str_replace_all(local_municipality_name, "Durban", "eThekwini")) %>% 
    mutate(local_municipality_name = str_replace_all(local_municipality_name, "Nelson Mandela [Port Elizabeth]", "Nelson Mandela Bay"))
  return(data)
}


# PRIMARY FUNCTION FOR DATA CLEANING AND PROCESSING
clean_data <- function(data) {
  data <- rename_columns(data)
  data <- tidy_data(data)
  data <- convert_class(data)
  data <- subset_to_ward(data)
  data <- clean_strings(data)
  return(data)
}

# PERFORM FUNCTION AND ENJOY MY NEW, CLEAN DATASET
data_final <- clean_data(data)