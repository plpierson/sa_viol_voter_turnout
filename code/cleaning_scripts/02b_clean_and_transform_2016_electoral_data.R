library(tidyverse)
library(stringi)
library(here)

rm(list=ls())


# FUNCTION FOR RENAMING COLUMNS ------------------
rename_columns <- function(data) {
  data <- data %>% rename(province = Province,
                          municipality = Municipality, 
                          ward_id = Ward, 
                          voting_district = VotingDistrict,
                          party_name = PartyName,
                          ballot_type = BallotType,
                          registered_voters = RegisteredVoters, 
                          total_valid_votes_cast = TotalValidVotes,
                          spoilt_votes_cast = SpoiltVotes)
  return(data)
}

# FUNCTION FOR TIDYING DATA ------------------
tidy_data <- function(data) {
  data <- data %>% mutate(electoral_cycle = 2016,
                          election_type = "lge") %>% 
    separate(municipality, c("local_municipality_id", "local_municipality_name"), sep = "-", 
             extra = "merge", fill = "right") %>%
    select(-c(VotingStationName, DateGenerated)) %>% 
    mutate_if(is.character, str_trim) %>% #remove any whitespace in character vectors
    mutate(ward_id = str_replace_all(ward_id, "Ward ", ""))
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
    filter(ballot_type == "Ward")
  return(data)
}


# FUNCTION TO CLEAN UP STRINGS 
clean_strings <- function(data) {
  data <- data %>% 
    mutate(party_name = str_replace_all(party_name, " ", "_")) %>% 
    mutate(across(c(province, local_municipality_id, local_municipality_name, party_name), .fns = ~stringi::stri_replace_all_regex(.x, "'", ""))) %>% 
    mutate(across(c(province, local_municipality_id, local_municipality_name, party_name), .fns = ~stringi::stri_replace_all_regex(.x, "/", ""))) %>% 
    mutate(across(c(province, local_municipality_id, local_municipality_name, party_name), .fns = ~stringi::stri_replace_all_regex(.x, "-", ""))) %>% 
    #mutate(local_municipality_id = str_replace_all(local_municipality_id, "KZ", "KZN")) %>% 
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

# READ IN DATA USING HERE PACKAGE
data <- read.csv(here("data", "raw_data", "electoral_returns", "lge_2016.csv"))
glimpse(data)

data_2016_clean <- clean_data(data)
glimpse(data_2016_clean)



# NOW TRANSFORM UNIT OF ANALYSIS TO VOTING DISTRICT LEVEL ---------------------------
data_2016_clean <- data_2016_clean %>%
  pivot_wider(names_from = "party_name",
              names_prefix = "for_", 
              values_from = "total_valid_votes_cast")


# RE-ORDER VARIABLES TO MAKE SUBSEQUENT AGGREGATION CALL CLEAN ---------------------------
data_2016_clean <- data_2016_clean %>% 
  relocate(election_type, .after = voting_district) %>% 
  relocate(electoral_cycle, .after = local_municipality_name) %>% 
  relocate(ballot_type, .after = electoral_cycle)


# AGGREGATE TO WARD ---------------------------
data_2016_clean <- data_2016_clean %>%
  group_by(province, local_municipality_id, ward_id) %>% # group at municipality level
  mutate(across(.cols = registered_voters:for_KAROO_DEMOCRATIC_FORCE, .fns = sum, na.rm=TRUE)) %>% 
  distinct(local_municipality_id, .keep_all = TRUE)  # keep one observation for each unique municipality (all others are now duplicates)

# Create total_votes_cast variable
data_2016_clean <- data_2016_clean %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(total_votes_cast = sum(dplyr::c_across(for_AFRICAN_CHRISTIAN_DEMOCRATIC_PARTY:
                                                         for_KAROO_DEMOCRATIC_FORCE), na.rm=TRUE))

data_2016_clean <- data_2016_clean %>% 
  relocate(total_votes_cast, .after = registered_voters)


#now create new variables
data_2016_clean <- data_2016_clean %>% 
  mutate(across(starts_with("for_"), (function(x) (x/total_votes_cast)), .names = "vote_share_{.col}")) %>% 
  mutate(pct_turnout = (total_votes_cast/registered_voters)*100,
         pct_spoilt = (spoilt_votes_cast/total_votes_cast)*100) 


saveRDS(data_2016_clean, here("data", "processed_data", "data_2016_clean_ward.RDS"))
