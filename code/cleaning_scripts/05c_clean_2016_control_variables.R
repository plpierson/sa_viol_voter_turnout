library(tidyverse)
library(here)
library(readxl)
library(sf)
library(areal)

rm(list=ls())

options(scipen = 999) # get rid of scientific notation for variables



# IMPORT CONTROL VARIABLES ----------------------------

  # employment
employ <- read.csv(here("data", "raw_data", "control_variables", "ward_2016_employment_status.csv"))

names(employ)
employ <- employ %>% 
  rename(ward_id = Official.employment.status) 

employ$ward_id <- gsub(" .*", "", employ$ward_id)
employ <- employ %>% 
  mutate_if(is.numeric, round, 0)

employ <- employ %>% 
  mutate(unemploy_rate = (Unemployed + Discouraged.work.seeker) / (Total_employ_respondents - not_applicable_employ_respondents))



  # first language
lang <- read.csv(here("data", "raw_data", "control_variables", "ward_2016_first_language.csv"))

names(lang)
lang <- lang %>% 
  rename(ward_id = Language..first.)

lang$ward_id <- gsub(" .*", "", lang$ward_id)
lang <- lang %>% 
  mutate_if(is.numeric, round, 0)

lang <- lang %>% 
  mutate(sum = rowSums(across(where(is.numeric)))) %>% 
  mutate(prop_english = (English / sum))



  # main dwelling type
dwell <- read_excel(here("data", "raw_data", "control_variables", "ward_2016_main_dwelling.xls"))

dwell <- dwell %>% 
  rename(ward_id = "Type of main dwelling - main")

dwell$ward_id <- gsub(" .*", "", dwell$ward_id)
dwell <- dwell %>% 
  mutate_if(is.numeric, round, 0)

dwell <- dwell %>% 
  mutate(prop_informal = (`Informal dwelling/shack in back yard` + `Informal dwelling/shack NOT in back yard; e.g. in an informal/squatter settlement or on farm`) / (Total - `Unspecified` -`Not applicable`))


  # population groups
pop <- read.csv(here("data", "raw_data", "control_variables", "ward_2016_population_groups.csv"))

pop <- pop %>% 
  rename(ward_id = Geography.2016)

pop$ward_id <- gsub(" .*", "", pop$ward_id)

pop <- pop %>% 
  mutate_if(is.numeric, round, 0)

pop <- pop %>% 
  pivot_wider(names_from = Population.group,
              values_from = Count)

pop <- pop %>% 
  mutate(prop_black = `Black African` / (Total - Unspecified))

#pop accidentally has extra row still there...need to drop it
pop <- pop[1:4392, ]



# SUBSET EACH DATAFRAME TO INCLUDE ONLY THOSE VARIABLES NEEDED ----------------------------
employ_sub <- employ %>% 
  select(ward_id, unemploy_rate)

lang_sub <- lang %>% 
  select(ward_id, prop_english)

dwell_sub <- dwell %>% 
  select(ward_id, prop_informal)

pop_sub <- pop %>% 
  select(ward_id, prop_black)


# MERGE TOGETHER USING RBIND ----------------------------
df_controls <- employ_sub %>% 
  inner_join(lang_sub, by = "ward_id")

df_controls <- df_controls %>% 
  inner_join(dwell_sub, by = "ward_id")

df_controls <- df_controls %>% 
  left_join(pop_sub, by = "ward_id")


# EXPORT DATA TO PROCESSED_DATA SUBDIRECTORY 
saveRDS(df_controls, here("data", "processed_data", "data_2016_controls.rds"))


