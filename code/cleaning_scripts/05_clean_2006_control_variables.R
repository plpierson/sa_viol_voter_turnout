rm(list=ls())
library(tidyverse)
library(here)
library(sf)
library(areal)

options(scipen = 999) # get rid of scientific notation for variables

# IMPORT CONTROL VARIABLES ----------------------------

# employment
employ <- read.csv(here("data", "raw_data", "control_variables", "ward_2001_employment_status.csv"))

names(employ)
employ <- employ %>% 
  rename(ward_id = Employment.status..official.) 

employ$ward_id <- gsub(":.*", "", employ$ward_id)
employ <- employ %>% 
  mutate_if(is.numeric, round, 0)

employ <- employ %>%    
  mutate(unemploy_rate = (Unemployed + Discouraged.work.seeker) / (Total - Not.applicable))


# first language
lang <- read.csv(here("data", "raw_data", "control_variables", "ward_2001_first_language.csv"))  

names(lang)
lang <- lang %>% 
  rename(ward_id = Main.language)

lang$ward_id <- gsub(":.*", "", lang$ward_id)
lang <- lang %>% 
  mutate_if(is.numeric, round, 0)

lang <- lang %>% 
  mutate(sum = rowSums(across(where(is.numeric)))) %>% 
  mutate(prop_english = (English / sum))


# main dwelling type
dwell <- read.csv(here("data", "raw_data", "control_variables", "ward_2001_main_dwelling.csv")) 

dwell <- dwell %>% 
  rename(ward_id = Type.of.main.dwelling..grouped.)

dwell$ward_id <- gsub(":.*", "", dwell$ward_id)
dwell <- dwell %>% 
  mutate_if(is.numeric, round, 0)

dwell <- dwell %>% 
  mutate(prop_informal = Informal.dwelling / Total)


#population groups
pop <- read.csv(here("data", "raw_data", "control_variables", "ward_2001_population_group.csv"))   

pop <- pop %>% 
  rename(ward_id = Population.group)

pop$ward_id <- gsub(":.*", "", pop$ward_id)

pop <- pop %>% 
  mutate_if(is.numeric, round, 0)

pop <- pop %>% 
  mutate(prop_black = Black.African / Total)


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




# NOW SYNC THESE VALUES WITH 2011 WARD BOUNDARIES (OFFICIAL WARD BOUNDARIES MOST TEMPORALLY PROXIMATE TO 2006) 
ward_2011 <- st_read(here("data", "gis_data_raw", "wards2011.shp"))
ward_2011 <- st_transform(ward_2011, 6148)

ward_2011 <- ward_2011 %>% 
  rename(ward_id = WardID) %>% 
  select(ward_id, geometry)

  # raw 2011 shape file has duplicates...clean these below
ward_2011 <- ward_2011 %>% 
  distinct(ward_id, .keep_all = TRUE)

controls_2006 <- left_join(ward_2011, df_controls, by = "ward_id")

  # now read in 2016 shape files
ward_2016 <- st_read(here("data", "gis_data_raw", "wards2016.shp"))
ward_2016 <- st_transform(ward_2016, 6148)

  # rename ward_id variable for subsequent merger
ward_2016 <- ward_2016 %>% 
  rename(ward_id = WardID)

  # clean GIS geometries to prepare for merge
controls_2006 <- st_as_sf(controls_2006)
ward_2016 <- st_as_sf(ward_2016)

controls_2006 <- st_make_valid(controls_2006)
ward_2016 <- st_make_valid(ward_2016)

  # areal interpolation to 2016 ward boundaries
df_areal <- aw_interpolate(ward_2016, 
                           tid = "ward_id", 
                           source = controls_2006,
                           sid = "ward_id",
                           weight = "sum", 
                           output = "tibble",
                           intensive = c("unemploy_rate", "prop_english", "prop_informal", "prop_black"))


  # select variables we would like to keep
df_areal_2 <- df_areal %>% 
  select(ward_id, prop_black, prop_english, prop_informal, unemploy_rate)



# EXPORT DATA TO PROCESSED_DATA SUBDIRECTORY 
saveRDS(df_areal_2, here("data", "processed_data", "data_2006_controls.rds"))
