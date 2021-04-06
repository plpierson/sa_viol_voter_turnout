library(tidyverse)
library(here)

#read in packages for areal interpolation
library(sf)
library(areal)

rm(list=ls())

# READ IN SHAPE FILES -----------------------
vd_2011_shape_file <- st_read(here("data", "gis_data_raw", "Election2011_VotingDistricts.shp"))
ward_2016_shape_file <- st_read(here("data", "gis_data_raw", "wards2016.shp"))


# TRANSFORM SHAPE FILES TO SA'S OFFICIAL COORDINATE SYSTEM -----------------------
vd_2011_shape_file <- st_transform(vd_2011_shape_file, 6148) #transform to SA's official coordinate system, Hartebeesthoek94 (code 6148)
ward_2016_shape_file <- st_transform(ward_2016_shape_file, 6148) #transform to SA's official coordinate system, Hartebeesthoek94 (code 6148)

# RENAME VARIABLES IN VOTING DISTRICT 2006 SHAPE FILE -----------------------
vd_2011_shape_file <- vd_2011_shape_file %>% 
  rename(voting_district = VDNumber) %>% 
  select(voting_district, geometry)

# RENAME VARIABLES IN WARD 2016 SHAPE FILE -----------------------
ward_2016_shape_file <- ward_2016_shape_file %>% 
  rename(ward_id = WardID) %>% 
  select(ward_id, geometry)


# READ IN CLEAN 2011 ELECTORAL RETURNS DATA
electoral_data_2011 <- readRDS(here("data", "processed_data", "data_2011_clean_voting_district.RDS"))
vd_2011_shape_file$voting_district <- as.character((vd_2011_shape_file$voting_district))

electoral_data_2011_with_geom <- left_join(electoral_data_2011, vd_2011_shape_file, by = "voting_district")

# CONVERT DATA TO SF OBJECTS IN ORDER TO PERFORM AREAL INTERPOLATION
electoral_data_2011_with_geom <- st_as_sf(electoral_data_2011_with_geom)
ward_2016_shape_file <- st_as_sf(ward_2016_shape_file)


# VALIDATE GEOM
electoral_data_2011_with_geom <- st_make_valid(electoral_data_2011_with_geom)
st_is_valid(electoral_data_2011_with_geom, reason = TRUE)

ward_2016_shape_file <- st_make_valid(ward_2016_shape_file)
st_is_valid(ward_2016_shape_file, reason=TRUE)


# CONVERT NAs IN ORDER FOR AREAL FUNCTIONS TO EXECUTE PROPERLY
electoral_data_2011_with_geom <- st_transform(electoral_data_2011_with_geom, 6148)

electoral_data_2011_with_geom <- electoral_data_2011_with_geom %>%
  mutate_at(c(7:128), ~replace(., is.na(.), 0)) %>% 
  mutate_at(c(7:128), as.numeric)

glimpse(electoral_data_2011_with_geom)


  ### extract column names to pass to aw_interpolate function
names <- colnames(electoral_data_2011_with_geom)
names <- names[6:128]


electoral_data_2011_areal_2016 <- aw_interpolate(ward_2016_shape_file,
                                                 tid = "ward_id",
                                                 source = electoral_data_2011_with_geom,
                                                 sid = "voting_district",
                                                 weight = "sum",
                                                 output = "tibble",
                                                 extensive = names)




options(scipen = 999) # get rid of scientific notation

electoral_data_2011_areal_2016_round <- electoral_data_2011_areal_2016 %>% 
  mutate_if(is.numeric, ~round(., 0)) # round to nearest whole number


# CREATE NEW VARIABLES -----------------------
electoral_data_2011_areal_2016_round <- electoral_data_2011_areal_2016_round %>% 
  mutate(across(starts_with("for_"), (function(x) (x/total_votes_cast)), .names = "vote_share_{.col}")) %>% 
  mutate(pct_turnout = (total_votes_cast/registered_voters)*100,
         pct_spoilt = (spoilt_votes_cast/total_votes_cast)*100) 


# WRITE TO PROCESSED_DATA FOLDER -----------------------
saveRDS(electoral_data_2011_areal_2016_round,
        here("data", "processed_data", "vd_2011_in_ward_2016_boundaries_raw.RDS"))



