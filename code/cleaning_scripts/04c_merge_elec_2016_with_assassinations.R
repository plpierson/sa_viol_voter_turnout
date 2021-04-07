rm(list=ls())

library(tidyverse)
library(here)
library(sf)
library(readxl)
library(numform)

# READ IN 2016 LGE ELECTORAL DATA
elec_data <- readRDS(here("data", "processed_data", "data_2016_clean_ward.RDS"))

# READ IN 2016 WARD BOUNDARY SHAPE FILE
ward_2016 <- st_read(here("data", "gis_data_raw", "wards2016.shp"))

  #transform to SA's official coordinate system, Hartebeesthoek94 (code 6148)
ward_2016 <- st_transform(ward_2016, 6148)

names(ward_2016)

ward_2016 <- ward_2016 %>% 
  select(-c(OBJECTID, Shape__Are, Shape__Len, DistrictMu, District_1, ProvinceNa, WardNumber, Year, LocalMun_1, LocalMunic)) %>% 
  rename(province_id = ProvinceCo,
         ward_id = WardID)

  # left join elec_data and shape file 
elec_data <- left_join(elec_data, ward_2016, by = "ward_id", "local_municipality_id")
elec_data <- as.data.frame(elec_data)
elec_data <- st_as_sf(elec_data)


# READ IN ASSASSINATIONS DATA
viol <- read_excel(here("data", "raw_data", "assassinations_data", "za_assassinations copy.xlsx"))

# CLEAN ASSASSINATIONS DATA ------------------
viol$year <- year(viol$date_of_attack)
viol$month <- format(viol$date_of_attack, "%m")
viol$month <- f_num(viol$month, digits=0)
viol$sitting <- as.numeric(viol$sitting)
viol$electoral_cycle <- as.integer(viol$electoral_cycle)

# FILTER TO LOOK EXCLUSIVELY AT ATTACKS ON LC/MC WARD POLITICIANS AND/OR CANDIDATES ------------------
viol_ward <- viol %>% 
  filter(seat_type=="LC Ward" | seat_type=="MC Ward" | seat_type=="MC Ward candidate" | seat_type=="LC Ward candidate") %>% 
  filter(date_of_attack>="2000-12-05" & date_of_attack<="2016-08-03") 


# CREATE DUMMY VARIABLES FOR ASSASSINATION EXPOSURE ------------------

  # begin with assassinations in the 14 day run-up to elections, then proceed to 30, 60, and 90 day windows, and finally any exposure
viol_14day <- viol_ward %>% 
  filter(date_of_attack >= "2016-07-20" & date_of_attack < "2016-08-03") 

viol_30day <- viol_ward %>% 
  filter(date_of_attack >= "2016-07-03" & date_of_attack < "2016-08-03") 

viol_60day <- viol_ward %>% 
  filter(date_of_attack >= "2016-06-03" & date_of_attack < "2016-08-03") 

viol_90day <- viol_ward %>% 
  filter(date_of_attack >= "2016-05-03" & date_of_attack < "2016-08-03") 

viol_any <- viol_ward %>% 
  filter(date_of_attack > "2011-05-18" & date_of_attack < "2016-08-03")


viol_any_ward_ids <- viol_any$ward_id
  # these are the ward_ids for all wards that experienced an assassination attempt after the 2011 LGEs and
  # prior to the 2016 LGEs. Some of these ward_ids remain unchanged between 2011 and 2016, while others 
  # change due to redistricting, administrative rejigging, etc. First, let's see which ones don't change

ward_ids_unchanged <- ward_2016 %>% 
  filter(ward_id %in% viol_any_ward_ids) #3 ward ids do not match up
  # upon visible inspection, we see that the three ward_ids that appear in 2011 but no longer appear in 2016 are
  # the following: ward_id 52304008 and ward_id 52306002 and ward_id 52101010

  # in order to get the appropriate centroid locations, I will first take the 14 values that synced correctly and pull their
  # centroid coordinates from the ward2016 shape file

centroids_vector_1 <- as.data.frame(st_coordinates(st_centroid(ward_ids_unchanged$geometry)))

  # now need to extract the appropriate centroid coordinates for the three wards that did not sync with 2016 ward_ids...
  # these three wards will need to be connected to the ward2011 shape files in order to extract the correct location

ward_2011 <- st_read(here("data", "gis_data_raw", "wards2011.shp"))

names(ward_2011)
ward_2011 <- ward_2011 %>% 
  rename(ward_id = WardID)

ward_2011 <- st_transform(ward_2011, 6148) #transform to SA's official coordinate system, Hartebeesthoek94 (code 6148)
ward_id_changes <- ward_2011 %>% 
  filter(ward_id == "52304008" | ward_id == "52306002" | ward_id == "52101010") 

  #there are duplicates in ward2011 file...drop those
ward_id_changes <- ward_id_changes %>% 
  distinct(ward_id, .keep_all = TRUE)

  #now extract the centroid coordinates for these two wards
centroids_vector_2 <- as.data.frame(st_coordinates(st_centroid(ward_id_changes$geometry)))

  #combine centroid vectors together to create one vector for all 17 ward observations that were treated between 2006 and 2011
centroids <- rbind(centroids_vector_1, centroids_vector_2)

  #convert centroid vector to an sf_object 
pnts_sf <- st_as_sf(centroids, coords = c("X", "Y"), crs = st_crs(6148))

  #find where these centroid coordinates intersect with wards in df_sf (the 2011 vd results that are in 2016 ward boundaries)
pnts <- pnts_sf %>% mutate(
  intersection = as.integer(st_intersects(geometry, elec_data)),
  area = if_else(is.na(intersection), '', elec_data$ward_id[intersection])
)

pnts <- pnts %>% 
  rename(ward_id = area)

pnts <- as.data.frame(pnts)
pnts <- pnts %>% 
  select(ward_id)

  # drop geom so that we can manipulate with tidyverse
elec_data_2 <- elec_data %>% 
  st_drop_geometry()

# CREATE DUMMY VARIABLES FOR ASSASSINATION EXPOSURE ---------------------
elec_data_2 <- elec_data_2 %>% 
  mutate(treat = if_else(ward_id %in% pnts$ward_id, 1, 0))

  # check to make sure this worked properly: 
length(which(elec_data_2$treat==1)) ###17 woohoo!

  #14day dummy
elec_data_2 <- elec_data_2 %>% 
  mutate(treat14 = if_else(ward_id == "29300004" | ward_id == "79800058", 1, 0),)

  #30day dummy
elec_data_2 <- elec_data_2 %>% 
  mutate(treat30 = if_else(ward_id == "52308015" |
                             ward_id == "52104001" |
                             ward_id == "52308020" |
                             ward_id == "29300004" |
                             ward_id == "79800058",
                           1, 0))

  #60day dummy
elec_data_2 <- elec_data_2 %>% 
  mutate(treat60 = if_else(ward_id == "52308015" |
                             ward_id == "52104001" |
                             ward_id == "52308020" |
                             ward_id == "29300004" |
                             ward_id == "79800058" |
                             ward_id == "29300015" |
                             ward_id == "52502006",
                           1, 0))

  #90day dummy
elec_data_2 <- elec_data_2 %>% 
  mutate(treat90 = if_else(ward_id == "52308015" |
                             ward_id == "52104001" |
                             ward_id == "52308020" |
                             ward_id == "29300004" |
                             ward_id == "79800058" |
                             ward_id == "29300015" |
                             ward_id == "52502006" |
                             ward_id == "52605010",
                           1, 0))

###############################
###############################
###############################
######## HHI VARIABLE #########
###############################
###############################
###############################


#mun_total_valid_votes = rowSums(pr_final[,9:211]),
hhi2 <- pivot_longer(elec_data_2,
                     cols = starts_with("vote_share"),
                     names_to = "party",
                     values_to = "vote_share")

hhi2 <- hhi2 %>% 
  relocate(party, .after = ward_id) %>% 
  relocate(vote_share, .after = ward_id)

#can't run subsequent calculations with NAs...remove and then execute calculations
hhi2 <- hhi2 %>% 
  filter(!is.na(vote_share))

hhi2 <- hhi2 %>% 
  group_by(local_municipality_id, ward_id) %>% 
  mutate(hhi_index = 1 - sum(vote_share*vote_share)) %>% 
  distinct(ward_id, .keep_all=TRUE)

#now merge hhi data with full dataset
hhi2 <- hhi2 %>% 
  select(local_municipality_id, ward_id, hhi_index)

data_2016_with_assassinations_merged <- left_join(elec_data_2, hhi2, by = c("local_municipality_id", "ward_id"))

data_2016_with_assassinations_merged <- data_2016_with_assassinations_merged %>% 
  mutate(electoral_cycle = as.factor(2016))



#export
saveRDS(data_2016_with_assassinations_merged, 
        here("data", "processed_data", "data_2016_with_assassinations_merged.rds"))
