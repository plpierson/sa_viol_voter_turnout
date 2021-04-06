rm(list=ls())

library(tidyverse)
library(sf)
library(readxl)
library(lubridate)
library(numform)
library(here)


# READ IN ASSASSINATIONS DATA ------------------
viol <- read_excel(here("data", "raw_data", "assassinations_data", "za_assassinations copy.xlsx"))

# READ IN ELECTORAL RETURNS DATA ------------------
elec_data <- readRDS(here("data", "processed_data", "vd_2011_in_ward_2016_boundaries_raw.RDS"))


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
  filter(date_of_attack >= "2011-05-04" & date_of_attack < "2011-05-18") 

viol_30day <- viol_ward %>% 
  filter(date_of_attack >= "2011-04-18" & date_of_attack < "2011-05-18") 

viol_60day <- viol_ward %>% 
  filter(date_of_attack >= "2011-03-18" & date_of_attack < "2011-05-18") 

viol_90day <- viol_ward %>% 
  filter(date_of_attack >= "2011-02-18" & date_of_attack < "2011-05-18") 

viol_any <- viol_ward %>% 
  filter(date_of_attack > "2006-03-01" & date_of_attack < "2011-05-18")


# IDENTIFY WARDS THAT CHANGED BETWEEN 2006 LGE AND 2011 LGE ------------------
viol_any_ward_ids <- viol_any$ward_id
  #these are the ward_ids for all wards that experienced an assassination attempt after the 2006 LGEs and
  #prior to the 2011 LGEs. Some of these ward_ids remain unchanged between 2006 and 2011, while others 
  #change due to redistricting, administrative rejigging, etc. First, let's see which ones don't change


# READ IN 2011 SHAPE FILE WITH WARD IDs ------------------
ward2011 <- st_read(here("data", "gis_data_raw", "Wards2011.shp"))

ward2011 <- ward2011 %>% 
  rename(ward_id = WardID)

  # there are duplicates in the original file...filter appropriately
ward2011 <- ward2011 %>% 
  distinct(ward_id, .keep_all = TRUE)

ward2011 <- st_transform(ward2011, 6148) #transform to SA's official coordinate system, Hartebeesthoek94 (code 6148)

  # identify wards that did not change
ward_ids_unchanged <- ward2011 %>% 
  filter(ward_id %in% viol_any_ward_ids)


  #upon visible inspection, we see that the two ward_ids that appear in 2006 but no longer appear in 2011 are
  #the following: ward_id 64005007 and ward_id 59200068

  #in order to get the appropriate centroid locations, I will first take the 15 values that synced correctly and pull their
  #centroid coordinates from the ward2011 shape file
centroids_vector_1 <- as.data.frame(st_coordinates(st_centroid(ward_ids_unchanged$geometry)))


  #now need to extract the appropriate centroid coordinates for the two wards that did not sync with 2011 ward_ids...
  #these two wards will need to be connected to the ward2006 shape files in order to extract the correct location
ward2006 <- st_read(here("data", "gis_data_raw", "Wards2006.shp"))
ward2006 <- ward2006 %>% 
  rename(ward_id = WARD_ID)

ward2006 <- st_transform(ward2006, 6148) #transform to SA's official coordinate system, Hartebeesthoek94 (code 6148)
ward_id_changes <- ward2006 %>% 
  filter(ward_id == "64005007" | ward_id == "59200068") 

  #now extract the centroid coordinates for these two wards
centroids_vector_2 <- as.data.frame(st_coordinates(st_centroid(ward_id_changes$geometry)))

  #combine centroid vectors together to create one vector for all 17 ward observations that were treated between 2006 and 2011
centroids <- rbind(centroids_vector_1, centroids_vector_2)

  #convert centroid vector to an sf_object 
pnts_sf <- st_as_sf(centroids, coords = c("X", "Y"), crs = st_crs(6148))

  #find where these centroid coordinates intersect with wards in elec_data (the 2011 vd results that are in 2016 ward boundaries)
  # first, need to merge elec_data with ward 2016 shape file

  # read in 2016 geometry
ward_2016 <- st_read(here("data", "gis_data_raw", "Wards2016.shp"))

ward_2016 <- st_transform(ward_2016, 6148) #transform to SA's official coordinate system, Hartebeesthoek94 (code 6148)
names(ward_2016)

ward_2016 <- ward_2016 %>% 
  select(-c(OBJECTID, Shape__Are, Shape__Len, DistrictMu, District_1, ProvinceNa, WardNumber, Year)) %>% 
  rename(province_id = ProvinceCo,
         local_municipality_id = LocalMunic,
         ward_id = WardID, 
         local_municipality_name = LocalMun_1)

  # left join
elec_data <- left_join(elec_data, ward_2016, by = "ward_id")
elec_data <- as.data.frame(elec_data)

  # transform to sf object 
elec_data <- st_as_sf(elec_data)


  # now find intersection of 2006 LGE ward centroids with 2016 ward boundaries
pnts <- pnts_sf %>% mutate(
  intersection = as.integer(st_intersects(geometry, elec_data)),
  area = if_else(is.na(intersection), '', elec_data$ward_id[intersection])
)

pnts <- pnts %>% 
  rename(ward_id = area)

pnts <- as.data.frame(pnts)
pnts <- pnts %>% 
  select(ward_id)



# CREATE DUMMY VARIABLE FOR ANY EXPOSED UNITS ------------------
elec_data_2 <- elec_data %>% 
  st_drop_geometry()

elec_data_2 <- elec_data_2 %>% 
  mutate(treat = ifelse(ward_id %in% pnts$ward_id, 1, 0))

  # check to make sure this worked properly: 
length(which(elec_data_2$treat==1)) ###17 woohoo!


# DUMMY VARIABLES FOR ALTERNATIVE EXPOSURE SPECIFICATIONS ------------------
  #now create dummy variables for the other exposure variables 
elec_data_2 <- elec_data_2 %>% 
  mutate(treat14 = 0, 
         treat30 = 0,
         treat60 = 0, 
         treat90 = 0)


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

data_2011_with_assassinations_merged <- left_join(elec_data_2, hhi2, by = c("local_municipality_id", "ward_id"))

data_2011_with_assassinations_merged <- data_2011_with_assassinations_merged %>% 
  mutate(electoral_cycle = as.factor(2011))





# WRITE OUT DATA TO PROCESSED_DATA FILE
saveRDS(data_2011_with_assassinations_merged, here("data", "processed_data", "data_2011_with_assassinations_merged.rds"))


