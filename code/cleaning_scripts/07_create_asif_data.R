rm(list=ls())

library(tidyverse)
library(here)

options(scipen = 999)


# IMPORT PANEL DATA -------------------
dat <- readRDS(here("data", "processed_data", "panel_data_sa.rds"))


  #identify treated units
dat_treat <- dat %>% 
  filter(treat==1) %>% 
  select(ward_id, local_municipality_id, electoral_cycle)


viol_ward <- viol_ward %>% 
  relocate(date_of_attack, .after=ward_id) %>% 
  relocate(failed, .after=date_of_attack) %>% 
  arrange(date_of_attack)


  # distinguish attempts (failed) vs. attempts (successful) for quasi-random analysis
dat <- dat %>% 
  mutate(failed = case_when(
    ward_id=="59500075" & electoral_cycle==2006 ~ 1,
    ward_id=="74804009" & electoral_cycle==2006 ~ 1,
    ward_id=="19100036" & electoral_cycle==2006 ~ 1,
    ward_id=="19100035" & electoral_cycle==2006 ~ 1,
    ward_id=="19100075" & electoral_cycle==2006 ~ 1,
    ward_id=="52308008" & electoral_cycle==2006 ~ 1,
    ward_id=="52405005" & electoral_cycle==2011 ~ 1,
    ward_id=="19100086" & electoral_cycle==2011 ~ 1,
    ward_id=="74804007" & electoral_cycle==2011 ~ 1,
    ward_id=="29200025" & electoral_cycle==2011 ~ 1,
    ward_id=="52308015" & electoral_cycle==2016 ~ 1,
    ward_id=="94702010" & electoral_cycle==2016 ~ 1,
    ward_id=="52502012" & electoral_cycle==2016 ~ 1,
    ward_id=="79800058" & electoral_cycle==2016 ~ 1,
    ward_id=="29300015" & electoral_cycle==2016 ~ 1,
    ward_id=="52307019" & electoral_cycle==2016 ~ 1)
  )


  # flag candidates vs. sitting officials
dat <- dat %>% 
  mutate(candidate = case_when(
    ward_id=="74804009" & electoral_cycle==2006 ~1, #merafong city
    ward_id=="52201011" & electoral_cycle==2006 ~1, #umshwathi
    ward_id=="29200025" & electoral_cycle==2011 ~1,
    ward_id=="29300015" & electoral_cycle==2016 ~1,
    ward_id=="52502006" & electoral_cycle==2016 ~1,
    ward_id=="52308015" & electoral_cycle==2016 ~1,
    ward_id=="52104001" & electoral_cycle==2016 ~1,
    ward_id=="52308020" & electoral_cycle==2016 ~1,
    ward_id=="29300004" & electoral_cycle==2016 ~1,
    ward_id=="79800058" & electoral_cycle==2016 ~1)
    )


dat_asif <- dat %>% 
  filter(treat==1)

dat_asif <- dat_asif%>% 
  mutate(failed = if_else(failed==1, 1, 0, missing=0)) 

#how many were successful assassination attempts?
dat_asif <- dat_asif %>% 
  mutate(success = if_else(failed==1, 0, 1))


dat_asif <- dat_asif %>% 
  relocate(candidate, .after=ward_id)


#export
saveRDS(dat_asif, "dat_asif.rds")


