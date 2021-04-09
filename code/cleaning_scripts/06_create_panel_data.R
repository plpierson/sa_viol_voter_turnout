rm(list=ls())

library(tidyverse)
library(here)

options(scipen = 999)


# IMPORT CONTROL VARIABLES -----------------
controls_2006 <- readRDS(here("data", "processed_data", "data_2006_controls.rds"))
controls_2011 <- readRDS(here("data", "processed_data", "data_2016_controls.rds"))
controls_2016 <- readRDS(here("data", "processed_data", "data_2016_controls.rds"))


# IMPORT PANELS OF ELECTORAL RETURNS AND ASSASSINATION EXPOSURE ----------------
df_2006 <- readRDS(here("data", "processed_data", "data_2006_with_assassinations_merged.rds"))
df_2011 <- readRDS(here("data", "processed_data", "data_2011_with_assassinations_merged.rds"))
df_2016 <- readRDS(here("data", "processed_data", "data_2016_with_assassinations_merged.rds"))


# JOIN
dat_2006 <- left_join(df_2006, controls_2006, by = "ward_id")
dat_2011 <- left_join(df_2011, controls_2011, by = "ward_id")
dat_2016 <- left_join(df_2016, controls_2016, by = "ward_id")

# STACK PANELS TOGETHER
dat <- do.call("bind_rows", list(dat_2006, dat_2011, dat_2016))


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

dat <- dat %>% 
  mutate(candidate = replace_na(candidate, 0))



# EXPORT
saveRDS(dat, here("data", "processed_data", "panel_data_sa.rds"))
        