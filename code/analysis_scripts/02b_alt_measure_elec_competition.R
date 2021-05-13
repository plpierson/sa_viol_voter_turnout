rm(list=ls())

library(tidyverse)
library(stargazer)
library(lfe)
library(plm)
library(wfe)
library(PanelMatch)
library(MatchIt)
library(cobalt)
library(lmtest)
library(sandwich)
library(here)

options(scipen = 999)

# IMPORT PANEL DATA --------------------------
dat <- readRDS(here("data", "processed_data", "panel_data_sa.rds"))
dat$ward_id <- as.factor(dat$ward_id)

top_vote_getters <- dat %>% 
  pivot_longer(cols = starts_with("for_"), 
               names_to = "top_n",
               values_to = "rank") %>% 
  group_by(ward_id, electoral_cycle) %>% 
  slice_max(rank, n = 2, with_ties = FALSE) %>% 
  mutate(vote_differential = abs(diff(rank))) %>% 
  select(ward_id, electoral_cycle, vote_differential) %>% 
  distinct(., .keep_all = TRUE)


dat_final <- left_join(dat, top_vote_getters, by = c("ward_id", "electoral_cycle"))


########### ESTIMATION WITH TWFE ################
mod1 <- plm(vote_differential ~ treat,
            data = dat_final,
            index = c("ward_id", "electoral_cycle"),
            model = "within",
            effect = "twoways")

mod2 <- plm(vote_differential ~ treat + unemploy_rate + prop_black + prop_informal + prop_english,
            data = dat_final,
            index = c("ward_id", "electoral_cycle"),
            model = "within",
            effect = "twoways")

mod3 <- plm(vote_differential ~ treat90,
            data = dat_final, 
            index = c("ward_id", "electoral_cycle"),
            model = "within",
            effect= "twoways")

mod4 <- plm(vote_differential ~ treat90 + unemploy_rate + prop_black + prop_informal + prop_english,
            data = dat_final, 
            index = c("ward_id", "electoral_cycle"),
            model = "within",
            effect= "twoways")

mod5 <- plm(vote_differential ~ treat30,
            data = dat_final, 
            index = c("ward_id", "electoral_cycle"),
            model = "within",
            effect = "twoways")

mod6 <- plm(vote_differential ~ treat30 + unemploy_rate + prop_black + prop_informal + prop_english,
            data = dat_final, 
            index = c("ward_id", "electoral_cycle"),
            model = "within",
            effect = "twoways")


# calculate robust standard errors
cov1 <- vcovHC(mod1, type = "HC1")
robust_se1 <- sqrt(diag(cov1)) 

cov2 <- vcovHC(mod2, type = "HC1")
robust_se2 <- sqrt(diag(cov2))

cov3 <- vcovHC(mod3, type = "HC1")
robust_se3 <- sqrt(diag(cov3))

cov4 <- vcovHC(mod4, type = "HC1")
robust_se4 <- sqrt(diag(cov4))

cov5 <- vcovHC(mod5, type = "HC1")
robust_se5 <- sqrt(diag(cov5))

cov6 <- vcovHC(mod6, type = "HC1")
robust_se6 <- sqrt(diag(cov6))


# output results
stargazer(mod1, mod3, mod5, mod2, mod4, mod6,
          covariate.labels = c("Any Exposure", "90 Day Window", "30 Day Window"),
          dep.var.caption = "Dependent Variable",
          dep.var.labels = c("Percent Change in Turnout"),
          se = list(robust_se1, robust_se3, robust_se5, robust_se2, robust_se4, robust_se6),
          add.lines = list(c("Unit FE", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                           c("Electoral Cycle FE", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                           c("Controls", "No", "No", "No", "Yes", "Yes", "Yes"),
                           c("Cluster-Robust SE", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"))
)





