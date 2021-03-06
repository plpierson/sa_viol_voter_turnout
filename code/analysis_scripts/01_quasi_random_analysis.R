rm(list=ls())

library(tidyverse)
library(here)
library(stargazer)
library(lfe)
library(plm)
library(wfe)
library(PanelMatch)
library(MatchIt)
library(cobalt)


# IMPORT DATA --------------------------
dat <- readRDS(here("data", "processed_data", "dat_asif.rds"))


##############################
####### MAIN RESULTS #########
##############################

########## DV1: TURNOUT USING AS-IF RANDOM ATTEMPTS ###########
###############################################################
###############################################################


  # base models
lm1 <- lm(pct_turnout ~ success, data = dat)
lm2 <- lm(hhi_index ~ success, data = dat)

lm3 <- lm(pct_turnout ~ success + unemploy_rate + prop_informal + prop_black + prop_english + province_id, data = dat)
lm4 <- lm(hhi_index ~ success + unemploy_rate + prop_informal + prop_black + prop_english + province_id, data = dat)

  # calculate standard errors
cov1 <- vcovHC(lm1, type = "HC1")
robust_se1 <- sqrt(diag(cov1))

cov2 <- vcovHC(lm2, type = "HC1")
robust_se2 <- sqrt(diag(cov2))

cov3 <- vcovHC(lm3, type = "HC1")
robust_se3 <- sqrt(diag(cov3))

cov4 <- vcovHC(lm4, type = "HC1")
robust_se4 <- sqrt(diag(cov4))

  # output results
stargazer(lm1, lm2, lm3, lm4,
          covariate.labels = c("Successful Asssassination"),
          dep.var.caption = "Dependent Variable",
          dep.var.labels = c("Percent Change in Turnout", "Change in HHI Index"),
          se = list(robust_se1, robust_se2, robust_se3, robust_se4),
          add.lines = list(c("Robust SE", "Yes", "Yes", "Yes", "Yes"),
                           c("Controls", "No", "No", "Yes", "Yes"),
                           c("No. Observations", "54", "54", "54", "54"))
)



# CHECK FOR BALANCE -----------------------------
failed <- dat %>% 
  filter(failed==1)

success <- dat %>% 
  filter(success==1)


t.test(failed$unemploy_rate, success$unemploy_rate)
t.test(failed$prop_informal, success$prop_informal)
t.test(failed$prop_black, success$prop_black)
t.test(failed$prop_english, success$prop_english)
t.test(failed$hhi_index, success$hhi_index)
t.test(failed$registered_voters, success$registered_voters)


  # conduct randomization inference for p-values
install.packages("ri2")
library(ri2)


  # first, declare randomization procedure
declare <- declare_ra(N = 54, m = 38)

# randomization inference for unemployment rate
 
  # set-up data to work nicely with ri2 package
data_unemploy <- dat
data_unemploy <- data_unemploy %>% 
  rename(Z = success,
         Y = unemploy_rate)


ri_unemploy <- conduct_ri(
  formula = Y ~ Z,
  declaration = declare,
  sharp_hypothesis = 0,
  data = data_unemploy
)

summary(ri_unemploy)


# randomization inference for prop_informal

# set-up data to work nicely with ri2 package
data_informal <- dat
data_informal <- data_informal %>% 
  rename(Z = success,
         Y = prop_informal)


ri_informal <- conduct_ri(
  formula = Y ~ Z,
  declaration = declare,
  sharp_hypothesis = 0,
  data = data_informal
)

summary(ri_informal)


# randomization inference for prop_black

# set-up data to work nicely with ri2 package
data_black <- dat
data_black <- data_black %>% 
  rename(Z = success,
         Y = prop_black)


ri_black <- conduct_ri(
  formula = Y ~ Z,
  declaration = declare,
  sharp_hypothesis = 0,
  data = data_black
)

summary(ri_black)



# randomization inference for prop_english

# set-up data to work nicely with ri2 package
data_english <- dat
data_english <- data_english %>% 
  rename(Z = success,
         Y = prop_english)


ri_english <- conduct_ri(
  formula = Y ~ Z,
  declaration = declare,
  sharp_hypothesis = 0,
  data = data_english
)

summary(ri_english)


# randomization inference for hhi_index

# set-up data to work nicely with ri2 package
data_hhi <- dat
data_hhi <- data_hhi %>% 
  rename(Z = success,
         Y = hhi_index)


ri_hhi <- conduct_ri(
  formula = Y ~ Z,
  declaration = declare,
  sharp_hypothesis = 0,
  data = data_hhi
)

summary(ri_hhi)


# randomization inference for registered_voters

# set-up data to work nicely with ri2 package
data_voters <- dat
data_voters <- data_voters %>% 
  rename(Z = success,
         Y = registered_voters)


ri_voters <- conduct_ri(
  formula = Y ~ Z,
  declaration = declare,
  sharp_hypothesis = 0,
  data = data_voters
)

summary(ri_voters)







# READ IN ASSASSINATIONS DATA TO COMPARE TARGET_AGE BETWEEN TREATMENT AND CONTROL
library(readxl)
library(lubridate)
library(numform)

viol <- read_excel(here("data", "raw_data", "assassinations_data", "za_assassinations copy.xlsx"))

# Clean/organize assassinations data
#viol$date_of_attack <- as.Date(as.numeric(viol$date_of_attack), origin = "1899-12-30")
viol$year <- year(viol$date_of_attack)
viol$month <- format(viol$date_of_attack, "%m")
viol$month <- f_num(viol$month, digits=0)
viol$sitting <- as.numeric(viol$sitting)
viol$electoral_cycle <- as.integer(viol$electoral_cycle)

# filter to look exclusively at attacks on LC/MC ward politicians and/or candidates
viol <- viol %>% 
  filter(seat_type=="LC Ward" | seat_type=="MC Ward" | seat_type=="MC Ward candidate" | seat_type=="LC Ward candidate") %>% 
  filter(date_of_attack>="2000-12-05" & date_of_attack<="2016-08-03") # needs to be dropped until 2021 LGE are held in South Africa


viol$age <- as.numeric(viol$age)

failed_viol <- viol %>% 
  filter(failed==1)

success_viol <- viol %>% 
  filter(failed!=1)

t.test(failed_viol$age, success_viol$age)

# randomization inference for age

# set-up data to work nicely with ri2 package
data_age <- viol
data_age <- data_age %>% 
  mutate(success = ifelse(failed==1, 0, 1)) %>% 
  filter(!is.na(age))

data_age <- data_age %>% 
  rename(Z = success,
         Y = age)

declare_age <- declare_ra(N = 50, m = 37)

ri_age <- conduct_ri(
  formula = Y ~ Z,
  declaration = declare_age,
  sharp_hypothesis = 0,
  data = data_age
)

summary(ri_age)




# ROBUSTNESS CHECK ----------------------------
  # drop ward candidates and only look at sitting officials

dat_robust <- dat %>% 
  mutate(candidate = replace_na(candidate, 0)) %>% 
  filter(candidate!=1)


# base models
rb1 <- lm(pct_turnout ~ success, data = dat_robust)
rb2 <- lm(hhi_index ~ success, data = dat_robust)

rb3 <- lm(pct_turnout ~ success + unemploy_rate + prop_informal + prop_black + prop_english + province_id, data = dat_robust)
rb4 <- lm(hhi_index ~ success + unemploy_rate + prop_informal + prop_black + prop_english + province_id, data = dat_robust)

# calculate standard errors
rb.cov1 <- vcovHC(rb1, type = "HC1")
rb.robust_se1 <- sqrt(diag(rb.cov1))

rb.cov2 <- vcovHC(rb2, type = "HC1")
rb.robust_se2 <- sqrt(diag(rb.cov2))

rb.cov3 <- vcovHC(rb3, type = "HC1")
rb.robust_se3 <- sqrt(diag(rb.cov3))

rb.cov4 <- vcovHC(rb4, type = "HC1")
rb.robust_se4 <- sqrt(diag(rb.cov4))

# output results
stargazer(rb1, rb2, rb3, rb4,
          covariate.labels = c("Successful Asssassination"),
          dep.var.caption = "Dependent Variable",
          dep.var.labels = c("Percent Change in Turnout", "Change in HHI Index"),
          se = list(rb.robust_se1, rb.robust_se2, rb.robust_se3, rb.robust_se4),
          add.lines = list(c("Robust SE", "Yes", "Yes", "Yes", "Yes"),
                           c("Controls", "No", "No", "Yes", "Yes")
                           )
)








