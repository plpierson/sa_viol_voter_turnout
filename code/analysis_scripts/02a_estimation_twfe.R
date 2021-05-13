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


##############################
####### MAIN RESULTS #########
##############################

##### DV1: WARD-LEVEL TURNOUT USING TWO-WAY FIXED EFFECTS #####
###############################################################
###############################################################
mod1 <- plm(pct_turnout ~ treat,
            data = dat,
            index = c("ward_id", "electoral_cycle"),
            model = "within",
            effect = "twoways")

mod2 <- plm(pct_turnout ~ treat + unemploy_rate + prop_black + prop_informal + prop_english,
            data = dat,
            index = c("ward_id", "electoral_cycle"),
            model = "within",
            effect = "twoways")

mod3 <- plm(pct_turnout ~ treat90,
            data = dat, 
            index = c("ward_id", "electoral_cycle"),
            model = "within",
            effect= "twoways")

mod4 <- plm(pct_turnout ~ treat90 + unemploy_rate + prop_black + prop_informal + prop_english,
            data = dat, 
            index = c("ward_id", "electoral_cycle"),
            model = "within",
            effect= "twoways")

mod5 <- plm(pct_turnout ~ treat30,
            data = dat, 
            index = c("ward_id", "electoral_cycle"),
            model = "within",
            effect = "twoways")

mod6 <- plm(pct_turnout ~ treat30 + unemploy_rate + prop_black + prop_informal + prop_english,
            data = dat, 
            index = c("ward_id", "electoral_cycle"),
            model = "within",
            effect = "twoways")


  # calculate robust standard errors
cov1 <- vcovHC(mod1, type = "HC1")
robust_se1 <- sqrt(diag(cov1)) #1.322504
coeftest(mod1, vcov = vcovHC(mod1, type = "HC1", cluster = "group")) #1.3225
coeftest(mod1, vcov = vcovHC, type = "HC1") #1.3225
coeftest(mod1, function(x) vcovHC(x, type = 'sss')) #1.3226 (this uses Stata's small-sample correction)



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


########## DV2: HHI INDEX USING TWO-WAY FIXED EFFECTS #########
###############################################################
###############################################################
mod7 <- plm(hhi_index ~ treat,
            data = dat,
            index = c("ward_id", "electoral_cycle"),
            model = "within",
            effect = "twoways")

mod8 <- plm(hhi_index ~ treat + unemploy_rate + prop_black + prop_informal + prop_english,
            data = dat,
            index = c("ward_id", "electoral_cycle"),
            model = "within",
            effect = "twoways")


mod9 <- plm(hhi_index ~ treat90,
            data = dat, 
            index = c("ward_id", "electoral_cycle"),
            model = "within",
            effect= "twoways")

mod10 <- plm(hhi_index ~ treat90 + unemploy_rate + prop_black + prop_informal + prop_english,
             data = dat, 
             index = c("ward_id", "electoral_cycle"),
             model = "within",
             effect= "twoways")

mod11 <- plm(hhi_index ~ treat30,
             data = dat, 
             index = c("ward_id", "electoral_cycle"),
             model = "within",
             effect = "twoways")

mod12 <- plm(hhi_index ~ treat30 + unemploy_rate + prop_black + prop_informal + prop_english,
             data = dat, 
             index = c("ward_id", "electoral_cycle"),
             model = "within",
             effect = "twoways")


#calculate robust standard errors
cov7 <- vcovHC(mod7, type = "HC1")
robust_se7 <- sqrt(diag(cov7))

cov8 <- vcovHC(mod8, type = "HC1")
robust_se8 <- sqrt(diag(cov8))

cov9 <- vcovHC(mod9, type = "HC1")
robust_se9 <- sqrt(diag(cov9))

cov10 <- vcovHC(mod10, type = "HC1")
robust_se10 <- sqrt(diag(cov10))

cov11 <- vcovHC(mod11, type = "HC1")
robust_se11 <- sqrt(diag(cov11))

cov12 <- vcovHC(mod12, type = "HC1")
robust_se12 <- sqrt(diag(cov12))


#output results
stargazer(mod7, mod9, mod11, mod8, mod10, mod12,
          covariate.labels = c("Any Exposure", "90 Day Window", "30 Day Window"),
          dep.var.caption = "Dependent Variable",
          dep.var.labels = c("Change in HHI"),
          se = list(robust_se7, robust_se9, robust_se11, robust_se8, robust_se10, robust_se12),
          add.lines = list(c("Unit FE", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                           c("Electoral Cycle FE", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                           c("Controls", "No", "No", "No", "Yes", "Yes", "Yes"),
                           c("Cluster-Robust SE", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"))
)



###############################################
###############################################
############## ROBUSTNESS CHECKS ##############
###############################################
###############################################


#### RCHECK 1: LOGGED OUTCOME VARIABLE


###############################################################
###############################################################
##### DV1: WARD-LEVEL TURNOUT USING TWO-WAY FIXED EFFECTS #####
###############################################################
###############################################################
dat_turn <- dat %>% 
  filter(!is.na(pct_turnout)) %>% #22 NA
  filter(pct_turnout != 0) #5 with pct_turnout==0...presumably incorrect data entry

mod19 <- plm(log(pct_turnout) ~ treat,
             data = dat_turn,
             index = c("ward_id", "electoral_cycle"),
             model = "within",
             effect = "twoways")

mod20 <- plm(log(pct_turnout) ~ treat + unemploy_rate + prop_black + prop_informal + prop_english,
             data = dat_turn,
             index = c("ward_id", "electoral_cycle"),
             model = "within",
             effect = "twoways")


mod21 <- plm(log(pct_turnout) ~ treat90,
             data = dat_turn, 
             index = c("ward_id", "electoral_cycle"),
             model = "within",
             effect= "twoways")

mod22 <- plm(log(pct_turnout) ~ treat90 + unemploy_rate + prop_black + prop_informal + prop_english,
             data = dat_turn, 
             index = c("ward_id", "electoral_cycle"),
             model = "within",
             effect= "twoways")



mod23 <- plm(log(pct_turnout) ~ treat30,
             data = dat_turn, 
             index = c("ward_id", "electoral_cycle"),
             model = "within",
             effect = "twoways")

mod24 <- plm(log(pct_turnout) ~ treat30 + unemploy_rate + prop_black + prop_informal + prop_english,
             data = dat_turn, 
             index = c("ward_id", "electoral_cycle"),
             model = "within",
             effect = "twoways")


#calculate robust standard errors
log1 <- vcovHC(mod19, type = "HC1")
log_se1 <- sqrt(diag(log1))

log2 <- vcovHC(mod20, type = "HC1")
log_se2 <- sqrt(diag(log2))

log3 <- vcovHC(mod21, type = "HC1")
log_se3 <- sqrt(diag(log3))

log4 <- vcovHC(mod22, type = "HC1")
log_se4 <- sqrt(diag(log4))

log5 <- vcovHC(mod23, type = "HC1")
log_se5 <- sqrt(diag(log5))

log6 <- vcovHC(mod24, type = "HC1")
log_se6 <- sqrt(diag(log6))


#output results
stargazer(mod19, mod21, mod23, mod20, mod22, mod24,
          covariate.labels = c("Any Exposure", "90 Day Window", "30 Day Window"),
          dep.var.caption = "Dependent Variable",
          dep.var.labels = c("Percent Change in Turnout"),
          se = list(log_se1, log_se3, log_se5, log_se2, log_se4, log_se6),
          add.lines = list(c("Unit FE", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                           c("Electoral Cycle FE", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                           c("Controls", "No", "No", "No", "Yes", "Yes", "Yes"),
                           c("Cluster-Robust SE", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"))
)


###############################################################
###############################################################
############ DV2: HHI USING TWO-WAY FIXED EFFECTS #############
###############################################################
###############################################################
dat_hhi <- dat %>% 
  filter(!is.na(hhi_index)) %>% #27 observations
  filter(hhi_index != 0) #3 observations

mod25 <- plm(log(hhi_index) ~ treat,
             data = dat_hhi,
             index = c("ward_id", "electoral_cycle"),
             model = "within",
             effect = "twoways")

mod26 <- plm(log(hhi_index) ~ treat + unemploy_rate + prop_black + prop_informal + prop_english,
             data = dat_hhi,
             index = c("ward_id", "electoral_cycle"),
             model = "within",
             effect = "twoways")


mod27 <- plm(log(hhi_index) ~ treat90,
             data = dat_hhi, 
             index = c("ward_id", "electoral_cycle"),
             model = "within",
             effect= "twoways")

mod28 <- plm(log(hhi_index) ~ treat90 + unemploy_rate + prop_black + prop_informal + prop_english,
             data = dat_hhi, 
             index = c("ward_id", "electoral_cycle"),
             model = "within",
             effect= "twoways")

mod29 <- plm(log(hhi_index) ~ treat30,
             data = dat_hhi, 
             index = c("ward_id", "electoral_cycle"),
             model = "within",
             effect = "twoways")

mod30 <- plm(log(hhi_index) ~ treat30 + unemploy_rate + prop_black + prop_informal + prop_english,
             data = dat_hhi, 
             index = c("ward_id", "electoral_cycle"),
             model = "within",
             effect = "twoways")


#calculate robust standard errors
log7 <- vcovHC(mod25, type = "HC1")
log_se7 <- sqrt(diag(log7))

log8 <- vcovHC(mod26, type = "HC1")
log_se8 <- sqrt(diag(log8))

log9 <- vcovHC(mod27, type = "HC1")
log_se9 <- sqrt(diag(log9))

log10 <- vcovHC(mod28, type = "HC1")
log_se10 <- sqrt(diag(log10))

log11 <- vcovHC(mod29, type = "HC1")
log_se11 <- sqrt(diag(log11))

log12 <- vcovHC(mod30, type = "HC1")
log_se12 <- sqrt(diag(log12))


#output results
stargazer(mod25, mod27, mod29, mod26, mod28, mod30,
          covariate.labels = c("Any Exposure", "90 Day Window", "30 Day Window"),
          dep.var.caption = "Dependent Variable",
          dep.var.labels = c("Hirschman-Herfindahl Index"),
          se = list(log_se7, log_se9, log_se11, log_se8, log_se10, log_se12),
          add.lines = list(c("Unit FE", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                           c("Electoral Cycle FE", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                           c("Controls", "No", "No", "No", "Yes", "Yes", "Yes"),
                           c("Cluster-Robust SE", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"))
)






######################################################
############ RCHECK2: WEIGHTED FIXED EFFECTS ##########
######################################################

  #DV1: turnout

weight1 <- wfe(pct_turnout ~ treat, dat, treat = "treat", unit.index = "ward_id",
               method = "unit", qoi = "ate", verbose = TRUE)

weight2 <- wfe(pct_turnout ~ treat + unemploy_rate + prop_black + prop_english + prop_informal, dat, treat = "treat", unit.index = "ward_id",
               method = "unit", qoi = "ate", verbose = TRUE)

weight3 <- wfe(pct_turnout ~ treat90, dat, treat = "treat90", unit.index = "ward_id",
               method = "unit", qoi = "ate", verbose = TRUE)

weight4 <- wfe(pct_turnout ~ treat90 + unemploy_rate + prop_black + prop_english + prop_informal, dat, treat = "treat90", unit.index = "ward_id",
               method = "unit", qoi = "ate", verbose = TRUE)

weight5 <- wfe(pct_turnout ~ treat30, dat, treat = "treat30", unit.index = "ward_id",
               method = "unit", qoi = "ate", verbose = TRUE)

weight6 <- wfe(pct_turnout ~ treat30 + unemploy_rate + prop_black + prop_english + prop_informal, dat, treat = "treat30", unit.index = "ward_id",
               method = "unit", qoi = "ate", verbose = TRUE)


  # wfe objects don't play nice with stargazer, so need to extract relevant values
val.weight1 <- weight1[c(1, 7, 19)]
val.weight2 <- weight2[c(1, 7, 19)]
val.weight3 <- weight3[c(1, 7, 19)]
val.weight4 <- weight4[c(1, 7, 19)]
val.weight5 <- weight5[c(1, 7, 19)]
val.weight6 <- weight6[c(1, 7, 19)]



#DV2: HHI
weight7 <- wfe(hhi_index ~ treat, dat, treat = "treat", unit.index = "ward_id",
               method = "unit", qoi = "ate", verbose = TRUE)

weight8 <- wfe(hhi_index ~ treat + unemploy_rate + prop_black + prop_english + prop_informal, dat, treat = "treat", unit.index = "ward_id",
               method = "unit", qoi = "ate", verbose = TRUE)

weight9 <- wfe(hhi_index ~ treat90, dat, treat = "treat90", unit.index = "ward_id",
               method = "unit", qoi = "ate", verbose = TRUE)

weight10 <- wfe(hhi_index ~ treat90 + unemploy_rate + prop_black + prop_english + prop_informal, dat, treat = "treat90", unit.index = "ward_id",
                method = "unit", qoi = "ate", verbose = TRUE)

weight11 <- wfe(hhi_index ~ treat30, dat, treat = "treat30", unit.index = "ward_id",
                method = "unit", qoi = "ate", verbose = TRUE)

weight12 <- wfe(hhi_index ~ treat30 + unemploy_rate + prop_black + prop_english + prop_informal, dat, treat = "treat30", unit.index = "ward_id",
                method = "unit", qoi = "ate", verbose = TRUE)


val.weight7 <- weight7[c(1, 7, 19)]
val.weight8 <- weight8[c(1, 7, 19)]
val.weight9 <- weight9[c(1, 7, 19)]
val.weight10 <- weight10[c(1, 7, 19)]
val.weight11 <- weight11[c(1, 7, 19)]
val.weight12 <- weight12[c(1, 7, 19)]




#################################################
############# RCHECK 3: RANDOM EFFECTS ############
#################################################

ran1 <- plm(pct_turnout ~ treat,
            data = dat,
            index = c("ward_id", "electoral_cycle"),
            model = "random",
            effect = "twoways")

ran2 <- plm(pct_turnout ~ treat90,
            data = dat, 
            index = c("ward_id", "electoral_cycle"),
            model = "random",
            effect= "twoways")

ran3 <- plm(pct_turnout ~ treat30,
            data = dat, 
            index = c("ward_id", "electoral_cycle"),
            model = "random",
            effect = "twoways")

ran4 <- plm(hhi_index ~ treat,
            data = dat,
            index = c("ward_id", "electoral_cycle"),
            model = "random",
            effect = "twoways")

ran5 <- plm(hhi_index ~ treat90,
            data = dat, 
            index = c("ward_id", "electoral_cycle"),
            model = "random",
            effect= "twoways")

ran6 <- plm(hhi_index ~ treat30,
            data = dat, 
            index = c("ward_id", "electoral_cycle"),
            model = "random",
            effect = "twoways")


#calculate robust standard errors
random1 <- vcovHC(ran1, type = "HC1")
ran_se1 <- sqrt(diag(random1))

random2 <- vcovHC(ran2, type = "HC1")
ran_se2 <- sqrt(diag(random2))

random3 <- vcovHC(ran3, type = "HC1")
ran_se3 <- sqrt(diag(random3))

random4 <- vcovHC(ran4, type = "HC1")
ran_se4 <- sqrt(diag(random4))

random5 <- vcovHC(ran5, type = "HC1")
ran_se5 <- sqrt(diag(random5))

random6 <- vcovHC(ran6, type = "HC1")
ran_se6 <- sqrt(diag(random6))


#output results
stargazer(ran1, ran2, ran3, ran4, ran5, ran6,
          covariate.labels = c("Any Exposure", "90 Day Window", "30 Day Window"),
          dep.var.caption = "Dependent Variable",
          dep.var.labels = c("Percent Change in Turnout", "HHI"),
          se = list(ran_se1, ran_se2, ran_se3, ran_se4, ran_se5, ran_se6),
          add.lines = list(c("Unit FE", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                           c("Electoral Cycle FE", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                           c("Cluster-Robust SE", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"))
)





#################################################
############# RCHECK 5: MATCHING ################
#################################################

  #missing values not allowed in matchit function
length(which(is.na(dat$unemploy_rate))) #0
length(which(is.na(dat$prop_black))) #0
length(which(is.na(dat$prop_english))) #0
length(which(is.na(dat$prop_informal))) #2
dat.balance <- dat %>% 
  filter(!is.na(prop_informal))

# NEAREST NEIGHBOR MATCHING from the MatchIt package
bal.cem <- matchit(treat ~ unemploy_rate + prop_black + prop_english + prop_informal, #just like a call to lm
                       data = dat.balance, #dataset
                       method = "exact", #form of matching you'd like to use
                       estimand = "ATT",
                       replace = FALSE,
                       ratio = 1) #see p14 of cran documentation for discussion of different estimands


  # in the code below I am just creating a little data frame to clean up the variable names in the subsequent plot
n <- data.frame(old = c("distance", "unemploy_rate", "prop_black", "prop_english", "prop_informal"),
                new = c("Distance", "Unemployment Rate", "Prop. Black African", "Prop. English First Language", "Prop. Informal Settlements"))

  #visualization 1
love_plot <- love.plot(bal.nearest, #call a MatchIt object (see line 7 above)
          var.order = "unadjusted",
          abs = TRUE,
          thresholds = c(m = 0.1),
          colors = c("forestgreen", "black"),
          position = "top",
          var.names = n,
          title = "Covariate Balance - Nearest Neighbor")


  #visualization 2
bal.plot.nearest <- bal.plot(bal.cem,
                             var.name = "unemploy_rate",
                             which = "both",
                             colors = c("forestgreen", "black"),
                             position = "top",
                             type = "histogram", 
                             mirror = TRUE)

  #add title, caption, labels, etc.
bal.plot.nearest + 
  labs(title = "Covariate Balance for Unemployment Rate",
       caption = "Distributional balance for unemployment rate using nearest neighbor matching",
       x = "Proportion Unemployed")



  #re-run main results using matched data
library(sandwich)
library(lmtest)
library(clubSandwich)

match_data <- match.data(bal.cem)

m.plm1 <- plm(pct_turnout ~ treat,
            data = match_data,
            weights = weights,
            index = c("ward_id", "electoral_cycle"),
            model = "within",
            effect = "twoways")

match_se1 <- coeftest(m.plm1, vcovCR = "CR2", cluster = match_data$ward_id)[1,2]



m.plm2 <- plm(pct_turnout ~ treat + unemploy_rate + prop_black + prop_english + prop_informal,
             data = match_data,
             weights = weights,
             index = c("ward_id", "electoral_cycle"),
             model = "within",
             effect = "twoways")

match_se2 <- coeftest(m.plm2, vcovCR = "CR2", cluster = match_data$ward_id)[1,2]


m.plm3 <- plm(hhi_index ~ treat,
              data = match_data,
              weights = weights,
              index = c("ward_id", "electoral_cycle"),
              model = "within",
              effect = "twoways")

match_se3 <- coeftest(m.plm3, vcovCR = "CR2", cluster = match_data$ward_id)[1,2]



m.plm4 <- plm(hhi_index ~ treat + unemploy_rate + prop_black + prop_english + prop_informal,
              data = match_data,
              weights = weights,
              index = c("ward_id", "electoral_cycle"),
              model = "within",
              effect = "twoways")

match_se4 <- coeftest(m.plm4, vcovCR = "CR2", cluster = match_data$ward_id)[1,2]


# output results
stargazer(m.plm1, m.plm2, m.plm3, m.plm4,
          covariate.labels = c("Any Exposure"),
          dep.var.caption = "Dependent Variable",
          dep.var.labels = c("Change in Turnout", "Change in HHI"),
          se = list(match_se1, match_se2, match_se3, match_se4),
          add.lines = list(c("Unit FE", "Yes", "Yes", "Yes", "Yes"),
                           c("Electoral Cycle FE", "Yes", "Yes", "Yes", "Yes"),
                           c("Controls", "No", "Yes", "No", "Yes"),
                           c("Cluster-Robust SE", "Yes", "Yes", "Yes", "Yes"))
)





#################################################
############# RCHECK 6: ONLY KZN ################
#################################################

##### DV1: WARD-LEVEL TURNOUT USING TWO-WAY FIXED EFFECTS #####
###############################################################
###############################################################
dat$province_id <- as.factor(dat$province_id)
dat_kzn <- dat %>% 
  filter(province_id == "KZN")


kzn1 <- plm(pct_turnout ~ treat,
            data = dat_kzn,
            index = c("ward_id", "electoral_cycle"),
            model = "within",
            effect = "twoways")

kzn2 <- plm(pct_turnout ~ treat + unemploy_rate + prop_black + prop_informal + prop_english,
            data = dat_kzn,
            index = c("ward_id", "electoral_cycle"),
            model = "within",
            effect = "twoways")

kzn3 <- plm(pct_turnout ~ treat90,
            data = dat_kzn, 
            index = c("ward_id", "electoral_cycle"),
            model = "within",
            effect= "twoways")

kzn4 <- plm(pct_turnout ~ treat90 + unemploy_rate + prop_black + prop_informal + prop_english,
            data = dat_kzn, 
            index = c("ward_id", "electoral_cycle"),
            model = "within",
            effect= "twoways")

kzn5 <- plm(pct_turnout ~ treat30,
            data = dat_kzn, 
            index = c("ward_id", "electoral_cycle"),
            model = "within",
            effect = "twoways")

kzn6 <- plm(pct_turnout ~ treat30 + unemploy_rate + prop_black + prop_informal + prop_english,
            data = dat_kzn, 
            index = c("ward_id", "electoral_cycle"),
            model = "within",
            effect = "twoways")


# calculate robust standard errors
kzn_cov1 <- vcovHC(kzn1, type = "HC1")
kzn_robust_se1 <- sqrt(diag(kzn_cov1)) #1.322504

kzn_cov2 <- vcovHC(kzn2, type = "HC1")
kzn_robust_se2 <- sqrt(diag(kzn_cov2))

kzn_cov3 <- vcovHC(kzn3, type = "HC1")
kzn_robust_se3 <- sqrt(diag(kzn_cov3))

kzn_cov4 <- vcovHC(kzn4, type = "HC1")
kzn_robust_se4 <- sqrt(diag(kzn_cov4))

kzn_cov5 <- vcovHC(kzn5, type = "HC1")
kzn_robust_se5 <- sqrt(diag(kzn_cov5))

kzn_cov6 <- vcovHC(kzn6, type = "HC1")
kzn_robust_se6 <- sqrt(diag(kzn_cov6))


# output results
stargazer(kzn1, kzn3, kzn5, kzn2, kzn4, kzn6,
          covariate.labels = c("Any Exposure", "90 Day Window", "30 Day Window"),
          dep.var.caption = "Dependent Variable",
          dep.var.labels = c("Percent Change in Turnout"),
          se = list(kzn_robust_se1, kzn_robust_se3, kzn_robust_se5, kzn_robust_se2, kzn_robust_se4, kzn_robust_se6),
          add.lines = list(c("Unit FE", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                           c("Electoral Cycle FE", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                           c("Controls", "No", "No", "No", "Yes", "Yes", "Yes"),
                           c("Cluster-Robust SE", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"))
)


########## DV2: HHI INDEX USING TWO-WAY FIXED EFFECTS #########
###############################################################
###############################################################
kzn7 <- plm(hhi_index ~ treat,
            data = dat_kzn,
            index = c("ward_id", "electoral_cycle"),
            model = "within",
            effect = "twoways")

kzn8 <- plm(hhi_index ~ treat + unemploy_rate + prop_black + prop_informal + prop_english,
            data = dat_kzn,
            index = c("ward_id", "electoral_cycle"),
            model = "within",
            effect = "twoways")


kzn9 <- plm(hhi_index ~ treat90,
            data = dat_kzn, 
            index = c("ward_id", "electoral_cycle"),
            model = "within",
            effect= "twoways")

kzn10 <- plm(hhi_index ~ treat90 + unemploy_rate + prop_black + prop_informal + prop_english,
             data = dat_kzn, 
             index = c("ward_id", "electoral_cycle"),
             model = "within",
             effect= "twoways")

kzn11 <- plm(hhi_index ~ treat30,
             data = dat_kzn, 
             index = c("ward_id", "electoral_cycle"),
             model = "within",
             effect = "twoways")

kzn12 <- plm(hhi_index ~ treat30 + unemploy_rate + prop_black + prop_informal + prop_english,
             data = dat_kzn, 
             index = c("ward_id", "electoral_cycle"),
             model = "within",
             effect = "twoways")


#calculate robust standard errors
kzn_cov7 <- vcovHC(kzn7, type = "HC1")
kzn_robust_se7 <- sqrt(diag(kzn_cov7))

kzn_cov8 <- vcovHC(kzn8, type = "HC1")
kzn_robust_se8 <- sqrt(diag(kzn_cov8))

kzn_cov9 <- vcovHC(kzn9, type = "HC1")
kzn_robust_se9 <- sqrt(diag(kzn_cov9))

kzn_cov10 <- vcovHC(kzn10, type = "HC1")
kzn_robust_se10 <- sqrt(diag(kzn_cov10))

kzn_cov11 <- vcovHC(kzn11, type = "HC1")
kzn_robust_se11 <- sqrt(diag(kzn_cov11))

kzn_cov12 <- vcovHC(kzn12, type = "HC1")
kzn_robust_se12 <- sqrt(diag(kzn_cov12))


#output results
stargazer(kzn7, kzn9, kzn11, kzn8, kzn10, kzn12,
          covariate.labels = c("Any Exposure", "90 Day Window", "30 Day Window"),
          dep.var.caption = "Dependent Variable",
          dep.var.labels = c("Change in HHI"),
          se = list(kzn_robust_se7, kzn_robust_se9, kzn_robust_se11, kzn_robust_se8, kzn_robust_se10, kzn_robust_se12),
          add.lines = list(c("Unit FE", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                           c("Electoral Cycle FE", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                           c("Controls", "No", "No", "No", "Yes", "Yes", "Yes"),
                           c("Cluster-Robust SE", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"))
)

