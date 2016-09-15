## CAESAR study analyses, some revised analyses -- Sep 2016

library(lme4)
library(lmerTest)
library(MASS)
library(HLMdiag)
library(lattice)
library(ggplot2)
library(car)
library(splines)
library(psych)

setwd("~/psych/Graduate Research (UBC)/CAESAR Retrospective Affect/Current Analyses (with Datasets)")
summary(d1 <- read.table("Rdata_levelscollapsed_onlyincludedparticipants_12sep16.csv",header=TRUE, sep=",")) # entire dataset
summary(d1_ex <- subset(d1, d1$is_fu == 0)) # exit reports only
summary(d1_fu <- subset(d1, d1$is_fu == 1)) # follow-up reports only
summary(dp <- read.table("level2_onlyincludedparticipants.csv",header=TRUE, sep=",")) # person-level data

d1$retro_wks <- d1$retro_time * 1/7

# Grand mean centering and standardizing individual-level variables

d1$sex_C <- d1$sex - mean(na.omit(d1$sex))
d1$age_C <- d1$age - mean(na.omit(d1$age))
d1$Avt_C <- d1$Avt - mean(na.omit(d1$Avt))
d1$Lst_C <- d1$Lst - mean(na.omit(d1$Lst))
d1$edu_C <- d1$edu - mean(na.omit(d1$edu))
d1$neur_C <- d1$neur - mean(na.omit(d1$neur))
describe(d1$neur_C)
d1$neur_C_again <- d1$neur - mean(na.omit(d1_fu$neur))
describe(d1$neur_C_again)
d1$neur_C_again_again <- d1$neur - mean(na.omit(d1_ex$neur))
describe(d1$neur_C_again_again)
d1$extra_C <- d1$extra - mean(na.omit(d1$extra))
d1$consc_C <- d1$consc - mean(na.omit(d1$consc))
d1$agree_C <- d1$agree - mean(na.omit(d1$agree))
d1$open_C <- d1$open - mean(na.omit(d1$open))

d1$hana_ret_C <- d1$hana_ret - mean(na.omit(d1$hana_ret))
d1$hana_rec_C <- d1$hana_rec - mean(na.omit(d1$hana_rec))
d1$hana_max_C <- d1$hana_max - mean(na.omit(d1$hana_max))
d1$hana_mean_C <- d1$hana_bp_mean - mean(na.omit(d1$hana_bp_mean))
d1$hana_max_day_C <- d1$hana_max_day - mean(na.omit(d1$hana_max_day))

d1$lana_ret_C <- d1$lana_ret - mean(na.omit(d1$lana_ret))
d1$lana_rec_C <- d1$lana_rec - mean(na.omit(d1$lana_rec))
d1$lana_max_C <- d1$lana_max - mean(na.omit(d1$lana_max))
d1$lana_mean_C <- d1$lana_bp_mean - mean(na.omit(d1$lana_bp_mean))
d1$lana_max_day_C <- d1$lana_max_day - mean(na.omit(d1$lana_max_day))

d1$hapa_ret_C <- d1$hapa_ret - mean(na.omit(d1$hapa_ret))
d1$hapa_rec_C <- d1$hapa_rec - mean(na.omit(d1$hapa_rec))
d1$hapa_max_C <- d1$hapa_max - mean(na.omit(d1$hapa_max))
d1$hapa_mean_C <- d1$hapa_bp_mean - mean(na.omit(d1$hapa_bp_mean))
d1$hapa_max_day_C <- d1$hapa_max_day - mean(na.omit(d1$hapa_max_day))

d1$lapa_ret_C <- d1$lapa_ret - mean(na.omit(d1$lapa_ret))
d1$lapa_rec_C <- d1$lapa_rec - mean(na.omit(d1$lapa_rec))
d1$lapa_max_C <- d1$lapa_max - mean(na.omit(d1$lapa_max))
d1$lapa_mean_C <- d1$lapa_bp_mean - mean(na.omit(d1$lapa_bp_mean))
d1$lapa_max_day_C <- d1$lapa_max_day - mean(na.omit(d1$lapa_max_day))

d1$happy_ret_C <- d1$happy_ret - mean(na.omit(d1$happy_ret))
d1$happy_rec_C <- d1$happybp_rec - mean(na.omit(d1$happybp_rec))
d1$happy_max_C <- d1$happybp_max - mean(na.omit(d1$happybp_max))
d1$happy_mean_C <- d1$happybp_mean - mean(na.omit(d1$happybp_mean))

d1$quiet_ret_C <- d1$quiet_ret - mean(na.omit(d1$quiet_ret))
d1$quiet_rec_C <- d1$quietbp_rec - mean(na.omit(d1$quietbp_rec))
d1$quiet_max_C <- d1$quietbp_max - mean(na.omit(d1$quietbp_max))
d1$quiet_mean_C <- d1$quietbp_mean - mean(na.omit(d1$quietbp_mean))

d1$exctd_ret_C <- d1$exctd_ret - mean(na.omit(d1$exctd_ret))
d1$exctd_rec_C <- d1$exctdbp_rec - mean(na.omit(d1$exctdbp_rec))
d1$exctd_max_C <- d1$exctdbp_max - mean(na.omit(d1$exctdbp_max))
d1$exctd_mean_C <- d1$exctdbp_mean - mean(na.omit(d1$exctdbp_mean))

d1$calm_ret_C <- d1$calm_ret - mean(na.omit(d1$calm_ret))
d1$calm_rec_C <- d1$calmbp_rec - mean(na.omit(d1$calmbp_rec))
d1$calm_max_C <- d1$calmbp_max - mean(na.omit(d1$calmbp_max))
d1$calm_mean_C <- d1$calmbp_mean - mean(na.omit(d1$calmbp_mean))

d1$sad_ret_C <- d1$sad_ret - mean(na.omit(d1$sad_ret))
d1$sad_rec_C <- d1$sadbp_rec - mean(na.omit(d1$sadbp_rec))
d1$sad_max_C <- d1$sadbp_max - mean(na.omit(d1$sadbp_max))
d1$sad_mean_C <- d1$sadbp_mean - mean(na.omit(d1$sadbp_mean))

d1$sleepy_ret_C <- d1$sleepy_ret - mean(na.omit(d1$sleepy_ret))
d1$sleepy_rec_C <- d1$slepybp_rec - mean(na.omit(d1$slepybp_rec))
d1$sleepy_max_C <- d1$slepybp_max - mean(na.omit(d1$slepybp_max))
d1$sleepy_mean_C <- d1$slepybp_mean - mean(na.omit(d1$slepybp_mean))

d1$nrvous_ret_C <- d1$nrvous_ret - mean(na.omit(d1$nrvous_ret))
d1$nrvous_rec_C <- d1$nervsbp_rec - mean(na.omit(d1$nervsbp_rec))
d1$nrvous_max_C <- d1$nervsbp_max - mean(na.omit(d1$nervsbp_max))
d1$nrvous_mean_C <- d1$nervsbp_mean - mean(na.omit(d1$nervsbp_mean))

d1$irrtatd_ret_C <- d1$irrtated_ret - mean(na.omit(d1$irrtated_ret))
d1$irrtatd_rec_C <- d1$irrtdbp_rec - mean(na.omit(d1$irrtdbp_rec))
d1$irrtatd_max_C <- d1$irrtdbp_max - mean(na.omit(d1$irrtdbp_max))
d1$irrtatd_mean_C <- d1$irrtdbp_mean - mean(na.omit(d1$irrtdbp_mean))

### USE

summary(hapa_kr <- lmer(hapa_bias ~ 1 + is_fu + hapa_mean_C + hapa_rec_C + hapa_max_C + neur_C + extra_C + consc_C + agree_C + open_C + is_fu*hapa_rec_C + is_fu*hapa_max_C + is_fu*neur_C + is_fu*extra_C + is_fu*consc_C + is_fu*agree_C + is_fu*open_C + age_C + Avt_C + Lst_C + is_cauc + (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lapa_kr <- lmer(lapa_bias ~ 1 + is_fu + lapa_mean_C + lapa_rec_C + lapa_max_C + neur_C + extra_C + consc_C + agree_C + open_C + is_fu*lapa_rec_C + is_fu*lapa_max_C + is_fu*neur_C + is_fu*extra_C + is_fu*consc_C + is_fu*agree_C + is_fu*open_C + age_C + Avt_C + Lst_C + is_cauc + (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hana_kr <- lmer(hana_bias ~ 1 + is_fu + hana_mean_C + hana_rec_C + hana_max_C + neur_C + extra_C + consc_C + agree_C + open_C + is_fu*hana_rec_C + is_fu*hana_max_C + is_fu*neur_C + is_fu*extra_C + is_fu*consc_C + is_fu*agree_C + is_fu*open_C + age_C + Avt_C + Lst_C + is_cauc + (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lana_kr <- lmer(lana_bias ~ 1 + is_fu + lana_mean_C + lana_rec_C + lana_max_C + neur_C + extra_C + consc_C + agree_C + open_C + is_fu*lana_rec_C + is_fu*lana_max_C + is_fu*neur_C + is_fu*extra_C + is_fu*consc_C + is_fu*agree_C + is_fu*open_C + age_C + Avt_C + Lst_C + is_cauc + (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))

# Same as above, with follow-up coded as 0 (to look at follow-up intercept)
summary(hapa_kr <- lmer(hapa_bias ~ 1 + is_fu_r + hapa_mean_C + hapa_rec_C + hapa_max_C + neur_C + extra_C + consc_C + agree_C + open_C + is_fu_r*hapa_rec_C + is_fu_r*hapa_max_C + is_fu_r*neur_C + is_fu_r*extra_C + is_fu_r*consc_C + is_fu_r*agree_C + is_fu_r*open_C + age_C + Avt_C + Lst_C + is_cauc + (1 + is_fu_r | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lapa_kr <- lmer(lapa_bias ~ 1 + is_fu_r + lapa_mean_C + lapa_rec_C + lapa_max_C + neur_C + extra_C + consc_C + agree_C + open_C + is_fu_r*lapa_rec_C + is_fu_r*lapa_max_C + is_fu_r*neur_C + is_fu_r*extra_C + is_fu_r*consc_C + is_fu_r*agree_C + is_fu_r*open_C + age_C + Avt_C + Lst_C + is_cauc + (1 + is_fu_r | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hana_kr <- lmer(hana_bias ~ 1 + is_fu_r + hana_mean_C + hana_rec_C + hana_max_C + neur_C + extra_C + consc_C + agree_C + open_C + is_fu_r*hana_rec_C + is_fu_r*hana_max_C + is_fu_r*neur_C + is_fu_r*extra_C + is_fu_r*consc_C + is_fu_r*agree_C + is_fu_r*open_C + age_C + Avt_C + Lst_C + is_cauc + (1 + is_fu_r | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lana_kr <- lmer(lana_bias ~ 1 + is_fu_r + lana_mean_C + lana_rec_C + lana_max_C + neur_C + extra_C + consc_C + agree_C + open_C + is_fu_r*lana_rec_C + is_fu_r*lana_max_C + is_fu_r*neur_C + is_fu_r*extra_C + is_fu_r*consc_C + is_fu_r*agree_C + is_fu_r*open_C + age_C + Avt_C + Lst_C + is_cauc + (1 + is_fu_r | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))


### SEP 12 - Simple slopes for High and Low Personality- EXIT VS FOLLOWUP

sd(na.omit(d1$neur_C))
d1$neur_Hi <- d1$neur_C - sd(na.omit(d1$neur_C))
d1$neur_Lo <- d1$neur_C + sd(na.omit(d1$neur_C))

sd(na.omit(d1$extra_C))
d1$extra_Hi <- d1$extra_C - sd(na.omit(d1$extra_C))
d1$extra_Lo <- d1$extra_C + sd(na.omit(d1$extra_C))

sd(na.omit(d1$consc_C))
d1$consc_Hi <- d1$consc_C - sd(na.omit(d1$consc_C))
d1$consc_Lo <- d1$consc_C + sd(na.omit(d1$consc_C))

sd(na.omit(d1$agree_C))
d1$agree_Hi <- d1$agree_C - sd(na.omit(d1$agree_C))
d1$agree_Lo <- d1$agree_C + sd(na.omit(d1$agree_C))

sd(na.omit(d1$open_C))
d1$open_Hi <- d1$open_C - sd(na.omit(d1$open_C))
d1$open_Lo <- d1$open_C + sd(na.omit(d1$open_C))

summary(hapa_hiE_AvgN <- lmer(hapa_bias ~ 1 + is_fu + extra_Hi + neur_C + hapa_mean_C + hapa_rec_C + hapa_max_C + is_fu*extra_Hi + is_fu*neur_C + is_fu*hapa_rec_C + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hapa_loE_AvgN <- lmer(hapa_bias ~ 1 + is_fu + extra_Lo + neur_C + hapa_mean_C + hapa_rec_C + hapa_max_C + is_fu*extra_Lo + is_fu*neur_C + is_fu*hapa_rec_C + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hapa_AvgE_hiN <- lmer(hapa_bias ~ 1 + is_fu + extra_C + neur_Hi + hapa_mean_C + hapa_rec_C + hapa_max_C + is_fu*extra_C + is_fu*neur_Hi + is_fu*hapa_rec_C + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hapa_AvgE_loN <- lmer(hapa_bias ~ 1 + is_fu + extra_C + neur_Lo + hapa_mean_C + hapa_rec_C + hapa_max_C + is_fu*extra_C + is_fu*neur_Lo + is_fu*hapa_rec_C + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))

summary(lapa_hiE_AvgN <- lmer(lapa_bias ~ 1 + is_fu + extra_Hi + neur_C + lapa_mean_C + lapa_rec_C + lapa_max_C + is_fu*extra_Hi + is_fu*neur_C + is_fu*lapa_rec_C + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lapa_loE_AvgN <- lmer(lapa_bias ~ 1 + is_fu + extra_Lo + neur_C + lapa_mean_C + lapa_rec_C + lapa_max_C + is_fu*extra_Lo + is_fu*neur_C + is_fu*lapa_rec_C + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lapa_AvgE_hiN <- lmer(lapa_bias ~ 1 + is_fu + extra_C + neur_Hi + lapa_mean_C + lapa_rec_C + lapa_max_C + is_fu*extra_C + is_fu*neur_Hi + is_fu*lapa_rec_C + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lapa_AvgE_loN <- lmer(lapa_bias ~ 1 + is_fu + extra_C + neur_Lo + lapa_mean_C + lapa_rec_C + lapa_max_C + is_fu*extra_C + is_fu*neur_Lo + is_fu*lapa_rec_C + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))

summary(hana_hiE_AvgN <- lmer(hana_bias ~ 1 + is_fu + extra_Hi + neur_C + hana_mean_C + hana_rec_C + hana_max_C + is_fu*extra_Hi + is_fu*neur_C + is_fu*hana_rec_C + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hana_loE_AvgN <- lmer(hana_bias ~ 1 + is_fu + extra_Lo + neur_C + hana_mean_C + hana_rec_C + hana_max_C + is_fu*extra_Lo + is_fu*neur_C + is_fu*hana_rec_C + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hana_AvgE_hiN <- lmer(hana_bias ~ 1 + is_fu + extra_C + neur_Hi + hana_mean_C + hana_rec_C + hana_max_C + is_fu*extra_C + is_fu*neur_Hi + is_fu*hana_rec_C + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hana_AvgE_loN <- lmer(hana_bias ~ 1 + is_fu + extra_C + neur_Lo + hana_mean_C + hana_rec_C + hana_max_C + is_fu*extra_C + is_fu*neur_Lo + is_fu*hana_rec_C + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))

summary(lana_hiE_AvgN <- lmer(lana_bias ~ 1 + is_fu + extra_Hi + neur_C + lana_mean_C + lana_rec_C + lana_max_C + is_fu*extra_Hi + is_fu*neur_C + is_fu*lana_rec_C + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lana_loE_AvgN <- lmer(lana_bias ~ 1 + is_fu + extra_Lo + neur_C + lana_mean_C + lana_rec_C + lana_max_C + is_fu*extra_Lo + is_fu*neur_C + is_fu*lana_rec_C + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lana_AvgE_hiN <- lmer(lana_bias ~ 1 + is_fu + extra_C + neur_Hi + lana_mean_C + lana_rec_C + lana_max_C + is_fu*extra_C + is_fu*neur_Hi + is_fu*lana_rec_C + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lana_AvgE_loN <- lmer(lana_bias ~ 1 + is_fu + extra_C + neur_Lo + lana_mean_C + lana_rec_C + lana_max_C + is_fu*extra_C + is_fu*neur_Lo + is_fu*lana_rec_C + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))


### SEP 12 - REDUCED Simple slopes for High and Low Personality- EXIT VS FOLLOWUP
# follow-up coded as 0 to look at intercept significance 

summary(hapa_hiE_AvgN <- lmer(hapa_bias ~ 1 + is_fu_r + extra_Hi + neur_C + hapa_mean_C + hapa_rec_C + hapa_max_C + is_fu_r*extra_Hi + is_fu_r*neur_C + is_fu_r*hapa_rec_C + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu_r | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hapa_loE_AvgN <- lmer(hapa_bias ~ 1 + is_fu_r + extra_Lo + neur_C + hapa_mean_C + hapa_rec_C + hapa_max_C + is_fu_r*extra_Lo + is_fu_r*neur_C + is_fu_r*hapa_rec_C + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu_r | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hapa_AvgE_hiN <- lmer(hapa_bias ~ 1 + is_fu_r + extra_C + neur_Hi + hapa_mean_C + hapa_rec_C + hapa_max_C + is_fu_r*extra_C + is_fu_r*neur_Hi + is_fu_r*hapa_rec_C + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu_r | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hapa_AvgE_loN <- lmer(hapa_bias ~ 1 + is_fu_r + extra_C + neur_Lo + hapa_mean_C + hapa_rec_C + hapa_max_C + is_fu_r*extra_C + is_fu_r*neur_Lo + is_fu_r*hapa_rec_C + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu_r | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))

summary(lapa_hiE_AvgN <- lmer(lapa_bias ~ 1 + is_fu_r + extra_Hi + neur_C + lapa_mean_C + lapa_rec_C + lapa_max_C + is_fu_r*extra_Hi + is_fu_r*neur_C + is_fu_r*lapa_rec_C + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu_r | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lapa_loE_AvgN <- lmer(lapa_bias ~ 1 + is_fu_r + extra_Lo + neur_C + lapa_mean_C + lapa_rec_C + lapa_max_C + is_fu_r*extra_Lo + is_fu_r*neur_C + is_fu_r*lapa_rec_C + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu_r | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lapa_AvgE_hiN <- lmer(lapa_bias ~ 1 + is_fu_r + extra_C + neur_Hi + lapa_mean_C + lapa_rec_C + lapa_max_C + is_fu_r*extra_C + is_fu_r*neur_Hi + is_fu_r*lapa_rec_C + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu_r | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lapa_AvgE_loN <- lmer(lapa_bias ~ 1 + is_fu_r + extra_C + neur_Lo + lapa_mean_C + lapa_rec_C + lapa_max_C + is_fu_r*extra_C + is_fu_r*neur_Lo + is_fu_r*lapa_rec_C + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu_r | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))

summary(hana_hiE_AvgN <- lmer(hana_bias ~ 1 + is_fu_r + extra_Hi + neur_C + hana_mean_C + hana_rec_C + hana_max_C + is_fu_r*extra_Hi + is_fu_r*neur_C + is_fu_r*hana_rec_C + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu_r | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hana_loE_AvgN <- lmer(hana_bias ~ 1 + is_fu_r + extra_Lo + neur_C + hana_mean_C + hana_rec_C + hana_max_C + is_fu_r*extra_Lo + is_fu_r*neur_C + is_fu_r*hana_rec_C + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu_r | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hana_AvgE_hiN <- lmer(hana_bias ~ 1 + is_fu_r + extra_C + neur_Hi + hana_mean_C + hana_rec_C + hana_max_C + is_fu_r*extra_C + is_fu_r*neur_Hi + is_fu_r*hana_rec_C + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu_r | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hana_AvgE_loN <- lmer(hana_bias ~ 1 + is_fu_r + extra_C + neur_Lo + hana_mean_C + hana_rec_C + hana_max_C + is_fu_r*extra_C + is_fu_r*neur_Lo + is_fu_r*hana_rec_C + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu_r | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))

summary(lana_hiE_AvgN <- lmer(lana_bias ~ 1 + is_fu_r + extra_Hi + neur_C + lana_mean_C + lana_rec_C + lana_max_C + is_fu_r*extra_Hi + is_fu_r*neur_C + is_fu_r*lana_rec_C + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu_r | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lana_loE_AvgN <- lmer(lana_bias ~ 1 + is_fu_r + extra_Lo + neur_C + lana_mean_C + lana_rec_C + lana_max_C + is_fu_r*extra_Lo + is_fu_r*neur_C + is_fu_r*lana_rec_C + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu_r | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lana_AvgE_hiN <- lmer(lana_bias ~ 1 + is_fu_r + extra_C + neur_Hi + lana_mean_C + lana_rec_C + lana_max_C + is_fu_r*extra_C + is_fu_r*neur_Hi + is_fu_r*lana_rec_C + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu_r | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lana_AvgE_loN <- lmer(lana_bias ~ 1 + is_fu_r + extra_C + neur_Lo + lana_mean_C + lana_rec_C + lana_max_C + is_fu_r*extra_C + is_fu_r*neur_Lo + is_fu_r*lana_rec_C + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu_r | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))

