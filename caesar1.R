## CAESAR study analyses, May 2014 and Dec 2015

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
summary(d1 <- read.table("Rdata_levelscollapsed_onlyincludedparticipants.csv",header=TRUE, sep=",")) # entire dataset
statsBy(data=d1, group="id", cors = FALSE, method="pearson",poly=FALSE)
summary(d1_ex <- subset(d1, is.na(d1$retro_time) | (d1$retro_time < 10))) # exit reports only

# checking for differences between people who dropped out before follow-up and those who didn't
summary(dp <- read.table("level2_onlyincludedparticipants.csv",header=TRUE, sep=",")) # person-level data
print(summary( a1 <- lm( sex ~ 1 + fu_did, data=dp )), digits=10)
print(summary( a1 <- lm( age ~ 1 + fu_did, data=dp )), digits=10)
print(summary( a1 <- lm( is_afri ~ 1 + fu_did, data=dp )), digits=10)
print(summary( a1 <- lm( is_asia ~ 1 + fu_did, data=dp )), digits=10)
print(summary( a1 <- lm( is_cauc ~ 1 + fu_did, data=dp )), digits=10)
print(summary( a1 <- lm( Avt ~ 1 + fu_did, data=dp )), digits=10)
print(summary( a1 <- lm( Lst ~ 1 + fu_did, data=dp )), digits=10)
print(summary( a1 <- lm( edu_high ~ 1 + fu_did, data=dp )), digits=10)
print(summary( a1 <- lm( neur5 ~ 1 + fu_did, data=dp )), digits=10)
print(summary( a1 <- lm( extra5 ~ 1 + fu_did, data=dp )), digits=10)

# Grand mean centering individual-level variables

d1$sex_C <- d1$sex - mean(na.omit(d1$sex))
d1$age_C <- d1$age - mean(na.omit(d1$age))
d1$Avt_C <- d1$Avt - mean(na.omit(d1$Avt))
d1$Lst_C <- d1$Lst - mean(na.omit(d1$Lst))
d1$edu_C <- d1$edu - mean(na.omit(d1$edu))
d1$neur_C <- d1$neur - mean(na.omit(d1$neur))
d1$extra_C <- d1$extra - mean(na.omit(d1$extra))

d1$hana_ret_C <- d1$hana_ret - mean(na.omit(d1$hana_ret))
d1$hana_rec_C <- d1$hana_rec - mean(na.omit(d1$hana_rec))
d1$hana_max_C <- d1$hana_max - mean(na.omit(d1$hana_max))
d1$hana_mean_C <- d1$hana_bp_mean - mean(na.omit(d1$hana_bp_mean))

d1$lana_ret_C <- d1$lana_ret - mean(na.omit(d1$lana_ret))
d1$lana_rec_C <- d1$lana_rec - mean(na.omit(d1$lana_rec))
d1$lana_max_C <- d1$lana_max - mean(na.omit(d1$lana_max))
d1$lana_mean_C <- d1$lana_bp_mean - mean(na.omit(d1$lana_bp_mean))

d1$hapa_ret_C <- d1$hapa_ret - mean(na.omit(d1$hapa_ret))
d1$hapa_rec_C <- d1$hapa_rec - mean(na.omit(d1$hapa_rec))
d1$hapa_max_C <- d1$hapa_max - mean(na.omit(d1$hapa_max))
d1$hapa_mean_C <- d1$hapa_bp_mean - mean(na.omit(d1$hapa_bp_mean))

d1$lapa_ret_C <- d1$lapa_ret - mean(na.omit(d1$lapa_ret))
d1$lapa_rec_C <- d1$lapa_rec - mean(na.omit(d1$lapa_rec))
d1$lapa_max_C <- d1$lapa_max - mean(na.omit(d1$lapa_max))
d1$lapa_mean_C <- d1$lapa_bp_mean - mean(na.omit(d1$lapa_bp_mean))

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

d1_ex$sex_C <- d1_ex$sex - mean(na.omit(d1_ex$sex))
d1_ex$age_C <- d1_ex$age - mean(na.omit(d1_ex$age))
d1_ex$Avt_C <- d1_ex$Avt - mean(na.omit(d1_ex$Avt))
d1_ex$Lst_C <- d1_ex$Lst - mean(na.omit(d1_ex$Lst))
d1_ex$edu_C <- d1_ex$edu - mean(na.omit(d1_ex$edu))
d1_ex$neur_C <- d1_ex$neur - mean(na.omit(d1_ex$neur))
d1_ex$extra_C <- d1_ex$extra - mean(na.omit(d1_ex$extra))

d1_ex$hana_ret_C <- d1_ex$hana_ret - mean(na.omit(d1_ex$hana_ret))
d1_ex$hana_rec_C <- d1_ex$hana_rec - mean(na.omit(d1_ex$hana_rec))
d1_ex$hana_max_C <- d1_ex$hana_max - mean(na.omit(d1_ex$hana_max))
d1_ex$hana_mean_C <- d1_ex$hana_bp_mean - mean(na.omit(d1_ex$hana_bp_mean))

d1_ex$lana_ret_C <- d1_ex$lana_ret - mean(na.omit(d1_ex$lana_ret))
d1_ex$lana_rec_C <- d1_ex$lana_rec - mean(na.omit(d1_ex$lana_rec))
d1_ex$lana_max_C <- d1_ex$lana_max - mean(na.omit(d1_ex$lana_max))
d1_ex$lana_mean_C <- d1_ex$lana_bp_mean - mean(na.omit(d1_ex$lana_bp_mean))

d1_ex$hapa_ret_C <- d1_ex$hapa_ret - mean(na.omit(d1_ex$hapa_ret))
d1_ex$hapa_rec_C <- d1_ex$hapa_rec - mean(na.omit(d1_ex$hapa_rec))
d1_ex$hapa_max_C <- d1_ex$hapa_max - mean(na.omit(d1_ex$hapa_max))
d1_ex$hapa_mean_C <- d1_ex$hapa_bp_mean - mean(na.omit(d1_ex$hapa_bp_mean))

d1_ex$lapa_ret_C <- d1_ex$lapa_ret - mean(na.omit(d1_ex$lapa_ret))
d1_ex$lapa_rec_C <- d1_ex$lapa_rec - mean(na.omit(d1_ex$lapa_rec))
d1_ex$lapa_max_C <- d1_ex$lapa_max - mean(na.omit(d1_ex$lapa_max))
d1_ex$lapa_mean_C <- d1_ex$lapa_bp_mean - mean(na.omit(d1_ex$lapa_bp_mean))

d1_ex$happy_ret_C <- d1_ex$happy_ret - mean(na.omit(d1_ex$happy_ret))
d1_ex$happy_rec_C <- d1_ex$happybp_rec - mean(na.omit(d1_ex$happybp_rec))
d1_ex$happy_max_C <- d1_ex$happybp_max - mean(na.omit(d1_ex$happybp_max))
d1_ex$happy_mean_C <- d1_ex$happybp_mean - mean(na.omit(d1_ex$happybp_mean))

d1_ex$quiet_ret_C <- d1_ex$quiet_ret - mean(na.omit(d1_ex$quiet_ret))
d1_ex$quiet_rec_C <- d1_ex$quietbp_rec - mean(na.omit(d1_ex$quietbp_rec))
d1_ex$quiet_max_C <- d1_ex$quietbp_max - mean(na.omit(d1_ex$quietbp_max))
d1_ex$quiet_mean_C <- d1_ex$quietbp_mean - mean(na.omit(d1_ex$quietbp_mean))

d1_ex$exctd_ret_C <- d1_ex$exctd_ret - mean(na.omit(d1_ex$exctd_ret))
d1_ex$exctd_rec_C <- d1_ex$exctdbp_rec - mean(na.omit(d1_ex$exctdbp_rec))
d1_ex$exctd_max_C <- d1_ex$exctdbp_max - mean(na.omit(d1_ex$exctdbp_max))
d1_ex$exctd_mean_C <- d1_ex$exctdbp_mean - mean(na.omit(d1_ex$exctdbp_mean))

d1_ex$calm_ret_C <- d1_ex$calm_ret - mean(na.omit(d1_ex$calm_ret))
d1_ex$calm_rec_C <- d1_ex$calmbp_rec - mean(na.omit(d1_ex$calmbp_rec))
d1_ex$calm_max_C <- d1_ex$calmbp_max - mean(na.omit(d1_ex$calmbp_max))
d1_ex$calm_mean_C <- d1_ex$calmbp_mean - mean(na.omit(d1_ex$calmbp_mean))

d1_ex$sad_ret_C <- d1_ex$sad_ret - mean(na.omit(d1_ex$sad_ret))
d1_ex$sad_rec_C <- d1_ex$sadbp_rec - mean(na.omit(d1_ex$sadbp_rec))
d1_ex$sad_max_C <- d1_ex$sadbp_max - mean(na.omit(d1_ex$sadbp_max))
d1_ex$sad_mean_C <- d1_ex$sadbp_mean - mean(na.omit(d1_ex$sadbp_mean))

d1_ex$sleepy_ret_C <- d1_ex$sleepy_ret - mean(na.omit(d1_ex$sleepy_ret))
d1_ex$sleepy_rec_C <- d1_ex$slepybp_rec - mean(na.omit(d1_ex$slepybp_rec))
d1_ex$sleepy_max_C <- d1_ex$slepybp_max - mean(na.omit(d1_ex$slepybp_max))
d1_ex$sleepy_mean_C <- d1_ex$slepybp_mean - mean(na.omit(d1_ex$slepybp_mean))

d1_ex$nerves_ret_C <- d1_ex$nrvous_ret - mean(na.omit(d1_ex$nrvous_ret))
d1_ex$nerves_rec_C <- d1_ex$nervsbp_rec - mean(na.omit(d1_ex$nervsbp_rec))
d1_ex$nerves_max_C <- d1_ex$nervsbp_max - mean(na.omit(d1_ex$nervsbp_max))
d1_ex$nerves_mean_C <- d1_ex$nervsbp_mean - mean(na.omit(d1_ex$nervsbp_mean))

d1_ex$irrtd_ret_C <- d1_ex$irrtated_ret - mean(na.omit(d1_ex$irrtated_ret))
d1_ex$irrtd_rec_C <- d1_ex$irrtdbp_rec - mean(na.omit(d1_ex$irrtdbp_rec))
d1_ex$irrtd_max_C <- d1_ex$irrtdbp_max - mean(na.omit(d1_ex$irrtdbp_max))
d1_ex$irrtd_mean_C <- d1_ex$irrtdbp_mean - mean(na.omit(d1_ex$irrtdbp_mean))

# Standardizing individual-level variables

d1$sex_S <- d1$sex_C / sd(na.omit(d1$sex))
d1$age_S <- d1$age_C / sd(na.omit(d1$age))
d1$Avt_S <- d1$Avt_C / sd(na.omit(d1$Avt))
d1$Lst_S <- d1$Lst_C / sd(na.omit(d1$Lst))
d1$edu_S <- d1$edu_C / sd(na.omit(d1$edu))
d1$neur_S <- d1$neur_C / sd(na.omit(d1$neur))
d1$extra_S <- d1$extra_C / sd(na.omit(d1$extra))

d1$hana_ret_S <- d1$hana_ret_C / sd(na.omit(d1$hana_ret))
d1$hana_rec_S <- d1$hana_rec_C / sd(na.omit(d1$hana_rec))
d1$hana_max_S <- d1$hana_max_C / sd(na.omit(d1$hana_max))
d1$hana_mean_S <- d1$hana_mean_C / sd(na.omit(d1$hana_bp_mean))

d1$lana_ret_S <- d1$lana_ret_C / sd(na.omit(d1$lana_ret))
d1$lana_rec_S <- d1$lana_rec_C / sd(na.omit(d1$lana_rec))
d1$lana_max_S <- d1$lana_max_C / sd(na.omit(d1$lana_max))
d1$lana_mean_S <- d1$lana_mean_C / sd(na.omit(d1$lana_bp_mean))

d1$hapa_ret_S <- d1$hapa_ret_C / sd(na.omit(d1$hapa_ret))
d1$hapa_rec_S <- d1$hapa_rec_C / sd(na.omit(d1$hapa_rec))
d1$hapa_max_S <- d1$hapa_max_C / sd(na.omit(d1$hapa_max))
d1$hapa_mean_S <- d1$hapa_mean_C / sd(na.omit(d1$hapa_bp_mean))

d1$lapa_ret_S <- d1$lapa_ret_C / sd(na.omit(d1$lapa_ret))
d1$lapa_rec_S <- d1$lapa_rec_C / sd(na.omit(d1$lapa_rec))
d1$lapa_max_S <- d1$lapa_max_C / sd(na.omit(d1$lapa_max))
d1$lapa_mean_S <- d1$lapa_mean_C / sd(na.omit(d1$lapa_bp_mean))

d1$happy_ret_S <- d1$happy_ret_C / sd(na.omit(d1$happy_ret))
d1$happy_rec_S <- d1$happy_rec_C / sd(na.omit(d1$happybp_rec))
d1$happy_max_S <- d1$happy_max_C / sd(na.omit(d1$happybp_max))
d1$happy_mean_S <- d1$happy_mean_C / sd(na.omit(d1$happybp_mean))

d1$quiet_ret_S <- d1$quiet_ret_C / sd(na.omit(d1$quiet_ret))
d1$quiet_rec_S <- d1$quiet_rec_C / sd(na.omit(d1$quietbp_rec))
d1$quiet_max_S <- d1$quiet_max_C / sd(na.omit(d1$quietbp_max))
d1$quiet_mean_S <- d1$quiet_mean_C / sd(na.omit(d1$quietbp_mean))

d1$exctd_ret_S <- d1$exctd_ret_C / sd(na.omit(d1$exctd_ret))
d1$exctd_rec_S <- d1$exctd_rec_C / sd(na.omit(d1$exctdbp_rec))
d1$exctd_max_S <- d1$exctd_max_C / sd(na.omit(d1$exctdbp_max))
d1$exctd_mean_S <- d1$exctd_mean_C / sd(na.omit(d1$exctdbp_mean))

d1$calm_ret_S <- d1$calm_ret_C / sd(na.omit(d1$calm_ret))
d1$calm_rec_S <- d1$calm_rec_C / sd(na.omit(d1$calmbp_rec))
d1$calm_max_S <- d1$calm_max_C / sd(na.omit(d1$calmbp_max))
d1$calm_mean_S <- d1$calm_mean_C / sd(na.omit(d1$calmbp_mean))

d1$sad_ret_S <- d1$sad_ret_C / sd(na.omit(d1$sad_ret))
d1$sad_rec_S <- d1$sad_rec_C / sd(na.omit(d1$sadbp_rec))
d1$sad_max_S <- d1$sad_max_C / sd(na.omit(d1$sadbp_max))
d1$sad_mean_S <- d1$sad_mean_C / sd(na.omit(d1$sadbp_mean))

d1$sleepy_ret_S <- d1$sleepy_ret_C / sd(na.omit(d1$sleepy_ret))
d1$sleepy_rec_S <- d1$sleepy_rec_C / sd(na.omit(d1$slepybp_rec))
d1$sleepy_max_S <- d1$sleepy_max_C / sd(na.omit(d1$slepybp_max))
d1$sleepy_mean_S <- d1$sleepy_mean_C / sd(na.omit(d1$slepybp_mean))

d1$nerves_ret_S <- d1$nerves_ret_C / sd(na.omit(d1$nrvous_ret))
d1$nerves_rec_S <- d1$nerves_rec_C / sd(na.omit(d1$nervsbp_rec))
d1$nerves_max_S <- d1$nerves_max_C / sd(na.omit(d1$nervsbp_max))
d1$nerves_mean_S <- d1$nerves_mean_C / sd(na.omit(d1$nervsbp_mean))

d1$irrtd_ret_S <- d1$irrtd_ret_C / sd(na.omit(d1$irrtated_ret))
d1$irrtd_rec_S <- d1$irrtd_rec_C / sd(na.omit(d1$irrtdbp_rec))
d1$irrtd_max_S <- d1$irrtd_max_C / sd(na.omit(d1$irrtdbp_max))
d1$irrtd_mean_S <- d1$irrtd_mean_C / sd(na.omit(d1$irrtdbp_mean))

d1_ex$sex_S <- d1_ex$sex_C / sd(na.omit(d1_ex$sex))
d1_ex$age_S <- d1_ex$age_C / sd(na.omit(d1_ex$age))
d1_ex$Avt_S <- d1_ex$Avt_C / sd(na.omit(d1_ex$Avt))
d1_ex$Lst_S <- d1_ex$Lst_C / sd(na.omit(d1_ex$Lst))
d1_ex$edu_S <- d1_ex$edu_C / sd(na.omit(d1_ex$edu))
d1_ex$neur_S <- d1_ex$neur_C / sd(na.omit(d1_ex$neur))
d1_ex$extra_S <- d1_ex$extra_C / sd(na.omit(d1_ex$extra))

d1_ex$hana_ret_S <- d1_ex$hana_ret_C / sd(na.omit(d1_ex$hana_ret))
d1_ex$hana_rec_S <- d1_ex$hana_rec_C / sd(na.omit(d1_ex$hana_rec))
d1_ex$hana_max_S <- d1_ex$hana_max_C / sd(na.omit(d1_ex$hana_max))
d1_ex$hana_mean_S <- d1_ex$hana_mean_C / sd(na.omit(d1_ex$hana_bp_mean))

d1_ex$lana_ret_S <- d1_ex$lana_ret_C / sd(na.omit(d1_ex$lana_ret))
d1_ex$lana_rec_S <- d1_ex$lana_rec_C / sd(na.omit(d1_ex$lana_rec))
d1_ex$lana_max_S <- d1_ex$lana_max_C / sd(na.omit(d1_ex$lana_max))
d1_ex$lana_mean_S <- d1_ex$lana_mean_C / sd(na.omit(d1_ex$lana_bp_mean))

d1_ex$hapa_ret_S <- d1_ex$hapa_ret_C / sd(na.omit(d1_ex$hapa_ret))
d1_ex$hapa_rec_S <- d1_ex$hapa_rec_C / sd(na.omit(d1_ex$hapa_rec))
d1_ex$hapa_max_S <- d1_ex$hapa_max_C / sd(na.omit(d1_ex$hapa_max))
d1_ex$hapa_mean_S <- d1_ex$hapa_mean_C / sd(na.omit(d1_ex$hapa_bp_mean))

d1_ex$lapa_ret_S <- d1_ex$lapa_ret_C / sd(na.omit(d1_ex$lapa_ret))
d1_ex$lapa_rec_S <- d1_ex$lapa_rec_C / sd(na.omit(d1_ex$lapa_rec))
d1_ex$lapa_max_S <- d1_ex$lapa_max_C / sd(na.omit(d1_ex$lapa_max))
d1_ex$lapa_mean_S <- d1_ex$lapa_mean_C / sd(na.omit(d1_ex$lapa_bp_mean))

d1_ex$happy_ret_S <- d1_ex$happy_ret_C / sd(na.omit(d1_ex$happy_ret))
d1_ex$happy_rec_S <- d1_ex$happy_rec_C / sd(na.omit(d1_ex$happybp_rec))
d1_ex$happy_max_S <- d1_ex$happy_max_C / sd(na.omit(d1_ex$happybp_max))
d1_ex$happy_mean_S <- d1_ex$happy_mean_C / sd(na.omit(d1_ex$happybp_mean))

d1_ex$quiet_ret_S <- d1_ex$quiet_ret_C / sd(na.omit(d1_ex$quiet_ret))
d1_ex$quiet_rec_S <- d1_ex$quiet_rec_C / sd(na.omit(d1_ex$quietbp_rec))
d1_ex$quiet_max_S <- d1_ex$quiet_max_C / sd(na.omit(d1_ex$quietbp_max))
d1_ex$quiet_mean_S <- d1_ex$quiet_mean_C / sd(na.omit(d1_ex$quietbp_mean))

d1_ex$exctd_ret_S <- d1_ex$exctd_ret_C / sd(na.omit(d1_ex$exctd_ret))
d1_ex$exctd_rec_S <- d1_ex$exctd_rec_C / sd(na.omit(d1_ex$exctdbp_rec))
d1_ex$exctd_max_S <- d1_ex$exctd_max_C / sd(na.omit(d1_ex$exctdbp_max))
d1_ex$exctd_mean_S <- d1_ex$exctd_mean_C / sd(na.omit(d1_ex$exctdbp_mean))

d1_ex$calm_ret_S <- d1_ex$calm_ret_C / sd(na.omit(d1_ex$calm_ret))
d1_ex$calm_rec_S <- d1_ex$calm_rec_C / sd(na.omit(d1_ex$calmbp_rec))
d1_ex$calm_max_S <- d1_ex$calm_max_C / sd(na.omit(d1_ex$calmbp_max))
d1_ex$calm_mean_S <- d1_ex$calm_mean_C / sd(na.omit(d1_ex$calmbp_mean))

d1_ex$sad_ret_S <- d1_ex$sad_ret_C / sd(na.omit(d1_ex$sad_ret))
d1_ex$sad_rec_S <- d1_ex$sad_rec_C / sd(na.omit(d1_ex$sadbp_rec))
d1_ex$sad_max_S <- d1_ex$sad_max_C / sd(na.omit(d1_ex$sadbp_max))
d1_ex$sad_mean_S <- d1_ex$sad_mean_C / sd(na.omit(d1_ex$sadbp_mean))

d1_ex$sleepy_ret_S <- d1_ex$sleepy_ret_C / sd(na.omit(d1_ex$sleepy_ret))
d1_ex$sleepy_rec_S <- d1_ex$sleepy_rec_C / sd(na.omit(d1_ex$slepybp_rec))
d1_ex$sleepy_max_S <- d1_ex$sleepy_max_C / sd(na.omit(d1_ex$slepybp_max))
d1_ex$sleepy_mean_S <- d1_ex$sleepy_mean_C / sd(na.omit(d1_ex$slepybp_mean))

d1_ex$nerves_ret_S <- d1_ex$nerves_ret_C / sd(na.omit(d1_ex$nrvous_ret))
d1_ex$nerves_rec_S <- d1_ex$nerves_rec_C / sd(na.omit(d1_ex$nervsbp_rec))
d1_ex$nerves_max_S <- d1_ex$nerves_max_C / sd(na.omit(d1_ex$nervsbp_max))
d1_ex$nerves_mean_S <- d1_ex$nerves_mean_C / sd(na.omit(d1_ex$nervsbp_mean))

d1_ex$irrtd_ret_S <- d1_ex$irrtd_ret_C / sd(na.omit(d1_ex$irrtated_ret))
d1_ex$irrtd_rec_S <- d1_ex$irrtd_rec_C / sd(na.omit(d1_ex$irrtdbp_rec))
d1_ex$irrtd_max_S <- d1_ex$irrtd_max_C / sd(na.omit(d1_ex$irrtdbp_max))
d1_ex$irrtd_mean_S <- d1_ex$irrtd_mean_C / sd(na.omit(d1_ex$irrtdbp_mean))


#############################################################################################
##  Replicating Findings: predictive utility of average, peak, and recent momentary affect ##
#############################################################################################

print(summary( M1_hana <- lm( hana_ret_S ~ 1 + hana_mean_S + hana_max_S + hana_rec_S, data=d1_ex )), digits=10)
print(summary( MC1_hana <- lm( hana_ret_S ~ 1 + hana_mean_S + hana_max_S + hana_rec_S + sex_S + age_S + Avt_S + Lst_S + edu_S + neur_S, data=d1_ex )), digits=10)

print(summary( M1_lana <- lm( lana_ret_S ~ 1 + lana_mean_S + lana_max_S + lana_rec_S, data=d1_ex )), digits=10)
print(summary( MC1_lana <- lm( lana_ret_S ~ 1 + lana_mean_S + lana_max_S + lana_rec_S + sex_S + age_S + Avt_S + Lst_S + edu_S + neur_S, data=d1_ex )), digits=10)

print(summary( M1_hapa <- lm( hapa_ret_S ~ 1 + hapa_mean_S + hapa_max_S + hapa_rec_S, data=d1_ex )), digits=10)
print(summary( MC1_hapa <- lm( hapa_ret_S ~ 1 + hapa_mean_S + hapa_max_S + hapa_rec_S + sex_S + age_S + Avt_S + Lst_S + edu_S + neur_S, data=d1_ex )), digits=10)

print(summary( M1_lapa <- lm( lapa_ret_S ~ 1 + lapa_mean_S + lapa_max_S + lapa_rec_S, data=d1_ex )), digits=10)
print(summary( MC1_lapa <- lm( lapa_ret_S ~ 1 + lapa_mean_S + lapa_max_S + lapa_rec_S + sex_S + age_S + Avt_S + Lst_S + edu_S + neur_S, data=d1_ex )), digits=10)

##################################################################################################
##  Predicting retrospective from average, peak, and recent momentary affect, incl extraversion ##
##################################################################################################

print(summary( MC2_hana <- lm( hana_ret_S ~ 1 + hana_mean_S + hana_max_S + hana_rec_S + sex_S + age_S + Avt_S + Lst_S + edu_S + neur_S + extra_S, data=d1_ex )), digits=10)
print(summary( MC2_lana <- lm( lana_ret_S ~ 1 + lana_mean_S + lana_max_S + lana_rec_S + sex_S + age_S + Avt_S + Lst_S + edu_S + neur_S + extra_S, data=d1_ex )), digits=10)
print(summary( MC2_hapa <- lm( hapa_ret_S ~ 1 + hapa_mean_S + hapa_max_S + hapa_rec_S + sex_S + age_S + Avt_S + Lst_S + edu_S + neur_S + extra_S, data=d1_ex )), digits=10)
print(summary( MC2_lapa <- lm( lapa_ret_S ~ 1 + lapa_mean_S + lapa_max_S + lapa_rec_S + sex_S + age_S + Avt_S + Lst_S + edu_S + neur_S + extra_S, data=d1_ex )), digits=10)

#########################################################################################################
##  Predicting discrete emotion reports from average, peak, and recent affect + personality predictors ##
#########################################################################################################

print(summary( MC2_happy <- lm( happy_ret_S ~ 1 + happy_mean_S + happy_max_S + happy_rec_S + sex_S + age_S + Avt_S + Lst_S + edu_S + neur_S + extra_S, data=d1_ex )), digits=10)
print(summary( MC2_quiet <- lm( quiet_ret_S ~ 1 + quiet_mean_S + quiet_max_S + quiet_rec_S + sex_S + age_S + Avt_S + Lst_S + edu_S + neur_S + extra_S, data=d1_ex )), digits=10)
print(summary( MC2_exctd <- lm( exctd_ret_S ~ 1 + exctd_mean_S + exctd_max_S + exctd_rec_S + sex_S + age_S + Avt_S + Lst_S + edu_S + neur_S + extra_S, data=d1_ex )), digits=10)
print(summary( MC2_calm <- lm( calm_ret_S ~ 1 + calm_mean_S + calm_max_S + calm_rec_S + sex_S + age_S + Avt_S + Lst_S + edu_S + neur_S + extra_S, data=d1_ex )), digits=10)
print(summary( MC2_sad <- lm( sad_ret_S ~ 1 + sad_mean_S + sad_max_S + sad_rec_S + sex_S + age_S + Avt_S + Lst_S + edu_S + neur_S + extra_S, data=d1_ex )), digits=10)
print(summary( MC2_sleepy <- lm( sleepy_ret_S ~ 1 + sleepy_mean_S + sleepy_max_S + sleepy_rec_S + sex_S + age_S + Avt_S + Lst_S + edu_S + neur_S + extra_S, data=d1_ex )), digits=10)
print(summary( MC2_nerves <- lm( nerves_ret_S ~ 1 + nerves_mean_S + nerves_max_S + nerves_rec_S + sex_S + age_S + Avt_S + Lst_S + edu_S + neur_S + extra_S, data=d1_ex )), digits=10)
print(summary( MC2_irrtd <- lm( irrtd_ret_S ~ 1 + irrtd_mean_S + irrtd_max_S + irrtd_rec_S + sex_S + age_S + Avt_S + Lst_S + edu_S + neur_S + extra_S, data=d1_ex )), digits=10)

#########################################################################################################
##  Predicting bias in emotion reports -- all covariates, emotion cateogories, and discrete emotions   ##
#########################################################################################################

print(summary( MC3_hapa <- lm( hapa_bias ~ 1 + hapa_mean_S + hapa_max_S + hapa_rec_S + sex_S + age_S + Avt_S + Lst_S + edu_S + neur_S + extra_S, data=d1_ex )), digits=10)
print(summary( MC3_lapa <- lm( lapa_bias ~ 1 + lapa_mean_S + lapa_max_S + lapa_rec_S + sex_S + age_S + Avt_S + Lst_S + edu_S + neur_S + extra_S, data=d1_ex )), digits=10)
print(summary( MC3_hana <- lm( hana_bias ~ 1 + hana_mean_S + hana_max_S + hana_rec_S + sex_S + age_S + Avt_S + Lst_S + edu_S + neur_S + extra_S, data=d1_ex )), digits=10)
print(summary( MC3_lana <- lm( lana_bias ~ 1 + lana_mean_S + lana_max_S + lana_rec_S + sex_S + age_S + Avt_S + Lst_S + edu_S + neur_S + extra_S, data=d1_ex )), digits=10)

print(summary( MC3_happy <- lm( happy_bias ~ 1 + happy_mean_S + happy_max_S + happy_rec_S + sex_S + age_S + Avt_S + Lst_S + edu_S + neur_S + extra_S, data=d1_ex )), digits=10)
print(summary( MC3_quiet <- lm( quiet_bias ~ 1 + quiet_mean_S + quiet_max_S + quiet_rec_S + sex_S + age_S + Avt_S + Lst_S + edu_S + neur_S + extra_S, data=d1_ex )), digits=10)
print(summary( MC3_exctd <- lm( exctd_bias ~ 1 + exctd_mean_S + exctd_max_S + exctd_rec_S + sex_S + age_S + Avt_S + Lst_S + edu_S + neur_S + extra_S, data=d1_ex )), digits=10)
print(summary( MC3_calm <- lm( calm_bias ~ 1 + calm_mean_S + calm_max_S + calm_rec_S + sex_S + age_S + Avt_S + Lst_S + edu_S + neur_S + extra_S, data=d1_ex )), digits=10)
print(summary( MC3_sad <- lm( sad_bias ~ 1 + sad_mean_S + sad_max_S + sad_rec_S + sex_S + age_S + Avt_S + Lst_S + edu_S + neur_S + extra_S, data=d1_ex )), digits=10)
print(summary( MC3_sleepy <- lm( sleepy_bias ~ 1 + sleepy_mean_S + sleepy_max_S + sleepy_rec_S + sex_S + age_S + Avt_S + Lst_S + edu_S + neur_S + extra_S, data=d1_ex )), digits=10)
print(summary( MC3_nerves <- lm( nrvous_bias ~ 1 + nerves_mean_S + nerves_max_S + nerves_rec_S + sex_S + age_S + Avt_S + Lst_S + edu_S + neur_S + extra_S, data=d1_ex )), digits=10)
print(summary( MC3_irrtd <- lm( irrtatd_bias ~ 1 + irrtd_mean_S + irrtd_max_S + irrtd_rec_S + sex_S + age_S + Avt_S + Lst_S + edu_S + neur_S + extra_S, data=d1_ex )), digits=10)






###################################################################################################################
##  Predicting bias in emotion reports using all data (includes reports from exit and followup session) 
##  -- all covariates, emotion cateogories, and discrete emotions  
###################################################################################################################

print(summary( MC3_hapa <- lm( hapa_bias ~ 1 + hapa_mean_S + hapa_max_S + hapa_rec_S + sex_S + age_S + Avt_S + Lst_S + edu_S + neur_S + extra_S, data=d1 )), digits=10)
print(summary( MC3_lapa <- lm( lapa_bias ~ 1 + lapa_mean_S + lapa_max_S + lapa_rec_S + sex_S + age_S + Avt_S + Lst_S + edu_S + neur_S + extra_S, data=d1 )), digits=10)
print(summary( MC3_hana <- lm( hana_bias ~ 1 + hana_mean_S + hana_max_S + hana_rec_S + sex_S + age_S + Avt_S + Lst_S + edu_S + neur_S + extra_S, data=d1 )), digits=10)
print(summary( MC3_lana <- lm( lana_bias ~ 1 + lana_mean_S + lana_max_S + lana_rec_S + sex_S + age_S + Avt_S + Lst_S + edu_S + neur_S + extra_S, data=d1 )), digits=10)

print(summary( MC3_happy <- lm( happy_bias ~ 1 + happy_mean_S + happy_max_S + happy_rec_S + sex_S + age_S + Avt_S + Lst_S + edu_S + neur_S + extra_S, data=d1 )), digits=10)
print(summary( MC3_quiet <- lm( quiet_bias ~ 1 + quiet_mean_S + quiet_max_S + quiet_rec_S + sex_S + age_S + Avt_S + Lst_S + edu_S + neur_S + extra_S, data=d1 )), digits=10)
print(summary( MC3_exctd <- lm( exctd_bias ~ 1 + exctd_mean_S + exctd_max_S + exctd_rec_S + sex_S + age_S + Avt_S + Lst_S + edu_S + neur_S + extra_S, data=d1 )), digits=10)
print(summary( MC3_calm <- lm( calm_bias ~ 1 + calm_mean_S + calm_max_S + calm_rec_S + sex_S + age_S + Avt_S + Lst_S + edu_S + neur_S + extra_S, data=d1 )), digits=10)
print(summary( MC3_sad <- lm( sad_bias ~ 1 + sad_mean_S + sad_max_S + sad_rec_S + sex_S + age_S + Avt_S + Lst_S + edu_S + neur_S + extra_S, data=d1 )), digits=10)
print(summary( MC3_sleepy <- lm( sleepy_bias ~ 1 + sleepy_mean_S + sleepy_max_S + sleepy_rec_S + sex_S + age_S + Avt_S + Lst_S + edu_S + neur_S + extra_S, data=d1 )), digits=10)
print(summary( MC3_nerves <- lm( nrvous_bias ~ 1 + nerves_mean_S + nerves_max_S + nerves_rec_S + sex_S + age_S + Avt_S + Lst_S + edu_S + neur_S + extra_S, data=d1 )), digits=10)
print(summary( MC3_irrtd <- lm( irrtatd_bias ~ 1 + irrtd_mean_S + irrtd_max_S + irrtd_rec_S + sex_S + age_S + Avt_S + Lst_S + edu_S + neur_S + extra_S, data=d1 )), digits=10)

###################################################################################################################
##  Predicting bias in emotion reports WITH TIME (includes reports from exit and followup session) 
##  -- all covariates, emotion cateogories, and discrete emotions  
###################################################################################################################

print(summary( MC3_hapa <- lm( hapa_bias ~ 1 + retro_time + hapa_mean_S + hapa_max_S + hapa_rec_S + sex_S + age_S + Avt_S + Lst_S + edu_S + neur_S + extra_S, data=d1 )), digits=10)
print(summary( MC3_lapa <- lm( lapa_bias ~ 1 + retro_time + lapa_mean_S + lapa_max_S + lapa_rec_S + sex_S + age_S + Avt_S + Lst_S + edu_S + neur_S + extra_S, data=d1 )), digits=10)
print(summary( MC3_hana <- lm( hana_bias ~ 1 + retro_time + hana_mean_S + hana_max_S + hana_rec_S + sex_S + age_S + Avt_S + Lst_S + edu_S + neur_S + extra_S, data=d1 )), digits=10)
print(summary( MC3_lana <- lm( lana_bias ~ 1 + retro_time + lana_mean_S + lana_max_S + lana_rec_S + sex_S + age_S + Avt_S + Lst_S + edu_S + neur_S + extra_S, data=d1 )), digits=10)

print(summary( MC3_happy <- lm( happy_bias ~ 1 + retro_time + happy_mean_S + happy_max_S + happy_rec_S + sex_S + age_S + Avt_S + Lst_S + edu_S + neur_S + extra_S, data=d1 )), digits=10)
print(summary( MC3_quiet <- lm( quiet_bias ~ 1 + retro_time + quiet_mean_S + quiet_max_S + quiet_rec_S + sex_S + age_S + Avt_S + Lst_S + edu_S + neur_S + extra_S, data=d1 )), digits=10)
print(summary( MC3_exctd <- lm( exctd_bias ~ 1 + retro_time + exctd_mean_S + exctd_max_S + exctd_rec_S + sex_S + age_S + Avt_S + Lst_S + edu_S + neur_S + extra_S, data=d1 )), digits=10)
print(summary( MC3_calm <- lm( calm_bias ~ 1 + retro_time + calm_mean_S + calm_max_S + calm_rec_S + sex_S + age_S + Avt_S + Lst_S + edu_S + neur_S + extra_S, data=d1 )), digits=10)
print(summary( MC3_sad <- lm( sad_bias ~ 1 + retro_time + sad_mean_S + sad_max_S + sad_rec_S + sex_S + age_S + Avt_S + Lst_S + edu_S + neur_S + extra_S, data=d1 )), digits=10)
print(summary( MC3_sleepy <- lm( sleepy_bias ~ 1 + retro_time + sleepy_mean_S + sleepy_max_S + sleepy_rec_S + sex_S + age_S + Avt_S + Lst_S + edu_S + neur_S + extra_S, data=d1 )), digits=10)
print(summary( MC3_nerves <- lm( nrvous_bias ~ 1 + retro_time + nerves_mean_S + nerves_max_S + nerves_rec_S + sex_S + age_S + Avt_S + Lst_S + edu_S + neur_S + extra_S, data=d1 )), digits=10)
print(summary( MC3_irrtd <- lm( irrtatd_bias ~ 1 + retro_time + irrtd_mean_S + irrtd_max_S + irrtd_rec_S + sex_S + age_S + Avt_S + Lst_S + edu_S + neur_S + extra_S, data=d1 )), digits=10)

###### Results from here on need to be added to Word document 

###################################################################################################################
##  Predicting bias in emotion reports WITH TIME and INTERACTIONS with max affect, recent affect, and personality
##  -- all covariates, emotion cateogories, and discrete emotions -- variables centered instead of standardizing
###################################################################################################################

summary(hana_ints_all_n <- lmer(hana_bias ~ 1 + retro_time + hana_mean_C + hana_rec_C + hana_max_C + neur_C + retro_time*hana_rec_C + retro_time*hana_max_C + retro_time*neur_C + sex + age_C + Avt_C + Lst_C + edu_C + (1 + retro_time | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hana_ints_all_e <- lmer(hana_bias ~ 1 + retro_time + hana_mean_C + hana_rec_C + hana_max_C + extra_C + retro_time*hana_rec_C + retro_time*hana_max_C + retro_time*extra_C + sex + age_C + Avt_C + Lst_C + edu_C + (1 + retro_time | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(nerves_ints_all_n <- lmer(nrvous_bias ~ 1 + retro_time + nerves_mean_C + nerves_rec_C + nerves_max_C + neur_C + retro_time*nerves_rec_C + retro_time*nerves_max_C + retro_time*neur_C + sex + age_C + Avt_C + Lst_C + edu_C + (1 + retro_time | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(nerves_ints_all_e <- lmer(nrvous_bias ~ 1 + retro_time + nerves_mean_C + nerves_rec_C + nerves_max_C + extra_C + retro_time*nerves_rec_C + retro_time*nerves_max_C + retro_time*extra_C + sex + age_C + Avt_C + Lst_C + edu_C + (1 + retro_time | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(irrtd_ints_all_n <- lmer(irrtatd_bias ~ 1 + retro_time + irrtd_mean_C + irrtd_rec_C + irrtd_max_C + neur_C + retro_time*irrtd_rec_C + retro_time*irrtd_max_C + retro_time*neur_C + sex + age_C + Avt_C + Lst_C + edu_C + (1 + retro_time | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(irrtd_ints_all_e <- lmer(irrtatd_bias ~ 1 + retro_time + irrtd_mean_C + irrtd_rec_C + irrtd_max_C + extra_C + retro_time*irrtd_rec_C + retro_time*irrtd_max_C + retro_time*extra_C + sex + age_C + Avt_C + Lst_C + edu_C + (1 + retro_time | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))

summary(hapa_ints_all_n <- lmer(hapa_bias ~ 1 + retro_time + hapa_mean_C + hapa_rec_C + hapa_max_C + neur_C + retro_time*hapa_rec_C + retro_time*hapa_max_C + retro_time*neur_C + sex + age_C + Avt_C + Lst_C + edu_C + (1 + retro_time | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hapa_ints_all_e <- lmer(hapa_bias ~ 1 + retro_time + hapa_mean_C + hapa_rec_C + hapa_max_C + extra_C + retro_time*hapa_rec_C + retro_time*hapa_max_C + retro_time*extra_C + sex + age_C + Avt_C + Lst_C + edu_C + (1 + retro_time | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(happy_ints_all_n <- lmer(happy_bias ~ 1 + retro_time + happy_mean_C + happy_rec_C + happy_max_C + neur_C + retro_time*happy_rec_C + retro_time*happy_max_C + retro_time*neur_C + sex + age_C + Avt_C + Lst_C + edu_C + (1 + retro_time | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(happy_ints_all_e <- lmer(happy_bias ~ 1 + retro_time + happy_mean_C + happy_rec_C + happy_max_C + extra_C + retro_time*happy_rec_C + retro_time*happy_max_C + retro_time*extra_C + sex + age_C + Avt_C + Lst_C + edu_C + (1 + retro_time | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(exctd_ints_all_n <- lmer(exctd_bias ~ 1 + retro_time + exctd_mean_C + exctd_rec_C + exctd_max_C + neur_C + retro_time*exctd_rec_C + retro_time*exctd_max_C + retro_time*neur_C + sex + age_C + Avt_C + Lst_C + edu_C + (1 + retro_time | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(exctd_ints_all_e <- lmer(exctd_bias ~ 1 + retro_time + exctd_mean_C + exctd_rec_C + exctd_max_C + extra_C + retro_time*exctd_rec_C + retro_time*exctd_max_C + retro_time*extra_C + sex + age_C + Avt_C + Lst_C + edu_C + (1 + retro_time | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))

summary(lana_ints_all_n <- lmer(lana_bias ~ 1 + retro_time + lana_mean_C + lana_rec_C + lana_max_C + neur_C + retro_time*lana_rec_C + retro_time*lana_max_C + retro_time*neur_C + sex + age_C + Avt_C + Lst_C + edu_C + (1 + retro_time | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lana_ints_all_e <- lmer(lana_bias ~ 1 + retro_time + lana_mean_C + lana_rec_C + lana_max_C + extra_C + retro_time*lana_rec_C + retro_time*lana_max_C + retro_time*extra_C + sex + age_C + Avt_C + Lst_C + edu_C + (1 + retro_time | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))

summary(lapa_ints_all_n <- lmer(lapa_bias ~ 1 + retro_time + lapa_mean_C + lapa_rec_C + lapa_max_C + neur_C + retro_time*lapa_rec_C + retro_time*lapa_max_C + retro_time*neur_C + sex + age_C + Avt_C + Lst_C + edu_C + (1 + retro_time | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lapa_ints_all_e <- lmer(lapa_bias ~ 1 + retro_time + lapa_mean_C + lapa_rec_C + lapa_max_C + extra_C + retro_time*lapa_rec_C + retro_time*lapa_max_C + retro_time*extra_C + sex + age_C + Avt_C + Lst_C + edu_C + (1 + retro_time | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))

## Models with elapsed time expressed in weeks instead of days
d1$retro_wks <- d1$retro_time * 1/7

summary(hana_ints_all_n_wks <- lmer(hana_bias ~ 1 + retro_wks + hana_mean_C + hana_rec_C + hana_max_C + neur_C + retro_wks*hana_rec_C + retro_wks*hana_max_C + retro_wks*neur_C + sex + age_C + Avt_C + Lst_C + edu_C + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hana_ints_all_e_wks <- lmer(hana_bias ~ 1 + retro_wks + hana_mean_C + hana_rec_C + hana_max_C + extra_C + retro_wks*hana_rec_C + retro_wks*hana_max_C + retro_wks*extra_C + sex + age_C + Avt_C + Lst_C + edu_C + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lana_ints_all_n_wks <- lmer(lana_bias ~ 1 + retro_wks + lana_mean_C + lana_rec_C + lana_max_C + neur_C + retro_wks*lana_rec_C + retro_wks*lana_max_C + retro_wks*neur_C + sex + age_C + Avt_C + Lst_C + edu_C + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lana_ints_all_e_wks <- lmer(lana_bias ~ 1 + retro_wks + lana_mean_C + lana_rec_C + lana_max_C + extra_C + retro_wks*lana_rec_C + retro_wks*lana_max_C + retro_wks*extra_C + sex + age_C + Avt_C + Lst_C + edu_C + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hapa_ints_all_n_wks <- lmer(hapa_bias ~ 1 + retro_wks + hapa_mean_C + hapa_rec_C + hapa_max_C + neur_C + retro_wks*hapa_rec_C + retro_wks*hapa_max_C + retro_wks*neur_C + sex + age_C + Avt_C + Lst_C + edu_C + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hapa_ints_all_e_wks <- lmer(hapa_bias ~ 1 + retro_wks + hapa_mean_C + hapa_rec_C + hapa_max_C + extra_C + retro_wks*hapa_rec_C + retro_wks*hapa_max_C + retro_wks*extra_C + sex + age_C + Avt_C + Lst_C + edu_C + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lapa_ints_all_n_wks <- lmer(lapa_bias ~ 1 + retro_wks + lapa_mean_C + lapa_rec_C + lapa_max_C + neur_C + retro_wks*lapa_rec_C + retro_wks*lapa_max_C + retro_wks*neur_C + sex + age_C + Avt_C + Lst_C + edu_C + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lapa_ints_all_e_wks <- lmer(lapa_bias ~ 1 + retro_wks + lapa_mean_C + lapa_rec_C + lapa_max_C + extra_C + retro_wks*lapa_rec_C + retro_wks*lapa_max_C + retro_wks*extra_C + sex + age_C + Avt_C + Lst_C + edu_C + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))

########################################################
# FINAL SPSP POSTER MODELS: N and E in the same model  #
########################################################

summary(hana_ints_all_wks <- lmer(hana_bias ~ 1 + retro_wks + hana_mean_C + hana_rec_C + hana_max_C + neur_C + extra_C + retro_wks*hana_rec_C + retro_wks*hana_max_C + retro_wks*neur_C + retro_wks*extra_C + sex + age_C + Avt_C + Lst_C + edu_C + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lana_ints_all_wks <- lmer(lana_bias ~ 1 + retro_wks + lana_mean_C + lana_rec_C + lana_max_C + neur_C + extra_C + retro_wks*lana_rec_C + retro_wks*lana_max_C + retro_wks*neur_C + retro_wks*extra_C + sex + age_C + Avt_C + Lst_C + edu_C + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hapa_ints_all_wks <- lmer(hapa_bias ~ 1 + retro_wks + hapa_mean_C + hapa_rec_C + hapa_max_C + neur_C + extra_C + retro_wks*hapa_rec_C + retro_wks*hapa_max_C + retro_wks*neur_C + retro_wks*extra_C + sex + age_C + Avt_C + Lst_C + edu_C + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lapa_ints_all_wks <- lmer(lapa_bias ~ 1 + retro_wks + lapa_mean_C + lapa_rec_C + lapa_max_C + neur_C + extra_C + retro_wks*lapa_rec_C + retro_wks*lapa_max_C + retro_wks*neur_C + retro_wks*extra_C + sex + age_C + Avt_C + Lst_C + edu_C + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))


########################################################
              # MANUSCRIPT MODELS #
########################################################

## MODELS 1 - no covariates
summary(hapa_1c <- lmer(hapa_bias ~ 1 + retro_wks + hapa_mean_C + hapa_rec_C + hapa_max_C + retro_wks*hapa_rec_C + retro_wks*hapa_max_C + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lapa_1c <- lmer(lapa_bias ~ 1 + retro_wks + lapa_mean_C + lapa_rec_C + lapa_max_C + retro_wks*lapa_rec_C + retro_wks*lapa_max_C + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hana_1c <- lmer(hana_bias ~ 1 + retro_wks + hana_mean_C + hana_rec_C + hana_max_C + retro_wks*hana_rec_C + retro_wks*hana_max_C + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lana_1c <- lmer(lana_bias ~ 1 + retro_wks + lana_mean_C + lana_rec_C + lana_max_C + retro_wks*lana_rec_C + retro_wks*lana_max_C + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))

## MODELS 1 - with covariates
summary(hapa_1c <- lmer(hapa_bias ~ 1 + retro_wks + hapa_mean_C + hapa_rec_C + hapa_max_C + retro_wks*hapa_rec_C + retro_wks*hapa_max_C + sex + age_C + Avt_C + Lst_C + edu_C + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lapa_1c <- lmer(lapa_bias ~ 1 + retro_wks + lapa_mean_C + lapa_rec_C + lapa_max_C + retro_wks*lapa_rec_C + retro_wks*lapa_max_C + sex + age_C + Avt_C + Lst_C + edu_C + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hana_1c <- lmer(hana_bias ~ 1 + retro_wks + hana_mean_C + hana_rec_C + hana_max_C + retro_wks*hana_rec_C + retro_wks*hana_max_C + sex + age_C + Avt_C + Lst_C + edu_C + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lana_1c <- lmer(lana_bias ~ 1 + retro_wks + lana_mean_C + lana_rec_C + lana_max_C + retro_wks*lana_rec_C + retro_wks*lana_max_C + sex + age_C + Avt_C + Lst_C + edu_C + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))

## MODELS 1 - with covariates (truncated set)
summary(hapa_1c <- lmer(hapa_bias ~ 1 + retro_wks + hapa_mean_C + hapa_rec_C + hapa_max_C + retro_wks*hapa_rec_C + sex + age_C + Avt_C + Lst_C + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lapa_1c <- lmer(lapa_bias ~ 1 + retro_wks + lapa_mean_C + lapa_rec_C + lapa_max_C + retro_wks*lapa_rec_C + sex + age_C + Avt_C + Lst_C + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hana_1c <- lmer(hana_bias ~ 1 + retro_wks + hana_mean_C + hana_rec_C + hana_max_C + retro_wks*hana_rec_C + sex + age_C + Avt_C + Lst_C + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lana_1c <- lmer(lana_bias ~ 1 + retro_wks + lana_mean_C + lana_rec_C + lana_max_C + retro_wks*lana_rec_C + sex + age_C + Avt_C + Lst_C + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))

## Empty models (look at ICC)
summary(hapa_emp <- lmer(hapa_bias ~ 1 + (1 | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lapa_emp <- lmer(lapa_bias ~ 1 + (1 | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hana_emp <- lmer(hana_bias ~ 1 + (1 | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lana_emp <- lmer(lana_bias ~ 1 + (1 | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))

## Testing the intercept variance (does not work)
FULL <- lmer(hapa_bias ~ 1 + retro_wks + (1 + retro_wks | id), data=d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
intercepts <- lmList(hapa_bias ~ 1 + retro_wks | id, data=na.omit(d1))
Bols <- summary(intercepts)$coefficients[,1,1]
Vols <- summary(intercepts)$coefficients[,2,1]^2 # estimate of sampling variance
sum( (Bols - fixef(FULL)[1])^2/Vols) # expected value for this is the df

# No random time slopes
summary(hapa_1c <- lmer(hapa_bias ~ 1 + retro_wks + hapa_mean_C + hapa_rec_C + hapa_max_C + retro_wks*hapa_rec_C + retro_wks*hapa_max_C + sex + age_C + Avt_C + Lst_C + edu_C + (1 | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lapa_1c <- lmer(lapa_bias ~ 1 + retro_wks + lapa_mean_C + lapa_rec_C + lapa_max_C + retro_wks*lapa_rec_C + retro_wks*lapa_max_C + sex + age_C + Avt_C + Lst_C + edu_C + (1 | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hana_1c <- lmer(hana_bias ~ 1 + retro_wks + hana_mean_C + hana_rec_C + hana_max_C + retro_wks*hana_rec_C + retro_wks*hana_max_C + sex + age_C + Avt_C + Lst_C + edu_C + (1 | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lana_1c <- lmer(lana_bias ~ 1 + retro_wks + lana_mean_C + lana_rec_C + lana_max_C + retro_wks*lana_rec_C + retro_wks*lana_max_C + sex + age_C + Avt_C + Lst_C + edu_C + (1 | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))

## MODELS 2 - no covariates
summary(hapa_ints_all_wks_n <- lmer(hapa_bias ~ 1 + retro_wks + hapa_mean_C + hapa_rec_C + hapa_max_C + neur_C + extra_C + retro_wks*hapa_rec_C + retro_wks*hapa_max_C + retro_wks*neur_C + retro_wks*extra_C + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lapa_ints_all_wks_n <- lmer(lapa_bias ~ 1 + retro_wks + lapa_mean_C + lapa_rec_C + lapa_max_C + neur_C + extra_C + retro_wks*lapa_rec_C + retro_wks*lapa_max_C + retro_wks*neur_C + retro_wks*extra_C + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hana_ints_all_wks_n <- lmer(hana_bias ~ 1 + retro_wks + hana_mean_C + hana_rec_C + hana_max_C + neur_C + extra_C + retro_wks*hana_rec_C + retro_wks*hana_max_C + retro_wks*neur_C + retro_wks*extra_C + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lana_ints_all_wks_n <- lmer(lana_bias ~ 1 + retro_wks + lana_mean_C + lana_rec_C + lana_max_C + neur_C + extra_C + retro_wks*lana_rec_C + retro_wks*lana_max_C + retro_wks*neur_C + retro_wks*extra_C + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))

## MODELS 2 - with covariates  --- same as SPSP poster models

# No random time slopes
summary(hana_ints_all_wks_f <- lmer(hana_bias ~ 1 + retro_wks + hana_mean_C + hana_rec_C + hana_max_C + neur_C + extra_C + retro_wks*hana_rec_C + retro_wks*hana_max_C + retro_wks*neur_C + retro_wks*extra_C + sex + age_C + Avt_C + Lst_C + edu_C + (1 | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lana_ints_all_wks_f <- lmer(lana_bias ~ 1 + retro_wks + lana_mean_C + lana_rec_C + lana_max_C + neur_C + extra_C + retro_wks*lana_rec_C + retro_wks*lana_max_C + retro_wks*neur_C + retro_wks*extra_C + sex + age_C + Avt_C + Lst_C + edu_C + (1 | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hapa_ints_all_wks_f <- lmer(hapa_bias ~ 1 + retro_wks + hapa_mean_C + hapa_rec_C + hapa_max_C + neur_C + extra_C + retro_wks*hapa_rec_C + retro_wks*hapa_max_C + retro_wks*neur_C + retro_wks*extra_C + sex + age_C + Avt_C + Lst_C + edu_C + (1 | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lapa_ints_all_wks_f <- lmer(lapa_bias ~ 1 + retro_wks + lapa_mean_C + lapa_rec_C + lapa_max_C + neur_C + extra_C + retro_wks*lapa_rec_C + retro_wks*lapa_max_C + retro_wks*neur_C + retro_wks*extra_C + sex + age_C + Avt_C + Lst_C + edu_C + (1 | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))

# No emotional salience x time interactions
summary(hana_ints_all_wks <- lmer(hana_bias ~ 1 + retro_wks + hana_mean_C + hana_rec_C + hana_max_C + neur_C + extra_C + retro_wks*neur_C + retro_wks*extra_C + sex + age_C + Avt_C + Lst_C + edu_C + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lana_ints_all_wks <- lmer(lana_bias ~ 1 + retro_wks + lana_mean_C + lana_rec_C + lana_max_C + neur_C + extra_C + retro_wks*neur_C + retro_wks*extra_C + sex + age_C + Avt_C + Lst_C + edu_C + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hapa_ints_all_wks <- lmer(hapa_bias ~ 1 + retro_wks + hapa_mean_C + hapa_rec_C + hapa_max_C + neur_C + extra_C + retro_wks*neur_C + retro_wks*extra_C + sex + age_C + Avt_C + Lst_C + edu_C + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lapa_ints_all_wks <- lmer(lapa_bias ~ 1 + retro_wks + lapa_mean_C + lapa_rec_C + lapa_max_C + neur_C + extra_C + retro_wks*neur_C + retro_wks*extra_C + sex + age_C + Avt_C + Lst_C + edu_C + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))

##### USE FOR MS ##### Increasing power by removing education covariate and peak effect x time interaction 
summary(hapa_ints_all_wks <- lmer(hapa_bias ~ 1 + retro_wks + hapa_mean_C + hapa_rec_C + hapa_max_C + neur_C + extra_C + retro_wks*hapa_rec_C + retro_wks*neur_C + retro_wks*extra_C + sex + age_C + Avt_C + Lst_C + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lapa_ints_all_wks <- lmer(lapa_bias ~ 1 + retro_wks + lapa_mean_C + lapa_rec_C + lapa_max_C + neur_C + extra_C + retro_wks*lapa_rec_C + retro_wks*neur_C + retro_wks*extra_C + sex + age_C + Avt_C + Lst_C + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hana_ints_all_wks <- lmer(hana_bias ~ 1 + retro_wks + hana_mean_C + hana_rec_C + hana_max_C + neur_C + extra_C + retro_wks*hana_rec_C + retro_wks*neur_C + retro_wks*extra_C + sex + age_C + Avt_C + Lst_C + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lana_ints_all_wks <- lmer(lana_bias ~ 1 + retro_wks + lana_mean_C + lana_rec_C + lana_max_C + neur_C + extra_C + retro_wks*lana_rec_C + retro_wks*neur_C + retro_wks*extra_C + sex + age_C + Avt_C + Lst_C + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))

## JAN 2016 - MODELS 2 - ethnicity (European vs not) added as covariate
summary(hapa_ints_all_wks <- lmer(hapa_bias ~ 1 + retro_wks + hapa_mean_C + hapa_rec_C + hapa_max_C + neur_C + extra_C + retro_wks*hapa_rec_C + retro_wks*neur_C + retro_wks*extra_C + sex + age_C + Avt_C + Lst_C + is_cauc + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lapa_ints_all_wks <- lmer(lapa_bias ~ 1 + retro_wks + lapa_mean_C + lapa_rec_C + lapa_max_C + neur_C + extra_C + retro_wks*lapa_rec_C + retro_wks*neur_C + retro_wks*extra_C + sex + age_C + Avt_C + Lst_C + is_cauc + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hana_ints_all_wks <- lmer(hana_bias ~ 1 + retro_wks + hana_mean_C + hana_rec_C + hana_max_C + neur_C + extra_C + retro_wks*hana_rec_C + retro_wks*neur_C + retro_wks*extra_C + sex + age_C + Avt_C + Lst_C + is_cauc + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lana_ints_all_wks <- lmer(lana_bias ~ 1 + retro_wks + lana_mean_C + lana_rec_C + lana_max_C + neur_C + extra_C + retro_wks*lana_rec_C + retro_wks*neur_C + retro_wks*extra_C + sex + age_C + Avt_C + Lst_C + is_cauc + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))


## FINAL JAN 2016 - MODELS 2 - ethnicity (European vs not) added as covariate, with retro_wks * max_affect PLUS education
summary(hapa_j <- lmer(hapa_bias ~ 1 + retro_wks + hapa_mean_C + hapa_rec_C + hapa_max_C + neur_C + extra_C + retro_wks*hapa_rec_C + retro_wks*hapa_max_C + retro_wks*neur_C + retro_wks*extra_C + sex + age_C + Avt_C + Lst_C + is_cauc + edu + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lapa_j <- lmer(lapa_bias ~ 1 + retro_wks + lapa_mean_C + lapa_rec_C + lapa_max_C + neur_C + extra_C + retro_wks*lapa_rec_C + retro_wks*lapa_max_C + retro_wks*neur_C + retro_wks*extra_C + sex + age_C + Avt_C + Lst_C + is_cauc + edu + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hana_j <- lmer(hana_bias ~ 1 + retro_wks + hana_mean_C + hana_rec_C + hana_max_C + neur_C + extra_C + retro_wks*hana_rec_C + retro_wks*hana_max_C + retro_wks*neur_C + retro_wks*extra_C + sex + age_C + Avt_C + Lst_C + is_cauc + edu + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lana_j <- lmer(lana_bias ~ 1 + retro_wks + lana_mean_C + lana_rec_C + lana_max_C + neur_C + extra_C + retro_wks*lana_rec_C + retro_wks*lana_max_C + retro_wks*neur_C + retro_wks*extra_C + sex + age_C + Avt_C + Lst_C + is_cauc + edu + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))


## JAN 2016 - MODELS 2 - after removing mean affect covariate
summary(hapa_j <- lmer(hapa_bias ~ 1 + retro_wks + hapa_rec_C + hapa_max_C + neur_C + extra_C + retro_wks*hapa_rec_C + retro_wks*hapa_max_C + retro_wks*neur_C + retro_wks*extra_C + sex + age_C + Avt_C + Lst_C + is_cauc + edu + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lapa_j <- lmer(lapa_bias ~ 1 + retro_wks + lapa_rec_C + lapa_max_C + neur_C + extra_C + retro_wks*lapa_rec_C + retro_wks*lapa_max_C + retro_wks*neur_C + retro_wks*extra_C + sex + age_C + Avt_C + Lst_C + is_cauc + edu + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hana_j <- lmer(hana_bias ~ 1 + retro_wks + hana_rec_C + hana_max_C + neur_C + extra_C + retro_wks*hana_rec_C + retro_wks*hana_max_C + retro_wks*neur_C + retro_wks*extra_C + sex + age_C + Avt_C + Lst_C + is_cauc + edu + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lana_j <- lmer(lana_bias ~ 1 + retro_wks + lana_rec_C + lana_max_C + neur_C + extra_C + retro_wks*lana_rec_C + retro_wks*lana_max_C + retro_wks*neur_C + retro_wks*extra_C + sex + age_C + Avt_C + Lst_C + is_cauc + edu + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))


## FINAL JAN 2016 - MODELS 2 - looking at retrospective affect itself instead of discrepancy between retrospective and avg momentary affect
summary(hapa_j <- lmer(hapa_ret ~ 1 + retro_wks + hapa_mean_C + hapa_rec_C + hapa_max_C + neur_C + extra_C + retro_wks*hapa_rec_C + retro_wks*hapa_max_C + retro_wks*neur_C + retro_wks*extra_C + sex + age_C + Avt_C + Lst_C + is_cauc + edu + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lapa_j <- lmer(lapa_ret ~ 1 + retro_wks + lapa_mean_C + lapa_rec_C + lapa_max_C + neur_C + extra_C + retro_wks*lapa_rec_C + retro_wks*lapa_max_C + retro_wks*neur_C + retro_wks*extra_C + sex + age_C + Avt_C + Lst_C + is_cauc + edu + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hana_j <- lmer(hana_ret ~ 1 + retro_wks + hana_mean_C + hana_rec_C + hana_max_C + neur_C + extra_C + retro_wks*hana_rec_C + retro_wks*hana_max_C + retro_wks*neur_C + retro_wks*extra_C + sex + age_C + Avt_C + Lst_C + is_cauc + edu + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lana_j <- lmer(lana_ret ~ 1 + retro_wks + lana_mean_C + lana_rec_C + lana_max_C + neur_C + extra_C + retro_wks*lana_rec_C + retro_wks*lana_max_C + retro_wks*neur_C + retro_wks*extra_C + sex + age_C + Avt_C + Lst_C + is_cauc + edu + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))


## LATEST JAN 2016 - DISCRETE EMOTION MODELS 2 - ethnicity (European vs not) added as covariate, with retro_wks * max_affect
summary(happy_ints_all_wks <- lmer(happy_bias ~ 1 + retro_wks + happy_mean_C + happy_rec_C + happy_max_C + neur_C + extra_C + retro_wks*happy_rec_C + retro_wks*happy_max_C + retro_wks*neur_C + retro_wks*extra_C + sex + age_C + Avt_C + Lst_C + is_cauc + edu + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(exctd_ints_all_wks <- lmer(exctd_bias ~ 1 + retro_wks + exctd_mean_C + exctd_rec_C + exctd_max_C + neur_C + extra_C + retro_wks*exctd_rec_C + retro_wks*exctd_max_C + retro_wks*neur_C + retro_wks*extra_C + sex + age_C + Avt_C + Lst_C + is_cauc + edu + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(calm_ints_all_wks <- lmer(calm_bias ~ 1 + retro_wks + calm_mean_C + calm_rec_C + calm_max_C + neur_C + extra_C + retro_wks*calm_rec_C + retro_wks*calm_max_C + retro_wks*neur_C + retro_wks*extra_C + sex + age_C + Avt_C + Lst_C + is_cauc + edu + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(quiet_ints_all_wks <- lmer(quiet_bias ~ 1 + retro_wks + quiet_mean_C + quiet_rec_C + quiet_max_C + neur_C + extra_C + retro_wks*quiet_rec_C + retro_wks*quiet_max_C + retro_wks*neur_C + retro_wks*extra_C + sex + age_C + Avt_C + Lst_C + is_cauc + edu + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(nrvous_ints_all_wks <- lmer(nrvous_bias ~ 1 + retro_wks + nrvous_mean_C + nrvous_rec_C + nrvous_max_C + neur_C + extra_C + retro_wks*nrvous_rec_C + retro_wks*nrvous_max_C + retro_wks*neur_C + retro_wks*extra_C + sex + age_C + Avt_C + Lst_C + is_cauc + edu + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(irrtatd_ints_all_wks <- lmer(irrtatd_bias ~ 1 + retro_wks + irrtatd_mean_C + irrtatd_rec_C + irrtatd_max_C + neur_C + extra_C + retro_wks*irrtatd_rec_C + retro_wks*irrtatd_max_C + retro_wks*neur_C + retro_wks*extra_C + sex + age_C + Avt_C + Lst_C + is_cauc + edu + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(sad_ints_all_wks <- lmer(sad_bias ~ 1 + retro_wks + sad_mean_C + sad_rec_C + sad_max_C + neur_C + extra_C + retro_wks*sad_rec_C + retro_wks*sad_max_C + retro_wks*neur_C + retro_wks*extra_C + sex + age_C + Avt_C + Lst_C + is_cauc + edu + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(sleepy_ints_all_wks <- lmer(sleepy_bias ~ 1 + retro_wks + sleepy_mean_C + sleepy_rec_C + sleepy_max_C + neur_C + extra_C + retro_wks*sleepy_rec_C + retro_wks*sleepy_max_C + retro_wks*neur_C + retro_wks*extra_C + sex + age_C + Avt_C + Lst_C + is_cauc + edu + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))



##### COMPARING EXIT & FOLLOWUP instead of treating time as continuous #####

# Discrete emotions

summary(happy_3 <- lmer(happy_bias ~ 1 + is_fu + happy_mean_C + happy_rec_C + happy_max_C + neur_C + extra_C + is_fu*happy_rec_C + is_fu*happy_max_C + is_fu*neur_C + is_fu*extra_C + sex + age_C + Avt_C + Lst_C + is_cauc + (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(exctd_3 <- lmer(exctd_bias ~ 1 + is_fu + exctd_mean_C + exctd_rec_C + exctd_max_C + neur_C + extra_C + is_fu*exctd_rec_C + is_fu*exctd_max_C + is_fu*neur_C + is_fu*extra_C + sex + age_C + Avt_C + Lst_C + is_cauc + (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(calm_3 <- lmer(calm_bias ~ 1 + is_fu + calm_mean_C + calm_rec_C + calm_max_C + neur_C + extra_C + is_fu*calm_rec_C + is_fu*calm_max_C + is_fu*neur_C + is_fu*extra_C + sex + age_C + Avt_C + Lst_C + is_cauc + (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(quiet_3 <- lmer(quiet_bias ~ 1 + is_fu + quiet_mean_C + quiet_rec_C + quiet_max_C + neur_C + extra_C + is_fu*quiet_rec_C + is_fu*quiet_max_C + is_fu*neur_C + is_fu*extra_C + sex + age_C + Avt_C + Lst_C + is_cauc + (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(nrvous_3 <- lmer(nrvous_bias ~ 1 + is_fu + nrvous_mean_C + nrvous_rec_C + nrvous_max_C + neur_C + extra_C + is_fu*nrvous_rec_C + is_fu*nrvous_max_C + is_fu*neur_C + is_fu*extra_C + sex + age_C + Avt_C + Lst_C + is_cauc + (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(irrtatd_3 <- lmer(irrtatd_bias ~ 1 + is_fu + irrtatd_mean_C + irrtatd_rec_C + irrtatd_max_C + neur_C + extra_C + is_fu*irrtatd_rec_C + is_fu*irrtatd_max_C + is_fu*neur_C + is_fu*extra_C + sex + age_C + Avt_C + Lst_C + is_cauc + (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(sad_3 <- lmer(sad_bias ~ 1 + is_fu + sad_mean_C + sad_rec_C + sad_max_C + neur_C + extra_C + is_fu*sad_rec_C + is_fu*sad_max_C + is_fu*neur_C + is_fu*extra_C + sex + age_C + Avt_C + Lst_C + is_cauc + (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(sleepy_3 <- lmer(sleepy_bias ~ 1 + is_fu + sleepy_mean_C + sleepy_rec_C + sleepy_max_C + neur_C + extra_C + is_fu*sleepy_rec_C + is_fu*sleepy_max_C + is_fu*neur_C + is_fu*extra_C + sex + age_C + Avt_C + Lst_C + is_cauc + (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))

# Affect categories
summary(hapa_k <- lmer(hapa_bias ~ 1 + is_fu + hapa_mean_C + hapa_rec_C + hapa_max_C + neur_C + extra_C + is_fu*hapa_rec_C + is_fu*hapa_max_C + is_fu*neur_C + is_fu*extra_C + sex + age_C + Avt_C + Lst_C + is_cauc + edu + (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lapa_k <- lmer(lapa_bias ~ 1 + is_fu + lapa_mean_C + lapa_rec_C + lapa_max_C + neur_C + extra_C + is_fu*lapa_rec_C + is_fu*lapa_max_C + is_fu*neur_C + is_fu*extra_C + sex + age_C + Avt_C + Lst_C + is_cauc + edu + (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hana_k <- lmer(hana_bias ~ 1 + is_fu + hana_mean_C + hana_rec_C + hana_max_C + neur_C + extra_C + is_fu*hana_rec_C + is_fu*hana_max_C + is_fu*neur_C + is_fu*extra_C + sex + age_C + Avt_C + Lst_C + is_cauc + edu + (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lana_k <- lmer(lana_bias ~ 1 + is_fu + lana_mean_C + lana_rec_C + lana_max_C + neur_C + extra_C + is_fu*lana_rec_C + is_fu*lana_max_C + is_fu*neur_C + is_fu*extra_C + sex + age_C + Avt_C + Lst_C + is_cauc + edu + (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))

### Affect categories - without demographic covariates
summary(hapa_3 <- lmer(hapa_bias ~ 1 + is_fu + hapa_mean_C + hapa_rec_C + hapa_max_C + neur_C + extra_C + is_fu*hapa_rec_C + is_fu*hapa_max_C + is_fu*neur_C + is_fu*extra_C + (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lapa_3 <- lmer(lapa_bias ~ 1 + is_fu + lapa_mean_C + lapa_rec_C + lapa_max_C + neur_C + extra_C + is_fu*lapa_rec_C + is_fu*lapa_max_C + is_fu*neur_C + is_fu*extra_C + (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hana_3 <- lmer(hana_bias ~ 1 + is_fu + hana_mean_C + hana_rec_C + hana_max_C + neur_C + extra_C + is_fu*hana_rec_C + is_fu*hana_max_C + is_fu*neur_C + is_fu*extra_C + (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lana_3 <- lmer(lana_bias ~ 1 + is_fu + lana_mean_C + lana_rec_C + lana_max_C + neur_C + extra_C + is_fu*lana_rec_C + is_fu*lana_max_C + is_fu*neur_C + is_fu*extra_C + (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))

### Affect categories - without education and max*time (for parsimony)
summary(hapa_k <- lmer(hapa_bias ~ 1 + is_fu + hapa_mean_C + hapa_rec_C + hapa_max_C + neur_C + extra_C + is_fu*hapa_rec_C + is_fu*neur_C + is_fu*extra_C + sex + age_C + Avt_C + Lst_C + is_cauc + (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lapa_k <- lmer(lapa_bias ~ 1 + is_fu + lapa_mean_C + lapa_rec_C + lapa_max_C + neur_C + extra_C + is_fu*lapa_rec_C + is_fu*neur_C + is_fu*extra_C + sex + age_C + Avt_C + Lst_C + is_cauc + (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hana_k <- lmer(hana_bias ~ 1 + is_fu + hana_mean_C + hana_rec_C + hana_max_C + neur_C + extra_C + is_fu*hana_rec_C + is_fu*neur_C + is_fu*extra_C + sex + age_C + Avt_C + Lst_C + is_cauc + (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lana_k <- lmer(lana_bias ~ 1 + is_fu + lana_mean_C + lana_rec_C + lana_max_C + neur_C + extra_C + is_fu*lana_rec_C + is_fu*neur_C + is_fu*extra_C + sex + age_C + Avt_C + Lst_C + is_cauc + (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))

### Affect categories - without gender, education, and max*time (for further parsimony!)
summary(hapa_kr <- lmer(hapa_bias ~ 1 + is_fu + hapa_mean_C + hapa_rec_C + hapa_max_C + neur_C + extra_C + is_fu*hapa_rec_C + is_fu*neur_C + is_fu*extra_C + age_C + Avt_C + Lst_C + is_cauc + (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lapa_kr <- lmer(lapa_bias ~ 1 + is_fu + lapa_mean_C + lapa_rec_C + lapa_max_C + neur_C + extra_C + is_fu*lapa_rec_C + is_fu*neur_C + is_fu*extra_C + age_C + Avt_C + Lst_C + is_cauc + (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hana_kr <- lmer(hana_bias ~ 1 + is_fu + hana_mean_C + hana_rec_C + hana_max_C + neur_C + extra_C + is_fu*hana_rec_C + is_fu*neur_C + is_fu*extra_C + age_C + Avt_C + Lst_C + is_cauc + (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lana_kr <- lmer(lana_bias ~ 1 + is_fu + lana_mean_C + lana_rec_C + lana_max_C + neur_C + extra_C + is_fu*lana_rec_C + is_fu*neur_C + is_fu*extra_C + age_C + Avt_C + Lst_C + is_cauc + (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))


# Unspecified models
summary(hapa_uns <- lmer(hapa_bias ~ 1 + (1 | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lapa_uns <- lmer(lapa_bias ~ 1 + (1 | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hana_uns <- lmer(hana_bias ~ 1 + (1 | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lana_uns <- lmer(lana_bias ~ 1 + (1 | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))

# Simple slopes breakdown
sd(na.omit(d1$neur_C))
d1$neur_Hi <- d1$neur_C - sd(na.omit(d1$neur_C))
d1$neur_Lo <- d1$neur_C + sd(na.omit(d1$neur_C))
sd(na.omit(d1$extra_C))
d1$extra_Hi <- d1$extra_C - sd(na.omit(d1$extra_C))
d1$extra_Lo <- d1$extra_C + sd(na.omit(d1$extra_C))

# Simple slopes modified since SPSP poster to increase power (education covariate, and peak affect x time interaction removed)

summary(hapa_hiE_n <- lmer(hapa_bias ~ 1 + retro_wks + extra_Hi + neur_C + hapa_mean_C + hapa_rec_C + hapa_max_C + retro_wks*extra_Hi + retro_wks*neur_C + retro_wks*hapa_rec_C + sex + age_C + Avt_C + Lst_C + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hapa_loE_n <- lmer(hapa_bias ~ 1 + retro_wks + extra_Lo + neur_C + hapa_mean_C + hapa_rec_C + hapa_max_C + retro_wks*extra_Lo + retro_wks*neur_C + retro_wks*hapa_rec_C + sex + age_C + Avt_C + Lst_C + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hapa_hiN_e <- lmer(hapa_bias ~ 1 + retro_wks + neur_Hi + extra_C + hapa_mean_C + hapa_rec_C + hapa_max_C + retro_wks*neur_Hi + retro_wks*extra_C + retro_wks*hapa_rec_C + sex + age_C + Avt_C + Lst_C + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hapa_loN_e <- lmer(hapa_bias ~ 1 + retro_wks + neur_Lo + extra_C + hapa_mean_C + hapa_rec_C + hapa_max_C + retro_wks*neur_Lo + retro_wks*extra_C + retro_wks*hapa_rec_C + sex + age_C + Avt_C + Lst_C + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))

summary(lapa_hiE_n <- lmer(lapa_bias ~ 1 + retro_wks + extra_Hi + neur_C + lapa_mean_C + lapa_rec_C + lapa_max_C + retro_wks*extra_Hi + retro_wks*neur_C + retro_wks*lapa_rec_C + sex + age_C + Avt_C + Lst_C + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lapa_loE_n <- lmer(lapa_bias ~ 1 + retro_wks + extra_Lo + neur_C + lapa_mean_C + lapa_rec_C + lapa_max_C + retro_wks*extra_Lo + retro_wks*neur_C + retro_wks*lapa_rec_C + sex + age_C + Avt_C + Lst_C + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lapa_hiN_e <- lmer(lapa_bias ~ 1 + retro_wks + neur_Hi + extra_C + lapa_mean_C + lapa_rec_C + lapa_max_C + retro_wks*neur_Hi + retro_wks*extra_C + retro_wks*lapa_rec_C + sex + age_C + Avt_C + Lst_C + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lapa_loN_e <- lmer(lapa_bias ~ 1 + retro_wks + neur_Lo + extra_C + lapa_mean_C + lapa_rec_C + lapa_max_C + retro_wks*neur_Lo + retro_wks*extra_C + retro_wks*lapa_rec_C + sex + age_C + Avt_C + Lst_C + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))

summary(hana_hiN_e <- lmer(hana_bias ~ 1 + retro_wks + neur_Hi + extra_C + hana_mean_C + hana_rec_C + hana_max_C + retro_wks*neur_Hi + retro_wks*extra_C + retro_wks*hana_rec_C + sex + age_C + Avt_C + Lst_C + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hana_loN_e <- lmer(hana_bias ~ 1 + retro_wks + neur_Lo + extra_C + hana_mean_C + hana_rec_C + hana_max_C + retro_wks*neur_Lo + retro_wks*extra_C + retro_wks*hana_rec_C + sex + age_C + Avt_C + Lst_C + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hana_hiE_n <- lmer(hana_bias ~ 1 + retro_wks + extra_Hi + neur_C + hana_mean_C + hana_rec_C + hana_max_C + retro_wks*extra_Hi + retro_wks*neur_C + retro_wks*hana_rec_C + sex + age_C + Avt_C + Lst_C + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hana_loE_n <- lmer(hana_bias ~ 1 + retro_wks + extra_Lo + neur_C + hana_mean_C + hana_rec_C + hana_max_C + retro_wks*extra_Lo + retro_wks*neur_C + retro_wks*hana_rec_C + sex + age_C + Avt_C + Lst_C + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))

summary(lana_hiN_e <- lmer(lana_bias ~ 1 + retro_wks + neur_Hi + extra_C + lana_mean_C + lana_rec_C + lana_max_C + retro_wks*neur_Hi + retro_wks*extra_C + retro_wks*lana_rec_C + sex + age_C + Avt_C + Lst_C + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lana_loN_e <- lmer(lana_bias ~ 1 + retro_wks + neur_Lo + extra_C + lana_mean_C + lana_rec_C + lana_max_C + retro_wks*neur_Lo + retro_wks*extra_C + retro_wks*lana_rec_C + sex + age_C + Avt_C + Lst_C + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lana_hiE_n <- lmer(lana_bias ~ 1 + retro_wks + extra_Hi + neur_C + lana_mean_C + lana_rec_C + lana_max_C + retro_wks*extra_Hi + retro_wks*neur_C + retro_wks*lana_rec_C + sex + age_C + Avt_C + Lst_C + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lana_loE_n <- lmer(lana_bias ~ 1 + retro_wks + extra_Lo + neur_C + lana_mean_C + lana_rec_C + lana_max_C + retro_wks*extra_Lo + retro_wks*neur_C + retro_wks*lana_rec_C + sex + age_C + Avt_C + Lst_C + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))


## Jan 1 2016 - Simple slopes with clustered time (education covariate, sex covariate, and peak affect x time interaction removed; is_cauc covariate added)

summary(hapa_hiE_n <- lmer(hapa_bias ~ 1 + is_fu + extra_Hi + neur_C + hapa_mean_C + hapa_rec_C + hapa_max_C + is_fu*extra_Hi + is_fu*neur_C + is_fu*hapa_rec_C + age_C + Avt_C + Lst_C + is_cauc + (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hapa_loE_n <- lmer(hapa_bias ~ 1 + is_fu + extra_Lo + neur_C + hapa_mean_C + hapa_rec_C + hapa_max_C + is_fu*extra_Lo + is_fu*neur_C + is_fu*hapa_rec_C + age_C + Avt_C + Lst_C + is_cauc + (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hapa_hiN_e <- lmer(hapa_bias ~ 1 + is_fu + neur_Hi + extra_C + hapa_mean_C + hapa_rec_C + hapa_max_C + is_fu*neur_Hi + is_fu*extra_C + is_fu*hapa_rec_C + age_C + Avt_C + Lst_C + is_cauc + (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hapa_loN_e <- lmer(hapa_bias ~ 1 + is_fu + neur_Lo + extra_C + hapa_mean_C + hapa_rec_C + hapa_max_C + is_fu*neur_Lo + is_fu*extra_C + is_fu*hapa_rec_C + age_C + Avt_C + Lst_C + is_cauc + (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))

summary(lapa_hiE_n <- lmer(lapa_bias ~ 1 + is_fu + extra_Hi + neur_C + lapa_mean_C + lapa_rec_C + lapa_max_C + is_fu*extra_Hi + is_fu*neur_C + is_fu*lapa_rec_C + age_C + Avt_C + Lst_C + is_cauc + (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lapa_loE_n <- lmer(lapa_bias ~ 1 + is_fu + extra_Lo + neur_C + lapa_mean_C + lapa_rec_C + lapa_max_C + is_fu*extra_Lo + is_fu*neur_C + is_fu*lapa_rec_C + age_C + Avt_C + Lst_C + is_cauc + (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lapa_hiN_e <- lmer(lapa_bias ~ 1 + is_fu + neur_Hi + extra_C + lapa_mean_C + lapa_rec_C + lapa_max_C + is_fu*neur_Hi + is_fu*extra_C + is_fu*lapa_rec_C + age_C + Avt_C + Lst_C + is_cauc + (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lapa_loN_e <- lmer(lapa_bias ~ 1 + is_fu + neur_Lo + extra_C + lapa_mean_C + lapa_rec_C + lapa_max_C + is_fu*neur_Lo + is_fu*extra_C + is_fu*lapa_rec_C + age_C + Avt_C + Lst_C + is_cauc + (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))

summary(hana_hiN_e <- lmer(hana_bias ~ 1 + is_fu + neur_Hi + extra_C + hana_mean_C + hana_rec_C + hana_max_C + is_fu*neur_Hi + is_fu*extra_C + is_fu*hana_rec_C + age_C + Avt_C + Lst_C + is_cauc + (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hana_loN_e <- lmer(hana_bias ~ 1 + is_fu + neur_Lo + extra_C + hana_mean_C + hana_rec_C + hana_max_C + is_fu*neur_Lo + is_fu*extra_C + is_fu*hana_rec_C + age_C + Avt_C + Lst_C + is_cauc + (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hana_hiE_n <- lmer(hana_bias ~ 1 + is_fu + extra_Hi + neur_C + hana_mean_C + hana_rec_C + hana_max_C + is_fu*extra_Hi + is_fu*neur_C + is_fu*hana_rec_C + age_C + Avt_C + Lst_C + is_cauc + (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hana_loE_n <- lmer(hana_bias ~ 1 + is_fu + extra_Lo + neur_C + hana_mean_C + hana_rec_C + hana_max_C + is_fu*extra_Lo + is_fu*neur_C + is_fu*hana_rec_C + age_C + Avt_C + Lst_C + is_cauc + (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))

summary(lana_hiN_e <- lmer(lana_bias ~ 1 + is_fu + neur_Hi + extra_C + lana_mean_C + lana_rec_C + lana_max_C + is_fu*neur_Hi + is_fu*extra_C + is_fu*lana_rec_C + age_C + Avt_C + Lst_C + is_cauc + (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lana_loN_e <- lmer(lana_bias ~ 1 + is_fu + neur_Lo + extra_C + lana_mean_C + lana_rec_C + lana_max_C + is_fu*neur_Lo + is_fu*extra_C + is_fu*lana_rec_C + age_C + Avt_C + Lst_C + is_cauc + (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lana_hiE_n <- lmer(lana_bias ~ 1 + is_fu + extra_Hi + neur_C + lana_mean_C + lana_rec_C + lana_max_C + is_fu*extra_Hi + is_fu*neur_C + is_fu*lana_rec_C + age_C + Avt_C + Lst_C + is_cauc + (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lana_loE_n <- lmer(lana_bias ~ 1 + is_fu + extra_Lo + neur_C + lana_mean_C + lana_rec_C + lana_max_C + is_fu*extra_Lo + is_fu*neur_C + is_fu*lana_rec_C + age_C + Avt_C + Lst_C + is_cauc + (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))

#### JAN 1 2016 - Simple slopes with is_cauc added, retro_wks*max added back

summary(hapa_hiE_n <- lmer(hapa_bias ~ 1 + retro_wks + extra_Hi + neur_C + hapa_mean_C + hapa_rec_C + hapa_max_C + retro_wks*extra_Hi + retro_wks*neur_C + retro_wks*hapa_rec_C + retro_wks*hapa_max_C + sex + age_C + Avt_C + Lst_C + is_cauc + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hapa_loE_n <- lmer(hapa_bias ~ 1 + retro_wks + extra_Lo + neur_C + hapa_mean_C + hapa_rec_C + hapa_max_C + retro_wks*extra_Lo + retro_wks*neur_C + retro_wks*hapa_rec_C + retro_wks*hapa_max_C + sex + age_C + Avt_C + Lst_C + is_cauc + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hapa_hiN_e <- lmer(hapa_bias ~ 1 + retro_wks + neur_Hi + extra_C + hapa_mean_C + hapa_rec_C + hapa_max_C + retro_wks*neur_Hi + retro_wks*extra_C + retro_wks*hapa_rec_C + retro_wks*hapa_max_C + sex + age_C + Avt_C + Lst_C + is_cauc + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hapa_loN_e <- lmer(hapa_bias ~ 1 + retro_wks + neur_Lo + extra_C + hapa_mean_C + hapa_rec_C + hapa_max_C + retro_wks*neur_Lo + retro_wks*extra_C + retro_wks*hapa_rec_C + retro_wks*hapa_max_C + sex + age_C + Avt_C + Lst_C + is_cauc + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))

summary(lapa_hiE_n <- lmer(lapa_bias ~ 1 + retro_wks + extra_Hi + neur_C + lapa_mean_C + lapa_rec_C + lapa_max_C + retro_wks*extra_Hi + retro_wks*neur_C + retro_wks*lapa_rec_C + retro_wks*lapa_max_C + sex + age_C + Avt_C + Lst_C + is_cauc + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lapa_loE_n <- lmer(lapa_bias ~ 1 + retro_wks + extra_Lo + neur_C + lapa_mean_C + lapa_rec_C + lapa_max_C + retro_wks*extra_Lo + retro_wks*neur_C + retro_wks*lapa_rec_C + retro_wks*lapa_max_C + sex + age_C + Avt_C + Lst_C + is_cauc + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lapa_hiN_e <- lmer(lapa_bias ~ 1 + retro_wks + neur_Hi + extra_C + lapa_mean_C + lapa_rec_C + lapa_max_C + retro_wks*neur_Hi + retro_wks*extra_C + retro_wks*lapa_rec_C + retro_wks*lapa_max_C + sex + age_C + Avt_C + Lst_C + is_cauc + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lapa_loN_e <- lmer(lapa_bias ~ 1 + retro_wks + neur_Lo + extra_C + lapa_mean_C + lapa_rec_C + lapa_max_C + retro_wks*neur_Lo + retro_wks*extra_C + retro_wks*lapa_rec_C + retro_wks*lapa_max_C + sex + age_C + Avt_C + Lst_C + is_cauc + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))

summary(hana_hiN_e <- lmer(hana_bias ~ 1 + retro_wks + neur_Hi + extra_C + hana_mean_C + hana_rec_C + hana_max_C + retro_wks*neur_Hi + retro_wks*extra_C + retro_wks*hana_rec_C + retro_wks*hana_max_C + sex + age_C + Avt_C + Lst_C + is_cauc + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hana_loN_e <- lmer(hana_bias ~ 1 + retro_wks + neur_Lo + extra_C + hana_mean_C + hana_rec_C + hana_max_C + retro_wks*neur_Lo + retro_wks*extra_C + retro_wks*hana_rec_C + retro_wks*hana_max_C + sex + age_C + Avt_C + Lst_C + is_cauc + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hana_hiE_n <- lmer(hana_bias ~ 1 + retro_wks + extra_Hi + neur_C + hana_mean_C + hana_rec_C + hana_max_C + retro_wks*extra_Hi + retro_wks*neur_C + retro_wks*hana_rec_C + retro_wks*hana_max_C + sex + age_C + Avt_C + Lst_C + is_cauc + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hana_loE_n <- lmer(hana_bias ~ 1 + retro_wks + extra_Lo + neur_C + hana_mean_C + hana_rec_C + hana_max_C + retro_wks*extra_Lo + retro_wks*neur_C + retro_wks*hana_rec_C + retro_wks*hana_max_C + sex + age_C + Avt_C + Lst_C + is_cauc + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))

summary(lana_hiN_e <- lmer(lana_bias ~ 1 + retro_wks + neur_Hi + extra_C + lana_mean_C + lana_rec_C + lana_max_C + retro_wks*neur_Hi + retro_wks*extra_C + retro_wks*lana_rec_C + retro_wks*lana_max_C + sex + age_C + Avt_C + Lst_C + is_cauc + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lana_loN_e <- lmer(lana_bias ~ 1 + retro_wks + neur_Lo + extra_C + lana_mean_C + lana_rec_C + lana_max_C + retro_wks*neur_Lo + retro_wks*extra_C + retro_wks*lana_rec_C + retro_wks*lana_max_C + sex + age_C + Avt_C + Lst_C + is_cauc + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lana_hiE_n <- lmer(lana_bias ~ 1 + retro_wks + extra_Hi + neur_C + lana_mean_C + lana_rec_C + lana_max_C + retro_wks*extra_Hi + retro_wks*neur_C + retro_wks*lana_rec_C + retro_wks*lana_max_C + sex + age_C + Avt_C + Lst_C + is_cauc + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lana_loE_n <- lmer(lana_bias ~ 1 + retro_wks + extra_Lo + neur_C + lana_mean_C + lana_rec_C + lana_max_C + retro_wks*extra_Lo + retro_wks*neur_C + retro_wks*lana_rec_C + retro_wks*lana_max_C + sex + age_C + Avt_C + Lst_C + is_cauc + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))


# HAPA - Neuroticism and extraversion simple slopes
fixef(hapa_ints_all_wks)
B1_n<-fixef(hapa_ints_all_wks)[1] # intercept
B3_n<-fixef(hapa_ints_all_wks)[6] # neuroticism slope
B5_n<-fixef(hapa_ints_all_wks)[7] # extraversion slope
B2_n<-fixef(hapa_ints_all_wks)[2] # time slope
B4_n<-fixef(hapa_ints_all_wks)[13] # neuroticism x time
B6_n<-fixef(hapa_ints_all_wks)[14] # extraversion x time

## plot hapa bias-time slope at different values of neuroticism
## +1 SD
n_int_hi<-(B1_n + B3_n*(mean(na.omit(d1$neur_C))+sd(na.omit(d1$neur_C))))
n_sl_hi<-(B2_n + B4_n*(mean(na.omit(d1$neur_C))+sd(na.omit(d1$neur_C))))
## -1 SD
n_int_lo<-(B1_n + B3_n*(mean(na.omit(d1$neur_C))-sd(na.omit(d1$neur_C))))
n_sl_lo<-(B2_n + B4_n*(mean(na.omit(d1$neur_C))-sd(na.omit(d1$neur_C))))

## plot hapa bias-time slope at different values of extraversion
## +1 SD
e_int_hi<-(B1_n + B5_n*(mean(na.omit(d1$extra_C))+sd(na.omit(d1$extra_C))))
e_sl_hi<-(B2_n + B6_n*(mean(na.omit(d1$extra_C))+sd(na.omit(d1$extra_C))))
## -1 SD
e_int_lo<-(B1_n + B5_n*(mean(na.omit(d1$extra_C))-sd(na.omit(d1$extra_C))))
e_sl_lo<-(B2_n + B6_n*(mean(na.omit(d1$extra_C))-sd(na.omit(d1$extra_C))))

# Plotting the datapoints
myGrid<-seq(from = -3, to = -3, by=.01)
plot(myGrid, xlim=c(0,20), ylim=c(-0.5,1), xlab="Elapsed Time (weeks)", ylab="HAPA Retrospective Report Bias", frame.plot=FALSE)

Xs<-c(0, 20)
# lines(Xs,(int_ave + sl_ave*Xs), col = "darkviolet", lwd = 3)
lines(Xs,(n_int_lo + n_sl_lo*Xs), col = "slateblue2", lwd = 3, lty="dashed")
lines(Xs,(n_int_hi + n_sl_hi*Xs), col = "slateblue2", lwd = 3)
lines(Xs,(e_int_lo + e_sl_lo*Xs), col = "maroon3", lwd = 3, lty="dashed")
lines(Xs,(e_int_hi + e_sl_hi*Xs), col = "maroon3", lwd = 3)

legend(0,1, c("High Neuroticism (+1 SD)", "Low Neuroticism (-1 SD)", "High Extraversion (+1 SD)", "Low Extraversion (-1 SD)"), lty = 1:2:1:2, col = c("slateblue2","slateblue2","maroon3","maroon3"), y.intersp=1)

# Effect sizes
print(lmer(retro_wks ~ 1 + (1  | id), data=d1), digits=10) # SD of predictor of interest = unconditional residual = numerator in effect size equations
print(lmer(hapa_bias ~ 1 + (1  | id), data=d1), digits=10) # SD of DV = unconditional residual = denominator in effect size calculations

print(hapa_d_lo <- (2*(B2_n + B4_n*(-1)*sd(na.omit(d1$neur_C)))*3.652569798)/0.4629653914, digits = 10)
print(hapa_d_hi <- (2*(B2_n + B4_n*(+1)*sd(na.omit(d1$neur_C)))*3.652569798)/0.4629653914, digits = 10)

print(hapa_d_lo <- (2*(B2_n + B6_n*(-1)*sd(na.omit(d1$extra_C)))*3.652569798)/0.4629653914, digits = 10)
print(hapa_d_hi <- (2*(B2_n + B6_n*(+1)*sd(na.omit(d1$extra_C)))*3.652569798)/0.4629653914, digits = 10)

# LAPA - Neuroticism and extraversion simple slopes
fixef(lapa_ints_all_wks)
B1_n<-fixef(lapa_ints_all_wks)[1] # intercept
B3_n<-fixef(lapa_ints_all_wks)[6] # neuroticism slope
B5_n<-fixef(lapa_ints_all_wks)[7] # extraversion slope
B2_n<-fixef(lapa_ints_all_wks)[2] # time slope
B4_n<-fixef(lapa_ints_all_wks)[13] # neuroticism x time
B6_n<-fixef(lapa_ints_all_wks)[14] # extraversion x time

## plot lapa bias-time slope at different values of neuroticism
## +1 SD
n_int_hi<-(B1_n + B3_n*(mean(na.omit(d1$neur_C))+sd(na.omit(d1$neur_C))))
n_sl_hi<-(B2_n + B4_n*(mean(na.omit(d1$neur_C))+sd(na.omit(d1$neur_C))))
## -1 SD
n_int_lo<-(B1_n + B3_n*(mean(na.omit(d1$neur_C))-sd(na.omit(d1$neur_C))))
n_sl_lo<-(B2_n + B4_n*(mean(na.omit(d1$neur_C))-sd(na.omit(d1$neur_C))))

## plot lapa bias-time slope at different values of extraversion
## +1 SD
e_int_hi<-(B1_n + B5_n*(mean(na.omit(d1$extra_C))+sd(na.omit(d1$extra_C))))
e_sl_hi<-(B2_n + B6_n*(mean(na.omit(d1$extra_C))+sd(na.omit(d1$extra_C))))
## -1 SD
e_int_lo<-(B1_n + B5_n*(mean(na.omit(d1$extra_C))-sd(na.omit(d1$extra_C))))
e_sl_lo<-(B2_n + B6_n*(mean(na.omit(d1$extra_C))-sd(na.omit(d1$extra_C))))

# Plotting the datapoints
myGrid<-seq(from = -3, to = -3, by=.01)
plot(myGrid, xlim=c(0,20), ylim=c(-1,0), xlab="Elapsed Time (weeks)", ylab="LAPA Retrospective Report Bias", frame.plot=FALSE)

Xs<-c(0, 20)
# lines(Xs,(int_ave + sl_ave*Xs), col = "darkviolet", lwd = 3)
lines(Xs,(n_int_lo + n_sl_lo*Xs), col = "slateblue2", lwd = 3, lty="dashed")
lines(Xs,(n_int_hi + n_sl_hi*Xs), col = "slateblue2", lwd = 3)
lines(Xs,(e_int_lo + e_sl_lo*Xs), col = "maroon3", lwd = 3, lty="dashed")
lines(Xs,(e_int_hi + e_sl_hi*Xs), col = "maroon3", lwd = 3)

# Effect sizes
print(lmer(retro_wks ~ 1 + (1  | id), data=d1), digits=10) # SD of predictor of interest = unconditional residual = numerator in effect size equations
print(lmer(lapa_bias ~ 1 + (1  | id), data=d1), digits=10) # SD of DV = unconditional residual = denominator in effect size calculations

print(lapa_d_lo <- (2*(B2_n + B4_n*(-1)*sd(na.omit(d1$neur_C)))*3.652569798)/0.5081852064, digits = 10)
print(lapa_d_hi <- (2*(B2_n + B4_n*(+1)*sd(na.omit(d1$neur_C)))*3.652569798)/0.5081852064, digits = 10)

print(lapa_d_lo <- (2*(B2_n + B6_n*(-1)*sd(na.omit(d1$extra_C)))*3.652569798)/0.5081852064, digits = 10)
print(lapa_d_hi <- (2*(B2_n + B6_n*(+1)*sd(na.omit(d1$extra_C)))*3.652569798)/0.5081852064, digits = 10)

# HANA - Neuroticism and extraversion simple slopes
fixef(hapa_ints_all_wks)
B1_n<-fixef(hapa_ints_all_wks)[1] # intercept
B3_n<-fixef(hapa_ints_all_wks)[6] # neuroticism slope
B5_n<-fixef(hapa_ints_all_wks)[7] # extraversion slope
B2_n<-fixef(hapa_ints_all_wks)[2] # time slope
B4_n<-fixef(hapa_ints_all_wks)[13] # neuroticism x time
B6_n<-fixef(hapa_ints_all_wks)[14] # extraversion x time

## plot hana bias-time slope at different values of neuroticism
## +1 SD
n_int_hi<-(B1_n + B3_n*(mean(na.omit(d1$neur_C))+sd(na.omit(d1$neur_C))))
n_sl_hi<-(B2_n + B4_n*(mean(na.omit(d1$neur_C))+sd(na.omit(d1$neur_C))))
## -1 SD
n_int_lo<-(B1_n + B3_n*(mean(na.omit(d1$neur_C))-sd(na.omit(d1$neur_C))))
n_sl_lo<-(B2_n + B4_n*(mean(na.omit(d1$neur_C))-sd(na.omit(d1$neur_C))))

## plot hana bias-time slope at different values of extraversion
## +1 SD
e_int_hi<-(B1_n + B5_n*(mean(na.omit(d1$extra_C))+sd(na.omit(d1$extra_C))))
e_sl_hi<-(B2_n + B6_n*(mean(na.omit(d1$extra_C))+sd(na.omit(d1$extra_C))))
## -1 SD
e_int_lo<-(B1_n + B5_n*(mean(na.omit(d1$extra_C))-sd(na.omit(d1$extra_C))))
e_sl_lo<-(B2_n + B6_n*(mean(na.omit(d1$extra_C))-sd(na.omit(d1$extra_C))))

# Plotting the datapoints
myGrid<-seq(from = -3, to = -3, by=.01)
plot(myGrid, xlim=c(0,20), ylim=c(-0.5,1), xlab="Elapsed Time (weeks)", ylab="HANA Retrospective Report Bias", frame.plot=FALSE)

Xs<-c(0, 20)
# lines(Xs,(int_ave + sl_ave*Xs), col = "darkviolet", lwd = 3)
lines(Xs,(n_int_lo + n_sl_lo*Xs), col = "slateblue2", lwd = 3, lty="dashed")
lines(Xs,(n_int_hi + n_sl_hi*Xs), col = "slateblue2", lwd = 3)
lines(Xs,(e_int_lo + e_sl_lo*Xs), col = "maroon3", lwd = 3, lty="dashed")
lines(Xs,(e_int_hi + e_sl_hi*Xs), col = "maroon3", lwd = 3)

# Effect sizes
print(lmer(retro_wks ~ 1 + (1  | id), data=d1), digits=10) # SD of predictor of interest = unconditional residual = numerator in effect size equations
print(lmer(hana_bias ~ 1 + (1  | id), data=d1), digits=10) # SD of DV = unconditional residual = denominator in effect size calculations

print(hana_d_lo <- (2*(B2_n + B4_n*(-1)*sd(na.omit(d1$neur_C)))*3.652569798)/0.4851232581, digits = 10)
print(hana_d_hi <- (2*(B2_n + B4_n*(+1)*sd(na.omit(d1$neur_C)))*3.652569798)/0.4851232581, digits = 10)

print(hana_d_lo <- (2*(B2_n + B6_n*(-1)*sd(na.omit(d1$extra_C)))*3.652569798)/0.4851232581, digits = 10)
print(hana_d_hi <- (2*(B2_n + B6_n*(+1)*sd(na.omit(d1$extra_C)))*3.652569798)/0.4851232581, digits = 10)

# LANA - Neuroticism and extraversion simple slopes
fixef(lana_ints_all_wks)
B1_n<-fixef(lana_ints_all_wks)[1] # intercept
B3_n<-fixef(lana_ints_all_wks)[6] # neuroticism slope
B5_n<-fixef(lana_ints_all_wks)[7] # extraversion slope
B2_n<-fixef(lana_ints_all_wks)[2] # time slope
B4_n<-fixef(lana_ints_all_wks)[13] # neuroticism x time
B6_n<-fixef(lana_ints_all_wks)[14] # extraversion x time

## plot lana bias-time slope at different values of neuroticism
## +1 SD
n_int_hi<-(B1_n + B3_n*(mean(na.omit(d1$neur_C))+sd(na.omit(d1$neur_C))))
n_sl_hi<-(B2_n + B4_n*(mean(na.omit(d1$neur_C))+sd(na.omit(d1$neur_C))))
## -1 SD
n_int_lo<-(B1_n + B3_n*(mean(na.omit(d1$neur_C))-sd(na.omit(d1$neur_C))))
n_sl_lo<-(B2_n + B4_n*(mean(na.omit(d1$neur_C))-sd(na.omit(d1$neur_C))))

## plot lana bias-time slope at different values of extraversion
## +1 SD
e_int_hi<-(B1_n + B5_n*(mean(na.omit(d1$extra_C))+sd(na.omit(d1$extra_C))))
e_sl_hi<-(B2_n + B6_n*(mean(na.omit(d1$extra_C))+sd(na.omit(d1$extra_C))))
## -1 SD
e_int_lo<-(B1_n + B5_n*(mean(na.omit(d1$extra_C))-sd(na.omit(d1$extra_C))))
e_sl_lo<-(B2_n + B6_n*(mean(na.omit(d1$extra_C))-sd(na.omit(d1$extra_C))))

# Plotting the datapoints
myGrid<-seq(from = -3, to = -3, by=.01)
plot(myGrid, xlim=c(0,20), ylim=c(-0.5,1), xlab="Elapsed Time (weeks)", ylab="LANA Retrospective Report Bias", frame.plot=FALSE)

Xs<-c(0, 20)
# lines(Xs,(int_ave + sl_ave*Xs), col = "darkviolet", lwd = 3)
lines(Xs,(n_int_lo + n_sl_lo*Xs), col = "slateblue2", lwd = 3, lty="dashed")
lines(Xs,(n_int_hi + n_sl_hi*Xs), col = "slateblue2", lwd = 3)
lines(Xs,(e_int_lo + e_sl_lo*Xs), col = "maroon3", lwd = 3, lty="dashed")
lines(Xs,(e_int_hi + e_sl_hi*Xs), col = "maroon3", lwd = 3)

# Effect sizes
print(lmer(retro_wks ~ 1 + (1  | id), data=d1), digits=10) # SD of predictor of interest = unconditional residual = numerator in effect size equations
print(lmer(lana_bias ~ 1 + (1  | id), data=d1), digits=10) # SD of DV = unconditional residual = denominator in effect size calculations

# neuroticism
print(lana_d_lo <- (2*(B2_n + B4_n*(-1)*sd(na.omit(d1$neur_C)))*3.652569798)/0.438083480, digits = 10)
print(lana_d_hi <- (2*(B2_n + B4_n*(+1)*sd(na.omit(d1$neur_C)))*3.652569798)/0.438083480, digits = 10)
# extraversion
print(lana_d_lo <- (2*(B2_n + B6_n*(-1)*sd(na.omit(d1$extra_C)))*3.652569798)/0.438083480, digits = 10)
print(lana_d_hi <- (2*(B2_n + B6_n*(+1)*sd(na.omit(d1$extra_C)))*3.652569798)/0.438083480, digits = 10)


### JAN 1 2-16 - Simple slopes for different combinations of High vs. Low Neuroticsm and Extraversion - CONTINUOUS TIME

summary(hapa_hiE_hiN <- lmer(hapa_bias ~ 1 + retro_wks + extra_Hi + neur_Hi + hapa_mean_C + hapa_rec_C + hapa_max_C + retro_wks*extra_Hi + retro_wks*neur_Hi + retro_wks*hapa_rec_C + retro_wks*hapa_max_C + sex + age_C + Avt_C + Lst_C + is_cauc + edu + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hapa_hiE_loN <- lmer(hapa_bias ~ 1 + retro_wks + extra_Hi + neur_Lo + hapa_mean_C + hapa_rec_C + hapa_max_C + retro_wks*extra_Hi + retro_wks*neur_Lo + retro_wks*hapa_rec_C + retro_wks*hapa_max_C + sex + age_C + Avt_C + Lst_C + is_cauc + edu + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hapa_loE_hiN <- lmer(hapa_bias ~ 1 + retro_wks + extra_Lo + neur_Hi + hapa_mean_C + hapa_rec_C + hapa_max_C + retro_wks*extra_Lo + retro_wks*neur_Hi + retro_wks*hapa_rec_C + retro_wks*hapa_max_C + sex + age_C + Avt_C + Lst_C + is_cauc + edu + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hapa_loE_loN <- lmer(hapa_bias ~ 1 + retro_wks + extra_Lo + neur_Lo + hapa_mean_C + hapa_rec_C + hapa_max_C + retro_wks*extra_Lo + retro_wks*neur_Lo + retro_wks*hapa_rec_C + retro_wks*hapa_max_C + sex + age_C + Avt_C + Lst_C + is_cauc + edu + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))

summary(lapa_hiE_hiN <- lmer(lapa_bias ~ 1 + retro_wks + extra_Hi + neur_Hi + lapa_mean_C + lapa_rec_C + lapa_max_C + retro_wks*extra_Hi + retro_wks*neur_Hi + retro_wks*lapa_rec_C + retro_wks*lapa_max_C + sex + age_C + Avt_C + Lst_C + is_cauc + edu + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lapa_hiE_loN <- lmer(lapa_bias ~ 1 + retro_wks + extra_Hi + neur_Lo + lapa_mean_C + lapa_rec_C + lapa_max_C + retro_wks*extra_Hi + retro_wks*neur_Lo + retro_wks*lapa_rec_C + retro_wks*lapa_max_C + sex + age_C + Avt_C + Lst_C + is_cauc + edu + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lapa_loE_hiN <- lmer(lapa_bias ~ 1 + retro_wks + extra_Lo + neur_Hi + lapa_mean_C + lapa_rec_C + lapa_max_C + retro_wks*extra_Lo + retro_wks*neur_Hi + retro_wks*lapa_rec_C + retro_wks*lapa_max_C + sex + age_C + Avt_C + Lst_C + is_cauc + edu + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lapa_loE_loN <- lmer(lapa_bias ~ 1 + retro_wks + extra_Lo + neur_Lo + lapa_mean_C + lapa_rec_C + lapa_max_C + retro_wks*extra_Lo + retro_wks*neur_Lo + retro_wks*lapa_rec_C + retro_wks*lapa_max_C + sex + age_C + Avt_C + Lst_C + is_cauc + edu + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))

summary(hana_hiE_hiN <- lmer(hana_bias ~ 1 + retro_wks + extra_Hi + neur_Hi + hana_mean_C + hana_rec_C + hana_max_C + retro_wks*extra_Hi + retro_wks*neur_Hi + retro_wks*hana_rec_C + retro_wks*hana_max_C + sex + age_C + Avt_C + Lst_C + is_cauc + edu + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hana_hiE_loN <- lmer(hana_bias ~ 1 + retro_wks + extra_Hi + neur_Lo + hana_mean_C + hana_rec_C + hana_max_C + retro_wks*extra_Hi + retro_wks*neur_Lo + retro_wks*hana_rec_C + retro_wks*hana_max_C + sex + age_C + Avt_C + Lst_C + is_cauc + edu + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hana_loE_hiN <- lmer(hana_bias ~ 1 + retro_wks + extra_Lo + neur_Hi + hana_mean_C + hana_rec_C + hana_max_C + retro_wks*extra_Lo + retro_wks*neur_Hi + retro_wks*hana_rec_C + retro_wks*hana_max_C + sex + age_C + Avt_C + Lst_C + is_cauc + edu + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hana_loE_loN <- lmer(hana_bias ~ 1 + retro_wks + extra_Lo + neur_Lo + hana_mean_C + hana_rec_C + hana_max_C + retro_wks*extra_Lo + retro_wks*neur_Lo + retro_wks*hana_rec_C + retro_wks*hana_max_C + sex + age_C + Avt_C + Lst_C + is_cauc + edu + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))

summary(lana_hiE_hiN <- lmer(lana_bias ~ 1 + retro_wks + extra_Hi + neur_Hi + lana_mean_C + lana_rec_C + lana_max_C + retro_wks*extra_Hi + retro_wks*neur_Hi + retro_wks*lana_rec_C + retro_wks*lana_max_C + sex + age_C + Avt_C + Lst_C + is_cauc + edu + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lana_hiE_loN <- lmer(lana_bias ~ 1 + retro_wks + extra_Hi + neur_Lo + lana_mean_C + lana_rec_C + lana_max_C + retro_wks*extra_Hi + retro_wks*neur_Lo + retro_wks*lana_rec_C + retro_wks*lana_max_C + sex + age_C + Avt_C + Lst_C + is_cauc + edu + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lana_loE_hiN <- lmer(lana_bias ~ 1 + retro_wks + extra_Lo + neur_Hi + lana_mean_C + lana_rec_C + lana_max_C + retro_wks*extra_Lo + retro_wks*neur_Hi + retro_wks*lana_rec_C + retro_wks*lana_max_C + sex + age_C + Avt_C + Lst_C + is_cauc + edu + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lana_loE_loN <- lmer(lana_bias ~ 1 + retro_wks + extra_Lo + neur_Lo + lana_mean_C + lana_rec_C + lana_max_C + retro_wks*extra_Lo + retro_wks*neur_Lo + retro_wks*lana_rec_C + retro_wks*lana_max_C + sex + age_C + Avt_C + Lst_C + is_cauc + edu + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))

### JAN 1 2-16 - Simple slopes for different combinations of High vs. Low Neuroticsm and Extraversion - EXIT VS FOLLOWUP

summary(hapa_hiE_hiN <- lmer(hapa_bias ~ 1 + is_fu + extra_Hi + neur_Hi + hapa_mean_C + hapa_rec_C + hapa_max_C + is_fu*extra_Hi + is_fu*neur_Hi + is_fu*hapa_rec_C + is_fu*hapa_max_C + sex + age_C + Avt_C + Lst_C + is_cauc + edu + (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hapa_hiE_loN <- lmer(hapa_bias ~ 1 + is_fu + extra_Hi + neur_Lo + hapa_mean_C + hapa_rec_C + hapa_max_C + is_fu*extra_Hi + is_fu*neur_Lo + is_fu*hapa_rec_C + is_fu*hapa_max_C + sex + age_C + Avt_C + Lst_C + is_cauc + edu + (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hapa_loE_hiN <- lmer(hapa_bias ~ 1 + is_fu + extra_Lo + neur_Hi + hapa_mean_C + hapa_rec_C + hapa_max_C + is_fu*extra_Lo + is_fu*neur_Hi + is_fu*hapa_rec_C + is_fu*hapa_max_C + sex + age_C + Avt_C + Lst_C + is_cauc + edu + (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hapa_loE_loN <- lmer(hapa_bias ~ 1 + is_fu + extra_Lo + neur_Lo + hapa_mean_C + hapa_rec_C + hapa_max_C + is_fu*extra_Lo + is_fu*neur_Lo + is_fu*hapa_rec_C + is_fu*hapa_max_C + sex + age_C + Avt_C + Lst_C + is_cauc + edu + (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))

summary(lapa_hiE_hiN <- lmer(lapa_bias ~ 1 + is_fu + extra_Hi + neur_Hi + lapa_mean_C + lapa_rec_C + lapa_max_C + is_fu*extra_Hi + is_fu*neur_Hi + is_fu*lapa_rec_C + is_fu*lapa_max_C + sex + age_C + Avt_C + Lst_C + is_cauc + edu + (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lapa_hiE_loN <- lmer(lapa_bias ~ 1 + is_fu + extra_Hi + neur_Lo + lapa_mean_C + lapa_rec_C + lapa_max_C + is_fu*extra_Hi + is_fu*neur_Lo + is_fu*lapa_rec_C + is_fu*lapa_max_C + sex + age_C + Avt_C + Lst_C + is_cauc + edu + (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lapa_loE_hiN <- lmer(lapa_bias ~ 1 + is_fu + extra_Lo + neur_Hi + lapa_mean_C + lapa_rec_C + lapa_max_C + is_fu*extra_Lo + is_fu*neur_Hi + is_fu*lapa_rec_C + is_fu*lapa_max_C + sex + age_C + Avt_C + Lst_C + is_cauc + edu + (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lapa_loE_loN <- lmer(lapa_bias ~ 1 + is_fu + extra_Lo + neur_Lo + lapa_mean_C + lapa_rec_C + lapa_max_C + is_fu*extra_Lo + is_fu*neur_Lo + is_fu*lapa_rec_C + is_fu*lapa_max_C + sex + age_C + Avt_C + Lst_C + is_cauc + edu + (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))

summary(hana_hiE_hiN <- lmer(hana_bias ~ 1 + is_fu + extra_Hi + neur_Hi + hana_mean_C + hana_rec_C + hana_max_C + is_fu*extra_Hi + is_fu*neur_Hi + is_fu*hana_rec_C + is_fu*hana_max_C + sex + age_C + Avt_C + Lst_C + is_cauc + edu + (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hana_hiE_loN <- lmer(hana_bias ~ 1 + is_fu + extra_Hi + neur_Lo + hana_mean_C + hana_rec_C + hana_max_C + is_fu*extra_Hi + is_fu*neur_Lo + is_fu*hana_rec_C + is_fu*hana_max_C + sex + age_C + Avt_C + Lst_C + is_cauc + edu + (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hana_loE_hiN <- lmer(hana_bias ~ 1 + is_fu + extra_Lo + neur_Hi + hana_mean_C + hana_rec_C + hana_max_C + is_fu*extra_Lo + is_fu*neur_Hi + is_fu*hana_rec_C + is_fu*hana_max_C + sex + age_C + Avt_C + Lst_C + is_cauc + edu + (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hana_loE_loN <- lmer(hana_bias ~ 1 + is_fu + extra_Lo + neur_Lo + hana_mean_C + hana_rec_C + hana_max_C + is_fu*extra_Lo + is_fu*neur_Lo + is_fu*hana_rec_C + is_fu*hana_max_C + sex + age_C + Avt_C + Lst_C + is_cauc + edu + (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))

summary(lana_hiE_hiN <- lmer(lana_bias ~ 1 + is_fu + extra_Hi + neur_Hi + lana_mean_C + lana_rec_C + lana_max_C + is_fu*extra_Hi + is_fu*neur_Hi + is_fu*lana_rec_C + is_fu*lana_max_C + sex + age_C + Avt_C + Lst_C + is_cauc + edu + (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lana_hiE_loN <- lmer(lana_bias ~ 1 + is_fu + extra_Hi + neur_Lo + lana_mean_C + lana_rec_C + lana_max_C + is_fu*extra_Hi + is_fu*neur_Lo + is_fu*lana_rec_C + is_fu*lana_max_C + sex + age_C + Avt_C + Lst_C + is_cauc + edu + (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lana_loE_hiN <- lmer(lana_bias ~ 1 + is_fu + extra_Lo + neur_Hi + lana_mean_C + lana_rec_C + lana_max_C + is_fu*extra_Lo + is_fu*neur_Hi + is_fu*lana_rec_C + is_fu*lana_max_C + sex + age_C + Avt_C + Lst_C + is_cauc + edu + (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lana_loE_loN <- lmer(lana_bias ~ 1 + is_fu + extra_Lo + neur_Lo + lana_mean_C + lana_rec_C + lana_max_C + is_fu*extra_Lo + is_fu*neur_Lo + is_fu*lana_rec_C + is_fu*lana_max_C + sex + age_C + Avt_C + Lst_C + is_cauc + edu + (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))


### (JAN 1 2-16 - Simple slopes for different combinations of High vs. Low Neuroticsm and Extraversion - EXIT VS FOLLOWUP)
# follow-up coded as 0 to look at intercept significance 

summary(hapa_hiE_hiN <- lmer(hapa_bias ~ 1 + is_fu_r + extra_Hi + neur_Hi + hapa_mean_C + hapa_rec_C + hapa_max_C + is_fu_r*extra_Hi + is_fu_r*neur_Hi + is_fu_r*hapa_rec_C + is_fu_r*hapa_max_C + sex + age_C + Avt_C + Lst_C + is_cauc + edu + (1 + is_fu_r | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hapa_hiE_loN <- lmer(hapa_bias ~ 1 + is_fu_r + extra_Hi + neur_Lo + hapa_mean_C + hapa_rec_C + hapa_max_C + is_fu_r*extra_Hi + is_fu_r*neur_Lo + is_fu_r*hapa_rec_C + is_fu_r*hapa_max_C + sex + age_C + Avt_C + Lst_C + is_cauc + edu + (1 + is_fu_r | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hapa_loE_hiN <- lmer(hapa_bias ~ 1 + is_fu_r + extra_Lo + neur_Hi + hapa_mean_C + hapa_rec_C + hapa_max_C + is_fu_r*extra_Lo + is_fu_r*neur_Hi + is_fu_r*hapa_rec_C + is_fu_r*hapa_max_C + sex + age_C + Avt_C + Lst_C + is_cauc + edu + (1 + is_fu_r | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hapa_loE_loN <- lmer(hapa_bias ~ 1 + is_fu_r + extra_Lo + neur_Lo + hapa_mean_C + hapa_rec_C + hapa_max_C + is_fu_r*extra_Lo + is_fu_r*neur_Lo + is_fu_r*hapa_rec_C + is_fu_r*hapa_max_C + sex + age_C + Avt_C + Lst_C + is_cauc + edu + (1 + is_fu_r | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))

summary(lapa_hiE_hiN <- lmer(lapa_bias ~ 1 + is_fu_r + extra_Hi + neur_Hi + lapa_mean_C + lapa_rec_C + lapa_max_C + is_fu_r*extra_Hi + is_fu_r*neur_Hi + is_fu_r*lapa_rec_C + is_fu_r*lapa_max_C + sex + age_C + Avt_C + Lst_C + is_cauc + edu + (1 + is_fu_r | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lapa_hiE_loN <- lmer(lapa_bias ~ 1 + is_fu_r + extra_Hi + neur_Lo + lapa_mean_C + lapa_rec_C + lapa_max_C + is_fu_r*extra_Hi + is_fu_r*neur_Lo + is_fu_r*lapa_rec_C + is_fu_r*lapa_max_C + sex + age_C + Avt_C + Lst_C + is_cauc + edu + (1 + is_fu_r | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lapa_loE_hiN <- lmer(lapa_bias ~ 1 + is_fu_r + extra_Lo + neur_Hi + lapa_mean_C + lapa_rec_C + lapa_max_C + is_fu_r*extra_Lo + is_fu_r*neur_Hi + is_fu_r*lapa_rec_C + is_fu_r*lapa_max_C + sex + age_C + Avt_C + Lst_C + is_cauc + edu + (1 + is_fu_r | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lapa_loE_loN <- lmer(lapa_bias ~ 1 + is_fu_r + extra_Lo + neur_Lo + lapa_mean_C + lapa_rec_C + lapa_max_C + is_fu_r*extra_Lo + is_fu_r*neur_Lo + is_fu_r*lapa_rec_C + is_fu_r*lapa_max_C + sex + age_C + Avt_C + Lst_C + is_cauc + edu + (1 + is_fu_r | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))

summary(hana_hiE_hiN <- lmer(hana_bias ~ 1 + is_fu_r + extra_Hi + neur_Hi + hana_mean_C + hana_rec_C + hana_max_C + is_fu_r*extra_Hi + is_fu_r*neur_Hi + is_fu_r*hana_rec_C + is_fu_r*hana_max_C + sex + age_C + Avt_C + Lst_C + is_cauc + edu + (1 + is_fu_r | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hana_hiE_loN <- lmer(hana_bias ~ 1 + is_fu_r + extra_Hi + neur_Lo + hana_mean_C + hana_rec_C + hana_max_C + is_fu_r*extra_Hi + is_fu_r*neur_Lo + is_fu_r*hana_rec_C + is_fu_r*hana_max_C + sex + age_C + Avt_C + Lst_C + is_cauc + edu + (1 + is_fu_r | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hana_loE_hiN <- lmer(hana_bias ~ 1 + is_fu_r + extra_Lo + neur_Hi + hana_mean_C + hana_rec_C + hana_max_C + is_fu_r*extra_Lo + is_fu_r*neur_Hi + is_fu_r*hana_rec_C + is_fu_r*hana_max_C + sex + age_C + Avt_C + Lst_C + is_cauc + edu + (1 + is_fu_r | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hana_loE_loN <- lmer(hana_bias ~ 1 + is_fu_r + extra_Lo + neur_Lo + hana_mean_C + hana_rec_C + hana_max_C + is_fu_r*extra_Lo + is_fu_r*neur_Lo + is_fu_r*hana_rec_C + is_fu_r*hana_max_C + sex + age_C + Avt_C + Lst_C + is_cauc + edu + (1 + is_fu_r | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))

summary(lana_hiE_hiN <- lmer(lana_bias ~ 1 + is_fu_r + extra_Hi + neur_Hi + lana_mean_C + lana_rec_C + lana_max_C + is_fu_r*extra_Hi + is_fu_r*neur_Hi + is_fu_r*lana_rec_C + is_fu_r*lana_max_C + sex + age_C + Avt_C + Lst_C + is_cauc + edu + (1 + is_fu_r | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lana_hiE_loN <- lmer(lana_bias ~ 1 + is_fu_r + extra_Hi + neur_Lo + lana_mean_C + lana_rec_C + lana_max_C + is_fu_r*extra_Hi + is_fu_r*neur_Lo + is_fu_r*lana_rec_C + is_fu_r*lana_max_C + sex + age_C + Avt_C + Lst_C + is_cauc + edu + (1 + is_fu_r | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lana_loE_hiN <- lmer(lana_bias ~ 1 + is_fu_r + extra_Lo + neur_Hi + lana_mean_C + lana_rec_C + lana_max_C + is_fu_r*extra_Lo + is_fu_r*neur_Hi + is_fu_r*lana_rec_C + is_fu_r*lana_max_C + sex + age_C + Avt_C + Lst_C + is_cauc + edu + (1 + is_fu_r | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lana_loE_loN <- lmer(lana_bias ~ 1 + is_fu_r + extra_Lo + neur_Lo + lana_mean_C + lana_rec_C + lana_max_C + is_fu_r*extra_Lo + is_fu_r*neur_Lo + is_fu_r*lana_rec_C + is_fu_r*lana_max_C + sex + age_C + Avt_C + Lst_C + is_cauc + edu + (1 + is_fu_r | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))

### JAN 1 2-16 - REDUCED Simple slopes for different combinations of High vs. Low Neuroticsm and Extraversion - EXIT VS FOLLOWUP

summary(hapa_hiE_hiN <- lmer(hapa_bias ~ 1 + is_fu + extra_Hi + neur_Hi + hapa_mean_C + hapa_rec_C + hapa_max_C + is_fu*extra_Hi + is_fu*neur_Hi + is_fu*hapa_rec_C + sex + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hapa_hiE_loN <- lmer(hapa_bias ~ 1 + is_fu + extra_Hi + neur_Lo + hapa_mean_C + hapa_rec_C + hapa_max_C + is_fu*extra_Hi + is_fu*neur_Lo + is_fu*hapa_rec_C + sex + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hapa_loE_hiN <- lmer(hapa_bias ~ 1 + is_fu + extra_Lo + neur_Hi + hapa_mean_C + hapa_rec_C + hapa_max_C + is_fu*extra_Lo + is_fu*neur_Hi + is_fu*hapa_rec_C + sex + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hapa_loE_loN <- lmer(hapa_bias ~ 1 + is_fu + extra_Lo + neur_Lo + hapa_mean_C + hapa_rec_C + hapa_max_C + is_fu*extra_Lo + is_fu*neur_Lo + is_fu*hapa_rec_C + sex + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))

summary(lapa_hiE_hiN <- lmer(lapa_bias ~ 1 + is_fu + extra_Hi + neur_Hi + lapa_mean_C + lapa_rec_C + lapa_max_C + is_fu*extra_Hi + is_fu*neur_Hi + is_fu*lapa_rec_C + sex + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lapa_hiE_loN <- lmer(lapa_bias ~ 1 + is_fu + extra_Hi + neur_Lo + lapa_mean_C + lapa_rec_C + lapa_max_C + is_fu*extra_Hi + is_fu*neur_Lo + is_fu*lapa_rec_C + sex + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lapa_loE_hiN <- lmer(lapa_bias ~ 1 + is_fu + extra_Lo + neur_Hi + lapa_mean_C + lapa_rec_C + lapa_max_C + is_fu*extra_Lo + is_fu*neur_Hi + is_fu*lapa_rec_C + sex + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lapa_loE_loN <- lmer(lapa_bias ~ 1 + is_fu + extra_Lo + neur_Lo + lapa_mean_C + lapa_rec_C + lapa_max_C + is_fu*extra_Lo + is_fu*neur_Lo + is_fu*lapa_rec_C + sex + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))

summary(hana_hiE_hiN <- lmer(hana_bias ~ 1 + is_fu + extra_Hi + neur_Hi + hana_mean_C + hana_rec_C + hana_max_C + is_fu*extra_Hi + is_fu*neur_Hi + is_fu*hana_rec_C + sex + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hana_hiE_loN <- lmer(hana_bias ~ 1 + is_fu + extra_Hi + neur_Lo + hana_mean_C + hana_rec_C + hana_max_C + is_fu*extra_Hi + is_fu*neur_Lo + is_fu*hana_rec_C + sex + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hana_loE_hiN <- lmer(hana_bias ~ 1 + is_fu + extra_Lo + neur_Hi + hana_mean_C + hana_rec_C + hana_max_C + is_fu*extra_Lo + is_fu*neur_Hi + is_fu*hana_rec_C + sex + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hana_loE_loN <- lmer(hana_bias ~ 1 + is_fu + extra_Lo + neur_Lo + hana_mean_C + hana_rec_C + hana_max_C + is_fu*extra_Lo + is_fu*neur_Lo + is_fu*hana_rec_C + sex + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))

summary(lana_hiE_hiN <- lmer(lana_bias ~ 1 + is_fu + extra_Hi + neur_Hi + lana_mean_C + lana_rec_C + lana_max_C + is_fu*extra_Hi + is_fu*neur_Hi + is_fu*lana_rec_C + sex + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lana_hiE_loN <- lmer(lana_bias ~ 1 + is_fu + extra_Hi + neur_Lo + lana_mean_C + lana_rec_C + lana_max_C + is_fu*extra_Hi + is_fu*neur_Lo + is_fu*lana_rec_C + sex + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lana_loE_hiN <- lmer(lana_bias ~ 1 + is_fu + extra_Lo + neur_Hi + lana_mean_C + lana_rec_C + lana_max_C + is_fu*extra_Lo + is_fu*neur_Hi + is_fu*lana_rec_C + sex + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lana_loE_loN <- lmer(lana_bias ~ 1 + is_fu + extra_Lo + neur_Lo + lana_mean_C + lana_rec_C + lana_max_C + is_fu*extra_Lo + is_fu*neur_Lo + is_fu*lana_rec_C + sex + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))


### (JAN 1 2-16 - REDUCED Simple slopes for different combinations of High vs. Low Neuroticsm and Extraversion - EXIT VS FOLLOWUP)
# follow-up coded as 0 to look at intercept significance 

summary(hapa_hiE_hiN <- lmer(hapa_bias ~ 1 + is_fu_r + extra_Hi + neur_Hi + hapa_mean_C + hapa_rec_C + hapa_max_C + is_fu_r*extra_Hi + is_fu_r*neur_Hi + is_fu_r*hapa_rec_C + sex + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu_r | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hapa_hiE_loN <- lmer(hapa_bias ~ 1 + is_fu_r + extra_Hi + neur_Lo + hapa_mean_C + hapa_rec_C + hapa_max_C + is_fu_r*extra_Hi + is_fu_r*neur_Lo + is_fu_r*hapa_rec_C + sex + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu_r | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hapa_loE_hiN <- lmer(hapa_bias ~ 1 + is_fu_r + extra_Lo + neur_Hi + hapa_mean_C + hapa_rec_C + hapa_max_C + is_fu_r*extra_Lo + is_fu_r*neur_Hi + is_fu_r*hapa_rec_C + sex + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu_r | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hapa_loE_loN <- lmer(hapa_bias ~ 1 + is_fu_r + extra_Lo + neur_Lo + hapa_mean_C + hapa_rec_C + hapa_max_C + is_fu_r*extra_Lo + is_fu_r*neur_Lo + is_fu_r*hapa_rec_C + sex + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu_r | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))

summary(lapa_hiE_hiN <- lmer(lapa_bias ~ 1 + is_fu_r + extra_Hi + neur_Hi + lapa_mean_C + lapa_rec_C + lapa_max_C + is_fu_r*extra_Hi + is_fu_r*neur_Hi + is_fu_r*lapa_rec_C + sex + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu_r | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lapa_hiE_loN <- lmer(lapa_bias ~ 1 + is_fu_r + extra_Hi + neur_Lo + lapa_mean_C + lapa_rec_C + lapa_max_C + is_fu_r*extra_Hi + is_fu_r*neur_Lo + is_fu_r*lapa_rec_C + sex + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu_r | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lapa_loE_hiN <- lmer(lapa_bias ~ 1 + is_fu_r + extra_Lo + neur_Hi + lapa_mean_C + lapa_rec_C + lapa_max_C + is_fu_r*extra_Lo + is_fu_r*neur_Hi + is_fu_r*lapa_rec_C + sex + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu_r | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lapa_loE_loN <- lmer(lapa_bias ~ 1 + is_fu_r + extra_Lo + neur_Lo + lapa_mean_C + lapa_rec_C + lapa_max_C + is_fu_r*extra_Lo + is_fu_r*neur_Lo + is_fu_r*lapa_rec_C + sex + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu_r | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))

summary(hana_hiE_hiN <- lmer(hana_bias ~ 1 + is_fu_r + extra_Hi + neur_Hi + hana_mean_C + hana_rec_C + hana_max_C + is_fu_r*extra_Hi + is_fu_r*neur_Hi + is_fu_r*hana_rec_C + sex + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu_r | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hana_hiE_loN <- lmer(hana_bias ~ 1 + is_fu_r + extra_Hi + neur_Lo + hana_mean_C + hana_rec_C + hana_max_C + is_fu_r*extra_Hi + is_fu_r*neur_Lo + is_fu_r*hana_rec_C + sex + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu_r | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hana_loE_hiN <- lmer(hana_bias ~ 1 + is_fu_r + extra_Lo + neur_Hi + hana_mean_C + hana_rec_C + hana_max_C + is_fu_r*extra_Lo + is_fu_r*neur_Hi + is_fu_r*hana_rec_C + sex + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu_r | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hana_loE_loN <- lmer(hana_bias ~ 1 + is_fu_r + extra_Lo + neur_Lo + hana_mean_C + hana_rec_C + hana_max_C + is_fu_r*extra_Lo + is_fu_r*neur_Lo + is_fu_r*hana_rec_C + sex + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu_r | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))

summary(lana_hiE_hiN <- lmer(lana_bias ~ 1 + is_fu_r + extra_Hi + neur_Hi + lana_mean_C + lana_rec_C + lana_max_C + is_fu_r*extra_Hi + is_fu_r*neur_Hi + is_fu_r*lana_rec_C + sex + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu_r | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lana_hiE_loN <- lmer(lana_bias ~ 1 + is_fu_r + extra_Hi + neur_Lo + lana_mean_C + lana_rec_C + lana_max_C + is_fu_r*extra_Hi + is_fu_r*neur_Lo + is_fu_r*lana_rec_C + sex + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu_r | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lana_loE_hiN <- lmer(lana_bias ~ 1 + is_fu_r + extra_Lo + neur_Hi + lana_mean_C + lana_rec_C + lana_max_C + is_fu_r*extra_Lo + is_fu_r*neur_Hi + is_fu_r*lana_rec_C + sex + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu_r | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lana_loE_loN <- lmer(lana_bias ~ 1 + is_fu_r + extra_Lo + neur_Lo + lana_mean_C + lana_rec_C + lana_max_C + is_fu_r*extra_Lo + is_fu_r*neur_Lo + is_fu_r*lana_rec_C + sex + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu_r | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))



## JAn 2 2016 - PLOT bias-time slopes for different combinations of extraversion and neuroticism - EXIT VS FOLLOWUP

# HAPA - Neuroticism and extraversion simple slopes
fixef(hapa_k)
B1<-fixef(hapa_k)[1] # intercept
B3<-fixef(hapa_k)[6] # neuroticism slope
B5<-fixef(hapa_k)[7] # extraversion slope
B2<-fixef(hapa_k)[2] # time slope
B4<-fixef(hapa_k)[16] # neuroticism x time
B6<-fixef(hapa_k)[17] # extraversion x time

## plot hapa bias-time slopes

# High extraversion, high neuroticism
int_hiE_hiN<-( B1 + B3 * ( mean(na.omit(d1$neur_C))+sd(na.omit(d1$neur_C)) ) + B5 * ( mean(na.omit(d1$extra_C))+sd(na.omit(d1$extra_C)) ) )
sl_hiE_hiN<-( B2 + B4 * ( mean(na.omit(d1$neur_C))+sd(na.omit(d1$neur_C)) ) + B6 * ( mean(na.omit(d1$extra_C))+sd(na.omit(d1$extra_C)) ) )

## High extraversion, low neuroticism
int_hiE_loN<-( B1 + B3 * ( mean(na.omit(d1$neur_C))-sd(na.omit(d1$neur_C)) ) + B5 * ( mean(na.omit(d1$extra_C))+sd(na.omit(d1$extra_C)) ) )
sl_hiE_loN<-( B2 + B4 * ( mean(na.omit(d1$neur_C))-sd(na.omit(d1$neur_C)) ) + B6 * ( mean(na.omit(d1$extra_C))+sd(na.omit(d1$extra_C)) ) )

## Low extraversion, high neuroticism
int_loE_hiN<-( B1 + B3 * ( mean(na.omit(d1$neur_C))+sd(na.omit(d1$neur_C)) ) + B5 * ( mean(na.omit(d1$extra_C))-sd(na.omit(d1$extra_C)) ) )
sl_loE_hiN<-( B2 + B4 * ( mean(na.omit(d1$neur_C))+sd(na.omit(d1$neur_C)) ) + B6 * ( mean(na.omit(d1$extra_C))-sd(na.omit(d1$extra_C)) ) )

## Low extraversion, low neuroticism
int_loE_loN<-( B1 + B3 * ( mean(na.omit(d1$neur_C))-sd(na.omit(d1$neur_C)) ) + B5 * ( mean(na.omit(d1$extra_C))-sd(na.omit(d1$extra_C)) ) )
sl_loE_loN<-( B2 + B4 * ( mean(na.omit(d1$neur_C))-sd(na.omit(d1$neur_C)) ) + B6 * ( mean(na.omit(d1$extra_C))-sd(na.omit(d1$extra_C)) ) )

# Plotting the datapoints
myGrid<-seq(from = -3, to = -3, by=.01)
plot(myGrid, xlim=c(0,1), ylim=c(-1,1), xlab="Elapsed Time (weeks)", ylab="HAPA Retrospective Report Bias", frame.plot=FALSE)

Xs<-c(0, 1)
lines(Xs,(int_hiE_hiN + sl_hiE_hiN*Xs), col = "slateblue2", lwd = 3, lty="dashed")
lines(Xs,(int_hiE_loN + sl_hiE_loN*Xs), col = "slateblue2", lwd = 3)
lines(Xs,(int_loE_hiN + sl_loE_hiN*Xs), col = "maroon3", lwd = 3, lty="dashed")
lines(Xs,(int_loE_loN + sl_loE_loN*Xs), col = "maroon3", lwd = 3)

## FROM HERE ON, FIX

legend(0,1, c("High Neuroticism (+1 SD)", "Low Neuroticism (-1 SD)", "High Extraversion (+1 SD)", "Low Extraversion (-1 SD)"), lty = 1:2:1:2, col = c("slateblue2","slateblue2","maroon3","maroon3"), y.intersp=1)

# Effect sizes
print(lmer(retro_wks ~ 1 + (1  | id), data=d1), digits=10) # SD of predictor of interest = unconditional residual = numerator in effect size equations
print(lmer(hapa_bias ~ 1 + (1  | id), data=d1), digits=10) # SD of DV = unconditional residual = denominator in effect size calculations

print(hapa_d_lo <- (2*(B2_n + B4_n*(-1)*sd(na.omit(d1$neur_C)))*3.652569798)/0.4629653914, digits = 10)
print(hapa_d_hi <- (2*(B2_n + B4_n*(+1)*sd(na.omit(d1$neur_C)))*3.652569798)/0.4629653914, digits = 10)

print(hapa_d_lo <- (2*(B2_n + B6_n*(-1)*sd(na.omit(d1$extra_C)))*3.652569798)/0.4629653914, digits = 10)
print(hapa_d_hi <- (2*(B2_n + B6_n*(+1)*sd(na.omit(d1$extra_C)))*3.652569798)/0.4629653914, digits = 10)


# LAPA - Neuroticism and extraversion simple slopes
fixef(lapa_k)
B1<-fixef(lapa_k)[1] # intercept
B3<-fixef(lapa_k)[6] # neuroticism slope
B5<-fixef(lapa_k)[7] # extraversion slope
B2<-fixef(lapa_k)[2] # time slope
B4<-fixef(lapa_k)[16] # neuroticism x time
B6<-fixef(lapa_k)[17] # extraversion x time

## plot lapa bias-time slopes

# High extraversion, high neuroticism
int_hiE_hiN<-( B1 + B3 * ( mean(na.omit(d1$neur_C))+sd(na.omit(d1$neur_C)) ) + B5 * ( mean(na.omit(d1$extra_C))+sd(na.omit(d1$extra_C)) ) )
sl_hiE_hiN<-( B2 + B4 * ( mean(na.omit(d1$neur_C))+sd(na.omit(d1$neur_C)) ) + B6 * ( mean(na.omit(d1$extra_C))+sd(na.omit(d1$extra_C)) ) )

## High extraversion, low neuroticism
int_hiE_loN<-( B1 + B3 * ( mean(na.omit(d1$neur_C))-sd(na.omit(d1$neur_C)) ) + B5 * ( mean(na.omit(d1$extra_C))+sd(na.omit(d1$extra_C)) ) )
sl_hiE_loN<-( B2 + B4 * ( mean(na.omit(d1$neur_C))-sd(na.omit(d1$neur_C)) ) + B6 * ( mean(na.omit(d1$extra_C))+sd(na.omit(d1$extra_C)) ) )

## Low extraversion, high neuroticism
int_loE_hiN<-( B1 + B3 * ( mean(na.omit(d1$neur_C))+sd(na.omit(d1$neur_C)) ) + B5 * ( mean(na.omit(d1$extra_C))-sd(na.omit(d1$extra_C)) ) )
sl_loE_hiN<-( B2 + B4 * ( mean(na.omit(d1$neur_C))+sd(na.omit(d1$neur_C)) ) + B6 * ( mean(na.omit(d1$extra_C))-sd(na.omit(d1$extra_C)) ) )

## Low extraversion, low neuroticism
int_loE_loN<-( B1 + B3 * ( mean(na.omit(d1$neur_C))-sd(na.omit(d1$neur_C)) ) + B5 * ( mean(na.omit(d1$extra_C))-sd(na.omit(d1$extra_C)) ) )
sl_loE_loN<-( B2 + B4 * ( mean(na.omit(d1$neur_C))-sd(na.omit(d1$neur_C)) ) + B6 * ( mean(na.omit(d1$extra_C))-sd(na.omit(d1$extra_C)) ) )

# Plotting the datapoints
myGrid<-seq(from = -3, to = -3, by=.01)
plot(myGrid, xlim=c(0,1), ylim=c(-1,1), xlab="Elapsed Time (weeks)", ylab="LAPA Retrospective Report Bias", frame.plot=FALSE)

Xs<-c(0, 1)
lines(Xs,(int_hiE_hiN + sl_hiE_hiN*Xs), col = "slateblue2", lwd = 3, lty="dashed")
lines(Xs,(int_hiE_loN + sl_hiE_loN*Xs), col = "slateblue2", lwd = 3)
lines(Xs,(int_loE_hiN + sl_loE_hiN*Xs), col = "maroon3", lwd = 3, lty="dashed")
lines(Xs,(int_loE_loN + sl_loE_loN*Xs), col = "maroon3", lwd = 3)


# HANA - Neuroticism and extraversion simple slopes
fixef(hana_k)
B1<-fixef(hana_k)[1] # intercept
B3<-fixef(hana_k)[6] # neuroticism slope
B5<-fixef(hana_k)[7] # extraversion slope
B2<-fixef(hana_k)[2] # time slope
B4<-fixef(hana_k)[16] # neuroticism x time
B6<-fixef(hana_k)[17] # extraversion x time

## plot hana bias-time slopes

# High extraversion, high neuroticism
int_hiE_hiN<-( B1 + B3 * ( mean(na.omit(d1$neur_C))+sd(na.omit(d1$neur_C)) ) + B5 * ( mean(na.omit(d1$extra_C))+sd(na.omit(d1$extra_C)) ) )
sl_hiE_hiN<-( B2 + B4 * ( mean(na.omit(d1$neur_C))+sd(na.omit(d1$neur_C)) ) + B6 * ( mean(na.omit(d1$extra_C))+sd(na.omit(d1$extra_C)) ) )

## High extraversion, low neuroticism
int_hiE_loN<-( B1 + B3 * ( mean(na.omit(d1$neur_C))-sd(na.omit(d1$neur_C)) ) + B5 * ( mean(na.omit(d1$extra_C))+sd(na.omit(d1$extra_C)) ) )
sl_hiE_loN<-( B2 + B4 * ( mean(na.omit(d1$neur_C))-sd(na.omit(d1$neur_C)) ) + B6 * ( mean(na.omit(d1$extra_C))+sd(na.omit(d1$extra_C)) ) )

## Low extraversion, high neuroticism
int_loE_hiN<-( B1 + B3 * ( mean(na.omit(d1$neur_C))+sd(na.omit(d1$neur_C)) ) + B5 * ( mean(na.omit(d1$extra_C))-sd(na.omit(d1$extra_C)) ) )
sl_loE_hiN<-( B2 + B4 * ( mean(na.omit(d1$neur_C))+sd(na.omit(d1$neur_C)) ) + B6 * ( mean(na.omit(d1$extra_C))-sd(na.omit(d1$extra_C)) ) )

## Low extraversion, low neuroticism
int_loE_loN<-( B1 + B3 * ( mean(na.omit(d1$neur_C))-sd(na.omit(d1$neur_C)) ) + B5 * ( mean(na.omit(d1$extra_C))-sd(na.omit(d1$extra_C)) ) )
sl_loE_loN<-( B2 + B4 * ( mean(na.omit(d1$neur_C))-sd(na.omit(d1$neur_C)) ) + B6 * ( mean(na.omit(d1$extra_C))-sd(na.omit(d1$extra_C)) ) )

# Plotting the datapoints
myGrid<-seq(from = -3, to = -3, by=.01)
plot(myGrid, xlim=c(0,1), ylim=c(-1,1), xlab="Elapsed Time (weeks)", ylab="HANA Retrospective Report Bias", frame.plot=FALSE)

Xs<-c(0, 1)
lines(Xs,(int_hiE_hiN + sl_hiE_hiN*Xs), col = "slateblue2", lwd = 3, lty="dashed")
lines(Xs,(int_hiE_loN + sl_hiE_loN*Xs), col = "slateblue2", lwd = 3)
lines(Xs,(int_loE_hiN + sl_loE_hiN*Xs), col = "maroon3", lwd = 3, lty="dashed")
lines(Xs,(int_loE_loN + sl_loE_loN*Xs), col = "maroon3", lwd = 3)


# LANA - Neuroticism and extraversion simple slopes
fixef(lana_k)
B1<-fixef(lana_k)[1] # intercept
B3<-fixef(lana_k)[6] # neuroticism slope
B5<-fixef(lana_k)[7] # extraversion slope
B2<-fixef(lana_k)[2] # time slope
B4<-fixef(lana_k)[16] # neuroticism x time
B6<-fixef(lana_k)[17] # extraversion x time

## plot lana bias-time slopes

# High extraversion, high neuroticism
int_hiE_hiN<-( B1 + B3 * ( mean(na.omit(d1$neur_C))+sd(na.omit(d1$neur_C)) ) + B5 * ( mean(na.omit(d1$extra_C))+sd(na.omit(d1$extra_C)) ) )
sl_hiE_hiN<-( B2 + B4 * ( mean(na.omit(d1$neur_C))+sd(na.omit(d1$neur_C)) ) + B6 * ( mean(na.omit(d1$extra_C))+sd(na.omit(d1$extra_C)) ) )

## High extraversion, low neuroticism
int_hiE_loN<-( B1 + B3 * ( mean(na.omit(d1$neur_C))-sd(na.omit(d1$neur_C)) ) + B5 * ( mean(na.omit(d1$extra_C))+sd(na.omit(d1$extra_C)) ) )
sl_hiE_loN<-( B2 + B4 * ( mean(na.omit(d1$neur_C))-sd(na.omit(d1$neur_C)) ) + B6 * ( mean(na.omit(d1$extra_C))+sd(na.omit(d1$extra_C)) ) )

## Low extraversion, high neuroticism
int_loE_hiN<-( B1 + B3 * ( mean(na.omit(d1$neur_C))+sd(na.omit(d1$neur_C)) ) + B5 * ( mean(na.omit(d1$extra_C))-sd(na.omit(d1$extra_C)) ) )
sl_loE_hiN<-( B2 + B4 * ( mean(na.omit(d1$neur_C))+sd(na.omit(d1$neur_C)) ) + B6 * ( mean(na.omit(d1$extra_C))-sd(na.omit(d1$extra_C)) ) )

## Low extraversion, low neuroticism
int_loE_loN<-( B1 + B3 * ( mean(na.omit(d1$neur_C))-sd(na.omit(d1$neur_C)) ) + B5 * ( mean(na.omit(d1$extra_C))-sd(na.omit(d1$extra_C)) ) )
sl_loE_loN<-( B2 + B4 * ( mean(na.omit(d1$neur_C))-sd(na.omit(d1$neur_C)) ) + B6 * ( mean(na.omit(d1$extra_C))-sd(na.omit(d1$extra_C)) ) )

# Plotting the datapoints
myGrid<-seq(from = -3, to = -3, by=.01)
plot(myGrid, xlim=c(0,1), ylim=c(-1,1), xlab="Elapsed Time (weeks)", ylab="HANA Retrospective Report Bias", frame.plot=FALSE)

Xs<-c(0, 1)
lines(Xs,(int_hiE_hiN + sl_hiE_hiN*Xs), col = "slateblue2", lwd = 3, lty="dashed")
lines(Xs,(int_hiE_loN + sl_hiE_loN*Xs), col = "slateblue2", lwd = 3)
lines(Xs,(int_loE_hiN + sl_loE_hiN*Xs), col = "maroon3", lwd = 3, lty="dashed")
lines(Xs,(int_loE_loN + sl_loE_loN*Xs), col = "maroon3", lwd = 3)


## Correlation Table - use this with dataset containing one row per participant to get all
## person-level intercorrelations
d1ex_stvars <- d1_ex[,c(2:3,5:6,8:9,35:38,47:50,59:62,71:74,13)]
ct <- corr.test(d1ex_stvars, y=NULL, use="pairwise", method="pearson", adjust="holm", alpha=0.5)
ct
cts <- corr.test(d1ex_stvars, y=NULL, use="pairwise", method="spearman", adjust="holm", alpha=0.5)
cts

## Correlation Table - use this with dataset containing all retrospective reports per participant
## to get correlation with elapsed time and correlation with retrospective affect bias
d1_stvars <- d1[,c(2:3,5:6,8:9,35:38,47:50,59:62,71:74,13)]
ct <- corr.test(d1_stvars, y=NULL, use="pairwise", method="pearson", adjust="holm", alpha=0.5)
ct
cts <- corr.test(d1_stvars, y=NULL, use="pairwise", method="spearman", adjust="holm", alpha=0.5)
cts

## Reliability coefficients

summary(d3 <- read.table("Rdata_levelscollapsed_onlyincludedparticipants.csv",header=TRUE, sep=",")) # included participants only
summary(d4 <- read.table("mailout_dems_bfi_jl_onlyincludedparticipants.csv",header=TRUE, sep=",")) # included participants only
summary(d5 <- read.table("level2_onlyincludedparticipants.csv",header=TRUE, sep=",")) # included participants only

alpha_retro_hapa_e = alpha(subset(d3, select = c(happy_ret_e, exctd_ret_e)))
summary(alpha_retro_hapa_e)
alpha_retro_hapa_f = alpha(subset(d3, select = c(happy_ret_f, exctd_ret_f)))
summary(alpha_retro_hapa_f)
alpha_retro_lapa_e = alpha(subset(d3, select = c(quiet_ret_e, calm_ret_e)))
summary(alpha_retro_lapa_e)
alpha_retro_lapa_f = alpha(subset(d3, select = c(quiet_ret_f, calm_ret_f)))
summary(alpha_retro_lapa_f)
alpha_retro_hana_e = alpha(subset(d3, select = c(nrvous_ret_e, irrtated_ret_e)))
summary(alpha_retro_hana_e)
alpha_retro_hana_f = alpha(subset(d3, select = c(nrvous_ret_f, irrtated_ret_f)))
summary(alpha_retro_hana_f)
alpha_retro_lana_e = alpha(subset(d3, select = c(sad_ret_e, sleepy_ret_e)))
summary(alpha_retro_lana_e)
alpha_retro_lana_f = alpha(subset(d3, select = c(sad_ret_f, sleepy_ret_f)))
summary(alpha_retro_lana_f)

alpha_neur = alpha(subset(d4, select = c(bfi4,bfi9_r,bfi14,bfi19,bfi24_r,bfi29,bfi34_r,bfi39)))
summary(alpha_neur)
alpha_extra = alpha(subset(d4, select = c(bfi1,bfi6_r,bfi11,bfi16,bfi21_r,bfi26,bfi31_r,bfi36)))
summary(alpha_extra)
alpha_avg_hapa = alpha(subset(d5, select = c(happybp_mean, exctdbp_mean)))
summary(alpha_avg_hapa)
alpha_avg_hana = alpha(subset(d5, select = c(nervsbp_mean, irrtdbp_mean)))
summary(alpha_avg_hana)
alpha_avg_lapa = alpha(subset(d5, select = c(quietbp_mean, calmbp_mean)))
summary(alpha_avg_lapa)
alpha_avg_lana = alpha(subset(d5, select = c(sadbp_mean, slepybp_mean)))
summary(alpha_avg_lana)


