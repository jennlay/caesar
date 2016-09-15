## CAESAR study analyses, cleaned version -- Jan 2016

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
summary(d1_ex <- subset(d1, d1$is_fu == 0)) # exit reports only
summary(d1_fu <- subset(d1, d1$is_fu == 1)) # follow-up reports only
summary(dp <- read.table("level2_onlyincludedparticipants.csv",header=TRUE, sep=",")) # person-level data

# checking for differences between people who dropped out before follow-up and those who didn't
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
print(summary( a1 <- lm( consc ~ 1 + fu_did, data=dp )), digits=10)

d1$retro_wks <- d1$retro_time * 1/7

# Histograms

hist(dp$hapa_max, main="Peak HAPA")
describe(dp$hapa_max)
dp$log_hapa_max <- (dp$hapa_max)^20
hist(dp$log_hapa_max, main="Log Peak HAPA") 
describe(dp$log_hapa_max)

hist(dp$hana_max, main="Peak HANA") 
describe(dp$hana_max)
hist(dp$lapa_max, main="Peak LAPA") 
describe(dp$lapa_max)
hist(dp$lana_max, main="Peak LANA") 
describe(dp$lana_max)

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


## Correlation Table - use this with dataset containing one row per participant to get all
## person-level intercorrelations
d1ex_stvars <- d1_ex[,c(4:5,7:12,68:71,80:83,92:95)]
ct <- corr.test(d1ex_stvars, y=NULL, use="pairwise", method="pearson", adjust="holm", alpha=0.5)
print(ct,short=FALSE)
cts <- corr.test(d1ex_stvars, y=NULL, use="pairwise", method="spearman", adjust="holm", alpha=0.5)
cts

## Correlation Table - use this with dataset containing all retrospective reports per participant
## to get correlation with elapsed time and correlation with retrospective affect bias
d1_stvars <- d1[,c(3:5,7:12,68:71,80:83,92:95,104,107,110,113)]
ct <- corr.test(d1_stvars, y=NULL, use="pairwise", method="pearson", adjust="holm", alpha=0.5)
ct
cts <- corr.test(d1_stvars, y=NULL, use="pairwise", method="spearman", adjust="holm", alpha=0.5)
cts

# Unspecified models (for ICC and Pseudo Delta R squared)

summary(hapa_uns <- lmer(hapa_bias ~ 1 + (1 | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lapa_uns <- lmer(lapa_bias ~ 1 + (1 | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hana_uns <- lmer(hana_bias ~ 1 + (1 | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lana_uns <- lmer(lana_bias ~ 1 + (1 | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))

# unspecified models for exit reports only
summary(hapa_uns <- lmer(hapa_bias ~ 1 + is_fu + (1 | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lapa_uns <- lmer(lapa_bias ~ 1 + is_fu + (1 | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hana_uns <- lmer(hana_bias ~ 1 + is_fu + (1 | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lana_uns <- lmer(lana_bias ~ 1 + is_fu + (1 | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
mean(d1_ex$hapa_bias)
mean(d1_fu$hapa_bias)


# unspecified models for follow-up reports only
summary(hapa_uns <- lmer(hapa_bias ~ 1 + is_fu_r + (1 | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lapa_uns <- lmer(lapa_bias ~ 1 + is_fu_r + (1 | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hana_uns <- lmer(hana_bias ~ 1 + is_fu_r + (1 | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lana_uns <- lmer(lana_bias ~ 1 + is_fu_r + (1 | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))





# Unspecified models controlling for average momentary affect only

summary(hapa_uns <- lmer(hapa_bias ~ 1 + hapa_mean_C + (1 | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lapa_uns <- lmer(lapa_bias ~ 1 + lapa_mean_C + (1 | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hana_uns <- lmer(hana_bias ~ 1 + hana_mean_C + (1 | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lana_uns <- lmer(lana_bias ~ 1 + lana_mean_C + (1 | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))

## Testing the intercept variances
## (does not work)

FULL <- lmer(hapa_bias ~ 1 + is_fu + (1 + is_fu | id), data=d1, control = lmerControl(check.nobs.vs.nRE = "warning"))
intercepts <- lmList(hapa_bias ~ 1 + is_fu | id, data=na.omit(d1))
Bols <- summary(intercepts)$coefficients[,1,1]
Vols <- summary(intercepts)$coefficients[,2,1]^2 # estimate of sampling variance
sum( (Bols - fixef(FULL)[1])^2/Vols) # expected value for this is the df
pchisq(312.3966, df=79, lower.tail=FALSE)

##### COMPARING EXIT & FOLLOWUP instead of treating time as continuous #####

# Discrete emotions

summary(happy_3 <- lmer(happy_bias ~ 1 + is_fu + happy_mean_C + happy_rec_C + happy_max_C + neur_C + extra_C + is_fu*happy_rec_C + is_fu*happy_max_C + is_fu*neur_C + is_fu*extra_C + age_C + Avt_C + Lst_C + is_cauc + (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(exctd_3 <- lmer(exctd_bias ~ 1 + is_fu + exctd_mean_C + exctd_rec_C + exctd_max_C + neur_C + extra_C + is_fu*exctd_rec_C + is_fu*exctd_max_C + is_fu*neur_C + is_fu*extra_C + age_C + Avt_C + Lst_C + is_cauc + (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(calm_3 <- lmer(calm_bias ~ 1 + is_fu + calm_mean_C + calm_rec_C + calm_max_C + neur_C + extra_C + is_fu*calm_rec_C + is_fu*calm_max_C + is_fu*neur_C + is_fu*extra_C + age_C + Avt_C + Lst_C + is_cauc + (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(quiet_3 <- lmer(quiet_bias ~ 1 + is_fu + quiet_mean_C + quiet_rec_C + quiet_max_C + neur_C + extra_C + is_fu*quiet_rec_C + is_fu*quiet_max_C + is_fu*neur_C + is_fu*extra_C + age_C + Avt_C + Lst_C + is_cauc + (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(nrvous_3 <- lmer(nrvous_bias ~ 1 + is_fu + nrvous_mean_C + nrvous_rec_C + nrvous_max_C + neur_C + extra_C + is_fu*nrvous_rec_C + is_fu*nrvous_max_C + is_fu*neur_C + is_fu*extra_C + age_C + Avt_C + Lst_C + is_cauc + (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(irrtatd_3 <- lmer(irrtatd_bias ~ 1 + is_fu + irrtatd_mean_C + irrtatd_rec_C + irrtatd_max_C + neur_C + extra_C + is_fu*irrtatd_rec_C + is_fu*irrtatd_max_C + is_fu*neur_C + is_fu*extra_C + age_C + Avt_C + Lst_C + is_cauc + (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(sad_3 <- lmer(sad_bias ~ 1 + is_fu + sad_mean_C + sad_rec_C + sad_max_C + neur_C + extra_C + is_fu*sad_rec_C + is_fu*sad_max_C + is_fu*neur_C + is_fu*extra_C + age_C + Avt_C + Lst_C + is_cauc + (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(sleepy_3 <- lmer(sleepy_bias ~ 1 + is_fu + sleepy_mean_C + sleepy_rec_C + sleepy_max_C + neur_C + extra_C + is_fu*sleepy_rec_C + is_fu*sleepy_max_C + is_fu*neur_C + is_fu*extra_C + age_C + Avt_C + Lst_C + is_cauc + (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))

# Affect categories

summary(hapa_k <- lmer(hapa_bias ~ 1 + is_fu + hapa_mean_C + hapa_rec_C + hapa_max_C + neur_C + extra_C + is_fu*hapa_rec_C + is_fu*hapa_max_C + is_fu*neur_C + is_fu*extra_C + sex + age_C + Avt_C + Lst_C + is_cauc + edu + (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lapa_k <- lmer(lapa_bias ~ 1 + is_fu + lapa_mean_C + lapa_rec_C + lapa_max_C + neur_C + extra_C + is_fu*lapa_rec_C + is_fu*lapa_max_C + is_fu*neur_C + is_fu*extra_C + sex + age_C + Avt_C + Lst_C + is_cauc + edu + (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hana_k <- lmer(hana_bias ~ 1 + is_fu + hana_mean_C + hana_rec_C + hana_max_C + neur_C + extra_C + is_fu*hana_rec_C + is_fu*hana_max_C + is_fu*neur_C + is_fu*extra_C + sex + age_C + Avt_C + Lst_C + is_cauc + edu + (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lana_k <- lmer(lana_bias ~ 1 + is_fu + lana_mean_C + lana_rec_C + lana_max_C + neur_C + extra_C + is_fu*lana_rec_C + is_fu*lana_max_C + is_fu*neur_C + is_fu*extra_C + sex + age_C + Avt_C + Lst_C + is_cauc + edu + (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))

### Affect categories - without gender, education, and max*time (for parsimony!)

summary(hapa_kr <- lmer(hapa_bias ~ 1 + is_fu + hapa_mean_C + hapa_rec_C + hapa_max_C + neur_C + extra_C + is_fu*hapa_rec_C + is_fu*neur_C + is_fu*extra_C + age_C + Avt_C + Lst_C + is_cauc + (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lapa_kr <- lmer(lapa_bias ~ 1 + is_fu + lapa_mean_C + lapa_rec_C + lapa_max_C + neur_C + extra_C + is_fu*lapa_rec_C + is_fu*neur_C + is_fu*extra_C + age_C + Avt_C + Lst_C + is_cauc + (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hana_kr <- lmer(hana_bias ~ 1 + is_fu + hana_mean_C + hana_rec_C + hana_max_C + neur_C + extra_C + is_fu*hana_rec_C + is_fu*neur_C + is_fu*extra_C + age_C + Avt_C + Lst_C + is_cauc + (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lana_kr <- lmer(lana_bias ~ 1 + is_fu + lana_mean_C + lana_rec_C + lana_max_C + neur_C + extra_C + is_fu*lana_rec_C + is_fu*neur_C + is_fu*extra_C + age_C + Avt_C + Lst_C + is_cauc + (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))

### USE --- Adding max*time back in

summary(hapa_kr <- lmer(hapa_bias ~ 1 + is_fu + hapa_mean_C + hapa_rec_C + hapa_max_C + neur_C + extra_C + is_fu*hapa_rec_C + is_fu*hapa_max_C + is_fu*neur_C + is_fu*extra_C + age_C + Avt_C + Lst_C + is_cauc + (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lapa_kr <- lmer(lapa_bias ~ 1 + is_fu + lapa_mean_C + lapa_rec_C + lapa_max_C + neur_C + extra_C + is_fu*lapa_rec_C + is_fu*lapa_max_C + is_fu*neur_C + is_fu*extra_C + age_C + Avt_C + Lst_C + is_cauc + (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hana_kr <- lmer(hana_bias ~ 1 + is_fu + hana_mean_C + hana_rec_C + hana_max_C + neur_C + extra_C + is_fu*hana_rec_C + is_fu*hana_max_C + is_fu*neur_C + is_fu*extra_C + age_C + Avt_C + Lst_C + is_cauc + (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lana_kr <- lmer(lana_bias ~ 1 + is_fu + lana_mean_C + lana_rec_C + lana_max_C + neur_C + extra_C + is_fu*lana_rec_C + is_fu*lana_max_C + is_fu*neur_C + is_fu*extra_C + age_C + Avt_C + Lst_C + is_cauc + (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))

# Same as above, with follow-up coded as 0 (to look at follow-up intercept)
summary(hapa_kr <- lmer(hapa_bias ~ 1 + is_fu_r + hapa_mean_C + hapa_rec_C + hapa_max_C + neur_C + extra_C + is_fu_r*hapa_rec_C + is_fu_r*hapa_max_C + is_fu_r*neur_C + is_fu_r*extra_C + age_C + Avt_C + Lst_C + is_cauc + (1 + is_fu_r | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lapa_kr <- lmer(lapa_bias ~ 1 + is_fu_r + lapa_mean_C + lapa_rec_C + lapa_max_C + neur_C + extra_C + is_fu_r*lapa_rec_C + is_fu_r*lapa_max_C + is_fu_r*neur_C + is_fu_r*extra_C + age_C + Avt_C + Lst_C + is_cauc + (1 + is_fu_r | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hana_kr <- lmer(hana_bias ~ 1 + is_fu_r + hana_mean_C + hana_rec_C + hana_max_C + neur_C + extra_C + is_fu_r*hana_rec_C + is_fu_r*hana_max_C + is_fu_r*neur_C + is_fu_r*extra_C + age_C + Avt_C + Lst_C + is_cauc + (1 + is_fu_r | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lana_kr <- lmer(lana_bias ~ 1 + is_fu_r + lana_mean_C + lana_rec_C + lana_max_C + neur_C + extra_C + is_fu_r*lana_rec_C + is_fu_r*lana_max_C + is_fu_r*neur_C + is_fu_r*extra_C + age_C + Avt_C + Lst_C + is_cauc + (1 + is_fu_r | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))


### Mar 20 - adding conscientiousness

summary(hapa_kr <- lmer(hapa_bias ~ 1 + is_fu + hapa_mean_C + hapa_rec_C + hapa_max_C + neur_C + extra_C + is_fu*hapa_rec_C + is_fu*hapa_max_C + is_fu*neur_C + is_fu*extra_C + age_C + Avt_C + Lst_C + is_cauc + consc + (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lapa_kr <- lmer(lapa_bias ~ 1 + is_fu + lapa_mean_C + lapa_rec_C + lapa_max_C + neur_C + extra_C + is_fu*lapa_rec_C + is_fu*lapa_max_C + is_fu*neur_C + is_fu*extra_C + age_C + Avt_C + Lst_C + is_cauc + consc + (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hana_kr <- lmer(hana_bias ~ 1 + is_fu + hana_mean_C + hana_rec_C + hana_max_C + neur_C + extra_C + is_fu*hana_rec_C + is_fu*hana_max_C + is_fu*neur_C + is_fu*extra_C + age_C + Avt_C + Lst_C + is_cauc + consc + (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lana_kr <- lmer(lana_bias ~ 1 + is_fu + lana_mean_C + lana_rec_C + lana_max_C + neur_C + extra_C + is_fu*lana_rec_C + is_fu*lana_max_C + is_fu*neur_C + is_fu*extra_C + age_C + Avt_C + Lst_C + is_cauc + consc + (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))

### Mar 22 - just a test, without average momentary affect

summary(hapa_kr <- lmer(hapa_bias ~ 1 + retro_wks + hapa_rec_C + hapa_max_C + neur_C + extra_C + retro_wks*hapa_rec_C + retro_wks*hapa_max_C + retro_wks*neur_C + retro_wks*extra_C + sex + age_C + Avt_C + Lst_C + is_cauc + edu + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lapa_kr <- lmer(lapa_bias ~ 1 + retro_wks + lapa_rec_C + lapa_max_C + neur_C + extra_C + retro_wks*lapa_rec_C + retro_wks*lapa_max_C + retro_wks*neur_C + retro_wks*extra_C + sex + age_C + Avt_C + Lst_C + is_cauc + edu + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hana_kr <- lmer(hana_bias ~ 1 + retro_wks + hana_rec_C + hana_max_C + neur_C + extra_C + retro_wks*hana_rec_C + retro_wks*hana_max_C + retro_wks*neur_C + retro_wks*extra_C + sex + age_C + Avt_C + Lst_C + is_cauc + edu + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lana_kr <- lmer(lana_bias ~ 1 + retro_wks + lana_rec_C + lana_max_C + neur_C + extra_C + retro_wks*lana_rec_C + retro_wks*lana_max_C + retro_wks*neur_C + retro_wks*extra_C + sex + age_C + Avt_C + Lst_C + is_cauc + edu + (1 + retro_wks | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))


## Looking at reduction in deviance

anova(hapa_kr, hapa_uns)
anova(lapa_kr, lapa_uns)
anova(hana_kr, hana_uns)
anova(lana_kr, lana_uns)


## Simple slopes

sd(na.omit(d1$neur_C))
d1$neur_Hi <- d1$neur_C - sd(na.omit(d1$neur_C))
d1$neur_Lo <- d1$neur_C + sd(na.omit(d1$neur_C))

sd(na.omit(d1$extra_C))
d1$extra_Hi <- d1$extra_C - sd(na.omit(d1$extra_C))
d1$extra_Lo <- d1$extra_C + sd(na.omit(d1$extra_C))

### USE *** JAN 1 2016 - REDUCED Simple slopes for different combinations of High vs. Low Neuroticsm and Extraversion - EXIT VS FOLLOWUP
# When testing intercept significance (for CIs), used df = 179-14-1 = 16

summary(hapa_hiE_hiN <- lmer(hapa_bias ~ 1 + is_fu + extra_Hi + neur_Hi + hapa_mean_C + hapa_rec_C + hapa_max_C + is_fu*extra_Hi + is_fu*neur_Hi + is_fu*hapa_rec_C + is_fu*hapa_max_C + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hapa_hiE_loN <- lmer(hapa_bias ~ 1 + is_fu + extra_Hi + neur_Lo + hapa_mean_C + hapa_rec_C + hapa_max_C + is_fu*extra_Hi + is_fu*neur_Lo + is_fu*hapa_rec_C + is_fu*hapa_max_C + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hapa_loE_hiN <- lmer(hapa_bias ~ 1 + is_fu + extra_Lo + neur_Hi + hapa_mean_C + hapa_rec_C + hapa_max_C + is_fu*extra_Lo + is_fu*neur_Hi + is_fu*hapa_rec_C + is_fu*hapa_max_C + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hapa_loE_loN <- lmer(hapa_bias ~ 1 + is_fu + extra_Lo + neur_Lo + hapa_mean_C + hapa_rec_C + hapa_max_C + is_fu*extra_Lo + is_fu*neur_Lo + is_fu*hapa_rec_C + is_fu*hapa_max_C + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))

summary(lapa_hiE_hiN <- lmer(lapa_bias ~ 1 + is_fu + extra_Hi + neur_Hi + lapa_mean_C + lapa_rec_C + lapa_max_C + is_fu*extra_Hi + is_fu*neur_Hi + is_fu*lapa_rec_C + is_fu*lapa_max_C + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lapa_hiE_loN <- lmer(lapa_bias ~ 1 + is_fu + extra_Hi + neur_Lo + lapa_mean_C + lapa_rec_C + lapa_max_C + is_fu*extra_Hi + is_fu*neur_Lo + is_fu*lapa_rec_C + is_fu*lapa_max_C + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lapa_loE_hiN <- lmer(lapa_bias ~ 1 + is_fu + extra_Lo + neur_Hi + lapa_mean_C + lapa_rec_C + lapa_max_C + is_fu*extra_Lo + is_fu*neur_Hi + is_fu*lapa_rec_C + is_fu*lapa_max_C + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lapa_loE_loN <- lmer(lapa_bias ~ 1 + is_fu + extra_Lo + neur_Lo + lapa_mean_C + lapa_rec_C + lapa_max_C + is_fu*extra_Lo + is_fu*neur_Lo + is_fu*lapa_rec_C + is_fu*lapa_max_C + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))

summary(hana_hiE_hiN <- lmer(hana_bias ~ 1 + is_fu + extra_Hi + neur_Hi + hana_mean_C + hana_rec_C + hana_max_C + is_fu*extra_Hi + is_fu*neur_Hi + is_fu*hana_rec_C + is_fu*hana_max_C + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hana_hiE_loN <- lmer(hana_bias ~ 1 + is_fu + extra_Hi + neur_Lo + hana_mean_C + hana_rec_C + hana_max_C + is_fu*extra_Hi + is_fu*neur_Lo + is_fu*hana_rec_C + is_fu*hana_max_C + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hana_loE_hiN <- lmer(hana_bias ~ 1 + is_fu + extra_Lo + neur_Hi + hana_mean_C + hana_rec_C + hana_max_C + is_fu*extra_Lo + is_fu*neur_Hi + is_fu*hana_rec_C + is_fu*hana_max_C + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hana_loE_loN <- lmer(hana_bias ~ 1 + is_fu + extra_Lo + neur_Lo + hana_mean_C + hana_rec_C + hana_max_C + is_fu*extra_Lo + is_fu*neur_Lo + is_fu*hana_rec_C + is_fu*hana_max_C + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))

summary(lana_hiE_hiN <- lmer(lana_bias ~ 1 + is_fu + extra_Hi + neur_Hi + lana_mean_C + lana_rec_C + lana_max_C + is_fu*extra_Hi + is_fu*neur_Hi + is_fu*lana_rec_C + is_fu*lana_max_C + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lana_hiE_loN <- lmer(lana_bias ~ 1 + is_fu + extra_Hi + neur_Lo + lana_mean_C + lana_rec_C + lana_max_C + is_fu*extra_Hi + is_fu*neur_Lo + is_fu*lana_rec_C + is_fu*lana_max_C + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lana_loE_hiN <- lmer(lana_bias ~ 1 + is_fu + extra_Lo + neur_Hi + lana_mean_C + lana_rec_C + lana_max_C + is_fu*extra_Lo + is_fu*neur_Hi + is_fu*lana_rec_C + is_fu*lana_max_C + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lana_loE_loN <- lmer(lana_bias ~ 1 + is_fu + extra_Lo + neur_Lo + lana_mean_C + lana_rec_C + lana_max_C + is_fu*extra_Lo + is_fu*neur_Lo + is_fu*lana_rec_C + is_fu*lana_max_C + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))


### (JAN 1 2016 - REDUCED Simple slopes for different combinations of High vs. Low Neuroticsm and Extraversion - EXIT VS FOLLOWUP)
# follow-up coded as 0 to look at intercept significance 

summary(hapa_hiE_hiN <- lmer(hapa_bias ~ 1 + is_fu_r + extra_Hi + neur_Hi + hapa_mean_C + hapa_rec_C + hapa_max_C + is_fu_r*extra_Hi + is_fu_r*neur_Hi + is_fu_r*hapa_rec_C + is_fu_r*hapa_max_C + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu_r | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hapa_hiE_loN <- lmer(hapa_bias ~ 1 + is_fu_r + extra_Hi + neur_Lo + hapa_mean_C + hapa_rec_C + hapa_max_C + is_fu_r*extra_Hi + is_fu_r*neur_Lo + is_fu_r*hapa_rec_C + is_fu_r*hapa_max_C + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu_r | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hapa_loE_hiN <- lmer(hapa_bias ~ 1 + is_fu_r + extra_Lo + neur_Hi + hapa_mean_C + hapa_rec_C + hapa_max_C + is_fu_r*extra_Lo + is_fu_r*neur_Hi + is_fu_r*hapa_rec_C + is_fu_r*hapa_max_C + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu_r | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hapa_loE_loN <- lmer(hapa_bias ~ 1 + is_fu_r + extra_Lo + neur_Lo + hapa_mean_C + hapa_rec_C + hapa_max_C + is_fu_r*extra_Lo + is_fu_r*neur_Lo + is_fu_r*hapa_rec_C + is_fu_r*hapa_max_C + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu_r | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))

summary(lapa_hiE_hiN <- lmer(lapa_bias ~ 1 + is_fu_r + extra_Hi + neur_Hi + lapa_mean_C + lapa_rec_C + lapa_max_C + is_fu_r*extra_Hi + is_fu_r*neur_Hi + is_fu_r*lapa_rec_C + is_fu_r*lapa_max_C + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu_r | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lapa_hiE_loN <- lmer(lapa_bias ~ 1 + is_fu_r + extra_Hi + neur_Lo + lapa_mean_C + lapa_rec_C + lapa_max_C + is_fu_r*extra_Hi + is_fu_r*neur_Lo + is_fu_r*lapa_rec_C + is_fu_r*lapa_max_C + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu_r | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lapa_loE_hiN <- lmer(lapa_bias ~ 1 + is_fu_r + extra_Lo + neur_Hi + lapa_mean_C + lapa_rec_C + lapa_max_C + is_fu_r*extra_Lo + is_fu_r*neur_Hi + is_fu_r*lapa_rec_C + is_fu_r*lapa_max_C + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu_r | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lapa_loE_loN <- lmer(lapa_bias ~ 1 + is_fu_r + extra_Lo + neur_Lo + lapa_mean_C + lapa_rec_C + lapa_max_C + is_fu_r*extra_Lo + is_fu_r*neur_Lo + is_fu_r*lapa_rec_C + is_fu_r*lapa_max_C + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu_r | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))

summary(hana_hiE_hiN <- lmer(hana_bias ~ 1 + is_fu_r + extra_Hi + neur_Hi + hana_mean_C + hana_rec_C + hana_max_C + is_fu_r*extra_Hi + is_fu_r*neur_Hi + is_fu_r*hana_rec_C + is_fu_r*hana_max_C + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu_r | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hana_hiE_loN <- lmer(hana_bias ~ 1 + is_fu_r + extra_Hi + neur_Lo + hana_mean_C + hana_rec_C + hana_max_C + is_fu_r*extra_Hi + is_fu_r*neur_Lo + is_fu_r*hana_rec_C + is_fu_r*hana_max_C + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu_r | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hana_loE_hiN <- lmer(hana_bias ~ 1 + is_fu_r + extra_Lo + neur_Hi + hana_mean_C + hana_rec_C + hana_max_C + is_fu_r*extra_Lo + is_fu_r*neur_Hi + is_fu_r*hana_rec_C + is_fu_r*hana_max_C + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu_r | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(hana_loE_loN <- lmer(hana_bias ~ 1 + is_fu_r + extra_Lo + neur_Lo + hana_mean_C + hana_rec_C + hana_max_C + is_fu_r*extra_Lo + is_fu_r*neur_Lo + is_fu_r*hana_rec_C + is_fu_r*hana_max_C + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu_r | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))

summary(lana_hiE_hiN <- lmer(lana_bias ~ 1 + is_fu_r + extra_Hi + neur_Hi + lana_mean_C + lana_rec_C + lana_max_C + is_fu_r*extra_Hi + is_fu_r*neur_Hi + is_fu_r*lana_rec_C + is_fu_r*lana_max_C + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu_r | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lana_hiE_loN <- lmer(lana_bias ~ 1 + is_fu_r + extra_Hi + neur_Lo + lana_mean_C + lana_rec_C + lana_max_C + is_fu_r*extra_Hi + is_fu_r*neur_Lo + is_fu_r*lana_rec_C + is_fu_r*lana_max_C + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu_r | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lana_loE_hiN <- lmer(lana_bias ~ 1 + is_fu_r + extra_Lo + neur_Hi + lana_mean_C + lana_rec_C + lana_max_C + is_fu_r*extra_Lo + is_fu_r*neur_Hi + is_fu_r*lana_rec_C + is_fu_r*lana_max_C + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu_r | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))
summary(lana_loE_loN <- lmer(lana_bias ~ 1 + is_fu_r + extra_Lo + neur_Lo + lana_mean_C + lana_rec_C + lana_max_C + is_fu_r*extra_Lo + is_fu_r*neur_Lo + is_fu_r*lana_rec_C + is_fu_r*lana_max_C + age_C + Avt_C + Lst_C + is_cauc +  (1 + is_fu_r | id), data = d1, control = lmerControl(check.nobs.vs.nRE = "warning")))


### JAN 1 2-16 - REDUCED Simple slopes for High and Low Neuroticsm and Extraversion - EXIT VS FOLLOWUP

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


### JAN 1 2-16 - REDUCED Simple slopes for High and Low Neuroticsm and Extraversion - EXIT VS FOLLOWUP
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

## Plotting simple slopes

## JAn 2 2016 - PLOT bias-time slopes for different combinations of extraversion and neuroticism - EXIT VS FOLLOWUP

# HAPA - Neuroticism and extraversion simple slopes
fixef(hapa_kr)
B1<-fixef(hapa_kr)[1] # intercept
B3<-fixef(hapa_kr)[6] # neuroticism slope
B5<-fixef(hapa_kr)[7] # extraversion slope
B2<-fixef(hapa_kr)[2] # time slope
B4<-fixef(hapa_kr)[14] # neuroticism x time
B6<-fixef(hapa_kr)[15] # extraversion x time

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
plot(myGrid, xlim=c(0,1), ylim=c(-0.5,1), xlab="Retrospective Report Time", ylab="HAPA Report Discrepancy", frame.plot=FALSE)

Xs<-c(0, 1)
Xsl<-c(-0.05,1.03)

points(Xs,(int_hiE_hiN + sl_hiE_hiN*Xs), col = "maroon3", cex = 1.4, pch = 19)
lines(Xs,(int_hiE_hiN + sl_hiE_hiN*Xs), col = "maroon3", lwd = 3)
points(Xs,(int_loE_hiN + sl_loE_hiN*Xs), col = "maroon3", cex = 1.4, pch = 19)
lines(Xs,(int_loE_hiN + sl_loE_hiN*Xs), col = "maroon3", lwd = 3, lty="dashed")
points(Xs,(int_hiE_loN + sl_hiE_loN*Xs), col = "slateblue2", cex = 1.4, pch = 19)
lines(Xs,(int_hiE_loN + sl_hiE_loN*Xs), col = "slateblue2", lwd = 3)
points(Xs,(int_loE_loN + sl_loE_loN*Xs), col = "slateblue2", cex = 1.4, pch = 19)
lines(Xs,(int_loE_loN + sl_loE_loN*Xs), col = "slateblue2", lwd = 3, lty="dashed")
lines(Xsl,c(0,0), col = "black", lwd = 2, lty="dotted")

legend(0,1, c("High Neuroticism (+1 SD), High Extraversion (+1 SD)", "High Neuroticism (+1 SD), Low Extraversion (-1 SD)", "Low Neuroticism (-1 SD), High Extraversion (+1 SD)", "Low Neuroticism (-1 SD), Low Extraversion (-1 SD)"), lty = 1:2:1:2, lwd = 3:3:3:3, col = c("maroon3","maroon3","slateblue2","slateblue2"), cex = 0.9:0.9:0.9:0.9, y.intersp=1)

# Slope Effect sizes
print(lmer(is_fu ~ 1 + (1  | id), data=d1), digits=10) # SD of predictor of interest = unconditional residual = numerator in effect size equations
print(lmer(hapa_bias ~ 1 + (1  | id), data=d1), digits=10) # SD of DV = unconditional residual = denominator in effect size calculations

print(hapa_dslope_hiN_hiE <- (2*(B2 + B4*(+1)*sd(na.omit(d1$neur_C)) + B6*(+1)*sd(na.omit(d1$extra_C)))*0.4976386258)/0.4629653889, digits = 10)
print(hapa_dslope_loN_hiE <- (2*(B2 + B4*(-1)*sd(na.omit(d1$neur_C)) + B6*(+1)*sd(na.omit(d1$extra_C)))*0.4976386258)/0.4629653889, digits = 10)

print(hapa_dslope_hiN_loE <- (2*(B2 + B4*(+1)*sd(na.omit(d1$neur_C)) + B6*(-1)*sd(na.omit(d1$extra_C)))*0.4976386258)/0.4629653889, digits = 10)
print(hapa_dslope_loN_loE <- (2*(B2 + B4*(-1)*sd(na.omit(d1$neur_C)) + B6*(-1)*sd(na.omit(d1$extra_C)))*0.4976386258)/0.4629653889, digits = 10)


## plot hapa bias-time slope at different values of neuroticism
## +1 SD
n_int_hi<-(B1 + B3*(mean(na.omit(d1$neur_C))+sd(na.omit(d1$neur_C))))
n_sl_hi<-(B2 + B4*(mean(na.omit(d1$neur_C))+sd(na.omit(d1$neur_C))))
## -1 SD
n_int_lo<-(B1 + B3*(mean(na.omit(d1$neur_C))-sd(na.omit(d1$neur_C))))
n_sl_lo<-(B2 + B4*(mean(na.omit(d1$neur_C))-sd(na.omit(d1$neur_C))))

## plot hapa bias-time slope at different values of extraversion
## +1 SD
e_int_hi<-(B1 + B5*(mean(na.omit(d1$extra_C))+sd(na.omit(d1$extra_C))))
e_sl_hi<-(B2 + B6*(mean(na.omit(d1$extra_C))+sd(na.omit(d1$extra_C))))
## -1 SD
e_int_lo<-(B1 + B5*(mean(na.omit(d1$extra_C))-sd(na.omit(d1$extra_C))))
e_sl_lo<-(B2 + B6*(mean(na.omit(d1$extra_C))-sd(na.omit(d1$extra_C))))

# Plotting the datapoints
myGrid<-seq(from = -3, to = -3, by=.01)
plot(myGrid, xlim=c(0,20), ylim=c(-0.5,1), xlab="Elapsed Time (weeks)", ylab="HAPA Retrospective Report Bias", frame.plot=FALSE)

Xs<-c(0, 20)
lines(Xs,(n_int_lo + n_sl_lo*Xs), col = "slateblue2", lwd = 3, lty="dashed")
lines(Xs,(n_int_hi + n_sl_hi*Xs), col = "slateblue2", lwd = 3)
lines(Xs,(e_int_lo + e_sl_lo*Xs), col = "maroon3", lwd = 3, lty="dashed")
lines(Xs,(e_int_hi + e_sl_hi*Xs), col = "maroon3", lwd = 3)

legend(10,1, c("High Neuroticism (+1 SD)", "Low Neuroticism (-1 SD)", "High Extraversion (+1 SD)", "Low Extraversion (-1 SD)"), lty = 1:2:1:2, col = c("slateblue2","slateblue2","maroon3","maroon3"), y.intersp=1)

# LAPA - Neuroticism and extraversion simple slopes
fixef(lapa_kr)
B1<-fixef(lapa_kr)[1] # intercept
B3<-fixef(lapa_kr)[6] # neuroticism slope
B5<-fixef(lapa_kr)[7] # extraversion slope
B2<-fixef(lapa_kr)[2] # time slope
B4<-fixef(lapa_kr)[14] # neuroticism x time
B6<-fixef(lapa_kr)[15] # extraversion x time

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
plot(myGrid, xlim=c(0,1), ylim=c(-0.5,1), xlab="Retrospective Report Time", ylab="LAPA Report Discrepancy", frame.plot=FALSE)

Xs<-c(0, 1)
Xsl<-c(-0.05,1.03)

points(Xs,(int_hiE_hiN + sl_hiE_hiN*Xs), col = "maroon3", cex = 1.4, pch = 19)
lines(Xs,(int_hiE_hiN + sl_hiE_hiN*Xs), col = "maroon3", lwd = 3)
points(Xs,(int_loE_hiN + sl_loE_hiN*Xs), col = "maroon3", cex = 1.4, pch = 19)
lines(Xs,(int_loE_hiN + sl_loE_hiN*Xs), col = "maroon3", lwd = 3, lty="dashed")
points(Xs,(int_hiE_loN + sl_hiE_loN*Xs), col = "slateblue2", cex = 1.4, pch = 19)
lines(Xs,(int_hiE_loN + sl_hiE_loN*Xs), col = "slateblue2", lwd = 3)
points(Xs,(int_loE_loN + sl_loE_loN*Xs), col = "slateblue2", cex = 1.4, pch = 19)
lines(Xs,(int_loE_loN + sl_loE_loN*Xs), col = "slateblue2", lwd = 3, lty="dashed")
lines(Xsl,c(0,0), col = "black", lwd = 2, lty="dotted")

legend(0,1, c("High Neuroticism (+1 SD), High Extraversion (+1 SD)", "High Neuroticism (+1 SD), Low Extraversion (-1 SD)", "Low Neuroticism (-1 SD), High Extraversion (+1 SD)", "Low Neuroticism (-1 SD), Low Extraversion (-1 SD)"), lty = 1:2:1:2, lwd = 3:3:3:3, col = c("maroon3","maroon3","slateblue2","slateblue2"), y.intersp=1)

# Slope Effect sizes
print(lmer(is_fu ~ 1 + (1  | id), data=d1), digits=10) # SD of predictor of interest = unconditional residual = numerator in effect size equations
print(lmer(lapa_bias ~ 1 + (1  | id), data=d1), digits=10) # SD of DV = unconditional residual = denominator in effect size calculations

print(lapa_dslope_hiN_hiE <- (2*(B2 + B4*(+1)*sd(na.omit(d1$neur_C)) + B6*(+1)*sd(na.omit(d1$extra_C)))*0.4976386258)/0.5081852004, digits = 10)
print(lapa_dslope_loN_hiE <- (2*(B2 + B4*(-1)*sd(na.omit(d1$neur_C)) + B6*(+1)*sd(na.omit(d1$extra_C)))*0.4976386258)/0.5081852004, digits = 10)

print(lapa_dslope_hiN_loE <- (2*(B2 + B4*(+1)*sd(na.omit(d1$neur_C)) + B6*(-1)*sd(na.omit(d1$extra_C)))*0.4976386258)/0.5081852004, digits = 10)
print(lapa_dslope_loN_loE <- (2*(B2 + B4*(-1)*sd(na.omit(d1$neur_C)) + B6*(-1)*sd(na.omit(d1$extra_C)))*0.4976386258)/0.5081852004, digits = 10)



# HANA - Neuroticism and extraversion simple slopes
fixef(hana_kr)
B1<-fixef(hana_kr)[1] # intercept
B3<-fixef(hana_kr)[6] # neuroticism slope
B5<-fixef(hana_kr)[7] # extraversion slope
B2<-fixef(hana_kr)[2] # time slope
B4<-fixef(hana_kr)[14] # neuroticism x time
B6<-fixef(hana_kr)[15] # extraversion x time

## plot hana bias-time slopes

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
plot(myGrid, xlim=c(0,1), ylim=c(-0.5,1), xlab="Retrospective Report Time", ylab="HANA Report Discrepancy", frame.plot=FALSE)

Xs<-c(0, 1)
Xsl<-c(-0.05,1.03)

points(Xs,(int_hiE_hiN + sl_hiE_hiN*Xs), col = "maroon3", cex = 1.4, pch = 19)
lines(Xs,(int_hiE_hiN + sl_hiE_hiN*Xs), col = "maroon3", lwd = 3)
points(Xs,(int_loE_hiN + sl_loE_hiN*Xs), col = "maroon3", cex = 1.4, pch = 19)
lines(Xs,(int_loE_hiN + sl_loE_hiN*Xs), col = "maroon3", lwd = 3, lty="dashed")
points(Xs,(int_hiE_loN + sl_hiE_loN*Xs), col = "slateblue2", cex = 1.4, pch = 19)
lines(Xs,(int_hiE_loN + sl_hiE_loN*Xs), col = "slateblue2", lwd = 3)
points(Xs,(int_loE_loN + sl_loE_loN*Xs), col = "slateblue2", cex = 1.4, pch = 19)
lines(Xs,(int_loE_loN + sl_loE_loN*Xs), col = "slateblue2", lwd = 3, lty="dashed")
lines(Xsl,c(0,0), col = "black", lwd = 2, lty="dotted")

legend(0,1, c("High Neuroticism (+1 SD), High Extraversion (+1 SD)", "High Neuroticism (+1 SD), Low Extraversion (-1 SD)", "Low Neuroticism (-1 SD), High Extraversion (+1 SD)", "Low Neuroticism (-1 SD), Low Extraversion (-1 SD)"), lty = 1:2:1:2, lwd = 3:3:3:3, col = c("maroon3","maroon3","slateblue2","slateblue2"), y.intersp=1)

# Slope Effect sizes
print(lmer(is_fu ~ 1 + (1  | id), data=d1), digits=10) # SD of predictor of interest = unconditional residual = numerator in effect size equations
print(lmer(hana_bias ~ 1 + (1  | id), data=d1), digits=10) # SD of DV = unconditional residual = denominator in effect size calculations

print(hana_dslope_hiN_hiE <- (2*(B2 + B4*(+1)*sd(na.omit(d1$neur_C)) + B6*(+1)*sd(na.omit(d1$extra_C)))*0.4976386258)/0.4851232584, digits = 10)
print(hana_dslope_loN_hiE <- (2*(B2 + B4*(-1)*sd(na.omit(d1$neur_C)) + B6*(+1)*sd(na.omit(d1$extra_C)))*0.4976386258)/0.4851232584, digits = 10)

print(hana_dslope_hiN_loE <- (2*(B2 + B4*(+1)*sd(na.omit(d1$neur_C)) + B6*(-1)*sd(na.omit(d1$extra_C)))*0.4976386258)/0.4851232584, digits = 10)
print(hana_dslope_loN_loE <- (2*(B2 + B4*(-1)*sd(na.omit(d1$neur_C)) + B6*(-1)*sd(na.omit(d1$extra_C)))*0.4976386258)/0.4851232584, digits = 10)




# LANA - Neuroticism and extraversion simple slopes
fixef(lana_kr)
B1<-fixef(lana_kr)[1] # intercept
B3<-fixef(lana_kr)[6] # neuroticism slope
B5<-fixef(lana_kr)[7] # extraversion slope
B2<-fixef(lana_kr)[2] # time slope
B4<-fixef(lana_kr)[14] # neuroticism x time
B6<-fixef(lana_kr)[15] # extraversion x time

## plot lana bias-time slopes

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
plot(myGrid, xlim=c(0,1), ylim=c(-0.5,1), xlab="Retrospective Report Time", ylab="LANA Report Discrepancy", frame.plot=FALSE)

Xs<-c(0, 1)
Xsl<-c(-0.05,1.03)

points(Xs,(int_hiE_hiN + sl_hiE_hiN*Xs), col = "maroon3", cex = 1.4, pch = 19)
lines(Xs,(int_hiE_hiN + sl_hiE_hiN*Xs), col = "maroon3", lwd = 3)
points(Xs,(int_loE_hiN + sl_loE_hiN*Xs), col = "maroon3", cex = 1.4, pch = 19)
lines(Xs,(int_loE_hiN + sl_loE_hiN*Xs), col = "maroon3", lwd = 3, lty="dashed")
points(Xs,(int_hiE_loN + sl_hiE_loN*Xs), col = "slateblue2", cex = 1.4, pch = 19)
lines(Xs,(int_hiE_loN + sl_hiE_loN*Xs), col = "slateblue2", lwd = 3)
points(Xs,(int_loE_loN + sl_loE_loN*Xs), col = "slateblue2", cex = 1.4, pch = 19)
lines(Xs,(int_loE_loN + sl_loE_loN*Xs), col = "slateblue2", lwd = 3, lty="dashed")
lines(Xsl,c(0,0), col = "black", lwd = 2, lty="dotted")

legend(0,1, c("High Neuroticism (+1 SD), High Extraversion (+1 SD)", "High Neuroticism (+1 SD), Low Extraversion (-1 SD)", "Low Neuroticism (-1 SD), High Extraversion (+1 SD)", "Low Neuroticism (-1 SD), Low Extraversion (-1 SD)"), lty = 1:2:1:2, lwd = 3:3:3:3, col = c("maroon3","maroon3","slateblue2","slateblue2"), y.intersp=1)

# Slope Effect sizes
print(lmer(is_fu ~ 1 + (1  | id), data=d1), digits=10) # SD of predictor of interest = unconditional residual = numerator in effect size equations
print(lmer(lana_bias ~ 1 + (1  | id), data=d1), digits=10) # SD of DV = unconditional residual = denominator in effect size calculations

print(lana_dslope_hiN_hiE <- (2*(B2 + B4*(+1)*sd(na.omit(d1$neur_C)) + B6*(+1)*sd(na.omit(d1$extra_C)))*0.4976386258)/0.4380834805, digits = 10)
print(lana_dslope_loN_hiE <- (2*(B2 + B4*(-1)*sd(na.omit(d1$neur_C)) + B6*(+1)*sd(na.omit(d1$extra_C)))*0.4976386258)/0.4380834805, digits = 10)

print(lana_dslope_hiN_loE <- (2*(B2 + B4*(+1)*sd(na.omit(d1$neur_C)) + B6*(-1)*sd(na.omit(d1$extra_C)))*0.4976386258)/0.4380834805, digits = 10)
print(lana_dslope_loN_loE <- (2*(B2 + B4*(-1)*sd(na.omit(d1$neur_C)) + B6*(-1)*sd(na.omit(d1$extra_C)))*0.4976386258)/0.4380834805, digits = 10)


