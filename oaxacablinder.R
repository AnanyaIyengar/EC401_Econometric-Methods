########################################
###ASSIGNMENT 2: Blau and Kahn (2017)###
########################################

#Here, we focus on only the 2011 wave of the PSID

#Installing Required Packages
library(haven)
library(readr)
library(dplyr)
library(ggplot2)
library(lmtest)
library(rmarkdown)
library(AER)
library(stats)
library(sandwich)
library(oaxaca)

#Importing the .dta file 
raw_data <- read_dta("assignment2.dta")
raw_data <- as.data.frame(raw_data)

#Retrieving Relevant Variables: lnrealwg, sex, sch, white, black, hisp, othrace, unjob, labincbus, annhrs, yrsexp, yrsExpSq

subset <- raw_data%>%dplyr::select(wave, lnrealwg, sex, sch, white, black, hisp, othrace, unjob, labincbus, annhrs, yrsexp, yrsExpSq)
subset <- subset[complete.cases(subset), ]

#We choose survey data of the 2011 wave
subset <- subset%>%dplyr::filter(wave == 2011)

subset$sex <- replace(subset$sex, subset$sex == 2, 0) #reference category: female (originally, 1: male, 2: female)

#Q1: Estimating the model y = bo + b1D + xb + e where y := lnrealwg, D := sex, X := the other chosen controls

#Specification 1 with schooling (sch) as a control
reg1 <- lm(data = subset, lnrealwg ~ sex + sch + white + black + hisp + unjob + labincbus + annhrs)
summary(reg1)
AIC(reg1)
#Specification 2 with experience (yrsexp) as a control
reg2 <- lm(data = subset, lnrealwg ~ sex + yrsexp + white + black + hisp + unjob + labincbus + annhrs)
summary(reg2)
AIC(reg2)
#Specification 3 with quadratic experience as a control
reg3 <- lm(data = subset, lnrealwg ~ sex + yrsexp + yrsExpSq + white + black + hisp + unjob + labincbus + annhrs)
summary(reg3)
AIC(reg3)
#Specification 4 with schooling, linear and quadratic experience as controls
reg4 <- lm(data = subset, lnrealwg ~ sex + yrsexp + yrsExpSq + sch + white + black + hisp + unjob + labincbus + annhrs)
summary(reg4)
AIC(reg4)

#Heteroskedasticity Check: Null is rejected!
bptest(reg4)

#Using Robust Standard Errors-> Will aid in estimation of variances of coefficient and covariance effects!
reg4_se <- coeftest(reg4, vcov = vcovHC(reg4, type = "HC0"))
reg4_se

#Choosing the best specification:

#In terms of Adjusted R^2: reg4 > reg1 > reg3 > reg2
#In terms of AIC: reg4 > reg1 > reg3 > reg2

#Thus, we choose specification 4!

#As per our a priori assumption, on an average, mean have higher hourly wages than women. Since we have a semi-log model wrt the dummy "sex", the coefficient is interpreted as per Halvorsen and Palmquist (1980)

#Q2: Estimating separate regression equations for male and female workers

subset_male <- subset%>%dplyr::filter(sex == 1)
subset_female <- subset%>%dplyr::filter(sex == 0)

reg_male <- lm(data = subset_male, lnrealwg ~ yrsexp + yrsExpSq + sch + white + black + hisp + unjob + labincbus + annhrs)
summary(reg_male)  

reg_female <- lm(data = subset_female, lnrealwg ~ yrsexp + yrsExpSq + sch + white + black + hisp + unjob + labincbus + annhrs)
summary(reg_female)


#TRIVIA!!! Evelyn Kitagawa was a demographer who first developed the covariance-coefficient effect decomposition.20 year later, Alan Blinder and Robert Oaxaca published their decomposition *without* citing Evelyn. Therefore, I will henceforth be calling it the Kitawaga-Oaxaca-Blinder (KOB) decomposition!

#Q3: Point Estimates

#Point Estimate of the Covariance Effect (Xa - Xb)Ba

Xa <- subset_male%>%dplyr::select(yrsexp, yrsExpSq, sch, white, black, hisp, unjob, labincbus, annhrs)
Xb <- subset_female%>%dplyr::select(yrsexp, yrsExpSq, sch, white, black, hisp, unjob, labincbus, annhrs)

summary(Xa)
summary(Xb)
meanXa <- matrix(c(1,18.37, 431.1, 13.98, 0.6721, 0.2747, 0.04118, 0.1806, 13.27, 2140 ), nrow = 1, ncol = 10)
meanXb <- matrix(c(1, 17.7, 396.7, 14.26, 0.5942, 0.3553, 0.03813, 0.1656, 7.547, 1828), nrow = 1, ncol = 10)

Ba <- reg_male$coefficients
Ba <- as.matrix(Ba)

Bb <- reg_female$coefficients
Bb <- as.matrix(Bb)

cov_diff <- meanXa - meanXb
cov_diff <- as.matrix(cov_diff)
point_estimate_covariance_effect <- cov_diff%*%Ba

#Point Estimate of the Coefficient Effect Xb(Ba-Bb)

coeff_diff <- Ba-Bb
dim(meanXb)
dim(coeff_diff)

point_estimate_coefficient_effect <- meanXb%*%coeff_diff

#Evaluating Mean Difference in y (lnrealwg)

mean_diff_in_y <- mean(subset_male$lnrealwg) - mean(subset_female$lnrealwg)

#Summarising:
mean_diff_in_y #(male-female)
point_estimate_coefficient_effect 
point_estimate_covariance_effect


#Q4: Finding Variances of the Estimated Coefficient and Covariate Effects

#Variance of Covariate Effect (meanXa-meanXb)'*V_Ba*(meanXa-meanXb)

var_covariate_effect <- cov_diff%*%vcov(reg_male)%*%t(cov_diff)

#Variance of the Coefficient Effect 

var_coefficient_effect <- meanXb%*%(vcov(reg_male)+vcov(reg_female))%*%t(meanXb)


#Q5: What if we add and subtract XaBb instead? We have mean(male) - mean(female) = Xa*Ba - Xb*Bb + XaBb - XaBb = (Xa-XB)'Bb + Xa'(Ba-Bb)

#Point Estimate of the Covariate Effect (Xa-Xb)'Bb

point_estimate_covariate_effect_2 <- cov_diff%*%Bb
point_estimate_covariate_effect_2

#Point Estimate of the Coefficient Effect Xa'(Ba-Bb)

point_estimate_coefficient_effect_2 <- meanXa%*%(coeff_diff)
point_estimate_coefficient_effect_2

#Index Number Problem!

#Q6: Adding the Black dummy to analysis while calculating the coefficient and covariate effects: Discrimination owing to race

blackmale <- subset%>%dplyr::filter(sex==1 & black==1)
blackfemale <- subset%>%dplyr::filter(sex==0 & black==1)
nonblackmale <- subset%>%dplyr::filter(sex==1 & black==0)
nonblackfemale <- subset%>%dplyr::filter(sex==0 & black==0)

blackmalereg <- lm(data = blackmale, lnrealwg ~ yrsexp + yrsExpSq + sch + unjob + annhrs)
blackfemalereg <- lm(data = blackfemale, lnrealwg ~ yrsexp + yrsExpSq + sch + unjob + annhrs)
nonblackfemalereg <- lm(data = nonblackfemale, lnrealwg ~ yrsexp + yrsExpSq + sch + unjob + annhrs)
nonblackmalereg <- lm(data = nonblackmale, lnrealwg ~ yrsexp + yrsExpSq + sch + unjob + annhrs)

beta_blackmale <- as.matrix(blackmalereg$coefficients)
beta_blackfemale <- as.matrix(blackfemalereg$coefficients)
beta_nonblackmale <- as.matrix(nonblackmalereg$coefficients)
beta_nonblackfemale <- as.matrix(nonblackfemalereg$coefficients)

meanblackmale <- matrix(c(1, mean(blackmale$yrsexp), mean(blackmale$yrsExpSq), mean(blackmale$sch), mean(blackmale$unjob), mean(blackmale$annhrs)), nrow = 1, ncol = 6)
meanblackfemale <- matrix(c(1, mean(blackfemale$yrsexp), mean(blackfemale$yrsExpSq), mean(blackfemale$sch), mean(blackfemale$unjob), mean(blackfemale$annhrs)), nrow = 1, ncol = 6)
meannonblackmale <- matrix(c(1, mean(nonblackmale$yrsexp), mean(nonblackmale$yrsExpSq), mean(nonblackmale$sch), mean(nonblackmale$unjob), mean(nonblackmale$annhrs)), nrow = 1, ncol = 6)
meannonblackfemale <- matrix(c(1, mean(blackmale$yrsexp), mean(blackmale$yrsExpSq), mean(blackmale$sch), mean(blackmale$unjob),  mean(blackmale$annhrs)), nrow = 1, ncol = 6)

#Among Men: Discrimination on the Basis of Race

cov_diff_men <- meanblackmale - meannonblackmale
coeff_diff_men <- beta_blackmale - beta_nonblackmale

#Covariate Effect

point_estimate_covariate_effect_men <- cov_diff_men%*%beta_nonblackmale
point_estimate_covariate_effect_men

#Coefficient Effect

point_estimate_coefficient_effect_men <- meanblackmale%*%coeff_diff_men
point_estimate_coefficient_effect_men

#Among Women: Discrimination on the Basis of Race

cov_diff_women <- meanblackfemale - meannonblackfemale
coeff_diff_women <- beta_blackfemale - beta_nonblackfemale

#Covariate Effect

point_estimate_covariate_effect_women <- cov_diff_women%*%beta_nonblackfemale
point_estimate_covariate_effect_women

#Coefficient Effect

point_estimate_coefficient_effect_women <- meanblackfemale%*%coeff_diff_women
point_estimate_coefficient_effect_women

#7 Using In-Built Commands for Discrimination on the Basis of Gender 

decomposition <- oaxaca(formula = lnrealwg ~ yrsexp + yrsExpSq + sch + white + black + hisp + unjob + labincbus + annhrs | sex, data = subset)
decomposition$threefold$overall

#Different to ensure insensitivity to the choice of the reference category
