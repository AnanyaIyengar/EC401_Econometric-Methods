#########################################################################
###ASSIGNMENT 1 on Koop-Tobias (2004), Journal of Applied Econometrics###
#########################################################################

#Downloading Required Packages
library(readxl)
library(dplyr)
library(ggplot2)
library(lmtest)
library(rmarkdown)
library(gridExtra)

#Importing Data
time_invar <- read_excel("time_invar.xlsx")
time_var <- read_excel("time_var.xlsx")

#Grouping duplicate observations in the "time_var" dataset by taking means across time for each cross-sectional unit

time_var_1<-time_var%>%dplyr::group_by(person_id)%>%summarise(education = mean(education), logwage = mean(logwage), potential_exp = mean(potential_exp))
nrow(time_var_1)
nrow(time_invar)
nrow(time_var)
max(time_var$person_id) #Returns 999

#The "time_var" data set only has data for unique person ids ranging from 1 to 999, therefore we truncate the "time_invar" data set to only include the first 999 observations

time_invar_1<-time_invar[1:999, ] #Since row j corresponds to person_id j
nrow(time_invar_1)

#Given set X1: constant, education, experience, ability (OWN CHARACTERISTICS)

constant <- rep(1, 999)
x1 <- cbind(constant, time_var_1$education, time_var_1$potential_exp, time_invar_1$ability)
colnames(x1) <- c("constant", "education", "potential_exp", "ability")

#Given set X2: mother's education, father's education, number of siblings (HH CHARACTERISTICS)

x2 <- cbind(time_invar_1$mother_edu, time_invar_1$father_edu, time_invar_1$siblings)
colnames(x2) <- c("mother_edu", "father_edu", "siblings")

#Given y: log wage

y <- time_var_1$logwage

#Y and X1 Data

data1<- data.frame(y, x1)


#Q1 and Q2: Scatter plot of Y and X1 variables, and calculating correlation

plot1 <- ggplot(data1, aes(education, y)) + geom_jitter(colour = "purple4") + theme_gray() + xlab("Own Education") + ylab("Log Wage") + ggtitle("Plotting Log Wage against Education")
plot1
cor(time_var_1$logwage, time_var_1$education) #-0.17: wages and education are negatively correlated (mainly due to some outliers)

plot2<- ggplot(data1, aes(ability, y)) + geom_jitter(colour = "purple4") + theme_gray() + xlab("Own Ability") + ylab("Log Wage") + ggtitle("Plotting Log Wage against Ability")
plot2
cor(time_var_1$logwage, time_invar_1$ability) #0.185: slight positive correlation

plot3<- ggplot(data1, aes(potential_exp, y)) + geom_jitter(colour = "purple4") + theme_gray() + xlab("Potential Experience") + ylab("Log Wage") + ggtitle("Plotting Log Wage against Potential Experience")
plot3
cor(time_var_1$logwage, time_var_1$potential_exp) #0.021: very slight positive to no correlation


#Multicollinearity Check
cor(time_invar_1$ability, time_var_1$education) #0.49 < 0.80
cor(time_var_1$education, time_var_1$potential_exp) #-0.35 > -0.80
cor(time_var_1$education, time_invar_1$mother_edu) #0.32 < 0.80
cor(time_var_1$education, time_invar_1$father_edu) #0.343 < 0.80 


#Q3: Regression Specification: logwage ~ edu + potential_exp + ability

#We expect, a priori, for the coefficient of education to have a positive sign! 

#Q4: Estimating OLS without the built-in functions. We know that beta-hat = (X'X)^(-1)(X'y)

x1_transpose <- t(x1)
x_prime_x <- x1_transpose%*%x1
x_prime_x_inverse <- solve(x_prime_x)
x_prime_y <- x1_transpose%*%y
beta_hat <- x_prime_x_inverse%*%x_prime_y
beta_hat

#No, the coefficient of education does not match our a priori assumptions!

#Q5: Using the built-in OLS function "lm" (takes intercept by default)

regression_1 <- lm(data = data1, y ~ education + potential_exp + ability)
regression_1$coefficients #They match our matrix calculation!

#Q6: Regress variables in X2 on all variables in X1 and find residuals

data_2 <- cbind(x1, x2, y)
data_2 <- as.data.frame(data_2)
nrow(data_2)

mother_edu_regression <- lm(data = data_2, mother_edu ~ education + potential_exp + ability)
father_edu_regression <- lm(data = data_2, father_edu ~ education + potential_exp + ability)
siblings_regression <- lm(data = data_2, siblings ~ education + potential_exp + ability)


mother_residuals <- mother_edu_regression$residuals
father_residuals <- father_edu_regression$residuals
siblings_residuals <- siblings_regression$residuals

x2_star <- cbind(mother_residuals, father_residuals, siblings_residuals)

mean(mother_residuals)
mean(father_residuals)
mean(siblings_residuals)  #residual means are almost zero because of tiny numerical errors 

#Q7: Regression coefficients of Y on X1

regression_1$coefficients

#Q8: Regression coefficients of Y on X1 and X2

regression_2 <- lm(data = data_2, y ~ education + potential_exp + ability + mother_edu + father_edu + siblings)
regression_2$coefficients

#Q9: Regressing y on X1 and X2*, and comparing results

data_3 <- cbind(x1, x2_star, y)
data_3 <- as.data.frame(data_3)

regression_3 <- lm(data = data_3, y ~ education + potential_exp + ability + mother_residuals + father_residuals + siblings_residuals)
regression_3$coefficients

