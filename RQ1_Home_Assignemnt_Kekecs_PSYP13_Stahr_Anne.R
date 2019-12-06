# # # # # # # # # # # # # # # # # # 
# # # # RESEARCH QUESTION 1 # # # #
# # # # # # # # # # # # # # # # # # 

# SET WORKING DIRECTORY #
setwd("C:/Users/Malin/Desktop/Lund/FirstSemester/Statistics/Zoltan")

### GET DATA SAMPLE ###
home_sample_1 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2019/master/home_sample_1.csv") 

### ACTIVATE PACKAGES ###
library(psych) # for describe	
library(tidyverse) # for tidy code
library(olsrr) # for Cook's Distance
library(lm.beta) 
library(lmtest)
library(apaTables)
library(MBESS)

### CUSTOM FUNCTIONS ###
# The following custom functions will come in handy during the visualization of the error of the predictions of the regressions line. 
error_plotter <- function(mod, col = "black", x_var = NULL){	
  mod_vars = as.character(mod$call[2])	
  data = as.data.frame(eval(parse(text = as.character(mod$call[3]))))	
  y = substr(mod_vars, 1, as.numeric(gregexpr(pattern ='~',mod_vars))-2)	
  x = substr(mod_vars, as.numeric(gregexpr(pattern ='~',mod_vars))+2, nchar(mod_vars))	
  
  data$pred = predict(mod)	
  
  if(x == "1" & is.null(x_var)){x = "response_ID"	
  data$response_ID = 1:nrow(data)} else if(x == "1"){x = x_var}	
  
  plot(data[,y] ~ data[,x], ylab = y, xlab = x)	
  abline(mod)	
  
  for(i in 1:nrow(data)){	
    clip(min(data[,x]), max(data[,x]), min(data[i,c(y,"pred")]), max(data[i,c(y,"pred")]))	
    abline(v = data[i,x], lty = 2, col = col)	
  }	
  
}	

# VIEW DATA #
View(home_sample_1)
describe(home_sample_1)

# INVESTIGATE STRUCTURE OF THE DATASET
str(home_sample_1)

# GET A BASIC OVERVIEW
summary(home_sample_1)


####### CHECK THE DATASET FOR IRREGULARITIES ######
# before cleaning the data making a copy of the rawdatafile 
# Cleaning all irregularities 

### CHECK CODING ERRORS/EXPECTED VALUES ####
# ID = 160 participants
# pain = 0 - 10 
# sex = male / female
# age = ~ 0 - 100 
# STAI trait = 20 - 80
# pain_cat = 0 - 52
# cortisol_serum = numeric value
# cortisol_saliva = numeric value 
# mindfulness = 0 - 6
# weight = numeric value 
# IQ = ~ 0 - 200
# household_income = numeric value

# REMOVE WRONG VALUE (3.50) IN STAI_trait (Participant No. 18)
# create dataset without errors -> cleanhome_sample_1
cleanhome_sample_1 <- home_sample_1[!home_sample_1$STAI_trait < 20,]


###### MODELS ######
model1 <- lm(pain ~ age + sex, data = cleanhome_sample_1)
model2 <- lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + cortisol_saliva + mindfulness, data = cleanhome_sample_1)


### OUTLIERS (COOK'S DISTANCE) ###
# get olsrr package
# define model
# pain = dependent variable
# age, sex, STAI_trait, pain_cat, cortisol_serum, mindfulness
model2 <- lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + cortisol_saliva + mindfulness, data = cleanhome_sample_1)

# plot Cook's D
ols_plot_cooksd_bar(model1)
theme_apa()
# outliers Row 27, 73, 87, 113, 122, 128, 156 
ols_plot_cooksd_bar(model2)
# outliers 73, 81, 87, 113, 118, 122, 128, 156 
# ATTENTION: Participant Number and Row number of Participant differ! (Row 73 = Participant 74)

### REMOVE OUTLIERS ###
# look at participants to be removed
nrow(cleanhome_sample_1)
cleanhome_sample_1.1<-cleanhome_sample_1[27, ]
cleanhome_sample_1.1
cleanhome_sample_1.1<-cleanhome_sample_1[73, ]
cleanhome_sample_1.1
cleanhome_sample_1.1<-cleanhome_sample_1[81, ]
cleanhome_sample_1.1
cleanhome_sample_1.1<-cleanhome_sample_1[87, ]
cleanhome_sample_1.1
cleanhome_sample_1.1<-cleanhome_sample_1[113, ]
cleanhome_sample_1.1
cleanhome_sample_1.1<-cleanhome_sample_1[118, ]
cleanhome_sample_1.1
cleanhome_sample_1.1<-cleanhome_sample_1[122, ]
cleanhome_sample_1.1
cleanhome_sample_1.1<-cleanhome_sample_1[128, ]
cleanhome_sample_1.1
cleanhome_sample_1.1<-cleanhome_sample_1[156, ]
cleanhome_sample_1.1

# remove participants No. 28, 74, 82, 88, 114, 119, 123, 129, 157
# REMOVE PARTICIPANT No. 49 WITH WRONG HOUSEHOLD INCOME (-4562)
cleanhome_sample_final <- cleanhome_sample_1[-c(27, 48, 73, 81, 87, 113, 118, 122, 128, 156),]
summary(cleanhome_sample_final)
view(cleanhome_sample_final)

# Re-Creating Model 2, deriving Data from cleaned dataset: cleanhome_sample_final
model2 <- lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + cortisol_saliva + mindfulness, data = cleanhome_sample_final)

# Creating Histograms to check Distributions
hist(cleanhome_sample_final$pain, breaks = 20)
hist(cleanhome_sample_final$age, breaks = 20)
hist(cleanhome_sample_final$STAI_trait, breaks = 20)
hist(cleanhome_sample_final$pain_cat, breaks = 20)
hist(cleanhome_sample_final$cortisol_serum, breaks = 20)
hist(cleanhome_sample_final$cortisol_saliva, breaks = 20)
hist(cleanhome_sample_final$mindfulness, breaks = 20)
hist(cleanhome_sample_final$weight, breaks = 20)
hist(cleanhome_sample_final$IQ, breaks = 20)
hist(cleanhome_sample_final$household_income, breaks = 20)
# all Variables look normally distributed

# Visualising Patterns between Pain and Variables through Scatterplots
plot(age ~ pain, data = cleanhome_sample_final)
plot(STAI_trait ~ pain, data = cleanhome_sample_final)
plot(pain_cat ~ pain, data = cleanhome_sample_final)
plot(cortisol_serum ~ pain, data = cleanhome_sample_final)
plot(cortisol_saliva ~ pain, data = cleanhome_sample_final)
plot(mindfulness ~ pain, data = cleanhome_sample_final)
plot(weight ~ pain, data = cleanhome_sample_final)
plot(IQ ~ pain, data = cleanhome_sample_final)

# # # TESTING ASSUMPTIONS OF LINEAR REGRESSION # # #
# 1) Normality of the Residuals
# 2) Linearity (of the Relationship) (Tucky Test)
# 3) Homoscedasticity (Homogeneity of the Variance)
# 4) No excess multicollinearity (uncorrelated predictors)
# Navarro S. 446
# Anova Chapter S. 427, Chapter 14.2

###### MODEL 1 ######
# 1) Normality of the Residuals
model1 <- lm(pain ~ age + sex, data = cleanhome_sample_final)
summary(model1)
plot(model1)
residual <- residuals(model1)
hist(residual)
shapiro.test(residuals(model1))
# QQ Plot and Shapiro Wilk (p = 0.5788) show no violation of normality assumption

# 2) Linearity (graphplot)
residualPlots(model1) # p 0.9839
residualPlots(model1, terms = ~1)
# shows linearity 

# 3) Homoscedasticity - Navarro 14.7
# Different options: Levene Test, Brown Forsythe Test, Bartlett Test are not working
# Our null hypothesis is that all groups have the same variance; that is, the same overall deviations from the group means! 
# Breush Pagan Test (Heteroscedasticity)
# H0 = The residuals have constant variance
library(lmtest)
ols_test_breusch_pagan(model1, fitted.values =  TRUE, rhs = TRUE, multiple = TRUE)
# p > 0.05 Null hypothesis is true -> all groups have the same variance!

# 4) Multicolinearity (everything above 10 is critical, everything beneath no multicolinearity)
vif(model1)
# values ~ 1, so no reason to assume multicolinearity

###### MODEL 2 ######
# 1) Normality of the Residuals (Navarro 14.9)
# Assumption about the residuals Eik, namely that Eik ~ Normal (0,Sigma2)
# QQ Plots or Run a Shapiro Wilk Test

library(car)
model2 <- lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + cortisol_saliva + mindfulness, data = cleanhome_sample_final)
summary(model2)
plot(model2)
residual2 <- residuals(model2)
hist(residual2)
shapiro.test(residuals(model2))
# QQ-Plot and Shapiro-Wilk normality (p-value of 0.643) show that assumption of normality is not violated

# 2) Linearity (graphplot)
residualPlots(model2) # p = 0.7163
residualPlots(model2, terms= ~1)
# shows linearity 

# 3) Homoscedasticity - Navarro 14.7
ols_test_breusch_pagan(model2, fitted.values =  TRUE, rhs = TRUE, multiple = TRUE)
# p > 0.05 ( p = 0.94) Null hypothesis is true -> all groups have the same variance!

# 4) Multicolinearity (everything above 10 is critical, everything beneath no multicolinearity)
vif(model2)
# values are between 1.36 and 4.80, so reason to assume multicolinearity


######## HIERARCHICAL REGRESSION ########
### MODEL 1 ###
# Age and Sex as Predictors of Pain 
# Subset of the Predictors in Model 2 #
# Regression Equation: Y = b(0) + b(1)*x1 + b(2)*x2
# Y(Pain) = Intercept + Age*x1 + Sex*x2
summary(model1) # Adjusted R-squared: 0.1538
lm.beta(model1) # Coefficients
AIC (model1) # AIC model1 500,3185
confint(model1, level = 0.95) # confidence intervals
# Interpretation Model 1: Age and Sex influence 15.38 Percent of the variance of Pain (Adjusted R-squared)

### MODEL 2 ###
# Age, Sex STAI, pain, catastrophizing, mindfulness, cortisol measures as Predictors of Pain
# Regression Equation: Y = b(0) + b(1)*x1 + b(2)*x2 + b(3)*x3 + b(4)*x4 + b(5)*x5 + b(6)*b6 + b(7)*b7
# Y(Pain) = Intercept + Age*x1 + Sex*x2 + STAI_trait*x3 + pain_cat *x4 + cortisol_serum*x5 + cortisol_saliva*x6 + mindfulness*x7
summary(model2) # Adjusted R-squared: 0.4549
lm.beta(model2) # Coefficients
AIC (model2) # AIC model2 439,6073
# Performance of Model (AIC): model2 is better, since the AIC of model 2 is 439,6073 and the AIC of model 1 is 500,3185
confint(model2, level = 0.95) # confidence intervals
# Interpretation Model 2: Age, Sex, STAI_trait, pain_cat, cortisol_serum, cortisol_saliva and mindfulness influence 45.49 Percent of the variance of Pain (Adjusted R-squared)

### COMPARISON OF MODEL1 AND MODEL2 ### 
anova(model1, model2)
# Models differ from each other, since p < 0,001 --> highly significant!
# report values in a table with all regressions from all predictors

# create APA table 
apa.reg.table(model1)
apa.reg.table(model2)


# CREATING COEFFICIENT TABLE FOR MODEL 1 #
coef_table1 <- function(model2){
  require(lm.beta)
  tablecoeff <- summary(model2)
  p_values <- as.character(round(tablecoeff$coefficients[,4], 3)) 
  p_values[p_values != "0" & p_values != "1"] = substr(p_values[p_values != "0" & p_values != "1"], 2, nchar(p_values[p_values != "0" & p_values != "1"])) 
  p_values[p_values == "0"] = "<.001"
  
  table_summary <- cbind(as.data.frame(round(cbind(coef(model1), confint(model1), c(0, lm.beta(model1)$standardized.coefficients[c(2:length(model1$coefficients))])), 2)), p_values) 
  names(table_summary) <- c("b", "95%CI lb", "95%CI ub", "Std.Beta", "p-value") 
  table_summary["(Intercept)","Std.Beta"] = "0" 
  return(table_summary)
}
coef_table1(model1)


# CREATING COEFFICIENT TABLE FOR MODEL 2 #
coef_table2 = function(model2){
  require(lm.beta)
  table_coeff2 <- summary(model2)
  p_values <- as.character(round(table_coeff2$coefficients[,4], 3)) 
  p_values[p_values != "0" & p_values != "1"] = substr(p_values[p_values != "0" & p_values != "1"], 2, nchar(p_values[p_values != "0" & p_values != "1"])) 
  mod_sum_p_values[mod_sum_p_values == "0"] = "<.001"
  
  table_summary2 <- cbind(as.data.frame(round(cbind(coef(model2), confint(model2), c(0, lm.beta(model2)$standardized.coefficients[c(2:length(model2$coefficients))])), 2)), p_values) 
  names(table_summary2) = c("b", "95%CI lb", "95%CI ub", "Std.Beta", "p-value") 
  table_summary2["(Intercept)","Std.Beta"] = "0" 
  return(table_summary2)
}

coef_table1(model2)
