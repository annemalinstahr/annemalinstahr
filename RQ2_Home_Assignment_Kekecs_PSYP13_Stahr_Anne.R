# # # # # # # # # # # # # # # # # # # # # #
# # # # ZOLTAN: RESEARCH QUESTION 2 # # # #
# # # # # # # # # # # # # # # # # # # # # # 

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
library(car)

### CUSTOM FUNCTIONS	###
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

# VIEW DATA
View(home_sample_1)

# INVESTIGATE STRUCTURE OF THE DATASET
str(home_sample_1)

# GET A BASIC OVERVIEW
summary(home_sample_1)
describe(home_sample_1)

# REMOVE WRONG VALUE (3.50) IN STAI_trait
# consequently, participant 18 is removed from the sample
cleanhome_sample_1 <- home_sample_1[!home_sample_1$STAI_trait < 20,]

# only person with household_income -4562, Proband 49 (but in sample row 48)
sample_regression_backward <- cleanhome_sample_1[-c(48),]
view(sample_regression_backward)
summary(sample_regression_backward)

### CHECK OUTLIERS FROM MODEL2 INCLUDING THE OUTLIERS ###
MODEL2 <- lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + cortisol_saliva + mindfulness + weight + IQ + household_income, data = sample_regression_backward)
# Cook's Distance #
ols_plot_cooksd_bar(MODEL2)
# Participants 

# Remove same participants from prior Research Question 1: No. 28, 74, 82, 88, 114, 119, 123, 129, 157
# attention: row and participant differ: row 53 = participant 55 etc.
sample_regression_backward_excluded1 <- sample_regression_backward[-c(26, 72, 80, 86, 112, 117, 121, 127, 155),]

# Also further Participants would be outliers, due to new Variables No. 18, 55, 77, 116, 127 but will not be removed due to several reasons, mainly because the ANOVA should be run on an equal dataset. also, because outliers in the variables income, IQ are not really outliers
SAMPLE_REGRESSION_BACKWARD_FINAL <- sample_regression_backward_excluded1
view(SAMPLE_REGRESSION_BACKWARD_FINAL)
summary(SAMPLE_REGRESSION_BACKWARD_FINAL)

### MODEL 2 without Outliers from first Research Question
MODEL2_cleaned <- lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + cortisol_saliva + mindfulness + weight + IQ + household_income, data = SAMPLE_REGRESSION_BACKWARD_FINAL)

### BACKWARD MODEL WITHOUT SALIVARY CORTISOL ###
BACKWARD_MODEL <- lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + mindfulness + weight + IQ + household_income, data = SAMPLE_REGRESSION_BACKWARD_FINAL)


###### ASSUMPTION TESTING ###### 

# 1) Normality of the Residuals
summary(BACKWARD_MODEL) # Rsquared 0.4493
plot(BACKWARD_MODEL)
residuals_backward <- residuals(BACKWARD_MODEL)
hist(residuals_backward)
shapiro.test(residuals_backward)
# QQ Plot and Shapiro Wilk (p = 0.7328) show no violation of normality assumption

# 2) Linearity (of the Relationship)
residualPlots(BACKWARD_MODEL) # Tuckey: 

# 3) Homoscedasticity (Homogeneity of the Variance)
ols_test_breusch_pagan(BACKWARD_MODEL, fitted.values = TRUE, rhs = TRUE, multiple = TRUE)
# p > 0.05 Null hypothesis is true -> all groups have the same variance!

# 4) Multicolinearity (everything above 10 is critical, everything beneath no multicolinearity)
vif(BACKWARD_MODEL)
# values are between 1.02 and 1.62, so reason to assume multicolinearity

### RUN BACKWARD REGRESSION ### Navarro S. 491
step(BACKWARD_MODEL, direction = "backward")

# the following predictors predict the regression of the model the best: (AIC 14.94)
# age + sex + pain_cat + cortisol_serum + mindfulness + weight
# Intercept: 2.92891, age -0.03169, sex 0.46923, pain_cat 0.08941, cortisol_serum 0.37268, mindfulness -0.25169, weight -0.01213
MODEL_BACKWARD_FINAL <- lm(formula = pain ~ age + sex + pain_cat + cortisol_serum + mindfulness + weight, data = SAMPLE_REGRESSION_BACKWARD_FINAL)

load("C:/Users/Malin/Desktop/Lund/FirstSemester/Statistics/ASSIGNMENT/Environment2.RData")


# CREATING COEFFICIENT TABLE FOR BACKWARD MODEL #
backward_coefficients <- function(MODEL_BACKWARD_FINAL){
  require(lm.beta)
  mod_sum <- summary(BACKWAR_MODEL_FINAL)
  p_values <- as.character(round(mod_sum$coefficients[,4], 3)) 
  p_values[p_values != "0" & p_values != "1"] = substr(p_values[p_values != "0" & p_values != "1"], 2, nchar(p_values[p_values != "0" & p_values != "1"])) 
  p_values[p_values == "0"] = "<.001"
  
  table_backwards <- cbind(as.data.frame(round(cbind(coef(MODEL_BACKWARD_FINAL), confint(MODEL_BACKWARD_FINAL), c(0, lm.beta(MODEL_BACKWARD_FINAL)$standardized.coefficients[c(2:length(MODEL_BACKWARD_FINAL$coefficients))])), 2)), p_values) 
  names(table_backwards) <- c("b", "95%CI lb", "95%CI ub", "Std.Beta", "p-value") 
  table_backwards["(Intercept)","Std.Beta"] = "0" 
  return(table_backwards)
}
coef_table1(MODEL_BACKWARD_FINAL)


