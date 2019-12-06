### ZOLTAN RESEARCH QUESTION 2.2 ### 

setwd("C:/Users/Malin/Desktop/Lund/FirstSemester/Statistics/ASSIGNMENT")

### GET DATA SAMPLE ###
data_sample_2 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2019/master/home_sample_2.csv")

### ACTIVATE PACKAGES ###
library(psych) # for describe	
library(tidyverse) # for tidy code
library(olsrr) # for Cook's Distance
library(lm.beta) 
library(lmtest)
library(car)

view(data_sample_2)
describe(data_sample_2)
summary(data_sample_2)

# REGRESSION EQUATIONS OF THE 2 MODELS #
# 1)  Regression equation: Y = b0 + b(1)x1 + b(2)*x2 + b(3)*x3 + b(4)*x4 + b(5)*x5 + b(6)*x6
MODEL_BACKWARD_FINAL <- lm(formula = pain ~ age + sex + pain_cat + cortisol_serum + mindfulness + weight, data = SAMPLE_REGRESSION_BACKWARD_FINAL)

# 2) Regression equation: Y = b0 + b(1)x1 + b(2)*x2 + b(3)*x3 + b(4)*x4 + b(5)*x5 + b(6)*x6 + b(7)*x7
THEORY_BASED_MODEL <- lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + cortisol_saliva + mindfulness, data = SAMPLE_REGRESSION_BACKWARD_FINAL)

### PREDICTIONS ###
Prediction_Backward_Model <- predict(MODEL_BACKWARD_FINAL, home_sample_2)
print(Prediction_Backward_Model)

Prediction_Theory_Model <- predict(THEORY_BASED_MODEL, home_sample_2)
print(Prediction_Theory_Model)

# COMPARISON OF PREDICTED VALUES WITH ACTUAL VALUES BY CALCULATING THE SUM OF SQUARES (RESIDUALS) #
# RSS of Backward Model
ResidualsBackwards <- sum((home_sample_2$pain - Prediction_Backward_Model)^2)
sum((home_sample_2$pain - Prediction_Backward_Model)^2) #239.1203

# RSS of Theory Based Model
ResidualsTheory <- sum((home_sample_2$pain - Prediction_Theory_Model)^2)
sum((home_sample_2$pain - Prediction_Theory_Model)^2) #234.0067

# the unbiased estimate of error variance is slightly less in the Backward Model (234.1123) compared to the Theory Based Model (235.7203)
# though only on Data Sample 2
# also, it's probably a non significant difference
# Which model would you choose to predict pain in an actual clinical context? The one with less variables -> more economic!

