### ZOLTAN RESEARCH QUESTION 2.2 ### 
# After the backward regression the new model we use as a new initial model now called Backward Model # 

setwd("C:/Users/Malin/Desktop/Lund/FirstSemester/Statistics/ASSIGNMENT")

### GET DATA SAMPLE ###
data_sample_1 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2019/master/home_sample_1.csv")

### ACTIVATE PACKAGES ###
library(psych) # for describe	
library(tidyverse) # for tidy code
library(olsrr) # for Cook's Distance
library(lm.beta) 
library(lmtest)
library(car)

MODEL_BACKWARD_FINAL <- lm(formula = pain ~ age + sex + pain_cat + cortisol_serum + mindfulness + weight, data = SAMPLE_REGRESSION_BACKWARD_FINAL)
# Regression equation: Y = b0 + b(1)x1 + b(2)*x2 + b(3)*x3 + b(4)*x4 + b(5)*x5 + b(6)*x6

summary(MODEL_BACKWARD_FINAL) # R squared 0.4548
AIC(MODEL_BACKWARD_FINAL) # 425.5966
confint(MODEL_BACKWARD_FINAL)
lm.beta(MODEL_BACKWARD_FINAL) # standardized coefficients !

### ASSUMPTION TESTING BACKWARD MODEL ###
# 1) Normality of the Residuals
summary(MODEL_BACKWARD_FINAL) # unstandardized coefficients ! 
plot(MODEL_BACKWARD_FINAL)
residuals_backward_final <- residuals(MODEL_BACKWARD_FINAL)
hist(residuals_backward_final)
describe(residuals_backward_final) # -0.08
shapiro.test(residuals_backward_final)
# QQ Plot and Shapiro Wilk (p = 0.8176) show no violation of normality assumption

# 2) Linearity (of the Relationship) 
residualPlots(MODEL_BACKWARD_FINAL) # Tukey: hää was soll hier der p Wert sein?
# Graphs show Linearity

# 3) Homoscedasticity (Homogeinity of Variance)
ols_test_breusch_pagan(MODEL_BACKWARD_FINAL, fitted.values = TRUE, rhs = TRUE, multiple = TRUE)
# p (0.977) > 0.05 Null hypothesis is true -> all groups have the same variance!

# 4) Multicolinearity (everything above 10 is critical, everything beneath no multicolinearity)
vif(MODEL_BACKWARD_FINAL)
# values are between 1.05 and 1.67, so no reason to assume a violation of multicolinearity

###### COMPARING MODELS ######
# COMPARING THEORY_BASED_MODEL AND MODEL_BACKWARD_FINAL
THEORY_BASED_MODEL <- lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + cortisol_saliva + mindfulness, data = SAMPLE_REGRESSION_BACKWARD_FINAL)
AIC(MODEL_BACKWARD_FINAL) # 425.5966
AIC(THEORY_BASED_MODEL) # 424.6738
# Theory based model slightly lower score on AIC

# ANOVA #
anova(MODEL_BACKWARD_FINAL, THEORY_BASED_MODEL) # no significant difference between the two models








