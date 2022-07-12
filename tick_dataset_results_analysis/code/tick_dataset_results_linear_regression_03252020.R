
# Feb 16 2020
# Running linear regression with stability time as response

#-------------------------------------------
# load libraries

library(ggplot2)
library(dplyr)
library(wesanderson)
library(RColorBrewer)
library(MuMIn)
library(car)
library(nlme)  
library(ncf)
library(lme4)
library(Matrix)
library(coefplot)
library(MASS)

#-------------------------------------------

######################################

# Linear Regression 2/16/2020

#####################################

tick_dataset_results <- readxl::read_xlsx("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/data/tick_dataset_results_2_11_2020.xlsx", sheet = 1)

#Predictors:
#life stage
#geographic scope
#start year
#end year
#data range
#response units

#Possible random variables:
#study
#state

#Response variables
#Stability time
#Absolute range
#Relative range
#Proportion significant

#testing correlation of numerical variables
#start year vs end year
cor(tick_dataset_results$start_year, tick_dataset_results$end_year)
#r = -0.1419066
#little relationship

#start year vs data range
cor(tick_dataset_results$start_year, tick_dataset_results$data_range)
#r = -0.9637017
#strong relationship, so should not be used in the same model

#end year vs data range
cor(tick_dataset_results$end_year, tick_dataset_results$data_range)
#r = 0.4010352
#some correlation


fit1=lmer(stability_time ~ data_range + life_stage + sampling_technique + geographic_scope + response_units + (1|state), data = tick_dataset_results, REML=FALSE)
anova(fit1)
#Analysis of Variance Table
#Df  Sum Sq Mean Sq  F value
#data_range          1 2901.93 2901.93 868.4617
#life_stage          5   35.39    7.08   2.1181
#sampling_technique  2    0.41    0.21   0.0617
#geographic_scope    3    2.08    0.69   0.2072
#response_units      2  303.95  151.97  45.4810


summary(fit1)
# Linear mixed model fit by maximum likelihood  ['lmerMod']
# Formula: stability_time ~ data_range + life_stage + sampling_technique +  
#   geographic_scope + response_units + (1 | state)
# Data: tick_dataset_results
# 
# AIC      BIC   logLik deviance df.resid 
# 1200.8   1259.5   -584.4   1168.8      273 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -2.8960 -0.5366  0.0104  0.4908  4.4843 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# state    (Intercept) 0.000    0.000   
# Residual             3.341    1.828   
# Number of obs: 289, groups:  state, 5
# 
# Fixed effects:
#   Estimate Std. Error t value
# (Intercept)                         -0.948617   2.814101  -0.337
# data_range                           0.490808   0.060219   8.150
# life_stagelarvae                    -0.248536   0.765657  -0.325
# life_stagenot specified              1.999962   2.362721   0.846
# life_stagenymph                     -0.298792   0.326666  -0.915
# life_stagenymphs                     2.060671   1.454953   1.416
# life_stageunspecified                1.741347   0.825576   2.109
# sampling_techniquedragging           2.888281   2.693024   1.073
# sampling_techniquefound on a person  0.490808   2.585837   0.190
# geographic_scopeGrid                -0.365381   0.876579  -0.417
# geographic_scopeState forest        -0.757537   1.309246  -0.579
# geographic_scopeTown                -0.285854   0.694767  -0.411
# response_units# per person           2.816490   0.550289   5.118
# response_units%                      0.007436   0.464676   0.016
# 
# Correlation matrix not shown by default, as p = 14 > 12.
# Use print(x, correlation=TRUE)  or
# vcov(x)        if you need it
# 
# fit warnings:
#   fixed-effect model matrix is rank deficient so dropping 1 column / coefficient
# convergence code: 0
# boundary (singular) fit: see ?isSingular
