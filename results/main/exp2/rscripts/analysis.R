# analysis file for experiment investigating the relationship between at-issueness and prior
# in predicting projection for the contents of the complements of 20 predicates

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# load required packages
require(tidyverse)
library(lme4)
library(lmerTest)

# load helper functions
source('../../helpers.R')

# load data
d = read.csv("../data/data_preprocessed.csv")
nrow(d) #28340 / 52 Trials =545 Turkers 

# spread responses over separate columns for projectivity and at-issueness
t = d %>%
  mutate(block_ai = as.factor(ifelse(question_type == "ai", ifelse(block == "block1", "block1", "block2"), ifelse(block == "block1", "block2", "block1")))) %>%
  select(workerid,content,short_trigger,question_type,response,block_ai,prior) %>%
  spread(question_type,response) %>%
  unite(item,short_trigger,content,remove=F)
nrow(t) #14170 / 26 stimuli per Turker = 545 Turkers

# exclude main clause controls
t_nomc = droplevels(subset(t, short_trigger != "MC"))
nrow(t_nomc) #10900 / 545 = 20 target stimuli per Turker

# center the block, at-issueness and prior variables
t_nomc = cbind(t_nomc,myCenter(t_nomc[,c("block_ai","ai","prior")]))
summary(t_nomc)

length(unique(t_nomc$item)) #400

# two main analyses of interest:

# 1. predict at-issueness from prior, while controlling for the effect of block
# random effects by participant and item. get p-values via lmerTest (Satterthwaite's approximation)
m.ai.prior_S = lmer(cai ~ cprior + (1+cprior|workerid) + (1+cprior|item), data=t_nomc)
summary(m.ai.prior_S) # prior not significant

m.ai.prior = lmer(cai ~ cprior * cblock_ai + (1+cprior*cblock_ai|workerid) + (1+cprior*cblock_ai|item), data=t_nomc)
summary(m.ai.prior) # does not converge

# 2.predict projectivity from prior and at-issueness and their interaction
# while controlling for the effect of block on proj and ai ratings
# random effects by participant and item (lexical content+target expression). 
# get p-values via lmerTest (Satterthwaite's approximation)

# the model reported in the paper
m.proj = lmer(projective ~ cai * cprior * cblock_ai + (1+cai * cprior * cblock_ai|workerid) + (1+cai * cprior * cblock_ai|item), data=t_nomc)


# models that do not converge
m.proj = lmer(projective ~ cai * cprior * cblock_ai + (1+cai * cprior + cblock_ai|workerid) + (1+cai * cprior + cblock_ai|item), data=t_nomc)
m.proj = lmer(projective ~ cai * cprior + (1+cai + cprior|workerid) + (1+cai + cprior|item), data=t_nomc)


# simplistic model
m.proj = lmer(projective ~ cai * cprior + (1+cai|workerid) + (1+cai|item), data=t_nomc)
summary(m.proj)

# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: projective ~ cai * cprior + (1 + cai | workerid) + (1 + cai |      item)
# Data: t_nomc
# 
# REML criterion at convergence: 4085.6
# 
# Scaled residuals: 
# Min      1Q  Median      3Q     Max 
# -3.7124 -0.6344 -0.0112  0.6334  3.3543 
# 
# Random effects:
# Groups   Name        Variance Std.Dev. Corr
# workerid (Intercept) 0.01711  0.1308       
# cai         0.02706  0.1645   0.02
# item     (Intercept) 0.03174  0.1782       
# cai         0.02003  0.1415   0.93
# Residual             0.06834  0.2614       
# Number of obs: 10900, groups:  workerid, 545; item, 400
# 
# Fixed effects:
# Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)  4.444e-01  1.090e-02  6.353e+02   40.77   <2e-16 ***
# cai          2.010e-01  1.366e-02  5.367e+02   14.71   <2e-16 ***
# cprior      -8.891e-02  5.176e-03  9.976e+03  -17.18   <2e-16 ***
# cai:cprior  -1.890e-02  1.390e-02  9.884e+03   -1.36    0.174    
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
# (Intr) cai    cprior
# cai         0.408              
# cprior      0.000  0.000       
# cai:cprior -0.001 -0.002  0.002

# simplistic model without interaction
m.proj.b = lmer(projective ~ cai + cprior + (1+cai|workerid) + (1+cai|item), data=t_nomc)
summary(m.proj.b)

anova(m.proj,m.proj.b) # p=0.174

# if too much of the variance in at-issueness is explained by the prior 
# so that collinearity is too high: 
# regress prior onto ai and enter residuals as new predictor

# if none of the block effects reach significance, re-run analysis without block predictor 
# for ease of interpretability

# if random effects structure prevents model from converging, 
# remove slopes step-wise, starting with block interactions and main effect