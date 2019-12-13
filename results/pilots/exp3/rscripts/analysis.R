# analysis file for experiment investigating the relationship between at-issueness and prior
# in predicting projection for the contents of the complements of 20 predicates

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# load required packages
require(tidyverse)
library(ggrepel)
library(lme4)
library(lmerTest)

# load helper functions
source('../../helpers.R')

# load data
d = read.csv("../data/data_preprocessed.csv")
nrow(d) #702 / 9 Turkers = 78 trials

head(d)

table(d$question_type,d$block,d$workerid)

# rename "prior" column into "priorType" with levels "low_prior" and "high_prior" so that we don't
# run into trouble when spread the responses across three columns "projection", "ai" and "prior"
# DO THIS!!

# block0 is "prior", block1 is either projection or at-issueness, block2 is either projection or at-issueness
# spread responses over separate columns for prior, projectivity and at-issueness
t = d %>%
  mutate(block_ai = as.factor(ifelse(question_type == "ai", 
                                     ifelse(block == "block1", "block1", "block2"), 
                                     ifelse(block == "block1", "block2", "block1")))) %>%
  select(workerid,content,short_trigger,question_type,response,block_ai,prior) %>%
  spread(question_type,response) %>%
  unite(item,short_trigger,content,remove=F)
nrow(t) #234 / 9 = 26 stimuli per Turker

# exclude main clause controls
t_nomc = droplevels(subset(t, short_trigger != "MC"))
nrow(t_nomc) #180 / 9 = 20 target stimuli per Turker

# center the block, at-issueness and prior variables
t_nomc = cbind(t_nomc,myCenter(t_nomc[,c("block_ai","ai","prior")]))
summary(t_nomc)
# two main analyses of interest:

# 1. predict at-issueness from prior, while controlling for the effect of block
# random effects by participant and item. get p-values via lmerTest (Satterthwaite's approximation)
m.ai.prior = lmer(cai ~ cprior * cblock_ai + (1+cprior*cblock_ai|workerid) + (1+cprior*cblock_ai|item), data=t_nomc)
summary(m.ai.prior)

# 2.predict projectivity from prior and at-issueness and their interaction
# while controlling for the effect of block on proj and ai ratings
# random effects by participant and item (lexical content+target expression). get p-values via lmerTest (Satterthwaite's approximation)

# the model reported in the paper
m.proj = lmer(projective ~ cai * cprior * cblock_ai + (1+cai * cprior * cblock_ai|workerid) + (1+cai * cprior * cblock_ai|item), data=t_nomc)
summary(m.proj)

# if too much of the variance in at-issueness is explained by the prior so that collinearity is too high: regress prior onto ai and enter residuals as new predictor

# if none of the block effects reach significance, re-run analysis without block predictor for ease of interpretability

# if random effects structure prevents model from converging, remove slopes step-wise, starting with block interactions and main effect