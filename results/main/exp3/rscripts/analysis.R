# analysis file for investigating whether participants' prior probability ratings of a content
# predict their projectivity ratings for that content (Lehnhardt thesis) and for JD/JT research on 
# whether participants' prior probability and at-issueness ratings predict projectivity, and whether the
# two factors are independent

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# load required packages
library(tidyverse)
library(lme4)
library(lmerTest)

# load helper functions
source('../../helpers.R')

# load data
d = read.csv("../data/data_preprocessed.csv")
nrow(d) # [702] / 78 trials = [9] Turkers

# prepare for spreading: rename the 'prior' column into 'prior_type'
colnames(d)[colnames(d)=="prior"] = "prior_type"

# doing this step before spreading solves the problem with prior_type in MC trials
# exclude main clause controls
d_nomc = droplevels(subset(d, short_trigger != "MC"))
nrow(d_nomc) # [540] / [9] = 60 target stimuli per Turker

# spread responses over separate columns for projectivity, at-issueness and prior probability
t_nomc = d_nomc %>%
  mutate(block_ai = ifelse(question_type=="ai"&block=="block1", "block1",
                           ifelse(question_type=="ai"&block=="block2", "block2",
                                  ifelse(question_type=="projective"&block=="block1", "block2",
                                         ifelse(question_type=="projective"&block=="block2", "block1", NA))))) %>%
  na.locf(fromLast = TRUE) %>%   # replaces NA with the nearest non-NA; fromLast causes observations to be carried backward 
  select(workerid,content,short_trigger,question_type,response,prior_type) %>%     # 'event' (CC) is missing... (?)
  spread(question_type,response) %>%
  unite(item,short_trigger,content,remove=F)
nrow(t_nomc) # [180] / [9] Turkers = 20 rows per Turker

# center prior probability, projectivity, and at-issueness
t_nomc = cbind(t_nomc,myCenter(t_nomc[,c("prior","projective","ai")]))
summary(t_nomc)

# Lehnhardt analysis ----
# to address the question of whether a participant's prior probability rating influences their
# projection rating
# this model accounts for the fact that we've seen by-predicate variability, both in projection and in effect of prior on projection
model = lmer(projective ~ cprior  + (1+cprior|workerid) + (1|content) + (1+cprior|short_trigger), data = t_nomc, REML=F)
summary(model)

# compare to model with block interaction, to identify whether block order mattered
model.2 = lmer(projective ~ cprior * cblock_ai + (1+cprior|workerid) + (1|content) + (1+cprior|short_trigger), data = t_nomc, REML=F)
summary(model.2)

anova(model,model.2)

# auxiliary analysis to investigate which of the predicates the prior has a bigger or smaller effect on
model = lmer(projective ~ cprior  * short_trigger - cprior + (1+cprior+short_trigger|workerid) + (1+short_trigger|content), data = t_nomc, REML=F)
summary(model)
# this model will show us the effect of the prior on each predicate, if there's a significant p-value then the prior has an effect on projectivity at 
# that level of the short_trigger

# JD/JT analyses

# the model we want to fit to test whether prior and at-issueness predict projection and whether the two
# factors are independent from one another
model = lmer(projective ~ cprior  *  cai + (1+cprior+cai|workerid) + (1+cprior+cai|content) + (1+cprior+cai|short_trigger)), data = t_nomc, REML=F)
summary(model)

# if this model does not converge, remove slopes, starting with those that, per the random effects part of the output of the non-converging
# model have the smallest variance

# we want to investigate whether there is an interaction between prior and at-issueness:
model.1b = lmer(projective ~ cprior  +  cai + (1+cprior+cai|workerid) + (1+cprior+cai|content) + (1+cprior+cai|short_trigger)), data = t_nomc, REML=F)
summary(model.1b)

anova(model,model.1b)

# we also want to investigate whether block order mattered:
model.2 = lmer(projective ~ cprior  *  cai * cblock_ai + (1+cprior+cai|workerid) + (1+cprior+cai|content) + (1+cprior+cai|short_trigger)), data = t_nomc, REML=F)
summary(model.2)

anova(model,model.2)


# if too much of the variance in at-issueness is explained by the prior 
# so that collinearity is too high: 
# regress prior onto ai and enter residuals as new predictor

# if none of the block effects reach significance, re-run analysis without block predictor 
# for ease of interpretability

# if random effects structure prevents model from converging, 
# remove slopes step-wise, starting with block interactions and main effect




