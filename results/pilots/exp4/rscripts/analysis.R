# experiment investigating prior and presupposition
# contents of complements of 20 predicates
# analysis.R

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
nrow(d) #416

# prepare for spreading: rename the 'prior' column into 'prior_type'
colnames(d)[colnames(d)=="prior"] = "prior_type"

# doing this step before spreading solves the problem with prior_type in MC trials
# exclude main clause controls
d_nomc = droplevels(subset(d, short_trigger != "MC"))
nrow(d_nomc) # [540] / [9] = 60 target stimuli per Turker

# spread responses over separate columns for projectivity, at-issueness and prior probability
t_nomc = d_nomc %>%
  mutate(block_proj = ifelse(question_type=="projective"&block=="block1", "block1", 
                             ifelse(question_type=="projective"&block=="block2","block2",
                                    ifelse(question_type=="prior"&block=="block1","block2","block1")))) %>%
  select(workerid,content,short_trigger,question_type,response,prior_type,block_proj) %>%     # 'event' (CC) is missing... (?)
  spread(question_type,response) %>%
  unite(item,short_trigger,content,remove=F)
nrow(t_nomc) #160 = 8 turkers x 20 rows

# center prior probability, projectivity, and at-issueness
t_nomc = cbind(t_nomc,myCenter(t_nomc[,c("prior","projective")]))
summary(t_nomc)

# does prior predict projection?
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

