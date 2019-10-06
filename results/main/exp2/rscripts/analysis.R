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
head(d)

# exclude the main clause controls
d_nomc = droplevels(subset(d, short_trigger != "MC"))
nrow(d_nomc) #21800 / 545 = 40 target stimuli per Turker

## merge prior means from norming study into cd
head(d_nomc)
table(d_nomc$prior_fact)
# here "content" is the name of the CC and "prior_fact" is the fact

# load prior means from norming study (moved to this repo, for accessibility)
pmeans = read.csv("../data/prior_means.csv")
pmeans$fact = gsub(".","",as.character(pmeans$fact),fixed=T)
pmeans
head(pmeans)
# here "event" is the CC and "fact" is the fact, PriorMean is the info we need
pmeans$prior_fact <- pmeans$fact
table(pmeans$prior_fact)

# merge prior means into cd
d = left_join(d_nomc,pmeans,by=c("prior_fact"))
nrow(d) #21800

# spread responses over separate columns for projectivity and at-issueness
t = d %>%
  mutate(block_ai = as.factor(ifelse(question_type == "ai", ifelse(block == "block1", "block1", "block2"), ifelse(block == "block1", "block2", "block1")))) %>%
  dplyr :: select(workerid,content,short_trigger,question_type,response,block_ai,prior,PriorMean,event) %>%
  spread(question_type,response) %>%
  unite(item,short_trigger,content,remove=F)
nrow(t) #10900 / 20 stimuli per Turker = 545 Turkers
head(t)

# exclude main clause controls
t_nomc = droplevels(subset(t, short_trigger != "MC"))
nrow(t_nomc) #10900 / 545 Turkers = 20 target items

# center the block, at-issueness and prior variables
t_nomc = cbind(t_nomc,myCenter(t[,c("block_ai","ai","PriorMean")]))
summary(t_nomc)

length(unique(t_nomc$item)) #400

# predict projection from at-issueness and verb (no block here) --- 
names(t_nomc)

t_nomc$short_trigger <- relevel(t_nomc$short_trigger, ref = "be_right")

model = lmer(projective ~ cai * cPriorMean * short_trigger + (1+cai|workerid), data = t_nomc, REML=F)
summary(model)

model.b = lmer(projective ~ cai * cPriorMean + short_trigger + (1+cai|workerid), data = t_nomc, REML=F)
summary(model.b)

model.c = lmer(projective ~ cai * short_trigger + cPriorMean + (1+cai|workerid), data = t_nomc, REML=F)
summary(model.c)

anova(model,model.b) # model with interaction between at-issueness and predicate is better
anova(model,model.c) # model with interaction between at-issueness and prior is better


# predict projection from at-issueness and verb (with block now) ----
names(t_nomc)

t_nomc$short_trigger <- relevel(t_nomc$short_trigger, ref = "be_right")

model = lmer(projective ~ cai * cblock_ai * short_trigger + (1+cai|workerid) + (1|content), data = t_nomc, REML=F)
summary(model)

model.b = lmer(projective ~ cai * cblock_ai + short_trigger + (1+cai|workerid) + (1|content), data = t_nomc, REML=F)
summary(model.b)

model.c = lmer(projective ~ cai * short_trigger + cblock_ai + (1+cai|workerid) + (1|content), data = t_nomc, REML=F)
summary(model.c)

anova(model,model.b) # model with interaction between at-issueness and predicate is better
anova(model,model.c) # model with interaction between at-issueness and block is better

# two main analyses of interest ----

# 1. predict at-issueness from prior, while controlling for the effect of block
# random effects by participant and item. get p-values via lmerTest (Satterthwaite's approximation)
m.ai.prior = lmer(cai ~ cPriorMean + cblock_ai + (1+cPriorMean|workerid) + (1+cPriorMean|item), data=t)
summary(m.ai.prior) # cPriorMean not significant
out <- capture.output(summary(m.ai.prior))
cat("Predict at-issueness from prior", out, file="../models/predict-ai-from-prior.txt", 
    sep="\n", append=TRUE)

m.ai.prior = lmer(cai ~ cPriorMean * cblock_ai + (1+cPriorMean*cblock_ai|workerid) + (1+cPriorMean*cblock_ai|item), data=t)
summary(m.ai.prior) # doesn't converge after 20 min

# 2.predict projectivity from prior and at-issueness and their interaction
# while controlling for the effect of block on proj and ai ratings
# random effects by participant and item (lexical content+target expression). 
# get p-values via lmerTest (Satterthwaite's approximation)

# the model reported in the paper (didn't converge after 3h)
m.proj = lmer(projective ~ cai * cPriorMean * cblock_ai + (1+cai * cPriorMean * cblock_ai|workerid) + (1+cai * cPriorMean * cblock_ai|item), data=t)


# models that do not converge (haven't tried this one yet)
m.proj.0 = lmer(projective ~ cai * cPriorMean * cblock_ai + (1+cai * cPriorMean + cblock_ai|workerid) + (1+cai * cPriorMean + cblock_ai|item), data=t)

# model that converges
m.proj = lmer(projective ~ cai * cPriorMean + (1+cai + cPriorMean|workerid) + (1+cai + cPriorMean|item), data=t)
summary(m.proj) 
out <- capture.output(summary(m.proj))
cat("Predict projection from ai and prior", out, file="../models/predict-proj-from-ai-and-prior.txt", 
    sep="\n", append=TRUE)

m.proj.b = lmer(projective ~ cai + cPriorMean + (1+cai + cPriorMean|workerid) + (1+cai + cPriorMean|item), data=t)
summary(m.proj.b)
out <- capture.output(summary(m.proj.b))
cat("Predict projection from ai and prior", out, file="../models/predict-proj-from-ai-and-prior.txt", 
    sep="\n", append=TRUE)

anova(m.proj,m.proj.b) # p=.12 (so interaction is not significant)

# refitting model(s) with ML (instead of REML)
# Data: t
# Models:
#   m.proj.b: projective ~ cai + cPriorMean + (1 + cai + cPriorMean | workerid) + 
#   m.proj.b:     (1 + cai + cPriorMean | item)
# m.proj: projective ~ cai * cPriorMean + (1 + cai + cPriorMean | workerid) + 
#   m.proj:     (1 + cai + cPriorMean | item)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
# m.proj.b 16 3623.7 3739.2 -1795.8   3591.7                         
# m.proj   17 3623.2 3745.9 -1794.6   3589.2 2.4657      1     0.1164




# if too much of the variance in at-issueness is explained by the prior 
# so that collinearity is too high: 
# regress prior onto ai and enter residuals as new predictor

# if none of the block effects reach significance, re-run analysis without block predictor 
# for ease of interpretability

# if random effects structure prevents model from converging, 
# remove slopes step-wise, starting with block interactions and main effect