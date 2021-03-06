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

model = lmer(projective ~ cai * short_trigger + short_trigger * cPriorMean + (1+cai|workerid), data = t_nomc, REML=F)
summary(model)

####################
# START JD's MODELS
t_nomc$VeridicalityGroup = as.factor(
  ifelse(t_nomc$short_trigger %in% c("know", "discover", "reveal", "see", "be_annoyed","know"), "F", 
         ifelse(t_nomc$short_trigger %in% c("pretend", "think", "suggest", "say"), "NF", "OF")))

# model with predicate group as interaction effect:
# main effect of at-issueness, prior, predicate group (ie, compared to factives, non-factives and opt-factives have lower proj ratings)
# significant interactions between at-issueness and predicate group (see simple effects analysis below for interpretation)
# significant 3way interaction between at-issueness, prior, and predicate group (see simple effects analysis below for interpretation)
model = lmer(projective ~ cai * cPriorMean * VeridicalityGroup + (1+cai+cPriorMean|workerid) + (1|content) + (1|short_trigger), data = t_nomc, REML=F)
summary(model)

# simple effects:
# at-issueness only significant for factive and optionaly factive predicates
# prior significant for all three groups
# in only the opt-factive group, there's a 2way interaction between at-issueness and prior that i don't know how to interpret, but that we can probably ignore for the time being because this is starting to feel a little esoteric
model = lmer(projective ~ VeridicalityGroup * ai * PriorMean - ai - PriorMean - ai:PriorMean + (1+cai+cPriorMean|workerid) + (1|content) + (1|short_trigger), data = t_nomc, REML=F)
summary(model)

# model with predicate as main effect (to be reported as per skype conversation)
model = lmer(projective ~ cai * cPriorMean + short_trigger + (1+cai+cPriorMean|workerid) + (1|content), data = t_nomc, REML=F)
summary(model)

# model with predicate as random effect (only to be reported if it comes up in Q&A. 
# cai*priormean interaction significant, but the fact that it isnt when predicate is included as 
# main effect suggests that this interaction is driven by the outlier predicates think, suggest,..)
model = lmer(projective ~ cai * cPriorMean + (1+cai+cPriorMean|workerid) + (1|content) + (1+ cPriorMean|short_trigger), data = t_nomc, REML=F)
summary(model)

# simple effects analysis for the interaction between prior and at-issueness (CAN IGNORE BECAUSE YOU'RE NOT REPORTING THE MODEL WITH PREDICATE AS RANDOM EFFECT, JUST LEAVING IN FOR COMPLETENESS)
model = lmer(projective ~ prior*ai - ai + (1+ai+prior|workerid) + (1|content) + (1+ prior|short_trigger), data = t_nomc, REML=F)
summary(model)

# END JD'S MODELS
####################

model.b = lmer(projective ~ cai + short_trigger * cPriorMean + (1+cai|workerid), data = t_nomc, REML=F)
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