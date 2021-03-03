# experiment investigating whether at-issueness predicts projection
# for the contents of the complements of 20 predicates
# analysis.R

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

# load clean data
d = read.csv("../data/cd.csv")
nrow(d) #12584 / 242 Turkers = 52 trials

# spread responses over separate columns for projectivity and at-issueness
t = d %>%
  mutate(block_ai = as.factor(ifelse(question_type == "ai", ifelse(block == "block1", "block1", "block2"), ifelse(block == "block1", "block2", "block1")))) %>%
  dplyr :: select(workerid,content,short_trigger,question_type,response,block_ai) %>%
  spread(question_type,response)
nrow(t) #6292 / 242 = 26 items per Turker

# exclude main clause controls
t_nomc = droplevels(subset(t, short_trigger != "MC"))
nrow(t_nomc) #4840 / 242 = 20 target items

# calculate mean at-issueness for each predicate

ai.means = t_nomc %>%
  group_by(short_trigger) %>%
  summarize(AImean = mean(ai)) %>%
  ungroup()
ai.means

# merge mean at-issueness with data
t_nomc <- merge(t_nomc,ai.means,by="short_trigger")
head(t_nomc)

# center the block and at-issueness variables
t_nomc = cbind(t_nomc,myCenter(t_nomc[,c("block_ai","ai","AImean")]))
summary(t_nomc)

# create item variable
t_nomc$item = as.factor(paste(t_nomc$short_trigger,t_nomc$content))
t_nomc$workerid = as.factor(as.character(t_nomc$workerid))


# main analysis of interest: predict projectivity from at-issueness ----
# while controlling for block; random effects by subject, lexical content, and target expression
# RE for content and predicate rather than item (pred+content) so that we can look at how much variability is introduced
# by the predicate and the content; also, this way we can identify variability introduced by predicates
# only slope for content, no intercept because of convergence issues: by-content intercept has least variability, 
# so we removed them (for JoS paper; now we should try to fit the model with them)

# use lmerTest instead of model comparison

# predict projection from at-issueness and predicate
t_nomc$short_trigger <- relevel(t_nomc$short_trigger, ref = "be_right")

model = lmer(projective ~ cai * short_trigger + (1+cai|workerid) + (1|content), data = t_nomc, REML=F)
summary(model)
# main effect of at-issueness
# cai 0.33782    0.04843 4286.83686   6.976 3.51e-12 ***
# interaction with several predicates

model.1 = lmer(projective ~ cai + short_trigger + (1+cai|workerid) + (1|content), data = t_nomc, REML=F)
summary(model.1)
anova(model,model.1) # model with interaction is better

model.2 = lmer(projective ~ cai * cblock_ai * short_trigger + (1+cai|workerid) + (1|content), data = t_nomc, REML=F)
summary(model.2)
anova(model,model.2) # model with block effect is better

model.3 = lmer(projective ~ cai * short_trigger + cblock_ai + (1+cai|workerid) + (1|content), data = t_nomc, REML=F)
summary(model.3)
anova(model.2,model.3) # model with interaction block effect is better

# the model reported 
m.mr.1 = lmer(projective ~ cai * cblock_ai + (1+cai|workerid) + (0+cai|content) + (1+cai|short_trigger), data=t_nomc, REML=F)
summary(m.mr.1)

# get p-values via likelihood ratio tests
m.mr.0a = lmer(projective ~ cai + cblock_ai + (1+cai|workerid) + (0+cai|content) + (1+cai|short_trigger), data=t_nomc, REML=F)
summary(m.mr.0a)

m.mr.0b = lmer(projective ~ cai + cai : cblock_ai + (1+cai|workerid) + (0+cai|content) + (1+cai|short_trigger), data=t_nomc, REML=F)
summary(m.mr.0b)

m.mr.0c = lmer(projective ~ cblock_ai + cai : cblock_ai + (1+cai|workerid) + (0+cai|content) + (1+cai|short_trigger), data=t_nomc, REML=F)
summary(m.mr.0c)

anova(m.mr.0a,m.mr.1) #p-value for interaction: .3901
anova(m.mr.0b,m.mr.1) #p-value for block: .09019
anova(m.mr.0c,m.mr.1) #p-value for at-issueness: .000002012

# simple effects for interaction interpretation (when interaction is significant)
# - ai removes main effect of at-issueness, so that we can see what the slope of ai is in the two blocks
m.mr.simple = lmer(projective ~ ai * block_ai - ai + (1+ai|workerid) + (0+ai|content) + (1+ai|short_trigger), data=t_nomc, REML=F)
summary(m.mr.simple)

### ANALYSES from prior paper adapted to at-issueness -----


# analysis 1: projection (mean at-issueness) ----
# does mean at-issueness predict projection ratings?
m.proj = lmer(projective ~ cAImean + (1|item) + (1|workerid), data=t_nomc, REML=F)
summary(m.proj)
#AImean        1.08636    0.04771 397.62419   22.77  < 2e-16 ***
ranef(m.proj)

# analysis 1b: does block order predict projection ratings beyond high/low prob fact?

m.proj.block = lmer(projective ~ cAImean*cblock_ai + (1|item) + (1|workerid), data=t_nomc, REML=F)
summary(m.proj.block)
# effect of block order
#(Intercept)          0.50784    0.01150  503.19578  44.155   <2e-16 ***
#cAImean              1.08638    0.04771  397.55227  22.769   <2e-16 ***
#cblock_ai            0.03237    0.01607  240.60395   2.014   0.0451 *  
#cAImean:cblock_ai   -0.02796    0.04059 4314.77252  -0.689   0.4910 

# analysis 2: does the at-issueness effect hold independently of predicate?
m.proj.pred = lmer(projective ~ cAImean*short_trigger + (1|content) + (1|workerid), data=t_nomc, REML=F)
# does not converge, removing by-content RE
m.proj.pred = lmer(projective ~ cAImean*short_trigger + (1|workerid), data=t_nomc, REML=F)
# does not converge
summary(m.proj.pred)
# answer FROM PRIOR PAPER: yes! lots of main effects of predicate, but no significant interactions with prior type 
# (except for marginal interaction for know, p < .1, but not to be taken seriously)

# analysis 3: projection (individual at-issueness ratings) ----
# do individual at-issueness ratings predict projection, 
# and do they do so better than mean at-issueness?
m.proj.ind = lmer(projective ~ cai + (1|item) + (1+ai|workerid), data=t_nomc, REML=F)
summary(m.proj.ind)
#cai           0.22082    0.01737 307.80260   12.71   <2e-16 ***
summary(m.proj)

# BIC: lower is better, so the mean model is better than the individual model
BIC(m.proj.ind) #1870.491
BIC(m.proj) #1861.387



