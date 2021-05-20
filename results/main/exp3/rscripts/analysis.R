# interaction of prior beliefs, at-issueness and projection
# for 20 clause-embedding predicates
# analysis.R

# load required packages
library(tidyverse)
library(lme4)
library(lmerTest)
library(optimx)

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# load helper functions
source('../../helpers.R')

# load data
d = read.csv("../data/cd.csv")
nrow(d) #39390 / 78 = 505 Turkers

# prepare for spreading: rename the 'prior' column into 'prior_type'
colnames(d)[colnames(d)=="prior"] = "prior_type"

# doing this step before spreading solves the problem with prior_type in MC trials
# exclude main clause controls
d_nomc = droplevels(subset(d, short_trigger != "MC"))
nrow(d_nomc) # 30300 / 505 = 60 (3 x 20 target trials)

# spread responses over separate columns for projectivity, at-issueness and prior probability
t_nomc = d_nomc %>%
  mutate(block_ai = as.factor(ifelse(question_type=="ai"&block=="block1", "block1",
                           ifelse(question_type=="ai"&block=="block2", "block2",
                                  ifelse(question_type=="projective"&block=="block1", "block2",
                                         ifelse(question_type=="projective"&block=="block2", "block1", NA)))))) %>%
  na.locf(fromLast = TRUE) %>%   # replaces NA with the nearest non-NA; fromLast causes observations to be carried backward 
  select(workerid,content,short_trigger,question_type,response,prior_type,block_ai) %>%     # 'event' (CC) is missing... (?)
  spread(question_type,response) %>%
  unite(item,short_trigger,content,remove=F) %>%
  mutate(predicate_type = as.factor(case_when(
    short_trigger %in% c("know", "discover", "reveal", "see", "be_annoyed") ~ "factive", 
    short_trigger %in% c("pretend", "think", "suggest", "say") ~ "non-factive",
    TRUE ~ "optionally factive"))) %>%
  mutate(predicate_type = fct_relevel(predicate_type,"non-factive","optionally factive"))
nrow(t_nomc) # 10100 

contrasts(t_nomc$block_ai)
contrasts(t_nomc$predicate_type)

# center prior probability, projectivity, and at-issueness
t_nomc = cbind(t_nomc,myCenter(t_nomc[,c("prior","projective","ai","block_ai","prior_type")]))
summary(t_nomc)

# define item
t_nomc$item = as.factor(paste(t_nomc$short_trigger,t_nomc$content))
t_nomc$workerid = as.factor(as.character(t_nomc$workerid))

# set lower probability fact as reference level of prior_type
contrasts(t_nomc$prior_type) = c(1,0)
table(t_nomc$prior_type)

# ANALYSES FROM PRIOR PAPER ----

# analysis 1: prior ----
# does high/low prob fact predict actual prior ratings? 
m.prior = lmer(prior ~ prior_type + (1+prior_type|item) + (1+prior_type|workerid), data=t_nomc, REML=F)
summary(m.prior)
# PRIOR PAPER: prior_type1   0.45166    0.01451 376.16696   31.12   <2e-16 ***
# prior_type1 5.141e-01  9.979e-03 7.566e+02   51.52   <2e-16 ***

# analysis 1a: does block order predict prior ratings beyond high/low prob fact?

m.prior.block = lmer(prior ~ cprior_type*cblock_ai + (1+cprior_type+cblock_ai|item) + (1+cprior_type|workerid), data=t_nomc, REML=F)
summary(m.prior.block)
# PRIOR PAPER: there was a main effect of block order (-.03, negligible), but no interaction 
# here: no main effect of block order, no interaction
# (Intercept)             0.433335   0.004977 724.462469  87.073   <2e-16 ***
# cprior_type            -0.514142   0.009976 756.447807 -51.540   <2e-16 ***
# cblock_ai              -0.003275   0.007013 488.909866  -0.467    0.641    
# cprior_type:cblock_ai  -0.009290   0.015313 504.893027  -0.607    0.544 


# analysis 2: projection (categorical priors) ----
# does high/low prob fact predict projection ratings?
m.proj = lmer(projective ~ prior_type + (1|item) + (1+prior_type|workerid), data=t_nomc, REML=F)
summary(m.proj)
# PRIOR PAPER: prior_type1   0.13666    0.01116 286.55761   12.24   <2e-16 ***
# here: prior_type1 9.351e-02  7.224e-03 5.049e+02   12.94   <2e-16 ***
ranef(m.proj)

# analysis 2a: does block order predict projection ratings beyond high/low prob fact?

# do not rerun this code, if already run for analysis 1a
# center fixed effects predictors first to reduce collinearity
# d_nomc = cbind(d_nomc,myCenter(d_nomc[,c("prior_type","block_proj")]))

m.proj.block = lmer(projective ~ cprior_type*cblock_ai + (1+cblock_ai|item) + (1+cprior_type|workerid), data=t_nomc, REML=F)
summary(m.proj.block)
# PRIOR PAPER: no effect of block order
# different here, small effect of block order
# cprior_type            -0.093618   0.007141 504.271348 -13.110  < 2e-16 ***
# cblock_ai               0.036598   0.012811 505.379920   2.857 0.004457 ** 
# cprior_type:cblock_ai  -0.050412   0.014274 502.956515  -3.532 0.000451 ***

# analysis 2b: does the prior effect hold independently of predicate?
t_nomc$short_trigger <- relevel(t_nomc$short_trigger, ref = "pretend")

m.proj.pred = lmer(projective ~ cprior_type*short_trigger + (1|content) + (1+cprior_type|workerid), data=t_nomc, REML=F)
summary(m.proj.pred)
# PRIOR PAPER answer: yes! lots of main effects of predicate, but no significant interactions with prior type (except for marginal interaction for know, p < .1, but not to be taken seriously)
# here: interactions with be_annoyed, pretend, prove, some others marginal

# analysis 3: projection (group level priors) ----
# does group level prior rating predict projection?
summary(t_nomc)

priormeans = t_nomc %>% 
  group_by(content, prior_type) %>% 
  summarise(prior_mean = mean(prior))
nrow(priormeans) # 40 means, sanity check

t_nomc = t_nomc %>% 
  left_join(priormeans, by=c("content","prior_type"))

m.proj.group = lmer(projective ~ prior_mean + (1|item) + (1+prior_mean|workerid), data=t_nomc, REML=F)
summary(m.proj.group)
# PRIOR PAPER: prior_mean    0.30507    0.02424 298.33374   12.58   <2e-16 ***
# here: prior_mean    0.18494    0.01370 537.03585    13.5   <2e-16 ***


# analysis 4: projection (individual prior ratings) ----
# does individual prior rating predict projection, 
# and does it do so better than categorical high/low prior predictor and group prior mean?
m.proj.ind = lmer(projective ~ prior + (1|item) + (1+prior|workerid), data=t_nomc, REML=F)
summary(m.proj.ind)
# PRIOR PAPER: prior         0.27546    0.01988 298.66260   13.85   <2e-16 ***
# here: prior         0.16952    0.01203 538.50001   14.10   <2e-16 ***

summary(m.proj.group)
summary(m.proj)

#BIC comparison (smaller is better)
# individual prior ratings best
BIC(m.proj.ind) #3685.419
BIC(m.proj.group) #3776.706
BIC(m.proj) #3831.055

m.proj.ind.plus = lmer(projective ~ prior + prior_type + (1|item) + (1+prior|workerid), data=t_nomc, REML=F)
summary(m.proj.ind.plus)

anova(m.proj.ind,m.proj.ind.plus) #plus marginally better
anova(m.proj,m.proj.ind.plus) #plus better
# both the BIC comparison and the likelihood ratio comparison indicate that 
# the individual-level prior model is better than the population-level or categorical one

# # analysis 5: projection from mean at-issueness and predicate ----
# does mean at-issueness predict projection ratings?
ai.means = t_nomc %>%
  group_by(short_trigger) %>%
  summarize(AImean = mean(ai)) %>%
  ungroup()
ai.means

proj.means = t_nomc %>%
  group_by(short_trigger) %>%
  summarize(Projmean = mean(projective)) %>%
  ungroup()
proj.means

agr = t_nomc %>%
  group_by(short_trigger) %>%
  summarise(mean_ai = mean(ai), ci.low.ai=ci.low(ai), ci.high.ai=ci.high(ai), mean_proj = mean(projective), ci.low.proj=ci.low(projective),ci.high.proj=ci.high(projective))
agr = as.data.frame(agr)
agr$YMin = agr$mean_proj - agr$ci.low.proj
agr$YMax = agr$mean_proj + agr$ci.high.proj
agr$XMin = agr$mean_ai - agr$ci.low.ai
agr$XMax = agr$mean_ai + agr$ci.high.ai

#correlation (as in JoS paper)
cor(agr$mean_ai,agr$mean_proj) #.73



# merge mean at-issueness with data
t_nomc <- merge(t_nomc,ai.means,by="short_trigger")
t_nomc <- merge(t_nomc,proj.means,by="short_trigger")
head(t_nomc)

# center the block and at-issueness variables
t_nomc = cbind(t_nomc,myCenter(t_nomc[,c("ai","AImean")]))
summary(t_nomc)

# predict mean projection from mean at-issueness, predicate and interaction
m.proj = lmer(projective ~ cAImean + short_trigger + (1|item) + (1|workerid), data=t_nomc, REML=F)
summary(m.proj)
#cAImean     9.114e-01  5.083e-02 5.521e+02   17.93   <2e-16 ***
  
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
t_nomc$short_trigger <- relevel(t_nomc$short_trigger, ref = "confirm")

m.proj.ind = lmer(projective ~ cai*short_trigger + (1|item) + (1+cai|workerid), data=t_nomc, REML=F)
summary(m.proj.ind)
#cai           0.22082    0.01737 307.80260   12.71   <2e-16 ***
summary(m.proj)

# BIC: lower is better, so the mean model is better than the individual model
BIC(m.proj.ind) #1870.491
BIC(m.proj) #1861.387


### ANALYSES WITH AT-ISSUENESS ----

# the model we want to fit to test whether prior and at-issueness predict projection and whether the two
# factors are independent from one another
model = lmer(projective ~ cprior  *  cai  + (1+cprior+cai|workerid) + (1|content) + (1+cprior+cai|short_trigger), data = t_nomc, REML=F,control = lmerControl(
  optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))
summary(model)

#(Intercept) 4.403e-01  3.730e-02 2.104e+01  11.804 9.66e-11 ***
#cprior      1.616e-01  1.173e-02 1.282e+02  13.777  < 2e-16 ***
#cai         1.769e-01  3.677e-02 2.266e+01   4.812 7.73e-05 ***
#cprior:cai  3.707e-02  2.227e-02 3.948e+03   1.665    0.096 .

# the interaction with block is included as a control
model = lmer(projective ~ cprior  *  cai * cblock_ai + (1+cprior+cai|workerid) + (1|content) + (1+cprior+cai|short_trigger), data = t_nomc, REML=F,control = lmerControl(
  optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))
summary(model)
#Estimate Std. Error         df t value Pr(>|t|)    
#(Intercept)             0.44037    0.03727   21.09605  11.815 9.14e-11 ***
#cprior                  0.16156    0.01152  130.82748  14.026  < 2e-16 ***
#cai                     0.17588    0.03672   22.74310   4.790 8.08e-05 ***
#cblock_ai               0.03560    0.01203  470.43523   2.960 0.003236 ** 
#cprior:cai              0.03403    0.02220 4005.81767   1.533 0.125395    
#cprior:cblock_ai        0.08100    0.02194  501.12364   3.691 0.000248 ***
#cai:cblock_ai          -0.04600    0.02226  439.73620  -2.067 0.039356 *  
#cprior:cai:cblock_ai   -0.02859    0.04418 6993.58286  -0.647 0.517624 

# if this model does not converge, remove slopes, starting with those that, per the random effects part of the output of the non-converging
# model have the smallest variance
# JD: changed optimizer to make sure model converges even with complex random effects structure

# include predicate
t_nomc = t_nomc %>%
  mutate(short_trigger=fct_relevel(short_trigger,"pretend"))

# additionally add interaction with predicate (remove block for the time being since it's not doing much)
model.pred.simple = lmer(projective ~  short_trigger * cprior * cai - cai + (1+cprior+cai|workerid) + (1|content), data = t_nomc, REML=F,control = lmerControl(
  optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))
summary(model.pred.simple)


# if too much of the variance in at-issueness is explained by the prior
# so that collinearity is too high: 
# regress prior onto ai and enter residuals as new predictor

# if none of the block effects reach significance, re-run analysis without block predictor 
# for ease of interpretability

# if random effects structure prevents model from converging, 
# remove slopes step-wise, starting with block interactions and main effect




