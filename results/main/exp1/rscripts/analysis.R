# analysis file for experiment investigating whether at-issueness predicts projection
# for the contents of the complements of 20 predicates

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# load required packages
require(tidyverse)
library(ggrepel)
library(lsmeans)
library(lme4)
library(lmerTest)

# load helper functions
source('../../helpers.R')

# load data
d = read.csv("../data/data_preprocessed.csv")
nrow(d) #13520 / 260 Turkers = 52 trials

# spread responses over separate columns for projectivity and at-issueness
t = d %>%
  mutate(block_ai = as.factor(ifelse(question_type == "ai", ifelse(block == "block1", "block1", "block2"), ifelse(block == "block1", "block2", "block1")))) %>%
  dplyr :: select(workerid,content,short_trigger,question_type,response,block_ai) %>%
  spread(question_type,response)
nrow(t) #6760 / 260 = 26 items per Turker

# exclude main clause controls
t_nomc = droplevels(subset(t, short_trigger != "MC"))
nrow(t_nomc) #5200 / 260 = 20 target items

# center the block and at-issueness variables
t_nomc = cbind(t_nomc,myCenter(t_nomc[,c("block_ai","ai")]))
summary(t_nomc)

# main analysis of interest: predict projectivity from at-issueness
# while controlling for block; random effects by subject, lexical content, and target expression
# RE for content and predicate rather than item (pred+content) so that we can look at how much variability is introduced
# by the predicate and the content; also, this way we can identify variability introduced by predicates
# only slope for content, no intercept because of convergence issues: by-content intercept has least variability, 
# so we removed them (for JoS paper; now we should try to fit the model with them)

# use lmerTest instead of model comparison

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

# pairwise comparisons of projectivity of the predicates using tukey 
# run the model again with short_trigger as fixed effect 
# no at-issueness or block effects
levels(t_nomc$short_trigger)

# order short_trigger so that comparison is presented by projectivity mean, not alphabetically
proj = t_nomc %>%
  group_by(short_trigger) %>%
  summarize(Mean = mean(projective)) %>%
  mutate(short_trigger = fct_reorder(as.factor(short_trigger),Mean))
proj
levels(proj$short_trigger)

t_nomc$short_trigger <- factor(t_nomc$short_trigger, levels = unique(levels(proj$short_trigger)))
levels(t_nomc$short_trigger)

# model (why don't we have a random effect for content here?)
m.mr.fixedtrigger = lmer(projective ~ short_trigger + (1|workerid), data=t_nomc, REML=F)
summary(m.mr.fixedtrigger)
# no main clauses, 5200 data points

pc = lsmeans(m.mr.fixedtrigger, pairwise~short_trigger, adjust="tukey")
options(max.print=10000)
pc

# Degrees-of-freedom method: satterthwaite 
# Confidence level used: 0.95 
# 
# $contrasts
# contrast                      estimate         SE   df t.ratio p.value
# be_right - say            -0.029192308 0.02361268 4940  -1.236  0.9996
# be_right - suggest        -0.038076923 0.02361268 4940  -1.613  0.9884
# be_right - think          -0.038576923 0.02361268 4940  -1.634  0.9866
# be_right - pretend        -0.041192308 0.02361268 4940  -1.744  0.9729
# be_right - prove          -0.116500000 0.02361268 4940  -4.934  0.0002
# be_right - confirm        -0.151615385 0.02361268 4940  -6.421  <.0001
# be_right - establish      -0.169307692 0.02361268 4940  -7.170  <.0001
# be_right - demonstrate    -0.262807692 0.02361268 4940 -11.130  <.0001
# be_right - announce       -0.381346154 0.02361268 4940 -16.150  <.0001
# be_right - confess        -0.415384615 0.02361268 4940 -17.592  <.0001
# be_right - admit          -0.436730769 0.02361268 4940 -18.496  <.0001
# be_right - reveal         -0.475961538 0.02361268 4940 -20.157  <.0001
# be_right - acknowledge    -0.522000000 0.02361268 4940 -22.107  <.0001
# be_right - hear           -0.579230769 0.02361268 4940 -24.530  <.0001
# be_right - discover       -0.595038462 0.02361268 4940 -25.200  <.0001
# be_right - see            -0.627769231 0.02361268 4940 -26.586  <.0001
# be_right - inform         -0.638230769 0.02361268 4940 -27.029  <.0001
# be_right - know           -0.680423077 0.02361268 4940 -28.816  <.0001
# be_right - be_annoyed     -0.690923077 0.02361268 4940 -29.261  <.0001
# say - suggest             -0.008884615 0.02361268 4940  -0.376  1.0000
# say - think               -0.009384615 0.02361268 4940  -0.397  1.0000
# say - pretend             -0.012000000 0.02361268 4940  -0.508  1.0000
# say - prove               -0.087307692 0.02361268 4940  -3.697  0.0300
# say - confirm             -0.122423077 0.02361268 4940  -5.185  <.0001
# say - establish           -0.140115385 0.02361268 4940  -5.934  <.0001
# say - demonstrate         -0.233615385 0.02361268 4940  -9.894  <.0001
# say - announce            -0.352153846 0.02361268 4940 -14.914  <.0001
# say - confess             -0.386192308 0.02361268 4940 -16.355  <.0001
# say - admit               -0.407538462 0.02361268 4940 -17.259  <.0001
# say - reveal              -0.446769231 0.02361268 4940 -18.921  <.0001
# say - acknowledge         -0.492807692 0.02361268 4940 -20.870  <.0001
# say - hear                -0.550038462 0.02361268 4940 -23.294  <.0001
# say - discover            -0.565846154 0.02361268 4940 -23.964  <.0001
# say - see                 -0.598576923 0.02361268 4940 -25.350  <.0001
# say - inform              -0.609038462 0.02361268 4940 -25.793  <.0001
# say - know                -0.651230769 0.02361268 4940 -27.580  <.0001
# say - be_annoyed          -0.661730769 0.02361268 4940 -28.024  <.0001
# suggest - think           -0.000500000 0.02361268 4940  -0.021  1.0000
# suggest - pretend         -0.003115385 0.02361268 4940  -0.132  1.0000
# suggest - prove           -0.078423077 0.02361268 4940  -3.321  0.0998
# suggest - confirm         -0.113538462 0.02361268 4940  -4.808  0.0003
# suggest - establish       -0.131230769 0.02361268 4940  -5.558  <.0001
# suggest - demonstrate     -0.224730769 0.02361268 4940  -9.517  <.0001
# suggest - announce        -0.343269231 0.02361268 4940 -14.537  <.0001
# suggest - confess         -0.377307692 0.02361268 4940 -15.979  <.0001
# suggest - admit           -0.398653846 0.02361268 4940 -16.883  <.0001
# suggest - reveal          -0.437884615 0.02361268 4940 -18.544  <.0001
# suggest - acknowledge     -0.483923077 0.02361268 4940 -20.494  <.0001
# suggest - hear            -0.541153846 0.02361268 4940 -22.918  <.0001
# suggest - discover        -0.556961538 0.02361268 4940 -23.587  <.0001
# suggest - see             -0.589692308 0.02361268 4940 -24.974  <.0001
# suggest - inform          -0.600153846 0.02361268 4940 -25.417  <.0001
# suggest - know            -0.642346154 0.02361268 4940 -27.203  <.0001
# suggest - be_annoyed      -0.652846154 0.02361268 4940 -27.648  <.0001
# think - pretend           -0.002615385 0.02361268 4940  -0.111  1.0000
# think - prove             -0.077923077 0.02361268 4940  -3.300  0.1061
# think - confirm           -0.113038462 0.02361268 4940  -4.787  0.0003
# think - establish         -0.130730769 0.02361268 4940  -5.536  <.0001
# think - demonstrate       -0.224230769 0.02361268 4940  -9.496  <.0001
# think - announce          -0.342769231 0.02361268 4940 -14.516  <.0001
# think - confess           -0.376807692 0.02361268 4940 -15.958  <.0001
# think - admit             -0.398153846 0.02361268 4940 -16.862  <.0001
# think - reveal            -0.437384615 0.02361268 4940 -18.523  <.0001
# think - acknowledge       -0.483423077 0.02361268 4940 -20.473  <.0001
# think - hear              -0.540653846 0.02361268 4940 -22.897  <.0001
# think - discover          -0.556461538 0.02361268 4940 -23.566  <.0001
# think - see               -0.589192308 0.02361268 4940 -24.952  <.0001
# think - inform            -0.599653846 0.02361268 4940 -25.395  <.0001
# think - know              -0.641846154 0.02361268 4940 -27.182  <.0001
# think - be_annoyed        -0.652346154 0.02361268 4940 -27.627  <.0001
# pretend - prove           -0.075307692 0.02361268 4940  -3.189  0.1442
# pretend - confirm         -0.110423077 0.02361268 4940  -4.676  0.0005
# pretend - establish       -0.128115385 0.02361268 4940  -5.426  <.0001
# pretend - demonstrate     -0.221615385 0.02361268 4940  -9.385  <.0001
# pretend - announce        -0.340153846 0.02361268 4940 -14.406  <.0001
# pretend - confess         -0.374192308 0.02361268 4940 -15.847  <.0001
# pretend - admit           -0.395538462 0.02361268 4940 -16.751  <.0001
# pretend - reveal          -0.434769231 0.02361268 4940 -18.413  <.0001
# pretend - acknowledge     -0.480807692 0.02361268 4940 -20.362  <.0001
# pretend - hear            -0.538038462 0.02361268 4940 -22.786  <.0001
# pretend - discover        -0.553846154 0.02361268 4940 -23.455  <.0001
# pretend - see             -0.586576923 0.02361268 4940 -24.842  <.0001
# pretend - inform          -0.597038462 0.02361268 4940 -25.285  <.0001
# pretend - know            -0.639230769 0.02361268 4940 -27.072  <.0001
# pretend - be_annoyed      -0.649730769 0.02361268 4940 -27.516  <.0001
# prove - confirm           -0.035115385 0.02361268 4940  -1.487  0.9956
# prove - establish         -0.052807692 0.02361268 4940  -2.236  0.7777
# prove - demonstrate       -0.146307692 0.02361268 4940  -6.196  <.0001
# prove - announce          -0.264846154 0.02361268 4940 -11.216  <.0001
# prove - confess           -0.298884615 0.02361268 4940 -12.658  <.0001
# prove - admit             -0.320230769 0.02361268 4940 -13.562  <.0001
# prove - reveal            -0.359461538 0.02361268 4940 -15.223  <.0001
# prove - acknowledge       -0.405500000 0.02361268 4940 -17.173  <.0001
# prove - hear              -0.462730769 0.02361268 4940 -19.597  <.0001
# prove - discover          -0.478538462 0.02361268 4940 -20.266  <.0001
# prove - see               -0.511269231 0.02361268 4940 -21.652  <.0001
# prove - inform            -0.521730769 0.02361268 4940 -22.095  <.0001
# prove - know              -0.563923077 0.02361268 4940 -23.882  <.0001
# prove - be_annoyed        -0.574423077 0.02361268 4940 -24.327  <.0001
# confirm - establish       -0.017692308 0.02361268 4940  -0.749  1.0000
# confirm - demonstrate     -0.111192308 0.02361268 4940  -4.709  0.0005
# confirm - announce        -0.229730769 0.02361268 4940  -9.729  <.0001
# confirm - confess         -0.263769231 0.02361268 4940 -11.171  <.0001
# confirm - admit           -0.285115385 0.02361268 4940 -12.075  <.0001
# confirm - reveal          -0.324346154 0.02361268 4940 -13.736  <.0001
# confirm - acknowledge     -0.370384615 0.02361268 4940 -15.686  <.0001
# confirm - hear            -0.427615385 0.02361268 4940 -18.110  <.0001
# confirm - discover        -0.443423077 0.02361268 4940 -18.779  <.0001
# confirm - see             -0.476153846 0.02361268 4940 -20.165  <.0001
# confirm - inform          -0.486615385 0.02361268 4940 -20.608  <.0001
# confirm - know            -0.528807692 0.02361268 4940 -22.395  <.0001
# confirm - be_annoyed      -0.539307692 0.02361268 4940 -22.840  <.0001
# establish - demonstrate   -0.093500000 0.02361268 4940  -3.960  0.0115
# establish - announce      -0.212038462 0.02361268 4940  -8.980  <.0001
# establish - confess       -0.246076923 0.02361268 4940 -10.421  <.0001
# establish - admit         -0.267423077 0.02361268 4940 -11.325  <.0001
# establish - reveal        -0.306653846 0.02361268 4940 -12.987  <.0001
# establish - acknowledge   -0.352692308 0.02361268 4940 -14.937  <.0001
# establish - hear          -0.409923077 0.02361268 4940 -17.360  <.0001
# establish - discover      -0.425730769 0.02361268 4940 -18.030  <.0001
# establish - see           -0.458461538 0.02361268 4940 -19.416  <.0001
# establish - inform        -0.468923077 0.02361268 4940 -19.859  <.0001
# establish - know          -0.511115385 0.02361268 4940 -21.646  <.0001
# establish - be_annoyed    -0.521615385 0.02361268 4940 -22.090  <.0001
# demonstrate - announce    -0.118538462 0.02361268 4940  -5.020  0.0001
# demonstrate - confess     -0.152576923 0.02361268 4940  -6.462  <.0001
# demonstrate - admit       -0.173923077 0.02361268 4940  -7.366  <.0001
# demonstrate - reveal      -0.213153846 0.02361268 4940  -9.027  <.0001
# demonstrate - acknowledge -0.259192308 0.02361268 4940 -10.977  <.0001
# demonstrate - hear        -0.316423077 0.02361268 4940 -13.401  <.0001
# demonstrate - discover    -0.332230769 0.02361268 4940 -14.070  <.0001
# demonstrate - see         -0.364961538 0.02361268 4940 -15.456  <.0001
# demonstrate - inform      -0.375423077 0.02361268 4940 -15.899  <.0001
# demonstrate - know        -0.417615385 0.02361268 4940 -17.686  <.0001
# demonstrate - be_annoyed  -0.428115385 0.02361268 4940 -18.131  <.0001
# announce - confess        -0.034038462 0.02361268 4940  -1.442  0.9970
# announce - admit          -0.055384615 0.02361268 4940  -2.346  0.7021
# announce - reveal         -0.094615385 0.02361268 4940  -4.007  0.0095
# announce - acknowledge    -0.140653846 0.02361268 4940  -5.957  <.0001
# announce - hear           -0.197884615 0.02361268 4940  -8.380  <.0001
# announce - discover       -0.213692308 0.02361268 4940  -9.050  <.0001
# announce - see            -0.246423077 0.02361268 4940 -10.436  <.0001
# announce - inform         -0.256884615 0.02361268 4940 -10.879  <.0001
# announce - know           -0.299076923 0.02361268 4940 -12.666  <.0001
# announce - be_annoyed     -0.309576923 0.02361268 4940 -13.111  <.0001
# confess - admit           -0.021346154 0.02361268 4940  -0.904  1.0000
# confess - reveal          -0.060576923 0.02361268 4940  -2.565  0.5323
# confess - acknowledge     -0.106615385 0.02361268 4940  -4.515  0.0011
# confess - hear            -0.163846154 0.02361268 4940  -6.939  <.0001
# confess - discover        -0.179653846 0.02361268 4940  -7.608  <.0001
# confess - see             -0.212384615 0.02361268 4940  -8.995  <.0001
# confess - inform          -0.222846154 0.02361268 4940  -9.438  <.0001
# confess - know            -0.265038462 0.02361268 4940 -11.224  <.0001
# confess - be_annoyed      -0.275538462 0.02361268 4940 -11.669  <.0001
# admit - reveal            -0.039230769 0.02361268 4940  -1.661  0.9839
# admit - acknowledge       -0.085269231 0.02361268 4940  -3.611  0.0403
# admit - hear              -0.142500000 0.02361268 4940  -6.035  <.0001
# admit - discover          -0.158307692 0.02361268 4940  -6.704  <.0001
# admit - see               -0.191038462 0.02361268 4940  -8.091  <.0001
# admit - inform            -0.201500000 0.02361268 4940  -8.534  <.0001
# admit - know              -0.243692308 0.02361268 4940 -10.320  <.0001
# admit - be_annoyed        -0.254192308 0.02361268 4940 -10.765  <.0001
# reveal - acknowledge      -0.046038462 0.02361268 4940  -1.950  0.9219
# reveal - hear             -0.103269231 0.02361268 4940  -4.373  0.0021
# reveal - discover         -0.119076923 0.02361268 4940  -5.043  0.0001
# reveal - see              -0.151807692 0.02361268 4940  -6.429  <.0001
# reveal - inform           -0.162269231 0.02361268 4940  -6.872  <.0001
# reveal - know             -0.204461538 0.02361268 4940  -8.659  <.0001
# reveal - be_annoyed       -0.214961538 0.02361268 4940  -9.104  <.0001
# acknowledge - hear        -0.057230769 0.02361268 4940  -2.424  0.6434
# acknowledge - discover    -0.073038462 0.02361268 4940  -3.093  0.1850
# acknowledge - see         -0.105769231 0.02361268 4940  -4.479  0.0013
# acknowledge - inform      -0.116230769 0.02361268 4940  -4.922  0.0002
# acknowledge - know        -0.158423077 0.02361268 4940  -6.709  <.0001
# acknowledge - be_annoyed  -0.168923077 0.02361268 4940  -7.154  <.0001
# hear - discover           -0.015807692 0.02361268 4940  -0.669  1.0000
# hear - see                -0.048538462 0.02361268 4940  -2.056  0.8789
# hear - inform             -0.059000000 0.02361268 4940  -2.499  0.5850
# hear - know               -0.101192308 0.02361268 4940  -4.286  0.0031
# hear - be_annoyed         -0.111692308 0.02361268 4940  -4.730  0.0004
# discover - see            -0.032730769 0.02361268 4940  -1.386  0.9982
# discover - inform         -0.043192308 0.02361268 4940  -1.829  0.9565
# discover - know           -0.085384615 0.02361268 4940  -3.616  0.0396
# discover - be_annoyed     -0.095884615 0.02361268 4940  -4.061  0.0077
# see - inform              -0.010461538 0.02361268 4940  -0.443  1.0000
# see - know                -0.052653846 0.02361268 4940  -2.230  0.7819
# see - be_annoyed          -0.063153846 0.02361268 4940  -2.675  0.4475
# inform - know             -0.042192308 0.02361268 4940  -1.787  0.9654
# inform - be_annoyed       -0.052692308 0.02361268 4940  -2.232  0.7809
# know - be_annoyed         -0.010500000 0.02361268 4940  -0.445  1.0000
# 
# P value adjustment: tukey method for comparing a family of 20 estimates 
