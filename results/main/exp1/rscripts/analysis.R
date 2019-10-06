# analysis file for experiment investigating whether at-issueness predicts projection
# for the contents of the complements of 20 predicates

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# load required packages
require(tidyverse)
library(ggrepel)
library(emmeans)
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

# main analysis of interest: predict projectivity from at-issueness ----
# while controlling for block; random effects by subject, lexical content, and target expression
# RE for content and predicate rather than item (pred+content) so that we can look at how much variability is introduced
# by the predicate and the content; also, this way we can identify variability introduced by predicates
# only slope for content, no intercept because of convergence issues: by-content intercept has least variability, 
# so we removed them (for JoS paper; now we should try to fit the model with them)

# use lmerTest instead of model comparison

# predict projection from at-issueness and verb
names(t_nomc)

t_nomc$short_trigger <- relevel(t_nomc$short_trigger, ref = "be_right")

model = lmer(projective ~ cai * cblock_ai * short_trigger + (1+cai|workerid) + (1|content), data = t_nomc, REML=F)
summary(model)

model.b = lmer(projective ~ cai * cblock_ai + short_trigger + (1+cai|workerid) + (1|content), data = t_nomc, REML=F)
summary(model.b)

anova(model,model.b) # model with interaction is better

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

# pairwise comparisons of projectivity of the predicates using tukey ----
# run the model again with short_trigger as fixed effect 
# no at-issueness or block effects
levels(t$short_trigger)

# order short_trigger so that comparison is presented by projectivity mean, not alphabetically
proj = t %>%
  group_by(short_trigger) %>%
  summarize(Mean = mean(projective)) %>%
  mutate(short_trigger = fct_reorder(as.factor(short_trigger),Mean))
proj
levels(proj$short_trigger)

t$short_trigger <- factor(t$short_trigger, levels = unique(levels(proj$short_trigger)))
levels(t$short_trigger)

# model 
m.mr.fixedtrigger = lmer(projective ~ short_trigger + (1|workerid) + (1|content), data=t, REML=F)
summary(m.mr.fixedtrigger)

pc = lsmeans(m.mr.fixedtrigger, pairwise~short_trigger, adjust="tukey")
options(max.print=10000)
pc

# $contrasts
# contrast                   estimate     SE  df z.ratio p.value
# MC - be_right             -0.073797 0.0182 Inf  -4.045 0.0089 
# MC - say                  -0.103259 0.0182 Inf  -5.662 <.0001 
# MC - suggest              -0.111267 0.0182 Inf  -6.101 <.0001 
# MC - think                -0.111813 0.0182 Inf  -6.131 <.0001 
# MC - pretend              -0.114709 0.0182 Inf  -6.289 <.0001 
# MC - prove                -0.189462 0.0182 Inf -10.387 <.0001 
# MC - confirm              -0.224455 0.0182 Inf -12.301 <.0001 
# MC - establish            -0.242362 0.0182 Inf -13.291 <.0001 
# MC - demonstrate          -0.336430 0.0182 Inf -18.447 <.0001 
# MC - announce             -0.456191 0.0182 Inf -25.015 <.0001 
# MC - confess              -0.488910 0.0182 Inf -26.816 <.0001 
# MC - admit                -0.509783 0.0182 Inf -27.958 <.0001 
# MC - reveal               -0.550251 0.0182 Inf -30.179 <.0001 
# MC - acknowledge          -0.595373 0.0182 Inf -32.655 <.0001 
# MC - hear                 -0.652794 0.0182 Inf -35.796 <.0001 
# MC - discover             -0.668642 0.0182 Inf -36.650 <.0001 
# MC - see                  -0.701138 0.0182 Inf -38.441 <.0001 
# MC - inform               -0.711693 0.0182 Inf -39.016 <.0001 
# MC - know                 -0.754584 0.0182 Inf -41.376 <.0001 
# MC - be_annoyed           -0.765447 0.0182 Inf -41.953 <.0001 
# be_right - say            -0.029462 0.0222 Inf  -1.330 0.9993 
# be_right - suggest        -0.037470 0.0221 Inf  -1.692 0.9846 
# be_right - think          -0.038016 0.0222 Inf  -1.716 0.9820 
# be_right - pretend        -0.040912 0.0222 Inf  -1.847 0.9607 
# be_right - prove          -0.115666 0.0222 Inf  -5.221 <.0001 
# be_right - confirm        -0.150659 0.0221 Inf  -6.802 <.0001 
# be_right - establish      -0.168565 0.0222 Inf  -7.606 <.0001 
# be_right - demonstrate    -0.262633 0.0222 Inf -11.850 <.0001 
# be_right - announce       -0.382395 0.0222 Inf -17.258 <.0001 
# be_right - confess        -0.415114 0.0222 Inf -18.740 <.0001 
# be_right - admit          -0.435986 0.0222 Inf -19.683 <.0001 
# be_right - reveal         -0.476454 0.0222 Inf -21.510 <.0001 
# be_right - acknowledge    -0.521577 0.0221 Inf -23.551 <.0001 
# be_right - hear           -0.578998 0.0222 Inf -26.136 <.0001 
# be_right - discover       -0.594845 0.0222 Inf -26.852 <.0001 
# be_right - see            -0.627341 0.0222 Inf -28.308 <.0001 
# be_right - inform         -0.637896 0.0222 Inf -28.778 <.0001 
# be_right - know           -0.680787 0.0221 Inf -30.737 <.0001 
# be_right - be_annoyed     -0.691650 0.0222 Inf -31.209 <.0001 
# say - suggest             -0.008008 0.0221 Inf  -0.362 1.0000 
# say - think               -0.008554 0.0222 Inf  -0.386 1.0000 
# say - pretend             -0.011450 0.0222 Inf  -0.517 1.0000 
# say - prove               -0.086204 0.0221 Inf  -3.892 0.0160 
# say - confirm             -0.121197 0.0221 Inf  -5.472 <.0001 
# say - establish           -0.139103 0.0221 Inf  -6.281 <.0001 
# say - demonstrate         -0.233171 0.0222 Inf -10.526 <.0001 
# say - announce            -0.352932 0.0221 Inf -15.936 <.0001 
# say - confess             -0.385652 0.0221 Inf -17.415 <.0001 
# say - admit               -0.406524 0.0221 Inf -18.360 <.0001 
# say - reveal              -0.446992 0.0221 Inf -20.187 <.0001 
# say - acknowledge         -0.492115 0.0221 Inf -22.220 <.0001 
# say - hear                -0.549536 0.0221 Inf -24.818 <.0001 
# say - discover            -0.565383 0.0222 Inf -25.518 <.0001 
# say - see                 -0.597879 0.0222 Inf -26.987 <.0001 
# say - inform              -0.608434 0.0222 Inf -27.465 <.0001 
# say - know                -0.651325 0.0222 Inf -29.403 <.0001 
# say - be_annoyed          -0.662188 0.0222 Inf -29.890 <.0001 
# suggest - think           -0.000546 0.0222 Inf  -0.025 1.0000 
# suggest - pretend         -0.003442 0.0222 Inf  -0.155 1.0000 
# suggest - prove           -0.078196 0.0222 Inf  -3.529 0.0569 
# suggest - confirm         -0.113189 0.0222 Inf  -5.109 0.0001 
# suggest - establish       -0.131095 0.0221 Inf  -5.919 <.0001 
# suggest - demonstrate     -0.225163 0.0221 Inf -10.167 <.0001 
# suggest - announce        -0.344925 0.0222 Inf -15.569 <.0001 
# suggest - confess         -0.377644 0.0221 Inf -17.052 <.0001 
# suggest - admit           -0.398516 0.0221 Inf -17.995 <.0001 
# suggest - reveal          -0.438984 0.0221 Inf -19.824 <.0001 
# suggest - acknowledge     -0.484106 0.0221 Inf -21.861 <.0001 
# suggest - hear            -0.541528 0.0221 Inf -24.452 <.0001 
# suggest - discover        -0.557375 0.0222 Inf -25.163 <.0001 
# suggest - see             -0.589871 0.0222 Inf -26.630 <.0001 
# suggest - inform          -0.600426 0.0222 Inf -27.099 <.0001 
# suggest - know            -0.643317 0.0222 Inf -29.042 <.0001 
# suggest - be_annoyed      -0.654180 0.0222 Inf -29.507 <.0001 
# think - pretend           -0.002896 0.0221 Inf  -0.131 1.0000 
# think - prove             -0.077649 0.0221 Inf  -3.506 0.0613 
# think - confirm           -0.112642 0.0222 Inf  -5.081 0.0001 
# think - establish         -0.130549 0.0221 Inf  -5.896 <.0001 
# think - demonstrate       -0.224617 0.0222 Inf -10.138 <.0001 
# think - announce          -0.344378 0.0221 Inf -15.548 <.0001 
# think - confess           -0.377097 0.0221 Inf -17.026 <.0001 
# think - admit             -0.397970 0.0221 Inf -17.967 <.0001 
# think - reveal            -0.438438 0.0221 Inf -19.797 <.0001 
# think - acknowledge       -0.483560 0.0221 Inf -21.840 <.0001 
# think - hear              -0.540981 0.0222 Inf -24.423 <.0001 
# think - discover          -0.556829 0.0222 Inf -25.133 <.0001 
# think - see               -0.589325 0.0222 Inf -26.602 <.0001 
# think - inform            -0.599880 0.0222 Inf -27.071 <.0001 
# think - know              -0.642771 0.0221 Inf -29.029 <.0001 
# think - be_annoyed        -0.653634 0.0222 Inf -29.501 <.0001 
# pretend - prove           -0.074754 0.0221 Inf  -3.377 0.0913 
# pretend - confirm         -0.109747 0.0222 Inf  -4.953 0.0001 
# pretend - establish       -0.127653 0.0221 Inf  -5.764 <.0001 
# pretend - demonstrate     -0.221721 0.0221 Inf -10.011 <.0001 
# pretend - announce        -0.341482 0.0221 Inf -15.418 <.0001 
# pretend - confess         -0.374202 0.0222 Inf -16.894 <.0001 
# pretend - admit           -0.395074 0.0222 Inf -17.833 <.0001 
# pretend - reveal          -0.435542 0.0222 Inf -19.659 <.0001 
# pretend - acknowledge     -0.480664 0.0221 Inf -21.703 <.0001 
# pretend - hear            -0.538085 0.0222 Inf -24.291 <.0001 
# pretend - discover        -0.553933 0.0222 Inf -24.993 <.0001 
# pretend - see             -0.586429 0.0222 Inf -26.463 <.0001 
# pretend - inform          -0.596984 0.0221 Inf -26.958 <.0001 
# pretend - know            -0.639875 0.0221 Inf -28.891 <.0001 
# pretend - be_annoyed      -0.650738 0.0222 Inf -29.375 <.0001 
# prove - confirm           -0.034993 0.0222 Inf  -1.580 0.9932 
# prove - establish         -0.052899 0.0221 Inf  -2.389 0.6943 
# prove - demonstrate       -0.146967 0.0222 Inf  -6.632 <.0001 
# prove - announce          -0.266729 0.0222 Inf -12.041 <.0001 
# prove - confess           -0.299448 0.0222 Inf -13.519 <.0001 
# prove - admit             -0.320321 0.0222 Inf -14.460 <.0001 
# prove - reveal            -0.360788 0.0222 Inf -16.284 <.0001 
# prove - acknowledge       -0.405911 0.0222 Inf -18.324 <.0001 
# prove - hear              -0.463332 0.0222 Inf -20.915 <.0001 
# prove - discover          -0.479179 0.0222 Inf -21.614 <.0001 
# prove - see               -0.511676 0.0222 Inf -23.094 <.0001 
# prove - inform            -0.522231 0.0222 Inf -23.575 <.0001 
# prove - know              -0.565122 0.0221 Inf -25.517 <.0001 
# prove - be_annoyed        -0.575984 0.0222 Inf -25.991 <.0001 
# confirm - establish       -0.017906 0.0222 Inf  -0.808 1.0000 
# confirm - demonstrate     -0.111974 0.0222 Inf  -5.053 0.0001 
# confirm - announce        -0.231736 0.0222 Inf -10.455 <.0001 
# confirm - confess         -0.264455 0.0222 Inf -11.936 <.0001 
# confirm - admit           -0.285328 0.0221 Inf -12.882 <.0001 
# confirm - reveal          -0.325795 0.0222 Inf -14.708 <.0001 
# confirm - acknowledge     -0.370918 0.0222 Inf -16.739 <.0001 
# confirm - hear            -0.428339 0.0222 Inf -19.331 <.0001 
# confirm - discover        -0.444186 0.0222 Inf -20.042 <.0001 
# confirm - see             -0.476683 0.0222 Inf -21.513 <.0001 
# confirm - inform          -0.487238 0.0222 Inf -21.983 <.0001 
# confirm - know            -0.530129 0.0222 Inf -23.921 <.0001 
# confirm - be_annoyed      -0.540991 0.0222 Inf -24.409 <.0001 
# establish - demonstrate   -0.094068 0.0221 Inf  -4.247 0.0039 
# establish - announce      -0.213830 0.0221 Inf  -9.656 <.0001 
# establish - confess       -0.246549 0.0221 Inf -11.135 <.0001 
# establish - admit         -0.267421 0.0221 Inf -12.078 <.0001 
# establish - reveal        -0.307889 0.0221 Inf -13.906 <.0001 
# establish - acknowledge   -0.353011 0.0221 Inf -15.941 <.0001 
# establish - hear          -0.410432 0.0222 Inf -18.528 <.0001 
# establish - discover      -0.426280 0.0222 Inf -19.240 <.0001 
# establish - see           -0.458776 0.0221 Inf -20.715 <.0001 
# establish - inform        -0.469331 0.0221 Inf -21.191 <.0001 
# establish - know          -0.512222 0.0221 Inf -23.129 <.0001 
# establish - be_annoyed    -0.523085 0.0222 Inf -23.612 <.0001 
# demonstrate - announce    -0.119762 0.0221 Inf  -5.409 <.0001 
# demonstrate - confess     -0.152481 0.0221 Inf  -6.887 <.0001 
# demonstrate - admit       -0.173353 0.0221 Inf  -7.827 <.0001 
# demonstrate - reveal      -0.213821 0.0221 Inf  -9.656 <.0001 
# demonstrate - acknowledge -0.258943 0.0221 Inf -11.695 <.0001 
# demonstrate - hear        -0.316365 0.0221 Inf -14.284 <.0001 
# demonstrate - discover    -0.332212 0.0222 Inf -14.995 <.0001 
# demonstrate - see         -0.364708 0.0221 Inf -16.466 <.0001 
# demonstrate - inform      -0.375264 0.0221 Inf -16.950 <.0001 
# demonstrate - know        -0.418154 0.0222 Inf -18.876 <.0001 
# demonstrate - be_annoyed  -0.429017 0.0222 Inf -19.367 <.0001 
# announce - confess        -0.032719 0.0221 Inf  -1.477 0.9971 
# announce - admit          -0.053592 0.0221 Inf  -2.420 0.6708 
# announce - reveal         -0.094060 0.0221 Inf  -4.247 0.0039 
# announce - acknowledge    -0.139182 0.0221 Inf  -6.285 <.0001 
# announce - hear           -0.196603 0.0221 Inf  -8.876 <.0001 
# announce - discover       -0.212451 0.0222 Inf  -9.586 <.0001 
# announce - see            -0.244947 0.0222 Inf -11.058 <.0001 
# announce - inform         -0.255502 0.0221 Inf -11.536 <.0001 
# announce - know           -0.298393 0.0221 Inf -13.475 <.0001 
# announce - be_annoyed     -0.309256 0.0221 Inf -13.967 <.0001 
# confess - admit           -0.020873 0.0221 Inf  -0.943 1.0000 
# confess - reveal          -0.061340 0.0221 Inf  -2.771 0.3976 
# confess - acknowledge     -0.106463 0.0221 Inf  -4.809 0.0003 
# confess - hear            -0.163884 0.0221 Inf  -7.401 <.0001 
# confess - discover        -0.179732 0.0221 Inf  -8.118 <.0001 
# confess - see             -0.212228 0.0221 Inf  -9.584 <.0001 
# confess - inform          -0.222783 0.0221 Inf -10.060 <.0001 
# confess - know            -0.265674 0.0221 Inf -11.995 <.0001 
# confess - be_annoyed      -0.276537 0.0222 Inf -12.483 <.0001 
# admit - reveal            -0.040468 0.0221 Inf  -1.828 0.9646 
# admit - acknowledge       -0.085590 0.0221 Inf  -3.865 0.0177 
# admit - hear              -0.143011 0.0221 Inf  -6.459 <.0001 
# admit - discover          -0.158859 0.0221 Inf  -7.173 <.0001 
# admit - see               -0.191355 0.0221 Inf  -8.641 <.0001 
# admit - inform            -0.201910 0.0222 Inf  -9.115 <.0001 
# admit - know              -0.244801 0.0221 Inf -11.054 <.0001 
# admit - be_annoyed        -0.255664 0.0222 Inf -11.539 <.0001 
# reveal - acknowledge      -0.045122 0.0221 Inf  -2.038 0.9019 
# reveal - hear             -0.102543 0.0221 Inf  -4.630 0.0007 
# reveal - discover         -0.118391 0.0221 Inf  -5.346 <.0001 
# reveal - see              -0.150887 0.0221 Inf  -6.814 <.0001 
# reveal - inform           -0.161442 0.0221 Inf  -7.289 <.0001 
# reveal - know             -0.204333 0.0221 Inf  -9.228 <.0001 
# reveal - be_annoyed       -0.215196 0.0222 Inf  -9.714 <.0001 
# acknowledge - hear        -0.057421 0.0221 Inf  -2.593 0.5350 
# acknowledge - discover    -0.073269 0.0221 Inf  -3.308 0.1114 
# acknowledge - see         -0.105765 0.0221 Inf  -4.776 0.0004 
# acknowledge - inform      -0.116320 0.0221 Inf  -5.253 <.0001 
# acknowledge - know        -0.159211 0.0221 Inf  -7.191 <.0001 
# acknowledge - be_annoyed  -0.170074 0.0221 Inf  -7.679 <.0001 
# hear - discover           -0.015848 0.0221 Inf  -0.716 1.0000 
# hear - see                -0.048344 0.0221 Inf  -2.183 0.8308 
# hear - inform             -0.058899 0.0222 Inf  -2.659 0.4830 
# hear - know               -0.101790 0.0222 Inf  -4.595 0.0008 
# hear - be_annoyed         -0.112653 0.0222 Inf  -5.084 0.0001 
# discover - see            -0.032496 0.0221 Inf  -1.467 0.9973 
# discover - inform         -0.043051 0.0222 Inf  -1.942 0.9361 
# discover - know           -0.085942 0.0222 Inf  -3.879 0.0168 
# discover - be_annoyed     -0.096805 0.0222 Inf  -4.366 0.0023 
# see - inform              -0.010555 0.0222 Inf  -0.476 1.0000 
# see - know                -0.053446 0.0221 Inf  -2.413 0.6761 
# see - be_annoyed          -0.064309 0.0222 Inf  -2.902 0.3068 
# inform - know             -0.042891 0.0222 Inf  -1.936 0.9380 
# inform - be_annoyed       -0.053754 0.0222 Inf  -2.427 0.6658 
# know - be_annoyed         -0.010863 0.0222 Inf  -0.490 1.0000 
# 
# Degrees-of-freedom method: asymptotic 
# P value adjustment: tukey method for comparing a family of 21 estimates