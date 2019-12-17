# graphs file for investigating whether participants' prior probability ratings of a content
# predict their projectivity ratings for that content (Lehnhardt thesis) and for JD/JT research on 
# whether participants' prior probability and at-issueness ratings predict projectivity, and whether the
# two factors are independent

# load required packages
library(tidyverse)
library(ggrepel)
library(dichromat)
library(forcats)
library(RColorBrewer)
library(zoo)  # needed for function na_locf() which replaces each NA with the next non-NA 

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

theme_set(theme_bw())

# load helper functions
source('../../helpers.R')

d = read_csv("../data/data_preprocessed.csv")
nrow(d) # [44304] / 78 trials = [568] Turkers

summary(d)
table(d$prior) # half is high_prior, half is low_prior

# prepare for spreading:
# (1) rename the "prior" column into "prior_type" and
colnames(d)[colnames(d)=="prior"] = "prior_type"
# (2) fill in (arbitrarily) 'high_prior' for the "prior_type" of main clauses (MC)
d[d$short_trigger == "MC",]$prior_type <- "high_prior"

# exclude the main clause controls
# d_nomc = droplevels(subset(d, short_trigger != "MC"))
# nrow(d_nomc) #21800 / 545 = 40 target stimuli per Turker

# spread responses over separate columns for prior probability, projectivity and at-issueness
cd = d %>%
  mutate(block_ai = ifelse(question_type=="ai"&block=="block1", "block1",
                           ifelse(question_type=="ai"&block=="block2", "block2",
                                  ifelse(question_type=="projective"&block=="block1", "block2",
                                         ifelse(question_type=="projective"&block=="block2", "block1", NA))))) %>%
  na.locf(fromLast = TRUE) %>%   # replaces NA with the nearest non-NA; fromLast causes observations to be carried backward 
  select(content,question_type,short_trigger,response,workerid,prior_type,prior_fact,block_ai) %>% 
  spread(question_type,response) %>%
  unite(item,short_trigger,content,remove=F)

table(cd$block_ai) 
# block1 block2 
# 7306    7462
## 4 participants did at-issueness in block1, 5 in block 2

nrow(cd)
table(d$block)
nrow(d)
# without MC: ([80] "block1" + [100] "block2") * 3 = ([234] - 6 * [9]) * 3
# with MC: ([104] "block1" + [130] "block2") * 3 = [702] rows

# the step below ('change cd verb names') seems unnecessary to me because of
levels(as.factor(cd$short_trigger)) # but the extra column turned out to be useful when working with existing plots...

# there's no need to change cd verb names to match veridicality names but it's helpful to
# copy the column "short_trigger" and name it "verb"
cd = cd %>%
  mutate(verb=recode(short_trigger, control = "MC", annoyed = "be_annoyed", be_right_that = "be_right", inform_Sam = "inform"))

# reconstruct 'eventItemNr' using 'prior_fact'
cd = cd %>%
  mutate(eventItemNr = ifelse(prior_fact == "Charley lives in Mexico" | prior_fact == "Charley lives in Korea", "20: Charley speaks Spanish",
                        ifelse(prior_fact == "Danny is a diabetic" | prior_fact == "Danny loves cake", "11: Danny ate the last cupcake",
                        ifelse(prior_fact == "Emily has been saving for a year" | prior_fact == "Emily never has any money", "8: Emily bought a car yesterday",
                        ifelse(prior_fact == "Emma is in first grade" | prior_fact == "Emma is in law school", "3: Emma studied on Saturday morning",
                        ifelse(prior_fact == "Frank has always wanted a pet" | prior_fact == "Frank is allergic to cats", "12: Frank got a cat",
                        ifelse(prior_fact == "Grace hates her sister" | prior_fact == "Grace loves her sister", "9: Grace visited her sister",
                        ifelse(prior_fact == "Isabella is a vegetarian" | prior_fact == "Isabella is from Argentina", "7: Isabella ate a steak on Sunday",
                        ifelse(prior_fact == "Jackson is obese" | prior_fact == "Jackson is training for a marathon", "13: Jackson ran 10 miles",
                        ifelse(prior_fact == "Jayden's car is in the shop" | prior_fact == "Jayden doesn't have a driver's license", "14: Jayden rented a car",
                        ifelse(prior_fact == "Jon lives 10 miles away from work" | prior_fact == "Jon lives 2 blocks away from work", "19: Jon walks to work",
                        ifelse(prior_fact == "Josh is a 5-year old boy" | prior_fact == "Josh is a 75-year old man", "16: Josh learned to ride a bike yesterday",
                        ifelse(prior_fact == "Josie doesn't have a passport" | prior_fact == "Josie loves France", "2: Josie went on vacation to France",
                        ifelse(prior_fact == "Julian is Cuban" | prior_fact == "Julian is German", "18: Julian dances salsa",
                        ifelse(prior_fact == "Mary is a middle school student" | prior_fact == "Mary is taking a prenatal yoga class", "1: Mary is pregnant",
                        ifelse(prior_fact == "Mia is a college student" | prior_fact == "Mia is a nun", "6: Mia drank 2 cocktails last night",
                        ifelse(prior_fact == "Olivia has two small children" | prior_fact == "Olivia works the third shift", "4: Olivia sleeps until noon",
                        ifelse(prior_fact == "Owen lives in Chicago" | prior_fact == "Owen lives in New Orleans", "17: Owen shoveled snow last winter",
                        ifelse(prior_fact == "Sophia is a high end fashion model" | prior_fact == "Sophia is a hipster", "5: Sophia got a tattoo",
                        ifelse(prior_fact == "Tony has been sober for 20 years" | prior_fact == "Tony really likes to party with his friends", "15: Tony had a drink last night",
                        ifelse(prior_fact == "Zoe is 5 years old" | prior_fact == "Zoe is a math major", "10: Zoe calculated the tip",
                                       NA)))))))))))))))))))))

# color-blind-friendly palette
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") # c("#999999",


#### plot ratings by CC (PDF called target-ratings) ----
# to see whether prior manipulation worked for each of the 20 contents
# also for comparison to data presented at XPRAG 2019

means = cd %>%
  group_by(prior_type,prior_fact,eventItemNr) %>%
  summarise(prior=mean(prior),CILow=ci.low(prior),CIHigh=ci.high(prior)) %>%
  ungroup() %>%
  mutate(YMin=prior-CILow,YMax=prior+CIHigh)
  
ggplot(cd, aes(x=eventItemNr,y=prior,color=prior_type)) +
  geom_jitter(aes(colour = prior_type),alpha=.1) +
  geom_errorbar(data = means, aes(ymin=YMin, ymax=YMax)) +
  geom_point(data = means, size = 3) +
  scale_y_continuous(breaks = seq(0,1,by = .2)) +
  #geom_text(aes(label=workerid), vjust = 1, cex= 5)+  # labels data points with workerid
  scale_color_manual(values=c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
  theme(axis.text.x = element_text(size = 12, angle = 75, hjust = 1)) +
  theme(axis.title=element_text(size=14)) +
  theme(legend.position="none") +
  ylab("Likeliness rating") +
  xlab("Event") 
ggsave(f="../graphs/target-ratings.pdf",height=8,width=10)


#### plot ratings by CC (PDF called ratings-for-CCs) ----
ggplot(means, aes(x=eventItemNr, y=prior, color=prior_type)) + 
  geom_point() +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  scale_y_continuous(limits = c(-0.05,1.05),breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  scale_color_manual(name="Fact", breaks=c("high","low"),labels=c("high", "low"), 
                     values=cbPalette) +
  theme(legend.position = "top") +
  theme(axis.text.x = element_text(size = 12, angle = 75, hjust = 1)) +
  theme(axis.title=element_text(size=14)) +
  ylab("Mean prior probability") +
  xlab("Content of complement") 
ggsave(f="../graphs/ratings-for-CCs.pdf",height=8,width=10)

#### plot projection by predicate and prior_type (PDF called means-projectivity-by-predicate-and-prior) ----
# to see how projection of the CC of predicates is influenced by the prior manipulation
# for comparison with XPRAG 2019 talk 

# mean projectivity by predicate, with main clause controls
proj.means = cd %>%
  group_by(short_trigger,prior_type) %>%
  summarize(Mean = mean(projective), CILow = ci.low(projective), CIHigh = ci.high(projective)) %>%
  ungroup() %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, verb = fct_reorder(as.factor(short_trigger),Mean))  # the levels of "short_trigger" are reordered by their mean projectivity rating
proj.means

# define colors for the predicates
cols = data.frame(V=levels(proj.means$verb))

cols$VeridicalityGroup = as.factor(
  ifelse(cols$V %in% c("know", "discover", "reveal", "see", "be_annoyed"), "F", 
         ifelse(cols$V %in% c("pretend", "think", "suggest", "say"), "NF", 
                ifelse(cols$V %in% c("be_right","demonstrate"),"VNF",
                       ifelse(cols$V %in% c("MC"),"MC","V")))))

levels(cols$V)
cols$V <- factor(cols$V, levels = cols[order(as.character(proj.means$verb)),]$V, ordered = TRUE)

cols$Colors =  ifelse(cols$VeridicalityGroup == "F", "darkorchid", 
                      ifelse(cols$VeridicalityGroup == "NF", "gray60", 
                             ifelse(cols$VeridicalityGroup == "VNF","dodgerblue",
                                    ifelse(cols$VeridicalityGroup == "MC","black","tomato1"))))

cols$Colors
cols$V <- factor(cols$V, levels = cols[order(as.character(proj.means$verb)),]$V, ordered = TRUE)
levels(cols$V)

proj.means$VeridicalityGroup = as.factor(
  ifelse(proj.means$verb %in% c("know", "discover", "reveal", "see", "be_annoyed"), "F", 
         ifelse(proj.means$verb  %in% c("pretend", "think", "suggest", "say"), "NF", 
                ifelse(proj.means$verb  %in% c("be_right","demonstrate"),"VNF",
                       ifelse(proj.means$verb  %in% c("MC"),"MC","V")))))

# plot of means, 95% bootstrapped CIs and participants' ratings
ggplot(proj.means, aes(x=verb, y=Mean, fill=prior_type)) + 
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0) +
  geom_point(pch = 21, colour = "black", size = 3) +
  scale_y_continuous(limits = c(-0.05,1.05),breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  scale_fill_manual(name="Prior probability of content", breaks=c("high_prior","low_prior"),labels=c("high", "low"), 
                    values=cbPalette) +
  scale_color_manual(name="Prior probability of content", breaks=c("high_prior","low_prior"),labels=c("high", "low"), 
                     values=cbPalette) +  
  scale_alpha(range = c(.3,1)) +
  theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 45, hjust = 1, 
                                                                 color=cols$Colors)) +
  theme(legend.position = "top") +
  geom_errorbar(aes(x=4,ymin=proj.means[proj.means$verb == "MC",]$YMin,ymax=proj.means[proj.means$verb == "MC",]$YMax,width=.25),color="black",width=0) +  # set x to the position of MC
  geom_point(aes(x=4,y=proj.means[proj.means$verb == "MC",]$Mean), color="black",show.legend = FALSE ) +  # set x to the position of MC
  ylab("Mean certainty rating") +
  xlab("Predicate") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1))
ggsave("../graphs/means-projectivity-by-predicate-and-prior.pdf",height=4,width=7)

#### plot at-issueness by predicate and prior_type (PDF called means-projectivity-by-predicate-and-prior) ----
# to see how at-issueness of the CC of predicates is influenced by the prior manipulation

# mean projectivity by predicate, with main clause controls
ai.means = cd %>%
  group_by(short_trigger,prior_type) %>%
  summarize(Mean = mean(ai), CILow = ci.low(ai), CIHigh = ci.high(ai)) %>%
  ungroup() %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, verb = fct_reorder(as.factor(short_trigger),Mean))  
ai.means

# define colors for the predicates
cols = data.frame(V=levels(ai.means$verb))

cols$VeridicalityGroup = as.factor(
  ifelse(cols$V %in% c("know", "discover", "reveal", "see", "be_annoyed"), "F", 
         ifelse(cols$V %in% c("pretend", "think", "suggest", "say"), "NF", 
                ifelse(cols$V %in% c("be_right","demonstrate"),"VNF",
                       ifelse(cols$V %in% c("MC"),"MC","V")))))

levels(cols$V)
cols$V <- factor(cols$V, levels = cols[order(as.character(ai.means$verb)),]$V, ordered = TRUE)

cols$Colors =  ifelse(cols$VeridicalityGroup == "F", "darkorchid", 
                      ifelse(cols$VeridicalityGroup == "NF", "gray60", 
                             ifelse(cols$VeridicalityGroup == "VNF","dodgerblue",
                                    ifelse(cols$VeridicalityGroup == "MC","black","tomato1"))))

cols$Colors
cols$V <- factor(cols$V, levels = cols[order(as.character(ai.means$verb)),]$V, ordered = TRUE)
levels(cols$V)

ai.means$VeridicalityGroup = as.factor(
  ifelse(ai.means$verb %in% c("know", "discover", "reveal", "see", "be_annoyed"), "F", 
         ifelse(ai.means$verb  %in% c("pretend", "think", "suggest", "say"), "NF", 
                ifelse(ai.means$verb  %in% c("be_right","demonstrate"),"VNF",
                       ifelse(ai.means$verb  %in% c("MC"),"MC","V")))))

# plot of means, 95% bootstrapped CIs and participants' ratings
ggplot(ai.means, aes(x=verb, y=Mean, fill=prior_type)) + 
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0) +
  geom_point(pch = 21, colour = "black", size = 3) +
  scale_y_continuous(limits = c(-0.05,1.05),breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  scale_fill_manual(name="Prior probability of content", breaks=c("high_prior","low_prior"),labels=c("high", "low"), 
                    values=cbPalette) +
  scale_color_manual(name="Prior probability of content", breaks=c("high_prior","low_prior"),labels=c("high", "low"), 
                     values=cbPalette) +  
  scale_alpha(range = c(.3,1)) +
  theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 45, hjust = 1, 
                                                                 color=cols$Colors)) +
  theme(legend.position = "top") +
  geom_errorbar(aes(x=4,ymin=proj.means[proj.means$verb == "MC",]$YMin,ymax=proj.means[proj.means$verb == "MC",]$YMax,width=.25),color="black",width=0) +  # set x to the position of MC
  geom_point(aes(x=4,y=proj.means[proj.means$verb == "MC",]$Mean), color="black",show.legend = FALSE ) +  # set x to the position of MC
  ylab("Mean at-issueness rating") +
  xlab("Predicate") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1))
ggsave("../graphs/means-at-issueness-by-predicate-and-prior.pdf",height=4,width=7)


### plot mean at-issueness ratings against mean projectivity ratings by prior (4-way distinction) ----

ai.means
proj.means

# combine at-issueness and projection means
tmp.ai = ai.means %>%
  rename(AIMean="Mean",AIYMin="YMin",AIYMax="YMax",VG = "VeridicalityGroup") %>%
  select(-verb)
tmp.ai

tmp.proj = proj.means %>%
  rename(ProjMean="Mean",ProjYMin="YMin",ProjYMax="YMax") %>%
  select(-verb)
tmp.proj


toplot = tmp.ai %>%
  left_join(tmp.proj, by=c("short_trigger","prior_type")) %>%
  mutate(prior_type = replace_na(prior_type, "")) %>%
  mutate(prior_type=recode(prior_type,low_prior="L",high_prior="H")) 
# unite("verb_prior",short_trigger,prior) %>%
# mutate(verb_prior = recode(verb_prior,MC_ = "MC"))

summary(toplot)
toplot

# toplot already has VeridicalityGroup, just need to define colors
# cols = data.frame(V=levels(as.factor(toplot$verb_prior)))
cols = data.frame(V=levels(as.factor(toplot$short_trigger)))
cols

cols$VeridicalityGroup = as.factor(
  ifelse(cols$V %in% c("know", "discover", "reveal", "see", "be_annoyed","know", "discover", "reveal", "see", "be_annoyed"), "F", 
         ifelse(cols$V %in% c("pretend", "think", "suggest", "say","pretend", "think", "suggest", "say"), "NF", 
                ifelse(cols$V %in% c("be_right","demonstrate","be_right","demonstrate"),"VNF",
                       ifelse(cols$V %in% c("MC"),"MC","V")))))

levels(cols$V)
# cols$V <- factor(cols$V, levels = cols[order(as.character(proj.means$verb)),]$V, ordered = TRUE)

cols$Colors =  ifelse(cols$VeridicalityGroup == "F", "darkorchid", 
                      ifelse(cols$VeridicalityGroup == "NF", "gray60", 
                             ifelse(cols$VeridicalityGroup == "VNF","dodgerblue",
                                    ifelse(cols$VeridicalityGroup == "MC","black","tomato1"))))


cols$Colors
# cols$V <- factor(cols$V, levels = cols[order(as.character(proj.means$verb)),]$V, ordered = TRUE)

# remove black for MC
cols2 <- droplevels(subset(cols,cols$V != "MC"))
cols2$Colors

levels(cols$V)


toplot[toplot$short_trigger != "MC",]$short_trigger

# toplot$short_trigger <- factor(toplot$short_trigger, levels = toplot[order(as.character(proj.means$verb)),]$short_trigger, ordered = TRUE)
# levels(toplot$short_trigger)

#fill_cols = c("darkorchid","black","gray60","tomato1","dodgerblue","black")
fill_cols = c("darkorchid","gray60","tomato1","dodgerblue")

ggplot(toplot[toplot$short_trigger != "MC",], aes(x=AIMean,y=ProjMean,fill=VeridicalityGroup)) +
  geom_point(shape=21,stroke=.5,size=2.5,color="black") +
  facet_wrap(~prior_type) +
  scale_fill_manual(values=fill_cols) +
  geom_abline(intercept=0,slope=1,linetype="dashed",color="gray60") +
  geom_errorbar(aes(ymin=ProjYMin,ymax=ProjYMax)) +
  geom_errorbarh(aes(xmin=AIYMin,xmax=AIYMax)) +
  guides(fill=FALSE) +
  geom_text_repel(aes(label=short_trigger),color=rep(cols2$Colors,each=2),alpha=1,size=4) +
  #geom_text_repel(aes(label=short_trigger),color=rep(cols$Colors,each=2),alpha=1,size=4) +
  ylab("Mean projectivity rating") +
  xlab("Mean not-at-issueness rating") 
ggsave("../graphs/mean-projectivity-by-at-issueness-and-prior.pdf",height=5,width=10)


#### plot projection by predicate and prior_type and block order (PDF called means-projectivity-by-predicate-and-prior-block) ----
# to see how projection of the CC of predicates is influenced by the prior manipulation
# for comparison with XPRAG 2019 talk 

# mean projectivity by predicate, with main clause controls
proj.means = cd %>%
  group_by(short_trigger,prior_type,block_ai) %>%
  summarize(Mean = mean(projective), CILow = ci.low(projective), CIHigh = ci.high(projective)) %>%
  ungroup() %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, verb = fct_reorder(as.factor(short_trigger),Mean))  # the levels of "short_trigger" are reordered by their mean projectivity rating
proj.means

# define colors for the predicates
cols = data.frame(V=levels(proj.means$verb))

cols$VeridicalityGroup = as.factor(
  ifelse(cols$V %in% c("know", "discover", "reveal", "see", "be_annoyed"), "F", 
         ifelse(cols$V %in% c("pretend", "think", "suggest", "say"), "NF", 
                ifelse(cols$V %in% c("be_right","demonstrate"),"VNF",
                       ifelse(cols$V %in% c("MC"),"MC","V")))))

levels(cols$V)
cols$V <- factor(cols$V, levels = cols[order(as.character(proj.means$verb)),]$V, ordered = TRUE)

cols$Colors =  ifelse(cols$VeridicalityGroup == "F", "darkorchid", 
                      ifelse(cols$VeridicalityGroup == "NF", "gray60", 
                             ifelse(cols$VeridicalityGroup == "VNF","dodgerblue",
                                    ifelse(cols$VeridicalityGroup == "MC","black","tomato1"))))

cols$Colors
cols$V <- factor(cols$V, levels = cols[order(as.character(proj.means$verb)),]$V, ordered = TRUE)
levels(cols$V)

proj.means$VeridicalityGroup = as.factor(
  ifelse(proj.means$verb %in% c("know", "discover", "reveal", "see", "be_annoyed"), "F", 
         ifelse(proj.means$verb  %in% c("pretend", "think", "suggest", "say"), "NF", 
                ifelse(proj.means$verb  %in% c("be_right","demonstrate"),"VNF",
                       ifelse(proj.means$verb  %in% c("MC"),"MC","V")))))

# plot of means, 95% bootstrapped CIs and participants' ratings
ggplot(proj.means, aes(x=verb, y=Mean, fill=prior_type,alpha=block_ai)) + 
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0) +
  geom_point(pch = 21, colour = "black", size = 3) +
  scale_y_continuous(limits = c(-0.05,1.05),breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  scale_fill_manual(name="Prior probability of content", breaks=c("high_prior","low_prior"),labels=c("high", "low"), 
                    values=cbPalette) +
  scale_color_manual(name="Prior probability of content", breaks=c("high_prior","low_prior"),labels=c("high", "low"), 
                     values=cbPalette) +  
  scale_alpha_discrete(range = c(.4,1)) +
  # geom_errorbar(aes(x=4,ymin=proj.means[proj.means$verb == "MC",]$YMin,ymax=proj.means[proj.means$verb == "MC",]$YMax,width=.25),color="black",width=0) +  # set x to the position of MC
  # geom_point(aes(x=4,y=proj.means[proj.means$verb == "MC",]$Mean), color="black",show.legend = FALSE ) +  # set x to the position of MC
  ylab("Mean certainty rating") +
  xlab("Predicate") +
  geom_hline(yintercept=.5,color="gray60",linetype="dashed") +
  # facet_wrap(~block_ai)  +
  theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 45, hjust = 1, 
                                                                 color=cols$Colors)) +
  theme(legend.position = "top") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1))
ggsave("../graphs/means-projectivity-by-predicate-and-prior-block.pdf",height=4,width=8)

#### plot at-issueness by predicate and prior_type (PDF called means-projectivity-by-predicate-and-prior) ----
# to see how at-issueness of the CC of predicates is influenced by the prior manipulation

# mean projectivity by predicate, with main clause controls
ai.means = cd %>%
  group_by(short_trigger,prior_type,block_ai) %>%
  summarize(Mean = mean(ai), CILow = ci.low(ai), CIHigh = ci.high(ai)) %>%
  ungroup() %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, verb = fct_reorder(as.factor(short_trigger),Mean))  
ai.means

# define colors for the predicates
cols = data.frame(V=levels(ai.means$verb))

cols$VeridicalityGroup = as.factor(
  ifelse(cols$V %in% c("know", "discover", "reveal", "see", "be_annoyed"), "F", 
         ifelse(cols$V %in% c("pretend", "think", "suggest", "say"), "NF", 
                ifelse(cols$V %in% c("be_right","demonstrate"),"VNF",
                       ifelse(cols$V %in% c("MC"),"MC","V")))))

levels(cols$V)
cols$V <- factor(cols$V, levels = cols[order(as.character(ai.means$verb)),]$V, ordered = TRUE)

cols$Colors =  ifelse(cols$VeridicalityGroup == "F", "darkorchid", 
                      ifelse(cols$VeridicalityGroup == "NF", "gray60", 
                             ifelse(cols$VeridicalityGroup == "VNF","dodgerblue",
                                    ifelse(cols$VeridicalityGroup == "MC","black","tomato1"))))

cols$Colors
cols$V <- factor(cols$V, levels = cols[order(as.character(ai.means$verb)),]$V, ordered = TRUE)
levels(cols$V)

ai.means$VeridicalityGroup = as.factor(
  ifelse(ai.means$verb %in% c("know", "discover", "reveal", "see", "be_annoyed"), "F", 
         ifelse(ai.means$verb  %in% c("pretend", "think", "suggest", "say"), "NF", 
                ifelse(ai.means$verb  %in% c("be_right","demonstrate"),"VNF",
                       ifelse(ai.means$verb  %in% c("MC"),"MC","V")))))

# plot of means, 95% bootstrapped CIs and participants' ratings
ggplot(ai.means, aes(x=verb, y=Mean, fill=prior_type)) + 
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0) +
  geom_point(pch = 21, colour = "black", size = 3) +
  scale_y_continuous(limits = c(-0.05,1.05),breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  scale_fill_manual(name="Prior probability of content", breaks=c("high_prior","low_prior"),labels=c("high", "low"), 
                    values=cbPalette) +
  scale_color_manual(name="Prior probability of content", breaks=c("high_prior","low_prior"),labels=c("high", "low"), 
                     values=cbPalette) +  
  scale_alpha(range = c(.3,1)) +
  # geom_errorbar(aes(x=4,ymin=proj.means[proj.means$verb == "MC",]$YMin,ymax=proj.means[proj.means$verb == "MC",]$YMax,width=.25),color="black",width=0) +  # set x to the position of MC
  # geom_point(aes(x=4,y=proj.means[proj.means$verb == "MC",]$Mean), color="black",show.legend = FALSE ) +  # set x to the position of MC
  ylab("Mean at-issueness rating") +
  xlab("Predicate") +
  facet_wrap(~block_ai) +
  theme(legend.position = "top") +
  theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 45, hjust = 1, 
                                                                 color=cols$Colors)) +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1))
ggsave("../graphs/means-at-issueness-by-predicate-and-prior-block.pdf",height=4,width=9)


### plot mean at-issueness ratings against mean projectivity ratings by prior (4-way distinction) ----

ai.means
proj.means

# combine at-issueness and projection means
tmp.ai = ai.means %>%
  rename(AIMean="Mean",AIYMin="YMin",AIYMax="YMax",VG = "VeridicalityGroup") %>%
  select(-verb)
tmp.ai

tmp.proj = proj.means %>%
  rename(ProjMean="Mean",ProjYMin="YMin",ProjYMax="YMax") %>%
  select(-verb)
tmp.proj


toplot = tmp.ai %>%
  left_join(tmp.proj, by=c("short_trigger","prior_type","block_ai")) %>%
  mutate(prior_type = replace_na(prior_type, "")) %>%
  mutate(block_ai = replace_na(block_ai, "")) %>%
  mutate(prior_type=recode(prior_type,low_prior="L",high_prior="H")) 
# unite("verb_prior",short_trigger,prior) %>%
# mutate(verb_prior = recode(verb_prior,MC_ = "MC"))

summary(toplot)
toplot

# toplot already has VeridicalityGroup, just need to define colors
# cols = data.frame(V=levels(as.factor(toplot$verb_prior)))
cols = data.frame(V=levels(as.factor(toplot$short_trigger)))
cols

cols$VeridicalityGroup = as.factor(
  ifelse(cols$V %in% c("know", "discover", "reveal", "see", "be_annoyed","know", "discover", "reveal", "see", "be_annoyed"), "F", 
         ifelse(cols$V %in% c("pretend", "think", "suggest", "say","pretend", "think", "suggest", "say"), "NF", 
                ifelse(cols$V %in% c("be_right","demonstrate","be_right","demonstrate"),"VNF",
                       ifelse(cols$V %in% c("MC"),"MC","V")))))

levels(cols$V)
# cols$V <- factor(cols$V, levels = cols[order(as.character(proj.means$verb)),]$V, ordered = TRUE)

cols$Colors =  ifelse(cols$VeridicalityGroup == "F", "darkorchid", 
                      ifelse(cols$VeridicalityGroup == "NF", "gray60", 
                             ifelse(cols$VeridicalityGroup == "VNF","dodgerblue",
                                    ifelse(cols$VeridicalityGroup == "MC","black","tomato1"))))


cols$Colors
# cols$V <- factor(cols$V, levels = cols[order(as.character(proj.means$verb)),]$V, ordered = TRUE)

# remove black for MC
cols2 <- droplevels(subset(cols,cols$V != "MC"))
cols2$Colors

levels(cols$V)


toplot[toplot$short_trigger != "MC",]$short_trigger

# toplot$short_trigger <- factor(toplot$short_trigger, levels = toplot[order(as.character(proj.means$verb)),]$short_trigger, ordered = TRUE)
# levels(toplot$short_trigger)

#fill_cols = c("darkorchid","black","gray60","tomato1","dodgerblue","black")
fill_cols = c("darkorchid","gray60","tomato1","dodgerblue")

ggplot(toplot[toplot$short_trigger != "MC",], aes(x=AIMean,y=ProjMean,fill=VeridicalityGroup)) +
  geom_point(shape=21,stroke=.5,size=2.5,color="black") +
  facet_grid(block_ai~prior_type) +
  scale_fill_manual(values=fill_cols) +
  geom_abline(intercept=0,slope=1,linetype="dashed",color="gray60") +
  geom_errorbar(aes(ymin=ProjYMin,ymax=ProjYMax)) +
  geom_errorbarh(aes(xmin=AIYMin,xmax=AIYMax)) +
  guides(fill=FALSE) +
  geom_text_repel(aes(label=short_trigger),color=rep(cols2$Colors,each=4),alpha=1,size=4) +
  #geom_text_repel(aes(label=short_trigger),color=rep(cols$Colors,each=2),alpha=1,size=4) +
  ylab("Mean projectivity rating") +
  xlab("Mean not-at-issueness rating") 
ggsave("../graphs/mean-projectivity-by-at-issueness-and-prior-block.pdf",height=7,width=8)





# trying a plot in which both effect of ai and prior are better visualized
toplot
toplot$alpha <- ifelse(toplot$prior_type == "H", 0.9, 0.35)

ggplot(toplot[toplot$short_trigger != "MC",], aes(x=AIMean,y=ProjMean,fill=VeridicalityGroup,alpha=prior_type)) +
  geom_errorbar(aes(ymin=ProjYMin,ymax=ProjYMax,alpha = prior_type)) +
  geom_errorbarh(aes(xmin=AIYMin,xmax=AIYMax,alpha = prior_type)) +
  scale_alpha_manual(values = c(1, 0.5), guide = FALSE) +
  geom_point(shape=21,stroke=.5,size=2.5,color="black") +
  geom_line(aes(group = short_trigger)) +
  scale_fill_manual(values=fill_cols) +
  geom_abline(intercept=0,slope=1,linetype="dashed",color="gray60") +
  guides(fill=FALSE) +
  theme(legend.position = "none") +
  geom_text_repel(aes(label=short_trigger),color=rep(cols2$Colors),alpha=1,size=4,
                  data = toplot[toplot$prior_type == "L",]) +
  #geom_text_repel(aes(label=short_trigger),color=rep(cols2$Colors,each=2),alpha=1,size=4) +
  ylab("Mean projectivity rating") +
  xlab("Mean not-at-issueness rating") 
ggsave("../graphs/mean-projectivity-by-at-issueness-and-prior2.pdf",height=5,width=5)


#### plot projectivity by prior probability on a by-participant level ----
# these graphs are relevant for Lehnhardt's thesis, which explores the hypothesis that some projection variability
# is due to by-participant in prior content probability

# IMPORTANT: order levels of "verb" to make them fit the veridicality colors
cd$verb <- factor(cd$verb, levels = cols[order(as.character(proj.means$verb)),]$V, ordered = TRUE)
cd$verb

# remove main clauses since they are not relevant here
target <- droplevels(subset(cd,cd$verb != "MC"))
nrow(target)

# relevel for color coding
target$verb <- factor(target$verb, levels = cols[order(as.character(proj.means$verb)),]$V, ordered = TRUE)
target$verb

# no facet wrap, veridicality colors
ggplot(target, aes(x=prior, y=projective, color=verb)) +
  # geom_abline(intercept=0, slope=1, linetype="dashed", color="gray50") +
  geom_smooth(method="lm", color="black") +
  geom_point(pch=20, size=3, show.legend = FALSE) +
  scale_color_manual(breaks=c("F","NF","V","VNF"), labels=c("factive","non-factive","V","VNF"), values=cols$Colors) +
  xlab("Prior probability rating") +
  ylab("Certainty rating") +
  xlim(0,1) +
  ylim(0,1) 
ggsave(f="../graphs/projectivity-by-prior_color-veridicality.pdf",height=8,width=8)

# no facet wrap, individual color for each verb
ggplot(target, aes(x=prior, y=projective, color=verb)) +
  # geom_abline(intercept=0,slope=1,linetype="dashed",color="gray50") +
  geom_smooth(method="lm",color="black") +
  geom_point(pch=20, size=3) +
  theme(legend.position = "right") +
  xlab("Prior probability rating") +
  ylab("Certainty rating") +
  xlim(0,1) +
  ylim(0,1) 
ggsave(f="../graphs/projectivity-by-prior_color.pdf",height=8,width=11)

# facet wrap: worker ID, color veridicality
ggplot(target, aes(x=prior, y=projective, color=verb)) +
  # geom_abline(intercept=0,slope=1,linetype="dashed",color="gray50") +
  geom_smooth(method="lm", colour="black") +
  geom_point(pch=20, size=3, show.legend = FALSE) +
  scale_color_manual(breaks=c("F","NF","V","VNF"), labels=c("factive","non-factive","V","VNF"), values=cols$Colors) +
  xlab("Prior probability rating") +
  ylab("Certainty rating") +
  xlim(0,1) +
  ylim(0,1) +
  facet_wrap(~workerid)
ggsave(f="../graphs/projectivity-by-prior_facet-wrap-workerid_color-veridicality.pdf",height=8,width=8)

# facet wrap: worker ID, individual color for each verb
ggplot(target, aes(x=prior, y=projective, color=verb)) +
  # geom_abline(intercept=0,slope=1,linetype="dashed",color="gray50") +
  geom_smooth(method="lm",colour="black") +
  geom_point(pch=20, size=3) +
  xlab("Prior probability rating") +
  ylab("Certainty rating") +
  xlim(0,1) +
  ylim(0,1) +
  facet_wrap(~workerid)
ggsave(f="../graphs/projectivity-by-prior_facet-wrap-workerid_color.pdf",height=8,width=10)

# facet wrap: CC
ggplot(target, aes(x=prior,y=projective)) +
  # geom_abline(intercept=0,slope=1,linetype="dashed",color="gray50") +
  geom_smooth(method="lm",colour="black") +
  geom_point(size=1) +
  xlab("Prior probability rating") +
  ylab("Certainty rating") +
  xlim(0,1) +
  ylim(0,1) +
  facet_wrap(~eventItemNr)
ggsave(f="../graphs/projectivity-by-prior_facet-wrap-CC.pdf",height=8,width=10)

# facet wrap: verb
ggplot(target, aes(x=prior, y=projective, fill=verb)) +
  # geom_abline(intercept=0,slope=1,linetype="dashed",color="gray50") +
  geom_smooth(method="lm",colour="black",show.legend = FALSE) +
  geom_point(pch=21, size=2, show.legend = FALSE) +
  scale_fill_manual(breaks=c("F","NF","V","VNF"), values=cols$Colors) +
  #scale_color_manual(breaks=c("F","MC","NF","V","VNF"), values=cols$Colors) +  
  xlab("Prior probability rating") +
  ylab("Certainty rating") +
  xlim(0,1) +
  ylim(0,1) +
  facet_wrap(~verb)
ggsave(f="../graphs/projectivity-by-prior_facet-wrap-verb.pdf",height=8,width=10)

#### save "PriorMean" to data folder ----
#means = cd.event %>%
#  group_by(eventItemNr,prior_type,prior_fact) %>%
#  summarize(PriorMean = mean(prior)) %>%
#  ungroup() #%>%
#  mutate(itemType = paste("fact",itemType,sep=""))
#write.csv(means,file="../data/prior_means.csv",row.names=F,quote=F)
