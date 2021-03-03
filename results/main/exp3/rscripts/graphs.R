# interaction of prior beliefs, at-issueness and projection
# for 20 clause-embedding predicates
# graphs.R

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

d = read_csv("../data/cd.csv")
nrow(d) #39390 / 78 trials = 505 Turkers

summary(d)
table(d$prior) # half is high_prior, half is low_prior

# prepare for spreading:
# (1) rename the "prior" column into "prior_type" and
colnames(d)[colnames(d)=="prior"] = "prior_type"
# (2) fill in (arbitrarily) 'high_prior' for the "prior_type" of main clauses (MC)
d[d$short_trigger == "MC",]$prior_type <- "main_clause"

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
# 6474    6656

nrow(cd)

# fix predicate names
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

# target data
t = cd %>% 
  filter(short_trigger != "MC") %>% 
  droplevels()
nrow(t) #10100 / 505 Turkers = 20 rows (one for each predicate, each with prior, proj, ai)

# prior ratings by content (no main clause content) ----
means = t %>%
  group_by(prior_type,eventItemNr) %>%
  summarise(Mean=mean(prior),CILow=ci.low(prior),CIHigh=ci.high(prior)) %>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)
means

# save for comparison with Exp2 findings
write.csv(means,file="../data/prior_means.csv",row.names=F,quote=F)

names(means)
table(means$prior_type)

high = means %>%
  filter(prior_type == "high_prior") %>%
  mutate(eventItem = fct_reorder(eventItemNr,Mean))

means = means %>%
  mutate(eventItemNr = fct_relevel(eventItemNr,levels(high$eventItemNr))) %>% 
  mutate(prior_type = fct_relevel(prior_type,"low_prior"))
means

subjmeans = t %>%
  group_by(eventItemNr,workerid,prior_type) %>%
  summarize(Mean = mean(prior)) %>% 
  ungroup() %>% 
  mutate(prior_type = fct_relevel(as.factor(as.character(prior_type)),"low_prior"))
subjmeans$eventItemNr <- factor(subjmeans$eventItemNr, levels = unique(levels(means$eventItemNr)))
nrow(subjmeans)
levels(subjmeans$eventItemNr)
names(subjmeans)

ggplot(means, aes(x=eventItemNr, y=Mean, color=prior_type,shape=prior_type,fill=prior_type)) + 
  geom_point(data=subjmeans,aes(fill=prior_type,color=prior_type),shape=21,alpha=.05) +
  geom_point(stroke=.5,size=3,color="black") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  scale_shape_manual(values=c(25, 24),labels=c("lower probability","higher probability"),name="Fact") +
  scale_fill_manual(values=c("#56B4E9","#E69F00"),labels=c("lower probability","higher probability"),name="Fact") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,.2,.4,.6,.8,1), labels=c("0", ".2", ".4", ".6", ".8", "1")) +
  scale_color_manual(name="Fact",labels=c("lower probability", "higher probability"), values=c("#56B4E9","#E69F00")) +
  coord_flip() +
  theme(legend.position = "top", legend.text=element_text(size=12)) +
  # theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 75, hjust = 1)) +
  ylab("Mean prior probability rating") +
  xlab("Content") 
ggsave(f="../graphs/prior-ratings.pdf",height=5,width=8)


# plot projection by prior type collapsing over predicate (with main clause content) ----
nrow(cd)
table(cd$prior_type)

# mean projectivity by predicate and prior type, with main clause controls
proj.means = cd %>%
  group_by(short_trigger,prior_type) %>%
  summarize(Mean = mean(projective), CILow = ci.low(projective), CIHigh = ci.high(projective)) %>%
  ungroup() %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, verb = fct_reorder(as.factor(short_trigger),Mean))  
proj.means

# order predicates by high_prior
high = proj.means %>%
  filter(prior_type != "low_prior") %>%
  mutate(short_trigger = fct_reorder(short_trigger,Mean))
high
levels(high$short_trigger)

proj.means = proj.means %>%
  mutate(short_trigger = fct_relevel(short_trigger,levels(high$short_trigger)))
levels(proj.means$short_trigger)

# change factor levels for prior_type for plotting
proj.means = proj.means %>%
  mutate(prior_type = fct_relevel(prior_type, "main_clause", "low_prior", "high_prior"))
levels(proj.means$prior_type)

# to plot MC in different color and shape, copy MC data to new data frame and 
# remove MC data, but not factor level, from proj.means
mc.data = droplevels(subset(proj.means, proj.means$verb == "MC"))
mc.data
#View(mc.data)

proj.means[proj.means$short_trigger == "MC",]$Mean <- NA
proj.means[proj.means$short_trigger == "MC",]$YMin <- NA
proj.means[proj.means$short_trigger == "MC",]$YMax <- NA

# to add participants' ratings
subjmeans = cd %>%
  group_by(workerid,short_trigger,prior_type) %>%
  summarize(Mean = mean(projective))
subjmeans$short_trigger <- factor(subjmeans$short_trigger, levels = unique(levels(proj.means$short_trigger)))
levels(subjmeans$short_trigger)
subjmeans
subjmeans$prior_type <- as.factor(subjmeans$prior_type)

# change factor levels for prior_type for plotting
subjmeans = subjmeans %>%
  mutate(prior_type = fct_relevel(prior_type, "main_clause", "low_prior", "high_prior"))
levels(subjmeans$prior_type)

levels(proj.means$prior_type)
# [1] "main_clause"
# [2] "low_prior"  
# [3] "high_prior" 

ggplot(proj.means, aes(x=short_trigger, y=Mean, color=prior_type,fill=prior_type,shape=prior_type)) + 
  theme(legend.position = "top", legend.text=element_text(size=12)) +
  geom_point(data=subjmeans,aes(fill=prior_type,color=prior_type),shape=21,alpha=.08) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  geom_point(size = 3,color="black") +
  scale_shape_manual(values=rev(c(24, 25, 21)),labels=rev(c("higher probability","lower probability","main clause")),name="Fact") +
  scale_fill_manual(values=rev(c("#E69F00","#56B4E9","black")),labels=rev(c("higher probability","lower probability","main clause")),name="Fact") +
  scale_color_manual(values=rev(c("#E69F00","#56B4E9","black")),labels=rev(c("higher probability","lower probability","main clause")),name="Fact") +  
  scale_alpha(range = c(.3,1)) +
  scale_y_continuous(limits = c(0,1),breaks = c(0,.2,.4,.6,.8,1), labels=c("0", ".2", ".4", ".6", ".8", "1")) +
  theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 45, hjust = 1)) +
  geom_errorbar(aes(x=1,ymin=mc.data$YMin,ymax=mc.data$YMax,width=.25),color="black",width=0) +  # set x to the position of MC
  geom_point(shape=20,size=4,aes(x=1,y=mc.data$Mean),color="black",show.legend = FALSE ) +  # set x to the position of MC
  ylab("Mean certainty rating") +
  xlab("Predicate")
ggsave("../graphs/means-projectivity-by-predicate-and-prior.pdf",height=5,width=7)

#### plot projectivity by prior probability on a by-participant level (no MC content) ----

proj.means = t %>%
  group_by(short_trigger,prior_type) %>%
  summarize(Mean = mean(projective), CILow = ci.low(projective), CIHigh = ci.high(projective)) %>%
  ungroup() %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, short_trigger = fct_reorder(as.factor(short_trigger),Mean))  
proj.means
#View(proj.means)

nrow(proj.means) #40 (high_prior and low_prior for each of the 20 predicates)

high = proj.means %>%
  filter(prior_type == "high_prior") %>%
  mutate(short_trigger = fct_reorder(short_trigger,Mean))

t = t %>%
  mutate(short_trigger = fct_relevel(short_trigger,levels(high$short_trigger)))
table(t$short_trigger)

# change factor levels for prior_type for plotting
t = t %>%
  mutate(prior_type = fct_relevel(prior_type, "low_prior", "high_prior"))
levels(t$prior_type)

ggplot(t, aes(x=prior, y=projective,color=prior_type)) +
  #geom_abline(intercept=0,slope=1,linetype="dashed",color="gray50") +
  geom_smooth(method="lm",colour="grey50") +
  geom_point(shape=20, size=1, alpha=.3) +
  scale_color_manual(values=c("#56B4E9","#E69F00"),labels=c("lower probability","higher probability"),name="Fact") +
  xlab("Prior probability rating") +
  ylab("Certainty rating") +
  theme(legend.position = "top", legend.text=element_text(size=12)) +
  guides(colour = guide_legend(override.aes = list(alpha = 1,size=3))) +
  #xlim(0,1) +
  #ylim(0,1) +
  scale_x_continuous(breaks=c(0,.5,1),labels=c("0",".5","1")) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1")) +
  theme(panel.spacing.x = unit(4, "mm")) +
  coord_fixed(ratio = 1) +
  facet_wrap(~short_trigger)
ggsave(f="../graphs/projection-by-prior.pdf",height=7,width=7)

#### plot at-issueness by predicate and prior_type----

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

# to plot MC in different color and shape, copy MC data to new data frame and 
# remove MC data, but not factor level, from proj.means
mc.data = droplevels(subset(ai.means, ai.means$verb == "MC"))
mc.data
#View(mc.data)

ai.means[ai.means$short_trigger == "MC",]$Mean <- NA
ai.means[ai.means$short_trigger == "MC",]$YMin <- NA
ai.means[ai.means$short_trigger == "MC",]$YMax <- NA


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
  geom_errorbar(aes(x=4,ymin=mc.data[ai.means$verb == "MC",]$YMin,ymax=mc.data[mc.data$verb == "MC",]$YMax,width=.25),color="black",width=0) + 
  geom_point(aes(x=4,y=mc.data[mc.data$verb == "MC",]$Mean), color="black",show.legend = FALSE ) +  
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
  rename(ProjMean="Mean",ProjYMin="YMin",ProjYMax="YMax") 
  #%>%  select(-verb)
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

ggplot(toplot[toplot$short_trigger != "MC",], aes(x=AIMean,y=ProjMean)) +
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



# one plot for effect of at-issueness and prior ----
toplot
toplot$alpha <- ifelse(toplot$prior_type == "H", 0.9, 0.35)

ggplot(toplot[toplot$short_trigger != "MC",], aes(x=AIMean,y=ProjMean,alpha=prior_type)) +
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

