# graphs file for experiment investigating whether at-issueness predicts projection
# for the contents of the complements of 20 predicates


# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# load required packages
require(tidyverse)
library(ggrepel)
library(dichromat)
library(forcats)

theme_set(theme_bw())

# load helper functions
source('../../helpers.R')

d = read_csv("../data/data_preprocessed.csv")
nrow(d) #13520 / 260 Turkers = 52 trials

# spread responses over separate columns for projectivity and at-issueness
cd = d %>%
  mutate(block_ai = ifelse(question_type == "ai", ifelse(block == "block1", "block1", "block2"), ifelse(block == "block1", "block2", "block1"))) %>%
  select(workerid,content,short_trigger,question_type,response,block_ai) %>%
  spread(question_type,response)

# change cd verb names to match veridicality names
cd = cd %>%
  mutate(verb=recode(short_trigger, control = "MC", annoyed = "be_annoyed", be_right_that = "be_right", inform_Sam = "inform"))

# mean not-at-issueness by predicate, including the main clause controls
ai.means = cd %>%
  group_by(short_trigger) %>%
  summarize(Mean = mean(ai), CILow = ci.low(ai), CIHigh = ci.high(ai)) %>%
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

# by-participant ai responses for their items, including main clause controls 
ai.subjmeans = cd %>%
  group_by(short_trigger,workerid) %>%
  summarize(Mean = mean(ai)) 
ai.subjmeans$verb <- factor(ai.subjmeans$short_trigger, levels = unique(levels(ai.means$verb)))
levels(ai.subjmeans$verb)


# plot of means, 95% bootstrapped CIs and participants' ratings 
ggplot(ai.means, aes(x=verb, y=Mean, fill=VeridicalityGroup)) +
  geom_point(shape=21,fill="gray60",data=ai.subjmeans, alpha=.1, color="gray40") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0.1,color="black") +
  geom_point(shape=21,stroke=.5,size=2.5,color="black") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  scale_alpha(range = c(.3,1)) +
  scale_fill_manual(values=c("darkorchid","black","gray60","tomato1","dodgerblue")) +
  guides(fill=FALSE) +
  theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 45, hjust = 1, 
                                                                 color=cols$Colors)) +
  theme(legend.position="top") +
  ylab("Mean not-at-issueness rating") +
  xlab("Predicate") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1)) 
ggsave("../graphs/means-nai-by-predicate-variability.pdf",height=4,width=7)


# mean projectivity by predicate, including the main clause controls
proj.means = cd %>%
  group_by(short_trigger) %>%
  summarize(Mean = mean(projective), CILow = ci.low(projective), CIHigh = ci.high(projective)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, verb = fct_reorder(as.factor(short_trigger),Mean))
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

# by-participant projectino responses for their items, including main clause controls 
proj.subjmeans = cd %>%
  group_by(short_trigger,workerid) %>%
  summarize(Mean = mean(projective)) 
proj.subjmeans$verb <- factor(proj.subjmeans$short_trigger, levels = unique(levels(proj.means$verb)))
levels(proj.subjmeans$verb)


# plot of means, 95% bootstrapped CIs and participants' ratings 
ggplot(proj.means, aes(x=verb, y=Mean, fill=VeridicalityGroup)) +
  geom_point(shape=21,fill="gray60",data=proj.subjmeans, alpha=.1, color="gray40") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0.1,color="black") +
  geom_point(shape=21,stroke=.5,size=2.5,color="black") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  scale_alpha(range = c(.3,1)) +
  scale_fill_manual(values=c("darkorchid","black","gray60","tomato1","dodgerblue")) +
  guides(fill=FALSE) +
  theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 45, hjust = 1, 
                                                                 color=cols$Colors)) +
  theme(legend.position="top") +
  ylab("Mean certainty rating") +
  xlab("Predicate") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1)) 
ggsave("../graphs/means-projectivity-by-predicate-variability.pdf",height=4,width=7)


### plot mean at-issueness ratings against mean projectivity ratings

# combine at-issueness and projection means
tmp.ai = ai.means %>%
  rename(AIMean="Mean",AIYMin="YMin",AIYMax="YMax",VG = "VeridicalityGroup")
tmp.proj = proj.means %>%
  rename(ProjMean="Mean",ProjYMin="YMin",ProjYMax="YMax") 
toplot = tmp.ai %>%
  left_join(tmp.proj, by=c("short_trigger")) 
head(toplot)
toplot

# toplot already has VeridicalityGroup, just need to define colors
cols = data.frame(V=levels(as.factor(toplot$short_trigger)))
cols

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

toplot$short_trigger <- factor(toplot$short_trigger, levels = toplot[order(as.character(proj.means$verb)),]$short_trigger, ordered = TRUE)
levels(toplot$short_trigger)

fill_cols = c("darkorchid","black","gray60","tomato1","dodgerblue",
              "darkorchid","black","gray60","tomato1","dodgerblue",
              "darkorchid","black","gray60","tomato1","dodgerblue",
              "darkorchid","black","gray60","tomato1","dodgerblue","black")

ggplot(toplot, aes(x=AIMean,y=ProjMean,fill=VeridicalityGroup)) +
  geom_text_repel(aes(label=short_trigger),color=cols$Colors,alpha=1,size=4) +
  geom_point(shape=21,stroke=.5,size=2.5,color="black") +
  scale_fill_manual(values=fill_cols) +
  geom_abline(intercept=0,slope=1,linetype="dashed",color="gray60") +
  geom_errorbar(aes(ymin=ProjYMin,ymax=ProjYMax)) +
  geom_errorbarh(aes(xmin=AIYMin,xmax=AIYMax)) +
  guides(fill=FALSE) +
  ylab("Mean not-at-issueness rating") +
  xlab("Mean projectivity rating") 
ggsave("../graphs/mean-projectivity-by-at-issueness.pdf",height=5,width=5)
  
# correlation between at-issueness and projectivity by predicate
# including main clauses
cor(toplot$AIMean,toplot$ProjMean) #0.7987381 (so .8)

# correlation between at-issueness and projectivity by predicate/content combination
# including main clauses
means = t %>%
  group_by(short_trigger, content) %>%
  summarize(AIMean = mean(ai), ProjMean = mean(projective))
means
cor(means$AIMean,means$ProjMean) #0.6970511 (so .7)
