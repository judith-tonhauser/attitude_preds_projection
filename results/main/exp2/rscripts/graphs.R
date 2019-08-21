# graphs file for experiment investigating the relationship between at-issueness and prior
# in predicting projection for the contents of the complements of 20 predicates

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# load required packages
require(tidyverse)
library(ggrepel)
library(dichromat)
library(forcats)
library(RColorBrewer)

theme_set(theme_bw())

# load helper functions
source('../../helpers.R')

d = read_csv("../data/data_preprocessed.csv")
nrow(d) #28340 / 52 Trials = 545 Turkers

summary(d)
table(d$prior)

# spread responses over separate columns for projectivity and at-issueness
cd = d %>%
  mutate(block_ai = ifelse(question_type == "ai", ifelse(block == "block1", "block1", "block2"), ifelse(block == "block1", "block2", "block1"))) %>%
  select(workerid,content,short_trigger,question_type,response,block_ai,prior) %>%
  spread(question_type,response)

# change cd verb names to match veridicality names
cd = cd %>%
  mutate(verb=recode(short_trigger, control = "MC", annoyed = "be_annoyed", be_right_that = "be_right", inform_Sam = "inform"))

# plot at-issueness by prior ----

# mean projectivity by predicate, including the main clause controls
ai.means = cd %>%
  group_by(short_trigger,prior) %>%
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


# color-blind-friendly palette
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") # c("#999999",

# plot of means, 95% bootstrapped CIs and participants' ratings 
ggplot(ai.means, aes(x=verb, y=Mean, fill=prior)) + 
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
  geom_errorbar(aes(x=1,ymin=ai.means[ai.means$verb == "MC",]$YMin,ymax=ai.means[ai.means$verb == "MC",]$YMax,width=.25),color="black") +
  geom_point(aes(x=1,y=ai.means[ai.means$verb == "MC",]$Mean), color="black",show.legend = FALSE ) +
  ylab("Mean not-at-issueness rating") +
  xlab("Predicate") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1))
ggsave("../graphs/means-nai-by-predicate-and-prior.pdf",height=4,width=7)

## plot projection by prior ----

# mean projectivity by predicate, including the main clause controls
proj.means = cd %>%
  group_by(short_trigger,prior) %>%
  summarize(Mean = mean(projective), CILow = ci.low(projective), CIHigh = ci.high(projective)) %>%
  ungroup() %>%
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


# color-blind-friendly palette
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") # c("#999999",

# plot of means, 95% bootstrapped CIs and participants' ratings 
ggplot(proj.means, aes(x=verb, y=Mean, fill=prior)) + 
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
  geom_errorbar(aes(x=1,ymin=proj.means[proj.means$verb == "MC",]$YMin,ymax=proj.means[proj.means$verb == "MC",]$YMax,width=.25),color="black") +
  geom_point(aes(x=1,y=proj.means[proj.means$verb == "MC",]$Mean), color="black",show.legend = FALSE ) +
  ylab("Mean certainty rating") +
  xlab("Predicate") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1))
ggsave("../graphs/means-projectivity-by-predicate-and-prior.pdf",height=4,width=7)

### plot mean at-issueness ratings against mean projectivity ratings by prior ----

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
  left_join(tmp.proj, by=c("short_trigger","prior")) %>%
  mutate(prior = replace_na(prior, "")) %>%
  mutate(prior=recode(prior,low_prior="L",high_prior="H")) 
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
  facet_wrap(~prior) +
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
  
# correlation between at-issueness and projectivity by predicate
# including main clauses
cor(toplot$AIMean,toplot$ProjMean) #0.73

# correlation between at-issueness and projectivity by predicate/content combination
# including main clauses
means = cd %>%
  group_by(short_trigger, content) %>%
  summarize(AIMean = mean(ai), ProjMean = mean(projective))
means
cor(means$AIMean,means$ProjMean) #0.68
