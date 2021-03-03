# interaction of prior beliefs, at-issueness and projection
# for 20 clause-embedding predicates
# preprocessing.R

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source('../../helpers.R')

# load required packages for pre-processing data
library(tidyverse)

theme_set(theme_bw()) 

# read in the raw data
d = read_csv("../data/experiment-trials.csv") 
nrow(d) #46800 = 600 participants x 78 trials (3x20 target + 3x6 filler/control)
head(d)
summary(d) 

length(unique(d$workerid)) #600

# count of how often each Turker did the experiment
count = d %>%
  select(workerid) %>%
  group_by(workerid) %>%
  tally(sort=T)
count #nobody did it more than once

# read in the subject information
ds = read_csv("../data/experiment-subject_information.csv")
length(unique(ds$workerid)) #600

# look at Turkers' comments
unique(ds$comments)

# merge subject information into data
d = d %>%
  left_join(ds, by=c("workerid")) 
nrow(d) #46800

# age and gender info 
# 3 people did not provide age info
str(d$age)
min(d$age,na.rm=T) #18
max(d$age,na.rm=T) #73
mean(d$age,na.rm=T) #38.5
ggplot(d, aes(x=age)) +
  geom_histogram()

# change the response for ai condition so that what was 0/not-at-issue is now 1/not-at-issue
# by subtracting the ai responses from 1
table(d$question_type,d$response)
d[d$question_type == "ai",]$response = 1 - d[d$question_type == "ai",]$response

# make a trial number
unique(d$slide_number_in_experiment) # slide numbers from 5 to 84
d$trial = d$slide_number_in_experiment - 4 # adds the column "trial"
unique(d$trial) # trial numbers from 1 to 80 (27 and 54 missing because instruction)
d[d$trial > 26,]$trial = d[d$trial > 26,]$trial - 1
unique(d$trial) # trials from 1 to 79 (53 missing because instruction)
d[d$trial > 52,]$trial = d[d$trial > 52,]$trial - 1
unique(d$trial) # trials from 1 to 78

# exclude non-American English speakers
length(unique(d$workerid)) #600
length(which(is.na(d$language))) #390 missing responses = 5 Turkers
table(d$language) 

# exclude anybody who didn't include English among languages spoken
d <- d %>%
  filter(!is.na(language)) %>%
  filter(language != "Spanish" & language != "Chinese" & 
           language != "Arabic" & language != "Female" & 
           language != "Turkish" & language != "Italian") %>% 
  droplevels()
length(unique(d$workerid)) #589 (data from 11 Turkers excluded)

# exclude non-American English speakers
length(unique(d$workerid)) #589
length(which(is.na(d$american))) #156 missing responses = 2 Turkers
table(d$american) # yes = 45552 / 78 = 584 Turkers

d <- d %>%
  filter(american == "Yes") %>%
  droplevels()
length(unique(d$workerid)) #584 (data from 5 Turkers excluded)

# exclude turkers who always clicked on roughly the same point on the scale 
# ie turkers whose variance in overall response distribution is more 
# than 2 sd below mean by-participant variance
variances = d %>%
  filter(short_trigger != "MC") %>%
  group_by(workerid) %>%
  summarize(Variance = var(response)) %>%
  mutate(TooSmall = Variance < mean(Variance) - 2*sd(Variance))

lowvarworkers = as.character(variances[variances$TooSmall,]$workerid)
summary(variances)
lowvarworkers # 15 turkers consistently clicked on roughly the same point on the scale

lvw = d %>%
  filter(as.character(workerid) %in% lowvarworkers) %>%
  droplevels() %>%
  mutate(Participant = as.factor(as.character(workerid)))

# only one Turker really wasn't doing the task right (0)
ggplot(lvw,aes(x=Participant,y=response)) +
  geom_point()

# exclude the Turker with ID 0
d <- droplevels(subset(d, d$workerid != "0"))
length(unique(d$workerid)) #583 Turkers remain

# exclude Turkers based on main clause controls

# main clauses
names(d)
d.MC <- d %>%
  filter(short_trigger == "MC") %>%
  droplevels()
nrow(d.MC) #10494 / 583 Turkers = 18 (6 main clause controls in each of the three blocks)

# projection of main clause data
table(d$question_type)
d.MC.Proj <- d.MC %>%
  filter(question_type == "projective") %>%
  droplevels()
nrow(d.MC.Proj) #3494 / 583 Turkers = 6 main clause controls in projection block

# group projection mean 
round(mean(d.MC.Proj$response),2) #.21

# calculate each Turkers mean response to the projection of main clauses
p.means = d.MC.Proj %>%
  group_by(workerid) %>%
  summarize(Mean = mean(response), CI.Low=ci.low(response), CI.High=ci.high(response)) %>% 
  ungroup() %>%
  mutate(YMin = Mean-CI.Low, YMax = Mean+CI.High) # 

ggplot(p.means, aes(x=workerid,y=Mean)) +
  geom_point() +
  geom_errorbar(aes(ymin=YMin, ymax=YMax))+
  geom_text(aes(label=workerid), vjust = 1, cex= 5)+
  ylab("Projection response mean")

# ai of main clause data
d.MC.AI <- d.MC %>%
  filter(question_type == "ai") %>%
  droplevels
nrow(d.MC.AI) #3494 / 583 Turkers = 6 main clause controls in ai block

# group not-at-issueness mean 
round(mean(d.MC.AI$response),2) #.09

# calculate each Turkers mean response to the ai of main clauses
ai.means = d.MC.AI %>%
  group_by(workerid) %>%
  summarize(Mean = mean(response), CI.Low=ci.low(response), CI.High=ci.high(response)) %>%
  ungroup() %>%
  mutate(YMin = Mean-CI.Low, YMax = Mean+CI.High) 

ggplot(ai.means, aes(x=workerid,y=Mean)) +
  geom_point() +
  geom_errorbar(aes(ymin=YMin, ymax=YMax))+
  geom_text(aes(label=workerid), vjust = 1, cex= 5)+
  ylab("NAI response mean")

# look at Turkers whose response mean on projection and ainess of main clauses is more than 2
# standard deviations away from the overall mean

p <- p.means[p.means$Mean > (mean(p.means$Mean) + 2*sd(p.means$Mean)),]
p # 32 Turkers 

ai <- ai.means[ai.means$Mean > (mean(ai.means$Mean) + 2*sd(ai.means$Mean)),]
ai # 48 Turkers

# make data subset of just the outliers
outliers <- d.MC %>%
  filter(workerid %in% p$workerid | workerid %in% ai$workerid) 
outliers = droplevels(outliers)
nrow(outliers) #1404 / 18 = 78 Turker

# plot outlier Turkers

ggplot(p, aes(x=workerid,y=Mean)) +
  geom_point() +
  geom_errorbar(aes(ymin=YMin, ymax=YMax))+
  geom_text(aes(label=workerid), vjust = 1, cex= 5)+
  ylab("projection response mean")


ggplot(ai, aes(x=workerid,y=Mean)) +
  geom_point() +
  geom_errorbar(aes(ymin=YMin, ymax=YMax))+
  geom_text(aes(label=workerid), vjust = 1, cex= 5)+
  ylab("ai response mean")

# exclude all outlier Turkers identified above
d <- d %>%
  filter(!(workerid %in% p$workerid | workerid %in% ai$workerid)) %>% 
  droplevels()
length(unique(d$workerid)) #505 Turkers (78 Turkers excluded)

# age info (for all remaining Turkers)
median(d$age) #37
mean(d$age) #39.5
ggplot(d, aes(x=age)) +
  geom_histogram()


# write cleaned dataset to file
write_csv(d, path="../data/cd.csv")
