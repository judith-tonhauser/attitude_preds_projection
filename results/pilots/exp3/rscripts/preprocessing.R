# preprocessing file for experiment investigating the relationship between at-issueness and prior
# in predicting projection for the contents of the complements of 20 predicates

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source('../../helpers.R')

# load required packages for pre-processing data
library(tidyverse)
theme_set(theme_bw())

# read in the raw data
d = read_csv("../data/experiment-trials.csv")
nrow(d) #702 / 9 = 78 trials (the pilot was done 9 times, as planned)
head(d)
summary(d) #9 unique workerids

length(unique(d$workerid)) #9 

# count of how often each Turker did the experiment
count = d %>%
  select(workerid) %>%
  group_by(workerid) %>%
  tally(sort=T)
count
# View(count)
# nobody did the pilot more than once (this was run with UniqueTurker)

# read in the subject information
ds = read_csv("../data/experiment-subject_information.csv")
length(unique(ds$workerid)) #9
nrow(ds) #9
head(ds)
summary(d) # experiment took 11 minutes (median), 13 minutes (mean)

# look at Turkers' comments
unique(ds$comments)

# look at age
mean(ds$age) #37.3
median(ds$age) #35

# merge subject information into data
d = d %>%
  left_join(ds, by=c("workerid"))

nrow(d) #702 / 9 Turkers = 78

# change the response for ai condition so that what was 0/not-at-issue is now 1/not-at-issue
# by subtracting the ai responses from 1
table(d$question_type,d$response)
d[d$question_type == "ai",]$response = 1 - d[d$question_type == "ai",]$response

# make a trial number
unique(d$slide_number_in_experiment) #slide numbers from 5 to 84
d$trial = d$slide_number_in_experiment - 4
unique(d$trial) # trial numbers from 1 to 80 (27 and 54 missing because instruction)
d[d$trial > 26,]$trial = d[d$trial > 26,]$trial - 1
d[d$trial > 52,]$trial = d[d$trial > 52,]$trial - 1
unique(d$trial) # trials from 1 to 78

### exclude non-American English speakers
length(unique(d$workerid)) #9
length(which(is.na(d$language))) #no missing responses
table(d$language) 

# exclude anybody who didn't include English among languages spoken
# d <- d %>%
#   filter(language != "Spanish" & language != "Korean" & language != "romanian" 
#          & language != "United States") %>%
#   droplevels()
# length(unique(d$workerid)) 

# exclude non-American English speakers
length(unique(d$workerid))# 9
length(which(is.na(d$american))) #0 (everybody responded)
table(d$american) 

# d <- d %>%
#   filter(american == "Yes") %>%
#   droplevels()
# length(unique(d$workerid)) #277 (data from 3 Turkers excluded)

# exclude Turkers based on main clause controls (not done for pilot data)
table(d$short_trigger,d$question_type)

# main clauses
names(d)
d.MC <- d %>%
  filter(short_trigger == "MC") %>%
  droplevels()
nrow(d.MC) #162 / 9 Turkers = 18 (6 main clause controls in each of the three blocks)

# projection of main clause data
table(d$question_type)

d.MC.Proj <- d.MC %>%
  filter(question_type == "projective") %>%
  droplevels()
nrow(d.MC.Proj) #54 / 9 Turkers = 6 main clause controls in projection block

# group projection mean (all Turkers, all clauses)
round(mean(d.MC.Proj$response),2) #0.35

# calculate each Turkers mean response to the projection of main clauses
p.means = d.MC.Proj %>%
  group_by(workerid) %>%
  summarize(Mean = mean(response), CI.Low=ci.low(response), CI.High=ci.high(response)) %>%
  ungroup() %>%
  mutate(YMin = Mean-CI.Low, YMax = Mean+CI.High)
p.means

ggplot(p.means, aes(x=workerid,y=Mean)) +
  geom_point() +
  geom_errorbar(aes(ymin=YMin, ymax=YMax))+
  geom_text(aes(label=workerid), vjust = 1, cex= 5)+
  ylab("Projection response mean")

# ai of main clause data
d.MC.AI <- d.MC %>%
  filter(question_type == "ai") %>%
  droplevels()
nrow(d.MC.AI) #54 / 9 Turkers = 6 main clause controls in ai block

# group not-at-issueness mean (all Turkers, all clauses)
round(mean(d.MC.AI$response),2) #0.14

# calculate each Turkers mean response to the projection of main clauses
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

# look at Turkers whose response mean on projection and ainess of main clauses is more than 3
# standard deviations away from the overall mean

# get the Turkers who are more than 3 standard deviations above the mean on projection 
p <- p.means[p.means$Mean > (mean(p.means$Mean) + 3*sd(p.means$Mean)),]
p #0 Turkers

# get the Turkers who are more than 3 standard deviations above the mean on ai 
ai <- ai.means[ai.means$Mean > (mean(ai.means$Mean) + 3*sd(ai.means$Mean)),]
ai #0 Turkers

# make data subset of just the outliers
outliers <- d.MC %>%
  filter(workerid %in% p$workerid | workerid %in% ai$workerid)
outliers = droplevels(outliers)
nrow(outliers) #0 / 12 = 0 outlier Turkers

# exclude all outlier Turkers identified above
d <- d %>%
  filter(!(workerid %in% p$workerid | workerid %in% ai$workerid)) %>%
  droplevels()
length(unique(d$workerid)) # 9 remaining Turkers (0 Turkers excluded)

# look at age again (and gender!)
mean(ds$age) #37.3
median(ds$age) #35

# write cleaned dataset to file
write_csv(d, path="../data/data_preprocessed.csv")
