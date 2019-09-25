# preprocessing file for experiment investigating the relationship between at-issueness and prior
# in predicting projection for the contents of the complements of 20 predicates

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source('../../helpers.R')

# load required packages for pre-processing data
require(tidyverse)
theme_set(theme_bw())

# read in the raw data
d = read_csv("../data/experiment-trials.csv")
nrow(d) #31200 / 600 Turkers = 52 trials 
head(d)
summary(d) #600 unique workerids, experiment took 10 min (median), 11 min (mean)

length(unique(d$workerid)) #600

# count of how often each Turker did the experiment
count = d %>%
  dplyr :: select(workerid) %>%
  group_by(workerid) %>%
  tally(sort=T)
count 
View(count)
# nobody did the experiment more than once (this was run with UniqueTurker)

# read in the subject information
ds = read_csv("../data/experiment-subject_information.csv")
length(unique(ds$workerid)) #600
nrow(ds) #600
head(ds)

# look at Turkers' comments
unique(ds$comments)

# merge subject information into data
d = d %>%
  left_join(ds, by=c("workerid"))

nrow(d) #31200

# change the response for ai condition so that what was 0/not-at-issue is now 1/not-at-issue
# by subtracting the ai responses from 1
table(d$question_type,d$response)
d[d$question_type == "ai",]$response = 1 - d[d$question_type == "ai",]$response

# make a trial number
unique(d$slide_number_in_experiment) #slide numbers from 5 to 57
d$trial = d$slide_number_in_experiment - 4
unique(d$trial) # trial numbers from 1 to 53 (27 missing because instruction)
d[d$trial > 26,]$trial = d[d$trial > 26,]$trial - 1
unique(d$trial) # trials from 1 to 52

### exclude non-American English speakers
length(unique(d$workerid)) #600
length(which(is.na(d$language))) #260 (= 5 missing responses)
table(d$language) 

d[d$workerid == "420",]$language
d[d$workerid == "420",]$age
d[d$workerid == "420",]$american

# submitted via email to JT:
# I completed your entire study thoughtfully and then I accidentally double clicked on the last page. 
# I saw that it was demographic information but I was not able to fill it out. Please let me know the 
# questions asked. Age 31 gender: male, first language learned: English only language: English, 
# Nationality: American Race: White. Please let me know what to do I spent a lot of time on this 
# and accidentally screwed up on the last page [WORKERID REDACTED; corresponds to de-identified workerid 420]

d[d$workerid == "420",]$language <- "English"
str(d$language)
d[d$workerid == "420",]$age = 31
d[d$workerid == "420",]$american <- "Yes"
str(d$american)

#exclude anybody who didn't include English among languages spoken
d <- d %>%
  filter(language != "Italian" & language != "Polish" & language != "Tamil"
         & language != "Vietnamese" & language != "Bulgarian"
         & language != "Hindi" & language != "Kannada"
         & language != "Spanish" & language != "Telugu"
         & language != "Nepali" & language != "United States"
         & language != "khmer") %>%
  droplevels()
length(unique(d$workerid)) #582 (18 Turkers excluded)

# exclude non-American English speakers
length(unique(d$workerid)) #582
length(which(is.na(d$american))) #104 (2 didn't respond)
table(d$american) 

d <- d %>%
  filter(american == "Yes") %>%
  droplevels()
length(unique(d$workerid)) #574 (data from 8 Turkers excluded)

# exclude Turkers based on main clause controls

# main clauses
names(d)
d.MC <- d %>%
  filter(short_trigger == "MC") %>%
  droplevels()
nrow(d.MC) #6888 / 574 Turkers = 12 (6 main clause controls in each of the two blocks)

# projection of main clause data
table(d$question_type)
d.MC.Proj <- d.MC %>%
  filter(question_type == "projective") %>%
  droplevels()
nrow(d.MC.Proj) #3444 / 574 Turkers = 6 main clause controls in projection block

# group projection mean (all Turkers, all clauses)
round(mean(d.MC.Proj$response),2) #.13

# calculate each Turkers mean response to the projection of main clauses
p.means = d.MC.Proj %>%
  group_by(workerid) %>%
  summarize(Mean = mean(response), CI.Low=ci.low(response), CI.High=ci.high(response)) %>%
  ungroup() %>%
  mutate(YMin = Mean-CI.Low, YMax = Mean+CI.High)

ggplot(p.means, aes(x=workerid,y=Mean)) +
  geom_point() +
  geom_errorbar(aes(ymin=YMin, ymax=YMax))+
  geom_text(aes(label=workerid), vjust = 1, cex= 5)+
  ylab("Projection response mean")

# ai of main clause data
d.MC.AI <- d.MC %>%
  filter(question_type == "ai") %>%
  droplevels()
nrow(d.MC.AI) #3444 / 574 Turkers = 6 main clause controls in ai block

# group not-at-issueness mean (all Turkers, all clauses)
round(mean(d.MC.AI$response),2) #.06

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
p #15 Turkers

# get the Turkers who are more than 3 standard deviations above the mean on ai 
ai <- ai.means[ai.means$Mean > (mean(ai.means$Mean) + 3*sd(ai.means$Mean)),]
ai #16 Turkers

# make data subset of just the outliers
outliers <- d.MC %>%
  filter(workerid %in% p$workerid | workerid %in% ai$workerid)
outliers = droplevels(outliers)
nrow(outliers) #348 / 12 = 29 outlier Turkers

# exclude all outlier Turkers identified above
d <- d %>%
  filter(!(workerid %in% p$workerid | workerid %in% ai$workerid)) %>%
  droplevels()
length(unique(d$workerid)) #545 remaining Turkers

# write cleaned dataset to file
write_csv(d, path="../data/data_preprocessed.csv")

