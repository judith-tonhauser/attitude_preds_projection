# experiment investigating prior and presupposition
# contents of complements of 20 predicates
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
nrow(d) #468 / 9 = 52 trials (the pilot was done 9 times, as planned)
head(d)
summary(d) #9 unique workerids

length(unique(d$workerid)) #9 

# both blocks occurred first (randomization worked)
table(d$block,d$question_type)

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
summary(d) # experiment took 8 minutes (median), 8 minutes (mean)

# look at Turkers' comments
unique(ds$comments)

# look at age
mean(ds$age) #42
median(ds$age) #38

# merge subject information into data
d = d %>%
  left_join(ds, by=c("workerid"))

mean(d$Answer.time_in_minutes) #8.49
median(d$Answer.time_in_minutes) #7.68

nrow(d) #468 / 9 Turkers = 52

# age and gender info
length(which(is.na(d$age))) #0
table(d$age) #22-65
median(d$age,na.rm=TRUE) #38
d %>% 
  select(gender, workerid) %>% 
  unique() %>% 
  group_by(gender) %>% 
  summarize(count=n())
#5 female, 4 male, 0 other

# no recoding of responses
#table(d$question_type,d$response)
#d[d$question_type == "ai",]$response = 1 - d[d$question_type == "ai",]$response

# make a trial number
unique(d$slide_number_in_experiment) #slide numbers from 5 to 57
d$trial = d$slide_number_in_experiment - 4
unique(d$trial) # trial numbers from 1 to 53 (27 missing because instruction)
d[d$trial > 26,]$trial = d[d$trial > 26,]$trial - 1
unique(d$trial) # trials from 1 to 52

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

# exclude Turkers based on main clause controls in projection block
# same exclusion criterion as in XPRAG 2019 projection experiment
table(d$short_trigger,d$question_type)

# main clause controls in projection block
names(d)
table(d$question_type)

d.MC.Proj <- d %>%
  filter(short_trigger == "MC" & question_type == "projective") %>%
  droplevels()
nrow(d.MC.Proj) #54 / 9 Turkers = 6 controls

# group projection mean (all Turkers, all clauses)
round(mean(d.MC.Proj$response),2) #0.11

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

# Turkers with mean response more than 2 standard deviations above group mean
p <- p.means[p.means$Mean > (mean(p.means$Mean) + 2*sd(p.means$Mean)),]
p #0 Turkers

# make data subset of just the outliers
outliers <- d.MC %>%
  filter(workerid %in% p$workerid)
outliers = droplevels(outliers)
nrow(outliers) #0 / 12 = 0 outlier Turkers

# exclude all outlier Turkers identified above
d <- d %>%
  filter(!(workerid %in% p$workerid)) %>%
  droplevels()
length(unique(d$workerid)) # 9 remaining Turkers (0 Turkers excluded)

# age and gender info of remaining Turkers
length(which(is.na(d$age))) #0
table(d$age) #22-65
median(d$age,na.rm=TRUE) #38
d %>% 
  select(gender, workerid) %>% 
  unique() %>% 
  group_by(gender) %>% 
  summarize(count=n())
#5 female, 4 male, 0 other

# write cleaned dataset to file
write_csv(d, path="../data/data_preprocessed.csv")
