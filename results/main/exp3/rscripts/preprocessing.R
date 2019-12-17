# preprocessing file for investigating whether participants' prior probability ratings of a content
# predict their projectivity ratings for that content (Lehnhardt thesis) and for JD/JT research on 
# whether participants' prior probability and at-issueness ratings predict projectivity, and whether the
# two factors are independent

# load required packages for pre-processing data
library(tidyverse)

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source('../../helpers.R')

theme_set(theme_bw()) # ggplot2 provides functions that create a theme that can be added to a plot: ggplot() + ... + theme_bw(); you can override some options with opts(...) - but make sure to add it after the theme!


# read in the raw data
d = read_csv("../data/experiment-trials.csv") # double variables can store floating point numbers, character variables can store strings of characters
nrow(d) # [702] / [9] = 78 trials (the experiment will be done 78 times)
head(d)
summary(d) # experiment took [11] min (median), [13] min (mean)

length(unique(d$workerid)) # [9] uniqe workerids
# so none of the [9] experiments were done by Turkers who had already done the exp (UniqueTurker)

# count of how often each Turker did the experiment
count = d %>%
  select(workerid) %>%
  group_by(workerid) %>%
  tally(sort=T)
View(count)
# this again confirms that no Turker did the experiment more than once

# read in the subject information
ds = read_csv("../data/experiment-subject_information.csv")
length(unique(ds$workerid)) # [9]
nrow(ds) # [9]

# look at Turkers' comments
unique(ds$comments)

# merge subject information into data
d = d %>%
  left_join(ds, by=c("workerid")) # returns all rows from the first table and all columns from both tables

# age and gender info (for all [9] unique Turkers that participated)
median(d$age) # [35]
mean(d$age) # [37.33]
ggplot(d, aes(x=age)) +
  geom_histogram()
#table(d$gender) # gender info was not available in pilot

nrow(d) # [702] / 78 = [9]

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
length(unique(d$workerid)) # [9]
length(which(is.na(d$language))) # [0] missing responses
table(d$language) 

# exclude anybody who didn't include English among languages spoken
d <- d %>%
  filter(language != "Spanish" & language != "Korean" & language != "romanian" & language != "Chinese" & language != "Arabic" & language != "Female" & language != "Turkish" & language != "Italian" & language != "United States") %>% # just take the data from 'table'
  droplevels()
length(unique(d$workerid)) # [589] (data from [11] Turkers excluded)

# exclude non-American English speakers
length(unique(d$workerid)) # [589]
length(which(is.na(d$american))) # [156] missing responses
table(d$american) # [45552] / 78 = [584] Turkers

d <- d %>%
  filter(american == "Yes") %>%
  droplevels()
length(unique(d$workerid)) # [584] (data from [5] Turkers excluded)

# exclude Turkers based on main clause controls

# main clauses
names(d)
d.MC <- d %>%
  filter(short_trigger == "MC") %>%
  droplevels()
nrow(d.MC) # [10512] / [584] Turkers = 18 (6 main clause controls in each of the three blocks)

# projection of main clause data
table(d$question_type)
d.MC.Proj <- d.MC %>%
  filter(question_type == "projective") %>%
  droplevels()
nrow(d.MC.Proj) # [3504] / [584] Turkers = 6 main clause controls in projection block

# group projection mean (all Turkers, all clauses)
round(mean(d.MC.Proj$response),2) # [0.21]

# calculate each Turkers mean response to the projection of main clauses
p.means = d.MC.Proj %>%
  group_by(workerid) %>%
  summarize(Mean = mean(response), CI.Low=ci.low(response), CI.High=ci.high(response)) %>% # mean and bootstrapped (!) confidence intervals
  ungroup() %>%
  mutate(YMin = Mean-CI.Low, YMax = Mean+CI.High) # mutate() adds columns

ggplot(p.means, aes(x=workerid,y=Mean)) +
  geom_point() +
  geom_errorbar(aes(ymin=YMin, ymax=YMax))+
  geom_text(aes(label=workerid), vjust = 1, cex= 5)+
  ylab("Projection response mean")

# ai of main clause data
d.MC.AI <- d.MC %>%
  filter(question_type == "ai") %>%
  droplevels
nrow(d.MC.AI) # [3504] / [584] Turkers = 6 main clause controls in ai block

# group not-at-issueness mean (all Turkers, all clauses)
round(mean(d.MC.AI$response),2) # [0.09]

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

# calculate each Turkers mean response to the ai of main clauses
ai.means = d.MC.AI %>%
  group_by(workerid) %>%
  summarize(Mean = mean(response), CI.Low=ci.low(response), CI.High=ci.high(response)) %>%
  ungroup() %>%
  mutate(YMin = Mean-CI.Low, YMax = Mean+CI.High)

# look at Turkers whose response mean on projection and ainess of main clauses is more than 3
# standard deviations away from the overall mean

# get the Turkers who are more than 3 standard deviations above the mean on projection 
p <- p.means[p.means$Mean > (mean(p.means$Mean) + 3*sd(p.means$Mean)),]
p # [0] Turkers 

# get the Turkers who are more than 3 standard deviations above the mean on ai 
ai <- ai.means[ai.means$Mean > (mean(ai.means$Mean) + 3*sd(ai.means$Mean)),]
ai # [0] Turkers

# make data subset of just the outliers
outliers <- d.MC %>%
  filter(workerid %in% p$workerid | workerid %in% ai$workerid) # | indicates logical or
outliers = droplevels(outliers)
nrow(outliers) # [288] / 18 = [16] outlier Turkers

# exclude all outlier Turkers identified above
d <- d %>%
  filter(!(workerid %in% p$workerid | workerid %in% ai$workerid)) %>% # selects all worker IDs except for those that appear in p or ai
  droplevels()
length(unique(d$workerid)) # [568] remaining Turkers ([16] Turkers excluded)

# age info (for all remaining Turkers)
median(d$age) # [36]
mean(d$age) # [38.66]
ggplot(d, aes(x=age)) +
  geom_histogram()
#table(d$gender) # gender info was not available in pilot

# write cleaned dataset to file
write_csv(d, path="../data/data_preprocessed.csv")
