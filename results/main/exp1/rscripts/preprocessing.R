# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source('../../helpers.R')

theme_set(theme_bw())

# load required packages for pre-processing data
require(tidyverse)

# read in the raw data
d = read_csv("../data/experiment-trials.csv")

# read in the subject information
ds = read_csv("../data/experiment-subject_information.csv")

# merge subject information into data
d = d %>%
  left_join(ds, by=c("workerid"))

# look at Turkers' comments
unique(ds$comments)

# age info
median(ds$age)
mean(ds$age)
ggplot(ds, aes(x=age)) +
  geom_histogram()

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

### exclude non-English speakers and non-American English speakers
# exclude non-English speakers
length(unique(ds$workerid)) # ( Turkers participated)
length(which(is.na(ds$language))) #no missing responses
table(ds$language) 


d <- d %>%
  filter(language != "Chinese" & language != "Russian" 
                & language != "telugu")) %>%
  droplevels()
length(unique(d$workerid)) # (data from X Turker excluded, X remaining Turkers)

# exclude non-American English speakers
length(unique(d$workerid))# X
length(which(is.na(d$american))) #X (everybody responded)
table(d$american) 

# exclude Turkers based on main clause controls

# main clauses
names(d)
d.MC <- d %>%
  filter(short_trigger == "MC") %>%
  droplevels()
nrow(d.MC) #X (X Turkers x 8 MCs x 2 questions)

# projection of main clause data
table(d$question_type)
d.MC.Proj <- d.MC %>%
  filter(question_type == "projective") %>%
  droplevels()
nrow(d.MC.Proj) #

# group projection mean (all Turkers, all clauses)
round(mean(d.MC.Proj$response),2)

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
nrow(d.MC.AI) #X

# group not-at-issueness mean (all Turkers, all clauses)
round(mean(d.MC.AI$response),2)

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
p

# get the Turkers who are more than 3 standard deviations above the mean on ai 
ai <- ai.means[ai.means$Mean > (mean(ai.means$Mean) + 3*sd(ai.means$Mean)),]
ai

# look at the main clauses that these "outlier" Turkers did
# make data subset of just the outliers
outliers <- d.MC %>%
  filter(workerid %in% p$workerid | workerid %in% ai$workerid)
outliers = droplevels(outliers)
nrow(outliers) #X (X unique outlier Turkers x 16 = 8 main clauses x 2 questions)

# exclude all outliers identified above
d <- d %>%
  filter(!(workerid %in% p$workerid | workerid %in% ai$workerid)) %>%
  droplevels()
length(unique(d$workerid)) # X remaining Turkers (X Turkers excluded)

# write cleaned dataset to file
write.csv(d, file="../data/data_preprocessed.csv",row.names=F,quote=F)
