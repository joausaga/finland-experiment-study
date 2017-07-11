####
## In this script we perform
## more complex statistical
## analyses to study the activity
## of the participants on the
## platform
####

library(dplyr)
library(ggplot2)
library(ROCR)
library(FSA)
library(likert)
library(coin)


activity_by_author = function(ds1, ds2, col_names) {
  ds1 = group_by(ds1,author_email)
  ds2 = group_by(ds2,author_email)
  ds_by_authors = merge(
    summarise(ds1, n_s1=n()),
    summarise(ds2, n_s2=n()),
    by.x='author_email',
    by.y='author_email',
    all = T
  )
  num_col = ncol(ds_by_authors)
  ds_by_authors[,c(2:num_col)] = lapply(ds_by_authors[,c(2:num_col)], function(x) ifelse(is.na(x),0,x))
  ds_by_authors = mutate(ds_by_authors, n=n_s1+n_s2)
  colnames(ds_by_authors) = col_names
  return (ds_by_authors)
}


compute_participant_lifespan = function(participant_activity, part_reg_day) {
  participant_activity = arrange(participant_activity, desc(creation_datetime))
  last_activity_day =  as.Date(as.character(participant_activity[1,'creation_datetime']))
  first_activity_day = as.Date(as.character(participant_activity[nrow(participant_activity),'creation_datetime']))
  lifespan = 0
  if (first_activity_day >= begin_first_stage) {
    if (last_activity_day >= end_first_stage) {
      lifespan = days_first_stage
      lifespan = lifespan + as.numeric(round(abs(difftime(last_activity_day, begin_second_stage, units="days")),0)) + 1
    } else {
      lifespan = as.numeric(abs(difftime(last_activity_day, begin_first_stage, units="days"))) + 1
    } 
  }
  else {
    lifespan = -1
  }
  
  return (lifespan)
}


compute_participant_days_of_activity = function(participant_activity) {
  participant_activity = arrange(participant_activity, creation_datetime)
  participant_activity$creation_date = as.Date(as.character(participant_activity$creation_datetime))
  participant_activity$day=as.numeric(
    round(
      difftime(participant_activity[,'creation_date'], 
               participant_activity[1,'creation_date'], units = "days"),
      0)
    ) + 1
  activities_by_day = group_by(participant_activity, day)
  sum_activity = summarise(activities_by_day, num_activities=n())
  return(sum_activity)
}


# Define constants
begin_first_stage = as.Date('19/05/14', format="%d/%m/%y")
end_first_stage = as.Date('20/06/14', format="%d/%m/%y")
begin_second_stage = as.Date('24/05/15', format="%d/%m/%y")
end_second_stage = as.Date('30/06/15', format="%d/%m/%y")
total_days_process = 71
days_first_stage = 33
day_second_stage = 38


# Load data
ideas_1_stage = read.csv("./data/activity logs/idea_log_1_stage.csv", sep=",", header=T, stringsAsFactors=FALSE)
ideas_2_stage = read.csv("./data/activity logs/idea_log_2_stage.csv", sep=",", header=T, stringsAsFactors=FALSE)
comments_1_stage = read.csv("./data/activity logs/comment_log_1_stage.csv", sep=",", header=T, stringsAsFactors=FALSE)
comments_2_stage = read.csv("./data/activity logs/comment_log_2_stage.csv", sep=",", header=T, stringsAsFactors=FALSE)
votes_1_stage = read.csv("./data/activity logs/vote_log_1_stage_new.csv", sep=",", header=T, stringsAsFactors=FALSE)
votes_2_stage = read.csv("./data/activity logs/vote_log_2_stage_new.csv", sep=",", header=T, stringsAsFactors=FALSE)
users = read.csv("./data/activity logs/user_log.csv", sep=",", header=T, stringsAsFactors=FALSE)


# Reading survey data
survey_1 = read.csv("./data/survey1-condlaw-complete.csv",header=TRUE,sep=",",stringsAsFactors=F)
survey_2 = read.csv("./data/survey2-condlaw-complete.csv",header=T, sep=",",stringsAsFactors=F)
survey_3 = read.csv("./data/survey3-condlaw2-complete.csv",header=T, sep=",",stringsAsFactors=F)


# Data processing and transformation
ideas_1_stage$creation_datetime = as.POSIXct(ideas_1_stage$creation_datetime, format="%d/%m/%y %H:%M")
ideas_2_stage$creation_datetime = as.POSIXct(ideas_2_stage$creation_datetime, format="%d/%m/%y %H:%M")
comments_1_stage$creation_datetime = as.POSIXct(comments_1_stage$creation_datetime, format="%d/%m/%y %H:%M")
comments_2_stage$creation_datetime = as.POSIXct(comments_2_stage$creation_datetime, format="%d/%m/%y %H:%M")
votes_1_stage$creation_datetime = as.POSIXct(votes_1_stage$creation_datetime, format="%d/%m/%y %H:%M")
votes_2_stage$creation_datetime = as.POSIXct(votes_2_stage$creation_datetime, format="%d/%m/%y %H:%M")
users$creation_datetime = as.POSIXct(users$creation_datetime, format="%B-%d-%Y %H:%M")
users_1_stage = filter(users, creation_datetime<=as.POSIXct('30/06/14', format="%d/%m/%y"))
users_2_stage = filter(users, creation_datetime>as.POSIXct('30/06/14', format="%d/%m/%y"))

# Polish survey data
survey_1 = survey_1 %>% filter(email!='') %>% select(-V9)
survey_2 = survey_2 %>% filter(email!='') %>% select(-V9)
survey_3 = survey_3 %>% filter(email!='') %>% select(-V9)
# Rename columns adding survey identifier
colnames(survey_1)[2:130] = unlist(lapply(colnames(survey_1[2:130]),function(x) paste(x,'.s1',sep='')))
colnames(survey_2)[2:44] = unlist(lapply(colnames(survey_2[2:44]),function(x) paste(x,'.s2',sep='')))
colnames(survey_3)[2:71] = unlist(lapply(colnames(survey_3[2:71]),function(x) paste(x,'.s3',sep='')))


# Create table containing participation per user
# Ideas
ideas_by_authors = arrange(activity_by_author(ideas_1_stage,ideas_2_stage,c('email','n_ideas_s1','n_ideas_s2','n_ideas')), desc(n_ideas))
# How many posted ideas on both stages
nrow(filter(ideas_by_authors,n_ideas_s1!=0&n_ideas_s2!=0)) # output: 3
# Comments
comments_by_authors = arrange(activity_by_author(comments_1_stage,comments_2_stage,c('email','n_comments_s1','n_comments_s2','n_comments')), 
                              desc(n_comments))
# How many posted comments on both stages
nrow(filter(comments_by_authors,n_comments_s1!=0&n_comments_s2!=0)) # output: 23
# Votes
votes_by_authors = arrange(activity_by_author(votes_1_stage,votes_2_stage,c('email','n_votes_s1','n_votes_s2','n_votes')), 
                              desc(n_votes))
# How many voted on both stages
nrow(filter(votes_by_authors,n_votes_s1!=0&n_votes_s2!=0)) # output: 15
# All activities
participant_activities = merge(
  ideas_by_authors,
  comments_by_authors,
  by.x = 'email',
  by.y = 'email',
  all = T
)
participant_activities = merge(
  participant_activities,
  votes_by_authors,
  by.x = 'email',
  by.y = 'email',
  all = T
)
num_col = ncol(participant_activities)
participant_activities[,c(2:num_col)] = lapply(participant_activities[,c(2:num_col)], function(x) ifelse(is.na(x),0,x))
participant_activities[,'n'] = apply(participant_activities[,c(2:num_col)], 1, function(x) x['n_ideas']+x['n_comments']+x['n_votes'])
participant_activities = arrange(participant_activities, desc(n))

# Add registration day
participant_activities = merge(
  participant_activities,
  users,
  by.x = 'email',
  by.y = 'emai',
  all = T
)
participant_activities = filter(participant_activities, email != '')
participant_activities[,c(2:11)] = lapply(participant_activities[,c(2:11)], function(x) ifelse(is.na(x),0,x))
participant_activities = select(participant_activities, -id)
colnames(participant_activities)[12] = 'registration_datetime'


# Distribution of the participants by contributions
round(nrow(filter(participant_activities,n==0))/nrow(participant_activities),2) # output: 42%
round(nrow(filter(participant_activities,n>0&n<5))/nrow(participant_activities),2) # output: 24%
round(nrow(filter(participant_activities,n>5&n<=15))/nrow(participant_activities),2) # output: 12%
round(nrow(filter(participant_activities,n>15&n<=30))/nrow(participant_activities),2) # output: 10%
round(nrow(filter(participant_activities,n>30&n<=60))/nrow(participant_activities),2) # output: 5%
round(nrow(filter(participant_activities,n>60&n<=100))/nrow(participant_activities),2) # output: 3%
round(nrow(filter(participant_activities,n>100))/nrow(participant_activities),2) # output: 3%


# Study first action of the participants
first_action_participants = data.frame(a_email=NULL,first_action=NULL)
for (email in participant_activities$email) {
  p_activities = ideas_1_stage %>% filter(author_email==email) %>% select(author_email, creation_datetime) %>% mutate(type='idea')
  p_activities = rbind(p_activities, 
                       ideas_2_stage %>% filter(author_email==email) %>% select(author_email, creation_datetime) %>% mutate(type='idea'))
  p_activities = rbind(p_activities, 
                       comments_1_stage %>% filter(author_email==email) %>% select(author_email, creation_datetime) %>% mutate(type='comment'))
  p_activities = rbind(p_activities, 
                       comments_2_stage %>% filter(author_email==email) %>% select(author_email, creation_datetime) %>% mutate(type='comment'))
  p_activities = rbind(p_activities, 
                       votes_1_stage %>% filter(author_email==email) %>% select(author_email, creation_datetime) %>% mutate(type='vote'))
  p_activities = rbind(p_activities, 
                       votes_2_stage %>% filter(author_email==email) %>% select(author_email, creation_datetime) %>% mutate(type='vote'))
  p_activities = arrange(p_activities,creation_datetime)
  first_action_participants = rbind(first_action_participants,data.frame(a_email=email,type_a=p_activities[1,'type']))
}
# How many posted an idea as the first action
nrow(filter(first_action_participants,type_a=='idea'))/nrow(first_action_participants)  #output: 14%
# How many posted a comment as the first action
nrow(filter(first_action_participants,type_a=='comment'))/nrow(first_action_participants)   #output: 28%
# How many posted a vote as the first action
nrow(filter(first_action_participants,type_a=='vote'))/nrow(first_action_participants)   #output: 58%


# Compute the participants' lifespan (time between registration and last 
# activity on the platform) and days of activity
contrib = filter(participant_activities,n>0)
contrib = arrange(contrib, registration_datetime)
for (i in 1:nrow(contrib)) {
  p_email = contrib[i,'email']
  part_reg_day = contrib[i,'registration_datetime']
  p_activities = ideas_1_stage %>% filter(author_email==p_email) %>% select(author_email, creation_datetime) %>% mutate(type='idea_s1')
  p_activities = rbind(p_activities, 
                       ideas_2_stage %>% filter(author_email==p_email) %>% select(author_email, creation_datetime) %>% mutate(type='idea_s2'))
  p_activities = rbind(p_activities, 
                       comments_1_stage %>% filter(author_email==p_email) %>% select(author_email, creation_datetime) %>% mutate(type='comment_s1'))
  p_activities = rbind(p_activities, 
                       comments_2_stage %>% filter(author_email==p_email) %>% select(author_email, creation_datetime) %>% mutate(type='comment_s2'))
  p_activities = rbind(p_activities, 
                       votes_1_stage %>% filter(author_email==p_email) %>% select(author_email, creation_datetime) %>% mutate(type='vote_s1'))
  p_activities = rbind(p_activities, 
                       votes_2_stage %>% filter(author_email==p_email) %>% select(author_email, creation_datetime) %>% mutate(type='vote_s2'))
  if (is.na(part_reg_day)) {
    part_reg_day = arrange(p_activities, creation_datetime)[1,'creation_datetime']
    contrib[i,'registration_datetime'] = part_reg_day
  }
  contrib[i,'lifespan'] = compute_participant_lifespan(p_activities,part_reg_day)
  contrib[i,'days_of_activity'] = nrow(compute_participant_days_of_activity(p_activities))
  contrib[i,'first_stage'] = ifelse(contrib[i,'n_ideas_s1']>0|contrib[i,'n_comments_s1']>0|contrib[i,'n_votes_s1']>0,1,0)
  contrib[i,'second_stage'] = ifelse(contrib[i,'n_ideas_s2']>0|contrib[i,'n_comments_s2']>0|contrib[i,'n_votes_s2']>0,1,0)
  # Compute activity on the first 24 hs
  day_reg_activties = filter(p_activities,as.numeric(round(abs(difftime(part_reg_day, creation_datetime, units="hours")),0))<=24)
  contrib[i,'activity_24'] = nrow(day_reg_activties)
}


# Get rid of the participants whose lifespan == -1, meaning those whose first activity
# was before the process started
contrib = filter(contrib, lifespan!=-1)


# Mean of active days
summary(contrib$days_of_activity/contrib$lifespan)


# Print quantile of activity on the first 24 hs
quantile(contrib$activity_24, probs=c(.10, .25, .50, .75, 1))
# Output:
# 10%  25%  50%  75% 100% 
# 0    1    3   10  111


# Print quantile of lifespan
quantile(contrib$lifespan, probs=c(.10, .25, .50, .75, 1))
# Output:
# 10%  25%  50%  75% 100% 
# 2   13   31   45   71


# Print quantile of the participants' active days 
quantile(contrib$days_of_activity, probs=c(.10, .25, .50, .75, 1))
# Output: 
# 10%  25%  50%  75% 100% 
# 1    1    1    2   58


# Calculate some statistics about content production
nrow(filter(contrib,n_votes>0))/nrow(contrib)   # 74% voted
nrow(filter(contrib,n_comments>0))/nrow(contrib)   # 61% voted
nrow(filter(contrib,n_ideas>0))/nrow(contrib)   # 30% voted

# Compute active contributors per day
dates_first_stage = seq(begin_first_stage,end_first_stage,by='days')
dates_second_stage = seq(begin_second_stage,end_second_stage,by='days')
dates_process = data.frame(num_day=rep(1:length(dates_first_stage)),date=dates_first_stage)
dates_process$stage = 'first'
dates_process = rbind(dates_process,
                      data.frame(num_day=rep(length(dates_first_stage)+1:length(dates_second_stage)),date=dates_second_stage,stage='second'))
all_activities = rbind(mutate(ideas_1_stage,type='idea'),
                       mutate(ideas_2_stage,type='idea'))
all_activities = rbind(all_activities, 
                       mutate(comments_1_stage,type='comment'))
all_activities = rbind(all_activities, 
                       mutate(comments_2_stage,type='comment'))
all_activities = rbind(all_activities, 
                       votes_1_stage %>% mutate(idea_id=content_id,type='vote') %>% select(creation_datetime,idea_id,author_email,type))
all_activities = rbind(all_activities, 
                       votes_2_stage %>% mutate(idea_id=content_id,type='vote') %>% select(creation_datetime,idea_id,author_email,type))
all_activities$creation_date = as.Date(all_activities$creation_datetime)
all_activities = arrange(all_activities,creation_date)
for (i in 1:nrow(dates_process)) {
  activities_on_date = filter(all_activities,creation_date==dates_process[i,'date'])
  num_active_participants = length(unique(activities_on_date$author_email))
  dates_process[i,'active_participants'] = num_active_participants
}
ggplot(dates_process, aes(x=num_day, y=active_participants)) +
  geom_line() +
  labs(x='Day of the process') + labs(y='Number of active participants') +
  geom_vline(xintercept = filter(dates_process, date==end_first_stage)$num_day, linetype="dashed", color = "red", size=1) +
  theme(axis.text=element_text(size=18), axis.title.y=element_text(size=20), 
        axis.title.x=element_text(size=20), legend.position=c(0.9,0.9), 
        legend.text=element_text(size=20), legend.title=element_blank(), 
        strip.text.x=element_text(size=20))


# Compute ideas, votes, and comments per day
for (i in 1:nrow(dates_process)) {
  ideas_on_date = filter(all_activities,creation_date==dates_process[i,'date']&type=='idea')
  dates_process[i,'num_ideas'] = nrow(ideas_on_date)
  votes_on_date = filter(all_activities,creation_date==dates_process[i,'date']&type=='vote')
  dates_process[i,'num_votes'] = nrow(votes_on_date)
  comments_on_date = filter(all_activities,creation_date==dates_process[i,'date']&type=='comment')
  dates_process[i,'num_comments'] = nrow(comments_on_date)
}
ggplot(dates_process, aes(x=num_day, y=num_ideas)) +
  geom_point() +
  geom_line(size=1) +
  labs(x='Day of the process') + labs(y='Number of ideas') +
  geom_vline(xintercept = filter(dates_process, date==end_first_stage)$num_day, linetype="dashed", color = "red", size=1) +
  theme(axis.text=element_text(size=18), axis.title.y=element_text(size=20), 
        axis.title.x=element_text(size=20), legend.position=c(0.9,0.9), 
        legend.text=element_text(size=20), legend.title=element_blank(), 
        strip.text.x=element_text(size=20))
ggplot(dates_process, aes(x=num_day)) +
  geom_point(aes(y=num_comments)) +
  geom_point(aes(y=num_votes)) +
  geom_line(aes(y=num_comments), size=1) +
  geom_line(aes(y=num_votes), size=1, linetype='dashed') +
  labs(x='Day of the process') + labs(y='Number of comments/votes') +
  geom_vline(xintercept = filter(dates_process, date==end_first_stage)$num_day, linetype="dotted", color = "red", size=1) +
  theme(axis.text=element_text(size=18), axis.title.y=element_text(size=20), 
        axis.title.x=element_text(size=20), 
        strip.text.x=element_text(size=20))


# Compute average frequency of contributions of the
# participants
for (p_email in unique(all_activities$author_email)) {
  p_activities = filter(all_activities, author_email==p_email)
  p_activities = arrange(p_activities, creation_datetime)
  freq = c()
  cur_dt = p_activities[1,'creation_datetime']
  for (i in 2:nrow(p_activities)) {
    freq = rbind(freq, difftime(p_activities[i,'creation_datetime'], cur_dt, units='secs'))
    cur_dt = p_activities[i,'creation_datetime']
  }
  contrib[contrib$email==p_email,'freq_cont_secs'] = mean(freq)
}


# Penalize the participants who contributed
contrib[is.na(contrib$freq_cont_secs),'freq_cont_secs'] = 200000000


# Check difference in contributions between those who stayed around in both phases
# and those who dropped out during the first phase
contrib_fs = filter(contrib,first_stage==1&second_stage==0)   # N=195
contrib_bs = filter(contrib,first_stage==1&second_stage==1)   # N=28
# Contribution of ideas
ds_c = data.frame(n=contrib_fs$n_ideas_s1,group='fs')
ds_c = rbind(ds_c, data.frame(n=contrib_bs$n_ideas_s1,group='bs'))
wilcox.test(n ~ group, data = ds_c)
# Contribution of comments
ds_c = data.frame(n=contrib_fs$n_comments_s1,group='fs')
ds_c = rbind(ds_c, data.frame(n=contrib_bs$n_comments_s1,group='bs'))
wilcox.test(n ~ group, data = ds_c)   # Two-sample Mann–Whitney U test
summary(contrib_fs$n_comments_s1)
summary(contrib_bs$n_comments_s1)
# Contribution of votes
ds_c = data.frame(n=contrib_fs$n_votes_s1,group='fs')
ds_c = rbind(ds_c, data.frame(n=contrib_bs$n_votes_s1,group='bs'))
wilcox.test(n ~ group, data = ds_c)
summary(contrib_fs$n_votes_s1)
summary(contrib_bs$n_votes_s1)
# Activity on the first 24 hs
ds_c = data.frame(n=contrib_fs$activity_24,group='fs')
ds_c = rbind(ds_c, data.frame(n=contrib_bs$activity_24,group='bs'))
wilcox.test(n ~ group, data = ds_c)
# Frequency of contributions
ds_c = data.frame(n=contrib_fs$freq_cont_secs,group='fs')
ds_c = rbind(ds_c, data.frame(n=contrib_bs$freq_cont_secs,group='bs'))
wilcox.test(n ~ group, data = ds_c)
summary(contrib_fs$freq_cont_secs/86400)
summary(contrib_bs$freq_cont_secs/86400)


# Analyse motivation factors of both groups the ones who dropped out
# and the ones who stayed around in the second stage
mot_factors_s1 = survey_1 %>% select(email, Q2_1.s1, Q2_2.s1, Q2_3.s1, Q2_4.s1, Q2_5.s1, Q2_6.s1, Q2_7.s1)
colnames(mot_factors_s1) = c('email','F1','F2','F3','F4','F5','F6','F7')
mot_factors_s1 = filter(mot_factors_s1, !is.na(F1) & !is.na(F2) & !is.na(F3) & !is.na(F4) & !is.na(F5) & !is.na(F6) & !is.na(F7))
f_mot_factors_s1 = mot_factors_s1
f_mot_factors_s1[,c(2:8)] = lapply(f_mot_factors_s1[,c(2:8)], 
                                   function(x) factor(x, levels=c('1','2','3','4','5','6','7'), ordered=T))
emails_contrib_fs_bs = c(contrib_fs$email,contrib_bs$email)
f_mot_factors_s1 = filter(f_mot_factors_s1, email %in% emails_contrib_fs_bs)
f_mot_factors_s1$group = ifelse(f_mot_factors_s1$email %in% contrib_fs$email,'first_stage','first_and_second_stage')
nrow(filter(f_mot_factors_s1,group=='first_stage'))  #N=91
nrow(filter(f_mot_factors_s1,group=='first_and_second_stage'))  #N=19
ret_likert = likert(f_mot_factors_s1[2:8], grouping = f_mot_factors_s1$group)
summary(ret_likert)
plot(ret_likert,type='bar')

# Compare difference in the score of the motivation factors between both groups
m_f_mot_factors_s1 = melt(f_mot_factors_s1, id.vars='group', 
                          measure.vars=c('F1','F2','F3','F4','F5','F6','F7'))
m_f_mot_factors_s1$value = as.integer(m_f_mot_factors_s1$value)
colnames(m_f_mot_factors_s1) = c('group','factor','score')
wilcox.test(score ~ group, data=filter(m_f_mot_factors_s1,factor=='F1'))   # W = 973.5, p-value = 0.3447
wilcox.test(score ~ group, data=filter(m_f_mot_factors_s1,factor=='F2'))   # W = 915, p-value = 0.6609
wilcox.test(score ~ group, data=filter(m_f_mot_factors_s1,factor=='F3'))   # W = 925, p-value = 0.6308
wilcox.test(score ~ group, data=filter(m_f_mot_factors_s1,factor=='F4'))   # W = 788, p-value = 0.5273   
wilcox.test(score ~ group, data=filter(m_f_mot_factors_s1,factor=='F5'))   # W = 817, p-value = 0.6523
wilcox.test(score ~ group, data=filter(m_f_mot_factors_s1,factor=='F6'))   # W = 881, p-value = 0.8976
wilcox.test(score ~ group, data=filter(m_f_mot_factors_s1,factor=='F7'))   # W = 1077.5, p-value = 0.08197
# No significant differences


# Analyze correlation between number of comments and scores of the motivation factors
contrib_fs_c = merge(contrib_fs,mot_factors_s1,by.x='email',by.y='email')
contrib_bs_c = merge(contrib_bs,mot_factors_s1,by.x='email',by.y='email')
# F1
cor(contrib_fs_c$n_comments_s1,contrib_fs_c$F1,method='spearman',use='complete.obs')
cor(contrib_bs_c$n_comments_s1,contrib_bs_c$F1,method='spearman',use='complete.obs')
# F2
cor(contrib_fs_c$n_comments_s1,contrib_fs_c$F2,method='spearman',use='complete.obs')
cor(contrib_bs_c$n_comments_s1,contrib_bs_c$F2,method='spearman',use='complete.obs')
# F3
cor(contrib_fs_c$n_comments_s1,contrib_fs_c$F3,method='spearman',use='complete.obs')
cor(contrib_bs_c$n_comments_s1,contrib_bs_c$F3,method='spearman',use='complete.obs')
# F4
cor(contrib_fs_c$n_comments_s1,contrib_fs_c$F4,method='spearman',use='complete.obs')
cor(contrib_bs_c$n_comments_s1,contrib_bs_c$F4,method='spearman',use='complete.obs')
# F5
cor(contrib_fs_c$n_comments_s1,contrib_fs_c$F5,method='spearman',use='complete.obs')
cor(contrib_bs_c$n_comments_s1,contrib_bs_c$F5,method='spearman',use='complete.obs')
# F6
cor(contrib_fs_c$n_comments_s1,contrib_fs_c$F6,method='spearman',use='complete.obs')
cor(contrib_bs_c$n_comments_s1,contrib_bs_c$F6,method='spearman',use='complete.obs')
# F7
cor(contrib_fs_c$n_comments_s1,contrib_fs_c$F7,method='spearman',use='complete.obs')
cor(contrib_bs_c$n_comments_s1,contrib_bs_c$F7,method='spearman',use='complete.obs')
# No correlation


# Analyze changes in the motivation factors of those who dropped out 
# in the first stage and those who stayed in the process
mot_factors_s2 = survey_2 %>% select(email, Q7_1.s2, Q7_2.s2, Q7_3.s2, Q7_4.s2, Q7_5.s2, Q7_6.s2, Q7_7.s2)
colnames(mot_factors_s2) = c('email','F1','F2','F3','F4','F5','F6','F7')
mot_factors_s2 = filter(mot_factors_s2, !is.na(F1) & !is.na(F2) & !is.na(F3) & !is.na(F4) & !is.na(F5) & !is.na(F6) & !is.na(F7))
f_mot_factors_s2 = mot_factors_s2
f_mot_factors_s2[,c(2:8)] = lapply(f_mot_factors_s2[,c(2:8)], 
                                   function(x) factor(x, levels=c('1','2','3','4','5','6','7'), ordered=T))
f_mot_factors_s2 = filter(f_mot_factors_s2, email %in% emails_contrib_fs_bs)


# Check changes in those who dropped out in the first stage (N=66)
f_mot_factors_s2_a = filter(f_mot_factors_s2, email %in% f_mot_factors_s1$email & email %in% contrib_fs$email)
f_mot_factors_s1_a = f_mot_factors_s1 %>% filter(email %in% f_mot_factors_s2_a$email & email %in% contrib_fs$email) %>% select(-group)
mot_contrib_fs = rbind(f_mot_factors_s1_a %>% mutate(survey='survey1'), 
                       f_mot_factors_s2_a %>% mutate(survey='survey2'))
nrow(mot_contrib_fs)/2
m_mot_contrib_fs = melt(mot_contrib_fs, id.vars='survey', 
                        measure.vars=c('F1','F2','F3','F4','F5','F6','F7'))
m_mot_contrib_fs$value = as.integer(m_mot_contrib_fs$value)
colnames(m_mot_contrib_fs) = c('survey','factor','score')
wilcox.test(score ~ survey, data=filter(m_mot_contrib_fs,factor=='F1'), paired=T)  # W = 768, p-value = 0.0002305 *
# apply wilcoxon signed rank in library coin since the exact p value cannot be calculated because of ties
wilcox_test(score ~ as.factor(survey), data=filter(m_mot_contrib_fs,factor=='F1'), 
            distribution="exact", paired=T)
2.6184/sqrt(nrow(filter(m_mot_contrib_fs,factor=='F1')))
wilcox.test(score ~ survey, data=filter(m_mot_contrib_fs,factor=='F2'), paired=T)  # W = 405.5, p-value = 0.001604 *
# apply wilcoxon in library coin since the exact p value cannot be calculated because of ties
wilcox_test(score ~ as.factor(survey), data=filter(m_mot_contrib_fs,factor=='F2'), 
            distribution="exact", paired=T)
2.2945/sqrt(nrow(filter(m_mot_contrib_fs,factor=='F2')))
wilcox.test(score ~ survey, data=filter(m_mot_contrib_fs,factor=='F3'), paired=T)  # W = 508, p-value = 0.1775
wilcox.test(score ~ survey, data=filter(m_mot_contrib_fs,factor=='F4'), paired=T)  # W = 500, p-value = 0.1031
wilcox.test(score ~ survey, data=filter(m_mot_contrib_fs,factor=='F5'), paired=T)  # W = 225, p-value = 0.878
wilcox.test(score ~ survey, data=filter(m_mot_contrib_fs,factor=='F6'), paired=T)  # W = 460, p-value = 0.2559
wilcox.test(score ~ survey, data=filter(m_mot_contrib_fs,factor=='F7'), paired=T)  # W = 420, p-value = 0.6701
ret_likert = likert(mot_contrib_fs[,2:8], grouping = mot_contrib_fs$survey)
summary(ret_likert)
# F1 and F2 decrease significantly

mean(filter(contrib, first_stage==1&second_stage==0)$n_comments_s1)
sd(filter(contrib, first_stage==1&second_stage==0)$n_comments_s1)
mean(filter(contrib, first_stage==1&second_stage==1)$n_comments_s1)
sd(filter(contrib, first_stage==1&second_stage==1)$n_comments_s1)
t.test(filter(contrib, first_stage==1&second_stage==0)$n_comments_s1,
       filter(contrib, first_stage==1&second_stage==1)$n_comments_s1)

# Check changes in those who sticked around until the second stage (N=17)
f_mot_factors_s2_a = filter(f_mot_factors_s2, email %in% f_mot_factors_s1$email & email %in% contrib_bs$email)
f_mot_factors_s1_a = f_mot_factors_s1 %>% filter(email %in% f_mot_factors_s2_a$email & email %in% contrib_bs$email) %>% select(-group)
mot_contrib_bs = rbind(f_mot_factors_s2_a %>% mutate(survey='survey1'), 
                       f_mot_factors_s1_a %>% mutate(survey='survey2'))
m_mot_contrib_bs = melt(mot_contrib_bs, id.vars='survey', 
                        measure.vars=c('F1','F2','F3','F4','F5','F6','F7'))
m_mot_contrib_bs$value = as.integer(m_mot_contrib_bs$value)
colnames(m_mot_contrib_bs) = c('survey','factor','score')
wilcox.test(score ~ survey, data=filter(m_mot_contrib_bs,factor=='F1'), paired=T)  # V = 9, p-value = 0.1169
wilcox.test(score ~ survey, data=filter(m_mot_contrib_bs,factor=='F2'), paired=T)  # V = 12.5, p-value = 0.4778
wilcox.test(score ~ survey, data=filter(m_mot_contrib_bs,factor=='F3'), paired=T)  # V = 12.5, p-value = 0.4722
wilcox.test(score ~ survey, data=filter(m_mot_contrib_bs,factor=='F4'), paired=T)  # V = 35, p-value = 0.4374
wilcox.test(score ~ survey, data=filter(m_mot_contrib_bs,factor=='F5'), paired=T)  # V = 1.5, p-value = 0.2693
wilcox.test(score ~ survey, data=filter(m_mot_contrib_bs,factor=='F6'), paired=T)  # V = 59, p-value = 0.3568
wilcox.test(score ~ survey, data=filter(m_mot_contrib_bs,factor=='F7'), paired=T)  # V = 16, p-value = 0.8211
ret_likert = likert(mot_contrib_bs[,2:8], grouping = mot_contrib_bs$survey)
summary(ret_likert)



# Analyse motivation factors of both groups the ones who dropped out
# and the ones who stayed around in the second stage
avg_comment_fs = mean(contrib$n_comments_s1)
f_mot_factors_s1_g = f_mot_factors_s1
f_mot_factors_s1_g = merge(contrib,f_mot_factors_s1_g,by.x='email',by.y='email')
f_mot_factors_s1_g$group = ifelse(f_mot_factors_s1_g$n_comments_s1 > avg_comment_fs, 'commenter','normal')
f_mot_factors_s1_g[,c(18:24)] = lapply(f_mot_factors_s1_g[,c(18:24)], 
                                       function(x) factor(x, levels=c('1','2','3','4','5','6','7'), ordered=T))
nrow(filter(f_mot_factors_s1_g, group=='commenter'))
nrow(filter(f_mot_factors_s1_g, group=='normal'))
ret_likert = likert(f_mot_factors_s1_g[,18:24], grouping = f_mot_factors_s1_g$group)
summary(ret_likert)
plot(ret_likert,type='bar')
# Compare difference in the score of the motivation factors between both groups (N=112)
m_f_mot_factors_s1 = melt(f_mot_factors_s1_g[,c(18:25)], id.vars='group', 
                          measure.vars=c('F1','F2','F3','F4','F5','F6','F7'))
m_f_mot_factors_s1$value = as.integer(m_f_mot_factors_s1$value)
colnames(m_f_mot_factors_s1) = c('group','factor','score')
wilcox.test(score ~ group, data=filter(m_f_mot_factors_s1,factor=='F1'))   # W = 1340, p-value = 0.947
wilcox.test(score ~ group, data=filter(m_f_mot_factors_s1,factor=='F2'))   # W = 1214, p-value = 0.578
wilcox.test(score ~ group, data=filter(m_f_mot_factors_s1,factor=='F3'))   # W = 1353, p-value = 0.6918
wilcox.test(score ~ group, data=filter(m_f_mot_factors_s1,factor=='F4'))   # W = 1247.5, p-value = 0.7646   
wilcox.test(score ~ group, data=filter(m_f_mot_factors_s1,factor=='F5'))   # W = 1204.5, p-value = 0.495
wilcox.test(score ~ group, data=filter(m_f_mot_factors_s1,factor=='F6'))   # W = 1269, p-value = 0.8823
wilcox.test(score ~ group, data=filter(m_f_mot_factors_s1,factor=='F7'))   # W = 1371, p-value = 0.5992


# Check changes in the motivation factors on 'normal' participants (N=53)
f_mot_factors_s2_g = f_mot_factors_s2
f_mot_factors_s2_g = merge(contrib,f_mot_factors_s2_g,by.x='email',by.y='email')
mot_contrib_np = rbind(f_mot_factors_s1_g[,-25] %>% mutate(survey='survey1') %>% filter(n_comments_s1<=avg_comment_fs & email %in% f_mot_factors_s2_g$email), 
                       f_mot_factors_s2_g %>% mutate(survey='survey2') %>% filter(n_comments_s1<=avg_comment_fs & email %in% f_mot_factors_s1_g$email))
m_mot_contrib_np = melt(mot_contrib_np[,c(18:25)], id.vars='survey', 
                        measure.vars=c('F1','F2','F3','F4','F5','F6','F7'))
m_mot_contrib_np$value = as.integer(m_mot_contrib_np$value)
colnames(m_mot_contrib_np) = c('survey','factor','score')
wilcox.test(score ~ survey, data=filter(m_mot_contrib_np,factor=='F1'), paired=T)  # V = 574, p-value = 9.23e-05  *
wilcox.test(score ~ survey, data=filter(m_mot_contrib_np,factor=='F2'), paired=T)  # V = 281, p-value = 0.001195  *
wilcox.test(score ~ survey, data=filter(m_mot_contrib_np,factor=='F3'), paired=T)  # V = 336, p-value = 0.3132
wilcox.test(score ~ survey, data=filter(m_mot_contrib_np,factor=='F4'), paired=T)  # V = 288, p-value = 0.2271
wilcox.test(score ~ survey, data=filter(m_mot_contrib_np,factor=='F5'), paired=T)  # V = 138, p-value = 1
wilcox.test(score ~ survey, data=filter(m_mot_contrib_np,factor=='F6'), paired=T)  # V = 299, p-value = 0.1917
wilcox.test(score ~ survey, data=filter(m_mot_contrib_np,factor=='F7'), paired=T)  # V = 361.5, p-value = 0.2586
mot_contrib_np[,c(18:24)] = lapply(mot_contrib_np[,c(18:24)], 
                                   function(x) factor(x, levels=c('1','2','3','4','5','6','7'), ordered=T))
ret_likert = likert(mot_contrib_np[,c(18:24)], grouping = mot_contrib_np$survey)
summary(ret_likert)


# Check changes in the motivation factors on 'commenter' participants (N=68)
mot_contrib_cp = rbind(f_mot_factors_s1_g[,-25] %>% mutate(survey='survey1') %>% filter(n_comments_s1>avg_comment_fs & email %in% f_mot_factors_s2_g$email), 
                       f_mot_factors_s2_g %>% mutate(survey='survey2') %>% filter(n_comments_s1>avg_comment_fs & email %in% f_mot_factors_s1_g$email))
m_mot_contrib_cp = melt(mot_contrib_cp[,c(18:25)], id.vars='survey', 
                        measure.vars=c('F1','F2','F3','F4','F5','F6','F7'))
m_mot_contrib_cp$value = as.integer(m_mot_contrib_cp$value)
colnames(m_mot_contrib_cp) = c('survey','factor','score')
wilcox.test(score ~ survey, data=filter(m_mot_contrib_cp,factor=='F1'), paired=T)  # V = 95, p-value = 0.1618
wilcox.test(score ~ survey, data=filter(m_mot_contrib_cp,factor=='F2'), paired=T)  # V = 65.5, p-value = 0.4183
wilcox.test(score ~ survey, data=filter(m_mot_contrib_cp,factor=='F3'), paired=T)  # V = 83, p-value = 0.1856
wilcox.test(score ~ survey, data=filter(m_mot_contrib_cp,factor=='F4'), paired=T)  # V = 103.5, p-value = 0.7288
wilcox.test(score ~ survey, data=filter(m_mot_contrib_cp,factor=='F5'), paired=T)  # V = 37.5, p-value = 0.3259
wilcox.test(score ~ survey, data=filter(m_mot_contrib_cp,factor=='F6'), paired=T)  # V = 97, p-value = 0.522
wilcox.test(score ~ survey, data=filter(m_mot_contrib_cp,factor=='F7'), paired=T)  # V = 33, p-value = 0.3834

# Analyse motivation factors of both groups the ones who dropped out
# and the ones who stayed around in the second stage
ideas_95q_fs = as.numeric(quantile(contrib$n_ideas_s1,probs = c(.95))[1])
f_mot_factors_s1_g = f_mot_factors_s1
f_mot_factors_s1_g = merge(contrib,f_mot_factors_s1_g,by.x='email',by.y='email')
f_mot_factors_s1_g$group = ifelse(f_mot_factors_s1_g$n_ideas_s1 >= ideas_95q_fs, 'ideators','normal')
f_mot_factors_s1_g[,c(18:24)] = lapply(f_mot_factors_s1_g[,c(18:24)], 
                                       function(x) factor(x, levels=c('1','2','3','4','5','6','7'), ordered=T))
nrow(filter(f_mot_factors_s1_g, group=='ideators'))  # N=17
nrow(filter(f_mot_factors_s1_g, group=='normal'))    # N=93
ret_likert = likert(f_mot_factors_s1_g[,18:24], grouping = f_mot_factors_s1_g$group)
summary(ret_likert)
plot(ret_likert,type='bar')
# Compare difference in the score of the motivation factors between both groups (N=112)
m_f_mot_factors_s1 = melt(f_mot_factors_s1_g[,c(18:25)], id.vars='group', 
                          measure.vars=c('F1','F2','F3','F4','F5','F6','F7'))
m_f_mot_factors_s1$value = as.integer(m_f_mot_factors_s1$value)
colnames(m_f_mot_factors_s1) = c('group','factor','score')
wilcox.test(score ~ group, data=filter(m_f_mot_factors_s1,factor=='F1'))   # W = 769.5, p-value = 0.8519
wilcox.test(score ~ group, data=filter(m_f_mot_factors_s1,factor=='F2'))   # W = 866.5, p-value = 0.4885
wilcox.test(score ~ group, data=filter(m_f_mot_factors_s1,factor=='F3'))   # W = 875, p-value = 0.4816
wilcox.test(score ~ group, data=filter(m_f_mot_factors_s1,factor=='F4'))   # W = 765.5, p-value = 0.8312
wilcox.test(score ~ group, data=filter(m_f_mot_factors_s1,factor=='F5'))   # W = 713.5, p-value = 0.4431
wilcox.test(score ~ group, data=filter(m_f_mot_factors_s1,factor=='F6'))   # W = 532.5, p-value = 0.03032 *
wilcox.test(score ~ group, data=filter(m_f_mot_factors_s1,factor=='F7'))   # W = 799.5, p-value = 0.942
# Significant difference in curiosity


# Check changes in the motivation factors on 'normal' ideators (N=67)
f_mot_factors_s2_g = f_mot_factors_s2
f_mot_factors_s2_g = merge(contrib,f_mot_factors_s2_g,by.x='email',by.y='email')
mot_contrib_np = rbind(f_mot_factors_s1_g[,-25] %>% mutate(survey='survey1') %>% filter(n_ideas_s1<ideas_95q_fs & email %in% f_mot_factors_s2_g$email), 
                       f_mot_factors_s2_g %>% mutate(survey='survey2') %>% filter(n_ideas_s1<ideas_95q_fs & email %in% f_mot_factors_s1_g$email))
nrow(mot_contrib_np)/2
m_mot_contrib_np = melt(mot_contrib_np[,c(18:25)], id.vars='survey', 
                        measure.vars=c('F1','F2','F3','F4','F5','F6','F7'))
m_mot_contrib_np$value = as.integer(m_mot_contrib_np$value)
colnames(m_mot_contrib_np) = c('survey','factor','score')
wilcox.test(score ~ survey, data=filter(m_mot_contrib_np,factor=='F1'), paired=T)  # V = 886.5, p-value = 2.586e-06  *
wilcox.test(score ~ survey, data=filter(m_mot_contrib_np,factor=='F2'), paired=T)  # V = 449.5, p-value = 0.0003991  *
wilcox.test(score ~ survey, data=filter(m_mot_contrib_np,factor=='F3'), paired=T)  # V = 555, p-value = 0.09674
wilcox.test(score ~ survey, data=filter(m_mot_contrib_np,factor=='F4'), paired=T)  # V = 397, p-value = 0.4637
wilcox.test(score ~ survey, data=filter(m_mot_contrib_np,factor=='F5'), paired=T)  # V = 202.5, p-value = 0.4947
wilcox.test(score ~ survey, data=filter(m_mot_contrib_np,factor=='F6'), paired=T)  # V = 433.5, p-value = 0.1536
wilcox.test(score ~ survey, data=filter(m_mot_contrib_np,factor=='F7'), paired=T)  # V = 419, p-value = 0.9045


# Check changes in the motivation factors on ideators (N=16)
f_mot_factors_s2_g = f_mot_factors_s2
f_mot_factors_s2_g = merge(contrib,f_mot_factors_s2_g,by.x='email',by.y='email')
mot_contrib_i = rbind(f_mot_factors_s1_g[,-25] %>% mutate(survey='survey1') %>% filter(n_ideas_s1>=ideas_95q_fs & email %in% f_mot_factors_s2_g$email), 
                      f_mot_factors_s2_g %>% mutate(survey='survey2') %>% filter(n_ideas_s1>=ideas_95q_fs & email %in% f_mot_factors_s1_g$email))
nrow(mot_contrib_i)/2
m_mot_contrib_i = melt(mot_contrib_i[,c(18:25)], id.vars='survey', 
                        measure.vars=c('F1','F2','F3','F4','F5','F6','F7'))
m_mot_contrib_i$value = as.integer(m_mot_contrib_i$value)
colnames(m_mot_contrib_i) = c('survey','factor','score')
wilcox.test(score ~ survey, data=filter(m_mot_contrib_i,factor=='F1'), paired=T)  # V = 12, p-value = 0.4291
wilcox.test(score ~ survey, data=filter(m_mot_contrib_i,factor=='F2'), paired=T)  # V = 13.5, p-value = 1
wilcox.test(score ~ survey, data=filter(m_mot_contrib_i,factor=='F3'), paired=T)  # V = 15, p-value = 0.9317
wilcox.test(score ~ survey, data=filter(m_mot_contrib_i,factor=='F4'), paired=T)  # V = 51.5, p-value = 0.3229
wilcox.test(score ~ survey, data=filter(m_mot_contrib_i,factor=='F5'), paired=T)  # V = 13, p-value = 0.9319
wilcox.test(score ~ survey, data=filter(m_mot_contrib_i,factor=='F6'), paired=T)  # V = 35.5, p-value = 0.5007
wilcox.test(score ~ survey, data=filter(m_mot_contrib_i,factor=='F7'), paired=T)  # V = 19.5, p-value = 0.3929



# Run logistic regression trying to see if there are factors
# that can predict whether or not a participant goes to the
# second stage
lr_contrib = contrib %>% filter(first_stage==1) %>% 
  select(n_ideas_s1, n_comments_s1, n_votes_s1, second_stage)
# Split dataset
lr_contrib$second_stage = as.factor(lr_contrib$second_stage)
train = lr_contrib[c(1:round(80*nrow(lr_contrib)/100,0)),]
test = lr_contrib[c(1:round(20*nrow(lr_contrib)/100,0)),]
# Train algorithm
lr_model = glm(second_stage ~ .,family=binomial(link='logit'), data=train)
summary(lr_model)
# Converting predictors
exp(lr_model$coeff)
# Check the table of deviance
anova(lr_model, test="Chisq")
# Test model performance
# Mean of errors
fitted_res = predict(lr_model,newdata=select(test,-second_stage),type='response')
fitted_res = ifelse(fitted_res > 0.5,1,0)
model_error = mean(fitted_res != test$second_stage)
print(paste('Accuracy',1-model_error))


# Actions during the day of registration
activities_reg_day = matrix(NA,ncol=67)
for (p_email in contrib$email) {
  part_reg_day = filter(contrib, email==p_email)$registration_datetime
  p_activities = ideas_1_stage %>% filter(author_email==p_email) %>% select(author_email, creation_datetime) %>% mutate(type='idea_s1')
  p_activities = rbind(p_activities, 
                       ideas_2_stage %>% filter(author_email==p_email) %>% select(author_email, creation_datetime) %>% mutate(type='idea_s2'))
  p_activities = rbind(p_activities, 
                       comments_1_stage %>% filter(author_email==p_email) %>% select(author_email, creation_datetime) %>% mutate(type='comment_s1'))
  p_activities = rbind(p_activities, 
                       comments_2_stage %>% filter(author_email==p_email) %>% select(author_email, creation_datetime) %>% mutate(type='comment_s2'))
  p_activities = rbind(p_activities, 
                       votes_1_stage %>% filter(author_email==p_email) %>% select(author_email, creation_datetime) %>% mutate(type='vote_s1'))
  p_activities = rbind(p_activities, 
                       votes_2_stage %>% filter(author_email==p_email) %>% select(author_email, creation_datetime) %>% mutate(type='vote_s2'))
  lifespan = compute_participant_lifespan(p_activities,part_reg_day)
  days_and_activities = compute_participant_days_of_activity(p_activities)
  p_activities = arrange(p_activities,desc(creation_datetime))
  first_activity = p_activities[nrow(p_activities),'creation_datetime']
  activities_first_w = matrix(NA,ncol=16,nrow=4)
  for (i in 0:15) {
    day_reg_activties = filter(p_activities,as.numeric(round(abs(difftime(part_reg_day, creation_datetime, units="days")),0))<=i)
    n_ideas_s1 = nrow(filter(day_reg_activties,type=='idea_s1'))
    n_ideas_s2 = nrow(filter(day_reg_activties,type=='idea_s2'))
    n_ideas = n_ideas_s1 + n_ideas_s2
    activities_first_w[1,i+1] = n_ideas
    n_comments_s1 = nrow(filter(day_reg_activties,type=='comment_s1'))
    n_comments_s2 = nrow(filter(day_reg_activties,type=='comment_s2'))
    n_comments = n_comments_s1 + n_comments_s2
    activities_first_w[2,i+1] = n_comments
    n_votes_s1 = nrow(filter(day_reg_activties,type=='vote_s1'))
    n_votes_s2 = nrow(filter(day_reg_activties,type=='vote_s2'))
    n_votes = n_votes_s1 + n_votes_s2
    activities_first_w[3,i+1] = n_votes
    n = n_ideas+n_comments+n_votes
    activities_first_w[4,i+1] = n
  }
  
  activities_reg_day = rbind(activities_reg_day, 
                             c(p_email,as.vector(activities_first_w),
                               lifespan,nrow(days_and_activities)))
}
activities_reg_day = activities_reg_day[-1,]
num_activities_reg_day = activities_reg_day[,-1]
num_activities_reg_day = apply(num_activities_reg_day,2,function(x) as.numeric(x))
num_activities_reg_day = as.data.frame(num_activities_reg_day)
num_activities_reg_day = filter(num_activities_reg_day,V65!=-1)
num_activities_reg_day = mutate(num_activities_reg_day,V67=num_activities_reg_day$V66/num_activities_reg_day$V65)
for (j in 1:64) {
  rsq = cor(log(num_activities_reg_day[,j]),log(num_activities_reg_day[,65]),method='spearman')
  if (rsq >= 0.50) {
    print(paste('Correlation column',j,'lifespan:',rsq))  
  }
  rsq = cor(log(num_activities_reg_day[,j]),log(num_activities_reg_day[,66]),method='spearman')
  if (rsq >= 0.50) {
    print(paste('Correlation column',j,'days_of_activity:',rsq))  
  }
  rsq = cor(log(num_activities_reg_day[,j]),num_activities_reg_day[,67],method='spearman')
  if (rsq >= 0.50) {
    print(paste('Correlation column',j,'percentage of active days:',rsq)) 
  }
}

plot()

lm_a = lm(V66~V32,data=num_activities_reg_day)
summary(lm_a)

summary(num_activities_reg_day$V66)

num_activities_reg_day = arrange(num_activities_reg_day, desc(V66))
View(select(num_activities_reg_day,V30,V66))
plot(log(num_activities_reg_day[,30]),log(num_activities_reg_day[,66]))

cor.test(num_activities_reg_day[,29],num_activities_reg_day[,66],method='spearman')


activities_reg_day = arrange(activities_reg_day, desc(days_of_activity))
activities_reg_day$per_days_active = activities_reg_day$days_of_activity/activities_reg_day$lifespan
activities_reg_day = arrange(activities_reg_day, desc(per_days_active))
# Average of lifespan
mean(activities_reg_day$lifespan)  # Output: 40 days
summary(activities_reg_day$lifespan)
# Average of active days
mean(activities_reg_day$per_days_active*100)  # Output: 10% of the lifespan


# Correlations between level of activity and lifespan
print(paste('Level of activity and Lifespan, R^2:',cor(activities_reg_day$n,activities_reg_day$lifespan,method='spearman')))
# Correlations between level of activity and active days
print(paste('Level of activity and Lifespan, R^2:',cor(activities_reg_day$n,activities_reg_day$per_days_active,method='spearman')))

