####
## In this script we study
## the factors that might influence 
## the users to keep participating
## on the platform during the
## entire crowdsourcing process
####

source('utils.R')
library(dplyr)

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

# Create tables containing participation per user
# Ideas
ideas_by_authors = arrange(activity_by_author(ideas_1_stage,ideas_2_stage,c('email','n_ideas_s1','n_ideas_s2','n_ideas')), 
                           desc(n_ideas))
# Comments
comments_by_authors = arrange(activity_by_author(comments_1_stage,comments_2_stage,c('email','n_comments_s1','n_comments_s2','n_comments')), 
                              desc(n_comments))
# Votes
votes_by_authors = arrange(activity_by_author(votes_1_stage,votes_2_stage,c('email','n_votes_s1','n_votes_s2','n_votes')), 
                           desc(n_votes))

# Merge previous tables
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

# Fill missing values
num_col = ncol(participant_activities)
participant_activities[,c(2:num_col)] = lapply(participant_activities[,c(2:num_col)], function(x) ifelse(is.na(x),0,x))

# Add new columns to the merged table and order table from top to less contributors
participant_activities[,'n'] = apply(participant_activities[,c(2:num_col)], 1, function(x) x['n_ideas']+x['n_comments']+x['n_votes'])
participant_activities = arrange(participant_activities, desc(n))

# Add registration day to the merged table
participant_activities = merge(
  participant_activities,
  users,
  by.x = 'email',
  by.y = 'emai',
  all = T
)

# Remove participants without email address
participant_activities = filter(participant_activities, email != '')

# Fill missing values
participant_activities[,c(2:11)] = lapply(participant_activities[,c(2:11)], function(x) ifelse(is.na(x),0,x))

# Get rid off irrelevant columns
participant_activities = select(participant_activities, -id)

# Re-name registration day column
colnames(participant_activities)[12] = 'registration_datetime'

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

# Run logistic regression trying to see if there are factors
# that can predict whether or not a participant goes to the
# second stage of the crowdsourcing processs
lr_contrib = contrib %>% filter(first_stage==1) %>% 
  select(n_ideas_s1, n_comments_s1, n_votes_s1, second_stage)
# Split data in train and test set
lr_contrib$second_stage = as.factor(lr_contrib$second_stage)
train = lr_contrib[c(1:round(80*nrow(lr_contrib)/100,0)),]
test = lr_contrib[c(1:round(20*nrow(lr_contrib)/100,0)),]
# Train algorithm
lr_model = glm(second_stage ~ .,family=binomial(link='logit'), data=train)
summary(lr_model)
# Converting predictors
exp(lr_model$coeff)
# Test model performance
# Mean of errors
fitted_res = predict(lr_model,newdata=select(test,-second_stage),type='response')
fitted_res = ifelse(fitted_res > 0.5,1,0)
model_error = mean(fitted_res != test$second_stage)
print(paste('Accuracy',1-model_error))
