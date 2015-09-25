#######
# Considering the three surveys an analysis was conducted to study 
# the relationship between the level of participation and changes in 
# the motivational factors
#######

# Load libraries
library(ggplot2)
library(dplyr)
library(stringr)
source('utils.R')

# Reading data
mot_survey_1 = read.csv("./data/survey1-condlaw-no-duplicates.csv",header=T, sep=",",stringsAsFactors=F)
mot_survey_2 = read.csv("./data/survey2-condlaw-no-duplicates.csv",header=T, sep=",",stringsAsFactors=F)
mot_survey_3 = read.csv("./data/survey3-condlaw-no-duplicates2.csv",header=T, sep=",",stringsAsFactors=F)
users_camp_other = read.csv("./data/users_camp_other.csv",header=T,sep=";",stringsAsFactors=F) # outside the pre-defined campaigns
users_camp_vies = read.csv("./data/users_camp_vies.csv",header=T,sep=";",stringsAsFactors=F)   # communication campaign (vies)
users_camp_hall = read.csv("./data/users_camp_hall.csv",header=T,sep=";",stringsAsFactors=F)   # governance campaign (hall)
users_camp_erim = read.csv("./data/users_camp_erim.csv",header=T,sep=";",stringsAsFactors=F)   # conflict campaign (erim)
users_camp_other_survey_3 = read.csv("./data/users_camp_other_survey3.csv",header=T,sep=",",stringsAsFactors=F)
users_camp_1_survey_3 = read.csv("./data/users_camp_1_survey3.csv",header=T,sep=",",stringsAsFactors=F)
users_camp_2_survey_3 = read.csv("./data/users_camp_2_survey3.csv",header=T,sep=",",stringsAsFactors=F)
users_camp_0_survey_3 = read.csv("./data/users_camp_0_survey3.csv",header=T,sep=",",stringsAsFactors=F)

# Data preparation
colnames(mot_survey_1) = c('email', 'datetime', 'o1s1', 'o2s1', 'o3s1', 'o4s1', 'o5s1', 'o6s1', 'o7s1')
colnames(mot_survey_2) = c('email', 'datetime', 'o1s2', 'o2s2', 'o3s2', 'o4s2', 'o5s2', 'o6s2', 'o7s2', 'o8s2')
colnames(mot_survey_3) = c('email', 'datetime', 'o1s3', 'o2s3', 'o3s3', 'o4s3', 'o5s3', 'o6s3', 'o7s3', 'o8s3')
mot_survey_1 = mot_survey_1 %>% filter(email!='') %>% filter(!is.na(o1s1)) %>% filter(o1s1!='') %>% select(-datetime)
mot_survey_2 = mot_survey_2 %>% filter(email!='') %>% filter(!is.na(o1s2)) %>% filter(o1s2!='') %>% select(-datetime)
mot_survey_3 = mot_survey_3 %>% filter(email!='') %>% filter(!is.na(o1s3)) %>% filter(o1s3!='') %>% select(-datetime)
participants = users_camp_other
participants = create_joint_dataset(users_camp_vies, participants)
participants = create_joint_dataset(users_camp_hall, participants)
participants = create_joint_dataset(users_camp_erim, participants)
participants = create_joint_dataset(users_camp_other_survey_3, participants)
participants = create_joint_dataset(users_camp_0_survey_3, participants)
participants = create_joint_dataset(users_camp_1_survey_3, participants)
participants = create_joint_dataset(users_camp_2_survey_3, participants)
participants = participants %>% select(-id)
participants = participants %>% filter(email != '')
all_surveys = merge(mot_survey_1, mot_survey_2, by.x='email', by.y='email', all=F)
all_surveys = merge(all_surveys, mot_survey_3, by.x='email', by.y='email', all=F)
all_surveys = all_surveys %>% select(-o6s1, -o6s2, -o8s2, -o7s3, -o8s3)
colnames(all_surveys) = c('email', 'o1s1', 'o2s1', 'o3s1', 'o4s1', 'o5s1', 'o6s1',
                          'o1s2', 'o2s2', 'o3s2', 'o4s2', 'o5s2', 'o6s2',
                          'o1s3', 'o2s3', 'o3s3', 'o4s3', 'o5s3', 'o6s3')
merged_ds = merge(participants, all_surveys, by.x='email', by.y='email')

merged_ds = merge(participants, mot_survey_3, by.x='email', by.y='email')

# Create clusters by level of participation
observers = merged_ds %>% filter(ideas==0&votes==0&comments==0) # nothing, only observe what happened
voters = merged_ds %>% filter(ideas==0&votes!=0&comments==0)    # only vote
mid_cont = merged_ds %>% filter((ideas==0&votes!=0&comments!=0)|
                                (ideas!=0&votes==0&comments!=0)|
                                (ideas!=0&votes!=0&comments==0)|
                                (ideas!=0&votes==0&comments==0)|
                                (ideas==0&votes==0&comments!=0))   
full_cont = merged_ds %>% filter(ideas!=0&votes!=0&comments!=0) # suggest ideas, comment and vote

# Annotate main dataset with the level of participation
merged_ds$activity_level = ifelse(merged_ds[,'email'] %in% observers$email, 'observer', NA)
merged_ds$activity_level = ifelse(merged_ds[,'email'] %in% voters$email, 'voters', merged_ds[,'activity_level'])
merged_ds$activity_level = ifelse(merged_ds[,'email'] %in% mid_cont$email, 'medium_contributor', merged_ds[,'activity_level'])
merged_ds$activity_level = ifelse(merged_ds[,'email'] %in% full_cont$email, 'full_contributor', merged_ds[,'activity_level'])
check_rel_three_surveys(merged_ds %>% select(o1s1:o6s3,activity_level) %>% filter(!is.na(activity_level)),'activity_level',6)