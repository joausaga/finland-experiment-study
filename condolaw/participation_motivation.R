#######
# An analysis of the relationship between level of participation and changes in 
# the motivational factors
#######

# Load libraries
library(ggplot2)
library(dplyr)
library(stringr)
source('utils.R')

# Reading data
users_camp_other = read.csv("./data/users_camp_other.csv",header=T,sep=";",stringsAsFactors=F) # outside the pre-defined campaigns
users_camp_vies = read.csv("./data/users_camp_vies.csv",header=T,sep=";",stringsAsFactors=F)   # communication campaign (vies)
users_camp_hall = read.csv("./data/users_camp_hall.csv",header=T,sep=";",stringsAsFactors=F)   # governance campaign (hall)
users_camp_erim = read.csv("./data/users_camp_erim.csv",header=T,sep=";",stringsAsFactors=F)   # conflict campaign (erim)
motivation_pre = read.csv("./data/survey1-condlaw-no-duplicates.csv",header=T, sep=",",stringsAsFactors=F)
motivation_pos = read.csv("./data/survey2-condlaw-no-duplicates.csv",header=T, sep=",",stringsAsFactors=F)

# Data preparation
participants = users_camp_other
participants = create_joint_dataset(users_camp_vies, participants)
participants = create_joint_dataset(users_camp_hall, participants)
participants = create_joint_dataset(users_camp_erim, participants)
participants = participants %>% select(-id)
colnames(motivation_pre) = c('email', 'datetime', 'o1s1', 'o2s1', 'o3s1', 'o4s1', 'o5s1', 'o6s1', 'o7s1')
colnames(motivation_pos) = c('email', 'datetime', 'o1s2', 'o2s2', 'o3s2', 'o4s2', 'o5s2', 'o6s2', 'o7s2', 'o8s2')
motivation_pre = motivation_pre %>% filter(email!='') %>% filter(!is.na(o1s1)) %>% select(-datetime)
motivation_pos = motivation_pos %>% filter(email!='') %>% filter(!is.na(o1s2)) %>% select(-datetime)
motivation_both = merge(motivation_pre, motivation_pos, by.x='email', by.y='email', all=F)
merged_ds = merge(participants, motivation_both, by.x='email', by.y='email')

# Create clusters of level of participation
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
check_rel(merged_ds %>% select(o1s1:o8s2,activity_level) %>% filter(!is.na(activity_level)),'activity_level')