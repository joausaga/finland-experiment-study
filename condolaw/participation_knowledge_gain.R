#######
# An analysis of how the levels of participation
# affect changes in knowledge gain
#######

# Load libraries
library(dplyr)

# Reading data
raw_survey_1 = read.csv("./data/survey1-condlaw-complete.csv",header=T, sep=",",stringsAsFactors=F)
raw_survey_2 = read.csv("./data/survey2-condlaw-complete.csv",header=T, sep=",",stringsAsFactors=F)
raw_survey_3 = read.csv("./data/survey3-condlaw2-complete.csv",header=T, sep=",",stringsAsFactors=F)
users_camp_other = read.csv("./data/users_camp_other.csv",header=T,sep=";",stringsAsFactors=F) # outside the pre-defined campaigns
users_camp_vies = read.csv("./data/users_camp_vies.csv",header=T,sep=";",stringsAsFactors=F)   # communication campaign (vies)
users_camp_hall = read.csv("./data/users_camp_hall.csv",header=T,sep=";",stringsAsFactors=F)   # governance campaign (hall)
users_camp_erim = read.csv("./data/users_camp_erim.csv",header=T,sep=";",stringsAsFactors=F)   # conflict campaign (erim)
users_camp_other_survey_3 = read.csv("./data/users_camp_other_survey3.csv",header=T,sep=",",stringsAsFactors=F)
users_camp_1_survey_3 = read.csv("./data/users_camp_1_survey3.csv",header=T,sep=",",stringsAsFactors=F)
users_camp_2_survey_3 = read.csv("./data/users_camp_2_survey3.csv",header=T,sep=",",stringsAsFactors=F)
users_camp_0_survey_3 = read.csv("./data/users_camp_0_survey3.csv",header=T,sep=",",stringsAsFactors=F)

# Data preparation
survey_1 = raw_survey_1 %>% select(email,Q6_3,Q6_4,Q6_5) %>% filter(!is.na(Q6_3))
survey_2 = raw_survey_2 %>% select(email,Q11_6,Q11_7,Q11_8) %>% filter(!is.na(Q11_6))
survey_3 = raw_survey_3 %>% select(email,Q11_6,Q11_7,Q11_8) %>% filter(!is.na(Q11_6))
survey_1$survey = 'survey_1'
survey_2$survey = 'survey_2'
survey_3$survey = 'survey_3'
all_surveys = merge(survey_1, survey_2, by.x='email', by.y='email', all=F)
all_surveys = merge(all_surveys, survey_3, by.x='email', by.y='email', all=F)

survey_1 = survey_1 %>% filter(email %in% all_surveys$email) %>% select(-email)
survey_2 = survey_2 %>% filter(email %in% all_surveys$email) %>% select(-email)
survey_3 = survey_3 %>% filter(email %in% all_surveys$email) %>% select(-email)
colnames(survey_1) = c('o1','o2','o3', 'group')
colnames(survey_2) = c('o1','o2','o3', 'group')
colnames(survey_3) = c('o1','o2','o3', 'group')
survey_1$o1[survey_1$o1==1] = 'yes'
survey_1$o1[survey_1$o1==2] = 'no'
survey_1$o2[survey_1$o2==1] = 'yes'
survey_1$o2[survey_1$o2==2] = 'no'
survey_1$o3[survey_1$o3==1] = 'yes'
survey_1$o3[survey_1$o3==2] = 'no'
survey_2$o1[survey_2$o1==1] = 'yes'
survey_2$o1[survey_2$o1==2] = 'no'
survey_2$o2[survey_2$o2==1] = 'yes'
survey_2$o2[survey_2$o2==2] = 'no'
survey_2$o3[survey_2$o3==1] = 'yes'
survey_2$o3[survey_2$o3==2] = 'no'
survey_3$o1[survey_3$o1==1] = 'yes'
survey_3$o1[survey_3$o1==2] = 'no'
survey_3$o2[survey_3$o2==1] = 'yes'
survey_3$o2[survey_3$o2==2] = 'no'
survey_3$o3[survey_3$o3==1] = 'yes'
survey_3$o3[survey_3$o3==2] = 'no'
dataset = rbind(survey_1,survey_2,survey_3)
