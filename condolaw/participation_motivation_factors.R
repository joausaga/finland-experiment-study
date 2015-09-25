#######
# An analysis of the relationship between level of 
# participation and motivational factors
#######

# Load libraries
library(dplyr)
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

participation_s1s2 = users_camp_other
participation_s1s2 = create_joint_dataset(users_camp_vies, participation_s1s2)
participation_s1s2 = create_joint_dataset(users_camp_hall, participation_s1s2)
participation_s1s2 = create_joint_dataset(users_camp_erim, participation_s1s2)
participation_s1s2 = participation_s1s2 %>% select(-id)
participation_s1s2 = participation_s1s2 %>% filter(email != '')

participation_s3 = users_camp_other_survey_3
participation_s3 = create_joint_dataset(users_camp_0_survey_3, participation_s3)
participation_s3 = create_joint_dataset(users_camp_1_survey_3, participation_s3)
participation_s3 = create_joint_dataset(users_camp_2_survey_3, participation_s3)
participation_s3 = participation_s3 %>% select(-id)
participation_s3 = participation_s3 %>% filter(email != '')

# Analysis of level of participation and motivation factors in survey 1
participation_s1s2 = filter(participation_s1s2, email %in% mot_survey_1$email)
participation_s1s2 = mutate(participation_s1s2,content=ideas+votes+comments)
participation_s1s2 = mutate(participation_s1s2,ideas_votes=ideas+votes)
participation_s1s2 = mutate(participation_s1s2,ideas_comments=ideas+comments)
participation_s1s2 = mutate(participation_s1s2,votes_comments=votes+comments)
for (type_participation in c('content', 'ideas_votes', 'ideas_comments', 'votes_comments', 'votes', 'ideas', 'comments')) {
  for (factor in c('o1s1', 'o2s1', 'o3s1', 'o4s1', 'o5s1', 'o6s1', 'o7s1')) {
    ret = prepare_data(participation_s1s2, mot_survey_1, type_participation, factor)
    cor_coef = cor(ret$ds1,ret$ds2)
    print(paste('The correlation between the production of ',type_participation,' and the factor ',factor,' is ',cor_coef,sep=''))
    if (cor_coef > 0.50) {
      t_test = cor.test(ret$ds1,ret$ds2)
      if (t_test$p.value < 0.05) {
        print(paste('Found a high and significant correlation!, p-value: ',t_test$p.value,sep=''))
        
      } else {
        print(paste('Found a high but not significant correlation, p-value: ',t_test$p.value,sep=''))
      }
    }
  } 
}

# Analysis of level of participation and motivation factors in survey 2
participation_s1s2 = users_camp_other
participation_s1s2 = create_joint_dataset(users_camp_vies, participation_s1s2)
participation_s1s2 = create_joint_dataset(users_camp_hall, participation_s1s2)
participation_s1s2 = create_joint_dataset(users_camp_erim, participation_s1s2)
participation_s1s2 = participation_s1s2 %>% select(-id)
participation_s1s2 = participation_s1s2 %>% filter(email != '')
participation_s1s2 = filter(participation_s1s2, email %in% mot_survey_2$email)
participation_s1s2 = mutate(participation_s1s2,content=ideas+votes+comments)
participation_s1s2 = mutate(participation_s1s2,ideas_votes=ideas+votes)
participation_s1s2 = mutate(participation_s1s2,ideas_comments=ideas+comments)
participation_s1s2 = mutate(participation_s1s2,votes_comments=votes+comments)
for (type_participation in c('content', 'ideas_votes', 'ideas_comments', 'votes_comments', 'votes', 'ideas', 'comments')) {
  for (factor in c('o1s2', 'o2s2', 'o3s2', 'o4s2', 'o5s2', 'o6s2', 'o7s2','o8s2')) {
    ret = prepare_data(participation_s1s2, mot_survey_2, type_participation, factor)
    cor_coef = cor(ret$ds1,ret$ds2)
    print(paste('The correlation between the production of ',type_participation,' and the factor ',factor,' is ',cor_coef,sep=''))
    if (cor_coef > 0.50) {
      t_test = cor.test(ret$ds1,ret$ds2)
      if (t_test$p.value < 0.05) {
        print(paste('Found a high and significant correlation!, p-value: ',t_test$p.value,sep=''))
        
      } else {
        print(paste('Found a high but not significant correlation, p-value: ',t_test$p.value,sep=''))
      }
    }
  } 
}

# Analysis of level of participation and motivation factors in survey 3
participation_s3 = filter(participation_s3, email %in% mot_survey_3$email)
participation_s3 = mutate(participation_s3,content=ideas+votes+comments)
participation_s3 = mutate(participation_s3,ideas_votes=ideas+votes)
participation_s3 = mutate(participation_s3,ideas_comments=ideas+comments)
participation_s3 = mutate(participation_s3,votes_comments=votes+comments)
for (type_participation in c('content', 'ideas_votes', 'ideas_comments', 'votes_comments', 'votes', 'ideas', 'comments')) {
  for (factor in c('o1s3', 'o2s3', 'o3s3', 'o4s3', 'o5s3', 'o6s3', 'o7s3', 'o8s3')) {
    ret = prepare_data(participation_s3, mot_survey_3, type_participation, factor)
    cor_coef = cor(ret$ds1,ret$ds2)
    print(paste('The correlation between the production of ',type_participation,' and the factor ',factor,' is ',cor_coef,sep=''))
    if (cor_coef > 0.50) {
      t_test = cor.test(ret$ds1,ret$ds2)
      if (t_test$p.value < 0.05) {
        print(paste('Found a high and significant correlation!, p-value: ',t_test$p.value,sep=''))
        
      } else {
        print(paste('Found a high but not significant correlation, p-value: ',t_test$p.value,sep=''))
      }
    }
  } 
}