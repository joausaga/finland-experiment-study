#######
# An analysis of participants' civic activity
#######

# Load libraries
library(dplyr)
source('utils.R')

# Functions
count_activities = function(participant_reg) {
  activities = 0

  for (i in (2:15)) {
    activities = activities + ifelse(is.na(participant_reg[[i]]),0,1)  
  }
  
  return (activities)
}

# Load data
raw_survey_1 = read.csv("./data/survey1-condlaw-complete.csv",header=T, sep=",", stringsAsFactors=F)
raw_survey_2 = read.csv("./data/survey2-condlaw-complete.csv",header=T, sep=",", stringsAsFactors=F)
raw_survey_3 = read.csv("./data/survey3-condlaw2-complete.csv",header=T, sep=",", stringsAsFactors=F)
demographic = read.csv("./data/demographic-condlaw-no-duplicates.csv",header=T,sep=",",stringsAsFactors=F)
demographic_columns = read.table("./data/demographic-condlaw-columns.txt")
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
colnames(demographic) = demographic_columns[,2]
civic_activity = select(demographic, email, activity_oped:activity_campaign)
civic_activity = mutate(civic_activity, activities=count_activities(civic_activity))
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

# Analyze associations between civic activism and motivation factors (survey 1)
mot_survey_1 = filter(mot_survey_1, email %in% civic_activity$email)
civic_activity_ = filter(civic_activity, email %in% mot_survey_1$email)
factors_s1 = c('o1s1', 'o2s1', 'o3s1', 'o4s1', 'o5s1', 'o6s1', 'o7s1')
find_correlations(mot_survey_1,civic_activity_,factors_s1,c('activities'))

# Analyze associations between civic activism and motivation factors (survey 2)
mot_survey_2 = filter(mot_survey_2, email %in% civic_activity$email)
civic_activity_ = filter(civic_activity, email %in% mot_survey_2$email)
factors_s2 = c('o1s2', 'o2s2', 'o3s2', 'o4s2', 'o5s2', 'o6s2', 'o7s2', 'o8s2')
find_correlations(mot_survey_2,civic_activity_,factors_s2,c('activities'))

# Analyze associations between civic activism and motivation factors (survey 3)
mot_survey_3 = filter(mot_survey_3, email %in% civic_activity$email)
civic_activity_ = filter(civic_activity, email %in% mot_survey_3$email)
factors_s3 = c('o1s3', 'o2s3', 'o3s3', 'o4s3', 'o5s3', 'o6s3', 'o7s3', 'o8s3')
find_correlations(mot_survey_3,civic_activity_,factors_s3,c('activities'))

# Analyze correlation between participants' civic activity and level of participations (survey 1 and 2)
participation_s1s2 = mutate(participation_s1s2,content=ideas+votes+comments)
participation_s1s2 = mutate(participation_s1s2,ideas_votes=ideas+votes)
participation_s1s2 = mutate(participation_s1s2,ideas_comments=ideas+comments)
participation_s1s2 = mutate(participation_s1s2,votes_comments=votes+comments)
participation_s1s2 = filter(participation_s1s2, email %in% civic_activity$email)
participation_types = c('content', 'ideas_votes', 'ideas_comments', 'votes_comments', 'votes', 'ideas', 'comments')
find_correlations(participation_s1s2,civic_activity,participation_types,c('activities'))

# Analyze correlation between participants' civic activity and level of participations (survey 3)
participation_s3 = mutate(participation_s3,content=ideas+votes+comments)
participation_s3 = mutate(participation_s3,ideas_votes=ideas+votes)
participation_s3 = mutate(participation_s3,ideas_comments=ideas+comments)
participation_s3 = mutate(participation_s3,votes_comments=votes+comments)
participation_s3 = filter(participation_s3, email %in% civic_activity$email)
find_correlations(participation_s3,civic_activity,participation_types,c('activities'))

# Analyze correlation between participants' civic activity and self-efficacy (survey 1)
cols_se = c('Q3_1','Q3_2','Q4_1','Q4_2','Q4_3','Q4_4','Q4_5','Q5_1','Q5_3','Q5_4','Q5_5','Q5_6','Q5_7','Q5_14')
se_survey_1 = raw_survey_1 %>% select(email,Q3_1,Q3_2,Q4_1,Q4_2,Q4_3,Q4_4,Q4_5,Q5_1,Q5_3,Q5_4,Q5_5,Q5_6,Q5_7,Q5_14) %>% filter(!is.na(Q3_1))
se_survey_1 = filter(se_survey_1, email %in% civic_activity$email)
civic_activity_ = filter(civic_activity, email %in% se_survey_1$email)
find_correlations(se_survey_1,civic_activity_,cols_se,c('activities'))

# Analyze correlation between participants' civic activity and self-efficacy (survey 3)
cols_se = c('Q3_2','Q6_4','Q6_5','Q7_2')
se_survey_3 = raw_survey_3 %>% select(email,Q3_2,Q6_4,Q6_5,Q7_2) %>% filter(!is.na(Q3_2))
se_survey_3 = filter(se_survey_3, email %in% civic_activity$email)
civic_activity_ = filter(civic_activity, email %in% se_survey_3$email)
find_correlations(se_survey_3,civic_activity_,cols_se,c('activities'))

# Analyze correlation between participants' civic activity and learning (survey 1)
cols_le = c('Q6_3','Q6_4','Q6_5')
le_survey_1 = raw_survey_1 %>% select(email,Q6_3,Q6_4,Q6_5) %>% filter(!is.na(Q6_3))
le_survey_1 = filter(le_survey_1, email %in% civic_activity$email)
civic_activity_ = filter(civic_activity, email %in% le_survey_1$email)
find_correlations(le_survey_1,civic_activity_,cols_le,c('activities'))

# Analyze correlation between participants' civic activity and learning (survey 2)
cols_le = c('Q11_6','Q11_7','Q11_8')
le_survey_2 = raw_survey_2 %>% select(email,Q11_6,Q11_7,Q11_8) %>% filter(!is.na(Q11_6))
le_survey_2 = filter(le_survey_2, email %in% civic_activity$email)
civic_activity_ = filter(civic_activity, email %in% le_survey_2$email)
find_correlations(le_survey_2,civic_activity_,cols_le,c('activities'))

# Analyze correlation between participants' civic activity and learning (survey 3)
cols_le = c('Q11_6','Q11_7','Q11_8')
le_survey_3 = raw_survey_3 %>% select(email,Q11_6,Q11_7,Q11_8) %>% filter(!is.na(Q11_6))
le_survey_3 = filter(le_survey_3, email %in% civic_activity$email)
civic_activity_ = filter(civic_activity, email %in% le_survey_3$email)
find_correlations(le_survey_3,civic_activity_,cols_le,c('activities'))

# Analyze correlation between trust in the law-making process and civic activity
cols_t = c('Q9_7','Q9_8','Q9_9','Q9_10','Q9_12')
t_survey_1 = raw_survey_1 %>% select(email,Q9_7,Q9_8,Q9_9,Q9_10,Q9_12) %>% filter(!is.na(Q9_7))
t_survey_1 = filter(t_survey_1, email %in% civic_activity$email)
civic_activity_ = filter(civic_activity, email %in% t_survey_1$email)
find_correlations(t_survey_1,civic_activity_,cols_t,c('activities'))

# Analyze associations between civic activity and changes in the motivation factors
ch_mot_factors = merge(mot_survey_1,mot_survey_2,by.x='email',by.y='email')
ch_mot_factors = merge(ch_mot_factors,mot_survey_3,by.x='email',by.y='email')
ch_mot_factors = select(ch_mot_factors, -o6s1, -o6s2, -o8s2, -o7s3, -o8s3)
ch_mot_factors = mutate(ch_mot_factors, o1s1s3=as.numeric(as.character(o1s1))-as.numeric(as.character(o1s3)))
ch_mot_factors = mutate(ch_mot_factors, o2s1s3=as.numeric(as.character(o2s1))-as.numeric(as.character(o2s3)))
ch_mot_factors = mutate(ch_mot_factors, o3s1s3=as.numeric(as.character(o3s1))-as.numeric(as.character(o3s3)))
ch_mot_factors = mutate(ch_mot_factors, o4s1s3=as.numeric(as.character(o4s1))-as.numeric(as.character(o4s3)))
ch_mot_factors = mutate(ch_mot_factors, o5s1s3=as.numeric(as.character(o5s1))-as.numeric(as.character(o5s3)))
ch_mot_factors = mutate(ch_mot_factors, o6s1s3=as.numeric(as.character(o7s1))-as.numeric(as.character(o6s3)))
ch_mot_factors = mutate(ch_mot_factors, o1s1s2=as.numeric(as.character(o1s1))-as.numeric(as.character(o1s2)))
ch_mot_factors = mutate(ch_mot_factors, o2s1s2=as.numeric(as.character(o2s1))-as.numeric(as.character(o2s2)))
ch_mot_factors = mutate(ch_mot_factors, o3s1s2=as.numeric(as.character(o3s1))-as.numeric(as.character(o3s2)))
ch_mot_factors = mutate(ch_mot_factors, o4s1s2=as.numeric(as.character(o4s1))-as.numeric(as.character(o4s2)))
ch_mot_factors = mutate(ch_mot_factors, o5s1s2=as.numeric(as.character(o5s1))-as.numeric(as.character(o5s2)))
ch_mot_factors = mutate(ch_mot_factors, o6s1s2=as.numeric(as.character(o7s1))-as.numeric(as.character(o7s2)))
ch_mot_factors = mutate(ch_mot_factors, o1s2s3=as.numeric(as.character(o1s2))-as.numeric(as.character(o1s3)))
ch_mot_factors = mutate(ch_mot_factors, o2s2s3=as.numeric(as.character(o2s2))-as.numeric(as.character(o2s3)))
ch_mot_factors = mutate(ch_mot_factors, o3s2s3=as.numeric(as.character(o3s2))-as.numeric(as.character(o3s3)))
ch_mot_factors = mutate(ch_mot_factors, o4s2s3=as.numeric(as.character(o4s2))-as.numeric(as.character(o4s3)))
ch_mot_factors = mutate(ch_mot_factors, o5s2s3=as.numeric(as.character(o5s2))-as.numeric(as.character(o5s3)))
ch_mot_factors = mutate(ch_mot_factors, o6s2s3=as.numeric(as.character(o7s2))-as.numeric(as.character(o6s3)))
ch_factors = c('o1s1s2','o2s1s2','o3s1s2','o4s1s2','o5s1s2','o6s1s2',
               'o1s1s3','o2s1s3','o3s1s3','o4s1s3','o5s1s3','o6s1s3',
               'o1s2s3','o2s2s3','o3s2s3','o4s2s3','o5s2s3','o6s2s3')
civic_activity_ = filter(civic_activity, email %in% ch_mot_factors$email)
ch_mot_factors_ = filter(ch_mot_factors, email %in% civic_activity_$email)
find_correlations(ch_mot_factors_,civic_activity_,ch_factors,c('activities'))
