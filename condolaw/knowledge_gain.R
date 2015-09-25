#######
# An analysis of changes in the knowledge gain
# taking into consideration the first, second, 
# and third survey
#######

# Load libraries
library(dplyr)
source('utils.R')

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
mot_survey_1 = read.csv("./data/survey1-condlaw-no-duplicates.csv",header=T, sep=",",stringsAsFactors=F)
mot_survey_2 = read.csv("./data/survey2-condlaw-no-duplicates.csv",header=T, sep=",",stringsAsFactors=F)
mot_survey_3 = read.csv("./data/survey3-condlaw-no-duplicates2.csv",header=T, sep=",",stringsAsFactors=F)

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
colnames(mot_survey_1) = c('email', 'datetime', 'o1s1', 'o2s1', 'o3s1', 'o4s1', 'o5s1', 'o6s1', 'o7s1')
colnames(mot_survey_2) = c('email', 'datetime', 'o1s2', 'o2s2', 'o3s2', 'o4s2', 'o5s2', 'o6s2', 'o7s2', 'o8s2')
colnames(mot_survey_3) = c('email', 'datetime', 'o1s3', 'o2s3', 'o3s3', 'o4s3', 'o5s3', 'o6s3', 'o7s3', 'o8s3')
mot_survey_1 = mot_survey_1 %>% filter(email!='') %>% filter(!is.na(o1s1)) %>% filter(o1s1!='') %>% select(-datetime)
mot_survey_2 = mot_survey_2 %>% filter(email!='') %>% filter(!is.na(o1s2)) %>% filter(o1s2!='') %>% select(-datetime)
mot_survey_3 = mot_survey_3 %>% filter(email!='') %>% filter(!is.na(o1s3)) %>% filter(o1s3!='') %>% select(-datetime)
mot_surveys = merge(mot_survey_1, mot_survey_2, by.x='email', by.y='email', all=F)
mot_surveys = merge(mot_surveys, mot_survey_3, by.x='email', by.y='email', all=F)
mot_surveys = mot_surveys %>% select(-o6s1, -o6s2, -o8s2, -o7s3, -o8s3)
colnames(mot_surveys) = c('email', 'o1s1', 'o2s1', 'o3s1', 'o4s1', 'o5s1', 'o6s1',
                          'o1s2', 'o2s2', 'o3s2', 'o4s2', 'o5s2', 'o6s2',
                          'o1s3', 'o2s3', 'o3s3', 'o4s3', 'o5s3', 'o6s3')

# Perform the analysis
p_value_o1 = chisq.test(table(dataset$o1,dataset$group), simulate.p.value=T)$p.value
p_value_o2 = chisq.test(table(dataset$o2,dataset$group), simulate.p.value=T)$p.value  # **
p_value_o3 = chisq.test(table(dataset$o3,dataset$group), simulate.p.value=T)$p.value

# Investigate whether drops in the incorrect answers are significant
table_o1 = table(dataset[dataset$group!='survey_3'&dataset$o1=='no','o1'],dataset[dataset$group!='survey_3'&dataset$o1=='no','group'])
chisq.test(table_o1, simulate.p.value=T)$p.value
table_o2 = table(dataset[dataset$group!='survey_1'&dataset$o2=='yes','o2'],dataset[dataset$group!='survey_1'&dataset$o2=='yes','group'])
chisq.test(table_o2, simulate.p.value=T)$p.value
# prop.test(c(18,33),c(64,64))

# Investigate a potential association between level of participation and knowledge gain
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
participants = participants %>% filter(email %in% all_surveys$email)

# Create clusters by level of participation
observers = participants %>% filter(ideas==0&votes==0&comments==0) # nothing, only observe what happened
voters = participants %>% filter(ideas==0&votes!=0&comments==0)    # only vote
mid_cont = participants %>% filter((ideas==0&votes!=0&comments!=0)|
                                  (ideas!=0&votes==0&comments!=0)|
                                  (ideas!=0&votes!=0&comments==0)|
                                  (ideas!=0&votes==0&comments==0)|
                                  (ideas==0&votes==0&comments!=0))   
full_cont = participants %>% filter(ideas!=0&votes!=0&comments!=0) # suggest ideas, comment and vote

# Calculate whether participants has learned or not along the process
survey_1 = raw_survey_1 %>% select(email,Q6_3,Q6_4,Q6_5) %>% filter(!is.na(Q6_3))
survey_2 = raw_survey_2 %>% select(email,Q11_6,Q11_7,Q11_8) %>% filter(!is.na(Q11_6))
survey_3 = raw_survey_3 %>% select(email,Q11_6,Q11_7,Q11_8) %>% filter(!is.na(Q11_6))
all_surveys = merge(survey_1, survey_2, by.x='email', by.y='email', all=F)
all_surveys = merge(all_surveys, survey_3, by.x='email', by.y='email', all=F)
observers = has_learnt(merge(observers,all_surveys,by.x='email',by.y='email'))
voters = has_learnt(merge(voters,all_surveys,by.x='email',by.y='email'))
mid_cont = has_learnt(merge(mid_cont,all_surveys,by.x='email',by.y='email'))
full_cont = has_learnt(merge(full_cont,all_surveys,by.x='email',by.y='email'))

# Add a flag to identify groups
observers$group = 'observers'
voters$group = 'voters'
mid_cont$group = 'mid_contributors'
full_cont$group = 'full_contributors'

# Actually perfrom the analysis
dataset = rbind(observers,voters,mid_cont,full_cont)
chisq.test(table(dataset$learn,dataset$group), simulate.p.value=T)$p.value

# Investigate a potential association between motivation factors and knowledge gain
survey_1 = raw_survey_1 %>% select(email,Q6_3,Q6_4,Q6_5) %>% filter(!is.na(Q6_3))
survey_2 = raw_survey_2 %>% select(email,Q11_6,Q11_7,Q11_8) %>% filter(!is.na(Q11_6))
survey_3 = raw_survey_3 %>% select(email,Q11_6,Q11_7,Q11_8) %>% filter(!is.na(Q11_6))
all_surveys = merge(survey_1, survey_2, by.x='email', by.y='email', all=F)
all_surveys = merge(all_surveys, survey_3, by.x='email', by.y='email', all=F)
dataset = merge(all_surveys, mot_surveys, by.x='email', by.y='email')
dataset = has_learnt(dataset)
learnt = list()
no_learnt = list()
for(email in dataset$email) {
  factor_1 = dataset[dataset$email==email,c('o1s1','o1s2','o1s3')]
  factor_2 = dataset[dataset$email==email,c('o2s1','o2s2','o2s3')]
  factor_3 = dataset[dataset$email==email,c('o3s1','o3s2','o3s3')]
  factor_4 = dataset[dataset$email==email,c('o4s1','o4s2','o4s3')]
  factor_5 = dataset[dataset$email==email,c('o5s1','o5s2','o5s3')]
  factor_6 = dataset[dataset$email==email,c('o6s1','o6s2','o6s3')]
  print(email)
  if (dataset[dataset$email==email,'learnt'] == 'yes') {
    learnt$f1 = c(learnt$f1, as.numeric(as.character(factor_1)))
    learnt$f2 = c(learnt$f2, as.numeric(as.character(factor_2)))
    learnt$f3 = c(learnt$f3, as.numeric(as.character(factor_3)))
    learnt$f4 = c(learnt$f4, as.numeric(as.character(factor_4)))
    learnt$f5 = c(learnt$f5, as.numeric(as.character(factor_5)))
    learnt$f6 = c(learnt$f6, as.numeric(as.character(factor_6)))
  } else {
    no_learnt$f1 = c(no_learnt$f1, as.numeric(as.character(factor_1)))
    no_learnt$f2 = c(no_learnt$f2, as.numeric(as.character(factor_2)))
    no_learnt$f3 = c(no_learnt$f3, as.numeric(as.character(factor_3)))
    no_learnt$f4 = c(no_learnt$f4, as.numeric(as.character(factor_4)))
    no_learnt$f5 = c(no_learnt$f5, as.numeric(as.character(factor_5)))
    no_learnt$f6 = c(no_learnt$f6, as.numeric(as.character(factor_6)))
  }
}
for (factor in c('f1','f2','f3','f4','f5', 'f6')) {
  print(paste('Factor: ',factor,sep=''))
  print(paste('Mean learn: ',round(mean(learnt[[factor]]),3),sep=''))
  print(paste('Mean no learn: ',round(mean(no_learnt[[factor]]),3),sep=''))
  print(paste('Median learn: ',round(median(learnt[[factor]]),3),sep=''))
  print(paste('Median no learn: ',round(median(no_learnt[[factor]]),3),sep=''))
  test = t.test(learnt[[factor]],no_learnt[[factor]]) 
  print(paste('p-value: ',test$p.value,sep=''))
  if (test$p.value < 0.05) {
    print('It was found a significant relationship')
  }
}

# Search correlations between learning (survey 1) and changes in the motivation factors
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
survey_1_ = survey_1 %>% filter(email %in% ch_mot_factors$email)
l_options = c('Q6_3','Q6_4','Q6_5')
ch_mot_factors_ = ch_mot_factors %>% filter(email %in% survey_1_$email)
find_correlations(ch_mot_factors_,survey_1_,ch_factors,l_options)

# Search correlations between learning (survey 2) and changes in the motivation factors
survey_2_ = survey_2 %>% filter(email %in% ch_mot_factors$email)
l_options = c('Q11_6','Q11_7','Q11_8')
ch_mot_factors_ = ch_mot_factors %>% filter(email %in% survey_2_$email)
find_correlations(ch_mot_factors_,survey_2_,ch_factors,l_options)

# Search correlations between learning (survey 3) and changes in the motivation factors
survey_3_ = survey_3 %>% filter(email %in% ch_mot_factors$email)
l_options = c('Q11_6','Q11_7','Q11_8')
ch_mot_factors_ = ch_mot_factors %>% filter(email %in% survey_3_$email)
find_correlations(ch_mot_factors_,survey_3_,ch_factors,l_options)
