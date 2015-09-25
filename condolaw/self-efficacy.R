#######
# An analysis about participants' self-efficacy and expectations
#######

# Load libraries
library(dplyr)
source('utils.R')

# Reading data
raw_survey_1 = read.csv("./data/survey1-condlaw-complete.csv",header=T, sep=",", stringsAsFactors=F)
raw_survey_2 = read.csv("./data/survey2-condlaw-complete.csv",header=T, sep=",", stringsAsFactors=F)
raw_survey_3 = read.csv("./data/survey3-condlaw2-complete.csv",header=T, sep=",", stringsAsFactors=F)
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
survey_1 = raw_survey_1 %>% select(email,Q3_1,Q3_2,Q4_1,Q4_2,Q4_3,Q4_4,Q4_5,Q5_1,Q5_3,Q5_4,Q5_5,Q5_6,Q5_7,Q5_14) %>% filter(!is.na(Q3_1))
survey_3 = raw_survey_3 %>% select(email,Q3_2,Q6_4,Q6_5,Q7_2) %>% filter(!is.na(Q3_2))
colnames(survey_1) = c('email',
                       '1. My participation will impact the online conversation', 
                       '2. The online conversation will help resolving problems', 
                       '3. I enjoy interacting with people who criticize my views', 
                       '4. I hold strong opinions on anything', 
                       '5. I avoid the company of people with different values',
                       '6. It is often difficult for me to express my opinion\nin a group',
                       '7. I rarely change my opinion',
                       '8. I typically express my opinions without hesitation',
                       '9. I typically listen to others’ viewpoints before\nI express my opinion',
                       '10.I typically can find solutions that others accept',
                       '11.I often present compromise solutions',
                       '12.I typically don\'t change my opinion as a result\nof a group discussion',
                       '13.I often feel that my opinion is not taken into account',
                       '14.My opinion often impacts the directions in conversation')
melt_survey_1 = melt(survey_1, id.vars='email', measure.vars=c('1. My participation will impact the online conversation', 
                                                               '2. The online conversation will help resolving problems', 
                                                               '3. I enjoy interacting with people who criticize my views', 
                                                               '4. I hold strong opinions on anything', 
                                                               '5. I avoid the company of people with different values',
                                                               '6. It is often difficult for me to express my opinion\nin a group',
                                                               '7. I rarely change my opinion',
                                                               '8. I typically express my opinions without hesitation',
                                                               '9. I typically listen to others’ viewpoints before\nI express my opinion',
                                                               '10.I typically can find solutions that others accept',
                                                               '11.I often present compromise solutions',
                                                               '12.I typically don\'t change my opinion as a result\nof a group discussion',
                                                               '13.I often feel that my opinion is not taken into account',
                                                               '14.My opinion often impacts the directions in conversation'))
melt_survey_1$value = as.numeric(as.character(melt_survey_1$value))

colnames(survey_3) = c('email',
                       '1. In general I hold strong opinions on anything', 
                       '2. My opinion about the needs to reform the\nlaw changed in the online conversation', 
                       '3. My knowledge on off-road traffic was better\nthan that of other participants on the platform', 
                       '4. My participation made a difference\nto the conversation on the platform')
melt_survey_3 = melt(survey_3, id.vars='email', measure.vars=c('1. In general I hold strong opinions on anything', 
                                                               '2. My opinion about the needs to reform the\nlaw changed in the online conversation', 
                                                               '3. My knowledge on off-road traffic was better\nthan that of other participants on the platform', 
                                                               '4. My participation made a difference\nto the conversation on the platform'))
melt_survey_3$value = as.numeric(as.character(melt_survey_3$value))

participation_s1 = users_camp_other
participation_s1 = create_joint_dataset(users_camp_vies, participation_s1)
participation_s1 = create_joint_dataset(users_camp_hall, participation_s1)
participation_s1 = create_joint_dataset(users_camp_erim, participation_s1)
participation_s1 = participation_s1 %>% select(-id)
participation_s1 = participation_s1 %>% filter(email != '')

participation_s3 = users_camp_other_survey_3
participation_s3 = create_joint_dataset(users_camp_0_survey_3, participation_s3)
participation_s3 = create_joint_dataset(users_camp_1_survey_3, participation_s3)
participation_s3 = create_joint_dataset(users_camp_2_survey_3, participation_s3)
participation_s3 = participation_s3 %>% select(-id)
participation_s3 = participation_s3 %>% filter(email != '')

# Draw a boxplots
ggplot(melt_survey_1, aes(x=variable, y=value, fill=variable)) + 
  geom_boxplot() + 
  scale_y_continuous(breaks=c(0:8)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=16, color='black'), 
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=17), axis.title.y = element_text(size=15),
        strip.text.x = element_text(size=18, face="bold")) + 
  ggtitle("Pre-process") +
  labs(y="Scores") +
  guides(fill=FALSE)
ggplot(melt_survey_3, aes(x=variable, y=value, fill=variable)) + 
  geom_boxplot() + 
  scale_y_continuous(breaks=c(0:8)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=16, color='black'), 
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=17), axis.title.y = element_text(size=15),
        strip.text.x = element_text(size=18, face="bold")) + 
  ggtitle("After Post-process (survey 3)") +
  labs(y="Scores") +
  guides(fill=FALSE)

# Analyze whether the mean of scores of questions 2, 3, and 4 are positive
t.test(survey_3[,3],mu=3.5,alternative='greater')
t.test(survey_3[,4],mu=3.5,alternative='greater')
t.test(survey_3[,5],mu=3.5,alternative='greater')
for (i in c(2:15)) {
  t_test = t.test(survey_1[,i],mu=3.5,alternative='greater')
  print(paste('Option: ',i-1,' p-value: ',t_test$p.value,sep=''))
  
}

# Check whether the difference detected in the mean score of question 1
# is significant
surveys = merge(survey_1,survey_3,by.x='email', by.y='email')
survey_1 = survey_1 %>% filter(email %in% surveys$email)
survey_3 = survey_3 %>% filter(email %in% surveys$email)
t.test(survey_1[,5],survey_3[,2])

# Analyze correlation between motivation factors and self-efficacy (survey 1)
survey_1 = raw_survey_1 %>% select(email,Q3_1,Q3_2,Q4_1,Q4_2,Q4_3,Q4_4,Q4_5,Q5_1,Q5_3,Q5_4,Q5_5,Q5_6,Q5_7,Q5_14)
survey_1 = survey_1 %>% filter(email %in% mot_survey_1$email)
mot_survey_1 = mot_survey_1 %>% filter(email %in% survey_1$email) 
# Check correlation between motivation factors and the 14 self-efficacy and expectation options of survey 1
for (factor in c('o1s1', 'o2s1', 'o3s1', 'o4s1', 'o5s1', 'o6s1', 'o7s1')) {
  for (option in c('Q3_1','Q3_2','Q4_1','Q4_2','Q4_3','Q4_4','Q4_5','Q5_1','Q5_3','Q5_4','Q5_5','Q5_6','Q5_7','Q5_14')) {
    ret = prepare_data(mot_survey_1, survey_1, factor, option)
    cor_coef = cor(ret$ds1,ret$ds2)
    print(paste('The correlation between factor ',factor,' and option ',option,' is ',cor_coef,sep=''))
    if (cor_coef > 0.50) {
      print('Found asignificant correlation!')
    }
  }
}

# Analyze correlation between motivation factors and self-efficacy (survey 3)
survey_3 = raw_survey_3 %>% select(email,Q3_2,Q6_4,Q6_5,Q7_2)
survey_3 = survey_3 %>% filter(email %in% mot_survey_3$email)
mot_survey_3 = mot_survey_3 %>% filter(email %in% survey_3$email) 
# Check correlation between motivation factors and the 4 self-efficacy and expectation options of survey 3
for (factor in c('o1s3', 'o2s3', 'o3s3', 'o4s3', 'o5s3', 'o6s3', 'o7s3', 'o8s3')) {
  for (option in c('Q3_2','Q6_4','Q6_5','Q7_2')) {
    ret = prepare_data(mot_survey_3, survey_3, factor, option)
    cor_coef = cor(ret$ds1,ret$ds2)
    print(paste('The correlation between factor ',factor,' and option ',option,' is ',cor_coef,sep=''))
    if (cor_coef > 0.50) {
      print('Found asignificant correlation!')
    }
  }
}

# Further analyze the high and positive correlation found before
ret = prepare_data(mot_survey_3, survey_3, 'o4s3', 'Q3_2')
cor.test(ret$ds1,ret$ds2)
df = data.frame(x=ret$ds1,y=ret$ds2)
ggplot(df, aes(x, y)) + 
  geom_point(shape=1) + geom_smooth(color = 'blue') +
  scale_x_continuous(breaks=c(0:8)) +
  scale_y_continuous(breaks=c(0:8)) +
  labs(x="I wanted to learn more about the condominium law.", y="I hold strong opinions on anything") +
  theme(axis.text.x = element_text(hjust = 1, size=16, color='black'), 
        axis.text.y = element_text(size=17, color='black'), axis.title.y = element_text(size=20),
        axis.title.x = element_text(size=20),
        strip.text.x = element_text(size=18, face="bold"))

# Analyses of the level of participation and self-efficacy (survey 1)
participation_s1 = mutate(participation_s1,content=ideas+votes+comments)
participation_s1 = mutate(participation_s1,ideas_votes=ideas+votes)
participation_s1 = mutate(participation_s1,ideas_comments=ideas+comments)
participation_s1 = mutate(participation_s1,votes_comments=votes+comments)
participation_s1 = filter(participation_s1, email %in% survey_1$email)
for (type_participation in c('content', 'ideas_votes', 'ideas_comments', 'votes_comments', 'votes', 'ideas', 'comments')) {
  for (option in c('Q3_1','Q3_2','Q4_1','Q4_2','Q4_3','Q4_4','Q4_5','Q5_1','Q5_3','Q5_4','Q5_5','Q5_6','Q5_7','Q5_14')) {
    ret = prepare_data(participation_s1, survey_1, type_participation, option)
    cor_coef = cor(ret$ds1,ret$ds2)
    print(paste('The correlation between the production of ',type_participation,' and the option ',option,' is ',cor_coef,sep=''))
    if (cor_coef > 0.50) {
      print('Found asignificant correlation!')
    }
  } 
}

# Analyses of the level of participation and self-efficacy (survey 3)
participation_s3 = mutate(participation_s3,ideas_votes=ideas+votes)
participation_s3 = mutate(participation_s3,ideas_comments=ideas+comments)
participation_s3 = mutate(participation_s3,votes_comments=votes+comments)
participation_s3 = mutate(participation_s3,content=ideas+votes+comments)
participation_s3 = filter(participation_s3, email %in% survey_3$email)
for (type_participation in c('content', 'ideas_votes', 'ideas_comments', 'votes_comments', 'votes', 'ideas', 'comments')) {
  for (option in c('Q3_2','Q6_4','Q6_5','Q7_2')) {
    ret = prepare_data(participation_s3, survey_3, type_participation, option)
    cor_coef = cor(ret$ds1,ret$ds2)
    print(paste('The correlation between the production of ',type_participation,' and the option ',option,' is ',cor_coef,sep=''))
    if (cor_coef > 0.50) {
      print('Found asignificant correlation!')
    }
  }
}

# Analyses of knowledge gain and self-efficacy
# Prepare knowledge gain data
survey_1 = raw_survey_1 %>% select(email,Q6_3,Q6_4,Q6_5) %>% filter(!is.na(Q6_3))
survey_2 = raw_survey_2 %>% select(email,Q11_6,Q11_7,Q11_8) %>% filter(!is.na(Q11_6))
survey_3 = raw_survey_3 %>% select(email,Q11_6,Q11_7,Q11_8) %>% filter(!is.na(Q11_6))
learning_surveys = merge(survey_1, survey_2, by.x='email', by.y='email', all=F)
learning_surveys = merge(learning_surveys, survey_3, by.x='email', by.y='email', all=F)
# Prepare self-efficacy data
survey_1 = raw_survey_1 %>% select(email,Q3_1,Q3_2,Q4_1,Q4_2,Q4_3,Q4_4,Q4_5,Q5_1,Q5_3,Q5_4,Q5_5,Q5_6,Q5_7,Q5_14)
survey_3 = raw_survey_3 %>% select(email,Q3_2,Q6_4,Q6_5,Q7_2)
colnames(survey_1) = c('email','sf1','sf2','sf3','sf4_a','sf5','sf6','sf7','sf8','sf9','sf10','sf11','sf12','sf13','sf14')
colnames(survey_3) = c('email','sf4_b','sf15','sf16','sf17')
# Prepare joint dataset
dataset = merge(learning_surveys, survey_1, by.x='email', by.y='email')
dataset = merge(dataset, survey_3, by.x='email', by.y='email')
dataset = has_learnt(dataset)
# Separate those that have learnt from those that haven't learnt 
# either because they knew already the answers or because they did not learn
learnt = list()
no_learnt = list()
for(email in dataset$email) {
  sf_1 = dataset[dataset$email==email,c('sf1')]
  sf_2 = dataset[dataset$email==email,c('sf2')]
  sf_3 = dataset[dataset$email==email,c('sf3')]
  sf_4 = dataset[dataset$email==email,c('sf4_a','sf4_b')]
  sf_5 = dataset[dataset$email==email,c('sf5')]
  sf_6 = dataset[dataset$email==email,c('sf6')]
  sf_7 = dataset[dataset$email==email,c('sf7')]
  sf_8 = dataset[dataset$email==email,c('sf8')]
  sf_9 = dataset[dataset$email==email,c('sf9')]
  sf_10 = dataset[dataset$email==email,c('sf10')]
  sf_11 = dataset[dataset$email==email,c('sf11')]
  sf_12 = dataset[dataset$email==email,c('sf12')]
  sf_13 = dataset[dataset$email==email,c('sf13')]
  sf_14 = dataset[dataset$email==email,c('sf14')]
  sf_15 = dataset[dataset$email==email,c('sf15')]
  sf_16 = dataset[dataset$email==email,c('sf16')]
  sf_17 = dataset[dataset$email==email,c('sf17')]
  if (dataset[dataset$email==email,'learnt'] == 'yes') {
    learnt$sf1 = c(learnt$sf1, as.numeric(as.character(sf_1)))
    learnt$sf2 = c(learnt$sf2, as.numeric(as.character(sf_2)))
    learnt$sf3 = c(learnt$sf3, as.numeric(as.character(sf_3)))
    learnt$sf4 = c(learnt$sf4, as.numeric(as.character(sf_4)))
    learnt$sf5 = c(learnt$sf5, as.numeric(as.character(sf_5)))
    learnt$sf6 = c(learnt$sf6, as.numeric(as.character(sf_6)))
    learnt$sf7 = c(learnt$sf7, as.numeric(as.character(sf_7)))
    learnt$sf8 = c(learnt$sf8, as.numeric(as.character(sf_8)))
    learnt$sf9 = c(learnt$sf9, as.numeric(as.character(sf_9)))
    learnt$sf10 = c(learnt$sf10, as.numeric(as.character(sf_10)))
    learnt$sf11 = c(learnt$sf11, as.numeric(as.character(sf_11)))
    learnt$sf12 = c(learnt$sf12, as.numeric(as.character(sf_12)))
    learnt$sf13 = c(learnt$sf13, as.numeric(as.character(sf_13)))
    learnt$sf14 = c(learnt$sf14, as.numeric(as.character(sf_14)))
    learnt$sf15 = c(learnt$sf15, as.numeric(as.character(sf_15)))
    learnt$sf16 = c(learnt$sf16, as.numeric(as.character(sf_16)))
    learnt$sf17 = c(learnt$sf17, as.numeric(as.character(sf_17)))
  } else {
    no_learnt$sf1 = c(no_learnt$sf1, as.numeric(as.character(sf_1)))
    no_learnt$sf2 = c(no_learnt$sf2, as.numeric(as.character(sf_2)))
    no_learnt$sf3 = c(no_learnt$sf3, as.numeric(as.character(sf_3)))
    no_learnt$sf4 = c(no_learnt$sf4, as.numeric(as.character(sf_4)))
    no_learnt$sf5 = c(no_learnt$sf5, as.numeric(as.character(sf_5)))
    no_learnt$sf6 = c(no_learnt$sf6, as.numeric(as.character(sf_6)))
    no_learnt$sf7 = c(no_learnt$sf7, as.numeric(as.character(sf_7)))
    no_learnt$sf8 = c(no_learnt$sf8, as.numeric(as.character(sf_8)))
    no_learnt$sf9 = c(no_learnt$sf9, as.numeric(as.character(sf_9)))
    no_learnt$sf10 = c(no_learnt$sf10, as.numeric(as.character(sf_10)))
    no_learnt$sf11 = c(no_learnt$sf11, as.numeric(as.character(sf_11)))
    no_learnt$sf12 = c(no_learnt$sf12, as.numeric(as.character(sf_12)))
    no_learnt$sf13 = c(no_learnt$sf13, as.numeric(as.character(sf_13)))
    no_learnt$sf14 = c(no_learnt$sf14, as.numeric(as.character(sf_14)))
    no_learnt$sf15 = c(no_learnt$sf15, as.numeric(as.character(sf_15)))
    no_learnt$sf16 = c(no_learnt$sf16, as.numeric(as.character(sf_16)))
    no_learnt$sf17 = c(no_learnt$sf17, as.numeric(as.character(sf_17)))
  }
}
for (factor in c('sf1','sf2','sf3','sf4','sf5', 'sf6','sf7','sf8','sf9','sf10','sf11', 'sf12','sf13','sf14','sf15','sf16','sf17')) {
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

# Analyses of perceived learning and self-efficacy
# Load data about perceived learning
pl_survey_3 = raw_survey_3 %>% select(email,Q3_1,Q3_3,Q3_4,Q6_1)
# Load data about self-efficacy
se_survey_3 = raw_survey_3 %>% select(email,Q3_2,Q6_4,Q6_5,Q7_2)
for (pl_option in c('Q3_1','Q3_3','Q3_4','Q6_1')) {
  for (se_option in c('Q3_2','Q6_4','Q6_5','Q7_2')) {
    ret = prepare_data(pl_survey_3, se_survey_3, pl_option, se_option)
    cor_coef = cor(ret$ds1,ret$ds2)
    print(paste('The correlation between the perceived learning option ',pl_option,' and the self-efficay option ',se_option,' is ',cor_coef,sep=''))
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

# Search correlations between self-efficacy (survey 1) and changes in the motivation factors
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
se_options = c('Q3_1','Q3_2','Q4_1','Q4_2','Q4_3','Q4_4','Q4_5','Q5_1','Q5_3','Q5_4','Q5_5','Q5_6','Q5_7','Q5_14')
find_correlations(ch_mot_factors,survey_1_,ch_factors,se_options)

# Search correlations between self-efficacy (survey 3) and changes in the motivation factors
survey_3_ = survey_3 %>% filter(email %in% ch_mot_factors$email)
se_options = c('Q3_2','Q6_4','Q6_5','Q7_2')
find_correlations(ch_mot_factors,survey_3_,ch_factors,se_options)

# Analyze whether the set of participants that hold strong opinions is mutually exclusive with the set of participant that listen to others
survey_1_ = survey_1 %>% filter(!is.na(Q4_2)) %>% filter(!is.na(Q5_3))
aux = survey_1_[,c(5,10)]
colnames(aux) = c('strong','others')
aux$diff = aux$strong-aux$others
aux$diff = as.numeric(as.character(aux$diff))
mean(aux$diff)
