#######
# An analysis of whether participants perceive
# that they learned something in the process
#######

# Load libraries
library(dplyr)
source('utils.R')

# Reading data
raw_survey_2 = read.csv("./data/survey2-condlaw-complete.csv",header=T, sep=",", stringsAsFactors=F)
raw_survey_3 = read.csv("./data/survey3-condlaw2-complete.csv",header=T, sep=",", stringsAsFactors=F)
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
survey_2 = raw_survey_2 %>% select(email,Q3_1,Q3_2,Q3_3,Q3_4,Q2_1) %>% filter(!is.na(Q3_1))
colnames(survey_2) = c('email','Learned from others', 'Learned more about the law', 'Learned from materials on the website', 
                       'Learned from organizers', 'Learned to understand others\' point of views')
survey_3 = raw_survey_3 %>% select(email,Q3_1,Q3_3,Q3_4,Q6_1) %>% filter(!is.na(Q3_1))
colnames(survey_3) = c('email','Learned from others', 'Learned from materials on the website', 
                       'Learned from organizers', 'Learned to understand others\' point of views')
surveys = merge(survey_2, survey_3, by.x='email', by.y='email', all=F)
survey_2 = survey_2 %>% filter(email %in% surveys$email)
survey_3 = survey_3 %>% filter(email %in% surveys$email)
melt_survey_2 = melt(survey_2, id.vars='email', measure.vars=c('Learned from others', 'Learned more about the law',
                                                               'Learned from materials on the website', 
                                                               'Learned from organizers', 
                                                               'Learned to understand others\' point of views'))
melt_survey_2$survey = 'survey_2'
melt_survey_3 = melt(survey_3, id.vars='email', measure.vars=c('Learned from others',
                                                               'Learned from materials on the website', 
                                                               'Learned from organizers', 
                                                               'Learned to understand others\' point of views'))
melt_survey_3$survey = 'survey_3'
melt_surveys = rbind(melt_survey_2,melt_survey_3)
melt_surveys$value = as.numeric(as.character(melt_surveys$value))

participation_s2 = users_camp_other
participation_s2 = create_joint_dataset(users_camp_vies, participation_s2)
participation_s2 = create_joint_dataset(users_camp_hall, participation_s2)
participation_s2 = create_joint_dataset(users_camp_erim, participation_s2)
participation_s2 = participation_s2 %>% select(-id)
participation_s2 = participation_s2 %>% filter(email != '')

participation_s3 = users_camp_other_survey_3
participation_s3 = create_joint_dataset(users_camp_0_survey_3, participation_s3)
participation_s3 = create_joint_dataset(users_camp_1_survey_3, participation_s3)
participation_s3 = create_joint_dataset(users_camp_2_survey_3, participation_s3)
participation_s3 = participation_s3 %>% select(-id)
participation_s3 = participation_s3 %>% filter(email != '')

colnames(mot_survey_1) = c('email', 'datetime', 'o1s1', 'o2s1', 'o3s1', 'o4s1', 'o5s1', 'o6s1', 'o7s1')
colnames(mot_survey_2) = c('email', 'datetime', 'o1s2', 'o2s2', 'o3s2', 'o4s2', 'o5s2', 'o6s2', 'o7s2', 'o8s2')
colnames(mot_survey_3) = c('email', 'datetime', 'o1s3', 'o2s3', 'o3s3', 'o4s3', 'o5s3', 'o6s3', 'o7s3', 'o8s3')
mot_survey_1 = mot_survey_1 %>% filter(email!='') %>% filter(!is.na(o1s1)) %>% filter(o1s1!='') %>% select(-datetime)
mot_survey_2 = mot_survey_2 %>% filter(email!='') %>% filter(!is.na(o1s2)) %>% filter(o1s2!='') %>% select(-datetime)
mot_survey_3 = mot_survey_3 %>% filter(email!='') %>% filter(!is.na(o1s3)) %>% filter(o1s3!='') %>% select(-datetime)

# Draw a boxplot
ggplot(melt_surveys, aes(x=variable, y=value, fill=variable)) + 
  geom_boxplot() + 
  facet_grid(. ~ survey) +
  scale_y_continuous(breaks=c(0:8)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=16, color='black'), 
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=17), axis.title.y = element_text(size=15),
        strip.text.x = element_text(size=18, face="bold")) + 
  labs(y="Scores") +
  guides(fill=FALSE)

# Analysis significance in changes on the perceive learning
survey_2 = raw_survey_2 %>% select(email,Q3_1,Q3_3) %>% filter(!is.na(Q3_1)) %>% filter(email %in% surveys$email) %>% select(-email)
survey_2$group = 'survey_2'
survey_3 = raw_survey_3 %>% select(email,Q3_1,Q3_3) %>% filter(!is.na(Q3_1)) %>% filter(email %in% surveys$email) %>% select(-email)
survey_3$group = 'survey_3'
surveys = rbind(survey_2,survey_3)
groups = c('survey_2', 'survey_3')
ret = get_dynamic_report_diff_groups(surveys, groups, c('Q3_1','Q3_3'))

# Analysis of level of participation and perceive learning in survey 2
survey_2 = raw_survey_2 %>% select(email,Q3_1,Q3_2,Q3_3,Q3_4,Q2_1) %>% filter(!is.na(Q3_1))
participation_s2 = mutate(participation_s2,content=ideas+votes+comments)
participation_s2 = mutate(participation_s2,ideas_votes=ideas+votes)
participation_s2 = mutate(participation_s2,ideas_comments=ideas+comments)
participation_s2 = mutate(participation_s2,votes_comments=votes+comments)
participation_s2 = filter(participation_s2, email %in% survey_2$email)
for (type_participation in c('content', 'ideas_votes', 'ideas_comments', 'votes_comments', 'votes', 'ideas', 'comments')) {
  for (option in c('Q3_1','Q3_2','Q3_3','Q3_4','Q2_1')) {
    ret = prepare_data(participation_s2, survey_2, type_participation, option)
    cor_coef = cor(ret$ds1,ret$ds2)
    print(paste('The correlation between the production of ',type_participation,' and the option ',option,' is ',cor_coef,sep=''))
    if (cor_coef > 0.50) {
      print('Found asignificant correlation!')
    }
  } 
}

# Analysis of level of participation and perceive learning in survey 3
survey_3 = raw_survey_3 %>% select(email,Q3_1,Q3_3,Q3_4,Q6_1) %>% filter(!is.na(Q3_1))
participation_s3 = mutate(participation_s3,content=ideas+votes+comments)
participation_s3 = mutate(participation_s3,ideas_votes=ideas+votes)
participation_s3 = mutate(participation_s3,ideas_comments=ideas+comments)
participation_s3 = mutate(participation_s3,votes_comments=votes+comments)
participation_s3 = filter(participation_s3, email %in% survey_3$email)
for (type_participation in c('content', 'ideas_votes', 'ideas_comments', 'votes_comments', 'votes', 'ideas', 'comments')) {
  for (option in c('Q3_1','Q3_3','Q3_4','Q6_1')) {
    ret = prepare_data(participation_s3, survey_3, type_participation, option)
    cor_coef = cor(ret$ds1,ret$ds2)
    print(paste('The correlation between the production of ',type_participation,' and the option ',option,' is ',cor_coef,sep=''))
    if (cor_coef > 0.50) {
      print('Found asignificant correlation!')
    }
  } 
}

# Analysis of motivation factors and perceive learning in survey 2
survey_2 = raw_survey_2 %>% select(email,Q3_1,Q3_2,Q3_3,Q3_4,Q2_1) %>% filter(!is.na(Q3_1))
survey_2 = filter(survey_2, email %in% mot_survey_2$email)
mot_survey_2 = filter(mot_survey_2, email %in% survey_2$email)
# Check correlation between motivation factors and the options of perceived learning in survey 2
for (factor in c('o1s2', 'o2s2', 'o3s2', 'o4s2', 'o5s2', 'o6s2', 'o7s2', 'o8s2')) {
  for (option in c('Q3_1','Q3_2','Q3_3','Q3_4','Q2_1')) {
    ret = prepare_data(mot_survey_2, survey_2, factor, option)
    cor_coef = cor(ret$ds1,ret$ds2)
    print(paste('The correlation between factor ',factor,' and option ',option,' is ',cor_coef,sep=''))
    if (cor_coef > 0.50) {
      print('Found asignificant correlation!')
    }
  }
}

# Analysis of motivation factors and perceive learning in survey 3
survey_3 = raw_survey_3 %>% select(email,Q3_1,Q3_3,Q3_4,Q6_1) %>% filter(!is.na(Q3_1))
survey_3 = filter(survey_3, email %in% mot_survey_3$email)
mot_survey_3 = filter(mot_survey_3, email %in% survey_3$email)
# Check correlation between motivation factors and the options of perceived learning in survey 2
for (factor in c('o1s3', 'o2s3', 'o3s3', 'o4s3', 'o5s3', 'o6s3', 'o7s3', 'o8s3')) {
  for (option in c('Q3_1','Q3_3','Q3_4','Q6_1')) {
    ret = prepare_data(mot_survey_3, survey_3, factor, option)
    cor_coef = cor(ret$ds1,ret$ds2)
    print(paste('The correlation between factor ',factor,' and option ',option,' is ',cor_coef,sep=''))
    if (cor_coef > 0.50) {
      print('Found asignificant correlation!')
    }
  }
}

# Further analysis of the high and positive correlations found before
ret = prepare_data(mot_survey_3, survey_3, 'o4s3', 'Q3_1')
cor.test(ret$ds1,ret$ds2)
df = data.frame(x=ret$ds1,y=ret$ds2)
ggplot(df, aes(x, y)) + 
  geom_point(shape=1) + geom_smooth(color = 'blue') +
  scale_x_continuous(breaks=c(0:8)) +
  scale_y_continuous(breaks=c(0:8)) +
  labs(x="I wanted to learn more about the condominium law", y="I learned from other participants") +
  theme(axis.text.x = element_text(hjust = 1, size=16, color='black'), 
        axis.text.y = element_text(size=17, color='black'), axis.title.y = element_text(size=20),
        axis.title.x = element_text(size=20),
        strip.text.x = element_text(size=18, face="bold"))

ret = prepare_data(mot_survey_3, survey_3, 'o6s3', 'Q3_1')
cor.test(ret$ds1,ret$ds2)
df = data.frame(x=ret$ds1,y=ret$ds2)
ggplot(df, aes(x, y)) + 
  geom_point(shape=1) + geom_smooth(color = 'blue') +
  scale_x_continuous(breaks=c(0:8)) +
  scale_y_continuous(breaks=c(0:8)) +
  labs(x="I wanted to discuss the topic with others", y="I learned from other participants") +
  theme(axis.text.x = element_text(hjust = 1, size=16, color='black'), 
        axis.text.y = element_text(size=17, color='black'), axis.title.y = element_text(size=20),
        axis.title.x = element_text(size=20),
        strip.text.x = element_text(size=18, face="bold"))

ret = prepare_data(mot_survey_3, survey_3, 'o7s3', 'Q3_1')
cor.test(ret$ds1,ret$ds2)

# Analisys of whether the willigness to learn (motivation factor 4) is associated with the perceived learning
dataset_s2 = merge(mot_survey_2,survey_2,by.x='email',by.y='email')
mot_survey_2 = filter(mot_survey_2, email %in% dataset_s2$email)
survey_2 = filter(survey_2, email %in% dataset_s2$email)
dataset_s3 = merge(mot_survey_3,survey_3,by.x='email',by.y='email')
mot_survey_3 = filter(mot_survey_3, email %in% dataset_s3$email)
survey_3 = filter(survey_3, email %in% dataset_s3$email)
for (option in c('Q3_1','Q3_2','Q3_3','Q3_4','Q2_1')) {
  ret = prepare_data(mot_survey_2, survey_2, 'o4s2', option)
  cor_coef = cor(ret$ds1,ret$ds2)
  print(paste('The correlation between factor o4s2 and option ',option,' is ',cor_coef,sep=''))
  if (cor_coef > 0.50) {
    print('Found a high correlation!')
  }
}
for (option in c('Q3_1','Q3_3','Q3_4','Q6_1')) {
  ret = prepare_data(mot_survey_3, survey_3, 'o4s3', option)
  cor_coef = cor(ret$ds1,ret$ds2)
  print(paste('The correlation between factor o4s3 and option ',option,' is ',cor_coef,sep=''))
  if (cor_coef > 0.50) {
    print('Found a high correlation!')
  }
}

# Further analysis the high correlation detected previously
ret = prepare_data(mot_survey_3, survey_3, 'o4s3', 'Q3_1')
cor.test(ret$ds1,ret$ds2)
df = data.frame(x=ret$ds1,y=ret$ds2)
ggplot(df, aes(x, y)) + 
  geom_point(position = 'jitter') + 
  geom_smooth(color = 'blue') +
  #scale_x_continuous(breaks=c(1:7)) +
  #scale_y_continuous(breaks=c(1:7)) +
  labs(x="I wanted to learn more about the condominium law", y="Learned from others") +
  theme(axis.text.x = element_text(hjust = 1, size=16, color='black'), 
        axis.text.y = element_text(size=17, color='black'), axis.title.y = element_text(size=20),
        axis.title.x = element_text(size=20),
        strip.text.x = element_text(size=18, face="bold"))

# Search correlations between perceived learning (survey 2) and changes in the motivation factors
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
survey_2_ = survey_2 %>% filter(email %in% ch_mot_factors$email)
pkg_options = c('Q3_1','Q3_2','Q3_3','Q3_4','Q2_1')
ch_mot_factors_ = ch_mot_factors %>% filter(email %in% survey_2_$email)
find_correlations(ch_mot_factors_,survey_2_,ch_factors,pkg_options)

# Search correlations between perceived learning (survey 3) and changes in the motivation factors
survey_3_ = survey_3 %>% filter(email %in% ch_mot_factors$email)
pkg_options = c('Q3_1','Q3_3','Q3_4','Q6_1')
ch_mot_factors_ = ch_mot_factors %>% filter(email %in% survey_3_$email)
find_correlations(ch_mot_factors_,survey_3_,ch_factors,pkg_options)
