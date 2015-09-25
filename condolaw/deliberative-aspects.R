#######
# An analysis of democratic and deliberative aspects of the process
#######

# Load libraries
library(dplyr)
source('utils.R')

# Define columns' labels
col_labels = c('The tone of the exchanges on the platform was generally\ncivil and respectful',
               'The information produced and shared was generally accurate',
               'Participants made a genuine effort to hear and understand\nthe other side and their views',
               'Participants seemed sincere in their opinions and arguments',
               'Different viewpoints related to the law were brought up in\nthe conversation',
               'The participant group was diverse and represented well\nstakeholders that are affected by the law',
               'The discussion resulted to a conclusion',
               'The participants accepted differences of opinions',
               'Participants spoke with an equal degree of authority')

# Load data
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
survey_2 = raw_survey_2 %>% select(email,Q5_19,Q5_20,Q5_21,Q5_22,Q4_7,Q4_8,Q6_1,Q6_2,Q6_3) %>% filter(!is.na(Q5_19))
survey_3 = raw_survey_3 %>% select(email,Q5_19,Q5_20,Q5_21,Q5_22,Q7_7,Q7_8) %>% filter(!is.na(Q5_19))

survey_23 = merge(survey_2,survey_3,by.x='email',by.y='email')
survey_2 = filter(survey_2,email %in% survey_23$email)
survey_3 = filter(survey_3,email %in% survey_23$email)

melt_survey_2 = melt(survey_2, id.vars='email', measure.vars=c('Q5_19','Q5_20','Q5_21','Q5_22','Q4_7','Q4_8'))
melt_survey_3 = melt(survey_3, id.vars='email', measure.vars=c('Q5_19','Q5_20','Q5_21','Q5_22','Q7_7','Q7_8'))

melt_survey_2$value = as.numeric(as.character(melt_survey_2$value))
melt_survey_3$value = as.numeric(as.character(melt_survey_3$value))

melt_survey_2$group[melt_survey_2$variable=='Q5_19'] = col_labels[1]
melt_survey_2$group[melt_survey_2$variable=='Q5_20'] = col_labels[2]
melt_survey_2$group[melt_survey_2$variable=='Q5_21'] = col_labels[3]
melt_survey_2$group[melt_survey_2$variable=='Q5_22'] = col_labels[4]
melt_survey_2$group[melt_survey_2$variable=='Q4_7'] = col_labels[5]
melt_survey_2$group[melt_survey_2$variable=='Q4_8'] = col_labels[6]
melt_survey_2$survey = 'Survey 2'

melt_survey_3$group[melt_survey_3$variable=='Q5_19'] = col_labels[1]
melt_survey_3$group[melt_survey_3$variable=='Q5_20'] = col_labels[2]
melt_survey_3$group[melt_survey_3$variable=='Q5_21'] = col_labels[3]
melt_survey_3$group[melt_survey_3$variable=='Q5_22'] = col_labels[4]
melt_survey_3$group[melt_survey_3$variable=='Q7_7'] = col_labels[5]
melt_survey_3$group[melt_survey_3$variable=='Q7_8'] = col_labels[6]
melt_survey_3$survey = 'Survey 3'
both_melts = rbind(melt_survey_2,melt_survey_3)

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

# Draw a boxplot
ggplot(both_melts, aes(x=group, y=value, fill=group)) + 
  geom_boxplot() + 
  facet_grid(. ~ survey) +
  scale_y_continuous(breaks=c(0:15)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=16, color='black'), 
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=17), axis.title.y = element_text(size=15),
        strip.text.x = element_text(size=18, face="bold")) + 
  labs(y="Scores") +
  guides(fill=FALSE)

# Calculate centre of the data and see if answers are positive
survey_2 = raw_survey_2 %>% select(email,Q5_19,Q5_20,Q5_21,Q5_22,Q4_7,Q4_8,Q6_1,Q6_2,Q6_3) %>% filter(!is.na(Q5_19))
survey_3 = raw_survey_3 %>% select(email,Q5_19,Q5_20,Q5_21,Q5_22,Q7_7,Q7_8) %>% filter(!is.na(Q5_19))
for (i in c(2:10)) {
  col_name = colnames(survey_2)[i]
  print(paste('Mean ',col_name, ' : ',round(mean(survey_2[,i], na.rm=T),2),sep=''))
  print(paste('Median ',col_name, ' : ',round(median(survey_2[,i], na.rm=T),0),sep=''))
  print(paste('Answer significantly positive, p-value: ',signif(t.test(survey_2[,i],mu=3.5,alternative='greater')$p.value,3),sep=''))
}
for (i in c(2:7)) {
  col_name = colnames(survey_3)[i]
  print(paste('Mean ',col_name, ' : ',round(mean(survey_3[,i], na.rm=T),2),sep=''))
  print(paste('Median ',col_name, ' : ',round(median(survey_3[,i], na.rm=T),0),sep=''))
  print(paste('Answer significantly positive, p-value: ',signif(t.test(survey_3[,i],mu=3.5,alternative='greater')$p.value,3),sep=''))
}

# Analyze associations between deliberative and democratic aspects (survey 2) and motivation factors (survey 1)
mot_survey_1_ = filter(mot_survey_1, email %in% survey_2$email)
survey_2_ = filter(survey_2, email %in% mot_survey_1_$email)
factors_s1 = c('o1s1', 'o2s1', 'o3s1', 'o4s1', 'o5s1', 'o6s1', 'o7s1')
options_s2 = c('Q5_19','Q5_20','Q5_21','Q5_22','Q4_7','Q4_8','Q6_1','Q6_2','Q6_3')
find_correlations(mot_survey_1_,survey_2_,factors_s1,options_s2)

# Analyze associations between deliberative and democratic aspects (survey 3) and motivation factors (survey 1)
mot_survey_1_ = filter(mot_survey_1, email %in% survey_3$email)
survey_3_ = filter(survey_3, email %in% mot_survey_1_$email)
options_s3 = c('Q5_19','Q5_20','Q5_21','Q5_22','Q7_7','Q7_8')
find_correlations(mot_survey_1_,survey_3_,factors_s1,options_s3)

# Analyze associations between deliberative and democratic aspects (survey 2) and motivation factors (survey 2)
mot_survey_2_ = filter(mot_survey_2, email %in% survey_2$email)
survey_2_ = filter(survey_2, email %in% mot_survey_2_$email)
factors_s2 = c('o1s2', 'o2s2', 'o3s2', 'o4s2', 'o5s2', 'o6s2', 'o7s2', 'o8s2')
find_correlations(mot_survey_2_,survey_2_,factors_s2,options_s2)

# Analyze associations between deliberative and democratic aspects (survey 3) and motivation factors (survey 2)
mot_survey_2_ = filter(mot_survey_2, email %in% survey_3$email)
survey_3_ = filter(survey_3, email %in% mot_survey_2_$email)
find_correlations(mot_survey_2_,survey_3_,factors_s2,options_s3)

# Analyze associations between deliberative and democratic aspects (survey 2) and motivation factors (survey 3)
mot_survey_3_ = filter(mot_survey_3, email %in% survey_2$email)
survey_2_ = filter(survey_2, email %in% mot_survey_3_$email)
factors_s3 = c('o1s3', 'o2s3', 'o3s3', 'o4s3', 'o5s3', 'o6s3', 'o7s3', 'o8s3')
find_correlations(mot_survey_3_,survey_2_,factors_s3,options_s2)

# Analyze associations between deliberative and democratic aspects (survey 3) and motivation factors (survey 3)
mot_survey_3_ = filter(mot_survey_3, email %in% survey_3$email)
survey_3_ = filter(survey_3, email %in% mot_survey_3_$email)
find_correlations(mot_survey_3_,survey_3_,factors_s3,options_s3)

# Analyze associations between deliberative and democratic aspects (survey 2) and changes in the motivation factors
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
survey_2_ = filter(survey_2, email %in% ch_mot_factors$email)
ch_mot_factors_ = filter(ch_mot_factors, email %in% survey_2_$email)
find_correlations(ch_mot_factors_,survey_2_,ch_factors,options_s2)

# Analyze associations between deliberative and democratic aspects (survey 3) and changes in the motivation factors
survey_3_ = filter(survey_3, email %in% ch_mot_factors$email)
ch_mot_factors_ = filter(ch_mot_factors, email %in% survey_3_$email)
find_correlations(ch_mot_factors_,survey_3_,ch_factors,options_s3)

# Analyze correlation between deliberative and democratic aspects (survey 2) and level of participations (survey 1 and 2)
participation_s1s2 = mutate(participation_s1s2,content=ideas+votes+comments)
participation_s1s2 = mutate(participation_s1s2,ideas_votes=ideas+votes)
participation_s1s2 = mutate(participation_s1s2,ideas_comments=ideas+comments)
participation_s1s2 = mutate(participation_s1s2,votes_comments=votes+comments)
participation_s1s2_ = filter(participation_s1s2, email %in% survey_2$email)
participation_types = c('content', 'ideas_votes', 'ideas_comments', 'votes_comments', 'votes', 'ideas', 'comments')
find_correlations(participation_s1s2_,survey_2,participation_types,options_s2)

# Analyze correlation between deliberative and democratic aspects (survey 3) and level of participations (survey 1 and 2)
participation_s1s2_ = filter(participation_s1s2, email %in% survey_3$email)
survey_3_ = filter(survey_3, email %in% participation_s1s2_$email)
find_correlations(participation_s1s2_,survey_3_,participation_types,options_s3)

# Analyze correlation between deliberative and democratic aspects (survey 2) and level of participations (survey 3)
participation_s3 = mutate(participation_s3,content=ideas+votes+comments)
participation_s3 = mutate(participation_s3,ideas_votes=ideas+votes)
participation_s3 = mutate(participation_s3,ideas_comments=ideas+comments)
participation_s3 = mutate(participation_s3,votes_comments=votes+comments)
participation_s3_ = filter(participation_s3, email %in% survey_2$email)
find_correlations(participation_s3_,survey_2,participation_types,options_s2)

# Analyze correlation between deliberative and democratic aspects (survey 3) and level of participations (survey 3)
participation_s3_ = filter(participation_s3, email %in% survey_3$email)
find_correlations(participation_s3_,survey_3,participation_types,options_s3)

# Analyze correlation between deliberative and democratic aspects (survey 2) and self-efficacy (survey 1)
cols_se = c('Q3_1','Q3_2','Q4_1','Q4_2','Q4_3','Q4_4','Q4_5','Q5_1','Q5_3','Q5_4','Q5_5','Q5_6','Q5_7','Q5_14')
se_survey_1 = raw_survey_1 %>% select(email,Q3_1,Q3_2,Q4_1,Q4_2,Q4_3,Q4_4,Q4_5,Q5_1,Q5_3,Q5_4,Q5_5,Q5_6,Q5_7,Q5_14) %>% filter(!is.na(Q3_1))
se_survey_1 = filter(se_survey_1, email %in% survey_2$email)
survey_2_ = filter(survey_2, email %in% se_survey_1$email)
find_correlations(se_survey_1,survey_2_,cols_se,options_s2)

# Analyze correlation between deliberative and democratic aspects (survey 2) and self-efficacy (survey 3)
cols_se = c('Q3_2','Q6_4','Q6_5','Q7_2')
se_survey_3 = raw_survey_3 %>% select(email,Q3_2,Q6_4,Q6_5,Q7_2) %>% filter(!is.na(Q3_2))
se_survey_3 = filter(se_survey_3, email %in% survey_2$email)
survey_2_ = filter(survey_2, email %in% se_survey_3$email)
find_correlations(se_survey_3,survey_2_,cols_se,options_s2)

# Analyze correlation between deliberative and democratic aspects (survey 3) and self-efficacy (survey 1)
cols_se = c('Q3_1','Q3_2','Q4_1','Q4_2','Q4_3','Q4_4','Q4_5','Q5_1','Q5_3','Q5_4','Q5_5','Q5_6','Q5_7','Q5_14')
se_survey_1 = raw_survey_1 %>% select(email,Q3_1,Q3_2,Q4_1,Q4_2,Q4_3,Q4_4,Q4_5,Q5_1,Q5_3,Q5_4,Q5_5,Q5_6,Q5_7,Q5_14) %>% filter(!is.na(Q3_1))
se_survey_1 = filter(se_survey_1, email %in% survey_3$email)
survey_3_ = filter(survey_3, email %in% se_survey_1$email)
find_correlations(se_survey_1,survey_3_,cols_se,options_s3)

# Analyze correlation between deliberative and democratic aspects (survey 3) and self-efficacy (survey 3)
cols_se = c('Q3_2','Q6_4','Q6_5','Q7_2')
se_survey_3 = raw_survey_3 %>% select(email,Q3_2,Q6_4,Q6_5,Q7_2) %>% filter(!is.na(Q3_2))
se_survey_3 = filter(se_survey_3, email %in% survey_3$email)
survey_3_ = filter(survey_3, email %in% se_survey_3$email)
find_correlations(se_survey_3,survey_3_,cols_se,options_s3)

# Analyze correlation between deliberative and democratic aspects (survey 2) and learning (survey 1)
cols_le_s1 = c('Q6_3','Q6_4','Q6_5')
le_survey_1 = raw_survey_1 %>% select(email,Q6_3,Q6_4,Q6_5) %>% filter(!is.na(Q6_3))
le_survey_1_ = filter(le_survey_1, email %in% survey_2$email)
survey_2_ = filter(survey_2, email %in% le_survey_1_$email)
find_correlations(le_survey_1_,survey_2_,cols_le_s1,options_s2)

# Analyze correlation between deliberative and democratic aspects (survey 3) and learning (survey 1)
le_survey_1_ = filter(le_survey_1, email %in% survey_3$email)
survey_3_ = filter(survey_3, email %in% le_survey_1_$email)
find_correlations(le_survey_1_,survey_3_,cols_le_s1,options_s3)

# Analyze correlation between deliberative and democratic aspects (survey 2) and learning (survey 2)
cols_le_s2 = c('Q11_6','Q11_7','Q11_8')
le_survey_2 = raw_survey_2 %>% select(email,Q11_6,Q11_7,Q11_8) %>% filter(!is.na(Q11_6))
le_survey_2_ = filter(le_survey_2, email %in% survey_2$email)
survey_2_ = filter(survey_2, email %in% le_survey_2_$email)
find_correlations(le_survey_2_,survey_2_,cols_le_s2,options_s2)

# Analyze correlation between deliberative and democratic aspects (survey 3) and learning (survey 2)
le_survey_2_ = filter(le_survey_2, email %in% survey_3$email)
survey_3_ = filter(survey_3, email %in% le_survey_2_$email)
find_correlations(le_survey_2_,survey_3_,cols_le_s2,options_s3)

# Analyze correlation between deliberative and democratic aspects (survey 2) and learning (survey 3)
cols_le_s3 = c('Q11_6','Q11_7','Q11_8')
le_survey_3 = raw_survey_3 %>% select(email,Q11_6,Q11_7,Q11_8) %>% filter(!is.na(Q11_6))
le_survey_3_ = filter(le_survey_3, email %in% survey_2$email)
survey_2_ = filter(survey_2, email %in% le_survey_3_$email)
find_correlations(le_survey_3_,survey_2_,cols_le_s3,options_s2)

# Analyze correlation between deliberative and democratic aspects (survey 3) and learning (survey 3)
le_survey_3_ = filter(le_survey_3, email %in% survey_3$email)
survey_3_ = filter(survey_3, email %in% le_survey_3_$email)
find_correlations(le_survey_3_,survey_3_,cols_le_s3,options_s3)

# Analyze correlation between deliberative and democratic aspects (survey 2) and trust in the law-making process
cols_t = c('Q9_7','Q9_8','Q9_9','Q9_10','Q9_12')
t_survey_1 = raw_survey_1 %>% select(email,Q9_7,Q9_8,Q9_9,Q9_10,Q9_12) %>% filter(!is.na(Q9_7))
t_survey_1_ = filter(t_survey_1, email %in% survey_2$email)
survey_2_ = filter(survey_2, email %in% t_survey_1_$email)
find_correlations(t_survey_1_,survey_2_,cols_t,options_s2)

# Analyze correlation between deliberative and democratic aspects (survey 3) and trust in the law-making process
t_survey_1_ = filter(t_survey_1, email %in% survey_3$email)
survey_3_ = filter(survey_3, email %in% t_survey_1_$email)
find_correlations(t_survey_1_,survey_3_,cols_t,options_s3)

# Analyze correlation between deliberative and democratic aspects (survey 2) and perceived knwoledge gain (survey 2)
pkl_cols_s2 = c('Q3_1','Q3_2','Q3_3','Q3_4','Q2_1')
pkl_survey_2 = raw_survey_2 %>% select(email,Q3_1,Q3_2,Q3_3,Q3_4,Q2_1) %>% filter(!is.na(Q3_1))
pkl_survey_2_ = filter(pkl_survey_2, email %in% survey_2$email)
survey_2_ = filter(survey_2, email %in% pkl_survey_2_$email)
find_correlations(pkl_survey_2_,survey_2_,pkl_cols_s2,options_s2)

# Analyze correlation between deliberative and democratic aspects (survey 3) and perceived knwoledge gain (survey 2)
pkl_survey_2_ = filter(pkl_survey_2, email %in% survey_3$email)
survey_3_ = filter(survey_3, email %in% pkl_survey_2_$email)
find_correlations(pkl_survey_2_,survey_3_,pkl_cols_s2,options_s3)

# Analyze correlation between deliberative and democratic aspects (survey 2) and perceived knwoledge gain (survey 3)
pkl_cols_s3 = c('Q3_1','Q3_3','Q3_4','Q6_1')
pkl_survey_3 = raw_survey_3 %>% select(email,Q3_1,Q3_3,Q3_4,Q6_1) %>% filter(!is.na(Q3_1))
pkl_survey_3_ = filter(pkl_survey_3, email %in% survey_2$email)
survey_2_ = filter(survey_2, email %in% pkl_survey_3_$email)
find_correlations(pkl_survey_3_,survey_2_,pkl_cols_s3,options_s2)

# Analyze correlation between deliberative and democratic aspects (survey 3) and perceived knwoledge gain (survey 3)
pkl_survey_3_ = filter(pkl_survey_3, email %in% survey_3$email)
survey_3_ = filter(survey_3, email %in% pkl_survey_3_$email)
find_correlations(pkl_survey_3_,survey_3_,pkl_cols_s3,options_s3)