#######
# An analysis of participants' happiness with the law
#######

# Load libraries
library(dplyr)
source('utils.R')

# Load data
raw_survey_1 = read.csv("./data/survey1-condlaw-complete.csv",header=T, sep=",", stringsAsFactors=F)
raw_survey_2 = read.csv("./data/survey2-condlaw-complete.csv",header=T, sep=",", stringsAsFactors=F)
mot_survey_1 = read.csv("./data/survey1-condlaw-no-duplicates.csv",header=T, sep=",",stringsAsFactors=F)
mot_survey_2 = read.csv("./data/survey2-condlaw-no-duplicates.csv",header=T, sep=",",stringsAsFactors=F)
mot_survey_3 = read.csv("./data/survey3-condlaw-no-duplicates2.csv",header=T, sep=",",stringsAsFactors=F)

# Define columns' labels
survey_1_labels = c('There are a troubling amount of disagreements\nin condominium companies',
                    'There would be less disagreements, if information delivery\nwas handled better in condominium companies.',
                    'To resolve the disagreements, there is a need to establish\nnew mediating and solving bodies, lead by officials',
                    'There is a need for new solutions to\nresolve the disagreements.',
                    'Finnish condominium companies are\ngoverned professionally')
survey_2_labels = c('The limited liability condominium\ncompany law is too ambiguous',
                    'The current limited liability condominium\ncompany law regulates things well enough,\nand the law doesn’t need to be changed.',
                    'The law regulates too much activities in\ncondominium companies.')

# Data preparation
survey_1 = raw_survey_1 %>% select(email,Q8_1,Q8_2,Q8_4,Q8_5,Q8_8)
survey_2 = raw_survey_2 %>% select(email,Q12_1,Q12_2,Q12_4,Q12_5,Q12_8,Q13_3,Q13_5,Q13_7)
survey_12 = merge(survey_1,survey_2,by.x='email',by.y='email')
survey_1 = filter(survey_1,email %in% survey_12$email)
survey_2 = filter(survey_2,email %in% survey_12$email)
melt_survey_1 = melt(survey_1, id.vars='email', measure.vars=c('Q8_1','Q8_2','Q8_4','Q8_5','Q8_8'))
melt_survey_2 = melt(survey_2, id.vars='email', measure.vars=c('Q12_1','Q12_2','Q12_4','Q12_5','Q12_8'))
melt_survey_1$value = as.numeric(as.character(melt_survey_1$value))
melt_survey_2$value = as.numeric(as.character(melt_survey_2$value))
melt_survey_1$group[melt_survey_1$variable=='Q8_1'] = survey_1_labels[1]
melt_survey_1$group[melt_survey_1$variable=='Q8_2'] = survey_1_labels[2]
melt_survey_1$group[melt_survey_1$variable=='Q8_4'] = survey_1_labels[3]
melt_survey_1$group[melt_survey_1$variable=='Q8_5'] = survey_1_labels[4]
melt_survey_1$group[melt_survey_1$variable=='Q8_8'] = survey_1_labels[5]
melt_survey_1$survey = '1. Pre-Process'
melt_survey_2$group[melt_survey_2$variable=='Q12_1'] = survey_1_labels[1]
melt_survey_2$group[melt_survey_2$variable=='Q12_2'] = survey_1_labels[2]
melt_survey_2$group[melt_survey_2$variable=='Q12_4'] = survey_1_labels[3]
melt_survey_2$group[melt_survey_2$variable=='Q12_5'] = survey_1_labels[4]
melt_survey_2$group[melt_survey_2$variable=='Q12_8'] = survey_1_labels[5]
melt_survey_2$survey = '2. Post-Process'
both_melts = rbind(melt_survey_1,melt_survey_2)

colnames(mot_survey_1) = c('email', 'datetime', 'o1s1', 'o2s1', 'o3s1', 'o4s1', 'o5s1', 'o6s1', 'o7s1')
colnames(mot_survey_2) = c('email', 'datetime', 'o1s2', 'o2s2', 'o3s2', 'o4s2', 'o5s2', 'o6s2', 'o7s2', 'o8s2')
colnames(mot_survey_3) = c('email', 'datetime', 'o1s3', 'o2s3', 'o3s3', 'o4s3', 'o5s3', 'o6s3', 'o7s3', 'o8s3')
mot_survey_1 = mot_survey_1 %>% filter(email!='') %>% filter(!is.na(o1s1)) %>% filter(o1s1!='') %>% select(-datetime)
mot_survey_2 = mot_survey_2 %>% filter(email!='') %>% filter(!is.na(o1s2)) %>% filter(o1s2!='') %>% select(-datetime)
mot_survey_3 = mot_survey_3 %>% filter(email!='') %>% filter(!is.na(o1s3)) %>% filter(o1s3!='') %>% select(-datetime)

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

# Further explore the found differences
median(survey_1[,'Q8_4'],na.rm=T)
mean(survey_1[,'Q8_4'],na.rm=T)
median(survey_2[,'Q12_4'],na.rm=T)
mean(survey_2[,'Q12_4'],na.rm=T)
t.test(survey_1[,'Q8_4'],survey_2[,'Q12_4'])
median(survey_1[,'Q8_5'],na.rm=T)
mean(survey_1[,'Q8_5'],na.rm=T)
median(survey_2[,'Q12_5'],na.rm=T)
mean(survey_2[,'Q12_5'],na.rm=T)
t.test(survey_1[,'Q8_5'],survey_2[,'Q12_5'])

# Further explore the significance of answers
for (i in c(2:9)) {
  t_test = t.test(survey_2[,i],mu=3.5,alternative='greater')
  print(paste('Option: ',i-1,' p-value: ',t_test$p.value,sep=''))
}
t_test = t.test(survey_2[,i],mu=3.5,alternative='less')

# Explore additional happiness options of survey 2
melt_survey_2 = melt(survey_2, id.vars='email', measure.vars=c('Q13_3','Q13_5','Q13_7'))
melt_survey_2$group[melt_survey_2$variable=='Q13_3'] = survey_2_labels[1]
melt_survey_2$group[melt_survey_2$variable=='Q13_5'] = survey_2_labels[2]
melt_survey_2$group[melt_survey_2$variable=='Q13_7'] = survey_2_labels[3]

ggplot(melt_survey_2, aes(x=group, y=value, fill=group)) + 
  geom_boxplot() + 
  scale_y_continuous(breaks=c(0:15)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=16, color='black'), 
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=17), axis.title.y = element_text(size=15),
        strip.text.x = element_text(size=18, face="bold")) + 
  labs(y="Scores") +
  guides(fill=FALSE)

# Analyze associations between happiness (survey 1) and changes in the motivation factors
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
survey_1_ = filter(survey_1, email %in% ch_mot_factors$email)
find_correlations(ch_mot_factors,survey_1_,ch_factors,c('Q8_1','Q8_2','Q8_4','Q8_5','Q8_8'))

# Analyze associations between happiness (survey 2) and changes in the motivation factors
survey_2_ = filter(survey_2, email %in% ch_mot_factors$email)
find_correlations(ch_mot_factors,survey_2_,ch_factors,c('Q12_1','Q12_2','Q12_4','Q12_5','Q12_8','Q13_3','Q13_5','Q13_7'))
