#######
# An analysis of participants' level of trust in the government
#######

# Load libraries
library(dplyr)
source('utils.R')

# Reading data
raw_survey_2 = read.csv("./data/survey2-condlaw-complete.csv",header=T, sep=",", stringsAsFactors=F)
raw_survey_3 = read.csv("./data/survey3-condlaw2-complete.csv",header=T, sep=",", stringsAsFactors=F)
mot_survey_1 = read.csv("./data/survey1-condlaw-no-duplicates.csv",header=T, sep=",",stringsAsFactors=F)
mot_survey_2 = read.csv("./data/survey2-condlaw-no-duplicates.csv",header=T, sep=",",stringsAsFactors=F)
mot_survey_3 = read.csv("./data/survey3-condlaw-no-duplicates2.csv",header=T, sep=",",stringsAsFactors=F)

# Data preparation
survey_2 = raw_survey_2 %>% select(email,Q4_6)
survey_3 = raw_survey_3 %>% select(email,Q7_6)
colnames(mot_survey_1) = c('email', 'datetime', 'o1s1', 'o2s1', 'o3s1', 'o4s1', 'o5s1', 'o6s1', 'o7s1')
colnames(mot_survey_2) = c('email', 'datetime', 'o1s2', 'o2s2', 'o3s2', 'o4s2', 'o5s2', 'o6s2', 'o7s2', 'o8s2')
colnames(mot_survey_3) = c('email', 'datetime', 'o1s3', 'o2s3', 'o3s3', 'o4s3', 'o5s3', 'o6s3', 'o7s3', 'o8s3')
mot_survey_1 = mot_survey_1 %>% filter(email!='') %>% filter(!is.na(o1s1)) %>% filter(o1s1!='') %>% select(-datetime)
mot_survey_2 = mot_survey_2 %>% filter(email!='') %>% filter(!is.na(o1s2)) %>% filter(o1s2!='') %>% select(-datetime)
mot_survey_3 = mot_survey_3 %>% filter(email!='') %>% filter(!is.na(o1s3)) %>% filter(o1s3!='') %>% select(-datetime)
dataset_s2 = merge(survey_2,mot_survey_2,by.x='email',by.y='email')
mot_survey_2 = filter(mot_survey_2, email %in% dataset_s2$email)
survey_2 = filter(survey_2, email %in% dataset_s2$email)
dataset_s3 = merge(survey_3,mot_survey_3,by.x='email',by.y='email')
mot_survey_3 = filter(mot_survey_3, email %in% dataset_s3$email)
survey_3 = filter(survey_3, email %in% dataset_s3$email)

# Calculate the averages
mean(survey_2[,2], na.rm=T)
median(survey_2[,2], na.rm=T)
mean(survey_3[,2], na.rm=T)
median(survey_3[,2], na.rm=T)

#Analyze whether there is an association between motivation factors and group efficacy (survey 2)
for (factor in c('o1s2', 'o2s2', 'o3s2', 'o4s2', 'o5s2', 'o6s2', 'o7s2', 'o8s2')) {
  for (option in c('Q4_6')) {
    ret = prepare_data(mot_survey_2, survey_2, factor, option)
    cor_coef = cor(ret$ds1,ret$ds2)
    print(paste('The correlation between factor ',factor,' and option ',option,' is ',cor_coef,sep=''))
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

#Analyze whether there is an association between motivation factors and group efficacy (survey 3)
for (factor in c('o1s3', 'o2s3', 'o3s3', 'o4s3', 'o5s3', 'o6s3', 'o7s3', 'o8s3')) {
  for (option in c('Q7_6')) {
    ret = prepare_data(mot_survey_3, survey_3, factor, option)
    cor_coef = cor(ret$ds1,ret$ds2)
    print(paste('The correlation between factor ',factor,' and option ',option,' is ',cor_coef,sep=''))
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

# Analyze correlations between trust in the government (survey 2) and changes in the motivation factors
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
find_correlations(ch_mot_factors,survey_2_,ch_factors,c('Q4_6'))

# Analyze correlations between trust in the government (survey 3) and changes in the motivation factors
survey_3_ = survey_3 %>% filter(email %in% ch_mot_factors$email)
find_correlations(ch_mot_factors,survey_3_,ch_factors,c('Q7_6'))
