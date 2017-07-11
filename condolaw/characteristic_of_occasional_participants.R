####
## This script tries to discover
## factors that characterize the 
## group of people who dropped out
## through the process
####

library(dplyr)
library(reshape2)
library(likert)
library(FSA)
library(BSDA)

# Reading survey data
survey_1 = read.csv("./data/survey1-condlaw-complete.csv",header=TRUE,sep=",",stringsAsFactors=F)
survey_2 = read.csv("./data/survey2-condlaw-complete.csv",header=T, sep=",",stringsAsFactors=F)
survey_3 = read.csv("./data/survey3-condlaw2-complete.csv",header=T, sep=",",stringsAsFactors=F)

# Polish data
survey_1 = survey_1 %>% filter(email!='') %>% select(-V9)
survey_2 = survey_2 %>% filter(email!='') %>% select(-V9)
survey_3 = survey_3 %>% filter(email!='') %>% select(-V9)
# Rename columns adding survey identifier
colnames(survey_1)[2:130] = unlist(lapply(colnames(survey_1[2:130]),function(x) paste(x,'.s1',sep='')))
colnames(survey_2)[2:44] = unlist(lapply(colnames(survey_2[2:44]),function(x) paste(x,'.s2',sep='')))
colnames(survey_3)[2:71] = unlist(lapply(colnames(survey_3[2:71]),function(x) paste(x,'.s3',sep='')))

# Extract data of those who completed all the three surveys
respondents = merge(survey_1, survey_2, by.x = 'email',by.y = 'email')
respondents = merge(respondents, survey_3, by.x = 'email',by.y = 'email')
respondents = filter(respondents, !is.na(Q2_1.s1))
respondents = filter(respondents, !is.na(Q7_1.s2))
respondents = filter(respondents, !is.na(Q2_1.s3))

# Extract data of the occasional participants
occasional = merge(survey_1, survey_2, by.x = 'email', by.y = 'email', all = T)
occasional = merge(occasional, survey_3, by.x = 'email', by.y = 'email', all = T)
occasional = filter(occasional, !email %in% respondents$email)

# Classify occasional participants
cols_s1 = c(2:130)
cols_s2 = c(131:173)
cols_s3 = c(174:243)
survey_responded = data.frame(email=NULL,s1=NULL,s2=NULL,s3=NULL)
occasional$replied_s1 = FALSE
occasional$replied_s2 = FALSE
occasional$replied_s3 = FALSE
for (i in 1:nrow(occasional)) {
  # Check if the participant replied survey 1
  occasional[i,'replied_s1'] = F %in% sapply(occasional[i,cols_s1],is.na)
  # Check if the participant replied survey 2
  occasional[i,'replied_s2'] = F %in% sapply(occasional[i,cols_s2],is.na)
  # Check if the participant replied survey 3
  occasional[i,'replied_s3'] = F %in% sapply(occasional[i,cols_s3],is.na)
}

# Analyze motivation factors of those who responded to only one survey
one_survey = filter(occasional, replied_s1==T & replied_s2==F & replied_s3==F)
one_survey = rbind(one_survey, filter(occasional, replied_s1==F & replied_s2==T & replied_s3==F))
one_survey = rbind(one_survey, filter(occasional, replied_s1==F & replied_s2==F & replied_s3==T))                     
# 38 replied only survey 1, 1 replied only survey 2, and 153 replied only survey 3                    

# Anayze motivation factors of the occasional participants
# Extract those who replied survey 1
mot_factors_s1 = one_survey %>% filter(replied_s1==T) %>% 
  select(email, Q2_1.s1, Q2_2.s1, Q2_3.s1, Q2_4.s1, Q2_5.s1, Q2_6.s1, Q2_7.s1)
colnames(mot_factors_s1) = c('email','F1','F2','F3','F4','F5','F6','F7')
f_mot_factors_s1 = mot_factors_s1
f_mot_factors_s1[,c(2:8)] = lapply(f_mot_factors_s1[,c(2:8)], 
                                 function(x) factor(x,
                                                    levels=c('1','2','3','4','5','6','7'),
                                                    ordered=T))
f_mot_factors_s1$F8 = 'NA'
f_mot_factors_s1$F9 = 'NA'
# Extract those who replied survey 2
mot_factors_s2 = one_survey %>% filter(replied_s2==T) %>% 
  select(email, Q7_1.s2, Q7_2.s2, Q7_3.s2, Q7_4.s2, Q7_5.s2, Q7_6.s2, 
         Q7_7.s2, Q7_8.s2)
colnames(mot_factors_s2) = c('email','F1','F2','F3','F4','F5','F6','F7','F8')
f_mot_factors_s2 = mot_factors_s2
f_mot_factors_s2[,c(2:9)] = lapply(f_mot_factors_s2[,c(2:9)], 
                                   function(x) factor(x,
                                                   levels=c('1','2','3','4','5','6','7'),
                                                   ordered=T))
f_mot_factors_s2$F9 = 'NA'
# Extract those who replied survey 3
mot_factors_s3 = one_survey %>% filter(replied_s3==T) %>% 
  select(email, Q2_1.s3, Q2_2.s3, Q2_3.s3, Q2_4.s3, Q2_5.s3, 
         Q2_7.s3, Q2_8.s3, Q2_9.s3)
colnames(mot_factors_s3) = c('email','F1','F2','F3','F4','F5','F7','F8','F9')
f_mot_factors_s3 = mot_factors_s3
f_mot_factors_s3[,c(2:9)] = lapply(f_mot_factors_s3[,c(2:9)], 
                                   function(x) factor(x,
                                                      levels=c('1','2','3','4','5','6','7'),
                                                      ordered=T))
f_mot_factors_s3$F6 = 'NA'
# Combine all
f_mot_factors = rbind(f_mot_factors_s1,f_mot_factors_s2,f_mot_factors_s3)
likert_ret = likert(f_mot_factors[,c(2:10)])
summary(likert_ret)
plot(likert_ret, type="bar")
plot(likert_ret, type="heat",
     low.color = "white", 
     high.color = "blue",
     text.color = "black", 
     text.size = 4, wrap = 50)
# F2, F1, F8, and F4 are the strongest factors

# Analyze whether the difference between factors is significant
m_f_mot_factors = melt(f_mot_factors, id.vars='email',
                       measure.vars=c('F1','F2','F3','F4','F5',
                                      'F6','F7','F8','F9'))
m_f_mot_factors$value = as.integer(m_f_mot_factors$value)
colnames(m_f_mot_factors) = c('email','factor','score')
kruskal.test(score ~ factor, data = m_f_mot_factors)
# Conduct post-hoc test to see in which factors the difference of
# scores is significant
dunnTest(score ~ factor, data = m_f_mot_factors)
# Difference between F2 and F4 and F2 and F8 is significant.


# Analyze motivation factors of those who responded to two surveys
two_surveys = filter(occasional, replied_s1==T & replied_s2==T & replied_s3==F)
two_surveys = rbind(two_surveys, filter(occasional, replied_s1==T & replied_s2==F & replied_s3==T))
two_surveys = rbind(two_surveys, filter(occasional, replied_s1==F & replied_s2==T & replied_s3==T))
# 49 replied to survey 1 and survey 2, 16 replied survey 1 and 3, 0 replied 2 and 3

# Add the 65 that replied survey 1 to the group of those who replied only one survey
# to see if the new data has some impact on the previous result
to_add = select(two_surveys, email, Q2_1.s1, Q2_2.s1, Q2_3.s1, Q2_4.s1, Q2_5.s1, Q2_6.s1, Q2_7.s1)
colnames(to_add) = c('email','F1','F2','F3','F4','F5','F6','F7')
to_add[,c(2:8)] = lapply(to_add[,c(2:8)], function(x) factor(x,
                                                      levels=c('1','2','3','4','5','6','7'),
                                                      ordered=T))
to_add$F8 = 'NA'
to_add$F9 = 'NA'
f_mot_factors = rbind(f_mot_factors, to_add)
likert_ret = likert(f_mot_factors[,c(2:10)])
summary(likert_ret)
# The group of strongest and weakest factors is the same

# See if changes in the motivation factors in the group of those who replied to two surveys
# remain similar to the changes in the group of the 65 people who responded to the three
# surveys
# Survey 1 and 2
surveys_12 = two_surveys %>% filter(replied_s1==T&replied_s2==T)
# Survey 1
mot_factors_s1 = select(surveys_12, email, Q2_1.s1, Q2_2.s1, Q2_3.s1, Q2_4.s1, Q2_5.s1, Q2_6.s1, Q2_7.s1)
colnames(mot_factors_s1) = c('email','F1','F2','F3','F4','F5','F6','F7')
f_mot_factors_s1 = mot_factors_s1
f_mot_factors_s1[,c(2:8)] = lapply(f_mot_factors_s1[,c(2:8)], 
                                   function(x) factor(x,
                                                      levels=c('1','2','3','4','5','6','7'),
                                                      ordered=T))
f_mot_factors_s1$survey = 'survey_1'
# Survey 2
mot_factors_s2 = select(surveys_12, email, Q7_1.s2, Q7_2.s2, Q7_3.s2, Q7_4.s2, Q7_5.s2, Q7_6.s2, Q7_7.s2)
colnames(mot_factors_s2) = c('email','F1','F2','F3','F4','F5','F6','F7')
f_mot_factors_s2 = mot_factors_s2
f_mot_factors_s2[,c(2:8)] = lapply(f_mot_factors_s2[,c(2:8)], 
                                   function(x) factor(x,
                                                      levels=c('1','2','3','4','5','6','7'),
                                                      ordered=T))
f_mot_factors_s2$survey = 'survey_2'
# Both surveys
f_mot_surveys = rbind(f_mot_factors_s1,f_mot_factors_s2)
f_mot_surveys$survey = factor(f_mot_surveys$survey,
                              levels=c('survey_2','survey_1'),
                              ordered=T)
# Remove NAs
emails_to_remove = c()
for (i in 1:nrow(f_mot_surveys)) {
  if (T %in% sapply(f_mot_surveys[i,],is.na)) {
    emails_to_remove = cbind(emails_to_remove,f_mot_surveys[i,'email'])    
  }
}
f_mot_surveys = filter(f_mot_surveys, !email %in% emails_to_remove)
likert_ret = likert(f_mot_surveys[,c(2:8)], grouping=f_mot_surveys$survey)
summary(likert_ret)
plot(likert_ret, type="bar") + 
  ggtitle(paste('Motivation Factors - Respondents of Survey 1 and 2 (N=',nrow(f_mot_surveys)/2,')',sep=''))
# See how scores vary between survey 1 and survey 2
m_f_mot_surveys = melt(f_mot_surveys, id.vars='survey', 
                       measure.vars=c('F1','F2','F3','F4','F5','F6','F7'))
m_f_mot_surveys$value = as.integer(m_f_mot_surveys$value)
colnames(m_f_mot_surveys) = c('survey','factor','score')
for (c_factor in c('F1','F2','F3','F4','F5','F6','F7')) {
  ret = wilcox.test(score ~ survey, data=m_f_mot_surveys[m_f_mot_surveys$factor==c_factor,], paired=T)
  print(paste('Factor:',c_factor,'p-value',ret$p.value))
}

# Survey 1 and 3
surveys_13 = two_surveys %>% filter(replied_s1==T&replied_s3==T)
# Survey 1
mot_factors_s1 = select(surveys_13, email, Q2_1.s1, Q2_2.s1, Q2_3.s1, Q2_4.s1, Q2_5.s1, Q2_7.s1)
colnames(mot_factors_s1) = c('email','F1','F2','F3','F4','F5','F7')
f_mot_factors_s1 = mot_factors_s1
f_mot_factors_s1[,c(2:7)] = lapply(f_mot_factors_s1[,c(2:7)], 
                                   function(x) factor(x,
                                                      levels=c('1','2','3','4','5','6','7'),
                                                      ordered=T))
f_mot_factors_s1$survey = 'survey_1'
# Survey 3
mot_factors_s3 = select(surveys_13, email, Q2_1.s3, Q2_2.s3, Q2_3.s3, Q2_4.s3, Q2_5.s3, Q2_7.s3)
colnames(mot_factors_s3) = c('email','F1','F2','F3','F4','F5','F7')
f_mot_factors_s3 = mot_factors_s3
f_mot_factors_s3[,c(2:7)] = lapply(f_mot_factors_s3[,c(2:7)], 
                                   function(x) factor(x,
                                                      levels=c('1','2','3','4','5','6','7'),
                                                      ordered=T))
f_mot_factors_s3$survey = 'survey_3'
# Both surveys
f_mot_surveys = rbind(f_mot_factors_s1,f_mot_factors_s3)
f_mot_surveys$survey = factor(f_mot_surveys$survey,
                              levels=c('survey_3','survey_1'),
                              ordered=T)
# Remove NAs
emails_to_remove = c()
for (i in 1:nrow(f_mot_surveys)) {
  if (T %in% sapply(f_mot_surveys[i,],is.na)) {
    emails_to_remove = cbind(emails_to_remove,f_mot_surveys[i,'email'])    
  }
}
f_mot_surveys = filter(f_mot_surveys, !email %in% emails_to_remove)
likert_ret = likert(f_mot_surveys[,c(2:7)], grouping=f_mot_surveys$survey)
summary(likert_ret)
plot(likert_ret, type="bar") + 
  ggtitle(paste('Motivation Factors - Respondents of Survey 1 and 3 (N=',nrow(f_mot_surveys)/2,')',sep=''))
# See how scores vary between survey 1 and survey 2
m_f_mot_surveys = melt(f_mot_surveys, id.vars='survey', 
                       measure.vars=c('F1','F2','F3','F4','F5','F7'))
m_f_mot_surveys$value = as.integer(m_f_mot_surveys$value)
colnames(m_f_mot_surveys) = c('survey','factor','score')
for (c_factor in c('F1','F2','F3','F4','F5','F7')) {
  ret = wilcox.test(score ~ survey, data=m_f_mot_surveys[m_f_mot_surveys$factor==c_factor,], paired=T)
  print(paste('Factor:',c_factor,'p-value',ret$p.value))
}

