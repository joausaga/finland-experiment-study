#######
# An analysis of changes in the motivation factors
# taking in consideration the first, second, and third survey
#######

# Load libraries
library(ggplot2)
library(dplyr)
library(nortest)
library(xtable)
library(reshape)
source('utils.R')

# Reading data
mot_survey_1 = read.csv("./data/survey1-condlaw-no-duplicates.csv",header=T, sep=",",stringsAsFactors=F)
mot_survey_2 = read.csv("./data/survey2-condlaw-no-duplicates.csv",header=T, sep=",",stringsAsFactors=F)
mot_survey_3 = read.csv("./data/survey3-condlaw-no-duplicates2.csv",header=T, sep=",",stringsAsFactors=F)

# Data preparation
colnames(mot_survey_1) = c('email', 'datetime', 'o1s1', 'o2s1', 'o3s1', 'o4s1', 'o5s1', 'o6s1', 'o7s1')
colnames(mot_survey_2) = c('email', 'datetime', 'o1s2', 'o2s2', 'o3s2', 'o4s2', 'o5s2', 'o6s2', 'o7s2', 'o8s2')
colnames(mot_survey_3) = c('email', 'datetime', 'o1s3', 'o2s3', 'o3s3', 'o4s3', 'o5s3', 'o6s3', 'o7s3', 'o8s3')
mot_survey_1 = mot_survey_1 %>% filter(email!='') %>% filter(!is.na(o1s1)) %>% filter(o1s1!='') %>% select(-datetime)
mot_survey_2 = mot_survey_2 %>% filter(email!='') %>% filter(!is.na(o1s2)) %>% filter(o1s2!='') %>% select(-datetime)
mot_survey_3 = mot_survey_3 %>% filter(email!='') %>% filter(!is.na(o1s3)) %>% filter(o1s3!='') %>% select(-datetime)

mot_surveys = merge(mot_survey_1, mot_survey_2, by.x='email', by.y='email', all=F)
mot_surveys = merge(mot_surveys, mot_survey_3, by.x='email', by.y='email', all=F)
#mot_survey_1 = mot_survey_1 %>% filter(email %in% mot_surveys$email) %>% select(-o6s1)
mot_survey_1 = mot_survey_1 %>% filter(email %in% mot_surveys$email)
#mot_survey_2 = mot_survey_2 %>% filter(email %in% mot_surveys$email) %>% select(-o6s2, -o8s2)
mot_survey_2 = mot_survey_2 %>% filter(email %in% mot_surveys$email)
#mot_survey_3 = mot_survey_3 %>% filter(email %in% mot_surveys$email) %>% select(-(o7s3:o8s3))
mot_survey_3 = mot_survey_3 %>% filter(email %in% mot_surveys$email)

# Merge data
mdata_1 = melt(mot_survey_1, id.vars='email', measure.vars=c('o1s1', 'o2s1', 'o3s1', 'o4s1', 'o5s1', 'o6s1', 'o7s1'))
mdata_1$variable = ifelse(mdata_1[,'variable']=='o1s1', 'F1. Solve problems in\nhomeowners\' associations', mdata_1[,'variable'])
mdata_1$variable = ifelse(mdata_1[,'variable']=='2', 'F2. Improve the Housing\ncompany law', mdata_1[,'variable'])
mdata_1$variable = ifelse(mdata_1[,'variable']=='3', 'F3. Unhappy with my own\nhomeowner\'s association', mdata_1[,'variable'])
mdata_1$variable = ifelse(mdata_1[,'variable']=='4', 'F4. Learn more about the Housing\ncompany law', mdata_1[,'variable'])
mdata_1$variable = ifelse(mdata_1[,'variable']=='5', 'F5. Pass time', mdata_1[,'variable'])
mdata_1$variable = ifelse(mdata_1[,'variable']=='6', 'F6. Curiosity', mdata_1[,'variable'])
mdata_1$variable = ifelse(mdata_1[,'variable']=='7', 'F7. Discuss the topic with others', mdata_1[,'variable'])
mdata_1$survey = '1. Pre-Process'

mdata_2 = melt(mot_survey_2, id.vars='email', measure.vars=c('o1s2', 'o2s2', 'o3s2', 'o4s2', 'o5s2', 'o6s2', 'o7s2', 'o8s2'))
mdata_2$variable = ifelse(mdata_2[,'variable']=='o1s2', 'F1. Solve problems in\nhomeowners\' associations', mdata_2[,'variable'])
mdata_2$variable = ifelse(mdata_2[,'variable']=='2', 'F2. Improve the Housing\ncompany law', mdata_2[,'variable'])
mdata_2$variable = ifelse(mdata_2[,'variable']=='3', 'F3. Unhappy with my own\nhomeowner\'s association', mdata_2[,'variable'])
mdata_2$variable = ifelse(mdata_2[,'variable']=='4', 'F4. Learn more about the Housing\ncompany law', mdata_2[,'variable'])
mdata_2$variable = ifelse(mdata_2[,'variable']=='5', 'F5. Pass time', mdata_2[,'variable'])
mdata_2$variable = ifelse(mdata_2[,'variable']=='6', 'F6. Curiosity', mdata_2[,'variable'])
mdata_2$variable = ifelse(mdata_2[,'variable']=='7', 'F7. Discuss the topic with others', mdata_2[,'variable'])
mdata_2$variable = ifelse(mdata_2[,'variable']=='8', 'F8. Interested in others\' point\nof views', mdata_2[,'variable'])
mdata_2$survey = '2. Post-Process'

mdata_3 = melt(mot_survey_3, id.vars='email', measure.vars=c('o1s3', 'o2s3', 'o3s3', 'o4s3', 'o5s3', 'o6s3', 'o7s3', 'o8s3'))
mdata_3$variable = ifelse(mdata_3[,'variable']=='o1s3', 'F1. Solve problems in\nhomeowners\' associations', mdata_3[,'variable'])
mdata_3$variable = ifelse(mdata_3[,'variable']=='2', 'F2. Improve the Housing\ncompany law', mdata_3[,'variable'])
mdata_3$variable = ifelse(mdata_3[,'variable']=='3', 'F3. Unhappy with my own\nhomeowner\'s association', mdata_3[,'variable'])
mdata_3$variable = ifelse(mdata_3[,'variable']=='4', 'F4. Learn more about the Housing\ncompany law', mdata_3[,'variable'])
mdata_3$variable = ifelse(mdata_3[,'variable']=='5', 'F5. Pass time', mdata_3[,'variable'])
mdata_3$variable = ifelse(mdata_3[,'variable']=='6', 'F7. Discuss the topic with others', mdata_3[,'variable'])
mdata_3$variable = ifelse(mdata_3[,'variable']=='7', 'F8. Interested in others\' point\nof views', mdata_3[,'variable'])
mdata_3$variable = ifelse(mdata_3[,'variable']=='8', 'F9. Civic duty', mdata_3[,'variable'])
mdata_3$survey = '3. After Post-Process'

mdata_all = rbind(mdata_1,mdata_2,mdata_3)
mdata_all$value = as.numeric(as.character(mdata_all$value))
ggplot(mdata_all, aes(x=variable, y=value, fill=variable)) + 
  geom_boxplot() + 
  facet_grid(. ~ survey) +
  scale_y_continuous(breaks=c(0:8)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=16, color='black'), 
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=17), axis.title.y = element_text(size=15),
        strip.text.x = element_text(size=18, face="bold")) + 
  labs(y="Scores") +
  guides(fill=FALSE)

# Analysis significance in changes on the motivation factors
mot_survey_1 = mot_survey_1 %>% select(-email)
mot_survey_1$survey = 'survey_1'
mot_survey_2 = mot_survey_2 %>% select(-email)
mot_survey_2$survey = 'survey_2'
mot_survey_3 = mot_survey_3 %>% select(-email)
mot_survey_3$survey = 'survey_3'
colnames(mot_survey_1) = c('F1','F2','F3','F4', 'F5', 'F6', 'group')
colnames(mot_survey_2) = c('F1','F2','F3','F4', 'F5', 'F6', 'group')
colnames(mot_survey_3) = c('F1','F2','F3','F4', 'F5', 'F6', 'group')
mot_survey_1$F1 = as.numeric(mot_survey_1$F1)
mot_survey_1$F2 = as.numeric(mot_survey_1$F2)
mot_survey_1$F3 = as.numeric(mot_survey_1$F3)
mot_survey_1$F4 = as.numeric(mot_survey_1$F4)
mot_survey_1$F5 = as.numeric(mot_survey_1$F5)
mot_survey_1$F6 = as.numeric(mot_survey_1$F6)
mot_survey_2$F1 = as.numeric(mot_survey_2$F1)
mot_survey_2$F2 = as.numeric(mot_survey_2$F2)
mot_survey_2$F3 = as.numeric(mot_survey_2$F3)
mot_survey_2$F4 = as.numeric(mot_survey_2$F4)
mot_survey_2$F5 = as.numeric(mot_survey_2$F5)
mot_survey_2$F6 = as.numeric(mot_survey_2$F6)
mot_survey_3$F1 = as.numeric(mot_survey_3$F1)
mot_survey_3$F2 = as.numeric(mot_survey_3$F2)
mot_survey_3$F3 = as.numeric(mot_survey_3$F3)
mot_survey_3$F4 = as.numeric(mot_survey_3$F4)
mot_survey_3$F5 = as.numeric(mot_survey_3$F5)
mot_survey_3$F6 = as.numeric(mot_survey_3$F6)
all_surveys = rbind(mot_survey_1,mot_survey_2,mot_survey_3)

# Check normality of data, none of the variable show to follow a normal distribution
aux = filter(all_surveys, group=='survey_3')
hist(aux$F6, 100, col="black")
qqnorm(aux$F6)

# Actually perform the analysis
groups = c('survey_1','survey_2', 'survey_3')
ret = get_report_groups(all_surveys, groups)

# Results of motivation factor F1
var = 'F1'
table = data.frame(N=ret[[var]]$ns,means=ret[[var]]$means,median=ret[[var]]$medians)
rownames(table) = groups
print(xtable(table, align=(rep('c',ncol(table)+1))), type="html", comment=F, include.rownames=T)
if (ret[[var]]$p_value<=0.05) {
  cat(paste('The are significant differences between groups, p-value= ',
            round(ret[[var]]$p_value,3),sep=''))
} else {
  cat(paste('The are not significant differences between groups, p-value= ',
             round(ret[[var]]$p_value,3),sep=''))
}

# Results of motivation factor F2
var = 'F2'
table = data.frame(N=ret[[var]]$ns,means=ret[[var]]$means,median=ret[[var]]$medians)
rownames(table) = groups
print(xtable(table, align=(rep('c',ncol(table)+1))), type="html", comment=F, include.rownames=T)
if (ret[[var]]$p_value<=0.05) {
  cat(paste('The are significant differences between groups, p-value= ',
            round(ret[[var]]$p_value,3),sep=''))
} else {
  cat(paste('The are not significant differences between groups, p-value= ',
            round(ret[[var]]$p_value,3),sep=''))
}

# Results of motivation factor F3
var = 'F3'
table = data.frame(N=ret[[var]]$ns,means=ret[[var]]$means,median=ret[[var]]$medians)
rownames(table) = groups
print(xtable(table, align=(rep('c',ncol(table)+1))), type="html", comment=F, include.rownames=T)
if (ret[[var]]$p_value<=0.05) {
  cat(paste('The are significant differences between groups, p-value= ',
            round(ret[[var]]$p_value,3),sep=''))
} else {
  cat(paste('The are not significant differences between groups, p-value= ',
            round(ret[[var]]$p_value,3),sep=''))
}

# Results of motivation factor F4
var = 'F4'
table = data.frame(N=ret[[var]]$ns,means=ret[[var]]$means,median=ret[[var]]$medians)
rownames(table) = groups
print(xtable(table, align=(rep('c',ncol(table)+1))), type="html", comment=F, include.rownames=T)
if (ret[[var]]$p_value<=0.05) {
  cat(paste('The are significant differences between groups, p-value= ',
            round(ret[[var]]$p_value,3),sep=''))
} else {
  cat(paste('The are not significant differences between groups, p-value= ',
            round(ret[[var]]$p_value,3),sep=''))
}

# Results of motivation factor F5
var = 'F5'
table = data.frame(N=ret[[var]]$ns,means=ret[[var]]$means,median=ret[[var]]$medians)
rownames(table) = groups
print(xtable(table, align=(rep('c',ncol(table)+1))), type="html", comment=F, include.rownames=T)
if (ret[[var]]$p_value<=0.05) {
  cat(paste('The are significant differences between groups, p-value= ',
            round(ret[[var]]$p_value,3),sep=''))
} else {
  cat(paste('The are not significant differences between groups, p-value= ',
            round(ret[[var]]$p_value,3),sep=''))
}

# Results of motivation factor F6
var = 'F6'
table = data.frame(N=ret[[var]]$ns,means=ret[[var]]$means,median=ret[[var]]$medians)
rownames(table) = groups
print(xtable(table, align=(rep('c',ncol(table)+1))), type="html", comment=F, include.rownames=T)
if (ret[[var]]$p_value<=0.05) {
  cat(paste('The are significant differences between groups, p-value= ',
            round(ret[[var]]$p_value,3),sep=''))
} else {
  cat(paste('The are not significant differences between groups, p-value= ',
            round(ret[[var]]$p_value,3),sep=''))
}

# Analysis of motivation factors in survey 2 and 3
mot_surveys = merge(mot_survey_2, mot_survey_3, by.x='email', by.y='email', all=F)
mot_survey_2 = mot_survey_2 %>% filter(email %in% mot_surveys$email) %>% select(-o6s2)
mot_survey_3 = mot_survey_3 %>% filter(email %in% mot_surveys$email) %>% select(-o8s3)

mdata_2 = melt(mot_survey_2, id.vars='email', measure.vars=c('o1s2', 'o2s2', 'o3s2', 'o4s2', 'o5s2', 'o7s2', 'o8s2'))
mdata_2$variable = ifelse(mdata_2[,'variable']=='o1s2', 'F1. Solve problems in\nhomeowners\' associations', mdata_2[,'variable'])
mdata_2$variable = ifelse(mdata_2[,'variable']=='2', 'F2. Improve the Limited Liability\nHousing Companies Act (LLHCA)', mdata_2[,'variable'])
mdata_2$variable = ifelse(mdata_2[,'variable']=='3', 'F3. Unhappy with my own\nhomeowner\'s association', mdata_2[,'variable'])
mdata_2$variable = ifelse(mdata_2[,'variable']=='4', 'F4. Learn more about the LLHCA', mdata_2[,'variable'])
mdata_2$variable = ifelse(mdata_2[,'variable']=='5', 'F5. Pass time', mdata_2[,'variable'])
mdata_2$variable = ifelse(mdata_2[,'variable']=='6', 'F6. Discuss the topic with others', mdata_2[,'variable'])
mdata_2$variable = ifelse(mdata_2[,'variable']=='7', 'F7. Interested in others\' point of views', mdata_2[,'variable'])
mdata_2$survey = '1. Post-Process'

mdata_3 = melt(mot_survey_3, id.vars='email', measure.vars=c('o1s3', 'o2s3', 'o3s3', 'o4s3', 'o5s3', 'o6s3', 'o7s3'))
mdata_3$variable = ifelse(mdata_3[,'variable']=='o1s3', 'F1. Solve problems in\nhomeowners\' associations', mdata_3[,'variable'])
mdata_3$variable = ifelse(mdata_3[,'variable']=='2', 'F2. Improve the Limited Liability\nHousing Companies Act (LLHCA)', mdata_3[,'variable'])
mdata_3$variable = ifelse(mdata_3[,'variable']=='3', 'F3. Unhappy with my own\nhomeowner\'s association', mdata_3[,'variable'])
mdata_3$variable = ifelse(mdata_3[,'variable']=='4', 'F4. Learn more about the LLHCA', mdata_3[,'variable'])
mdata_3$variable = ifelse(mdata_3[,'variable']=='5', 'F5. Pass time', mdata_3[,'variable'])
mdata_3$variable = ifelse(mdata_3[,'variable']=='6', 'F6. Discuss the topic with others', mdata_3[,'variable'])
mdata_3$variable = ifelse(mdata_3[,'variable']=='7', 'F7. Interested in others\' point of views', mdata_3[,'variable'])
mdata_3$survey = '2. After Post-Process'

mdata_all = rbind(mdata_2,mdata_3)
mdata_all$value = as.numeric(as.character(mdata_all$value))
ggplot(mdata_all, aes(x=variable, y=value, fill=variable)) + 
  geom_boxplot() + 
  facet_grid(. ~ survey) +
  scale_y_continuous(breaks=c(0:8)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=16, color='black'), 
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=17), axis.title.y = element_text(size=15),
        strip.text.x = element_text(size=18, face="bold")) + 
  labs(y="Scores") +
  guides(fill=FALSE)

t.test(as.numeric(mot_survey_2$o7s2),mot_survey_3$o6s3)


mean(mot_survey_3[,'o8s3'], na.rm=T)
mean(mot_survey_3[,'o7s3'], na.rm=T)

survey_1_2_ = merge(mot_survey_1,mot_survey_2,by.x='email',by.y='email')
mean(as.numeric(as.character(survey_1_2_[,'o6s1'])), na.rm=T)
median(as.numeric(as.character(survey_1_2_[,'o6s1'])), na.rm=T)
mean(as.numeric(as.character(survey_1_2_[,'o6s2'])), na.rm=T)
median(as.numeric(as.character(survey_1_2_[,'o6s2'])), na.rm=T)
t.test(as.numeric(as.character(survey_1_2_[,'o6s1'])),as.numeric(as.character(survey_1_2_[,'o6s2'])))

survey_2_3_ = merge(mot_survey_2,mot_survey_3,by.x='email',by.y='email')
t.test(as.numeric(as.character(survey_2_3_[,'o8s2'])),as.numeric(as.character(survey_2_3_[,'o7s3'])))
mean(as.numeric(as.character(survey_2_3_[,'o8s2'])), na.rm=T)
mean(as.numeric(as.character(survey_2_3_[,'o7s3'])), na.rm=T)

