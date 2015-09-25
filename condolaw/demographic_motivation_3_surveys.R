#######
# Considering the 3 surveys an analysis was conducted to study 
# the relationship between demographic and changes in 
# the motivational factors
#######

# Load libraries
library(ggplot2)
library(dplyr)
library(stringr)
source('utils.R')

# Reading data
mot_survey_1 = read.csv("./data/survey1-condlaw-no-duplicates.csv",header=T, sep=",",stringsAsFactors=F)
mot_survey_2 = read.csv("./data/survey2-condlaw-no-duplicates.csv",header=T, sep=",",stringsAsFactors=F)
mot_survey_3 = read.csv("./data/survey3-condlaw-no-duplicates2.csv",header=T, sep=",",stringsAsFactors=F)
demographic = read.csv("./data/demographic-condlaw-no-duplicates.csv",header=T,sep=",",stringsAsFactors=F)
columns = read.table("./data/demographic-condlaw-columns.txt")

# Data preparation
colnames(demographic) = columns[,2]
colnames(mot_survey_1) = c('email', 'datetime', 'o1s1', 'o2s1', 'o3s1', 'o4s1', 'o5s1', 'o6s1', 'o7s1')
colnames(mot_survey_2) = c('email', 'datetime', 'o1s2', 'o2s2', 'o3s2', 'o4s2', 'o5s2', 'o6s2', 'o7s2', 'o8s2')
colnames(mot_survey_3) = c('email', 'datetime', 'o1s3', 'o2s3', 'o3s3', 'o4s3', 'o5s3', 'o6s3', 'o7s3', 'o8s3')
mot_survey_1 = mot_survey_1 %>% filter(email!='') %>% filter(!is.na(o1s1)) %>% filter(o1s1!='') %>% select(-datetime)
mot_survey_2 = mot_survey_2 %>% filter(email!='') %>% filter(!is.na(o1s2)) %>% filter(o1s2!='') %>% select(-datetime)
mot_survey_3 = mot_survey_3 %>% filter(email!='') %>% filter(!is.na(o1s3)) %>% filter(o1s3!='') %>% select(-datetime)
all_surveys = merge(mot_survey_1, mot_survey_2, by.x='email', by.y='email', all=F)
all_surveys = merge(all_surveys, mot_survey_3, by.x='email', by.y='email', all=F)
all_surveys = all_surveys %>% select(-o6s1, -o6s2, -o8s2, -o7s3, -o8s3)
colnames(all_surveys) = c('email', 'o1s1', 'o2s1', 'o3s1', 'o4s1', 'o5s1', 'o6s1',
                          'o1s2', 'o2s2', 'o3s2', 'o4s2', 'o5s2', 'o6s2',
                          'o1s3', 'o2s3', 'o3s3', 'o4s3', 'o5s3', 'o6s3')
dataset = merge(all_surveys, demographic, by.x='email', by.y='email')

# 1) Gender and motivational factors
check_rel_three_surveys(dataset %>% select(o1s1:o6s3,gender) %>% filter(!is.na(gender)),'gender',6)

# 2) Age and motivational factors ********** Factor 3
check_rel_three_surveys(dataset %>% select(o1s1:o6s3,age) %>% filter(!is.na(age)),'age',6)
# Find out in which age is seeing the difference
age_35 = dataset %>% filter(age==5)
median(as.numeric(age_35[,'o3s1']))
median(as.numeric(age_35[,'o3s2']))
median(as.numeric(age_35[,'o3s3']))
age_55 = dataset %>% filter(age==6)
median(as.numeric(age_55[,'o3s1']))
median(as.numeric(age_55[,'o3s2']))
median(as.numeric(age_55[,'o3s3']))
age_65 = dataset %>% filter(age==7)
median(as.numeric(age_65[,'o3s1']))
median(as.numeric(age_65[,'o3s2']))
median(as.numeric(age_65[,'o3s3']))
aux_survey_1 = mot_survey_1
aux_survey_1$survey = 'survey_1'
aux_survey_1 = aux_survey_1 %>% filter(email %in% age_55$email) %>% select(o3s1, survey)
aux_survey_2 = mot_survey_2
aux_survey_2$survey = 'survey_2'
aux_survey_2 = aux_survey_2 %>% filter(email %in% age_55$email) %>% select(o3s2, survey)
aux_survey_3 = mot_survey_3
aux_survey_3$survey = 'survey_3'
aux_survey_3 = aux_survey_3 %>% filter(email %in% age_55$email) %>% select(o3s3, survey)
colnames(aux_survey_1) = c('F3','group')
colnames(aux_survey_2) = c('F3','group')
colnames(aux_survey_3) = c('F3', 'group')
aux_survey_1$F3 = as.numeric(aux_survey_1$F3)
aux_survey_2$F3 = as.numeric(aux_survey_2$F3)
aux_survey_3$F3 = as.numeric(aux_survey_3$F3)
aux_surveys = rbind(aux_survey_1,aux_survey_2,aux_survey_3)
groups = c('survey_1','survey_2', 'survey_3')
vars = c('F3')
ret = get_dynamic_report_diff_groups(aux_surveys, groups, vars)
print(paste('Age 55-64, p-value: ',ret[['F3']]$p_value,sep=''))

# 3) Basic education and motivational factors
check_rel_three_surveys(dataset %>% select(o1s1:o6s3,basic_edu) %>% filter(!is.na(basic_edu)),'basic_edu',6)

# 4) Ocuppational education and motivational factors
check_rel_three_surveys(dataset %>% select(o1s1:o6s3,occupational_edu) %>% filter(!is.na(occupational_edu)),'occupational_edu',6)

# 5) Current situation and motivational factors
check_rel_three_surveys(dataset %>% select(o1s1:o6s3,current_situation) %>% filter(!is.na(current_situation)),'current_situation',6)

# 6) Occupational group and motivational factors
check_rel_three_surveys(dataset %>% select(o1s1:o6s3,occupational_group) %>% filter(!is.na(occupational_group)),'occupational_group',6)

# 9) Role in board and motivational factors **** Factor 1, 2, and 5 ****
## Checking Role chair factor 1 (significant)
check_rel_three_surveys(dataset %>% select(o1s1:o6s3,role_in_board) %>% filter(!is.na(role_in_board)),'role_in_board',6)
role_chair = dataset %>% filter(role_in_board==1)
median(as.numeric(role_chair[,'o1s1']))
median(as.numeric(role_chair[,'o1s2']))
median(as.numeric(role_chair[,'o1s3']))
aux_survey_1 = mot_survey_1
aux_survey_1$survey = 'survey_1'
aux_survey_1 = aux_survey_1 %>% filter(email %in% role_chair$email) %>% select(o1s1, survey)
aux_survey_2 = mot_survey_2
aux_survey_2$survey = 'survey_2'
aux_survey_2 = aux_survey_2 %>% filter(email %in% role_chair$email) %>% select(o1s2, survey)
aux_survey_3 = mot_survey_3
aux_survey_3$survey = 'survey_3'
aux_survey_3 = aux_survey_3 %>% filter(email %in% role_chair$email) %>% select(o1s3, survey)
colnames(aux_survey_1) = c('F1','group')
colnames(aux_survey_2) = c('F1','group')
colnames(aux_survey_3) = c('F1', 'group')
aux_survey_1$F1 = as.numeric(aux_survey_1$F1)
aux_survey_2$F1 = as.numeric(aux_survey_2$F1)
aux_survey_3$F1 = as.numeric(aux_survey_3$F1)
aux_surveys = rbind(aux_survey_1,aux_survey_2,aux_survey_3)
groups = c('survey_1','survey_2', 'survey_3')
vars = c('F1')
ret = get_dynamic_report_diff_groups(aux_surveys, groups, vars)
print(paste('Role chair (factor 1), p-value: ',ret[['F1']]$p_value,sep=''))
## Checking role chair factor 2
median(as.numeric(role_chair[,'o2s1']))
median(as.numeric(role_chair[,'o2s2']))
median(as.numeric(role_chair[,'o2s3']))
aux_survey_1 = mot_survey_1
aux_survey_1$survey = 'survey_1'
aux_survey_1 = aux_survey_1 %>% filter(email %in% role_chair$email) %>% select(o2s1, survey)
aux_survey_2 = mot_survey_2
aux_survey_2$survey = 'survey_2'
aux_survey_2 = aux_survey_2 %>% filter(email %in% role_chair$email) %>% select(o2s2, survey)
aux_survey_3 = mot_survey_3
aux_survey_3$survey = 'survey_3'
aux_survey_3 = aux_survey_3 %>% filter(email %in% role_chair$email) %>% select(o2s3, survey)
colnames(aux_survey_1) = c('F2','group')
colnames(aux_survey_2) = c('F2','group')
colnames(aux_survey_3) = c('F2', 'group')
aux_survey_1$F2 = as.numeric(aux_survey_1$F2)
aux_survey_2$F2 = as.numeric(aux_survey_2$F2)
aux_survey_3$F2 = as.numeric(aux_survey_3$F2)
aux_surveys = rbind(aux_survey_1,aux_survey_2,aux_survey_3)
groups = c('survey_1','survey_2', 'survey_3')
vars = c('F2')
ret = get_dynamic_report_diff_groups(aux_surveys, groups, vars)
print(paste('Role chair (factor 2), p-value: ',ret[['F2']]$p_value,sep=''))
## Checking role chair factor 5
median(as.numeric(role_chair[,'o5s1']))
median(as.numeric(role_chair[,'o5s2']))
median(as.numeric(role_chair[,'o5s3']))
## Checking role member factor 1
role_mem = dataset %>% filter(role_in_board==2)
median(as.numeric(role_mem[,'o1s1']))
median(as.numeric(role_mem[,'o1s2']))
median(as.numeric(role_mem[,'o1s3']))
aux_survey_1 = mot_survey_1
aux_survey_1$survey = 'survey_1'
aux_survey_1 = aux_survey_1 %>% filter(email %in% role_mem$email) %>% select(o1s1, survey)
aux_survey_2 = mot_survey_2
aux_survey_2$survey = 'survey_2'
aux_survey_2 = aux_survey_2 %>% filter(email %in% role_mem$email) %>% select(o1s2, survey)
aux_survey_3 = mot_survey_3
aux_survey_3$survey = 'survey_3'
aux_survey_3 = aux_survey_3 %>% filter(email %in% role_mem$email) %>% select(o1s3, survey)
colnames(aux_survey_1) = c('F1','group')
colnames(aux_survey_2) = c('F1','group')
colnames(aux_survey_3) = c('F1', 'group')
aux_survey_1$F1 = as.numeric(aux_survey_1$F1)
aux_survey_2$F1 = as.numeric(aux_survey_2$F1)
aux_survey_3$F1 = as.numeric(aux_survey_3$F1)
aux_surveys = rbind(aux_survey_1,aux_survey_2,aux_survey_3)
groups = c('survey_1','survey_2', 'survey_3')
vars = c('F1')
ret = get_dynamic_report_diff_groups(aux_surveys, groups, vars)
print(paste('Role member (factor 1), p-value: ',ret[['F1']]$p_value,sep=''))
## Checking role member factor 2
median(as.numeric(role_mem[,'o2s1']))
median(as.numeric(role_mem[,'o2s2']))
median(as.numeric(role_mem[,'o2s3']))
aux_survey_1 = mot_survey_1
aux_survey_1$survey = 'survey_1'
aux_survey_1 = aux_survey_1 %>% filter(email %in% role_mem$email) %>% select(o2s1, survey)
aux_survey_2 = mot_survey_2
aux_survey_2$survey = 'survey_2'
aux_survey_2 = aux_survey_2 %>% filter(email %in% role_mem$email) %>% select(o2s2, survey)
aux_survey_3 = mot_survey_3
aux_survey_3$survey = 'survey_3'
aux_survey_3 = aux_survey_3 %>% filter(email %in% role_mem$email) %>% select(o2s3, survey)
colnames(aux_survey_1) = c('F2','group')
colnames(aux_survey_2) = c('F2','group')
colnames(aux_survey_3) = c('F2','group')
aux_survey_1$F2 = as.numeric(aux_survey_1$F2)
aux_survey_2$F2 = as.numeric(aux_survey_2$F2)
aux_survey_3$F2 = as.numeric(aux_survey_3$F2)
aux_surveys = rbind(aux_survey_1,aux_survey_2,aux_survey_3)
groups = c('survey_1','survey_2', 'survey_3')
vars = c('F2')
ret = get_dynamic_report_diff_groups(aux_surveys, groups, vars)
print(paste('Role member (factor 2), p-value: ',ret[['F2']]$p_value,sep=''))
## Checking role member factor 5
median(as.numeric(role_mem[,'o5s1']))
median(as.numeric(role_mem[,'o5s2']))
median(as.numeric(role_mem[,'o5s3']))

# 8) Role and motivational factors
## Condominium owner
role = dataset %>% select(o1s1:o6s3, role_owner)
role[is.na(role$role_owner),'role_owner'] = '0'
check_rel_three_surveys(role,'role_owner',6)
## Tenant
role = dataset %>% select(o1s1:o6s3, role_tenant)
role[is.na(role$role_tenant),'role_tenant'] = '0'
check_rel_three_surveys(role,'role_tenant',6)
median(as.numeric(dataset[is.na(dataset$role_tenant),'o5s1']))
median(as.numeric(dataset[is.na(dataset$role_tenant),'o5s2']))
median(as.numeric(dataset[is.na(dataset$role_tenant),'o5s3']))
median(as.numeric(dataset[!is.na(dataset$role_tenant),'o5s1']))
median(as.numeric(dataset[!is.na(dataset$role_tenant),'o5s2']))
median(as.numeric(dataset[!is.na(dataset$role_tenant),'o5s3']))
## Board member (there isn't board member among those that responded to the three surveys)
role = dataset %>% select(o1s1:o6s3, role_board_member)
role[is.na(role$role_board_member),'role_board_member'] = '0'
#check_rel_three_surveys(role,'role_board_member',6)
## Condominium manager ************* Factor 4
role = dataset %>% select(o1s1:o6s3, role_condo_manager)
role[is.na(role$role_condo_manager),'role_condo_manager'] = '0'
check_rel_three_surveys(role,'role_condo_manager',6)
median(as.numeric(dataset[is.na(dataset$role_condo_manager),'o4s1']))
median(as.numeric(dataset[is.na(dataset$role_condo_manager),'o4s2']))
median(as.numeric(dataset[is.na(dataset$role_condo_manager),'o4s3']))
median(as.numeric(dataset[!is.na(dataset$role_condo_manager),'o4s1']))
median(as.numeric(dataset[!is.na(dataset$role_condo_manager),'o4s2']))
median(as.numeric(dataset[!is.na(dataset$role_condo_manager),'o4s3']))
condo_managers = dataset[!is.na(dataset$role_condo_manager),]
aux_survey_1 = mot_survey_1
aux_survey_1$survey = 'survey_1'
aux_survey_1 = aux_survey_1 %>% filter(email %in% condo_managers$email) %>% select(o4s1, survey)
aux_survey_2 = mot_survey_2
aux_survey_2$survey = 'survey_2'
aux_survey_2 = aux_survey_2 %>% filter(email %in% condo_managers$email) %>% select(o4s2, survey)
aux_survey_3 = mot_survey_3
aux_survey_3$survey = 'survey_3'
aux_survey_3 = aux_survey_3 %>% filter(email %in% condo_managers$email) %>% select(o4s3, survey)
colnames(aux_survey_1) = c('F4','group')
colnames(aux_survey_2) = c('F4','group')
colnames(aux_survey_3) = c('F4', 'group')
aux_survey_1$F4 = as.numeric(aux_survey_1$F4)
aux_survey_2$F4 = as.numeric(aux_survey_2$F4)
aux_survey_3$F4 = as.numeric(aux_survey_3$F4)
aux_surveys = rbind(aux_survey_1,aux_survey_2,aux_survey_3)
groups = c('survey_1','survey_2', 'survey_3')
vars = c('F4')
ret = get_dynamic_report_diff_groups(aux_surveys, groups, vars)
## Real Estate Agent (there is only 1 real state agent among those that responded to the three surveys)
role = dataset %>% select(o1s1:o6s3, role_agent)
role[is.na(role$role_agent),'role_agent'] = '0'
#check_rel_three_surveys(role,'role_agent',6)
## Other
role = dataset %>% select(o1s1:o6s3, role_other)
role[is.na(role$role_other),'role_other'] = '0'
check_rel_three_surveys(role,'role_other',6)

# 7) Participation in civic life and motivational factors
## Have written an op­ed to a newspaper or other publication
activity = dataset %>% select(o1s1:o6s3, activity_oped)
activity[is.na(activity$activity_oped),'activity_oped'] = '0'
check_rel_three_surveys(activity,'activity_oped',6)
## Have written about my views and opinions in a blog
activity = dataset %>% select(o1s1:o6s3, activity_blog)
activity[is.na(activity$activity_blog),'activity_blog'] = '0'
check_rel_three_surveys(activity,'activity_blog',6)
## Have have expressed my views on online forums (other than a blog), such as discussion 
## forums online or newspapersâ€™ commenting section
activity = dataset %>% select(email, o1s1:o6s3, activity_view_forum)
activity[is.na(activity$activity_view_forum),'activity_view_forum'] = '0'
check_rel_three_surveys(activity,'activity_view_forum',6)
## Have written to an MP
activity = dataset %>% select(o1s1:o6s3, activity_mp)
activity[is.na(activity$activity_mp),'activity_mp'] = '0'
check_rel_three_surveys(activity,'activity_mp', 6)
## Have written to a municipal councilor or a town councilor  ************ Factor 1
activity = dataset %>% select(o1s1:o6s3, activity_councilor)
activity[is.na(activity$activity_councilor),'activity_councilor'] = '0'
check_rel_three_surveys(activity,'activity_councilor',6)
median(as.numeric(dataset[is.na(dataset$activity_councilor),'o1s1']))
median(as.numeric(dataset[is.na(dataset$activity_councilor),'o1s2']))
median(as.numeric(dataset[is.na(dataset$activity_councilor),'o1s3']))
median(as.numeric(dataset[!is.na(dataset$activity_councilor),'o1s1']))
median(as.numeric(dataset[!is.na(dataset$activity_councilor),'o1s2']))
median(as.numeric(dataset[!is.na(dataset$activity_councilor),'o1s3']))
writters = dataset[!is.na(dataset$activity_councilor),]
aux_survey_1 = mot_survey_1
aux_survey_1$survey = 'survey_1'
aux_survey_1 = aux_survey_1 %>% filter(email %in% writters$email) %>% select(o1s1, survey)
aux_survey_2 = mot_survey_2
aux_survey_2$survey = 'survey_2'
aux_survey_2 = aux_survey_2 %>% filter(email %in% writters$email) %>% select(o1s2, survey)
aux_survey_3 = mot_survey_3
aux_survey_3$survey = 'survey_3'
aux_survey_3 = aux_survey_3 %>% filter(email %in% writters$email) %>% select(o1s3, survey)
colnames(aux_survey_1) = c('F1','group')
colnames(aux_survey_2) = c('F1','group')
colnames(aux_survey_3) = c('F1', 'group')
aux_survey_1$F1 = as.numeric(aux_survey_1$F1)
aux_survey_2$F1 = as.numeric(aux_survey_2$F1)
aux_survey_3$F1 = as.numeric(aux_survey_3$F1)
aux_surveys = rbind(aux_survey_1,aux_survey_2,aux_survey_3)
groups = c('survey_1','survey_2', 'survey_3')
vars = c('F1')
ret = get_dynamic_report_diff_groups(aux_surveys, groups, vars)

## Have participated in municipality council or town council meetings
activity = dataset %>% select(o1s1:o6s3, activity_municipality)
activity[is.na(activity$activity_municipality),'activity_municipality'] = '0'
check_rel_three_surveys(activity,'activity_municipality',6)
## Have participated in committee meetings
activity = dataset %>% select(o1s1:o6s3, activity_meeting)
activity[is.na(activity$activity_meeting),'activity_meeting'] = '0'
check_rel_three_surveys(activity,'activity_meeting',6)
## Have signed a petition either online or on paper
activity = dataset %>% select(o1s1:o6s3, activity_signed_petition)
activity[is.na(activity$activity_signed_petition),'activity_signed_petition'] = '0'
check_rel_three_surveys(activity,'activity_signed_petition',6)
## Have initiated a petition
activity = dataset %>% select(o1s1:o6s3, activity_initiated_petition)
activity[is.na(activity$activity_initiated_petition),'activity_initiated_petition'] = '0'
check_rel_three_surveys(activity,'activity_initiated_petition',6)
## Have participated in peaceful demonstrations
activity = dataset %>% select(o1s1:o6s3, activity_demons)
activity[is.na(activity$activity_demons),'activity_demons'] = '0'
check_rel_three_surveys(activity,'activity_demons',6)
## Have volunteered in a non­profit organization or association
activity = dataset %>% select(o1s1:o6s3, activity_volunteer)
activity[is.na(activity$activity_volunteer),'activity_volunteer'] = '0'
check_rel_three_surveys(activity,'activity_volunteer',6)
## Have run as a candidate in municipal elections
activity = dataset %>% select(o1s1:o6s3, activity_municipal_candidate)
activity[is.na(activity$activity_municipal_candidate),'activity_municipal_candidate'] = '0'
check_rel_three_surveys(activity,'activity_municipal_candidate',6)
## Have run as a candidate in parliamentary elections (there is none candidate to parliament among the responders of the three surveys)
activity = dataset %>% select(o1s1:o6s3, activity_parliament_candidate)
activity[is.na(activity$activity_parliament_candidate),'activity_parliament_candidate'] = '0'
#check_rel_three_surveys(activity,'activity_parliament_candidate',6)
## Have done campaign work for candidates in municipal, parliamentary or presidential elections
activity = dataset %>% select(o1s1:o6s3, activity_campaign)
activity[is.na(activity$activity_campaign),'activity_campaign'] = '0'
check_rel_three_surveys(activity,'activity_campaign',6)
