#######
# An analysis of the relationship between demographic data and changes in the motivational factors
# First Survey real responders: 168 (no duplicates, no unnamed)
# Second Survey real responders: 104 (no duplicates, no unnamed)
# Both Surveys: 104
#######

# Load libraries
library(ggplot2)
library(dplyr)
library(stringr)
library(reshape)
source('utils.R')

# Reading Data
demographic = read.csv("./data/demographic-condlaw-no-duplicates.csv",header=T,sep=",",stringsAsFactors=F)
columns = read.table("./data/demographic-condlaw-columns.txt")
motivation_pre = read.csv("./data/survey1-condlaw-no-duplicates.csv",header=T, sep=",",stringsAsFactors=F)
motivation_pos = read.csv("./data/survey2-condlaw-no-duplicates.csv",header=T, sep=",",stringsAsFactors=F)

# Pre-processing
colnames(demographic) = columns[,2]
colnames(motivation_pre) = c('email', 'datetime', 'o1s1', 'o2s1', 'o3s1', 'o4s1', 'o5s1', 'o6s1', 'o7s1')
colnames(motivation_pos) = c('email', 'datetime', 'o1s2', 'o2s2', 'o3s2', 'o4s2', 'o5s2', 'o6s2', 'o7s2', 'o8s2')
motivation_pre = motivation_pre %>% filter(email!='') %>% filter(!is.na(o1s1)) %>% select(-datetime)
motivation_pos = motivation_pos %>% filter(email!='') %>% filter(!is.na(o1s2)) %>% select(-datetime)
motivation_pre$email = str_trim(motivation_pre$email)  # Remove leading and trailing spaces
motivation_pos$email = str_trim(motivation_pos$email)  # Remove leading and trailing spaces 
motivation_both = merge(motivation_pre, motivation_pos, by.x='email', by.y='email', all=F)
dataset = merge(motivation_both, demographic, by.x='email', by.y='email')

# 1) Gender and motivational factors
check_rel(dataset %>% select(o1s1:o8s2,gender) %>% filter(!is.na(gender)),'gender')

# 2) Age and motivational factors
check_rel(dataset %>% select(o1s1:o8s2,age) %>% filter(!is.na(age)),'age')

# 3) Basic education and motivational factors
check_rel(dataset %>% select(o1s1:o8s2,basic_edu) %>% filter(!is.na(basic_edu)),'basic_edu')

# 4) Ocuppational education and motivational factors
check_rel(dataset %>% select(o1s1:o8s2,occupational_edu) %>% filter(!is.na(occupational_edu)),'occupational_edu')

# 5) Current situation and motivational factors
check_rel(dataset %>% select(o1s1:o8s2,current_situation) %>% filter(!is.na(current_situation)),'current_situation')

# 6) Occupational group and motivational factors
check_rel(dataset %>% select(o1s1:o8s2,occupational_group) %>% filter(!is.na(occupational_group)),'occupational_group')

# 7) Participation in civic life and motivational factors
## Have written an op-ed to a newspaper or other publication
activity = dataset %>% select(o1s1:o8s2, activity_oped)
activity[is.na(activity$activity_oped),'activity_oped'] = '0'
check_rel(activity,'activity_oped')
## Have written about my views and opinions in a blog
activity = dataset %>% select(o1s1:o8s2, activity_blog)
activity[is.na(activity$activity_blog),'activity_blog'] = '0'
check_rel(activity,'activity_blog')
## Have have expressed my views on online forums (other than a blog), such as discussion 
## forums online or newspapers’ commenting section **
activity = dataset %>% select(email, o1s1:o8s2, activity_view_forum)
activity[is.na(activity$activity_view_forum),'activity_view_forum'] = '0'
check_rel(activity,'activity_view_forum')
activity_express = filter(activity, activity_view_forum!=0)  # Filter out those that don't express their views
activity_no_express = filter(activity, activity_view_forum==0)  # Filter out those that do express their views
median(activity_express$o2s1, na.rm=T)
median(activity_express$o2s2, na.rm=T)
median(activity_no_express$o2s1, na.rm=T)
median(activity_no_express$o2s2, na.rm=T)
# Check the significance of the difference
p_value = t.test(activity_no_express[,'o2s1'],activity_no_express[,'o2s2'])$p.value
if (p_value < 0.05) {
  median_s1 = median(activity_no_express[,'o2s1'], na.rm=T)
  median_s2 = median(activity_no_express[,'o2s2'], na.rm=T)
  cat(paste('The difference is: ',median_s2-median_s1,' and is significant with p-value=',round(p_value,3),sep=''))
  mdata_pre = melt(activity_no_express, id.vars='email', measure.vars=c('o1s1', 'o2s1', 'o3s1', 'o4s1', 'o5s1', 'o6s1', 'o7s1'))
  mdata_pre$variable = ifelse(mdata_pre[,'variable']=='o1s1', 'F1. Solve problems in\nhomeowners\' associations', mdata_pre[,'variable'])
  mdata_pre$variable = ifelse(mdata_pre[,'variable']=='2', 'F2. Improve the Limited Liability\nHousing Companies Act (LLHCA)', mdata_pre[,'variable'])
  mdata_pre$variable = ifelse(mdata_pre[,'variable']=='3', 'F3. Unhappy with my own\nhomeowner\'s association', mdata_pre[,'variable'])
  mdata_pre$variable = ifelse(mdata_pre[,'variable']=='4', 'F4. Learn more about the LLHCA', mdata_pre[,'variable'])
  mdata_pre$variable = ifelse(mdata_pre[,'variable']=='5', 'F5. Pass time', mdata_pre[,'variable'])
  mdata_pre$variable = ifelse(mdata_pre[,'variable']=='6', 'F6. I\'m curious', mdata_pre[,'variable'])
  mdata_pre$variable = ifelse(mdata_pre[,'variable']=='7', 'F7. Discuss the topic with others', mdata_pre[,'variable'])
  mdata_pre$survey = '1. Pre-Process'
  mdata_pos = melt(activity_no_express, id.vars='email', measure.vars=c('o1s2', 'o2s2', 'o3s2', 'o4s2', 'o5s2', 'o6s2', 'o7s2'))
  mdata_pos$variable = ifelse(mdata_pos[,'variable']=='o1s2', 'F1. Solve problems in\nhomeowners\' associations', mdata_pos[,'variable'])
  mdata_pos$variable = ifelse(mdata_pos[,'variable']=='2', 'F2. Improve the Limited Liability\nHousing Companies Act (LLHCA)', mdata_pos[,'variable'])
  mdata_pos$variable = ifelse(mdata_pos[,'variable']=='3', 'F3. Unhappy with my own\nhomeowner\'s association', mdata_pos[,'variable'])
  mdata_pos$variable = ifelse(mdata_pos[,'variable']=='4', 'F4. Learn more about the LLHCA', mdata_pos[,'variable'])
  mdata_pos$variable = ifelse(mdata_pos[,'variable']=='5', 'F5. Pass time', mdata_pos[,'variable'])
  mdata_pos$variable = ifelse(mdata_pos[,'variable']=='6', 'F6. I\'m curious', mdata_pos[,'variable'])
  mdata_pos$variable = ifelse(mdata_pos[,'variable']=='7', 'F7. Discuss the topic with others', mdata_pos[,'variable'])
  mdata_pos$survey = '2. Post-Process'
  mdata_both = rbind(mdata_pre,mdata_pos)
  ggplot(mdata_both, aes(x=variable, y=value, fill=variable)) + 
    geom_boxplot() + 
    facet_grid(. ~ survey) +
    scale_y_continuous(breaks=c(0:8)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size=16, color='black'), 
          axis.title.x = element_blank(),
          axis.text.y = element_text(size=17), axis.title.y = element_text(size=15),
          strip.text.x = element_text(size=18, face="bold")) + 
    labs(y="Scores") +
    guides(fill=FALSE)
}else {
  cat(paste('The difference is: ',median_s2-median_s1,' but is not significant',sep=''))
}
## Have written to an MP
activity = dataset %>% select(o1s1:o8s2, activity_mp)
activity[is.na(activity$activity_mp),'activity_mp'] = '0'
check_rel(activity,'activity_mp')
## Have written to a municipal councilor or a town councilor
activity = dataset %>% select(o1s1:o8s2, activity_councilor)
activity[is.na(activity$activity_councilor),'activity_councilor'] = '0'
check_rel(activity,'activity_councilor')
## Have participated in municipality council or town council meetings
activity = dataset %>% select(o1s1:o8s2, activity_municipality)
activity[is.na(activity$activity_municipality),'activity_municipality'] = '0'
check_rel(activity,'activity_municipality')
## Have participated in committee meetings
activity = dataset %>% select(o1s1:o8s2, activity_meeting)
activity[is.na(activity$activity_meeting),'activity_meeting'] = '0'
check_rel(activity,'activity_meeting')
## Have signed a petition either online or on paper
activity = dataset %>% select(o1s1:o8s2, activity_signed_petition)
activity[is.na(activity$activity_signed_petition),'activity_signed_petition'] = '0'
check_rel(activity,'activity_signed_petition')
## Have initiated a petition
activity = dataset %>% select(o1s1:o8s2, activity_initiated_petition)
activity[is.na(activity$activity_initiated_petition),'activity_initiated_petition'] = '0'
check_rel(activity,'activity_initiated_petition')
## Have participated in peaceful demonstrations
activity = dataset %>% select(o1s1:o8s2, activity_demons)
activity[is.na(activity$activity_demons),'activity_demons'] = '0'
check_rel(activity,'activity_demons')
## Have volunteered in a non�profit organization or association
activity = dataset %>% select(o1s1:o8s2, activity_volunteer)
activity[is.na(activity$activity_volunteer),'activity_volunteer'] = '0'
check_rel(activity,'activity_volunteer')
## Have run as a candidate in municipal elections
activity = dataset %>% select(o1s1:o8s2, activity_municipal_candidate)
activity[is.na(activity$activity_municipal_candidate),'activity_municipal_candidate'] = '0'
check_rel(activity,'activity_municipal_candidate')
## Have run as a candidate in parliamentary elections
activity = dataset %>% select(o1s1:o8s2, activity_parliament_candidate)
activity[is.na(activity$activity_parliament_candidate),'activity_parliament_candidate'] = '0'
check_rel(activity,'activity_parliament_candidate')
## Have done campaign work for candidates in municipal, parliamentary or presidential elections
activity = dataset %>% select(o1s1:o8s2, activity_campaign)
activity[is.na(activity$activity_campaign),'activity_campaign'] = '0'
check_rel(activity,'activity_campaign')

# 8) Role and motivational factors
## Condominium owner
role = dataset %>% select(o1s1:o8s2, role_owner)
role[is.na(role$role_owner),'role_owner'] = '0'
check_rel(role,'role_owner')
## Tenant
role = dataset %>% select(o1s1:o8s2, role_tenant)
role[is.na(role$role_tenant),'role_tenant'] = '0'
check_rel(role,'role_tenant')
## Board member
role = dataset %>% select(o1s1:o8s2, role_board_member)
role[is.na(role$role_board_member),'role_board_member'] = '0'
check_rel(role,'role_board_member')
## Condominium manager
role = dataset %>% select(o1s1:o8s2, role_condo_manager)
role[is.na(role$role_condo_manager),'role_condo_manager'] = '0'
check_rel(role,'role_condo_manager')
## Real Estate Agent
role = dataset %>% select(o1s1:o8s2, role_agent)
role[is.na(role$role_agent),'role_agent'] = '0'
check_rel(role,'role_agent')
## Other
role = dataset %>% select(o1s1:o8s2, role_other)
role[is.na(role$role_other),'role_other'] = '0'
check_rel(role,'role_other')

# 9) Role in board and motivational factors
check_rel(dataset %>% select(o1s1:o8s2,role_in_board) %>% filter(!is.na(role_in_board)),'role_in_board')