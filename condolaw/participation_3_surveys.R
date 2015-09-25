#######
# An analysis of the level of participation in people that
# have replied: 
# i) group 1: only to the first survey; 
# ii) group 2: first, second, and third surveys;
# iii) group 3: only to the third survey;
# iv) group 4: only first and second survey;
# v) group 5: only first and third survey.
#######

# Load libraries
library(ggplot2)
library(dplyr)
source('utils.R')

# Reading data
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

# Draw distribution of idea, comments, and votes production
ideators = participants[participants$ideas>0&participants$ideas<=20,] # Get rid of contributors that didn't produce ideas
ggplot(ideators, aes(x=ideas)) + 
geom_histogram(binwidth=1, colour="black", fill="white") +
  #geom_vline(xintercept = mean_producers, linetype = 'dashed', color = 'red', size = 1) +
  scale_x_continuous(breaks=c(0:20)) +
  scale_y_continuous(breaks=c(0,5,10,15,20,25,30,35,40,45,50)) +
  labs(x="Ideas Count", y="Participants Count") +
  theme(axis.text=element_text(size=28), axis.title=element_text(size=28))

commenters = participants[participants$comments>0&participants$comments<=50,] # Get rid of contributors that didn't produce ideas
ggplot(commenters, aes(x=comments)) + 
  geom_histogram(binwidth=1, colour="black", fill="white") +
  #geom_vline(xintercept = mean_producers, linetype = 'dashed', color = 'red', size = 1) +
  #scale_x_continuous(breaks=c(0:25)) +
  #scale_y_continuous(breaks=c(0,5,10,15,20,25,30,35,40,45,50)) +
  labs(x="Comments Count", y="Participants Count") +
  theme(axis.text=element_text(size=28), axis.title=element_text(size=28))

voters = participants[participants$votes>0&participants$votes<=100,] # Get rid of contributors that didn't produce ideas
ggplot(voters, aes(x=votes)) + 
  geom_histogram(binwidth=1,colour="black", fill="white") +
  #geom_vline(xintercept = mean_producers, linetype = 'dashed', color = 'red', size = 1) +
  scale_x_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100)) +
  #scale_y_continuous(breaks=c(0:10)) +
  labs(x="Votes Count", y="Participants Count") +
  theme(axis.text=element_text(size=28), axis.title=element_text(size=28))

# Cluster survey responders
group_1 = mot_survey_1 %>% filter(!email %in% mot_survey_2$email & !email %in% mot_survey_3$email)
group_2 = merge(mot_survey_1, mot_survey_2, by.x='email', by.y='email', all=F)
group_2 = merge(group_2, mot_survey_3, by.x='email', by.y='email', all=F)
group_3 = mot_survey_3 %>% filter(!email %in% mot_survey_2$email & !email %in% mot_survey_1$email)
group_4 = merge(mot_survey_1, mot_survey_2, by.x='email', by.y='email', all=F)
group_5 = merge(mot_survey_1, mot_survey_3, by.x='email', by.y='email', all=F)

# Cluster survey responders by their level of participation
part_group_1 = participants %>% filter(email %in% group_1$email)
part_group_2 = participants %>% filter(email %in% group_2$email)
part_group_3 = participants %>% filter(email %in% group_3$email)
part_group_4 = participants %>% filter(email %in% group_4$email)
part_group_5 = participants %>% filter(email %in% group_5$email)

# Cluster participants of group 1
num_group_1 = nrow(part_group_1)
observers_g1 = part_group_1 %>% filter(ideas==0&votes==0&comments==0) # nothing, only observe what happened
voters_g1 = part_group_1 %>% filter(ideas==0&votes!=0&comments==0)    # only vote
mid_cont_g1 = part_group_1 %>% filter((ideas==0&votes!=0&comments!=0)|
                                      (ideas!=0&votes==0&comments!=0)|
                                      (ideas!=0&votes!=0&comments==0)|
                                      (ideas!=0&votes==0&comments==0)|
                                      (ideas==0&votes==0&comments!=0))   
full_cont_g1 = part_group_1 %>% filter(ideas!=0&votes!=0&comments!=0) # suggest ideas, comment and vote

# Cluster participants of group 2
num_group_2 = nrow(part_group_2)
observers_g2 = part_group_2 %>% filter(ideas==0&votes==0&comments==0) # nothing, only observe what happened
voters_g2 = part_group_2 %>% filter(ideas==0&votes!=0&comments==0)    # only vote
mid_cont_g2 = part_group_2 %>% filter((ideas==0&votes!=0&comments!=0)|
                                      (ideas!=0&votes==0&comments!=0)|
                                      (ideas!=0&votes!=0&comments==0)|
                                      (ideas!=0&votes==0&comments==0)|
                                      (ideas==0&votes==0&comments!=0))   
full_cont_g2 = part_group_2 %>% filter(ideas!=0&votes!=0&comments!=0) # suggest ideas, comment and vote

# Cluster participants of group 3
num_group_3 = nrow(part_group_3)
observers_g3 = part_group_3 %>% filter(ideas==0&votes==0&comments==0) # nothing, only observe what happened
voters_g3 = part_group_3 %>% filter(ideas==0&votes!=0&comments==0)    # only vote
mid_cont_g3 = part_group_3 %>% filter((ideas==0&votes!=0&comments!=0)|
                                      (ideas!=0&votes==0&comments!=0)|
                                      (ideas!=0&votes!=0&comments==0)|
                                      (ideas!=0&votes==0&comments==0)|
                                      (ideas==0&votes==0&comments!=0))   
full_cont_g3 = part_group_3 %>% filter(ideas!=0&votes!=0&comments!=0) # suggest ideas, comment and vote

# Cluster participants of group 4
num_group_4 = nrow(part_group_4)
observers_g4 = part_group_4 %>% filter(ideas==0&votes==0&comments==0) # nothing, only observe what happened
voters_g4 = part_group_4 %>% filter(ideas==0&votes!=0&comments==0)    # only vote
mid_cont_g4 = part_group_4 %>% filter((ideas==0&votes!=0&comments!=0)|
                                      (ideas!=0&votes==0&comments!=0)|
                                      (ideas!=0&votes!=0&comments==0)|
                                      (ideas!=0&votes==0&comments==0)|
                                      (ideas==0&votes==0&comments!=0))   
full_cont_g4 = part_group_4 %>% filter(ideas!=0&votes!=0&comments!=0) # suggest ideas, comment and vote

# Cluster participants of group 5
num_group_5 = nrow(part_group_5)
observers_g5 = part_group_5 %>% filter(ideas==0&votes==0&comments==0) # nothing, only observe what happened
voters_g5 = part_group_5 %>% filter(ideas==0&votes!=0&comments==0)    # only vote
mid_cont_g5 = part_group_5 %>% filter((ideas==0&votes!=0&comments!=0)|
                                      (ideas!=0&votes==0&comments!=0)|
                                      (ideas!=0&votes!=0&comments==0)|
                                      (ideas!=0&votes==0&comments==0)|
                                      (ideas==0&votes==0&comments!=0))   
full_cont_g5 = part_group_5 %>% filter(ideas!=0&votes!=0&comments!=0) # suggest ideas, comment and vote

# Compare clusters
row_obs = c(paste(nrow(observers_g1),' (',round((nrow(observers_g1)*100)/num_group_1,0),'%)',sep=''),
            paste(nrow(observers_g2),' (',round((nrow(observers_g2)*100)/num_group_2,0),'%)',sep=''),
            paste(nrow(observers_g3),' (',round((nrow(observers_g3)*100)/num_group_3,0),'%)',sep=''),
            paste(nrow(observers_g4),' (',round((nrow(observers_g4)*100)/num_group_4,0),'%)',sep=''),
            paste(nrow(observers_g5),' (',round((nrow(observers_g5)*100)/num_group_5,0),'%)',sep=''))
row_vot = c(paste(nrow(voters_g1),' (',round((nrow(voters_g1)*100)/num_group_1,0),'%)',sep=''),
            paste(nrow(voters_g2),' (',round((nrow(voters_g2)*100)/num_group_2,0),'%)',sep=''),
            paste(nrow(voters_g3),' (',round((nrow(voters_g3)*100)/num_group_3,0),'%)',sep=''),
            paste(nrow(voters_g4),' (',round((nrow(voters_g4)*100)/num_group_4,0),'%)',sep=''),
            paste(nrow(voters_g5),' (',round((nrow(voters_g5)*100)/num_group_5,0),'%)',sep=''))
row_mid = c(paste(nrow(mid_cont_g1),' (',round((nrow(mid_cont_g1)*100)/num_group_1,0),'%)',sep=''),
            paste(nrow(mid_cont_g2),' (',round((nrow(mid_cont_g2)*100)/num_group_2,0),'%)',sep=''),
            paste(nrow(mid_cont_g3),' (',round((nrow(mid_cont_g3)*100)/num_group_3,0),'%)',sep=''),
            paste(nrow(mid_cont_g4),' (',round((nrow(mid_cont_g4)*100)/num_group_4,0),'%)',sep=''),
            paste(nrow(mid_cont_g5),' (',round((nrow(mid_cont_g5)*100)/num_group_5,0),'%)',sep=''))
row_full = c(paste(nrow(full_cont_g1),' (',round((nrow(full_cont_g1)*100)/num_group_1,0),'%)',sep=''),
             paste(nrow(full_cont_g2),' (',round((nrow(full_cont_g2)*100)/num_group_2,0),'%)',sep=''),
             paste(nrow(full_cont_g3),' (',round((nrow(full_cont_g3)*100)/num_group_3,0),'%)',sep=''),
             paste(nrow(full_cont_g4),' (',round((nrow(full_cont_g4)*100)/num_group_4,0),'%)',sep=''),
             paste(nrow(full_cont_g5),' (',round((nrow(full_cont_g5)*100)/num_group_5,0),'%)',sep=''))
table_clusters = rbind(row_obs,row_vot,row_mid,row_full)
rownames(table_clusters) = c('observers','voters','middle contributors', 'full contributors')
colnames(table_clusters) = c('group 1', 'group 2', 'group 3', 'group 4', 'group 5')

# Cluster Contigency Table
row_obs = c(nrow(observers_g1), nrow(observers_g2), nrow(observers_g3), nrow(observers_g4), nrow(observers_g5))
row_vot = c(nrow(voters_g1), nrow(voters_g2), nrow(voters_g3), nrow(voters_g4), nrow(voters_g5))
row_mid = c(nrow(mid_cont_g1), nrow(mid_cont_g2), nrow(mid_cont_g3), nrow(mid_cont_g4), nrow(mid_cont_g5))
row_full = c(nrow(full_cont_g1), nrow(full_cont_g2), nrow(full_cont_g3), nrow(full_cont_g4), nrow(full_cont_g5))
cont_table_clusters = rbind(row_obs,row_vot,row_mid,row_full)
rownames(cont_table_clusters) = c('observers','voters','mid_contributors', 'full_contributors')
colnames(cont_table_clusters) = c('group_1','group_2', 'group_3', 'group_4', 'group_5')
chisq.test(cont_table_clusters)

# Print numbers for the paper
# 1- Number of registrations
reg_first_stage = nrow(users_camp_other)
print(paste('Register to the first stage:', reg_first_stage))
new_second_stage = nrow(filter(users_camp_other_survey_3, !email %in% users_camp_other$email))
print(paste('New participants on the second stage:', new_second_stage))
print(paste('Total participants:', reg_first_stage+new_second_stage))

# 2- Ideas proposed
ideas_fs = sum(users_camp_erim[,'ideas']) + sum(users_camp_hall[,'ideas']) + sum(users_camp_vies[,'ideas']) + sum(users_camp_other[,'ideas'])
print(paste('Ideas proposed on the first stage:',ideas_fs))
ideas_ss = sum(users_camp_0_survey_3[,'ideas']) + sum(users_camp_1_survey_3[,'ideas']) + sum(users_camp_2_survey_3[,'ideas']) + sum(users_camp_other_survey_3[,'ideas'])
print(paste('Ideas proposed on the second stage:',ideas_ss))
print(paste('Total ideas proposed:', ideas_fs+ideas_ss))

# 3- Comments posted
comments_fs = sum(users_camp_erim[,'comments']) + sum(users_camp_hall[,'comments']) + sum(users_camp_vies[,'comments']) + sum(users_camp_other[,'ideas'])
print(paste('Comments posted on the first stage:',comments_fs))
comments_ss = sum(users_camp_0_survey_3[,'comments']) + sum(users_camp_1_survey_3[,'comments']) + sum(users_camp_2_survey_3[,'comments']) + sum(users_camp_other_survey_3[,'ideas'])
print(paste('Comments proposed on the second stage:',comments_ss))
print(paste('Total comments posted:', comments_fs+comments_ss))

# 4- Votes casted
votes_fs = sum(users_camp_erim[,'votes']) + sum(users_camp_hall[,'votes']) + sum(users_camp_vies[,'votes']) + sum(users_camp_other[,'ideas'])
print(paste('Votes casted on the first stage:',votes_fs))
votes_ss = sum(users_camp_0_survey_3[,'votes']) + sum(users_camp_1_survey_3[,'votes']) + sum(users_camp_2_survey_3[,'votes'])  + sum(users_camp_other_survey_3[,'ideas'])
print(paste('Votes casted on the second stage:',votes_ss))
print(paste('Total votes casted:', votes_fs+votes_ss))

# 5- Participation (without those that posted content outside the predefined campaigns)
participation_s1s2 = users_camp_other
participation_s1s2 = create_joint_dataset(users_camp_vies, participation_s1s2)
participation_s1s2 = create_joint_dataset(users_camp_hall, participation_s1s2)
participation_s1s2 = create_joint_dataset(users_camp_erim, participation_s1s2)
participation_s1s2 = participation_s1s2 %>% select(-id)
participation_s1s2 = participation_s1s2 %>% filter(email != '')
contributors_fs = participation_s1s2 %>% filter(ideas!=0|comments!=0|votes!=0)
observers_fs = participation_s1s2 %>% filter(ideas==0&comments==0&votes==0)
print(paste('Contributors first stage: ',nrow(contributors_fs),' (',
            round((nrow(contributors_fs)/reg_first_stage)*100,1),'%)',sep=''))
print(paste('Observers first stage:',nrow(observers_fs)))

participation_s3 = users_camp_other_survey_3
participation_s3 = create_joint_dataset(users_camp_0_survey_3, participation_s3)
participation_s3 = create_joint_dataset(users_camp_1_survey_3, participation_s3)
participation_s3 = create_joint_dataset(users_camp_2_survey_3, participation_s3)
participation_s3 = participation_s3 %>% select(-id)
participation_s3 = participation_s3 %>% filter(email != '')
contributors_ss = participation_s3 %>% filter(ideas!=0|comments!=0|votes!=0)
contributors_ss_no_fs = participation_s3 %>% filter(ideas!=0|comments!=0|votes!=0) %>% filter(!email %in% participation_s1s2$email)
observers_ss = participation_s3 %>% filter(ideas==0&comments==0&votes==0)
print(paste('Contributors second stage:',nrow(contributors_ss)))
print(paste('Observers second stage:',nrow(observers_ss)))

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
total_contributors = participants %>% filter(ideas!=0|comments!=0|votes!=0)
total_observers = participants %>% filter(ideas==0&comments==0&votes==0)
print(paste('Total contributors: ',nrow(total_contributors),' (',round((nrow(total_contributors)/(reg_first_stage+new_second_stage))*100,1),'%)',sep=''))
print(paste('Total observers: ',nrow(total_observers),' (',round((nrow(total_observers)/(reg_first_stage+new_second_stage))*100,1),'%)',sep=''))


# 6- Calculate population of full contributors
full_contributors_fs = participation_s1s2 %>% filter(ideas!=0&comments!=0&votes!=0)
print(paste('Full contributors on the first stage: ',nrow(full_contributors_fs),' (',round((nrow(full_contributors_fs)/reg_first_stage)*100,1),'%)',sep=''))

full_contributors_ss = participation_s3 %>% filter(ideas!=0&comments!=0&votes!=0)
print(paste('Full contributors on the second stage: ',nrow(full_contributors_ss),' (',round((nrow(full_contributors_ss)/nrow(users_camp_0_survey_3))*100,1),'%)',sep=''))

full_contributors_total = participants %>% filter(ideas!=0&comments!=0&votes!=0)
print(paste('Total full contributors: ',nrow(full_contributors_total),' (',round((nrow(full_contributors_total)/(reg_first_stage+new_second_stage))*100,1),'%)',sep=''))

# 7- Calculate population of ocassional contributors
occasional_contributors_total = nrow(total_contributors) - nrow(full_contributors_total)

# 8- Calculate population of commenters
commenters_fs = participation_s1s2 %>% filter(comments!=0)
print(paste('Commenters on the first stage: ',nrow(commenters_fs),' (',round((nrow(commenters_fs)/reg_first_stage)*100,1),'%)',sep=''))

commenters_ss = participation_s3 %>% filter(comments!=0)
print(paste('Commenters on the second stage: ',nrow(commenters_ss),' (',round((nrow(commenters_ss)/nrow(users_camp_0_survey_3))*100,1),'%)',sep=''))

total_commenters = participants %>% filter(comments!=0)
print(paste('Total commenters: ',nrow(total_commenters),' (',round((nrow(total_commenters)/(reg_first_stage+new_second_stage))*100,1),'%)',sep=''))

# 9- Calculate population of voters
voters_fs = participation_s1s2 %>% filter(votes!=0)
print(paste('Voters on the first stage: ',nrow(voters_fs),' (',round((nrow(voters_fs)/reg_first_stage)*100,1),'%)',sep=''))

voters_ss = participation_s3 %>% filter(votes!=0)
print(paste('Voters on the second stage: ',nrow(voters_ss),' (',round((nrow(voters_ss)/nrow(users_camp_0_survey_3))*100,1),'%)',sep=''))

total_voters = participants %>% filter(votes!=0)
print(paste('Total voters: ',nrow(total_voters),' (',round((nrow(total_voters)/(reg_first_stage+new_second_stage))*100,1),'%)',sep=''))
