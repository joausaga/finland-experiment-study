#######
# An analysis of the level of participation in people that
# have replied only the first survey, only the second survey
# and both
#######

# Load libraries
library(ggplot2)
library(dplyr)
source('utils.R')

# Reading data
motivation_pre = read.csv("./data/survey1-condlaw-no-duplicates.csv",header=T, sep=",",stringsAsFactors=F)
motivation_pos = read.csv("./data/survey2-condlaw-no-duplicates.csv",header=T, sep=",",stringsAsFactors=F)
users_camp_other = read.csv("./data/users_camp_other.csv",header=T,sep=";",stringsAsFactors=F) # outside the pre-defined campaigns
users_camp_vies = read.csv("./data/users_camp_vies.csv",header=T,sep=";",stringsAsFactors=F)   # communication campaign (vies)
users_camp_hall = read.csv("./data/users_camp_hall.csv",header=T,sep=";",stringsAsFactors=F)   # governance campaign (hall)
users_camp_erim = read.csv("./data/users_camp_erim.csv",header=T,sep=";",stringsAsFactors=F)   # conflict campaign (erim)

# Data preparation
colnames(motivation_pre) = c('email', 'datetime', 'o1s1', 'o2s1', 'o3s1', 'o4s1', 'o5s1', 'o6s1', 'o7s1')
colnames(motivation_pos) = c('email', 'datetime', 'o1s2', 'o2s2', 'o3s2', 'o4s2', 'o5s2', 'o6s2', 'o7s2', 'o8s2')
motivation_pre = motivation_pre %>% filter(email!='') %>% filter(!is.na(o1s1)) %>% select(-(datetime:o7s1))
motivation_pos = motivation_pos %>% filter(email!='') %>% filter(!is.na(o1s2)) %>% select(-(datetime:o8s2))
participants = users_camp_other
participants = create_joint_dataset(users_camp_vies, participants)
participants = create_joint_dataset(users_camp_hall, participants)
participants = create_joint_dataset(users_camp_erim, participants)
participants = participants %>% select(-id)

# Cluster survey responders
only_pre = motivation_pre %>% filter(!email %in% motivation_pos$email)
both = motivation_pos %>% filter(email %in% motivation_pre$email)

# Cluster survey responders by their level of participation
part_only_pre = participants %>% filter(email %in% only_pre$email)
part_both = participants %>% filter(email %in% both$email)

# Cluster responders of first survey
num_only_pre = nrow(part_only_pre)
observers_pre = part_only_pre %>% filter(ideas==0&votes==0&comments==0) # nothing, only observe what happened
voters_pre = part_only_pre %>% filter(ideas==0&votes!=0&comments==0)    # only vote
mid_cont_pre = part_only_pre %>% filter((ideas==0&votes!=0&comments!=0)|
                                        (ideas!=0&votes==0&comments!=0)|
                                        (ideas!=0&votes!=0&comments==0)|
                                        (ideas!=0&votes==0&comments==0)|
                                        (ideas==0&votes==0&comments!=0))   
full_cont_pre = part_only_pre %>% filter(ideas!=0&votes!=0&comments!=0) # suggest ideas, comment and vote

# Cluster responders of both surveys
num_both = nrow(part_both)
observers_both = part_both %>% filter(ideas==0&votes==0&comments==0) # nothing, only observe what happened
voters_both = part_both %>% filter(ideas==0&votes!=0&comments==0)    # only vote
mid_cont_both = part_both %>% filter((ideas==0&votes!=0&comments!=0)|
                                     (ideas!=0&votes==0&comments!=0)|
                                     (ideas!=0&votes!=0&comments==0)|
                                     (ideas!=0&votes==0&comments==0)|
                                     (ideas==0&votes==0&comments!=0))
full_cont_both = part_both %>% filter(ideas!=0&votes!=0&comments!=0) # suggest ideas, comment and vote

# Compare clusters
row_obs = c('observer',paste(nrow(observers_pre),' (',round((nrow(observers_pre)*100)/num_only_pre,0),'%)',sep=''),
            paste(nrow(observers_both),' (',round((nrow(observers_both)*100)/num_both,0),'%)',sep=''))
row_vot = c('voters',paste(nrow(voters_pre),' (',round((nrow(voters_pre)*100)/num_only_pre,0),'%)',sep=''),
            paste(nrow(voters_both),' (',round((nrow(voters_both)*100)/num_both,0),'%)',sep=''))
row_mid = c('middle contributors',paste(nrow(mid_cont_pre),' (',round((nrow(mid_cont_pre)*100)/num_only_pre,0),'%)',sep=''),
            paste(nrow(mid_cont_both),' (',round((nrow(mid_cont_both)*100)/num_both,0),'%)',sep=''))
row_full = c('full contributors',paste(nrow(full_cont_pre),' (',round((nrow(full_cont_pre)*100)/num_only_pre,0),'%)',sep=''),
             paste(nrow(full_cont_both),' (',round((nrow(full_cont_both)*100)/num_both,0),'%)',sep=''))
table_clusters = rbind(row_obs,row_vot,row_mid,row_full)

# Cluster Contigency Table
row_obs = c(nrow(observers_pre), nrow(observers_both))
row_vot = c(nrow(voters_pre), nrow(voters_both))
row_mid = c(nrow(mid_cont_pre), nrow(mid_cont_both))
row_full = c(nrow(full_cont_pre), nrow(full_cont_both))
cont_table_clusters = rbind(row_obs,row_vot,row_mid,row_full)
rownames(cont_table_clusters) = c('observers','voters','mid_contributors', 'full_contributors')
colnames(cont_table_clusters) = c('only_pre_survey','both_surveys')
chisq.test(cont_table_clusters)

# Ideators in both clusters
ideators = part_only_pre[part_only_pre$ideas>0,] # Get rid of contributors that didn't produce ideas
ggplot(part_only_pre, aes(x=votes)) + 
  geom_histogram(binwidth=1, colour="black", fill="white") +
  #scale_x_continuous(breaks=c(0:6)) +
  #scale_y_continuous(breaks=c(0,5,10,15,20,25,30,35,40,45,50)) +
  labs(x="Ideas Count", y="Participants Count") +
  theme(axis.text=element_text(size=28), axis.title=element_text(size=28))