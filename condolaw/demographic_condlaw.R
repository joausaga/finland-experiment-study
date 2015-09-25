# Load library
library(ggplot2)
library(dplyr)

# Reading Data
demographic = read.csv("./data/demographic-condlaw.csv",header=TRUE,sep=",",stringsAsFactors=FALSE)
columns = read.table("./data/demographic-condlaw-columns.txt")
colnames(demographic) = columns[,2]
raw_survey_1 = read.csv("./data/survey1-condlaw-complete.csv",header=T, sep=",",stringsAsFactors=F)
demographic = filter(demographic, email %in% raw_survey_1$email)
raw_survey_3 = read.csv("./data/survey3-condlaw2-complete.csv",header=T, sep=",",stringsAsFactors=F)

# Gender
genders = raw_survey_1 %>% filter(!is.na(Q33)) %>% select(email, Q33)
genders_s3 = raw_survey_3 %>% filter(!is.na(Q13)) %>% filter(!email %in% genders$email) %>% select(Q13)
women = sum(genders$Q33==1) + sum(genders_s3$Q13==1)
men = sum(genders$Q33==2) + sum(genders_s3$Q13==2)
gender = data.frame(option=c("Male","Female"),quantity=c(men*100/(men+women),women*100/(men+women)))

ggplot(gender, aes(x=1, y=quantity, fill=option)) +
  geom_bar(stat="identity", color='black') +  # Black border
  coord_polar(theta='y') +
  guides(fill=guide_legend(override.aes=list(colour=NA)))  + # Remove black diagonals from legend
  scale_fill_discrete(name="Gender") +  # Change lengend title
  theme(axis.text.y=element_blank(), axis.title=element_blank(), 
        axis.ticks=element_blank(), legend.text=element_text(size=20),
        legend.title=element_text(size=20)) +
  guides(fill=FALSE)
  #geom_text(aes(x= factor(1), y=centr2, label = quantity), size=10)

# Age
ages = raw_survey_1 %>% filter(!is.na(Q34)) %>% select(email, Q34)
ages_s3 = raw_survey_3 %>% filter(!is.na(Q12)) %>% filter(!email %in% ages$email) %>% select(Q12)
age_17 = sum(ages$Q34==2) + sum(ages_s3$Q12==2)
age_18_25 = sum(ages$Q34==3) + sum(ages_s3$Q12==3)
age_26_34 = sum(ages$Q34==4) + sum(ages_s3$Q12==4)
age_35_54 = sum(ages$Q34==5) + sum(ages_s3$Q12==5)
age_55_64 = sum(ages$Q34==6) + sum(ages_s3$Q12==6)
age_65 = sum(ages$Q34==7) + sum(ages_s3$Q12==7)
tot_ages = nrow(ages) + nrow(ages_s3)
age = data.frame(option=c("34 years or less", "35-54 years", "55-64 years", "More than 65 years"),
                    quantity=c(age_17+age_18_25+age_26_34*100/tot_ages,
                               age_35_54*100/tot_ages,age_55_64*100/tot_ages,age_65*100/tot_ages))

ggplot(age, aes(x=1, y=quantity, fill=option)) +
  geom_bar(stat="identity", color='black') +  # Black border
  guides(fill=guide_legend(override.aes=list(colour=NA)))  + # Remove black diagonals from legend
  scale_fill_discrete(name="Ages")  + # Change lengend title
  theme(axis.text.y=element_blank(), axis.text.x=element_blank(), axis.title=element_blank(), 
        axis.ticks=element_blank(), legend.text=element_text(size=16),
        legend.title=element_text(size=20)) +
  guides(fill=FALSE)

# Occupational Education
edu = raw_survey_1 %>% filter(!is.na(Q36)) %>% select(email,Q36)
edu_s3 = raw_survey_3 %>% filter(!is.na(Q17)) %>% filter(!email %in% edu$email) %>% select(Q17)
school = sum(edu$Q36==1) + sum(edu_s3$Q17==1)
vocational = sum(edu$Q36==2) + sum(edu_s3$Q17==2)
college = sum(edu$Q36==3) + + sum(edu_s3$Q17==3)
uni = sum(edu$Q36==4) + sum(edu_s3$Q17==4)
science = sum(edu$Q36==5) + sum(edu_s3$Q17==5)
bach = sum(edu$Q36==6) + sum(edu_s3$Q17==6)
master = sum(edu$Q36==7) + sum(edu_s3$Q17==7)
doc = sum(edu$Q36==8) + sum(edu_s3$Q17==8)
no_formal_edu = sum(edu$Q36==9) + sum(edu_s3$Q17==9)
tot_edu = nrow(edu) + nrow(edu_s3)
df_edu = data.frame(option=c("PhD", "Univ", "No-Formal", "Vocational", "Bachelor", "Science", "Master","College"),
                 quantity=c(doc*100/tot_edu, uni*100/tot_edu, no_formal_edu*100/tot_edu,
                            vocational*100/tot_edu, bach*100/tot_edu, science*100/tot_edu,
                            master*100/tot_edu, college*100/tot_edu))

ggplot(df_edu, aes(x=1, y=quantity, fill=option)) +
  geom_bar(stat="identity", color='black') +  # Black border
  guides(fill=guide_legend(override.aes=list(colour=NA)))  + # Remove black diagonals from legend
  scale_fill_discrete(name="Education")  + # Change lengend title
  theme(axis.text.y=element_blank(), axis.text.x=element_blank(), axis.title=element_blank(), 
        axis.ticks=element_blank(), legend.text=element_text(size=16),
        legend.title=element_text(size=20)) +
  guides(fill=FALSE)
  

# Basic Education
basic_edu = raw_survey_1 %>% filter(!is.na(Q35)) %>% select(email,Q35)
basic_edu_s3 = raw_survey_3 %>% filter(!is.na(Q16)) %>% filter(!email %in% basic_edu$email) %>% select(Q16)
primary_school = sum(basic_edu$Q35==1) + sum(basic_edu_s3$Q16==1)
middle_school = sum(basic_edu$Q35==2) + sum(basic_edu_s3$Q16==2)
still_school = sum(basic_edu$Q35==3) + sum(basic_edu_s3$Q16==3)
secundary_school = sum(basic_edu$Q35==4) + sum(basic_edu_s3$Q16==4)
no_formal_edu = sum(basic_edu$Q35==5) + sum(basic_edu_s3$Q16==5)
tot_edu = nrow(basic_edu) + nrow(basic_edu_s3)
df_edu = data.frame(option=c("Other", "Middle School", "Secundary School"),
                    quantity=c((no_formal_edu+primary_school)*100/tot_edu,
                               middle_school*100/tot_edu, secundary_school*100/tot_edu))
ggplot(df_edu, aes(x=1, y=quantity, fill=option)) +
  geom_bar(stat="identity", color='black') +  # Black border
  guides(fill=guide_legend(override.aes=list(colour=NA)))  + # Remove black diagonals from legend
  scale_fill_discrete(name="Education")  + # Change lengend title
  theme(axis.text.y=element_blank(), axis.text.x=element_blank(), axis.title=element_blank(), 
        axis.ticks=element_blank(), legend.text=element_text(size=16),
        legend.title=element_text(size=20)) +
  guides(fill=FALSE)


# Current Situation
situ = raw_survey_1 %>% filter(!is.na(Q37)) %>% select(email,Q37)
situ_s3 = raw_survey_3 %>% filter(!is.na(Q15)) %>% filter(!email %in% situ$email) %>% select(Q15)
tot_situ = nrow(situ) + nrow(situ_s3)
full_time = sum(situ$Q37==1) + sum(situ_s3$Q15==1)
retired = sum(situ$Q37==7|situ$Q37==8) + sum(situ_s3$Q15==7|situ_s3$Q15==8)
other_situ = sum(situ$Q37==2|situ$Q37==3|situ$Q37==4|situ$Q37==5|
                 situ$Q37==6|situ$Q37==9|situ$Q37==10|situ$Q37==11) +
             sum(situ_s3$Q15==2|situ_s3$Q15==3|situ_s3$Q15==4|situ_s3$Q15==5|situ_s3$Q15==6|situ_s3$Q15==9|situ_s3$Q15==10|situ_s3$Q15==11)
df_situ = data.frame(option=c("Other","Full-Time Employee", "Retired"),
                    quantity=c(other_situ*100/tot_situ,full_time*100/tot_situ,retired*100/tot_situ))

ggplot(df_situ, aes(x=1, y=quantity, fill=option)) +
  geom_bar(stat="identity", color='black') +  # Black border
  guides(fill=guide_legend(override.aes=list(colour=NA)))  + # Remove black diagonals from legend
  scale_fill_discrete(name="Current Situation")  + # Change lengend title
  theme(axis.text.y=element_blank(), axis.text.x=element_blank(), axis.title=element_blank(), 
        axis.ticks=element_blank(), legend.text=element_text(size=16),
        legend.title=element_text(size=20))


# Calculate how many participants were either working as managers or had worked in a 
# managerial position before retiring
situ = raw_survey_1 %>% filter(!is.na(Q38)) %>% select(email,Q38)
situ_s3 = raw_survey_3 %>% filter(!is.na(Q14)) %>% filter(!email %in% situ$email) %>% select(Q14)
tot_situ = nrow(situ) + nrow(situ_s3)
managers = sum(situ$Q38==3) + sum(situ_s3$Q14==3)
managers/tot_situ


# Activity
total_surveyed = nrow(raw_survey_1)
total_surveyed = total_surveyed + nrow(filter(raw_survey_3, !email %in% raw_survey_1$email))

ds_w = raw_survey_1 %>% filter(!is.na(Q40_1)) %>% select(email, Q40_1)
ds_w_s3 = raw_survey_3 %>% filter(!is.na(Q10_5)) %>% filter(!email %in% ds_w$email) %>% select(Q10_5)
writers = nrow(ds_w) + nrow(ds_w_s3)
per_w = writers*100/total_surveyed

ds_b = raw_survey_1 %>% filter(!is.na(Q40_2) | !is.na(Q40_3)) %>% select(email, Q40_2, Q40_3)
ds_b_s3 = raw_survey_3 %>% filter(!is.na(Q10_6)|!is.na(Q10_7)) %>% filter(!email %in% ds_b$email) %>% select(Q10_6,Q10_7)
diss_bloggers = nrow(ds_b) + nrow(ds_b_s3)
per_db = (diss_bloggers)*100/total_surveyed

ds_w_mp = raw_survey_1 %>% filter(!is.na(Q40_4)) %>% select(email, Q40_4)
ds_w_mp_s3 = raw_survey_3 %>% filter(!is.na(Q10_8)) %>% filter(!email %in% ds_w_mp$email) %>% select(Q10_8)
writers_mp = nrow(ds_w_mp) + nrow(ds_w_mp_s3)
per_mp = writers_mp*100/total_surveyed

ds_volun = raw_survey_1 %>% filter(!is.na(Q40_11)) %>% select(email, Q40_11)
ds_volun_s3 = raw_survey_3 %>% filter(!is.na(Q10_15)) %>% filter(!email %in% ds_w_mp$email) %>% select(Q10_15)
volunteers = nrow(ds_volun) + nrow(ds_volun_s3)
per_volun = volunteers*100/total_surveyed

ds_w_local = raw_survey_1 %>% filter(!is.na(Q40_5)) %>% select(email, Q40_5)
ds_w_local_s3 = raw_survey_3 %>% filter(!is.na(Q10_9)) %>% filter(!email %in% ds_w_local$email) %>% select(Q10_9)
writers_local = nrow(ds_w_local) + nrow(ds_w_local_s3)
per_local = writers_local*100/total_surveyed

ds_vote_s3 = raw_survey_3 %>% filter(!is.na(Q10_4)) %>% select(email, Q10_4)
per_vote = nrow(ds_vote_s3)*100/nrow(raw_survey_3)

df_activity = data.frame(option=c("Voted in elections", "Written Op-Ed", "Expressed Opinion\nOnline", 
                                  "Contacted  Parliament\nAuthority", "Contacted Local\nAuthority", "Volunteered in a\nNon-Profit Org"),
                         per=c(per_vote,per_w,per_db,per_mp,per_local,per_volun))
df_activity$option = factor(df_activity$option, 
                            levels = c("Voted in elections", "Expressed Opinion\nOnline", "Written Op-Ed",
                                       "Volunteered in a\nNon-Profit Org", "Contacted  Parliament\nAuthority", 
                                       "Contacted Local\nAuthority"))

ggplot(data=df_activity, aes(x=option, y=per, ymax=110)) + 
  geom_bar(aes(fill=option), stat="identity", color='black') +
  geom_text(aes(label=paste(round(per,0),"%",sep=""), x=option, y=per), 
            position = position_dodge(width = 0.8), vjust=-.6, size=20) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=40, color='black'), 
        axis.title.x = element_blank()) +
  ylab("") +
  guides(fill=FALSE)


# Role in housing companies
ds_ow = raw_survey_1 %>% filter(!is.na(Q10_2)) %>% select(email, Q10_2)
ds_ow_s3_1 = raw_survey_3 %>% filter(!is.na(Q8_4)) %>% filter(!email %in% ds_ow$email) %>% select(Q8_4)
ds_ow_s3_2 = raw_survey_3 %>% filter(!is.na(Q8_10)) %>% 
  filter(!email %in% ds_ow$email & !email %in% ds_ow_s3_1) %>% 
  select(Q8_10)
tot_owners = nrow(ds_ow) + nrow(ds_ow_s3_1) + nrow(ds_ow_s3_2)
s1_s3 = rbind(select(raw_survey_1, email),select(raw_survey_3,email))
uniq_s1_s3 = length(unique(s1_s3$email))
(tot_owners/uniq_s1_s3)*100

role = merge(raw_survey_1,raw_survey_3,by.x='email',by.y='email', all=T)
board = role %>% filter(!is.na(Q10_2) | !is.na(Q8_6)) %>% select(email, Q10_2, Q8_6)
nrow(board)/uniq_s1_s3


# Analyze percentage of active participants
raw_survey_1 = read.csv("./data/survey1-condlaw-complete.csv",header=T, sep=",",stringsAsFactors=F)
raw_survey_3 = read.csv("./data/survey3-condlaw2-complete.csv",header=T, sep=",",stringsAsFactors=F)

# Get rid off those that have already answered suvery 1
raw_survey_3 = filter(raw_survey_3, !email %in% raw_survey_1$email)

# Get rid off unused columns
raw_survey_1 = select(raw_survey_1, email, c(Q40_1:Q40_15))
raw_survey_3 = select(raw_survey_3, email, c(Q10_5:Q10_19)) 

tot_surveyed = nrow(raw_survey_1) + nrow(raw_survey_3)

active_s1 = filter(raw_survey_1, !is.na(Q40_1)|!is.na(Q40_2)|!is.na(Q40_3)|!is.na(Q40_4)|!is.na(Q40_5)|!is.na(Q40_6)|
                   !is.na(Q40_7)|!is.na(Q40_8)|!is.na(Q40_9)|!is.na(Q40_10)|!is.na(Q40_11)|!is.na(Q40_12)|!is.na(Q40_13)|
                   !is.na(Q40_14))

active_s3 = filter(raw_survey_3, !is.na(Q10_5)|!is.na(Q10_6)|!is.na(Q10_7)|!is.na(Q10_8)|!is.na(Q10_9)|
                   !is.na(Q10_10)|!is.na(Q10_11)|!is.na(Q10_12)|!is.na(Q10_13)|!is.na(Q10_14)|!is.na(Q10_15)|!is.na(Q10_16)|
                   !is.na(Q10_17)|!is.na(Q10_18))  # Don't include voting


tot_active = nrow(active_s1) + nrow(active_s3)

tot_active/tot_surveyed

# Calculate the total of activities per person
for (i in 1:nrow(active_s1)) {
  total_activities = 0
  for (j in 2:16) {
    if (!is.na(active_s1[i,j])) {
      total_activities = total_activities + active_s1[i,j]  
    }    
  }
  active_s1[i,'total'] = total_activities
}

for (i in 1:nrow(active_s3)) {
  total_activities = 0
  for (j in 2:16) {
    if (!is.na(active_s3[i,j])) {
      total_activities = total_activities + active_s3[i,j]  
    }    
  }
  active_s3[i,'total'] = total_activities
}

# Calculate mean of activities
totals = c(active_s1[,'total'],active_s3[,'total'])
mean(totals)
summary(totals)

# Calculate some percentages
cat(paste(round(length(totals[totals == 1])/tot_surveyed * 100,0),'% do 1 activity',sep=''))
cat(paste(round(length(totals[totals == 2])/tot_surveyed * 100,0),'% do 2 activites',sep=''))
cat(paste(round(length(totals[totals == 3])/tot_surveyed * 100,0),'% do 3 activites',sep=''))
cat(paste(round(length(totals[totals == 4])/tot_surveyed * 100,0),'% do 4 activites',sep=''))
cat(paste(round(length(totals[totals > 4])/tot_surveyed * 100,0),'% do more than 4 activites',sep=''))

cat(paste(round(length(totals[totals >= 2])/tot_surveyed * 100,0),'% do at least 2 activites',sep=''))
cat(paste(round(length(totals[totals >= 3])/tot_surveyed * 100,0),'% do at least 3 activites',sep=''))

# Create a table of civic activities for active participants
civic_activity = matrix(c('written_op-ed',
                        round(sum(raw_survey_1$Q40_1,raw_survey_3$Q10_5,na.rm=T)/tot_surveyed,2)),ncol=2)
civic_activity = rbind(civic_activity, 
                       c('written_in_blog',round(sum(raw_survey_1$Q40_2,raw_survey_3$Q10_6,na.rm=T)/tot_surveyed,2)))
civic_activity = rbind(civic_activity, 
                       c('written_forum',round(sum(raw_survey_1$Q40_3,raw_survey_3$Q10_7,na.rm=T)/tot_surveyed,2)))
civic_activity = rbind(civic_activity, 
                       c('written_to_mp',round(sum(raw_survey_1$Q40_4,raw_survey_3$Q10_8,na.rm=T)/tot_surveyed,2)))
civic_activity = rbind(civic_activity, 
                       c('written_to_councilor',round(sum(raw_survey_1$Q40_5,raw_survey_3$Q10_9,na.rm=T)/tot_surveyed,2)))
civic_activity = rbind(civic_activity, 
                       c('council_meeting',round(sum(raw_survey_1$Q40_6,raw_survey_3$Q10_10,na.rm=T)/tot_surveyed,2)))
civic_activity = rbind(civic_activity, 
                       c('committee_meeting',round(sum(raw_survey_1$Q40_7,raw_survey_3$Q10_11,na.rm=T)/tot_surveyed,2)))
civic_activity = rbind(civic_activity, 
                       c('signed_petition',round(sum(raw_survey_1$Q40_8,raw_survey_3$Q10_12,na.rm=T)/tot_surveyed,2)))
civic_activity = rbind(civic_activity, 
                       c('initiated_petition',round(sum(raw_survey_1$Q40_9,raw_survey_3$Q10_13,na.rm=T)/tot_surveyed,2)))
civic_activity = rbind(civic_activity, 
                       c('peaceful_demo',round(sum(raw_survey_1$Q40_10,raw_survey_3$Q10_14,na.rm=T)/tot_surveyed,2)))
civic_activity = rbind(civic_activity, 
                       c('volunteered_ngo',round(sum(raw_survey_1$Q40_11,raw_survey_3$Q10_15,na.rm=T)/tot_surveyed,2)))
civic_activity = rbind(civic_activity, 
                       c('municipal_candidate',round(sum(raw_survey_1$Q40_12,raw_survey_3$Q10_16,na.rm=T)/tot_surveyed,2)))
civic_activity = rbind(civic_activity, 
                       c('parliamentary_candidate',round(sum(raw_survey_1$Q40_13,raw_survey_3$Q10_17,na.rm=T)/tot_surveyed,2)))
civic_activity = rbind(civic_activity, 
                       c('campaign_for_candidate',round(sum(raw_survey_1$Q40_14,raw_survey_3$Q10_18,na.rm=T)/tot_surveyed,2)))
civic_activity = rbind(civic_activity, 
                       c('voted',round(sum(raw_survey_3$Q10_4,na.rm=T)/nrow(raw_survey_3),2)))
colnames(civic_activity) = c('activity','total')

df_civic_activity = as.data.frame(civic_activity)

df_civic_activity = arrange(df_civic_activity, desc(total))


# Create a table of civic activities for the WHOLE population
raw_survey_1 = read.csv("./data/survey1-condlaw-complete.csv",header=T, sep=",",stringsAsFactors=F)
raw_survey_3 = read.csv("./data/survey3-condlaw2-complete.csv",header=T, sep=",",stringsAsFactors=F)

# Get rid off those that have already answered suvery 1
raw_survey_3 = filter(raw_survey_3, !email %in% raw_survey_1$email)

# Get rid off unused columns
raw_survey_1 = select(raw_survey_1, email, c(Q40_1:Q40_15))
raw_survey_3 = select(raw_survey_3, email, c(Q10_4:Q10_19)) 

tot_surveyed = nrow(raw_survey_1) + nrow(raw_survey_3)

civic_activity = matrix(c('written_op-ed',
                          round(sum(raw_survey_1$Q40_1,raw_survey_3$Q10_5,na.rm=T)/tot_surveyed,2)),ncol=2)
civic_activity = rbind(civic_activity, 
                       c('written_in_blog',round(sum(raw_survey_1$Q40_2,raw_survey_3$Q10_6,na.rm=T)/tot_surveyed,2)))
civic_activity = rbind(civic_activity, 
                       c('written_forum',round(sum(raw_survey_1$Q40_3,raw_survey_3$Q10_7,na.rm=T)/tot_surveyed,2)))
civic_activity = rbind(civic_activity, 
                       c('written_to_mp',round(sum(raw_survey_1$Q40_4,raw_survey_3$Q10_8,na.rm=T)/tot_surveyed,2)))
civic_activity = rbind(civic_activity, 
                       c('written_to_councilor',round(sum(raw_survey_1$Q40_5,raw_survey_3$Q10_9,na.rm=T)/tot_surveyed,2)))
civic_activity = rbind(civic_activity, 
                       c('council_meeting',round(sum(raw_survey_1$Q40_6,raw_survey_3$Q10_10,na.rm=T)/tot_surveyed,2)))
civic_activity = rbind(civic_activity, 
                       c('committee_meeting',round(sum(raw_survey_1$Q40_7,raw_survey_3$Q10_11,na.rm=T)/tot_surveyed,2)))
civic_activity = rbind(civic_activity, 
                       c('signed_petition',round(sum(raw_survey_1$Q40_8,raw_survey_3$Q10_12,na.rm=T)/tot_surveyed,2)))
civic_activity = rbind(civic_activity, 
                       c('initiated_petition',round(sum(raw_survey_1$Q40_9,raw_survey_3$Q10_13,na.rm=T)/tot_surveyed,2)))
civic_activity = rbind(civic_activity, 
                       c('peaceful_demo',round(sum(raw_survey_1$Q40_10,raw_survey_3$Q10_14,na.rm=T)/tot_surveyed,2)))
civic_activity = rbind(civic_activity, 
                       c('volunteered_ngo',round(sum(raw_survey_1$Q40_11,raw_survey_3$Q10_15,na.rm=T)/tot_surveyed,2)))
civic_activity = rbind(civic_activity, 
                       c('municipal_candidate',round(sum(raw_survey_1$Q40_12,raw_survey_3$Q10_16,na.rm=T)/tot_surveyed,2)))
civic_activity = rbind(civic_activity, 
                       c('parliamentary_candidate',round(sum(raw_survey_1$Q40_13,raw_survey_3$Q10_17,na.rm=T)/tot_surveyed,2)))
civic_activity = rbind(civic_activity, 
                       c('campaign_for_candidate',round(sum(raw_survey_1$Q40_14,raw_survey_3$Q10_18,na.rm=T)/tot_surveyed,2)))
civic_activity = rbind(civic_activity, 
                       c('voted',round(sum(raw_survey_3$Q10_4,na.rm=T)/nrow(raw_survey_3),2)))
colnames(civic_activity) = c('activity','total')

df_civic_activity = as.data.frame(civic_activity)

df_civic_activity = arrange(df_civic_activity, desc(total))
