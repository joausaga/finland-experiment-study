#######
# An analysis of how much the opinion of
# the participant has changed along the
# process
#######

# Load libraries
library(dplyr)
source('utils.R')

# Reading data
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
survey_1 = raw_survey_1 %>% select(email,Q7_3,Q7_5,Q7_7,Q8_1,Q8_2,Q8_4,Q8_5,Q8_8)
survey_2 = raw_survey_2 %>% select(email,Q13_3,Q13_5,Q13_7,Q12_1,Q12_2,Q12_4,Q12_5,Q12_8,Q2_2)
survey_3 = raw_survey_3 %>% select(email,Q6_4) %>% filter(!is.na(Q6_4))
survey_12 = merge(survey_1,survey_2,by.x='email',by.y='email')
survey_1 = filter(survey_1, email %in% survey_12$email)
survey_2 = filter(survey_2, email %in% survey_12$email)
colnames(mot_survey_1) = c('email', 'datetime', 'o1s1', 'o2s1', 'o3s1', 'o4s1', 'o5s1', 'o6s1', 'o7s1')
colnames(mot_survey_2) = c('email', 'datetime', 'o1s2', 'o2s2', 'o3s2', 'o4s2', 'o5s2', 'o6s2', 'o7s2', 'o8s2')
colnames(mot_survey_3) = c('email', 'datetime', 'o1s3', 'o2s3', 'o3s3', 'o4s3', 'o5s3', 'o6s3', 'o7s3', 'o8s3')
mot_survey_1 = mot_survey_1 %>% filter(email!='') %>% filter(!is.na(o1s1)) %>% filter(o1s1!='') %>% select(-datetime)
mot_survey_2 = mot_survey_2 %>% filter(email!='') %>% filter(!is.na(o1s2)) %>% filter(o1s2!='') %>% select(-datetime)
mot_survey_3 = mot_survey_3 %>% filter(email!='') %>% filter(!is.na(o1s3)) %>% filter(o1s3!='') %>% select(-datetime)
for (i in c(2:9)) {
  var = paste(colnames(survey_1)[i],'-',colnames(survey_2)[i],sep='')
  survey_12[,var] = survey_1[,i] - survey_2[,i]
}
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

# Actually analyze whether there are changes in the opinion of the participants
for (i in c(2:9)) {
  print(paste('Factor ',i,sep=''))
  print(paste('Median (survey 1): ',median(survey_1[,i],na.rm=T),sep=''))
  print(paste('Median (survey 2): ',median(survey_2[,i],na.rm=T),sep=''))
  print(paste('Mean (survey 1): ',round(mean(survey_1[,i],na.rm=T),3),sep=''))
  print(paste('Mean (survey 2): ',round(mean(survey_2[,i],na.rm=T),3),sep=''))
  t_test = t.test(survey_1[,i],survey_2[,i])  
  print(paste('p-value:',t_test$p.value,sep=''))
  if (t_test$p.value < 0.05) {
    print('Found a signifcant difference!')
  }
}

# How participants perceived their opinions changed along the process
mean(survey_2[,'Q2_2'],na.rm=T)
median(survey_2[,'Q2_2'],na.rm=T)
mean(survey_3[,2])
median(survey_3[,2])

# Analyze correlation between changes in participants' opinions and motivation factors (survey 1)
mot_survey_1 = filter(mot_survey_1, email %in% survey_12$email)
for (factor in c('o1s1', 'o2s1', 'o3s1', 'o4s1', 'o5s1', 'o6s1', 'o7s1')) {
  for (option in c('Q7_3-Q13_3','Q7_5-Q13_5','Q7_7-Q13_7','Q8_1-Q12_1','Q8_2-Q12_2','Q8_4-Q12_4', 
                   'Q8_5-Q12_5','Q8_8-Q12_8')) {
    ret = prepare_data(mot_survey_1, survey_12, factor, option)
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

# Analyze correlation between changes in participants' opinions and motivation factors (survey 2)
mot_survey_2 = filter(mot_survey_2, email %in% survey_12$email)
survey_12 = filter(survey_12, email %in% mot_survey_2$email)
for (factor in c('o1s2', 'o2s2', 'o3s2', 'o4s2', 'o5s2', 'o6s2', 'o7s2', 'o8s2')) {
  for (option in c('Q7_3-Q13_3','Q7_5-Q13_5','Q7_7-Q13_7','Q8_1-Q12_1','Q8_2-Q12_2','Q8_4-Q12_4', 
                   'Q8_5-Q12_5','Q8_8-Q12_8', 'Q2_2')) {
    ret = prepare_data(mot_survey_2, survey_12, factor, option)
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

# Analyze correlation between changes in participants' opinions and motivation factors (survey 3)
mot_survey_3 = filter(mot_survey_3, email %in% survey_3$email)
for (factor in c('o1s3', 'o2s3', 'o3s3', 'o4s3', 'o5s3', 'o6s3', 'o7s3', 'o8s3')) {
  ret = prepare_data(mot_survey_3, survey_3, factor, 'Q6_4')
  cor_coef = cor(ret$ds1,ret$ds2)
  print(paste('The correlation between factor ',factor,' and Q6_4 is ',cor_coef,sep=''))
  if (cor_coef > 0.50) {
    t_test = cor.test(ret$ds1,ret$ds2)
    if (t_test$p.value < 0.05) {
      print(paste('Found a high and significant correlation!, p-value: ',t_test$p.value,sep=''))
      
    } else {
      print(paste('Found a high but not significant correlation, p-value: ',t_test$p.value,sep=''))
    }
  }
}

# Analyze correlation between changes in participants' opinions and self-efficacy (survey 1)
se_survey_1 = raw_survey_1 %>% select(email,Q3_1,Q3_2,Q4_1,Q4_2,Q4_3,Q4_4,Q4_5,Q5_1,Q5_3,Q5_4,Q5_5,Q5_6,Q5_7,Q5_14) %>% filter(!is.na(Q3_1))
se_survey_1 = filter(se_survey_1, email %in% survey_12$email)
se_survey_12 = filter(survey_12, email %in% se_survey_1$email)
for (factor in c('Q3_1','Q3_2','Q4_1','Q4_2','Q4_3','Q4_4','Q4_5','Q5_1','Q5_3','Q5_4','Q5_5','Q5_6',
                 'Q5_7','Q5_14')) {
  for (option in c('Q7_3-Q13_3','Q7_5-Q13_5','Q7_7-Q13_7','Q8_1-Q12_1','Q8_2-Q12_2','Q8_4-Q12_4', 
                   'Q8_5-Q12_5','Q8_8-Q12_8')) {
    ret = prepare_data(se_survey_1, se_survey_12, factor, option)
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

# Analyze correlation between changes in participants' opinions and self-efficacy (survey 3)
se_survey_3 = raw_survey_3 %>% select(email,Q3_2,Q6_4,Q6_5,Q7_2) %>% filter(!is.na(Q3_2))
se_survey_3 = filter(se_survey_3, email %in% survey_12$email)
se_survey_12 = filter(survey_12, email %in% se_survey_3$email)
for (factor in c('Q3_2','Q6_4','Q6_5','Q7_2')) {
  for (option in c('Q7_3-Q13_3','Q7_5-Q13_5','Q7_7-Q13_7','Q8_1-Q12_1','Q8_2-Q12_2','Q8_4-Q12_4', 
                   'Q8_5-Q12_5','Q8_8-Q12_8')) {
    ret = prepare_data(se_survey_3, se_survey_12, factor, option)
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

# Analyze correlation between changes in participants' opinions and learning (survey 1)
le_survey_1 = raw_survey_1 %>% select(email,Q6_3,Q6_4,Q6_5) %>% filter(!is.na(Q6_3))
le_survey_1 = filter(le_survey_1, email %in% survey_12$email)
le_survey_12 = filter(survey_12, email %in% le_survey_1$email)
for (factor in c('Q6_3','Q6_4','Q6_5')) {
  for (option in c('Q7_3-Q13_3','Q7_5-Q13_5','Q7_7-Q13_7','Q8_1-Q12_1','Q8_2-Q12_2','Q8_4-Q12_4', 
                   'Q8_5-Q12_5','Q8_8-Q12_8')) {
    ret = prepare_data(le_survey_1, le_survey_12, factor, option)
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

# Analyze correlation between changes in participants' opinions and learning (survey 2)
le_survey_2 = raw_survey_2 %>% select(email,Q11_6,Q11_7,Q11_8) %>% filter(!is.na(Q11_6))
le_survey_2 = filter(le_survey_2, email %in% survey_12$email)
le_survey_12 = filter(survey_12, email %in% le_survey_2$email)
for (factor in c('Q11_6','Q11_7','Q11_8')) {
  for (option in c('Q7_3-Q13_3','Q7_5-Q13_5','Q7_7-Q13_7','Q8_1-Q12_1','Q8_2-Q12_2','Q8_4-Q12_4', 
                   'Q8_5-Q12_5','Q8_8-Q12_8')) {
    ret = prepare_data(le_survey_2, le_survey_12, factor, option)
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

# Analyze correlation between changes in participants' opinions and learning (survey 3)
le_survey_3 = raw_survey_3 %>% select(email,Q11_6,Q11_7,Q11_8) %>% filter(!is.na(Q11_6))
le_survey_3 = filter(le_survey_3, email %in% survey_12$email)
le_survey_12 = filter(survey_12, email %in% le_survey_3$email)
for (factor in c('Q11_6','Q11_7','Q11_8')) {
  for (option in c('Q7_3-Q13_3','Q7_5-Q13_5','Q7_7-Q13_7','Q8_1-Q12_1','Q8_2-Q12_2','Q8_4-Q12_4', 
                   'Q8_5-Q12_5','Q8_8-Q12_8')) {
    ret = prepare_data(le_survey_3, le_survey_12, factor, option)
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

# Analyze correlation between changes in participants' opinions and level of participations (survey 1 and 2)
participation_s1s2 = mutate(participation_s1s2,content=ideas+votes+comments)
participation_s1s2 = mutate(participation_s1s2,ideas_votes=ideas+votes)
participation_s1s2 = mutate(participation_s1s2,ideas_comments=ideas+comments)
participation_s1s2 = mutate(participation_s1s2,votes_comments=votes+comments)
participation_s1s2 = filter(participation_s1s2, email %in% survey_12$email)
for (type_participation in c('content', 'ideas_votes', 'ideas_comments', 'votes_comments', 'votes', 'ideas', 'comments')) {
  for (option in c('Q7_3-Q13_3','Q7_5-Q13_5','Q7_7-Q13_7','Q8_1-Q12_1','Q8_2-Q12_2','Q8_4-Q12_4', 
                   'Q8_5-Q12_5','Q8_8-Q12_8')) {
    ret = prepare_data(participation_s1s2, survey_12, type_participation, option)
    cor_coef = cor(ret$ds1,ret$ds2)
    print(paste('The correlation between the production of ',type_participation,' and the option ',option,' is ',cor_coef,sep=''))
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

# Analyze correlation between changes in participants' opinions and level of participations (survey)
participation_s3 = filter(participation_s3, email %in% survey_12$email)
participation_s3 = mutate(participation_s3,content=ideas+votes+comments)
participation_s3 = mutate(participation_s3,ideas_votes=ideas+votes)
participation_s3 = mutate(participation_s3,ideas_comments=ideas+comments)
participation_s3 = mutate(participation_s3,votes_comments=votes+comments)
for (type_participation in c('content', 'ideas_votes', 'ideas_comments', 'votes_comments', 'votes', 'ideas', 'comments')) {
  for (option in c('Q7_3-Q13_3','Q7_5-Q13_5','Q7_7-Q13_7','Q8_1-Q12_1','Q8_2-Q12_2','Q8_4-Q12_4', 
                   'Q8_5-Q12_5','Q8_8-Q12_8')) {
    ret = prepare_data(participation_s3, survey_12, type_participation, option)
    cor_coef = cor(ret$ds1,ret$ds2)
    print(paste('The correlation between the production of ',type_participation,' and the option ',option,' is ',cor_coef,sep=''))
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

# Analyze correlation between changes in participants' opinions and changes in the motivation factors
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
survey_12_ = survey_12 %>% filter(email %in% ch_mot_factors$email)
ch_mot_factors_ = filter(ch_mot_factors, email %in% survey_12_$email)
ch_opinions = c('Q7_3-Q13_3','Q7_5-Q13_5','Q7_7-Q13_7','Q8_1-Q12_1','Q8_2-Q12_2','Q8_4-Q12_4','Q8_5-Q12_5','Q8_8-Q12_8')
find_correlations(ch_mot_factors_,survey_12_,ch_factors,ch_opinions)
