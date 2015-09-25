#######
# An analysis about trust in the law-making process
#######

# Load libraries
library(dplyr)
source('utils.R')

# Reading data
raw_survey_1 = read.csv("./data/survey1-condlaw-complete.csv",header=T, sep=",",stringsAsFactors=F)
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
survey_1 = raw_survey_1 %>% select(email,Q9_7,Q9_8,Q9_9,Q9_10,Q9_12) %>% filter(!is.na(Q9_7))
colnames(survey_1) = c('email',
                       '1. Finnish mechamism produces good laws', 
                       '2. Civil servants have a sufficient\nlevel of expertise and knowledge', 
                       '3. Civil servants are taking all the\nstakeholders interests into account', 
                       '4. Civil servants are serving the\ncitizens\' best interest', 
                       '5. Civil servants are taking only\ncertain stakeholders\' interests into account')

melt_survey_1 = melt(survey_1, id.vars='email', measure.vars=c('1. Finnish mechamism produces good laws', 
                                                               '2. Civil servants have a sufficient\nlevel of expertise and knowledge', 
                                                               '3. Civil servants are taking all the\nstakeholders interests into account', 
                                                               '4. Civil servants are serving the\ncitizens\' best interest', 
                                                               '5. Civil servants are taking only\ncertain stakeholders\' interests into account'))
melt_survey_1$value = as.numeric(as.character(melt_survey_1$value))
colnames(mot_survey_1) = c('email', 'datetime', 'o1s1', 'o2s1', 'o3s1', 'o4s1', 'o5s1', 'o6s1', 'o7s1')
colnames(mot_survey_2) = c('email', 'datetime', 'o1s2', 'o2s2', 'o3s2', 'o4s2', 'o5s2', 'o6s2', 'o7s2', 'o8s2')
colnames(mot_survey_3) = c('email', 'datetime', 'o1s3', 'o2s3', 'o3s3', 'o4s3', 'o5s3', 'o6s3', 'o7s3', 'o8s3')
mot_survey_1 = mot_survey_1 %>% filter(email!='') %>% filter(!is.na(o1s1)) %>% filter(o1s1!='') %>% select(-datetime)
mot_survey_2 = mot_survey_2 %>% filter(email!='') %>% filter(!is.na(o1s2)) %>% filter(o1s2!='') %>% select(-datetime)
mot_survey_3 = mot_survey_3 %>% filter(email!='') %>% filter(!is.na(o1s3)) %>% filter(o1s3!='') %>% select(-datetime)
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

# Analyze how positive are participants' opinions
for (i in c(2:6)) {
  t_test = t.test(survey_1[,i],mu=3.5,alternative='greater')
  print(paste('Option: ',i-1,' p-value: ',t_test$p.value,sep=''))
  
}

# Draw a boxplot
ggplot(melt_survey_1, aes(x=variable, y=value, fill=variable)) + 
  geom_boxplot() + 
  scale_y_continuous(breaks=c(0:8)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=16, color='black'), 
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=17), axis.title.y = element_text(size=15),
        strip.text.x = element_text(size=18, face="bold")) + 
  labs(y="Scores") +
  guides(fill=FALSE)

# Analyze correlation between level of trust in the process and motivation factors (survey 1)
mot_survey_1 = filter(mot_survey_1, email %in% survey_1$email)
survey_1 = filter(survey_1, email %in% mot_survey_1$email)
for (factor in c('o1s1', 'o2s1', 'o3s1', 'o4s1', 'o5s1', 'o6s1', 'o7s1')) {
  for (option in c('Q9_7','Q9_8','Q9_9','Q9_10','Q9_12')) {
    ret = prepare_data(mot_survey_1, survey_1, factor, option)
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

# Analyze correlation between level of trust in the process and motivation factors (survey 2)
survey_1 = raw_survey_1 %>% select(email,Q9_7,Q9_8,Q9_9,Q9_10,Q9_12) %>% filter(!is.na(Q9_7))
survey_1 = filter(survey_1, email %in% mot_survey_2$email)
mot_survey_2 = filter(mot_survey_2, email %in% survey_1$email)
for (factor in c('o1s2', 'o2s2', 'o3s2', 'o4s2', 'o5s2', 'o6s2', 'o7s2', 'o8s2')) {
  for (option in c('Q9_7','Q9_8','Q9_9','Q9_10','Q9_12')) {
    ret = prepare_data(mot_survey_2, survey_1, factor, option)
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

# Analyze correlation between level of trust in the process and motivation factors (survey 3)
survey_1 = raw_survey_1 %>% select(email,Q9_7,Q9_8,Q9_9,Q9_10,Q9_12) %>% filter(!is.na(Q9_7))
mot_survey_3 = filter(mot_survey_3, email %in% survey_1$email)
survey_1 = filter(survey_1, email %in% mot_survey_3$email)
for (factor in c('o1s3', 'o2s3', 'o3s3', 'o4s3', 'o5s3', 'o6s3', 'o7s3', 'o8s3')) {
  for (option in c('Q9_7','Q9_8','Q9_9','Q9_10','Q9_12')) {
    ret = prepare_data(mot_survey_3, survey_1, factor, option)
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

# Analyze correlation between trust and self-efficacy (survey 1)
se_survey_1 = raw_survey_1 %>% select(email,Q3_1,Q3_2,Q4_1,Q4_2,Q4_3,Q4_4,Q4_5,Q5_1,Q5_3,Q5_4,Q5_5,Q5_6,Q5_7,Q5_14) %>% filter(!is.na(Q3_1))
survey_1 = raw_survey_1 %>% select(email,Q9_7,Q9_8,Q9_9,Q9_10,Q9_12) %>% filter(!is.na(Q9_7))
survey_1_set = merge(survey_1,se_survey_1,by.x='email',by.y='email')
se_survey_1 = filter(se_survey_1, email %in% survey_1_set$email)
survey_1 = filter(survey_1, email %in% se_survey_1$email)
for (factor in c('Q3_1','Q3_2','Q4_1','Q4_2','Q4_3','Q4_4','Q4_5','Q5_1','Q5_3','Q5_4','Q5_5','Q5_6',
                 'Q5_7','Q5_14')) {
  for (option in c('Q9_7','Q9_8','Q9_9','Q9_10','Q9_12')) {
    ret = prepare_data(se_survey_1, survey_1, factor, option)
    cor_coef = cor(ret$ds1,ret$ds2)
    print(paste('The correlation between ',factor,' and ',option,' is ',cor_coef,sep=''))
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

# Analyze correlation between trust and self-efficacy (survey 3)
se_survey_3 = raw_survey_3 %>% select(email,Q3_2,Q6_4,Q6_5,Q7_2) %>% filter(!is.na(Q3_2))
survey_1 = raw_survey_1 %>% select(email,Q9_7,Q9_8,Q9_9,Q9_10,Q9_12) %>% filter(!is.na(Q9_7))
survey_3_set = merge(survey_1,se_survey_3,by.x='email',by.y='email')
se_survey_3 = filter(se_survey_3, email %in% survey_3_set$email)
survey_1 = filter(survey_1, email %in% se_survey_3$email)
for (factor in c('Q3_2','Q6_4','Q6_5','Q7_2')) {
  for (option in c('Q9_7','Q9_8','Q9_9','Q9_10','Q9_12')) {
    ret = prepare_data(se_survey_3, survey_1, factor, option)
    cor_coef = cor(ret$ds1,ret$ds2)
    print(paste('The correlation between ',factor,' and ',option,' is ',cor_coef,sep=''))
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

# Analyze correlation between trust and learning (survey 1)
le_survey_1 = raw_survey_1 %>% select(email,Q6_3,Q6_4,Q6_5) %>% filter(!is.na(Q6_3))
survey_1 = raw_survey_1 %>% select(email,Q9_7,Q9_8,Q9_9,Q9_10,Q9_12) %>% filter(!is.na(Q9_7))
le_merged = merge(survey_1,le_survey_1,by.x='email',by.y='email')
le_survey_1 = filter(le_survey_1, email %in% le_merged$email)
survey_1 = filter(survey_1, email %in% le_merged$email)
for (factor in c('Q6_3','Q6_4','Q6_5')) {
  for (option in c('Q9_7','Q9_8','Q9_9','Q9_10','Q9_12')) {
    ret = prepare_data(le_survey_1, survey_1, factor, option)
    cor_coef = cor(ret$ds1,ret$ds2)
    print(paste('The correlation between ',factor,' and ',option,' is ',cor_coef,sep=''))
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

# Analyze correlation between trust and learning (survey 2)
le_survey_2 = raw_survey_2 %>% select(email,Q11_6,Q11_7,Q11_8) %>% filter(!is.na(Q11_6))
survey_1 = raw_survey_1 %>% select(email,Q9_7,Q9_8,Q9_9,Q9_10,Q9_12) %>% filter(!is.na(Q9_7))
le_merged = merge(survey_1,le_survey_2,by.x='email',by.y='email')
le_survey_2 = filter(le_survey_2, email %in% le_merged$email)
survey_1 = filter(survey_1, email %in% le_merged$email)
for (factor in c('Q11_6','Q11_7','Q11_8')) {
  for (option in c('Q9_7','Q9_8','Q9_9','Q9_10','Q9_12')) {
    ret = prepare_data(le_survey_2, survey_1, factor, option)
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
survey_1 = raw_survey_1 %>% select(email,Q9_7,Q9_8,Q9_9,Q9_10,Q9_12) %>% filter(!is.na(Q9_7))
le_merged = merge(survey_1,le_survey_3,by.x='email',by.y='email')
le_survey_3 = filter(le_survey_3, email %in% le_merged$email)
survey_1 = filter(survey_1, email %in% le_merged$email)
for (factor in c('Q11_6','Q11_7','Q11_8')) {
  for (option in c('Q9_7','Q9_8','Q9_9','Q9_10','Q9_12')) {
    ret = prepare_data(le_survey_3, survey_1, factor, option)
    cor_coef = cor(ret$ds1,ret$ds2)
    print(paste('The correlation between ',factor,' and ',option,' is ',cor_coef,sep=''))
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
survey_1 = raw_survey_1 %>% select(email,Q9_7,Q9_8,Q9_9,Q9_10,Q9_12) %>% filter(!is.na(Q9_7))
participation_s1s2 = mutate(participation_s1s2,content=ideas+votes+comments)
participation_s1s2 = mutate(participation_s1s2,ideas_votes=ideas+votes)
participation_s1s2 = mutate(participation_s1s2,ideas_comments=ideas+comments)
participation_s1s2 = mutate(participation_s1s2,votes_comments=votes+comments)
participation_s1s2 = filter(participation_s1s2, email %in% survey_1$email)
for (type_participation in c('content', 'ideas_votes', 'ideas_comments', 'votes_comments', 'votes', 'ideas', 'comments')) {
  for (option in c('Q9_7','Q9_8','Q9_9','Q9_10','Q9_12')) {
    ret = prepare_data(participation_s1s2, survey_1, type_participation, option)
    cor_coef = cor(ret$ds1,ret$ds2)
    print(paste('The correlation between ',type_participation,' and the option ',option,' is ',cor_coef,sep=''))
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

# Analyze correlation between changes in participants' opinions and level of participations (survey 3)
survey_1 = raw_survey_1 %>% select(email,Q9_7,Q9_8,Q9_9,Q9_10,Q9_12) %>% filter(!is.na(Q9_7))
participation_s3 = filter(participation_s3, email %in% survey_1$email)
participation_s3 = mutate(participation_s3,content=ideas+votes+comments)
participation_s3 = mutate(participation_s3,ideas_votes=ideas+votes)
participation_s3 = mutate(participation_s3,ideas_comments=ideas+comments)
participation_s3 = mutate(participation_s3,votes_comments=votes+comments)
for (type_participation in c('content', 'ideas_votes', 'ideas_comments', 'votes_comments', 'votes', 'ideas', 'comments')) {
  for (option in c('Q9_7','Q9_8','Q9_9','Q9_10','Q9_12')) {
    ret = prepare_data(participation_s3, survey_1, type_participation, option)
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

# Search correlations between trust in the law-making process and changes in the motivation factors
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
survey_1_ = survey_1 %>% filter(email %in% ch_mot_factors$email)
t_options = c('Q9_7','Q9_8','Q9_9','Q9_10','Q9_12')
find_correlations(ch_mot_factors,survey_1_,ch_factors,t_options)
