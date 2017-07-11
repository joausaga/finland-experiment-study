####
## This script tries to discover
## factors that
## differentiate those 65 people 
## who stayed throughout the process 
## and those who dropped out
####

library(dplyr)
library(likert)
library(BSDA)
source('utils.R')

#### 
## Check evolution of factors among those who rated high 
## in self-efficacy
####
check_evolution = function(dataset, col_names) {
  f_dataset = dataset
  num_col = ncol(dataset)
  f_dataset[,c(2:num_col)] = lapply(f_dataset[,c(2:num_col)], 
                                    function(x) factor(x, levels=c('1','2','3','4','5','6','7'), 
                                                ordered=T))
  for (i in 4:ncol(dataset)) {
    if (i %in% c(5,6,9,13)) {
      sub = f_dataset[f_dataset[,i]<4,c(1:3)]
    } else {
      sub = f_dataset[f_dataset[,i]>4,c(1:3)] 
    }
    # Melting data
    m_sub = melt(sub, id.vars='email', measure.vars=col_names)
    m_sub$value = as.integer(m_sub$value)
    colnames(m_sub) = c('email','factor','score')
    m_sub$survey = unlist(lapply(m_sub$factor, 
                                 function(x) ifelse(regexpr('.s2', x)[1]!=-1,'s2','s3')))
    w_test = wilcox.test(score ~ survey, data=m_sub, paired = TRUE)
    print(paste('Column idx:',i,'rows:',nrow(sub),'p-value',w_test$p.value))
  }
}



# Reading survey data
survey_1 = read.csv("./data/survey1-condlaw-complete.csv",header=TRUE,sep=",",stringsAsFactors=F)
survey_2 = read.csv("./data/survey2-condlaw-complete.csv",header=T, sep=",",stringsAsFactors=F)
survey_3 = read.csv("./data/survey3-condlaw2-complete.csv",header=T, sep=",",stringsAsFactors=F)

# Reading activity data
ideas_1_stage = read.csv("./data/activity logs/idea_log_1_stage.csv", sep=",", header=T, stringsAsFactors=FALSE)
ideas_2_stage = read.csv("./data/activity logs/idea_log_2_stage.csv", sep=",", header=T, stringsAsFactors=FALSE)
comments_1_stage = read.csv("./data/activity logs/comment_log_1_stage.csv", sep=",", header=T, stringsAsFactors=FALSE)
comments_2_stage = read.csv("./data/activity logs/comment_log_2_stage.csv", sep=",", header=T, stringsAsFactors=FALSE)
votes_1_stage = read.csv("./data/activity logs/vote_log_1_stage_new.csv", sep=",", header=T, stringsAsFactors=FALSE)
votes_2_stage = read.csv("./data/activity logs/vote_log_2_stage_new.csv", sep=",", header=T, stringsAsFactors=FALSE)

# Data processing and transformation
ideas_1_stage$creation_datetime = as.POSIXct(ideas_1_stage$creation_datetime, 
                                             format="%d/%m/%y %H:%M")
ideas_2_stage$creation_datetime = as.POSIXct(ideas_2_stage$creation_datetime, 
                                             format="%d/%m/%y %H:%M")
comments_1_stage$creation_datetime = as.POSIXct(comments_1_stage$creation_datetime, 
                                                format="%d/%m/%y %H:%M")
comments_2_stage$creation_datetime = as.POSIXct(comments_2_stage$creation_datetime, 
                                                format="%d/%m/%y %H:%M")
votes_1_stage$creation_datetime = as.POSIXct(votes_1_stage$creation_datetime, 
                                             format="%d/%m/%y %H:%M")
votes_2_stage$creation_datetime = as.POSIXct(votes_2_stage$creation_datetime, 
                                             format="%d/%m/%y %H:%M")

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

# Analyze demographic characteristic of the 65

# Age
age = respondents[,c('email','Q12.s3')]
age_table = table(age$Q12.s3)
prop.table(age_table)
# Age distribution
#  5         6         7 
# 0.1230769 0.2923077 0.5846154 
# Almost 60% consisted of individuals over 65 years old
# The group is older than the whole group of survey 
# respondents but the age distribution follows the pattern 
# of the entire group of survey respondents

# Gender
gender = respondents[,c('email','Q13.s3')]
gen_table = table(gender$Q13.s3)
prop.table(gen_table)
# Gender distribution
#  1         2 
# 0.4307692 0.5692308
# There are less males but the distribution
# follows more or less the pattern of the entire 
# group of survey respondents

# Occupational Education
o_edu = respondents[,c('email','Q17.s3')]
round(100*prop.table(table(o_edu$Q17.s3)),2)
# Distribution (%)
#  2     3     4     5     6     7     8     9 
# 13.85 23.08  6.15  6.15 15.38 26.15  3.08  6.15 
# Similar distribution than the entire group
# of respondents

# Basic Education
b_edu = respondents[,c('email','Q16.s3')]
round(100*prop.table(table(b_edu$Q16.s3)),2)
# Distribution (%)
#  1     2     3     4     5 
# 10.77 20.00  1.54 66.15  1.54
# The distribution is quite similar to
# the entire group of survey respondents

# Current Situation
c_situ = respondents[,c('email','Q15.s3')]
round(100*prop.table(table(c_situ$Q15.s3)),2)
# Distribution (%)
# 1     2     3     4     5     7     8 
# 23.08  1.54  4.62  1.54  7.69 55.38  6.15
# Similar distribution than the entire group
# of respondents

# Working Position
w_pos = respondents[,c('email','Q14.s3')]
sum(w_pos$Q14.s3==2)/nrow(w_pos)
# About 15% work as managers similar to the 18% of
# the entire group of survey respondents

# Role in housing companies
rol = respondents[,c('email','Q8_4.s3','Q8_5.s3','Q8_6.s3',
                     'Q8_7.s3','Q8_8.s3','Q8_9.s3','Q8_10.s3')]
sum(!is.na(rol$Q8_6.s3))/nrow(rol)  # Member of their housing company board
sum(!is.na(rol$Q8_4.s3))/nrow(rol)  # Condominium owner
sum(!is.na(rol$Q8_7.s3))/nrow(rol)  # Condominium manager
# 46% are or were member of their housing company board similar to the 47%
# of the entire group of survey respondents

# Civic Activity
civ = respondents[,c('email','Q10_4.s3','Q10_5.s3','Q10_6.s3','Q10_7.s3',
                     'Q10_8.s3','Q10_9.s3','Q10_10.s3','Q10_11.s3',
                     'Q10_12.s3','Q10_13.s3','Q10_14.s3','Q10_15.s3',
                     'Q10_16.s3','Q10_17.s3','Q10_18.s3','Q10_19.s3')]
sum(!is.na(civ$Q10_4.s3))/nrow(civ) # 95% voted in elections (87% entire group)
sum(!is.na(civ$Q10_5.s3))/nrow(civ) # 37% written an op-ed (46% entire group)
sum(!is.na(civ$Q10_6.s3)|!is.na(civ$Q10_7.s3))/nrow(civ) # 57% expressed opinion online (47%)
sum(!is.na(civ$Q10_8.s3))/nrow(civ) # 37% contacted MP (29% entire group)
sum(!is.na(civ$Q10_9.s3))/nrow(civ) # 34% contacted local authority (28% entire group)
sum(!is.na(civ$Q10_10.s3))/nrow(civ) # 5% participated in council meeting
sum(!is.na(civ$Q10_11.s3))/nrow(civ) # 1% participated in municipal committee meeting
sum(!is.na(civ$Q10_12.s3))/nrow(civ) # 45% signed petition
sum(!is.na(civ$Q10_13.s3))/nrow(civ) # 3% initiated petition
sum(!is.na(civ$Q10_14.s3))/nrow(civ) # 9% participated in protests
sum(!is.na(civ$Q10_15.s3))/nrow(civ) # 48% volunteered (43% entire group)
sum(!is.na(civ$Q10_16.s3))/nrow(civ) # 1% local candiate
sum(!is.na(civ$Q10_17.s3))/nrow(civ) # 0% parliament candiate
sum(!is.na(civ$Q10_18.s3))/nrow(civ) # 8% worked on electoral campaigns
sum(!is.na(civ$Q10_19.s3))/nrow(civ) # 3% none


# Check whether the differences in the demographic variable are significant
# Age
rest_age = survey_3[,c('email','Q12.s3')] %>% filter(!is.na(Q12.s3)) %>% filter(!email %in% respondents$email)
rest_age$group = 'rest'
age$group = '65_res'
all_age = rbind(age,rest_age)
age_table = table(all_age$group,all_age$Q12.s3)
chisq.test(all_age$group,all_age$Q12.s3,correct=F)
# The difference in the age distribution is not significant

# Gender
rest_gender = survey_3[,c('email','Q13.s3')] %>% filter(!is.na(Q13.s3)) %>% filter(!email %in% respondents$email)
rest_gender$group = 'rest'
gender$group = '65_res'
all_gender = rbind(gender,rest_gender)
gender_table = table(all_gender$group,all_gender$Q13.s3)
chisq.test(all_gender$group,all_gender$Q13.s3,correct=F)
# The difference in the gender distribution is not significant

# Civic activity
rest_civ = survey_3[,c('email','Q10_4.s3','Q10_5.s3','Q10_6.s3','Q10_7.s3',
                       'Q10_8.s3','Q10_9.s3','Q10_10.s3','Q10_11.s3',
                       'Q10_12.s3','Q10_13.s3','Q10_14.s3','Q10_15.s3',
                       'Q10_16.s3','Q10_17.s3','Q10_18.s3','Q10_19.s3')]
rest_civ = rest_civ %>% filter(!email %in% respondents$email)
rest_civ$group = 'rest'
civ$group = '65_res'

# Voting
all_civ = rbind(rest_civ[,c('email','Q10_4.s3','group')],civ[,c('email','Q10_4.s3','group')])
all_civ$Q10_4.s3 = ifelse(is.na(all_civ$Q10_4.s3),'2',all_civ$Q10_4.s3) 
civ_table = table(all_civ$group,all_civ$Q10_4.s3)
chisq.test(all_civ$group,all_civ$Q10_4.s3,correct=F)
# Output: Pearson's Chi-squared test
# data:  all_civ$group and all_civ$Q10_4.s3
# X-squared = 4.175, df = 1, p-value = 0.04102
# The difference is significant

# Express opinion online
all_civ = rbind(rest_civ[,c('email','Q10_6.s3','group')], civ[,c('email','Q10_6.s3','group')])
all_civ = list()
for (i in 1:nrow(rest_civ)) {
  if (!is.na(rest_civ[i,'Q10_6.s3']) | !is.na(rest_civ[i,'Q10_7.s3'])) {
    all_civ = rbind(all_civ,c(rest_civ[i,'email'],'1',rest_civ[i,'group']))
  } 
  else {
    all_civ = rbind(all_civ,c(rest_civ[i,'email'],'0',rest_civ[i,'group']))
  }
  
}
for (i in 1:nrow(civ)) {
  if (!is.na(civ[i,'Q10_6.s3']) | !is.na(civ[i,'Q10_7.s3'])) {
    all_civ = rbind(all_civ,c(civ[i,'email'],'1',civ[i,'group']))
  } 
  else {
    all_civ = rbind(all_civ,c(civ[i,'email'],'0',civ[i,'group']))
  }
  
}
all_civ = data.frame(matrix(unlist(all_civ), nrow=nrow(all_civ)),stringsAsFactors=FALSE)
colnames(all_civ) = c('email','online','group')
civ_table = table(all_civ$group,all_civ$online)
chisq.test(all_civ$group,all_civ$online,correct=F)
# Output: Pearson's Chi-squared test
# data:  all_civ$group and all_civ$online
# X-squared = 2.5458, df = 1, p-value = 0.1106

# Contacted MP
all_civ = rbind(rest_civ[,c('email','Q10_8.s3','group')],civ[,c('email','Q10_8.s3','group')])
all_civ$Q10_8.s3 = ifelse(is.na(all_civ$Q10_8.s3),'2',all_civ$Q10_8.s3) 
civ_table = table(all_civ$group,all_civ$Q10_8.s3)
chisq.test(all_civ$group,all_civ$Q10_8.s3,correct=F)
# Output: Pearson's Chi-squared test
# data:  all_civ$group and all_civ$Q10_8.s3
# X-squared = 1.0372, df = 1, p-value = 0.3085

# Contacted Local Authority
all_civ = rbind(rest_civ[,c('email','Q10_9.s3','group')],civ[,c('email','Q10_9.s3','group')])
all_civ$Q10_9.s3 = ifelse(is.na(all_civ$Q10_9.s3),'2',all_civ$Q10_9.s3) 
civ_table = table(all_civ$group,all_civ$Q10_9.s3)
chisq.test(all_civ$group,all_civ$Q10_9.s3,correct=F)
# Output: Pearson's Chi-squared test
# data:  all_civ$group and all_civ$Q10_9.s3
# X-squared = 1.0534, df = 1, p-value = 0.3047


# Analyze difference between civic duty of 65 against civic duty of the rest
rest_civic_duty = survey_3[,c('email','Q2_9.s3')] %>% filter(!is.na(Q2_9.s3)) %>% filter(!email %in% respondents$email)
rest_civic_duty$group = 'rest'
civic_duty = respondents[,c('email','Q2_9.s3')] %>% filter(!is.na(Q2_9.s3))
civic_duty$group = '65_res'
all_civic_duty = rbind(rest_civic_duty[,c('email','Q2_9.s3','group')],civic_duty[,c('email','Q2_9.s3','group')])
wilcox.test(Q2_9.s3 ~ group, data=all_civic_duty)
mean(filter(all_civic_duty, group=='rest')$Q2_9.s3)
mean(filter(all_civic_duty, group=='65_res')$Q2_9.s3)
# Output: Wilcoxon rank sum test with continuity correction
# data:  Q2_9.s3 by group
# W = 4700, p-value = 0.1558


######
## Self-efficacy and expectations of the 65 people
## who replied to the three surveys
######
selfefficacy = select(respondents, email, Q3_1.s1,Q3_2.s1,Q4_1.s1,Q4_2.s1,Q4_3.s1,Q4_4.s1,Q4_5.s1,
                      Q5_1.s1,Q5_3.s1,Q5_4.s1,Q5_5.s1,Q5_6.s1,Q5_7.s1,Q5_14.s1,Q3_2.s3,
                      Q6_4.s3, Q6_5.s3,Q7_2.s3,Q7_4.s3,Q7_6.s3)
f_selfefficacy = selfefficacy
f_selfefficacy[,c(2:21)] = lapply(f_selfefficacy[,c(2:21)], 
                                  function(x) factor(x, levels=c('1','2','3','4','5','6','7'), ordered=T))
ret = likert(f_selfefficacy[,c(2:21)])
summary(ret)
plot(ret, type="bar", title='Self-efficacy (N=65)')
f_selfefficacy$group = '65_res'

res_selfefficacy = merge(
  select(survey_1, email, Q3_1.s1,Q3_2.s1,Q4_1.s1,Q4_2.s1,
         Q4_3.s1,Q4_4.s1,Q4_5.s1,Q5_1.s1,Q5_3.s1,Q5_4.s1,
         Q5_5.s1,Q5_6.s1,Q5_7.s1,Q5_14.s1),
  select(survey_3, email, Q3_2.s3,Q6_4.s3,
         Q6_5.s3,Q7_2.s3,Q7_4.s3,Q7_6.s3),
  by.x='email',
  by.y='email',
  all=T
)
res_selfefficacy = filter(res_selfefficacy, !email %in% selfefficacy$email)
f_res_selfefficacy = res_selfefficacy
f_res_selfefficacy[,c(2:21)] = lapply(f_res_selfefficacy[,c(2:21)], 
                                      function(x) factor(x, levels=c('1','2','3','4','5','6','7'), 
                                                         ordered=T))
f_res_selfefficacy$group = 'rest'
ret_res = likert(f_res_selfefficacy[,c(2:19)])
summary(ret_res)
plot(ret_res, type="bar")
# Comparison with the rest of the survey respondents
all_selfefficacy = rbind(f_res_selfefficacy,f_selfefficacy)
all_selfefficacy$group = factor(all_selfefficacy$group, levels=c('65_res','rest'))
likert_se = likert(all_selfefficacy[,2:21], grouping=all_selfefficacy$group)
summary(likert_se)
plot(likert_se, type="bar") + 
  ggtitle('Comparison of self-efficacy and expectations between the 65 that replied to the three surveys and the rest')
# Dig deep to see if there are significant difference in self-efficacy and expectations
# factors between the group of the 65 people that responded to the three surveys and
# the rest
n_all_selfefficacy = rbind(
  mutate(selfefficacy,group='65_res'),
  mutate(res_selfefficacy,group='rest')
)
for (colname in colnames(n_all_selfefficacy[2:21])) {
  ret = wilcox.test(n_all_selfefficacy[n_all_selfefficacy$group=='65_res',colname], 
                    n_all_selfefficacy[n_all_selfefficacy$group=='rest',colname])  
  print('------')
  print(paste('Column:',colname))
  print(paste('Wilcoxon W:',ret$statistic))
  print(paste('p-value:',ret$p.value))
}

# Columns with significant differences
# "Column: Q3_2.s3, Wilcoxon W: 6138.5, p-value: 0.0299531704354645"
# "Column: Q6_5.s3, Wilcoxon W: 4029, p-value: 0.0369720097028056"
# Draw chart of these two columns
selfefficacy_sig = all_selfefficacy[,c('email','Q3_2.s3','Q6_5.s3','group')]
colnames(selfefficacy_sig) = c('email','In general I hold strong opinions on anything',
                               'My knowledge on off-road traffic was better than that of other participants on the platform',
                               'group')
likert_se = likert(selfefficacy_sig[,c(2:3)], grouping=selfefficacy_sig$group)
summary(likert_se)
plot(likert_se, type="bar") + 
  ggtitle('Self-efficacy and expectations of the 65 that replied to the three surveys and the rest (only significant)')
# Calculate effect size for columns with significant difference
wilcoxsign_test(Q3_2.s3 ~ group, data=all_selfefficacy,distribution="exact")
12.769/sqrt(225)  # 65 + 160 (n of rest group exclusing NAs)
wilcoxsign_test(Q6_5.s3 ~ group, data=all_selfefficacy,distribution="exact")
12.806/sqrt(225)  # 65 + 160 (n of rest group exclusing NAs)

####
## Follow-up of the previous analysis
## but only with the participants who
## replied to survey 3
####
res_selfefficacy = select(survey_3, email, Q3_2.s3, Q6_5.s3)
res_selfefficacy = filter(res_selfefficacy, !email %in% selfefficacy$email)
selfefficacy = select(selfefficacy, email, Q3_2.s3, Q6_5.s3)
wilcox.test(res_selfefficacy$Q3_2.s3, selfefficacy$Q3_2.s3) 
wilcox.test(res_selfefficacy$Q6_5.s3, selfefficacy$Q6_5.s3)
all_efficacy = rbind(
  mutate(selfefficacy, group='65_res'),
  mutate(res_selfefficacy, group='rest')
)
all_efficacy$Q3_2.s3 = as.numeric(all_efficacy$Q3_2.s3)
all_efficacy$Q6_5.s3 = as.numeric(all_efficacy$Q6_5.s3)
all_efficacy$group = as.factor(all_efficacy$group)
# Calculate effect size for columns with significant difference
wilcox_test(Q3_2.s3 ~ group, data=all_efficacy,distribution="exact")
2.1719/sqrt(nrow(all_efficacy))
wilcox_test(Q6_5.s3 ~ group, data=all_efficacy,distribution="exact")
2.0874/sqrt(nrow(all_efficacy))


####
## Evolution of a self-efficacy factor
####
# The 65
selfefficacy = select(respondents, email, Q2_4.s2, Q6_2.s3)
f_selfefficacy = selfefficacy
f_selfefficacy[,c(2:ncol(f_selfefficacy))] = lapply(f_selfefficacy[,c(2:ncol(f_selfefficacy))], 
                                  function(x) factor(x, levels=c('1','2','3','4','5','6','7'), 
                                                     ordered=T))
# Melting data
m_f_selfefficacy = melt(f_selfefficacy, id.vars='email', 
                       measure.vars=c('Q2_4.s2','Q6_2.s3'))
m_f_selfefficacy$value = as.integer(m_f_selfefficacy$value)
colnames(m_f_selfefficacy) = c('email','factor','score')
m_f_selfefficacy$survey = unlist(lapply(m_f_selfefficacy$factor, 
                                        function(x) ifelse(regexpr('.s2', x)[1]!=-1,'s2','s3')))
wilcox.test(score ~ survey, data=m_f_selfefficacy, paired = TRUE)
# Output:
# V = 505.5, p-value = 0.8952
##
##
# The rest
res_selfefficacy = merge(
  select(survey_2, email, Q2_4.s2),
  select(survey_3, email, Q6_2.s3),
  by.x='email',
  by.y='email',
  all=T
)
res_selfefficacy = filter(res_selfefficacy, !email %in% selfefficacy$email)
res_selfefficacy = filter(res_selfefficacy, !is.na(Q2_4.s2) & !is.na(Q6_2.s3))
## Cannot check the evolution of the factor since only one participant replied 
## to both survey 2 and survey 3
# Check evolution among those who rated high in self-efficacy
selfefficacy = select(respondents, email, Q2_4.s2, Q6_2.s3, Q4_2.s1, Q4_3.s1, 
                      Q4_4.s1, Q4_5.s1, Q5_1.s1, Q5_3.s1, Q5_4.s1, Q5_5.s1, Q5_6.s1,
                      Q5_7.s1, Q5_14.s1)
res_selfefficacy = merge(
  select(survey_1, email, Q4_2.s1, Q4_3.s1, 
         Q4_4.s1, Q4_5.s1, Q5_1.s1, Q5_3.s1, Q5_4.s1, Q5_5.s1, Q5_6.s1,
         Q5_7.s1, Q5_14.s1),
  select(survey_2, email, Q2_4.s2),
  by.x='email',
  by.y='email',
  all=T
)
res_selfefficacy = merge(
  res_selfefficacy,
  select(survey_3, email, Q6_2.s3),
  by.x='email',
  by.y='email',
  all=T
)
res_selfefficacy = filter(res_selfefficacy, !email %in% selfefficacy$email)
res_selfefficacy = filter(res_selfefficacy, !is.na(Q2_4.s2) & !is.na(Q6_2.s3))
selfefficacy = rbind(selfefficacy,res_selfefficacy)
check_evolution(selfefficacy,c('Q2_4.s2','Q6_2.s3'))


####
## Evolution of an expectation factor
####
expectation = select(respondents, email, Q2_5.s2, Q6_5.s3)
f_expectation = expectation
num_col = ncol(f_expectation)
f_expectation[,c(2:num_col)] = lapply(f_expectation[,c(2:num_col)], 
                                                    function(x) factor(x, levels=c('1','2','3','4','5','6','7'), 
                                                                       ordered=T))
# Melting data
m_f_expectation = melt(f_expectation, id.vars='email', measure.vars=c('Q2_5.s2','Q6_5.s3'))
m_f_expectation$value = as.integer(m_f_expectation$value)
colnames(m_f_expectation) = c('email','factor','score')
m_f_expectation$survey = unlist(lapply(m_f_expectation$factor, 
                                      function(x) ifelse(regexpr('.s2', x)[1]!=-1,'s2','s3')))
wilcox.test(score ~ survey, data=m_f_expectation, paired = TRUE)
# Output:
# V = 188.5, p-value = 0.5249
# Check evolution among those who rated high in self-efficacy
expectation = select(respondents, email, Q2_5.s2, Q6_5.s3, Q4_2.s1, Q4_3.s1, 
                     Q4_4.s1, Q4_5.s1, Q5_1.s1, Q5_3.s1, Q5_4.s1, Q5_5.s1, Q5_6.s1,
                     Q5_7.s1, Q5_14.s1)
res_expectation = merge(
  select(survey_1, email, Q4_2.s1, Q4_3.s1, 
         Q4_4.s1, Q4_5.s1, Q5_1.s1, Q5_3.s1, Q5_4.s1, Q5_5.s1, Q5_6.s1,
         Q5_7.s1, Q5_14.s1),
  select(survey_2, email, Q2_5.s2),
  by.x='email',
  by.y='email',
  all=T
)
res_expectation = merge(
  res_expectation,
  select(survey_3, email, Q6_5.s3),
  by.x='email',
  by.y='email',
  all=T
)
res_expectation = filter(res_expectation, !email %in% expectation$email)
res_expectation = filter(res_expectation, !is.na(Q2_5.s2) & !is.na(Q6_5.s3))
expectation = rbind(expectation,res_expectation)
check_evolution(expectation,c('Q2_5.s2','Q6_5.s3'))



#####
## Learning
#####
learning = select(respondents,email,Q3_1.s3,Q3_3.s3,Q3_4.s3,Q6_1.s3,
                  Q2_1.s2,Q3_1.s2,Q3_2.s2,Q3_3.s2,Q3_4.s2)
f_learning = learning
f_learning[,c(2:10)] = lapply(f_learning[,c(2:10)], 
                              function(x) factor(x, levels=c('1','2','3','4','5','6','7'), ordered=T))
f_learning$group = '65_res'
res_learning = merge(
  select(survey_2, email, Q2_1.s2,Q3_1.s2,Q3_2.s2,Q3_3.s2,Q3_4.s2),
  select(survey_3, email, Q3_1.s3,Q3_3.s3,Q3_4.s3,Q6_1.s3),
  by.x='email',
  by.y='email',
  all=T
)
res_learning = filter(res_learning, !email %in% learning$email)
f_res_learning = res_learning
f_res_learning[,c(2:10)] = lapply(f_res_learning[,c(2:10)], 
                                  function(x) factor(x, levels=c('1','2','3','4','5','6','7'), 
                                                     ordered=T))
f_res_learning$group = 'rest'
# Comparison
n_all_learning = rbind(
  mutate(learning,group='65_res'),
  mutate(res_learning,group='rest')
)
for (colname in colnames(n_all_learning[2:10])) {
  ret = wilcox.test(n_all_learning[n_all_learning$group=='65_res',colname], 
                    n_all_learning[n_all_learning$group=='rest',colname])  
  print('------')
  print(paste('Column:',colname))
  print(paste('p-value:',ret$p.value))
}
# There aren't columns with significant differences



# Learning only for the group of participants
# who replied to survey 3
learning_s3 = select(respondents,email,Q3_1.s3,Q3_2.s3,Q3_3.s3,Q3_4.s3, Q6_1.s3)
f_learning_s3 = learning_s3
f_learning_s3[,c(2:6)] = lapply(f_learning_s3[,c(2:6)], 
                              function(x) factor(x, levels=c('1','2','3','4','5','6','7'), ordered=T))
summary(likert(f_learning_s3[,c(2:6)]))
chisq.test(table(f_learning_s3$Q3_2.s3,f_learning_s3$Q6_1.s3)[,-1])
chisq.test(table(f_learning_s3$Q3_2.s3,f_learning_s3$Q3_1.s3))
chisq.test(table(f_learning_s3$Q3_2.s3,f_learning_s3$Q3_3.s3))
chisq.test(table(f_learning_s3$Q3_2.s3,f_learning_s3$Q3_4.s3))


####
## Evolution of a learning factor
####
learning = select(respondents,email,Q6_1.s3,Q2_1.s2)
f_learning = learning
num_col = ncol(f_learning)
f_learning[,c(2:num_col)] = lapply(f_learning[,c(2:num_col)], 
                              function(x) factor(x, levels=c('1','2','3','4','5','6','7'), ordered=T))
# Melting data
m_f_learning = melt(f_learning, id.vars='email', 
                    measure.vars=c('Q6_1.s3','Q2_1.s2'))
m_f_learning$value = as.integer(m_f_learning$value)
colnames(m_f_learning) = c('email','factor','score')
m_f_learning$survey = unlist(lapply(m_f_learning$factor, 
                                    function(x) ifelse(regexpr('.s2', x)[1]!=-1,'s2','s3')))
wilcox.test(score ~ survey, data=m_f_learning, paired = TRUE)
# Output:
# V = 460, p-value = 0.4886
# The rest
res_learning = merge(
  select(survey_2, email, Q2_1.s2),
  select(survey_3, email, Q6_1.s3),
  by.x='email',
  by.y='email',
  all=T
)
res_learning = filter(res_learning, !email %in% learning$email)
res_learning = filter(res_learning, !is.na(Q2_1.s2) & !is.na(Q6_1.s3))
## Cannot check the evolution of the factor since only one participant replied 
## to both survey 2 and survey 3
# Check evolution among those who rated high in self-efficacy
learning = select(respondents, email, Q2_1.s2, Q6_1.s3, Q4_2.s1, Q4_3.s1, 
                     Q4_4.s1, Q4_5.s1, Q5_1.s1, Q5_3.s1, Q5_4.s1, Q5_5.s1, Q5_6.s1,
                     Q5_7.s1, Q5_14.s1)
res_learning = merge(
  select(survey_1, email, Q4_2.s1, Q4_3.s1, 
         Q4_4.s1, Q4_5.s1, Q5_1.s1, Q5_3.s1, Q5_4.s1, Q5_5.s1, Q5_6.s1,
         Q5_7.s1, Q5_14.s1),
  select(survey_2, email, Q2_1.s2),
  by.x='email',
  by.y='email',
  all=T
)
res_learning = merge(
  res_learning,
  select(survey_3, email, Q6_1.s3),
  by.x='email',
  by.y='email',
  all=T
)
res_learning = filter(res_learning, !email %in% learning$email)
res_learning = filter(res_learning, !is.na(Q2_1.s2) & !is.na(Q6_1.s3))
learning = rbind(learning,res_learning)
check_evolution(learning,c('Q2_1.s2','Q6_1.s3'))



#####
## Perceived deliberative and democratic aspects of the process
####
deliaspects = select(respondents,email,Q5_19.s2,Q5_20.s2,Q5_21.s2,Q5_22.s2,Q4_7.s2,
                     Q4_8.s2,Q6_1.s2,Q6_2.s2,Q6_3.s2,Q5_19.s3,Q5_20.s3,Q5_21.s3,
                     Q5_22.s3,Q7_7.s3,Q7_8.s3)
res_deliaspects = merge(
  select(survey_2, email, Q5_19.s2,Q5_20.s2,Q5_21.s2,Q5_22.s2,Q4_7.s2,
         Q4_8.s2,Q6_1.s2,Q6_2.s2,Q6_3.s2),
  select(survey_3, email, Q5_19.s3,Q5_20.s3,Q5_21.s3,
         Q5_22.s3,Q7_7.s3,Q7_8.s3),
  by.x='email',
  by.y='email',
  all=T
)
res_deliaspects = filter(res_deliaspects, !email %in% deliaspects$email)
# Comparison
n_all_deliaspects = rbind(
  mutate(deliaspects,group='65_res'),
  mutate(res_deliaspects,group='rest')
)
for (colname in colnames(n_all_deliaspects[2:16])) {
  ret = wilcox.test(n_all_deliaspects[n_all_deliaspects$group=='65_res',colname], 
                    n_all_deliaspects[n_all_deliaspects$group=='rest',colname])  
  print('------')
  print(paste('Column:',colname))
  print(paste('p-value:',ret$p.value))
}
# There aren't columns with significant differences


#####
## Trust in goverment
####
trust = select(respondents,email,Q4_6.s2,Q7_6.s3)
res_trust = merge(
  select(survey_2, email, Q4_6.s2),
  select(survey_3, email, Q7_6.s3),
  by.x='email',
  by.y='email',
  all=T
)
res_trust = filter(res_trust, !email %in% trust$email)
# Comparison
n_all_trust = rbind(
  mutate(trust,group='65_res'),
  mutate(res_trust,group='rest')
)
for (colname in colnames(n_all_trust[2:3])) {
  ret = wilcox.test(n_all_trust[n_all_trust$group=='65_res',colname], 
                    n_all_trust[n_all_trust$group=='rest',colname])  
  print('------')
  print(paste('Column:',colname))
  print(paste('p-value:',ret$p.value))
}
# There aren't columns with significant differences



####
## Group Efficacy
####
group_efficacy = select(respondents,email,Q4_7.s2,Q7_7.s3)
res_group_efficacy = merge(
  select(survey_2, email, Q4_7.s2),
  select(survey_3, email, Q7_7.s3),
  by.x='email',
  by.y='email',
  all=T
)
res_group_efficacy = filter(res_group_efficacy, !email %in% group_efficacy$email)
# Comparison
n_all_ge = rbind(
  mutate(group_efficacy,group='65_res'),
  mutate(res_group_efficacy,group='rest')
)
for (colname in colnames(n_all_ge[2:3])) {
  ret = wilcox.test(n_all_ge[n_all_ge$group=='65_res',colname], 
                    n_all_ge[n_all_ge$group=='rest',colname])  
  print('------')
  print(paste('Column:',colname))
  print(paste('p-value:',ret$p.value))
}
# There aren't columns with significant differences



#####
## Happiness with the law
#####
happiness = select(respondents,email,Q8_1.s1,Q8_2.s1,Q8_4.s1,Q8_5.s1,Q8_8.s1,
                   Q12_1.s2,Q12_2.s2,Q12_4.s2,Q12_5.s2,Q12_8.s2,Q13_3.s2,Q13_5.s2,Q13_7.s2)
res_happiness = merge(
  select(survey_1, email, Q8_1.s1,Q8_2.s1,Q8_4.s1,Q8_5.s1,Q8_8.s1),
  select(survey_2, email, Q12_1.s2,Q12_2.s2,Q12_4.s2,Q12_5.s2,Q12_8.s2,Q13_3.s2,Q13_5.s2,Q13_7.s2),
  by.x='email',
  by.y='email',
  all=T
)
res_happiness = filter(res_happiness, !email %in% happiness$email)
# Comparison
n_all_happiness = rbind(
  mutate(happiness,group='65_res'),
  mutate(res_happiness,group='rest')
)
for (colname in colnames(n_all_happiness[2:14])) {
  ret = wilcox.test(n_all_happiness[n_all_happiness$group=='65_res',colname], 
                    n_all_happiness[n_all_happiness$group=='rest',colname])  
  print('------')
  print(paste('Column:',colname))
  print(paste('p-value:',ret$p.value))
}
# There aren't columns with significant differences



#####
## Perception of the stakeholders' interest and rigths
#####
interest = select(respondents,email,Q4_1.s3,Q4_2.s3,Q4_4.s3,Q4_5.s3,Q4_10.s3)
res_interest = select(survey_3, email, Q4_1.s3,Q4_2.s3,Q4_4.s3,Q4_5.s3,Q4_10.s3)
res_interest = filter(res_interest, !email %in% interest$email)
# Comparison
n_all_interest = rbind(
  mutate(interest,group='65_res'),
  mutate(res_interest,group='rest')
)
for (colname in colnames(n_all_interest[2:6])) {
  ret = wilcox.test(n_all_interest[n_all_interest$group=='65_res',colname], 
                    n_all_interest[n_all_interest$group=='rest',colname])  
  print('------')
  print(paste('Column:',colname))
  print(paste('p-value:',ret$p.value))
}
# There aren't columns with significant differences



#####
## Knowledge gain
#####
knowledge = select(respondents,email,Q6_3.s1,Q6_4.s1,Q6_5.s1,Q11_6.s2,
                   Q11_7.s2,Q11_8.s2,Q11_6.s3,Q11_7.s3,Q11_8.s3)
res_knowledge = merge(
  select(survey_1, email, Q6_3.s1,Q6_4.s1,Q6_5.s1),
  select(survey_2, email, Q11_6.s2,Q11_7.s2,Q11_8.s2),
  by.x='email',
  by.y='email',
  all=T
)
res_knowledge = merge(
  res_knowledge,
  select(survey_3, email, Q11_6.s3,Q11_7.s3,Q11_8.s3),
  by.x='email',
  by.y='email',
  all=T
)
res_knowledge = filter(res_knowledge, !email %in% knowledge$email)
# Comparison
n_all_knowledge = rbind(
  mutate(knowledge,group='65_res'),
  mutate(res_knowledge,group='rest')
)
for (colname in colnames(n_all_knowledge[2:10])) {
  ret = wilcox.test(n_all_knowledge[n_all_knowledge$group=='65_res',colname], 
                    n_all_knowledge[n_all_knowledge$group=='rest',colname])  
  print('------')
  print(paste('Column:',colname))
  print(paste('p-value:',ret$p.value))
}
# Columns with significant differences
# "Column: Q11_8.s3, p-value: 0.0310756841271979"
# Draw chart of this column
f_knowledge = knowledge[,c('email','Q11_8.s3')]
f_knowledge[,2] = as.factor(f_knowledge[,2])
f_knowledge$group = '65_res'
f_res_knowledge = res_knowledge[,c('email','Q11_8.s3')]
f_res_knowledge[,2] = as.factor(f_res_knowledge[,2])
f_res_knowledge$group = 'rest'
all_knowledge = rbind(f_knowledge,f_res_knowledge)
likert_se = likert(all_knowledge[,2,drop=F], grouping=all_knowledge$group)
summary(likert_se)
plot(likert_se, type="bar") + 
  ggtitle('Knowledge gain (only significant)')


#####
## Opinion change
#####
opinion = select(respondents, email, Q7_3.s1,Q7_5.s1,Q7_7.s1,Q8_1.s1,Q8_2.s1,Q8_4.s1,Q8_5.s1,Q8_8.s1,
                 Q13_3.s2,Q13_5.s2,Q13_7.s2,Q12_1.s2,Q12_2.s2,Q12_4.s2,Q12_5.s2,Q12_8.s2,Q2_2.s2,
                 Q6_4.s3)
res_opinion = merge(
  select(survey_1, email, Q7_3.s1,Q7_5.s1,Q7_7.s1,Q8_1.s1,Q8_2.s1,Q8_4.s1,Q8_5.s1,Q8_8.s1),
  select(survey_2, email, Q13_3.s2,Q13_5.s2,Q13_7.s2,Q12_1.s2,Q12_2.s2,Q12_4.s2,Q12_5.s2,Q12_8.s2,Q2_2.s2),
  by.x='email',
  by.y='email',
  all=T
)
res_opinion = merge(
  res_opinion,
  select(survey_3, email, Q6_4.s3),
  by.x='email',
  by.y='email',
  all=T
)
res_opinion = filter(res_opinion, !email %in% opinion$email)
# Comparison between groups
n_all_opinion = rbind(
  mutate(opinion,group='65_res'),
  mutate(res_opinion,group='rest')
)
for (colname in colnames(n_all_opinion[2:19])) {
  ret = wilcox.test(n_all_opinion[n_all_opinion$group=='65_res',colname], 
                    n_all_opinion[n_all_opinion$group=='rest',colname])  
  print('------')
  print(paste('Column:',colname))
  print(paste('p-value:',ret$p.value))
}
# There aren't columns with significant differences
# Comparison within groups
# First, the 65
wilcox.test(opinion[,'Q7_3.s1'], opinion[,'Q13_3.s2'], paired=T)  
wilcox.test(opinion[,'Q7_5.s1'], opinion[,'Q13_5.s2'], paired=T)  
wilcox.test(opinion[,'Q7_7.s1'], opinion[,'Q13_7.s2'], paired=T)  
wilcox.test(opinion[,'Q8_1.s1'], opinion[,'Q12_1.s2'], paired=T)  # found a significant change p-value = 0.03332
wilcox.test(opinion[,'Q8_2.s1'], opinion[,'Q12_2.s2'], paired=T)
wilcox.test(opinion[,'Q8_4.s1'], opinion[,'Q12_4.s2'], paired=T)
wilcox.test(opinion[,'Q8_5.s1'], opinion[,'Q12_5.s2'], paired=T)
wilcox.test(opinion[,'Q8_8.s1'], opinion[,'Q12_8.s2'], paired=T)
wilcox.test(opinion[,'Q2_2.s2'], opinion[,'Q6_4.s3'], paired=T)
# Second, the rest
wilcox.test(res_opinion[,'Q7_3.s1'], res_opinion[,'Q13_3.s2'], paired=T)  
wilcox.test(res_opinion[,'Q7_5.s1'], res_opinion[,'Q13_5.s2'], paired=T)  
wilcox.test(res_opinion[,'Q7_7.s1'], res_opinion[,'Q13_7.s2'], paired=T)  
wilcox.test(res_opinion[,'Q8_1.s1'], res_opinion[,'Q12_1.s2'], paired=T)
wilcox.test(res_opinion[,'Q8_2.s1'], res_opinion[,'Q12_2.s2'], paired=T)
wilcox.test(res_opinion[,'Q8_4.s1'], res_opinion[,'Q12_4.s2'], paired=T)
wilcox.test(res_opinion[,'Q8_5.s1'], res_opinion[,'Q12_5.s2'], paired=T)
wilcox.test(res_opinion[,'Q8_8.s1'], res_opinion[,'Q12_8.s2'], paired=T)
wilcox.test(res_opinion[,'Q2_2.s2'], res_opinion[,'Q6_4.s3'], paired=T)
# Draw chart for the column with significant change
f_opinion_s1 = opinion[,c('email','Q8_1.s1')]
f_opinion_s1[,2] = factor(f_opinion_s1[,2], levels=c('1','2','3','4','5','6','7'), ordered=T)
f_opinion_s1$group = 'before'
colnames(f_opinion_s1) = c('email','disagreement','group')
f_res_opinion_s2 = opinion[,c('email','Q12_1.s2')]
f_res_opinion_s2[,2] = factor(f_res_opinion_s2[,2], levels=c('1','2','3','4','5','6','7'), ordered=T)
f_res_opinion_s2$group = 'after'
colnames(f_res_opinion_s2) = c('email','disagreement','group')
opinion_s1_s2 = rbind(f_opinion_s1,f_res_opinion_s2)
opinion_s1_s2[,3] = factor(opinion_s1_s2[,3], levels=c('before','after'), ordered=T)
likert_se = likert(opinion_s1_s2[,2,drop=F], grouping=opinion_s1_s2$group)
summary(likert_se)
plot(likert_se, type="bar") + 
  ggtitle('Opinion change regarding disagreements in condominium companies (N=65)')


####
## Evolution of an opinion change factor
####
opinion = select(respondents, email, Q2_2.s2, Q6_4.s3)
f_opinion = opinion
num_col = ncol(f_opinion)
f_opinion[,c(2:num_col)] = lapply(f_opinion[,c(2:num_col)], 
                                   function(x) factor(x, levels=c('1','2','3','4','5','6','7'), ordered=T))
# Melting data
m_f_opinion = melt(f_opinion, id.vars='email', 
                    measure.vars=c('Q6_4.s3','Q2_2.s2'))
m_f_opinion$value = as.integer(m_f_opinion$value)
colnames(m_f_opinion) = c('email','factor','score')
m_f_opinion$survey = unlist(lapply(m_f_opinion$factor, 
                                    function(x) ifelse(regexpr('.s2', x)[1]!=-1,'s2','s3')))
wilcox.test(score ~ survey, data=m_f_opinion, paired = TRUE)
# Output:
# V = 663.5, p-value = 0.642
# The rest
res_opinion = merge(
  select(survey_2, email, Q2_2.s2),
  select(survey_3, email, Q6_4.s3),
  by.x='email',
  by.y='email',
  all=T
)
res_opinion = filter(res_opinion, !email %in% opinion$email)
res_opinion = filter(res_opinion, !is.na(Q2_2.s2) & !is.na(Q6_4.s3))
## Cannot check the evolution of the factor since only one participant replied 
## to both survey 2 and survey 3
# Check evolution among those who rated high in self-efficacy
opinion = select(respondents, email, Q2_2.s2, Q6_4.s3, Q4_2.s1, Q4_3.s1, 
                  Q4_4.s1, Q4_5.s1, Q5_1.s1, Q5_3.s1, Q5_4.s1, Q5_5.s1, Q5_6.s1,
                  Q5_7.s1, Q5_14.s1)
res_opinion = merge(
  select(survey_1, email, Q4_2.s1, Q4_3.s1, 
         Q4_4.s1, Q4_5.s1, Q5_1.s1, Q5_3.s1, Q5_4.s1, Q5_5.s1, Q5_6.s1,
         Q5_7.s1, Q5_14.s1),
  select(survey_2, email, Q2_2.s2),
  by.x='email',
  by.y='email',
  all=T
)
res_opinion = merge(
  res_opinion,
  select(survey_3, email, Q6_4.s3),
  by.x='email',
  by.y='email',
  all=T
)
res_opinion = filter(res_opinion, !email %in% opinion$email)
res_opinion = filter(res_opinion, !is.na(Q2_2.s2) & !is.na(Q6_4.s3))
opinion = rbind(opinion,res_opinion)
check_evolution(opinion,c('Q2_2.s2','Q6_4.s3'))




#####
## Activity level of the 65
#####
# Ideas
ideas_65_stage1 = ideas_1_stage %>% filter(author_email %in% respondents$email) %>% mutate(stage='first',type_activity='idea')
ideas_65_stage2 = ideas_2_stage %>% filter(author_email %in% respondents$email) %>% mutate(stage='second',type_activity='idea')
ideas_65 = rbind(select(ideas_65_stage1, -idea_id, -author_email),
                 select(ideas_65_stage2, -idea_id, -author_email))
plot_activity_per_week(ideas_65, 'idea', 'line')
ideas_rest = rbind(
  ideas_1_stage %>% filter(!author_email %in% respondents$email) %>% mutate(stage='first',type_activity='idea') %>% 
    select(-idea_id, -author_email),
  ideas_2_stage %>% filter(!author_email %in% respondents$email) %>% mutate(stage='second',type_activity='idea') %>% 
    select(-idea_id, -author_email)
)
plot_activity_per_week(ideas_rest, 'idea', 'line')
# Patterns in the generation of ideas are similar to the rest of the crowd
# The saturation point happens exactly on the same week (week 5)
# Comments
comments_65 = rbind(
  comments_1_stage %>% filter(author_email %in% respondents$email) %>% mutate(stage='first',type_activity='comment') %>% 
    select(-idea_id, -author_email),
  comments_2_stage %>% filter(author_email %in% respondents$email) %>% mutate(stage='second',type_activity='comment') %>% 
    select(-idea_id, -author_email)
)
plot_activity_per_week(comments_65, 'comment', 'line') + ggtitle('Evolution of comments of the 65')
comments_rest = rbind(
  comments_1_stage %>% filter(!author_email %in% respondents$email) %>% mutate(stage='first',type_activity='comment') %>% 
    select(-idea_id, -author_email),
  comments_2_stage %>% filter(!author_email %in% respondents$email) %>% mutate(stage='second',type_activity='comment') %>% 
    select(-idea_id, -author_email)
)
plot_activity_per_week(comments_rest, 'comment', 'line') + ggtitle('Evolution of comments of the general crowd (without the 65)')
# Votes and Comments
votes_65 = rbind(
  votes_1_stage %>% filter(author_email %in% respondents$email) %>% mutate(stage='first',type_activity='vote') %>% 
    select(-value, -author_email, -content_type, -content_id),
  votes_2_stage %>% filter(author_email %in% respondents$email) %>% mutate(stage='second',type_activity='vote') %>% 
    select(-value, -author_email, -content_type, -content_id)
)
aggregated_comments_votes_65 = rbind(comments_65,votes_65)
plot_activity_per_week(aggregated_comments_votes_65, 'aggregated', 'line') + 
  ggtitle('Evolution of votes and comments of the 65')
votes_rest = rbind(
  votes_1_stage %>% filter(!author_email %in% respondents$email) %>% mutate(stage='first',type_activity='vote') %>% 
    select(-value, -author_email, -content_type, -content_id),
  votes_2_stage %>% filter(!author_email %in% respondents$email) %>% mutate(stage='second',type_activity='vote') %>% 
    select(-value, -author_email, -content_type, -content_id)
)
aggregated_comments_votes_rest = rbind(comments_rest,votes_rest)
plot_activity_per_week(aggregated_comments_votes_rest, 'aggregated', 'line') + 
  ggtitle('Evolution of votes and comments of the general crowd (without the 65)')
# It can be seen that the 65 people who took the three surveys sustained for 
# more longer periods their active participation in the platform. They reached the
# saturation point later (week 5) in relation to the rest of the crowd that reached
# in week 2.