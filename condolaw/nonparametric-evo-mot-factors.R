####
## This script analyzes the evolution
## of the motivation factors
## using non-parametric tools, like
## Friedmann and Wilcoxon tests
####

library(dplyr)
library(psych)
library(likert)
library(reshape2)
library(FSA)
library(rcompanion)
library(coin)

# Reading data
mot_survey_1 = read.csv("./data/survey1-condlaw-no-duplicates.csv",header=T, sep=",",stringsAsFactors=F)
mot_survey_2 = read.csv("./data/survey2-condlaw-no-duplicates.csv",header=T, sep=",",stringsAsFactors=F)
mot_survey_3 = read.csv("./data/survey3-condlaw-no-duplicates2.csv",header=T, sep=",",stringsAsFactors=F)

# Data preparation
colnames(mot_survey_1) = c('email', 'datetime', 'F1', 'F2', 'F3', 'F4', 'F5', 'F6', 'F7')
colnames(mot_survey_2) = c('email', 'datetime', 'F1', 'F2', 'F3', 'F4', 'F5', 'F6', 'F7', 'F8')
colnames(mot_survey_3) = c('email', 'datetime', 'F1', 'F2', 'F3', 'F4', 'F5', 'F7', 'F8', 'F9')
mot_survey_1 = mot_survey_1 %>% filter(email!='') %>% filter(!is.na(F1)) %>% filter(F1!='') %>% select(-datetime)
mot_survey_2 = mot_survey_2 %>% filter(email!='') %>% filter(!is.na(F1)) %>% filter(F1!='') %>% select(-datetime)
mot_survey_3 = mot_survey_3 %>% filter(email!='') %>% filter(!is.na(F1)) %>% filter(F1!='') %>% select(-datetime)
mot_surveys = merge(mot_survey_1, mot_survey_2, by.x='email', by.y='email', all=F)
mot_surveys = merge(mot_surveys, mot_survey_3, by.x='email', by.y='email', all=F)   # Group of people how took surveys 1, 2 and 3
mot_s2_s3 = merge(mot_survey_2, mot_survey_3, by.x='email', by.y='email')  # Group of people how took only surveys 2 and 3
mot_survey_1 = mot_survey_1 %>% filter(email %in% mot_surveys$email)
mot_survey_2 = mot_survey_2 %>% filter(email %in% mot_surveys$email)
mot_survey_3 = mot_survey_3 %>% filter(email %in% mot_surveys$email)

# Extract descriptive statistics for the likert data (factors)
# http://rcompanion.org/handbook/E_03.html
# Survey 1
# Cast factors columns to number
mot_survey_1[,c(2:8)] = apply(mot_survey_1[,c(2:8)], 2, function(x) as.numeric(x))
summary(mot_survey_1)
sd(mot_survey_1$F7)
# 1. Change Likert scores to factor and specify levels
f_mot_survey_1 = mot_survey_1
f_mot_survey_1[,c(2:8)] = lapply(f_mot_survey_1[,c(2:8)], 
                                 function(x) factor(x,
                                            levels=c('1','2','3','4','5','6','7'),
                                            ordered=T))
# 2. Summary statistics
ret = likert(f_mot_survey_1[2:8])
summary(ret)
# 3. Plots
plot(ret, type="bar")
plot(ret, type="heat",
     low.color = "white", 
     high.color = "blue",
     text.color = "black", 
     text.size = 4, wrap = 50)
plot(ret, type="density",
     facet = TRUE, bw = 0.5)
# 4. Add group
f_mot_survey_1 = mutate(f_mot_survey_1, F8='NA')
f_mot_survey_1 = mutate(f_mot_survey_1, F9='NA')
f_mot_survey_1 = mutate(f_mot_survey_1, survey='Survey 1')

# Survey 2
# Cast factors columns to number
mot_survey_2[,c(2:9)] = apply(mot_survey_2[,c(2:9)], 2, function(x) as.numeric(x))
summary(mot_survey_2)
sd(mot_survey_2$F8)
# Repeat same steps as before
f_mot_survey_2 = mot_survey_2
f_mot_survey_2[,c(2:8)] = lapply(f_mot_survey_2[,c(2:8)], 
                                 function(x) factor(x,
                                                    levels=c('1','2','3','4','5','6','7'),
                                                    ordered=T))
f_mot_survey_2 = mutate(f_mot_survey_2, F9='NA')
f_mot_survey_2 = mutate(f_mot_survey_2, survey='Survey 2')

# Survey 3
# Cast factors columns to number
mot_survey_3[,c(2:9)] = apply(mot_survey_3[,c(2:9)], 2, function(x) as.numeric(x))
summary(mot_survey_3)
sd(mot_survey_3$F9)
# Repeat same steps as before
f_mot_survey_3 = mot_survey_3
f_mot_survey_3[,c(2:8)] = lapply(f_mot_survey_3[,c(2:8)], 
                                 function(x) factor(x,
                                                    levels=c('1','2','3','4','5','6','7'),
                                                    ordered=T))
f_mot_survey_3 = mutate(f_mot_survey_3, F6='NA')
f_mot_survey_3 = mutate(f_mot_survey_3, survey='Survey 3')

# Aggregate data frames
f_mot_surveys = rbind(f_mot_survey_1,f_mot_survey_2,f_mot_survey_3)
f_mot_surveys$survey = factor(f_mot_surveys$survey,
                             levels=c('Survey 3','Survey 2','Survey 1'),
                             ordered=T)

# General summary statistics
ret = likert(f_mot_surveys[,c(2:10)], grouping=f_mot_surveys[,11])
summary(ret)

# 3. Plots
plot(ret, type="bar")
ret2 = likert(f_mot_surveys[,c('F1','F2','F3','F4','F5','F7')], grouping=f_mot_surveys$survey)
plot(ret2, type="density")


### Interaction plot using medians and quantiles
# Melting data
m_f_mot_surveys = melt(f_mot_surveys, id.vars='survey', 
                       measure.vars=c('F1','F2','F3','F4','F5','F6','F7','F8','F9'))
m_f_mot_surveys$value = as.integer(m_f_mot_surveys$value)
colnames(m_f_mot_surveys) = c('survey','factor','score')
# Create a data frame called Sum with median and quartiles 
sum = Summarize(score ~ survey + factor, 
                data=m_f_mot_surveys, 
                digits=3)
# Draw interaction plot
pd = position_dodge(.2)
ggplot(sum, aes(x=factor,
                y=median,
                color=survey)) +
  geom_errorbar(aes(ymin=Q1,
                    ymax=Q3),
                width=.2, size=0.7, position=pd) +
  geom_point(shape=15, size=4, position=pd) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold")) +
  ylab("Median Likert score")


# Apply Friedmann Test to study changes in the motivation factors over time
m_f_mot_surveys = melt(f_mot_surveys, id.vars=c('survey','email'), 
                       measure.vars=c('F1','F2','F3','F4','F5','F6','F7','F8','F9'))
m_f_mot_surveys$value = as.integer(m_f_mot_surveys$value)
colnames(m_f_mot_surveys) = c('survey','email','factor','score')
# F1: Check whether the median score of F1 varies significantly across the three surveys
friedman.test(score ~ survey | email, data = filter(m_f_mot_surveys,factor=='F1'))
# Run a Wilcoxon as a post-hoc test to see where happen to be the significant difference
pairwise.wilcox.test(filter(m_f_mot_surveys,factor=='F1')$score, filter(m_f_mot_surveys,factor=='F1')$survey, 
                     p.adj="bonferroni", exact=F, paired=T)
# Calculate the effect size
# coin::wilcoxsign_test is applied instead of stats::wilcox.test because
# to calculate the effect we need Z, which a value returned by the
# former not the latter
wilcoxsign_test(score ~ survey, 
                data=filter(m_f_mot_surveys,factor=='F1'&(survey=='Survey 1'|survey=='Survey 2')), 
                distribution="exact", paired=T)
# Effect size z
9.9093/sqrt(nrow(filter(m_f_mot_surveys,factor=='F1'&(survey=='Survey 1'|survey=='Survey 2'))))

# F2: Check whether the median score of F2 varies significantly across the three surveys
friedman.test(score ~ survey | email, data = filter(m_f_mot_surveys,factor=='F2'))
# Run a Wilcoxon as a post-hoc test to see where happen to be the significant difference
pairwise.wilcox.test(filter(m_f_mot_surveys,factor=='F2')$score, filter(m_f_mot_surveys,factor=='F2')$survey, 
                     p.adj="bonferroni", exact=F, paired=T)
# Calculate the effect size
# coin::wilcox_test is applied instead of stats::wilcox.test because
# to calculate the effect we need Z, which a value returned by the
# former not the latter
wilcoxsign_test(score ~ survey, data=filter(m_f_mot_surveys,factor=='F2'&(survey=='Survey 1'|survey=='Survey 2')), distribution="exact", paired=T)
# Effect size z
9.9255/sqrt(nrow(filter(m_f_mot_surveys,factor=='F2'&(survey=='Survey 1'|survey=='Survey 2'))))

# F3: Check whether the median score of F3 varies significantly across the three surveys
friedman.test(score ~ survey | email, data = filter(m_f_mot_surveys,factor=='F3'))

# F4: Check whether the median score of F4 varies significantly across the three surveys
friedman.test(score ~ survey | email, data = filter(m_f_mot_surveys,factor=='F4'))

# F5: Check whether the median score of F5 varies significantly across the three surveys
friedman.test(score ~ survey | email, data = filter(m_f_mot_surveys,factor=='F5'))

# F7: Check whether the median score of F7 varies significantly across the three surveys
friedman.test(score ~ survey | email, data = filter(m_f_mot_surveys,factor=='F7'))
# Run a Wilcoxon as a post-hoc test to see where happen to be the significant difference
pairwise.wilcox.test(filter(m_f_mot_surveys,factor=='F7')$score, filter(m_f_mot_surveys,factor=='F7')$survey, 
                     p.adj="bonferroni", exact=F, paired=T)
# Calculate the effect size
# coin::wilcoxsign_test is applied instead of stats::wilcox.test because
# to calculate the effect we need Z, which is a value returned by the
# former not the latter
wilcoxsign_test(score ~ survey, data=filter(m_f_mot_surveys,factor=='F7'&(survey=='Survey 2'|survey=='Survey 3')), 
                distribution="exact", paired=T)
# Effect size z
9.9752/sqrt(nrow(filter(m_f_mot_surveys,factor=='F2'&(survey=='Survey 1'|survey=='Survey 2'))))

# Apply Wilcox for factors that are present only in two surveys
# F6: Check whether the median score of F6 varies significantly between S1 and S2
wilcox.test(score ~ survey, data=filter(m_f_mot_surveys,factor=='F6'&survey!='Survey 3'), paired = TRUE)
# Using stats::wilcox.test the exact p value cannot be calculated because of ties
# so in this case coin::wilcoxsign_test is used
# source: http://yatani.jp/teaching/doku.php?id=hcistats:wilcoxonsigned
wilcoxsign_test(score ~ survey, data=filter(m_f_mot_surveys,factor=='F6'&survey!='Survey 3'), 
                distribution="exact")

# F8: Check whether the median score of F8 varies significantly between S2 and S3
wilcox.test(score ~ survey, data=filter(m_f_mot_surveys,factor=='F8'&survey!='Survey 1'), paired = TRUE)
# Using stats::wilcox.test the exact p value cannot be calculated because of ties
# so in this case coin::wilcoxsign_test is used
# source: http://yatani.jp/teaching/doku.php?id=hcistats:wilcoxonsigned
wilcoxsign_test(score ~ survey, data=filter(m_f_mot_surveys,factor=='F8'&survey!='Survey 1'), 
                distribution="exact")
median(filter(m_f_mot_surveys,factor=='F8'&survey=='Survey 2')$score)
median(filter(m_f_mot_surveys,factor=='F8'&survey=='Survey 3')$score)
# Effect size z
10.016/sqrt(nrow(filter(m_f_mot_surveys,factor=='F8'&survey!='Survey 1')))

# Kruskal–Wallis Test to analyze variance in the score of factor within phases
# http://rcompanion.org/handbook/F_08.html
# S1: Check whether the score of factors varies significantly within phase 1 (survey 1)
mot_survey_1 = read.csv("./data/survey1-condlaw-no-duplicates.csv",header=T, sep=",",stringsAsFactors=F)
colnames(mot_survey_1) = c('email', 'datetime', 'F1', 'F2', 'F3', 'F4', 'F5', 'F6', 'F7')
mot_survey_1 = mot_survey_1 %>% filter(email!='') %>% filter(!is.na(F1)) %>% filter(F1!='') %>% select(-datetime)
f_mot_survey_1 = mot_survey_1
f_mot_survey_1[,c(2:8)] = lapply(f_mot_survey_1[,c(2:8)], 
                                 function(x) factor(x, levels=c('1','2','3','4','5','6','7'),
                                                    ordered=T))
str(f_mot_survey_1)
m_f_mot_survey_1 = melt(f_mot_survey_1, id.vars='email',
                        measure.vars=c('F1','F2','F3','F4','F5','F6','F7'))
m_f_mot_survey_1$value = as.integer(m_f_mot_survey_1$value)
colnames(m_f_mot_survey_1) = c('email','factor','score')
kruskal.test(score ~ factor, data = m_f_mot_survey_1)
# Post-hoc Dunn Test
# dunnTest(score ~ factor, data = m_f_mot_survey_1)
# Wilcoxon Post-hoc 
pairwise.wilcox.test(m_f_mot_survey_1$score, m_f_mot_survey_1$factor, p.adj="bonferroni", exact=F)

# S2: Check whether the score of factors varies significantly within phase 2 (survey 2)
mot_survey_2 = read.csv("./data/survey2-condlaw-no-duplicates.csv",header=T, sep=",",stringsAsFactors=F)
colnames(mot_survey_2) = c('email', 'datetime', 'F1', 'F2', 'F3', 'F4', 'F5', 'F6', 'F7','F8')
mot_survey_2 = mot_survey_2 %>% filter(email!='') %>% filter(!is.na(F1)) %>% filter(F1!='') %>% select(-datetime)
f_mot_survey_2 = mot_survey_2
f_mot_survey_2[,c(2:9)] = lapply(f_mot_survey_2[,c(2:9)], 
                                 function(x) factor(x, levels=c('1','2','3','4','5','6','7'),
                                                    ordered=T))
str(f_mot_survey_2)
m_f_mot_survey_2 = melt(f_mot_survey_2, id.vars='email',
                        measure.vars=c('F1','F2','F3','F4','F5','F6','F7','F8'))
m_f_mot_survey_2$value = as.integer(m_f_mot_survey_2$value)
colnames(m_f_mot_survey_2) = c('email','factor','score')
kruskal.test(score ~ factor, data = m_f_mot_survey_2)
# Post-hoc Dunn Test
# dunnTest(score ~ factor, data = m_f_mot_survey_2)
# Wilcoxon Post-hoc 
pairwise.wilcox.test(m_f_mot_survey_2$score, m_f_mot_survey_2$factor, p.adj="bonferroni", exact=F)


# S3: Check whether the score of factors varies significantly within phase 3 (survey 3)
mot_survey_3 = read.csv("./data/survey3-condlaw-no-duplicates2.csv",header=T, sep=",",stringsAsFactors=F)
colnames(mot_survey_3) = c('email', 'datetime', 'F1', 'F2', 'F3', 'F4', 'F5', 'F7', 'F8','F9')
mot_survey_3 = mot_survey_3 %>% filter(email!='') %>% filter(!is.na(F1)) %>% filter(F1!='') %>% select(-datetime)
f_mot_survey_3 = mot_survey_3
f_mot_survey_3[,c(2:9)] = lapply(f_mot_survey_3[,c(2:9)], 
                                 function(x) factor(x, levels=c('1','2','3','4','5','6','7'),
                                                    ordered=T))
str(f_mot_survey_3)
m_f_mot_survey_3 = melt(f_mot_survey_3, id.vars='email',
                        measure.vars=c('F1', 'F2', 'F3', 'F4', 'F5', 'F7', 'F8','F9'))
m_f_mot_survey_3$value = as.integer(m_f_mot_survey_3$value)
colnames(m_f_mot_survey_3) = c('email','factor','score')
kruskal.test(score ~ factor, data = m_f_mot_survey_3)
# Post-hoc Dunn Test
# dunnTest(score ~ factor, data = m_f_mot_survey_3)
# Wilcoxon Post-hoc 
pairwise.wilcox.test(m_f_mot_survey_3$score, m_f_mot_survey_3$factor, p.adj="bonferroni", exact=F)
