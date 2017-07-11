
prepare_offroadsurvey = function(raw_survey) {
  raw_survey = raw_survey[-1,]
  # Discard those that dind't complete the survey
  raw_survey = raw_survey %>% filter(V10 == 1)
  survey = raw_survey %>% select(V5, Q23:Q34_11) %>% select(-Q28) %>% select(-Q33_TEXT)
  rownames(survey) = NULL
  # Rename columns
  colnames(survey) = c('email','gender','age','basic_edu','occupational_edu','situation',
                       'occupational_group','living_area','living_zone','time_online','political_party',
                       'written an op-ed to a newspaper', 'written in a blog', 'written on online forums', 
                       'written to an MP', 'written to a municipal councilor',
                       'participated in municipality council', 'participated in committee meetings', 
                       'signed a petition', 'initiated a petition', 
                       'participated in peaceful demonstrations', 'volunteered in a NGO')
  # Separate participants that did and didn't answer the survey
  #participant_survey = participant_data %>% filter(email %in% survey$email)
  # Merge datasets
  #merged_ds = merge(part_survey_no_outliers,survey,by.x='email',by.y='email')

  return (survey)
}

prepare_housingsurvey = function(survey1,survey3) {
  survey_colnames = c('email','gender','age','basic_edu','occupational_edu','situation',
                      'occupational_group', 'written an op-ed to a newspaper', 
                      'written in a blog', 'written on online forums', 
                      'written to an MP', 'written to a municipal councilor',
                      'participated in municipality council', 'participated in committee meetings', 
                      'signed a petition', 'initiated a petition', 
                      'participated in peaceful demonstrations', 
                      'volunteered in a NGO')
  survey1 = select(survey1, email, Q33, Q34, Q35, Q36, Q37, 
                   Q38, Q40_1, Q40_2, Q40_3, Q40_4, Q40_5, Q40_6, 
                   Q40_7, Q40_8, Q40_9, Q40_10, Q40_11)
  survey3 = select(survey3, email, Q13, Q12, Q16, 
                   Q17, Q15, Q14, Q10_5, Q10_6, Q10_7, Q10_8,
                   Q10_9, Q10_10, Q10_11, Q10_12, Q10_13, Q10_14,
                   Q10_15)
  colnames(survey1) = survey_colnames
  colnames(survey3) = survey_colnames
  survey = rbind(survey1,survey3)
  survey = distinct(survey,email)
  
  return (survey)
}

add_content_author = function(dqi_dataset,ideas,comments,case) {
  #dqi_dataset = offroad_dqi
  #ideas = offroad_ideas
  #comments = offroad_comments
  #case='offroad'
  conflict_comments = 0
  ok_comments = 0
  tot_comments = nrow(filter(dqi_dataset,is.na(idea_id)))
  dqi_dataset$author_email = ''
  for (i in 1:nrow(dqi_dataset)) {
    content = dqi_dataset[i,]
    if (!is.na(content$idea_id)) {
      # It is an idea
      if (content$idea_id %in% ideas$idea_id) {
        content$author_email = ideas[ideas$idea_id==content$idea_id,'author_email'] 
      } else {
        print('Could not assign author\'s email')
      }
      idea_id = content$idea_id
      idea_num_comments = content$comment_count
    } else {
      # It is a comment
      comments_of_idea = comments[comments$idea_id==idea_id,]
      found_comment=F
      if (nrow(comments_of_idea) > 0) {
        content$comment = gsub("[^0-9A-Za-z ]", "", content$comment) # remove non-alphanumeric chars
        content$comment = gsub(" ", "", content$comment, fixed = TRUE) # remove white spaces
        for (z in 1:nrow(comments_of_idea)) {
          comment = comments_of_idea[z,]
          comment$comment = gsub("[^0-9A-Za-z ]", "", comment$comment) # remove non-alphanumeric chars
          comment$comment = gsub(" ", "", comment$comment, fixed = TRUE) # remove white spaces
          l_currentcomment = tolower(content$comment)
          l_thiscomment = tolower(comment$comment)
          if (identical(l_thiscomment, l_currentcomment)) {
            content$author_email = comment$author_email
            found_comment=T
            break
          } else {
            # We test whether dqi comment is contained in a comment obtained 
            # from db of comments or viceversa
            if (nchar(l_currentcomment) > nchar(l_thiscomment)) {
              if (grepl(l_thiscomment, l_currentcomment)) {
                content$author_email = comment$author_email
                found_comment=T
                break
              }
            } else {
              if (case=='offroad') {
                if (grepl(l_currentcomment, l_thiscomment)) {
                  content$author_email = comment$author_email
                  found_comment=T
                  break
                } 
              }
            }
          }
        } 
      }
      if (!found_comment) {
        conflict_comments = conflict_comments + 1
        break
      }
      else {
        ok_comments = ok_comments + 1
      }
    }
    dqi_dataset[i,] = content
  }
  print('================')
  print(paste('Case:',case))
  print(paste('Total Comments:',tot_comments))
  print(paste('Associated to author:',ok_comments))
  print(paste('Remained anonymous:',conflict_comments))
  print('================')
  
  return (dqi_dataset)
}

compare_means_between_groups = function(dataset, dv, iv) {  
  dataset = dataset[!is.na(dataset[iv]),]
  fig = ggplot(dataset, aes(x=as.character(dataset[,iv]), y=dataset[,dv])) +
        geom_boxplot(fill = "grey80", colour = "blue") +
        scale_x_discrete() + xlab(iv) +
        ylab("DQI score")
  print(fig)
  lm = lm(dataset[,dv] ~ as.character(dataset[,iv]))
  print(summary(lm))
  print(anova(lm))
}


get_group_profile = function(group) {
  print(paste('N:',nrow(group)))
  for(col in colnames(group)) {
    if (col=='email' || col=='cluster') {
      next
    }
    print(paste('Column', col))
    if (col=='mean_dqi' || col=='sd_dqi' || 
        col=='ideas' || col=='total_activity' ||
        col=='range_dqi') {
      print(summary(group[,col]))
    } else {
      print(table(group[,col])) 
    }
  }
}


# Load libraries
library(dplyr)
library(ggplot2)
library(cluster)
library(klaR)
library(caret)
source("./lib/dqi_utils.R")

# Load data
housing_dqi = read.csv("./data/jukka_housing_final_all_columns.csv", header=T, sep=",", stringsAsFactor=F)
housing_ideas = read.csv("./data/ideas_housing.csv", sep=",", header=T, stringsAsFactors=F)
housing_comments = read.csv("./data/comments_housing.csv", sep=",", header=T, stringsAsFactors=F)
housing_survey1 = read.csv("./data/survey1-housing.csv", sep=",", header=T, stringsAsFactors=F)
housing_survey3 = read.csv("./data/survey3-housing.csv", sep=",", header=T, stringsAsFactors=F)
housing_participants = read.csv("./data/participants_housing.csv", sep=",", header=T, stringsAsFactors=F)
offroad_dqi = read.csv("./data/jukka_offroad_final_all_columns.csv", header=T, sep=",", stringsAsFactor=F)
offroad_ideas = read.csv("./data/ideas_offroad.csv", sep=",", header=T, stringsAsFactors=F)
offroad_comments = read.csv("./data/comments_offroad.csv", sep=",", header=T, stringsAsFactors=F)
offroad_survey = read.csv("./data/surveyofftrafficlaw.csv", sep=",", header=T, stringsAsFactors=F)
offroad_participants = read.csv("./data/participants_offroad.csv", sep=",", header=T, stringsAsFactors=F)

# Prepare dqi dataset
housing_dqi = prepare_dqi_dataset(housing_dqi)
offroad_dqi = prepare_dqi_dataset(offroad_dqi)


# Add content author
housing_dqi = add_content_author(housing_dqi,housing_ideas,housing_comments,'housing')
offroad_dqi = add_content_author(offroad_dqi,offroad_ideas,offroad_comments,'offroad')


# Prepare dataset to compute dqi
housing_dqi = prepare_data(housing_dqi,FALSE)
housing_dqi = select(housing_dqi, -category, -group_supporting)
offroad_dqi = prepare_data(offroad_dqi,FALSE)
offroad_dqi = select(offroad_dqi, -category, -group_supporting)

# Define indicators that should be included when computing DQI score
indicators_idx = c(5,20:24,40)

# Compute ideas and comments posted by the participants
# Case: Housing
h_participants = merge(
  housing_ideas %>% group_by(author_email) %>% summarise(ideas=n()),
  housing_comments %>% group_by(author_email) %>% summarise(comments=n()),
  by.x='author_email',by.y='author_email', all=T)
h_participants = merge(h_participants, 
                       housing_participants %>% select(email,votes),
                       by.x='author_email',by.y='email', all=T)
h_participants[is.na(h_participants)] = 0
# Case: Offroad
o_participants = merge(
  offroad_ideas %>% group_by(author_email) %>% summarise(ideas=n()),
  offroad_comments %>% group_by(author_email) %>% summarise(comments=n()),
  by.x='author_email',by.y='author_email', all=T
)
o_participants = merge(o_participants, 
                       offroad_participants %>% select(email,votes),
                       by.x='author_email',by.y='email', all=T)
o_participants[is.na(o_participants)] = 0


####
# Are there "super-deliberators" vs. "low-quality deliberators"?
# Steps
# 1. Average columns (indicators) by email
housing_dqi_by_email = aggregate(select(housing_dqi,indicators_idx), 
                                 by=list(email=housing_dqi$author_email), mean)
offroad_dqi_by_email = aggregate(select(offroad_dqi,indicators_idx), 
                                 by=list(email=offroad_dqi$author_email), mean)
# 2. Sum up indicators by row
housing_dqi_by_email$mean_dqi = rowSums(housing_dqi_by_email[,2:ncol(housing_dqi_by_email)])
offroad_dqi_by_email$mean_dqi = rowSums(offroad_dqi_by_email[,2:ncol(offroad_dqi_by_email)])
# 3. Order from highest to lowest totals
# Housing
housing_dqi_by_email = arrange(housing_dqi_by_email, desc(mean_dqi))
View(select(housing_dqi_by_email,email,mean_dqi))
ggplot(housing_dqi_by_email, aes(x=mean_dqi)) +
  geom_histogram(binwidth=0.5, colour="black", fill="white") +
  scale_x_continuous(breaks=c(0:15)) +
  labs(x="DQI score", y="Number of participants") +
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20))
mean(housing_dqi_by_email$mean_dqi)
# Offroad
offroad_dqi_by_email = arrange(offroad_dqi_by_email, desc(mean_dqi))
View(select(offroad_dqi_by_email,email,mean_dqi))
ggplot(offroad_dqi_by_email, aes(x=mean_dqi)) +
  geom_histogram(binwidth=0.5, colour="black", fill="white") +
  scale_x_continuous(breaks=c(0:19)) +
  labs(x="DQI score", y="Number of participants") +
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20))
mean(offroad_dqi_by_email$mean_dqi)
####


###
# Compute the participants' DQI variation
###
# DQI Standard Deviation
housing_dqi_by_email2 = aggregate(select(housing_dqi,indicators_idx), 
                                        by=list(email=housing_dqi$author_email), sd)
housing_dqi_by_email$sd_dqi = rowSums(housing_dqi_by_email2[,2:ncol(housing_dqi_by_email2)])
offroad_dqi_by_email2 = aggregate(select(offroad_dqi,indicators_idx), 
                                  by=list(email=offroad_dqi$author_email), sd)
offroad_dqi_by_email$sd_dqi = rowSums(offroad_dqi_by_email2[,2:ncol(offroad_dqi_by_email2)])
# DQI Range
# Case: Housing
housing_dqi$sum = rowSums(select(housing_dqi, indicators_idx))
housing_dqi_by_email_max = aggregate(select(housing_dqi,sum), 
                                     by=list(email=housing_dqi$author_email), max)
housing_dqi_by_email = merge(housing_dqi_by_email,housing_dqi_by_email_max,by.x='email',by.y='email')
colnames(housing_dqi_by_email)[11] = 'max_dqi'
housing_dqi_by_email_min = aggregate(select(housing_dqi,sum), 
                                     by=list(email=housing_dqi$author_email), min)
housing_dqi_by_email = merge(housing_dqi_by_email,housing_dqi_by_email_min,by.x='email',by.y='email')
colnames(housing_dqi_by_email)[12] = 'min_dqi'
housing_dqi_by_email$range_dqi = housing_dqi_by_email$max_dqi - housing_dqi_by_email$min_dqi
# Case: Offroad
offroad_dqi$sum = rowSums(select(offroad_dqi, indicators_idx))
offroad_dqi_by_email_max = aggregate(select(offroad_dqi,sum), 
                                     by=list(email=offroad_dqi$author_email), max)
offroad_dqi_by_email = merge(offroad_dqi_by_email,offroad_dqi_by_email_max,by.x='email',by.y='email')
colnames(offroad_dqi_by_email)[11] = 'max_dqi'
offroad_dqi_by_email_min = aggregate(select(offroad_dqi,sum), 
                                     by=list(email=offroad_dqi$author_email), min)
offroad_dqi_by_email = merge(offroad_dqi_by_email,offroad_dqi_by_email_min,by.x='email',by.y='email')
colnames(offroad_dqi_by_email)[12] = 'min_dqi'
offroad_dqi_by_email$range_dqi = offroad_dqi_by_email$max_dqi - offroad_dqi_by_email$min_dqi
####


###
# Compute the participants' total contribution that was coded
###
housing_dqi_part = group_by(housing_dqi, author_email)
housing_dqi_by_email = merge(housing_dqi_by_email,
                             summarise(housing_dqi_part,coded_contributions = n()),
                             by.x='email',by.y='author_email')
offroad_dqi_part = group_by(offroad_dqi, author_email)
offroad_dqi_by_email = merge(offroad_dqi_by_email,
                             summarise(offroad_dqi_part,coded_contributions = n()),
                             by.x='email',by.y='author_email')


####
# Does their activity level (number of ideas, comments, votes) affect the 
# quality of deliberation?
# Case: Housing
housing_activity_dqi = merge(select(housing_dqi_by_email,email,mean_dqi,sd_dqi,coded_contributions,max_dqi,min_dqi,range_dqi),
                             select(h_participants,ideas,votes,comments,author_email),
                             by.x='email',by.y='author_email')
housing_activity_dqi = mutate(housing_activity_dqi,total_activity=ideas+votes+comments)
housing_activity_dqi = mutate(housing_activity_dqi,total_content=ideas+comments)
cor(housing_activity_dqi$ideas,housing_activity_dqi$mean_dqi)
cor(housing_activity_dqi$comments,housing_activity_dqi$mean_dqi)
cor(housing_activity_dqi$votes,housing_activity_dqi$mean_dqi)
cor(housing_activity_dqi$total_activity,housing_activity_dqi$mean_dqi)
cor(housing_activity_dqi$total_content,housing_activity_dqi$mean_dqi)
ggplot(housing_activity_dqi, aes(x=total_activity, y=mean_dqi)) +
  geom_point(shape=1) +
  labs(x="Activity level", y="DQI score")
ggplot(housing_activity_dqi, aes(x=total_content, y=mean_dqi)) +
  geom_point(shape=1) +
  labs(x="Ideas and comments created", y="DQI score")
# Case: Offroad
offroad_activity_dqi = merge(select(offroad_dqi_by_email,email,mean_dqi,sd_dqi,coded_contributions,max_dqi,min_dqi,range_dqi),
                             select(o_participants,ideas,votes,comments,author_email),
                             by.x='email',by.y='author_email')
offroad_activity_dqi = filter(offroad_activity_dqi,email!='')
offroad_activity_dqi = mutate(offroad_activity_dqi,total_activity=ideas+votes+comments)
offroad_activity_dqi = mutate(offroad_activity_dqi,total_content=ideas+comments)
cor(offroad_activity_dqi$ideas,offroad_activity_dqi$mean_dqi)
cor(offroad_activity_dqi$comments,offroad_activity_dqi$mean_dqi)
cor(offroad_activity_dqi$votes,offroad_activity_dqi$mean_dqi)
cor(offroad_activity_dqi$total_activity,offroad_activity_dqi$mean_dqi)
cor(offroad_activity_dqi$total_content,offroad_activity_dqi$mean_dqi)
ggplot(offroad_activity_dqi, aes(x=total_activity, y=mean_dqi)) +
  geom_point(shape=1) +
  labs(x="Activity level", y="DQI score")


###
# Do any of the demographic variables in the survey (gender, age, civic life activity, etc.) 
# affect the quality of deliberation?
# Case: Housing
housing_survey = prepare_housingsurvey(housing_survey1,housing_survey3)
housing_complete_df = merge(housing_survey,housing_activity_dqi,
                            by.x="email",by.y="email")
# Gender (Independent sample t-test)
t.test(filter(housing_complete_df,gender==1)$mean_dqi,
       filter(housing_complete_df,gender==2)$mean_dqi)
ggplot(housing_complete_df, aes(x=as.character(gender), y=mean_dqi)) +
  geom_boxplot(fill = "grey80", colour = "blue") +
  scale_x_discrete() + xlab("Gender") +
  ylab("DQI score")
# Age (One-way ANOVA)
compare_means_between_groups(housing_complete_df,'mean_dqi','age')
# Basic Education
compare_means_between_groups(housing_complete_df,'mean_dqi','basic_edu')
# Occupational Education
compare_means_between_groups(housing_complete_df,'mean_dqi','occupational_edu')
# Current Situation
compare_means_between_groups(housing_complete_df,'mean_dqi','situation')
# Occupation Group
compare_means_between_groups(housing_complete_df,'mean_dqi','occupational_group')
# Civic life activity
civic_columns = c('written an op-ed to a newspaper', 
                  'written in a blog', 'written on online forums', 
                  'written to an MP', 'written to a municipal councilor',
                  'participated in municipality council', 'participated in committee meetings', 
                  'signed a petition', 'initiated a petition', 
                  'participated in peaceful demonstrations', 
                  'volunteered in a NGO')
for (i in c(1:11)) {
  col = civic_columns[i]
  print(paste('Activity:',col))
  activity_doers = housing_complete_df[is.na(housing_complete_df[,col]),] 
  n_doers = nrow(activity_doers)
  print(paste('Doers:',n_doers))
  inactive_part  = housing_complete_df[!is.na(housing_complete_df[,col]),]
  n_inactive = nrow(inactive_part)
  print(paste('Inactive participants:',n_inactive))
  if (n_doers > 0 && n_inactive > 0) {
    t_test = t.test(activity_doers$mean_dqi,inactive_part$mean_dqi)
    if (t_test$p.value < 0.05) {
      print(t_test) 
      print(paste('Mean doers:',mean(activity_doers$mean_dqi)))
      print(paste('Mean inactive:',mean(inactive_part$mean_dqi)))
    }
  }
}
# Case: Offroad
offroad_survey = prepare_offroadsurvey(offroad_survey)
offroad_complete_df = merge(offroad_survey,offroad_activity_dqi,
                            by.x="email",by.y="email")
# Gender (Independent sample t-test)
t.test(filter(offroad_complete_df,gender==1)$mean_dqi,
       filter(offroad_complete_df,gender==2)$mean_dqi)
# Age (One-way ANOVA)
compare_means_between_groups(offroad_complete_df,'mean_dqi','age')
# Basic Education
compare_means_between_groups(offroad_complete_df,'mean_dqi','basic_edu')
# Occupational Education
compare_means_between_groups(offroad_complete_df,'mean_dqi','occupational_edu')
# Current Situation
compare_means_between_groups(offroad_complete_df,'mean_dqi','situation')
# Occupation Group
compare_means_between_groups(offroad_complete_df,'mean_dqi','occupational_group')
# Civic life activity
for (i in c(1:11)) {
  col = civic_columns[i]
  print(paste('Activity:',col))
  activity_doers = offroad_complete_df[offroad_complete_df[,col]==1,] 
  n_doers = nrow(activity_doers)
  print(paste('Doers:',n_doers))
  inactive_part  = offroad_complete_df[offroad_complete_df[,col]!=1,]
  n_inactive = nrow(inactive_part)
  print(paste('Inactive participants:',n_inactive))
  if (n_doers > 1 && n_inactive > 1) {
    t_test = t.test(activity_doers$mean_dqi,inactive_part$mean_dqi)
    if (t_test$p.value < 0.05) {
      print(t_test) 
      print(paste('Mean doers:',mean(activity_doers$mean_dqi)))
      print(paste('Mean inactive:',mean(inactive_part$mean_dqi)))
    }
  }
}


###
# Search common characteristics in super and low-quality deliberators
###
# Case: Housing
mean(housing_complete_df$mean_dqi)
# 1.Extract top-5 deliberators
housing_top_5_deli = housing_complete_df %>% arrange(desc(mean_dqi)) %>% 
                     filter(!is.na(gender) & !is.na(age)) %>% slice(1:5)
# 2. Select columns related to civic activity
civic_cols = c(1, 8:18)
View(housing_top_5_deli[,civic_cols])
# Get comments authored by super-deliberators
h_comments_sd = filter(housing_comments, author_email %in% housing_top_5_deli$email)
h_comments_sd$datetime = strptime(h_comments_sd$datetime, "%d/%m/%y %H:%M")
# Get ideas in which super-deliberators participated in
housing_ideas$title = remover(housing_ideas$title)
housing_ideas$description = remover(housing_ideas$description)
housing_ideas$tags = remover(housing_ideas$tags)
housing_ideas$title_num_words = vapply(strsplit(housing_ideas$title, "\\W+"), length, integer(1))
housing_ideas$desc_num_words = vapply(strsplit(housing_ideas$description, "\\W+"), length, integer(1))
housing_ideas$num_tags = apply(housing_ideas, 1, function(x) ifelse(x['tags']=='',0,length(strsplit(x['tags'],',')[[1]])))
h_ideas_sd = filter(housing_ideas, idea_id %in% h_comments_sd$idea_id)
h_ideas_nsd = filter(housing_ideas, !idea_id %in% h_comments_sd$idea_id)
# Get info about ideas and comments in which super-deliberators participated in
# Average of ideas' total votes
t.test(h_ideas_sd$total_votes,h_ideas_nsd$total_votes)
# Average of ideas' up votes
t.test(h_ideas_sd$vote_up,h_ideas_nsd$vote_up)
# Average of ideas' up votes
t.test(h_ideas_sd$vote_down,h_ideas_nsd$vote_down)
# Average of ideas'comments
t.test(h_ideas_sd$comments,h_ideas_nsd$comments)
# Get dqi info about ideas in which super-deliberators participated in
h_dqi_sd = filter(housing_dqi, idea_id %in% h_comments_sd$idea_id)
h_dqi_nsd = filter(housing_dqi, !idea_id %in% h_comments_sd$idea_id)
t.test(h_dqi_sd$sum,h_dqi_nsd$sum)
# Title length
t.test(h_ideas_sd$title_num_words,h_ideas_nsd$title_num_words)
# Body length
t.test(h_ideas_sd$desc_num_words,h_ideas_nsd$desc_num_words)
# Tags
t.test(h_ideas_sd$num_tags,h_ideas_nsd$num_tags)
# Check when super-deliberators participated in 
# (at the beggining, at the end, in the middle, etc)
comments_rank = c()
for (i in 1:nrow(h_ideas_sd)) {
  idea = h_ideas_sd[i,]
  idea_comments = filter(housing_comments, idea_id == idea$idea_id)
  idea_comments = idea_comments[order(idea_comments$datetime),]
  num_comments = nrow(idea_comments)
  print(paste('Idea Id:',idea$idea_id))
  print(paste('Num. comments:',nrow(idea_comments)))
  idx_sd = which(idea_comments$author_email %in% housing_top_5_deli$email)
  print('Super-deliberators order')
  print(idx_sd)
  comments_rank = c(comments_rank,idx_sd/num_comments)
}
summary(comments_rank)
###
#
# 3. Extract top-5 low-quality deliberators
housing_top_less_deli = housing_complete_df %>% arrange(mean_dqi) %>% 
  filter(!is.na(gender) & !is.na(age) & !is.na(basic_edu)) %>% slice(1:5)
# 4. Select columns related to civic activity
View(housing_top_less_deli[,civic_cols])
##########
# Case: Offroad
mean(offroad_complete_df$mean_dqi)
# 1. Extract top-5 deliberators
offroad_top_5_deli = offroad_complete_df %>% arrange(desc(mean_dqi)) %>% 
  filter(!is.na(gender) & !is.na(age)) %>% slice(1:5)
# 2. View civic activity
civic_cols = c(1, 11:23)
View(offroad_top_5_deli[,civic_cols])
sum(offroad_top_5_deli$ideas)
sum(offroad_top_5_deli$comments)
sum(offroad_top_5_deli$votes)
# Get comments authored by super-deliberators
o_comments_sd = filter(offroad_comments, author_email %in% offroad_top_5_deli$email)
# Get ideas in which super-deliberators participated in
offroad_ideas$title = remover(offroad_ideas$title)
offroad_ideas$description = remover(offroad_ideas$description)
offroad_ideas$tags = remover(offroad_ideas$tags)
offroad_ideas$title_num_words = vapply(strsplit(offroad_ideas$title, "\\W+"), length, integer(1))
offroad_ideas$desc_num_words = vapply(strsplit(offroad_ideas$description, "\\W+"), length, integer(1))
offroad_ideas$num_tags = apply(offroad_ideas, 1, function(x) ifelse(x['tags']=='',0,length(strsplit(x['tags'],',')[[1]])))
o_ideas_sd = filter(offroad_ideas, idea_id %in% o_comments_sd$idea_id)
o_ideas_nsd = filter(offroad_ideas, !idea_id %in% o_comments_sd$idea_id)
# Get info about ideas and comments in which super-deliberators participated in
# Average of ideas' total votes
t.test(o_ideas_sd$total_votes,o_ideas_nsd$total_votes)
# Average of ideas' up votes
t.test(o_ideas_sd$vote_up,o_ideas_nsd$vote_up)
# Average of ideas' down votes
t.test(o_ideas_sd$vote_down,o_ideas_nsd$vote_down)
# Average of ideas' comments
t.test(o_ideas_sd$comments,o_ideas_nsd$comments)
# Get dqi info about ideas in which super-deliberators participated in
o_dqi_sd = filter(offroad_dqi, idea_id %in% o_comments_sd$idea_id)
o_dqi_nsd = filter(offroad_dqi, !idea_id %in% o_comments_sd$idea_id)
t.test(o_dqi_sd$sum,o_dqi_nsd$sum)
# Title length
t.test(o_ideas_sd$title_num_words,o_ideas_nsd$title_num_words)
# Body length
t.test(o_ideas_sd$desc_num_words,o_ideas_nsd$desc_num_words)
# Tags
t.test(o_ideas_sd$num_tags,o_ideas_nsd$num_tags)
# Check when super-deliberators participated in 
# (at the beggining, at the end, in the middle, etc)
comments_rank = c()
for (i in 1:nrow(o_ideas_sd)) {
  idea = o_ideas_sd[i,]
  idea_comments = filter(offroad_comments, idea_id == idea$idea_id)
  idea_comments = idea_comments[order(idea_comments$datetime),]
  num_comments = nrow(idea_comments)
  print(paste('Idea Id:',idea$idea_id))
  print(paste('Num. comments:',nrow(idea_comments)))
  idx_sd = which(idea_comments$author_email %in% offroad_top_5_deli$email)
  print('Super-deliberators order')
  print(idx_sd)
  comments_rank = c(comments_rank,idx_sd/num_comments)
}
summary(comments_rank)
###

# 3. Extract top-5 low-quality deliberators
offroad_top_less_deli = offroad_complete_df %>% arrange(mean_dqi) %>% 
  filter(!is.na(gender) & !is.na(age)) %>% slice(1:5)
# 4. View civic activity
View(offroad_top_less_deli[,civic_cols])
sum(offroad_top_less_deli$votes)


###
# See if there are participants who deliberative quality varies a lot
# and participants who have middle-level DQI all the time
###
# Case: Housing
sd(housing_complete_df$sd_dqi, na.rm=T)
ggplot(housing_complete_df, aes(x=sd_dqi)) +
  geom_histogram(binwidth=0.5, colour="black", fill="white") +
  scale_x_continuous(breaks=c(0:19)) +
  labs(x="Variation of DQI score", y="Number of participants") +
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20))
# 1.Extract top-5 participants who DQI varies more
housing_top_5_variators = housing_complete_df %>% filter(!is.na(sd_dqi)) %>% 
  arrange(desc(sd_dqi)) %>% filter(!is.na(gender) & !is.na(age)) %>% 
  slice(1:5)
# 2.Extract top-5 participants who DQI varies less
housing_top_5_less_variators = housing_complete_df %>% filter(!is.na(sd_dqi)) %>% 
  arrange(sd_dqi) %>% filter(!is.na(gender) & !is.na(age)) %>% 
  slice(1:5)
# Case: Offroad
sd(housing_complete_df$sd_dqi, na.rm=T)
ggplot(housing_complete_df, aes(x=sd_dqi)) +
  geom_histogram(binwidth=0.5, colour="black", fill="white") +
  scale_x_continuous(breaks=c(0:19)) +
  labs(x="Variation of DQI score", y="Number of participants") +
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20))


###
# Apply clustering algorithm (k-means) to find cluster of users
# Feature vector will be composed of 
# demographic+activity+mean_dqi+sd_dqi
###
# 
# Case: Housing
hdf = dplyr::select(housing_complete_df, -coded_contributions, -total_content)
hdf = filter(hdf, !is.na(sd_dqi))
# Feature selection
# base on http://machinelearningmastery.com/feature-selection-with-the-caret-r-package
# 1- Remove redundant features
# Calculate correlation matrix
#correlationMatrix = cor(hdf[,19:27])
# Summarize the correlation matrix
#print(correlationMatrix)
# Find attributes that are highly corrected (ideally > 0.50)
#highlyCorrelated = findCorrelation(correlationMatrix, cutoff=0.5)
# Print indexes of highly correlated attributes
#print(highlyCorrelated)
# Remove highly correlated attrs
#hdf = dplyr::select(hdf, -votes, -comments)
# 1- Based on my knowledge of the domain
hdf = select(hdf, email, gender, age, basic_edu, occupational_edu, mean_dqi, range_dqi, total_activity)
# 2- Remove records with missing values
hdf = filter(hdf, !is.na(age) & !is.na(gender) & !is.na(basic_edu) & !is.na(occupational_edu))
hdf[is.na(hdf)] = 0
# Convert categorical vars into numerical data type
hdf_k = data.frame(email=hdf$email,mean_dqi=hdf$mean_dqi,range_dqi=hdf$range_dqi,total_activity=hdf$total_activity)
hdf_k$man = ifelse(hdf$gender==2,1,0)
hdf_k$woman = ifelse(hdf$gender==1,1,0)
hdf_k[,'26_34'] = ifelse(hdf$age==4,1,0)
hdf_k[,'35_54'] = ifelse(hdf$age==5,1,0)
hdf_k[,'55_64'] = ifelse(hdf$age==6,1,0)
hdf_k[,'65_more'] = ifelse(hdf$age==7,1,0)
hdf_k$elementary = ifelse(hdf$basic_edu==1,1,0)
hdf_k$middle = ifelse(hdf$basic_edu==2,1,0)
hdf_k$secondary_grad = ifelse(hdf$basic_edu==4,1,0)
hdf_k$noformalb = ifelse(hdf$basic_edu==5,1,0)
hdf_k$vocational = ifelse(hdf$occupational_edu==2,1,0)
hdf_k$college = ifelse(hdf$occupational_edu==3,1,0)
hdf_k$stu_uni = ifelse(hdf$occupational_edu==4,1,0)
hdf_k$science_degree = ifelse(hdf$occupational_edu==5,1,0)
hdf_k$bachelor = ifelse(hdf$occupational_edu==6,1,0)
hdf_k$master = ifelse(hdf$occupational_edu==7,1,0)
hdf_k$phd = ifelse(hdf$occupational_edu==8,1,0)
hdf_k$noformalo = ifelse(hdf$occupational_edu==9,1,0)
# Discard highly correlated features
correlationMatrix = cor(hdf_k[2:22])
highlycorr = findCorrelation(correlationMatrix, cutoff=0.5)
colnames(hdf_k)[highlycorr]
hdf_k = hdf_k[, -highlycorr]
# Remove civic columns, which aren't independent;
# a participant can do more than one civic activity at the same time
# hdf = hdf[,c(1:5,19:22)]
# Fit k-means algorithm
#fit = kmodes(hdf[2:22], 2, iter.max = 10, weighted = FALSE )
fit = kmeans(hdf_k[2:19], 3)
hdf_k$cluster = fit$cluster
# Draw cluster plot
clusplot(hdf_k[2:20], fit$cluster, color=TRUE, shade=TRUE, lines=0)
# Compute cluster profiles
hdf = merge(hdf,select(hdf_k,email,cluster),by.x='email',by.y='email')
get_group_profile(filter(hdf,cluster==1))
get_group_profile(filter(hdf,cluster==2))
get_group_profile(filter(hdf,cluster==3))
