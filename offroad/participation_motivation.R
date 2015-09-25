
# Reading Data
users_camp_vies = read.csv("./data/users_camp_vies.csv",header=TRUE,sep=";",stringsAsFactors=FALSE)
users_camp_hall = read.csv("./data/users_camp_hall.csv",header=TRUE,sep=";",stringsAsFactors=FALSE)
users_camp_erim = read.csv("./data/users_camp_erim.csv",header=TRUE,sep=";",stringsAsFactors=FALSE)
ans_q1_s1 = read.csv("./data/answers_q1_survey1_email.csv",header=TRUE,sep=";",stringsAsFactors=FALSE)
ans_q9_s2 = read.csv("./data/answers_q9_survey2_email.csv",header=TRUE,sep=";",stringsAsFactors=FALSE)

# Aggreate participation of the 3 campaigns
total_users = users_camp_vies
for (id in unique(users_camp_hall$id)) {
  pos = match(id,total_users$id,-1)
  if (pos == -1) {
    total_users = rbind(total_users,users_camp_hall[users_camp_hall$id==id,])
  } else {
    total_users[pos,"ideas"] = total_users[pos,"ideas"] + users_camp_hall[users_camp_hall$id==id,"ideas"]
    total_users[pos,"votes"] = total_users[pos,"votes"] + users_camp_hall[users_camp_hall$id==id,"votes"]
    total_users[pos,"comments"] = total_users[pos,"comments"] + users_camp_hall[users_camp_hall$id==id,"comments"]
  }
}
for (id in unique(users_camp_erim$id)) {
  pos = match(id,total_users$id,-1)
  if (pos == -1) {
    total_users = rbind(total_users,users_camp_erim[users_camp_erim$id==id,])
  } else {
    total_users[pos,"ideas"] = total_users[pos,"ideas"] + users_camp_erim[users_camp_erim$id==id,"ideas"]
    total_users[pos,"votes"] = total_users[pos,"votes"] + users_camp_erim[users_camp_erim$id==id,"votes"]
    total_users[pos,"comments"] = total_users[pos,"comments"] + users_camp_erim[users_camp_erim$id==id,"comments"] 
  }
}

remove(users_answered_survey2)
# Discard users that didn't reply the survey 1
for (email in total_users$email) {
  if (email %in% ans_q1_s1$email) {
    answers = ans_q1_s1[ans_q1_s1$email==email,2:8]
    production = total_users[total_users$email==email,]
    production = cbind(production,answers[1,])   # Only the first answers of the user will be saved in case the user answered more than once
    if (exists("users_answered_survey1")) {
      users_answered_survey1 = rbind(users_answered_survey1,production) 
    } else {
      users_answered_survey1 = production 
    }
  }
}

remove(users_answered_survey2)
# Discard users that didn't reply the survey 2
for (email in total_users$email) {
  if (email %in% ans_q9_s2$email) {
    answers = ans_q9_s2[ans_q9_s2$email==email,2:9]
    production = total_users[total_users$email==email,]
    production = cbind(production,answers[1,])   # Only the first answers of the user will be saved in case the user answered more than once
    if (exists("users_answered_survey2")) {
      users_answered_survey2 = rbind(users_answered_survey2,production) 
    } else {
      users_answered_survey2 = production 
    }
  }
}

# Discard users that didn't reply both surveys
remove(users_answered_both_surveys)
for (email in total_users$email) {
  if (email %in% ans_q1_s1$email && email %in% ans_q9_s2$email) {
    answers_s1 = ans_q1_s1[ans_q1_s1$email==email,2:8]
    colnames(answers_s1) = c("o1_s1","o2_s1","o3_s1","o4_s1","o5_s1","o6_s1","o7_s1")
    answers_s2 = ans_q9_s2[ans_q9_s2$email==email,2:9]
    colnames(answers_s2) = c("o1_s2","o2_s2","o3_s2","o4_s2","o5_s2","o6_s2","o7_s2","o8_s2")
    production = total_users[total_users$email==email,]
    production = cbind(production,answers_s1[1,])   # Only the first answers of the user will be saved in case the user answered more than once
    production = cbind(production,answers_s2[1,])
    if (exists("users_answered_both_surveys")) {
      users_answered_both_surveys = rbind(users_answered_both_surveys,production) 
    } else {
      users_answered_both_surveys = production 
    }
  }
}

# Remove NA and Transform columns to numeric (survey 1)
users_answered_survey1$option1[is.na(users_answered_survey1$option1)] = "0"
users_answered_survey1$option2[is.na(users_answered_survey1$option2)] = "0"
users_answered_survey1$option3[is.na(users_answered_survey1$option3)] = "0"
users_answered_survey1$option4[is.na(users_answered_survey1$option4)] = "0"
users_answered_survey1$option5[is.na(users_answered_survey1$option5)] = "0"
users_answered_survey1$option6[is.na(users_answered_survey1$option6)] = "0"
users_answered_survey1$option7[is.na(users_answered_survey1$option7)] = "0"
users_answered_survey1$option1 = as.numeric(users_answered_survey1$option1)
users_answered_survey1$option2 = as.numeric(users_answered_survey1$option2)
users_answered_survey1$option3 = as.numeric(users_answered_survey1$option3)
users_answered_survey1$option4 = as.numeric(users_answered_survey1$option4)
users_answered_survey1$option5 = as.numeric(users_answered_survey1$option5)
users_answered_survey1$option6 = as.numeric(users_answered_survey1$option6)
users_answered_survey1$option7 = as.numeric(users_answered_survey1$option7)

# Remove NA and Transform columns to numeric (survey 2)
users_answered_survey2$option1[is.na(users_answered_survey2$option1)] = "0"
users_answered_survey2$option2[is.na(users_answered_survey2$option2)] = "0"
users_answered_survey2$option3[is.na(users_answered_survey2$option3)] = "0"
users_answered_survey2$option4[is.na(users_answered_survey2$option4)] = "0"
users_answered_survey2$option5[is.na(users_answered_survey2$option5)] = "0"
users_answered_survey2$option6[is.na(users_answered_survey2$option6)] = "0"
users_answered_survey2$option7[is.na(users_answered_survey2$option7)] = "0"
users_answered_survey2$option8[is.na(users_answered_survey2$option8)] = "0"
users_answered_survey2$option1 = as.numeric(users_answered_survey2$option1)
users_answered_survey2$option2 = as.numeric(users_answered_survey2$option2)
users_answered_survey2$option3 = as.numeric(users_answered_survey2$option3)
users_answered_survey2$option4 = as.numeric(users_answered_survey2$option4)
users_answered_survey2$option5 = as.numeric(users_answered_survey2$option5)
users_answered_survey2$option6 = as.numeric(users_answered_survey2$option6)
users_answered_survey2$option7 = as.numeric(users_answered_survey2$option7)
users_answered_survey2$option8 = as.numeric(users_answered_survey2$option8)

# Remove NA and Transform columns to numeric (both surveys)
users_answered_both_surveys$o1_s1[is.na(users_answered_both_surveys$o1_s1)] = "0"
users_answered_both_surveys$o2_s1[is.na(users_answered_both_surveys$o2_s1)] = "0"
users_answered_both_surveys$o3_s1[is.na(users_answered_both_surveys$o3_s1)] = "0"
users_answered_both_surveys$o4_s1[is.na(users_answered_both_surveys$o4_s1)] = "0"
users_answered_both_surveys$o5_s1[is.na(users_answered_both_surveys$o5_s1)] = "0"
users_answered_both_surveys$o6_s1[is.na(users_answered_both_surveys$o6_s1)] = "0"
users_answered_both_surveys$o7_s1[is.na(users_answered_both_surveys$o7_s1)] = "0"
users_answered_both_surveys$o1_s2[is.na(users_answered_both_surveys$o1_s2)] = "0"
users_answered_both_surveys$o2_s2[is.na(users_answered_both_surveys$o2_s2)] = "0"
users_answered_both_surveys$o3_s2[is.na(users_answered_both_surveys$o3_s2)] = "0"
users_answered_both_surveys$o4_s2[is.na(users_answered_both_surveys$o4_s2)] = "0"
users_answered_both_surveys$o5_s2[is.na(users_answered_both_surveys$o5_s2)] = "0"
users_answered_both_surveys$o6_s2[is.na(users_answered_both_surveys$o6_s2)] = "0"
users_answered_both_surveys$o7_s2[is.na(users_answered_both_surveys$o7_s2)] = "0"
users_answered_both_surveys$o8_s2[is.na(users_answered_both_surveys$o8_s2)] = "0"
users_answered_both_surveys$o1_s1 = as.numeric(users_answered_both_surveys$o1_s1)
users_answered_both_surveys$o2_s1 = as.numeric(users_answered_both_surveys$o2_s1)
users_answered_both_surveys$o3_s1 = as.numeric(users_answered_both_surveys$o3_s1)
users_answered_both_surveys$o4_s1 = as.numeric(users_answered_both_surveys$o4_s1)
users_answered_both_surveys$o5_s1 = as.numeric(users_answered_both_surveys$o5_s1)
users_answered_both_surveys$o6_s1 = as.numeric(users_answered_both_surveys$o6_s1)
users_answered_both_surveys$o7_s1 = as.numeric(users_answered_both_surveys$o7_s1)
users_answered_both_surveys$o1_s2 = as.numeric(users_answered_both_surveys$o1_s2)
users_answered_both_surveys$o2_s2 = as.numeric(users_answered_both_surveys$o2_s2)
users_answered_both_surveys$o3_s2 = as.numeric(users_answered_both_surveys$o3_s2)
users_answered_both_surveys$o4_s2 = as.numeric(users_answered_both_surveys$o4_s2)
users_answered_both_surveys$o5_s2 = as.numeric(users_answered_both_surveys$o5_s2)
users_answered_both_surveys$o6_s2 = as.numeric(users_answered_both_surveys$o6_s2)
users_answered_both_surveys$o7_s2 = as.numeric(users_answered_both_surveys$o7_s2)
users_answered_both_surveys$o8_s2 = as.numeric(users_answered_both_surveys$o8_s2)

# Comparing means
diff_o1_o1 = t.test(users_answered_both_surveys$o1_s1,users_answered_both_surveys$o1_s2)
diff_o2_o2 = t.test(subset_q1_s1$option2,subset_q9_s2$option2)
diff_o4_o4 = t.test(subset_q1_s1$option4,subset_q9_s2$option4)
diff_o5_o5 = t.test(subset_q1_s1$option5,subset_q9_s2$option5)
diff_o6_o6 = t.test(subset_q1_s1$option6,subset_q9_s2$option6)
diff_o7_o7 = t.test(subset_q1_s1$option7,subset_q9_s2$option7)
diff_o3_o3 = t.test(subset_q1_s1$option3,subset_q9_s2$option3)

tot_surveyed1 = nrow(users_answered_survey1)
tot_surveyed2 = nrow(users_answered_survey2)
tot_both_surveys = nrow(users_answered_both_surveys)

# Survey 1
summary(users_answered_survey1$ideas)
no_ideas = length(users_answered_survey1[users_answered_survey1$ideas==0,"email"])
no_ideas * 100 / tot_surveyed1

summary(users_answered_survey1$votes)
no_votes = length(users_answered_survey1[users_answered_survey1$votes==0,"email"])
no_votes * 100 / tot_surveyed1

summary(users_answered_survey1$comments)
no_comments = length(users_answered_survey1[users_answered_survey1$comments==0,"email"])
no_comments * 100 / tot_surveyed1

observers_s1 = length(users_answered_survey1[users_answered_survey1$ideas==0&
                                             users_answered_survey1$votes==0&
                                             users_answered_survey1$comments==0,"email"])
observers_s1 * 100 / tot_surveyed1

# Survey 2
summary(users_answered_survey2$ideas)
no_ideas = length(users_answered_survey2[users_answered_survey2$ideas==0,"email"])
no_ideas * 100 / tot_surveyed2

summary(users_answered_survey2$votes)
no_votes = length(users_answered_survey2[users_answered_survey2$votes==0,"email"])
no_votes * 100 / tot_surveyed2

summary(users_answered_survey2$comments)
no_comments = length(users_answered_survey2[users_answered_survey2$comments==0,"email"])
no_comments * 100 / tot_surveyed2

observers_s2 = length(users_answered_survey2[users_answered_survey2$ideas==0&
                                             users_answered_survey2$votes==0&
                                             users_answered_survey2$comments==0,"email"])
tot_surveyed2 - observers_s2
observers_s2 * 100 / tot_surveyed2

# Both Surveys
observers_both = length(users_answered_both_surveys[users_answered_both_surveys$ideas==0&
                                                    users_answered_both_surveys$votes==0&
                                                    users_answered_both_surveys$comments==0,"email"])
tot_both_surveys-observers_both
observers_both * 100 / tot_both_surveys

# Calculate correlations (Survey 1)
cor(users_answered_survey1$ideas,users_answered_survey1$option1)
cor(users_answered_survey1$ideas,users_answered_survey1$option2)
cor(users_answered_survey1$ideas,users_answered_survey1$option3)
cor(users_answered_survey1$ideas,users_answered_survey1$option4)
cor(users_answered_survey1$ideas,users_answered_survey1$option5)
cor(users_answered_survey1$ideas,users_answered_survey1$option6)
cor(users_answered_survey1$ideas,users_answered_survey1$option7)

ideators_s1 = users_answered_survey1[users_answered_survey1$ideas>0,]
cor(ideators_s1$ideas,ideators_s1$option1)
cor(ideators_s1$ideas,ideators_s1$option2)
cor(ideators_s1$ideas,ideators_s1$option3)
cor(ideators_s1$ideas,ideators_s1$option4)
cor(ideators_s1$ideas,ideators_s1$option5)
cor(ideators_s1$ideas,ideators_s1$option6)
cor(ideators_s1$ideas,ideators_s1$option7)

cor(users_answered_survey1$comments,users_answered_survey1$option1)
cor(users_answered_survey1$comments,users_answered_survey1$option2)
cor(users_answered_survey1$comments,users_answered_survey1$option3)
cor(users_answered_survey1$comments,users_answered_survey1$option4)
cor(users_answered_survey1$comments,users_answered_survey1$option5)
cor(users_answered_survey1$comments,users_answered_survey1$option6)
cor(users_answered_survey1$comments,users_answered_survey1$option7)

commenters_s1 = users_answered_survey1[users_answered_survey1$comments>0,]
cor(commenters_s1$comments,commenters_s1$option1)
cor(commenters_s1$comments,commenters_s1$option2)
cor(commenters_s1$comments,commenters_s1$option3)
cor(commenters_s1$comments,commenters_s1$option4)
cor(commenters_s1$comments,commenters_s1$option5)
cor(commenters_s1$comments,commenters_s1$option6)
cor(commenters_s1$comments,commenters_s1$option7)

cor(users_answered_survey1$votes,users_answered_survey1$option1)
cor(users_answered_survey1$votes,users_answered_survey1$option2)
cor(users_answered_survey1$votes,users_answered_survey1$option3)
cor(users_answered_survey1$votes,users_answered_survey1$option4)
cor(users_answered_survey1$votes,users_answered_survey1$option5)
cor(users_answered_survey1$votes,users_answered_survey1$option6)
cor(users_answered_survey1$votes,users_answered_survey1$option7)

# Voters
voters_s1 = users_answered_survey1[users_answered_survey1$votes>0,]
cor(voters_s1$votes,voters_s1$option1)
cor(voters_s1$votes,voters_s1$option2)
cor(voters_s1$votes,voters_s1$option3)
cor(voters_s1$votes,voters_s1$option4)
cor(voters_s1$votes,voters_s1$option5)
cor(voters_s1$votes,voters_s1$option6)
cor(voters_s1$votes,voters_s1$option7)

ggplot(voters_s1, aes(votes, option1)) + 
  geom_point(shape=1) + labs(x="Votes Count", y="Answer Factor 1") +
  scale_y_continuous(breaks=c(0:10))

# Calculate correlations (Survey 2)
cor(users_answered_survey2$ideas,users_answered_survey2$option1)
cor(users_answered_survey2$ideas,users_answered_survey2$option2)
cor(users_answered_survey2$ideas,users_answered_survey2$option3)
cor(users_answered_survey2$ideas,users_answered_survey2$option4)
cor(users_answered_survey2$ideas,users_answered_survey2$option5)
cor(users_answered_survey2$ideas,users_answered_survey2$option6)
cor(users_answered_survey2$ideas,users_answered_survey2$option7)
cor(users_answered_survey2$ideas,users_answered_survey2$option8)

# Ideators
ideators_s2 = users_answered_survey2[users_answered_survey2$ideas>0,]
cor(ideators_s2$ideas,ideators_s2$option1)
cor(ideators_s2$ideas,ideators_s2$option2)
cor(ideators_s2$ideas,ideators_s2$option3)
cor(ideators_s2$ideas,ideators_s2$option4)
cor(ideators_s2$ideas,ideators_s2$option5)
cor(ideators_s2$ideas,ideators_s2$option6)
cor(ideators_s2$ideas,ideators_s2$option7)
cor(ideators_s2$ideas,ideators_s2$option8)

# Comments
cor(users_answered_survey2$comments,users_answered_survey2$option1)
cor(users_answered_survey2$comments,users_answered_survey2$option2)
cor(users_answered_survey2$comments,users_answered_survey2$option3)
cor(users_answered_survey2$comments,users_answered_survey2$option4)
cor(users_answered_survey2$comments,users_answered_survey2$option5)
cor(users_answered_survey2$comments,users_answered_survey2$option6)
cor(users_answered_survey2$comments,users_answered_survey2$option7)
cor(users_answered_survey2$comments,users_answered_survey2$option8)

# Commenters
commenters_s2 = users_answered_survey2[users_answered_survey2$comments>0,]
cor(commenters_s2$comments,commenters_s2$option1)
cor(commenters_s2$comments,commenters_s2$option2)
cor(commenters_s2$comments,commenters_s2$option3)
cor(commenters_s2$comments,commenters_s2$option4)
cor(commenters_s2$comments,commenters_s2$option5)
cor(commenters_s2$comments,commenters_s2$option6)
cor(commenters_s2$comments,commenters_s2$option7)

# Votes
cor(users_answered_survey2$votes,users_answered_survey2$option1)
cor(users_answered_survey2$votes,users_answered_survey2$option2)
cor(users_answered_survey2$votes,users_answered_survey2$option3)
cor(users_answered_survey2$votes,users_answered_survey2$option4)
cor(users_answered_survey2$votes,users_answered_survey2$option5)
cor(users_answered_survey2$votes,users_answered_survey2$option6)
cor(users_answered_survey2$votes,users_answered_survey2$option7)

# Voters
voters_s2 = users_answered_survey2[users_answered_survey2$votes>0,]
cor(voters_s2$votes,voters_s2$option1)
cor(voters_s2$votes,voters_s2$option2)
cor(voters_s2$votes,voters_s2$option3)
cor(voters_s2$votes,voters_s2$option4)
cor(voters_s2$votes,voters_s2$option5)
cor(voters_s2$votes,voters_s2$option6)
cor(voters_s2$votes,voters_s2$option7)