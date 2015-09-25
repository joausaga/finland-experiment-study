# Loading data
raw_survey = read.csv("./data/surveyofftrafficlaw.csv", sep=",", header=T, stringsAsFactors=F)
survey = raw_survey %>% select(V5, Q23:Q33) %>% select(-Q28)
survey = survey[-1,]
rownames(survey) = NULL
colnames(survey) = c('email','gender','age','basic_edu','occupational_edu','situation',
                     'occupational_group','living_area','living_zone','time_online','political_party')
participant_ideas = read.xlsx("./data/ideas.xls", sheetIndex=1, colIndex=c(5,13))

# Pre-processing and transformation
# Filter out those that didn't indicate their gender
survey = survey %>% filter(gender!='') %>% select(email,gender)
# Rename columns
colnames(participant_ideas) = c('email','idea_body')
# Filter out those records without email address
participant_ideas = participant_ideas %>% filter(email!='')
# Convert email column to character class
participant_ideas$email = as.character(participant_ideas$email)
# Convert idea body column to character class
participant_ideas$idea_body = as.character(participant_ideas$idea_body)
# Filter out those participants that didn't answer the survey
participant_ideas_survey = participant_ideas %>% filter(email %in% survey$email)
# Add a column to save the gender of the idea author
for (email in survey$email) {
  gender = survey[survey$email==email,'gender']
  participant_ideas_survey$gender[participant_ideas_survey$email==email] = gender
}
# Add a column to hold the gender groups
groups = c('female','male')
# Create a column to contain the groups
for (idx in 1:length(groups)) {
  participant_ideas_survey$group[participant_ideas_survey$gender==idx] = groups[idx]
}
# Add a column to hold the length of the idea
participant_ideas_survey = participant_ideas_survey %>% mutate(body_length=nchar(idea_body))


# Analyze whether the groups produce ideas of different length and if the difference is significant
ret = analyze_content_production(participant_ideas_survey,'body_length',groups) 

# Mean of male's idea length 
ret$means[2]

# Mean of female's idea length 
ret$means[1]

# Find out if the difference is significant
if (ret$p_value <= 0.05) {
  print(paste('The difference is significant, p-value= ',round(ret$p_value,3),sep=''))
} else {
  print(paste('The difference is not significant, p-value= ',round(ret$p_value,3),sep=''))
}