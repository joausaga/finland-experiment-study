discover_uncoded_registers = function(data) {
  uncoded_registers = c()
  for (i in 1:nrow(data)) {
    for (j in 1:ncol(data)) {
      if (is.na(data[i,j])) {
        uncoded_registers = c(uncoded_registers,i)
        break
      }
    } 
  }
  
  return (uncoded_registers)
}

add_column_to_hold_comment_id = function(dataset) {
  for (idx in 1: nrow(dataset)) {
    if (!is.na(dataset[idx,"idea_id"])) {
      idea_id = dataset[idx,"idea_id"]
    }
    dataset[idx,"comment_id"] = paste(idea_id, '_', dataset[idx,"comment_num"], sep='')
  }
  
  return (dataset)
}

prepare_dqi_dataset = function(dataset) {
  # Rename colums
  colnames(dataset)[21] = "type_justification_noise"
  colnames(dataset)[30] = "story_telling"
  colnames(dataset)[34] = "constructive_character_exchanges_question_normal_questions"
  # Add column for comment id
  dataset = add_column_to_hold_comment_id(dataset)
  
  return (dataset)
}

prepare_data = function(dataset,include_first_part) {
  if (include_first_part) {
    dataset = prepare_dqi_dataset(dataset) 
  }
  # Filter out uncoded content
  uncoded_content = discover_uncoded_registers(select(dataset,-idea_id,-comment_num,-comment_count, 
                                                      -comment, -comment_id, -idea, -The.tone, -category,
                                                      -group_supporting, -word_count))
  dataset = dataset[-uncoded_content,]
  rownames(dataset) = NULL
  # Remove remaining unused column
  dataset = select(dataset, -comment_num, -comment, -idea, -The.tone)
  # Code 2 doesn't exist for category Content Justification
  # so content having this code will be removed (one record)
  dataset = filter(dataset, !common_good_orientation_content_justifications=="2")
  # Add a dummy variable to measure if there had been an appeal to
  # the common good (stated either in utilitarian terms or in terms 
  # of the difference principle)
  dataset = mutate(dataset, 
                   common_good=ifelse(common_good_orientation_content_justifications == "2a" |
                                      common_good_orientation_content_justifications == "2b",
                                      1,0))
  # Transform values of column 'common_good_orientation_content_justifications'
  # by replacing 2a and 2b values for 2
  print(table(dataset$common_good_orientation_content_justifications))
  dataset[dataset$common_good_orientation_content_justifications=="2a",
          "common_good_orientation_content_justifications"] = "2"
  dataset[dataset$common_good_orientation_content_justifications=="2b",
          "common_good_orientation_content_justifications"] = "2"
  # Convert columns to numeric
  for (col in colnames(dataset)) {
    if (col != "idea_id" && col != "comment_id" && 
        col != "category" && col != "group_supporting" &&
        col != "author_email" && col != "comment_count") {
      dataset[,col] = as.numeric(dataset[,col])    
    }
  }
  # Remove Finnish special characters
  if("category" %in% colnames(dataset)) {
    dataset$category = remover(dataset$category)
  }
  if("group_supporting" %in% colnames(dataset)) {
    dataset$group_supporting = remover(dataset$group_supporting)
  }
  
  return (dataset)
}

compute_dqi = function(dataset) {
  print(paste("Mean:",mean(dataset)))
  print(paste("Median:", median(dataset)))
  print(paste("Standard Dev.:", sd(dataset)))
  print(paste("Min:", min(dataset)))
  print(paste("Max:", max(dataset)))
}

build_averaged_dataset = function(coder1, coder2, indexcol) {
  avg_set = c()
  for (reg_id in unique(coder1[,indexcol])) {
    c1 = coder1[coder1[,indexcol]==reg_id,]
    c2 = coder2[coder2[,indexcol]==reg_id,]
    avg = colMeans(rbind(select(c1,-idea_id,-comment_id),
                         select(c2,-idea_id,-comment_id)))
    avg_set = rbind(avg_set, avg)
  }
  rownames(avg_set) = NULL
  
  return (data.frame(avg_set))
}

compute_dqi_by_indicators = function(dataset, indicators) {
  for (indicator in indicators) {
    if (indicator != 'idea_id' && indicator != 'comment_id' &&
        indicator != 'category' && indicator != 'group_supporting' &&
        indicator != 'word_count' && indicator != 'comment_count') {
      print(paste("Indicator:",indicator))
      print(paste("Mean:",mean(dataset[,indicator]),
                  "-- Median:", median(dataset[,indicator]),
                  "-- Standard Dev.:", sd(dataset[,indicator]),
                  "-- Min:", min(dataset[,indicator]),
                  "-- Max:", max(dataset[,indicator])))
      print("###########") 
    }
  }
}

compute_frequency_by_category = function(dataset, categories) {
  for (category in categories) {
    print(paste("Category:",category))
    print(table(dataset[,category]))
    print("###########")
  }
}

analyze_signifance_of_differences = function(dataset1, dataset2, categories) {
  for (category in categories) {
    if (category != "participation_equality") {
      print(paste("Category:",category))
      print(t.test(dataset1[,category],dataset2[,category]))
      print("###########") 
    }
  }
}

# Remove Finnish special characters
remover = function(x){
  x <- gsub('\x8a', 'a', x)
  x <- gsub('\x9a', 'o', x)
  return(x)
}

# As recommended by Steenbergen the simplest way
# to build the DQI is to add up the indicators
build_dqi = function(dataset) {
  # Remove unused columns
  # coded_dataset = select(dataset, -comment_id, -idea_id, -category, -group_supporting)
  # Select only indicators that should be included in DQI's calculation
  coded_dataset = select(dataset, level_justification_rationality, respect_groups, respect_demands_others, respect_counterarguments,
                         constructive_politics, interactivity, common_good)
  # Remove record that was wrongly coded
  # coded_dataset= filter(coded_dataset, participation_equality!='2')
  # Sum up columns (indicators) by rows
  datasetsum_cats = rowSums(coded_dataset)
  # Compute DQI
  compute_dqi(datasetsum_cats)
  
  return (datasetsum_cats)
}