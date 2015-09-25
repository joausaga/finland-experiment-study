remove_outliers_dataset = function(dataset, var_name) {
  summary_var = summary(dataset[,var_name])
  first_q = summary_var[2]
  third_q = summary_var[5]
  iqr = third_q-first_q
  inner_fence_up = third_q + (iqr*1.5)
  inner_fence_down = first_q - (iqr*1.5)
  outer_fence_up = third_q + (iqr*3)
  outer_fence_down = first_q - (iqr*3)
  
  return(filter(dataset, dataset[,var_name] <= outer_fence_up))
} 

check_normality = function(vec) {
  res = sf.test(vec)
  return (res$p.value > 0.05)
}

remove_few_representative_groups = function(dataset, groups){
  groups_to_remove = c()
  for (group in groups) {
    if (nrow(dataset[dataset$group==group,]) <= 2) {
      groups_to_remove = c(groups_to_remove,as.character(group))
    }  
  }
  if (!is.null(groups_to_remove)) {
    new_ds = filter(dataset,!group%in%groups_to_remove)
    groups = groups[-which(groups%in%groups_to_remove)] 
  } else {
    new_ds = dataset
  }
  ret = list(new_ds,groups)
  
  return (ret)
}

analyze_content_production = function(dataset, type_content, groups) {
  #content_no_outliers = remove_outliers_dataset(dataset,type_content)
  content_no_outliers = dataset
  max = -1
  means = c()
  medians = c()
  ns = c()
  for (idx in 1:length(groups)) {
    ns = c(ns, length(content_no_outliers[content_no_outliers$group==groups[idx],type_content]))
    median = median(content_no_outliers[content_no_outliers$group==groups[idx],type_content])
    mean = mean(content_no_outliers[content_no_outliers$group==groups[idx],type_content])
    if (!is.na(mean) & !is.na(median)) {
      if (mean > max) { max=mean}  
      medians = c(medians, round(as.numeric(median),3))
      means = c(means, round(as.numeric(mean),3))
    }
    else {
      if (is.na(mean)) { means = c(means, 0) }
      if (is.na(median)) {medians = c(medians, 0)}
    }
  }
  
  max = max(medians)
  if (max == 0) {
    max = max(means)
    idx_max = match(max,means)
  }
  else {
    idx_max = match(max,medians)
  }
  max_group = groups[idx_max]
  
  
  ret = remove_few_representative_groups(content_no_outliers,groups)
  content_no_outliers = ret[[1]]
  groups = ret[[2]]
  data_normal = check_normality(content_no_outliers[,type_content])
  
  if (data_normal) {
    res = oneway.test(content ~ group, data=content_no_outliers)  
    type_test = "oneway"
  }
  else {
      data = list()
      for (group in groups) {
        data[[group]] = content_no_outliers[content_no_outliers$group==group,type_content]
      }
      res = kruskal.test(data)
      type_test = "kruskal"
  }
  
  ret = list(max=max_group,p_value=res$p.value,test=type_test,means=means,medians=medians,ns=ns,
             test_result=res)
  
  return (ret)
}

get_report_activity_level = function(dataset,groups) {
  rep = list()
  ret = analyze_content_production(dataset,'ideas',groups) 
  rep[['ideas']] = ret
  ret = analyze_content_production(dataset,'votes',groups) 
  rep[['votes']] = ret
  ret = analyze_content_production(dataset,'comments',groups) 
  rep[['comments']] = ret
  ret = analyze_content_production(dataset,'content',groups) 
  rep[['content']] = ret
  
  return (rep)
}