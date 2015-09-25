check_rel = function(dataset, var) {
  for (i in 1:7) {
    cols2 = paste('o',i,'s2',sep='')
    cols1 = paste('o',i,'s1',sep='')
    new_col = paste('diffo',i,sep='')
    dataset[,new_col] = ifelse(dataset[,cols1] - dataset[,cols2] != 0, 'change', 'no_change')
    table_gen = table(dataset[,var], dataset[,new_col])
    print(table_gen)
    p_value = chisq.test(table_gen, simulate.p.value=T)$p.value
    cat(paste('p value: ',p_value,'\n',sep=''))
    if (p_value < 0.05) {
      cat(paste('There is significant relationship between ',var,' and the motivation factor ',i,'\n',sep=''))
    }
  }  
}

check_rel_three_surveys = function(dataset, var, num_factors) {
  for (i in 1:num_factors) {
    cat(paste('\nFactor: ',i,'\n',sep=''))
    cols2 = paste('o',i,'s2',sep='')
    cols1 = paste('o',i,'s1',sep='')
    cols3 = paste('o',i,'s3',sep='')
    new_col = paste('diffo',i,sep='')
    dataset[,new_col] = ifelse(dataset[,cols1] == dataset[,cols2] & 
                               dataset[,cols1] == dataset[,cols3] &
                               dataset[,cols2] == dataset[,cols3], 'no_change', 'change')
    table_gen = table(dataset[,var], dataset[,new_col])
    print(table_gen)
    p_value = chisq.test(table_gen, simulate.p.value=T)$p.value
    cat(paste('p value: ',p_value,'\n',sep=''))
    if (p_value < 0.05) {
      cat(paste('There is significant relationship between ',var,' and the motivation factor ',i,'\n',sep=''))
    }
  }  
}

create_joint_dataset = function(part_campaign, dataset) {
  for (id in unique(part_campaign$id)) {
    pos = match(id,dataset$id,-1)
    if (pos == -1) {
      dataset = rbind(dataset,part_campaign[part_campaign$id==id,])
    } else {
      dataset[pos,'ideas'] = dataset[pos,'ideas'] + part_campaign[part_campaign$id==id,'ideas']
      dataset[pos,'votes'] = dataset[pos,'votes'] + part_campaign[part_campaign$id==id,'votes']
      dataset[pos,'comments'] = dataset[pos,'comments'] + part_campaign[part_campaign$id==id,'comments']
    }
  }  
  
  return (dataset)
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

analyze_significance = function(dataset, type_content, groups) {
  #dataset = all_surveys
  #type_content = 'F1'
  #groups = c('survey_1','survey_2', 'survey_3')
  
  content_no_outliers = dataset
  max = -1
  means = c()
  medians = c()
  ns = c()
  idx=1
  for (idx in 1:length(groups)) {
    ns = c(ns, length(content_no_outliers[content_no_outliers$group==groups[idx],type_content]))
    median = median(content_no_outliers[content_no_outliers$group==groups[idx],type_content], na.rm=T)
    mean = mean(content_no_outliers[content_no_outliers$group==groups[idx],type_content], na.rm=T)
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
    res = oneway.test(content_no_outliers[,type_content] ~ group, data=content_no_outliers)  
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

get_report_groups = function(dataset,groups) {
  rep = list()
  ret = analyze_significance(dataset,'F1',groups) 
  rep[['F1']] = ret
  ret = analyze_significance(dataset,'F2',groups) 
  rep[['F2']] = ret
  ret = analyze_significance(dataset,'F3',groups) 
  rep[['F3']] = ret
  ret = analyze_significance(dataset,'F4',groups) 
  rep[['F4']] = ret
  ret = analyze_significance(dataset,'F5',groups) 
  rep[['F5']] = ret
  ret = analyze_significance(dataset,'F6',groups) 
  rep[['F6']] = ret
  
  return (rep)
}

get_dynamic_report_diff_groups = function(dataset,groups,vars) {
  rep = list()
  for (var in vars) {
    ret = analyze_significance(dataset,var,groups) 
    rep[[var]] = ret
  }
  return (rep)
}

prepare_data = function(ds1, ds2, var1, var2) {
  na_emails = ds1[is.na(ds1[,var1]),'email']
  ds1 = ds1 %>% filter(!email %in% na_emails)
  ds2 = ds2 %>% filter(!email %in% na_emails)
  na_emails = ds1[ds1[,var1]=='','email']
  ds1 = ds1 %>% filter(!email %in% na_emails)
  ds2 = ds2 %>% filter(!email %in% na_emails)
  na_emails = ds2[is.na(ds2[,var2]),'email']
  ds1 = ds1 %>% filter(!email %in% na_emails)
  ds2 = ds2 %>% filter(!email %in% na_emails)
  na_emails = ds2[ds2[,var2]=='','email']
  ds1 = ds1 %>% filter(!email %in% na_emails)
  ds2 = ds2 %>% filter(!email %in% na_emails)
  ds1 = as.numeric(as.character(ds1[,var1]))
  ds2 = as.numeric(as.character(ds2[,var2]))
  
  ret = list()
  ret$ds1 = ds1
  ret$ds2 = ds2
  
  return (ret)
}

has_learnt = function(dataset) {
  for (email in dataset$email) {
    person = dataset[dataset$email==email,]
    if (person$Q11_6.y==1) {
      if (person$Q11_6.x==2 | person$Q6_3==2) {
        dataset$learnt[dataset$email==email] = 'yes'    
      } else {
        if (person$Q11_7.y==2) {
          if (person$Q11_7.x==1 | person$Q6_4==1) {
            dataset$learnt[dataset$email==email] = 'yes'
          }
          else {
            if (person$Q11_8.y==2) {
              if (person$Q11_8.x==1 | person$Q6_5==1) {
                dataset$learnt[dataset$email==email] = 'yes'
              } else {
                dataset$learnt[dataset$email==email] = 'no'
              }
            } else {
              dataset$learnt[dataset$email==email] = 'no'
            }
          }
        } else {
          if (person$Q11_8.y==2) {
            if (person$Q11_8.x==1 | person$Q6_5==1) {
              dataset$learnt[dataset$email==email] = 'yes'
            } else {
              dataset$learnt[dataset$email==email] = 'no'
            }
          } else {
            dataset$learnt[dataset$email==email] = 'no'
          }   
        }  
      }
    } else {
      if (person$Q11_7.y==2) {
        if (person$Q11_7.x==1 | person$Q6_4==1) {
          dataset$learnt[dataset$email==email] = 'yes'
        }
        else {
          if (person$Q11_8.y==2) {
            if (person$Q11_8.x==1 | person$Q6_5==1) {
              dataset$learnt[dataset$email==email] = 'yes'
            } else {
              dataset$learnt[dataset$email==email] = 'no'
            }
          } else {
            dataset$learnt[dataset$email==email] = 'no'
          }
        }
      } else {
        if (person$Q11_8.y==2) {
          if (person$Q11_8.x==1 | person$Q6_5==1) {
            dataset$learnt[dataset$email==email] = 'yes'
          } else {
            dataset$learnt[dataset$email==email] = 'no'
          }
        } else {
          dataset$learnt[dataset$email==email] = 'no'
        }   
      }
    }
  }
  
  return (dataset)
}

find_correlations = function(dataset_1,dataset_2,set_1,set_2) {
  for (option_set1 in set_1) {
    for (option_set2 in set_2) {
      ret = prepare_data(dataset_1, dataset_2, option_set1, option_set2)
      cor_coef = cor(ret$ds1,ret$ds2)
      print(paste('The correlation between ',option_set1,' and ',option_set2,' is ',cor_coef,sep=''))
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
}