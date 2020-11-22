#################################################################################
### Cleaning Functions for cleaning Script ###
#################################################################################
###  Hunter York, hyork@uw.edu
###  07/30/2019
#################################################################################
###  Description:
###
###  Called from clean script. Contains functions to do simple cleaning tasks.
###  
#################################################################################
# cohort_exposed_def_var <-  "cohort_exposed_def1"
# cohort_unexp_def_var <- "cohort_unexp_def1" 


create_bins <- function(c.data, cohort_exposed_def_var,cohort_unexp_def_var){
  #start with exposed, topcode to 18
  data <- copy(c.data)
  data[, cohort_exposed_var_arch := get(cohort_exposed_def_var)]
  data[get(cohort_exposed_def_var) %like% ",", (cohort_exposed_def_var) := NA]
  data[get(cohort_exposed_def_var) %like% "NA|N/A|na", (cohort_exposed_def_var) := NA]
  data[get(cohort_exposed_def_var) %like% "and|AND", (cohort_exposed_def_var) := NA]
  data[tolower(get(cohort_exposed_def_var)) %like% "no education", (cohort_exposed_def_var) := 0]
  
  data[, paste0(cohort_exposed_def_var) := gsub("-", "to", get(cohort_exposed_def_var), fixed = T)]
  data[!get(cohort_exposed_def_var) %like% ">|<|to|\u2264|\u2265", paste0(cohort_exposed_def_var) := paste0(gsub("[^0-9]", "",get(cohort_exposed_def_var)), "to", gsub("[^0-9]", "",get(cohort_exposed_def_var)))]
  data[, paste0(cohort_exposed_def_var) := gsub("=", "", get(cohort_exposed_def_var), fixed = T)]
  data[, paste0(cohort_exposed_def_var) := gsub(" ", "", get(cohort_exposed_def_var), fixed = T)]
  
  data[, cohort_exp_def_lower := tstrsplit(get(cohort_exposed_def_var), "to", keep = 1)]
  data[, cohort_exp_def_lower := gsub("[^0-9]", "", cohort_exp_def_lower)]
  data[, cohort_exp_def_upper := tstrsplit(get(cohort_exposed_def_var), "to", keep = 2)]
  data[, cohort_exp_def_upper := gsub("[^0-9]", "", cohort_exp_def_upper)]
  data[, cohort_exp_def_upper := as.double(cohort_exp_def_upper)]
  data[, cohort_exp_def_lower := as.double(cohort_exp_def_lower)]
  data[get(cohort_exposed_def_var) %like% ">", cohort_exp_def_upper := 18]
  data[get(cohort_exposed_def_var) %like% ">", cohort_exp_def_lower := cohort_exp_def_lower + 1]
  data[get(cohort_exposed_def_var) %like% "<",cohort_exp_def_upper := cohort_exp_def_lower - 1]
  data[get(cohort_exposed_def_var) %like% "<", cohort_exp_def_lower := 0]
  data[get(cohort_exposed_def_var) %like% "\u2265", cohort_exp_def_upper := 18]
  data[get(cohort_exposed_def_var) %like% "\u2264",cohort_exp_def_upper := cohort_exp_def_lower]
  data[get(cohort_exposed_def_var) %like% "\u2264", cohort_exp_def_lower := 0]
  data[!get(cohort_exposed_def_var) %like% ">|<|to|\u2264|\u2265", cohort_exp_def_lower := as.numeric(gsub("[^0-9]", "",get(cohort_exposed_def_var)))]#for single digit entries, copy value to both upper and lower
  data[!get(cohort_exposed_def_var) %like% ">|<|to|\u2264|\u2265", cohort_exp_def_upper := as.numeric(gsub("[^0-9]", "",get(cohort_exposed_def_var)))]#for single digit entries, copy value to both upper and lower
  data[, cohort_exp_def_upper := as.double(cohort_exp_def_upper)]
  data[, cohort_exp_def_lower := as.double(cohort_exp_def_lower)]
  data[, cohort_exp_def_mid := (cohort_exp_def_upper + cohort_exp_def_lower)/2]
  data[inrange(cohort_exp_def_upper, 18, 28), cohort_exp_def_upper := 18]
  data[!inrange(cohort_exp_def_lower, 0, 19), cohort_exp_def_lower := NA]
  data[!inrange(cohort_exp_def_upper, 0, 19), cohort_exp_def_upper := NA]
  data[, clean_cohort_exp_def := paste0(cohort_exp_def_lower, " to ", cohort_exp_def_upper)]
  data[get(cohort_exposed_def_var) %like% "NA", clean_cohort_exp_def := NA]
  data[, cohort_exp_def_upper := cohort_exp_def_upper + .99]
  
  #then unexposed
  data[, cohort_unexp_var_arch := get(cohort_unexp_def_var)]
  data[get(cohort_unexp_def_var) %like% ",", (cohort_unexp_def_var) := NA]
  data[get(cohort_unexp_def_var) %like% "NA|N/A|na", (cohort_unexp_def_var) := NA]
  data[get(cohort_unexp_def_var) %like% "and|AND", (cohort_unexp_def_var) := NA]
  data[tolower(get(cohort_unexp_def_var)) %like% "no education", (cohort_unexp_def_var) := 0]
  
  data[, paste0(cohort_unexp_def_var) := gsub("-", "to", get(cohort_unexp_def_var), fixed = T)]
  data[!get(cohort_unexp_def_var) %like% ">|<|to|\u2264|\u2265", paste0(cohort_unexp_def_var) := paste0(gsub("[^0-9]", "",get(cohort_unexp_def_var)), "to", gsub("[^0-9]", "",get(cohort_unexp_def_var)))] 
  data[, paste0(cohort_unexp_def_var) := gsub("=", "", get(cohort_unexp_def_var), fixed = T)]
  data[, paste0(cohort_unexp_def_var) := gsub(" ", "", get(cohort_unexp_def_var))]
  
  data[, cohort_unexp_def_lower := tstrsplit(get(cohort_unexp_def_var), "to", keep = 1)]
  data[, cohort_unexp_def_lower := gsub("[^0-9]", "", cohort_unexp_def_lower)]
  data[, cohort_unexp_def_upper := tstrsplit(get(cohort_unexp_def_var), "to", keep = 2)]
  data[, cohort_unexp_def_upper := gsub("[^0-9]", "", cohort_unexp_def_upper)]
  data[, cohort_unexp_def_upper := as.double(cohort_unexp_def_upper)]
  data[, cohort_unexp_def_lower := as.double(cohort_unexp_def_lower)]
  data[get(cohort_unexp_def_var) %like% ">", cohort_unexp_def_upper := 18]
  data[get(cohort_unexp_def_var) %like% ">", cohort_unexp_def_lower := cohort_unexp_def_lower + 1]
  data[get(cohort_unexp_def_var) %like% "<",cohort_unexp_def_upper := cohort_unexp_def_lower - 1]
  data[get(cohort_unexp_def_var) %like% "<", cohort_unexp_def_lower := 0]
  data[get(cohort_unexp_def_var) %like% "\u2265", cohort_unexp_def_upper := 18]
  data[get(cohort_unexp_def_var) %like% "\u2264",cohort_unexp_def_upper := cohort_unexp_def_lower]
  data[get(cohort_unexp_def_var) %like% "\u2264", cohort_unexp_def_lower := 0]
  data[get(cohort_unexp_def_var) %like% "<",cohort_unexp_def_upper := cohort_unexp_def_lower]
  data[get(cohort_unexp_def_var) %like% "<", cohort_unexp_def_lower := 0]
  data[!get(cohort_unexp_def_var) %like% ">|<|to|\u2264|\u2265", cohort_unexp_def_lower := as.numeric(gsub("[^0-9]", "",get(cohort_unexp_def_var)))]#for single digit entries, copy value to both upper and lower
  data[!get(cohort_unexp_def_var) %like% ">|<|to|\u2264|\u2265", cohort_unexp_def_upper := as.numeric(gsub("[^0-9]", "",get(cohort_unexp_def_var)))]#for single digit entries, copy value to both upper and lower
  data[, cohort_unexp_def_upper := as.double(cohort_unexp_def_upper)]
  data[, cohort_unexp_def_lower := as.double(cohort_unexp_def_lower)]
  data[, cohort_unexp_def_mid := (cohort_unexp_def_upper + cohort_unexp_def_lower)/2]
  data[inrange(cohort_unexp_def_upper, 18, 28), cohort_unexp_def_upper := 18]
  data[!inrange(cohort_unexp_def_lower, 0, 19), cohort_unexp_def_lower := 18]
  data[!inrange(cohort_unexp_def_upper, 0, 19), cohort_unexp_def_upper := 18]
  data[, clean_cohort_unexp_def := paste0(cohort_unexp_def_lower, " to ", cohort_unexp_def_upper)]
  data[get(cohort_unexp_def_var) %like% "NA", clean_cohort_unexp_def := NA]
  data[, cohort_unexp_def_upper := cohort_unexp_def_upper + .99]
  
  
  
  
  #print error message if studies are missing values
  if(!(all(!is.na(data$cohort_unexp_def_lower)))){
    warning("Warning: There are NA values of exposed or unexposed definitions")
    warning(
      paste0("Non-standard inputs include: ",
             paste(c(
               data[is.na(cohort_unexp_def_upper)|is.na(cohort_unexp_def_lower), unique(get(cohort_unexp_def_var))],
               data[is.na(cohort_exp_def_upper)|is.na(cohort_exp_def_lower), unique(get(cohort_exposed_def_var))]
             ), collapse = "; "
             )
      )
    )
    
  }
  return(data) 
}


create_conf_ints <- function(){
  working_data[,nonCI_uncertainty_value := as.double(nonCI_uncertainty_value)] #ensure these arent character strings
  working_data[,CI_uncertainty_type_value := as.double(CI_uncertainty_type_value)] #ensure these arent character strings
  working_data[,cohort_sample_size_total := as.double(cohort_sample_size_total)] #ensure these arent character strings
  working_data[,cohort_sample_size_exp := as.double(cohort_sample_size_exp)] #ensure these arent character strings
  working_data[,cohort_sample_size_unexp := as.double(cohort_sample_size_unexp)] #ensure these arent character strings
  
  working_data[,effect_size := as.numeric(effect_size)]
  
  working_data[, upper := as.numeric(upper)]
  working_data[, lower := as.numeric(lower)]
  
  working_data[(is.na(upper) | is.na(lower)) & !is.na(CI_uncertainty_type_value) &is.na(nonCI_uncertainty_value), CI_uncertainty_type_value := NA]
  
  working_data[( is.na(upper)|is.na(lower) )& nonCI_uncertainty_type %like% "Standard|standard|SE", upper := as.double(effect_size  + (1.96*(nonCI_uncertainty_value)))]
  working_data[( is.na(upper)|is.na(lower) )& nonCI_uncertainty_type %like% "Standard|standard|SE", lower := as.double(effect_size  - (1.96*(nonCI_uncertainty_value)))]
  working_data[lower < 0.01, lower := 0.01]
  #calculate SE from those cases where there is only upper and lower bounds
  working_data[!is.na(upper) & !is.na(lower) & is.na(nonCI_uncertainty_value) & CI_uncertainty_type_value  == 90, 
               nonCI_uncertainty_value := (log(upper) - 
                                             log(lower)) / (1.645 * 2)]
  working_data[!is.na(upper) & !is.na(lower) & is.na(nonCI_uncertainty_value) & CI_uncertainty_type_value  == 95, 
               nonCI_uncertainty_value :=  (log(upper) - 
                                              log(lower)) / (1.96 * 2)]
  working_data[!is.na(upper) & !is.na(lower) & is.na(nonCI_uncertainty_value) & CI_uncertainty_type_value == 99, 
               nonCI_uncertainty_value :=   (log(upper) - 
                                               log(lower)) / (2.58 * 2)]
  
  ##using new SEs, recalculate upper and lower for non-95% cis
  working_data[( is.na(upper)|is.na(lower) ) & CI_uncertainty_type_value %in% c(99,90), upper := as.double(effect_size  + (1.96*(nonCI_uncertainty_value)))]
  working_data[( is.na(upper)|is.na(lower) ) & CI_uncertainty_type_value %in% c(99,90), lower := as.double(effect_size  - (1.96*(nonCI_uncertainty_value)))]
  
  ##for locations missing information necessary to calculate a rigorous CI, simulate them based on other examples
  working_data[is.na(cohort_sample_size_total), cohort_sample_size_total := cohort_sample_size_exp + cohort_sample_size_unexp]
  training_data <- working_data[!is.na(nonCI_uncertainty_value) &
                                  !is.na(cohort_sample_size_total) & nonCI_uncertainty_value > 0] %>% 
    .[,.(nonCI_uncertainty_value, cohort_sample_size_total)] %>% unique()
  
  training_data <- training_data[inrange(cohort_sample_size_total,
                                         quantile(training_data$cohort_sample_size_total, .025), 
                                         quantile(training_data$cohort_sample_size_total, .975))]
  training_data <- training_data[inrange(nonCI_uncertainty_value,
                                         quantile(training_data$nonCI_uncertainty_value, .025), 
                                         quantile(training_data$nonCI_uncertainty_value, .975))]
  #plot relationship b/w SS and SE
  ggplot(training_data) + 
    geom_point(aes(x = log(as.numeric(cohort_sample_size_total)), y = log(nonCI_uncertainty_value)))
  
  #fit model
  model_SE_SS <- lm(log(as.numeric(nonCI_uncertainty_value)) ~ log(as.numeric(cohort_sample_size_total)),
                    data = training_data)
  
  #fill in sample size for studies missing it using the 5%ile of all SS
  working_data[is.na(cohort_sample_size_total),
               cohort_sample_size_total := quantile(working_data[,cohort_sample_size_total], .05, na.rm = T)]
  
  working_data[is.na(nonCI_uncertainty_value),
               nonCI_uncertainty_value := exp(predict(model_SE_SS, newdata = .SD)), .SDcols = names(working_data)]
  

  working_data[( is.na(upper)|is.na(lower) ) & !is.na(nonCI_uncertainty_value), upper := as.double(effect_size  + (1.96*(nonCI_uncertainty_value)))]
  working_data[( is.na(upper)|is.na(lower) ) & !is.na(nonCI_uncertainty_value), lower := as.double(effect_size  - (1.96*(nonCI_uncertainty_value)))]
  
  #print error message if studies are missing values
  if(!(all(!is.na(working_data$upper)))){
    warning("Warning: There are NA values of confidence intervals for effect sizes")
  }
  #fill in upper and lower with midpoint for visualizing
  # working_data[is.na(upper), upper := effect_size]
  # working_data[is.na(lower), lower := effect_size]
  
}


create_covariates <- function(data){
  covs_summary <- data[,.SD,.SDcols = c(names(data)[names(data) %like% "confounders" & names(data) != "confounders_other"], "field_citation_value")] %>% unique()
  covs_string <- names(data)[names(data) %like% "confounders" & names(data) != "confounders_other"]
  covs_summary[, (covs_string) := lapply(.SD, as.numeric), .SDcols = covs_string]
  covs_counts <- data.table(melt(covs_summary[,colSums(.SD, na.rm = T)/nrow(.SD), .SDcols =names(data)[names(data) %like% "confounders" & names(data) != "confounders_other"]]))
  covs_counts[, confounder := names(data)[names(data) %like% "confounders" & names(data) != "confounders_other"]]
  setnames(covs_counts, c("value"), c("prevalence"))
  covs_counts[, confounder := factor(confounder, levels = covs_counts[ order(prevalence)]$confounder)]
  
  #plot prevalence of covariates
  pdf(paste0(root, "/visuals/covariate_counts_orig.pdf"), onefile = T, width = 10, height = 8)
  gg1 <- ggplot(covs_counts) + 
    geom_bar(aes(x = confounder, y = prevalence), stat= "identity") + 
    theme_bw() + 
    scale_y_continuous(labels = percent)+
    coord_flip()
  print(gg1)
  dev.off()
  
 
  data[, confounders_other_clean := tolower(gsub(" ","",gsub("[^a-zA-Z0-9 ., ]", "", confounders_other)))]
  
  cov_map <- fread(paste0(root, "/ref/covariate_map.csv"))
  
  for(c.cov in cov_map$orig_covariate){
    print(c.cov)
    for(cc.cov in other_covs[other_covs %like% c.cov]){
    data[confounders_other_clean %like% paste0(cc.cov),
         confounders_other_clean := gsub(paste0(cc.cov),
                                         paste0(cov_map[orig_covariate == c.cov, new_covariate]), confounders_other_clean)]
    }
  }
  
  other_covs <- other_covs[!other_covs %in% cov_map$orig_covariate]
  for(c.cov in unique(cov_map$new_covariate)){
    for(cc.cov in other_covs[other_covs %like% c.cov]){
      print(cc.cov)
      data[confounders_other_clean %like% paste0(cc.cov) &
             !confounders_other_clean %like% paste0(cc.cov, "[a-z]") &
             !confounders_other_clean %like% paste0("[a-z]",cc.cov) ,confounders_other_clean := gsub(cc.cov, c.cov, confounders_other_clean)] 
      
    }
  }
  other_covs <- unique(c(other_covs, unique(cov_map$new_covariate)))
  other_cov_dt <- data.table()
  for(c.cov in unique(other_covs)){
    print(c.cov)
    temp <- data.table(confounder = c.cov,
               count = nrow(unique(data[confounders_other_clean %like% c.cov, .(confounders_other_clean)])))
    other_cov_dt <- rbind(other_cov_dt, temp)
  }
  other_cov_dt[, confounder := factor(confounder, levels = other_cov_dt[ order(count)]$confounder)]
  other_cov_dt[, prevalence := count/nrow(unique(data[,.(field_citation_value)]))]
  pdf(paste0(root, "/visuals/covariate_counts_free_text.pdf"), onefile = T, width = 10, height = 8)
  
  gg1 <- ggplot(other_cov_dt[count > 8]) + 
    geom_bar(aes(x = confounder, y = prevalence), stat= "identity") + 
    theme_bw() + 
    scale_y_continuous(labels = percent)+
    coord_flip() 
  print(gg1)
  dev.off()
  
  for(c.cov in other_covs){
    if(any(names(data) %like% paste0("confounders_", c.cov))){
      print(c.cov)
      data[confounders_other_clean %like% c.cov, paste0("confounders_", c.cov) := 1]
    }
  }
  
  for(c.cov in c(other_cov_dt[prevalence > .1, confounder], "otherparenteducation")){
    print(c.cov)
    data[confounders_other_clean %like% c.cov,paste0("confounders_", c.cov) := 1]
    data[is.na(get(paste0("confounders_", c.cov))), paste0("confounders_", c.cov) := 0]
  }
  
  data[confounders_wealth_index ==1, confounders_wealth := 1]
  
  
  covs_summary <- data[,.SD,.SDcols = c(names(data)[names(data) %like% "confounders" & names(data) != "confounders_other"], "field_citation_value")] %>% unique()
  covs_string <- names(data)[names(data) %like% "confounders" & names(data) != "confounders_other"]
  covs_summary[, (covs_string) := lapply(.SD, as.numeric), .SDcols = covs_string]
  covs_counts <- data.table(melt(covs_summary[,colSums(.SD, na.rm = T)/nrow(.SD), .SDcols =names(data)[names(data) %like% "confounders" & names(data) != "confounders_other"]]))
  covs_counts[, confounder := names(data)[names(data) %like% "confounders" & names(data) != "confounders_other"]]
  setnames(covs_counts, c("value"), c("prevalence"))
  covs_counts[, confounder := factor(confounder, levels = covs_counts[ order(prevalence)]$confounder)]
  covs_counts[, confounder_pretty := gsub("confounders_", "", confounder)]
  covs_counts[, confounder_pretty := gsub("_", " ", confounder_pretty, fixed = T)]
  covs_counts[, confounder_pretty := gsub(".", " ", confounder_pretty, fixed = T)]
  
  camel <- function(x){ #function for camel case
    capit <- function(x) paste0(toupper(substring(x, 1, 1)), substring(x, 2, nchar(x)))
    sapply(strsplit(x, " "), function(x) paste(capit(x), collapse=" "))
  }
  
  covs_counts[, confounder_pretty := camel(confounder_pretty)]
  covs_counts <- covs_counts[order(prevalence, decreasing = T)]
  covs_counts[confounder_pretty == "Otherparenteducation", confounder_pretty := "Other Parent's Education"]
  covs_counts[confounder %like% "bmi", confounder_pretty := "Body Mass Index"]
  covs_counts[confounder %like% "sex", confounder_pretty := "Sex of the Child"]
  covs_counts[confounder %like% "age", confounder_pretty := "Age of the Mother"]
  covs_counts[confounder %like% "water", confounder_pretty := "Access to Safe Water"]
  covs_counts[confounder %like% "rural", confounder_pretty := "Rural/Urban Residence"]
  covs_counts[confounder %like% "birthinterv", confounder_pretty := "Birth interval"]
  covs_counts[confounder %like% "birthorder", confounder_pretty := "Birth Order"]
  
  covs_counts <- covs_counts[!confounder %like% "index"]
  covs_counts[, confounder_pretty := factor(confounder_pretty, levels = rev(covs_counts$confounder_pretty))]
  #plot prevalence of covariates
  pdf(paste0(root, "/visuals/covariate_counts_all.pdf"), onefile = T, width = 10, height = 8)
  gg1 <- ggplot(covs_counts[1:20]) + 
    geom_bar(aes(x = confounder_pretty, y = prevalence), stat= "identity") + 
    theme_bw() + 
    scale_y_continuous(labels = percent)+
    coord_flip() + xlab("Confounders") + ylab("Prevalence") + 
    ggtitle("Prevalence of Study Covariates As a Proportion\nof All Covariates-Study Combinations")
  print(gg1)
  dev.off()
  
  return(data)
}
