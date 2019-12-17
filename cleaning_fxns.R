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

create_bins <- function(c.data, cohort_exposed_def_var,cohort_unexp_def_var){
  #start with exposed, topcode to 18
  data <- copy(c.data)
  data[!get(cohort_exposed_def_var) %like% ">|<|to|≤|≥", paste0(cohort_exposed_def_var) := paste0(gsub("[^0-9]", "",get(cohort_exposed_def_var)), "to", gsub("[^0-9]", "",get(cohort_exposed_def_var)))]
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
  data[get(cohort_exposed_def_var) %like% "≥", cohort_exp_def_upper := 18]
  data[get(cohort_exposed_def_var) %like% "≤",cohort_exp_def_upper := cohort_exp_def_lower]
  data[get(cohort_exposed_def_var) %like% "≤", cohort_exp_def_lower := 0]
  data[!get(cohort_exposed_def_var) %like% ">|<|to|≤|≥", cohort_exp_def_lower := as.numeric(gsub("[^0-9]", "",get(cohort_exposed_def_var)))]#for single digit entries, copy value to both upper and lower
  data[!get(cohort_exposed_def_var) %like% ">|<|to|≤|≥", cohort_exp_def_upper := as.numeric(gsub("[^0-9]", "",get(cohort_exposed_def_var)))]#for single digit entries, copy value to both upper and lower
  data[, cohort_exp_def_upper := as.double(cohort_exp_def_upper)]
  data[, cohort_exp_def_lower := as.double(cohort_exp_def_lower)]
  data[, cohort_exp_def_mid := (cohort_exp_def_upper + cohort_exp_def_lower)/2]
  data[inrange(cohort_exp_def_upper, 18, 28), cohort_exp_def_upper := 18]
  data[!inrange(cohort_exp_def_lower, 0, 19), cohort_exp_def_lower := NA]
  data[!inrange(cohort_exp_def_upper, 0, 19), cohort_exp_def_upper := NA]
  data[, clean_cohort_exp_def := paste0(cohort_exp_def_lower, " to ", cohort_exp_def_upper)]
  data[, cohort_exp_def_upper := cohort_exp_def_upper + .99]
  
  #then unexposed
  data[!get(cohort_unexp_def_var) %like% ">|<|to|≤|≥", paste0(cohort_unexp_def_var) := paste0(gsub("[^0-9]", "",get(cohort_unexp_def_var)), "to", gsub("[^0-9]", "",get(cohort_unexp_def_var)))]
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
  data[get(cohort_unexp_def_var) %like% "≥", cohort_unexp_def_upper := 18]
  data[get(cohort_unexp_def_var) %like% "≤",cohort_unexp_def_upper := cohort_unexp_def_lower]
  data[get(cohort_unexp_def_var) %like% "≤", cohort_unexp_def_lower := 0]
  data[get(cohort_unexp_def_var) %like% "<",cohort_unexp_def_upper := cohort_unexp_def_lower]
  data[get(cohort_unexp_def_var) %like% "<", cohort_unexp_def_lower := 0]
  data[!get(cohort_unexp_def_var) %like% ">|<|to|≤|≥", cohort_unexp_def_lower := as.numeric(gsub("[^0-9]", "",get(cohort_unexp_def_var)))]#for single digit entries, copy value to both upper and lower
  data[!get(cohort_unexp_def_var) %like% ">|<|to|≤|≥", cohort_unexp_def_upper := as.numeric(gsub("[^0-9]", "",get(cohort_unexp_def_var)))]#for single digit entries, copy value to both upper and lower
  data[, cohort_unexp_def_upper := as.double(cohort_unexp_def_upper)]
  data[, cohort_unexp_def_lower := as.double(cohort_unexp_def_lower)]
  data[, cohort_unexp_def_mid := (cohort_unexp_def_upper + cohort_unexp_def_lower)/2]
  data[inrange(cohort_unexp_def_upper, 18, 28), cohort_unexp_def_upper := 18]
  data[!inrange(cohort_unexp_def_lower, 0, 19), cohort_unexp_def_lower := 18]
  data[!inrange(cohort_unexp_def_upper, 0, 19), cohort_unexp_def_upper := 18]
  data[, clean_cohort_unexp_def := paste0(cohort_unexp_def_lower, " to ", cohort_unexp_def_upper)]
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
  working_data[,effect_size := as.numeric(effect_size)]
  
  working_data[, upper := as.numeric(upper)]
  working_data[, lower := as.numeric(lower)]
  
  working_data[( is.na(upper)|is.na(lower) )& nonCI_uncertainty_type %like% "Standard|standard|SE", upper := as.double(effect_size  + (1.96*(nonCI_uncertainty_value)))]
  working_data[( is.na(upper)|is.na(lower) )& nonCI_uncertainty_type %like% "Standard|standard|SE", lower := as.double(effect_size  - (1.96*(nonCI_uncertainty_value)))]
  
  
  #print error message if studies are missing values
  if(!(all(!is.na(working_data$upper)))){
    warning("Warning: There are NA values of confidence intervals for effect sizes")
  }
  
  #fill in upper and lower with midpoint for visualizing
  working_data[is.na(upper), upper := effect_size]
  working_data[is.na(lower), lower := effect_size]
  
}