#################################################################################
###  Crosswalk all binned data to referent category 0 using DHS    ###
#################################################################################
###  Hunter York, hyork@uw.edu
###  10/09/2019
#################################################################################
###  Description:
###  Takes pooled DHS birth cohort data and runs simple regressions to get RRs
###  Applies crosswalk to standardize unstandardized RRs from extractions
###  
#################################################################################


#################################################################################
## Setup ##
#################################################################################

# install libraries and load them
list.of.packages <- c("data.table", "plyr", "dplyr", "ggplot2", "readxl", "grid", "survival", "boot", "MASS", "gtools", "prediction")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
lapply(list.of.packages, require, character.only = TRUE)

#
root <- "C:/Users/hyork/Desktop/child_mort_dir" ## replace with directory on host computer
input_dir <- paste0(root, "/inputs")
code_dir <- paste0(root, "/code")
output_dir <- paste0(root, "/outputs/")

#################################################################################
## Read in cohort data from cbh##
#################################################################################

#assign gold standard ref category
gs_cat <- "0_0"
ideal_lower <- tstrsplit(gs_cat, "_", keep = 1, type.convert = T)[[1]]
ideal_upper<- tstrsplit(gs_cat, "_", keep = 2, type.convert = T)[[1]]

#read in all survyes
#dhs_cbh <- fread("J:/temp/hyork/comp_cases_7_11_2019.csv")

#subset to only 4 surveys to speed up analysis, only keep relevant columns
#dhs_cbh_subset <- dhs_cbh[survey_id %in% c("BFA2003", "MWI2015", "TUR2003", "KEN2014"), .(survey_id, ihme_loc_id, maternal_ed_yrs, paternal_ed_yrs, person_years, child_age_at_death_months, child_alive)]
dhs_cbh_subset <- fread("J:/temp/hyork/dhs_cbh_subset_20191009.csv")


bin_getr <- function(data, subset){
  #create referent and exposure variables
  data <- copy(c.data[xwalk_unit == subset])
  #create age span variable
  age_span_lower <- data[,as.numeric(unique(age_start))]
  age_span_upper <- data[,as.numeric(unique(age_end))]
  exp_bins <- data[,unique(paste0(cohort_exp_def_lower, "_", cohort_exp_def_upper))]
  ref_bin <- paste0(referent_lower, "_", referent_upper)
  all_bins <- c(ref_bin, exp_bins)

  #create survival variables in dataset to reflect age span
  pred_frame <- data.table(binz = c(all_bins, gs_cat))
  pred_frame[, bin := gsub("_", ",", binz)]
  pred_frame[, bin := gsub(".99", "", bin)]
  pred_frame[, bin := paste0("[", bin, "]")]
  pred_frame[, bin_lower := tstrsplit(bin,",", keep = 1)]
  pred_frame[, bin_lower := as.numeric(gsub("[^0-9]", "", bin_lower))]
  pred_frame[, bin_upper := tstrsplit(bin,",", keep = 2)]
  pred_frame[, bin_upper := as.numeric(gsub("[^0-9]", "", bin_upper))]
  pred_frame[binz == ref_bin, category := "reference_observed"]
  pred_frame[binz != ref_bin, category := "exposure_observed"]
  pred_frame[binz == gs_cat, category := "reference_preferred"]
  pred_frame[, binz := NULL]
  pred_frame[, age_upper := age_span_upper]
  pred_frame[, age_lower := age_span_lower]
  pred_frame[,xwalk_unit := subset]
  return(pred_frame)
}



estimate_rr <- function(binset, c.dataset, form="response ~ edu_bins  + paternal_ed_yrs + survey_id"){
  tryCatch({
  dataset <- copy(c.dataset)
  #get age args
  age_span_upper <- binset[,unique(age_upper)]
  age_span_lower <- binset[,unique(age_lower)]
  
  #set up dhs data
  #create survival variables in dataset to reflect age span
  dataset[,person_years_span := person_years]
  dataset[person_years_span > age_span_upper, person_years_span := age_span_upper]
  #create death variable for those who die inside the interval
  dataset[child_age_at_death_months <= (12 * age_span_upper) & child_age_at_death_months >= (12* age_span_lower), child_death_span := 1 ]
  #ensure no one who died before or after the interval is counted
  dataset[child_age_at_death_months >= (12*age_span_lower) & child_age_at_death_months > (12*age_span_upper) | is.na(child_age_at_death_months), child_death_span := 0]
  dataset[, dummy := age_span_lower]
  #subtract start time when age of interval does not start at 0
  dataset[, person_years_span := person_years_span - dummy]
  
  #get rid of nas
  dataset <- dataset[!is.na(maternal_ed_yrs) & !is.na(paternal_ed_yrs)  & 
                       !is.na(child_death_span)] %>% .[(child_alive == "No" & child_age_at_death_months >= dummy)|
                                                         child_alive == "Yes"]
  
  #Loop through bin combos for first run
  for (x in 1:nrow(binset)){
    
    l <- binset[x, bin_lower]
    u <- binset[x, bin_upper]
    
    
    if (binset[x]$category=="exposure_observed"){
      
      if (u >= 18){
        u <- ideal_upper
      }else{
        u <- u+1
      }
      
      new_bin <- sprintf("[%s,%s)", l, u)
      dataset[(maternal_ed_yrs >= l & maternal_ed_yrs < u), edu_bins := new_bin]
    }
    
    if (binset[x]$category=="reference_observed"){
      new_bin <- sprintf("[%s,%s]", l, u)
      dataset[(maternal_ed_yrs >= l & maternal_ed_yrs <= u), edu_bins := new_bin]
      ref <- copy(new_bin)
    }
  }
  
  #Copy dataset to save for second model, then drop NA values. Correctly order bins, with reference category first
  before_bins <- order_bins(unique(dataset[!is.na(edu_bins), edu_bins]), ref)
  model_df <- copy(dataset) %>%
    .[!is.na(edu_bins),] %>%
    .[, edu_bins := factor(edu_bins, levels=before_bins)]
  
  response      <- with(model_df, Surv( event = child_death_span, time = person_years_span, type='right'))
  model_before  <- coxph(formula(form), data=model_df)
  
  #Extract 1000 draws from parameter estimates for each bin
  bin_odds_before <- exp(mvrnorm(1000, coef(model_before), vcov(model_before))) %>% 
    data.table %>%
    .[, draw:=0:999] %>%
    melt(., id.var='draw', variable.name = "bin", value.name = 'before_value') %>%
    .[, bin := gsub("edu_bins", "", bin)]
  
  #Change reference category, regenerate bins and run the model again.
  
  #Note: variable names within the binset data below were chosen so that "reference_preferred""
  #when ordered, would come last. This was done so it would overwrite old bins which overlap 
  #with the new reference category (but maintain similar inital names for mapping adjustment factors later) 
  
  for (x in 1:nrow(binset)){
    
    l <- binset[x, bin_lower]
    u <- binset[x, bin_upper]
    
    if (binset[x]$category=="exposure_observed"){
      
      if (u >= 18){
        u <- ideal_upper
      }else{
        u <- u+1
      }
      
      new_bin <- sprintf("[%s,%s)", l, u)
      dataset[(maternal_ed_yrs >= l & maternal_ed_yrs < u), edu_bins := new_bin]
      binset[x, bin := new_bin]
    }
    
    if (binset[x]$category=="reference_observed"){
      new_bin <- sprintf("[%s,%s]", l, u)
      dataset[(maternal_ed_yrs >= l & maternal_ed_yrs <= u), edu_bins := new_bin]
      binset[x, bin := new_bin]
    }
    
    if (binset[x]$category=="reference_preferred"){
      new_bin <- sprintf("[%s,%s]", l, u)
      ref <- copy(new_bin)
      dataset[(maternal_ed_yrs >= l & maternal_ed_yrs <= u), edu_bins := new_bin]
      binset[x, bin := new_bin]
    }
  }
  
  after_bins <- order_bins(unique(dataset[!is.na(edu_bins), edu_bins]), ref)
  model_df <- copy(dataset) %>%
    .[!is.na(edu_bins),] %>%
    .[, edu_bins := factor(edu_bins, levels=after_bins)]
  
  response      <- with(model_df, Surv( event = child_death_span, time = person_years_span, type='right'))
  model_after  <- coxph(formula(form), data=model_df)

  
  bin_odds_after <- exp(mvrnorm(1000, coef(model_after), vcov(model_after))) %>% 
    data.table %>%
    .[, draw:=0:999] %>%
    melt(., id.var='draw', variable.name = "bin", value.name = 'after_value') %>%
    .[, bin := gsub("edu_bins", "", bin)]
  
  group <- binset[,unique(xwalk_unit)]
  print(group)
  binset <- binset[, .(bin, category)]
  adjustment_factor <- join(bin_odds_before, bin_odds_after, by=c("draw", "bin"), type='full') %>%
    data.table %>%
    .[is.na(before_value), before_value := 1] %>%
    .[is.na(after_value), after_value := 1] %>%
    .[, adjust := after_value/before_value]  %>%
    .[(bin != "sex_id" & bin != "age"),] %>%
    .[, .(draw, bin, adjust)] %>%
    join(., binset, by=c("bin"), type="left") %>% 
    data.table
  

  adjustment_factor[, xwalk_unit :=group]
  return(adjustment_factor)
  }, error = function(e){})
  
}

#apply prep data to be xwalked
working_data[,xwalk_unit := paste(field_citation_value, age_start, age_end, sep = "_")]
data_to_xwalk <- working_data[cohort_unexp_def_lower != 0 & cohort_unexp_def_upper != 0]
data_to_xwalk[, bin := paste0("[",cohort_exp_def_lower, ",", round(cohort_exp_def_upper),")")]
data_to_xwalk[, bin := gsub("18)", "18]", bin)]
prepped_list <- lapply(unique(data_to_xwalk$xwalk_unit), bin_getr, data = data_to_xwalk)

#apply crosswalk
lapply(prepped_list,estimate_rr, c.dataset = dhs_cbh_subset)->xwalked_data_list
xwalked_data <- rbindlist(xwalked_data_list)



#create draws of study rrs to apply xwalk to
sample_from_study_rr <- function(data, subset){
  print(subset)
  df <- copy(data[xwalk_unit == subset])
  se <- (log(as.numeric(df$upper)) - log(as.numeric(df$lower)))/1.96*2
  rr_draws <- rlnorm(1000, log(as.numeric(df$effect_size)), se)
  
  draws <- data.table(xwalk_unit  = df$xwalk_unit,
                      bin      = df$bin,
                      draw      = 0:999,
                      old_rr    = rr_draws)
  return(draws)
  
}

study_rr_draws <- lapply(unique(data_to_xwalk$xwalk_unit), sample_from_study_rr, data = data_to_xwalk) %>% rbindlist

#merge study rrs and xwalk factors and multiply
expand <- merge(xwalked_data, study_rr_draws, by = c("bin", "xwalk_unit", "draw"))
expand[, new_rr := adjust * old_rr]

#collapse to summary for each bin, age, exposure
final_rrs_collapsed <- expand[,.(effect_size = mean(new_rr, na.rm = T),
                                 upper = quantile(new_rr, .975, na.rm = T),
                                 lower = quantile(new_rr, .025, na.rm = T),
                                 old_effect_size = mean(old_rr, na.rm = T),
                                 old_upper = quantile(old_rr, .975, na.rm = T),
                                 old_lower = quantile(old_rr, .025, na.rm = T)),
                              by = .(xwalk_unit, 
                                     bin, category )] %>% 
  .[, `:=`(bin_lower = as.numeric(gsub("\\[|\\]|\\(|\\)|", "", tstrsplit(bin, ",")[[1]])),
           bin_upper = as.numeric(gsub("\\[|\\]|\\(|\\)|", "", tstrsplit(bin, ",")[[2]])))] %>%
  .[bin_upper > ideal_lower, bin_upper := ideal_lower] %>%
  .[!bin_upper < bin_lower,]
