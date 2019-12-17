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
list.of.packages <- c("data.table", "plyr", "dplyr", "ggplot2", "readxl", "grid", "survival")
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

#read in all survyes
#dhs_cbh <- fread("J:/temp/hyork/comp_cases_7_11_2019.csv")

#subset to only 4 surveys to speed up analysis, only keep relevant columns
#dhs_cbh_subset <- dhs_cbh[survey_id %in% c("BFA2003", "MWI2015", "TUR2003", "KEN2014"), .(survey_id, ihme_loc_id, maternal_ed_yrs, paternal_ed_yrs, person_years, child_age_at_death_months, child_alive)]
dhs_cbh_subset <- fread("J:/temp/hyork/dhs_cbh_subset_20191009.csv")
#################################################################################
## Create crosswalk function ##
## Arguments: survival age period start and end

dhs_standardizr <- function (c.data,subset, c.dhs_data){
  tryCatch({
    print(subset)
    #create referent and exposure variables
    data <- copy(c.data[xwalk_unit == subset])
    dhs_data <- copy(data.table(c.dhs_data))
    #create age span variable
    referent_lower <- data[,unique(cohort_unexp_def_lower)]
    referent_upper <- data[,unique(cohort_unexp_def_upper)]
    age_span_lower <- data[,as.numeric(unique(age_start))]
    age_span_upper <- data[,as.numeric(unique(age_end))]
    exp_bins <- data[,unique(paste0(cohort_exp_def_lower, "_", cohort_exp_def_upper))]
    ref_bin <- paste0(referent_lower, "_", referent_upper)
    all_bins <- c(ref_bin, exp_bins)
    min_bin_lower <- tstrsplit(all_bins, "_", keep = 1, type.convert = T) %>% unlist %>% min()
    min_bin <- all_bins[ unlist(tstrsplit(all_bins, "_", keep = 1, type.convert = T)) == min_bin_lower ]
    min_bin_upper <- tstrsplit(min_bin, "_", keep = 2, type.convert = T) %>% unlist %>% floor()
    #create survival variables in dataset to reflect age span
    dhs_data[,person_years_span := person_years]
    dhs_data[person_years_span > age_span_upper, person_years_span := age_span_upper]
    #create death variable for those who die inside the interval
    dhs_data[child_age_at_death_months <= (12 * age_span_upper) & child_age_at_death_months >= (12* age_span_lower), child_death_span := 1 ]
    #ensure no one who died before or after the interval is counted
    dhs_data[child_age_at_death_months >= (12*age_span_lower) & child_age_at_death_months > (12*age_span_upper) | is.na(child_age_at_death_months), child_death_span := 0]
    
    #create categorical variable to reflect all the categories of exposure and referent
    for (c.bin in all_bins){
      print(c.bin)
      dhs_data[maternal_ed_yrs %in%
                 floor(unlist(tstrsplit(c.bin, "_", keep = 1, type.convert = T))):floor(unlist(tstrsplit(c.bin, "_", keep = 2, type.convert = T))),
               binned_maternal := c.bin]
    }
    
    #ensure referent is referent in a factor variable
    dhs_data[,binned_maternal := relevel(factor(binned_maternal), ref = ref_bin)]
    
    #create variable with same exposure bin but with a referent of 0
    dhs_data[maternal_ed_yrs == 0,binned_maternal_std := "0"]
    dhs_data[!maternal_ed_yrs %in% min_bin_lower:min_bin_upper, binned_maternal_std := binned_maternal]
    dhs_data[,binned_maternal_std := relevel(factor(binned_maternal_std), ref = "0")]
    
    #create dummy variable for beginning of exposure
    dhs_data[, dummy := age_span_lower]
    
    #ensure you have complete cases
    
    
    
    dhs_data <- dhs_data[!is.na(child_death_span) & !is.na(person_years_span) & !is.na(binned_maternal) & !is.na(paternal_ed_yrs)]
    #run cox models to get rrs
    fit <- coxph(Surv(dhs_data$dummy,dhs_data$person_years_span,dhs_data$child_death_span) ~
                   binned_maternal +
                   paternal_ed_yrs + 
                   survey_id, data = dhs_data)
    fit_std <- coxph(Surv(dhs_data$dummy,dhs_data$person_years_span,dhs_data$child_death_span) ~
                       binned_maternal_std +
                       paternal_ed_yrs + 
                       survey_id, data = dhs_data)
    
    #get crosswalk ratios
    coefs_fit <- coef(fit)[names(coef(fit)) %like% "binned_maternal"]
    coefs_fit_std <-  coef(fit_std)[names(coef(fit_std)) %like% "binned_maternal"]
    #make dataframe
    coefs_fit <- data.table(fits = coefs_fit, names = gsub("binned_maternal", "", names(coefs_fit)))
    coefs_fit_std <- data.table(fits_std = coefs_fit_std, names = gsub("binned_maternal_std", "", names(coefs_fit_std)))
    all_coefs <- merge(coefs_fit, coefs_fit_std, all = T)
    
    #exponentiate to get RRS
    all_coefs[, fits_rr := exp(fits)] %>% .[,fits_std_rr := exp(fits_std)]
    all_coefs[,bin_lower := tstrsplit(names, "_", keep = 1, type.convert = T)]
    all_coefs[,bin_upper := tstrsplit(names, "_", keep = 2, type.convert = T)]
    
    #get crosswalk for all untransposed RRs
    all_coefs[!names == ref_bin & bin_lower > 0, crosswalk_ratio := fits_std_rr/fits_rr]
    
    #if the referent group was not originally overlapping with 0, inverse the RRs for the group containing 0 to get the rr for that group
    all_coefs[, inv_fits_rr := exp(1/fits[names == min_bin])]
    all_coefs[bin_lower == 0 & names != ref_bin, crosswalk_ratio := all_coefs[names == ref_bin]$fits_std_rr/inv_fits_rr]
    all_coefs[bin_lower == 0 & names != ref_bin ,new_bin := ref_bin]
    all_coefs[bin_lower != 0 & names != ref_bin, new_bin := names]
    #merge on original data and apply crosswalks
    data[, names := paste0(cohort_exp_def_lower, "_", cohort_exp_def_upper)]
    data <- merge(data, all_coefs[,.(names, crosswalk_ratio, new_bin)], by = "names")
    data[,crosswalked_effect := as.numeric(effect_size) * crosswalk_ratio]
    data[,crosswalked_upper := as.numeric(upper) * crosswalk_ratio]
    data[,crosswalked_lower := as.numeric(lower) * crosswalk_ratio]
    data[,xwalked_exposure_upper := tstrsplit(new_bin, "_", type.convert = T, keep = 2)]
    data[,xwalked_exposure_lower := tstrsplit(new_bin, "_", type.convert = T, keep = 1)]
    data[,xwalked_reference := 0]
    return(data)
  }, error = function(e){})
}

