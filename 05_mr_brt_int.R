##########################################################
## MR BRT Relative Risk Fitting Model for Education as a
## Risk factor for under-5 mortality
##########################################################
## Hunter York
## hyork@uw.edu
## 07/24/2020
##########################################################
## This code takes in data from 2 sources: a literature 
## review done by colleagues at CHAIN NTNU and relative risks
## extracted from DHS complete birth histories
##########################################################
library(mrbrt001, lib.loc = "<<<<< filepath redacted >>>>>R/packages/")
library(dummies, lib.loc = "<<<<< filepath redacted >>>>>rlibs")

library(dplyr)
library(data.table)
library(rlist)
library(gtools)
library(stringr)

## Assign number of draws to sample from posterior distribution (followed by L for python integer notation)
c.sample_size <- 50L
inpct <- .8
## Assign a model name
c.model_version <- "prod20201012"


##########################################################
## Load data
##########################################################

df_orig = fread('<<<<< filepath redacted >>>>>/cleaned_dataset.csv', encoding = "Latin-1")
df_dhs = fread("<<<<< filepath redacted >>>>>dhs_effect_sizes.csv")
df_dhs <- df_dhs[vars == "base, wealth_index_dhs_continuous, urban, child_sex, std_year_born"] %>% 
  .[,cohort_exp_def_upper := 8.99] %>% 
  .[,cohort_exp_def_lower := 8] %>% 
  .[,effect_size := effect_size ^8] %>% 
  .[,lower := lower ^8] %>% 
  .[,upper := upper ^8]

df_dhs[is.na(age_interval_std), age_interval_std := "1 to 5, real"]


df_orig2 = rbindlist(list(df_orig, df_dhs), fill = T) %>% 
  .[, seq_id := 1:nrow(.)] %>% 
  .[,ln_effect_size := log(effect_size)] %>% 
  .[,log_lower := log(lower)] %>% 
  .[,log_upper := log(upper)] %>% 
  .[,log_se := (log_upper - log_lower) / 3.92 ] %>%
  .[cohort_unexp_def_lower < cohort_unexp_def_upper] %>%
  .[cohort_exp_def_lower < cohort_exp_def_upper] %>% 
  .[log_se > 0] 

df_orig2[tolower(field_citation_value) %like% "dhs|demog"|(!is.na(as.numeric(str_sub(field_citation_value,-4,-1))) & nchar(field_citation_value)==7), is_dhs := "DHS"]
df_orig2[is.na(is_dhs), is_dhs := "Literature Review"]

##########################################################
## Load location data and clean up data
##########################################################

source("/<<<<< filepath redacted >>>>>r/get_covariate_estimates.R")
source("/<<<<< filepath redacted >>>>>r/get_location_metadata.R")
locs <- get_location_metadata(location_set_id = 22)

df_orig2[, ihme_loc_id := tstrsplit(location_name, "|", keep = 2, fixed = T)]
df_orig2[is.na(ihme_loc_id), ihme_loc_id := substr(field_citation_value,1,3)]
df_orig2[is_dhs == "DHS", year_id := as.numeric(str_sub(field_citation_value, -4, -1))]
df_orig2[is_dhs != "DHS", year_id := as.numeric(year_end_study)]
df_orig2[is.na(year_id), year_id := 2000]
df_orig2[year_id < 1980, year_id := 1980]
df_orig2[year_id > 2018, year_id := 2018]
df_orig2[is_dhs != "DHS",field_citation_value_substr:= str_sub(field_citation_value,1,-4)]
##########################################################
## Load covariates from GBD DB
##########################################################

haq <- get_covariate_estimates(covariate_id = c(1099),
                               location_id = locs[ihme_loc_id %in% df_orig2$ihme_loc_id, location_id], 
                               year_id = unique(df_orig2$year_id), 
                               gbd_round_id = 7,
                               decomp_step = "iterative")

sdi <- get_covariate_estimates(covariate_id = c(881),
                               location_id = locs[ihme_loc_id %in% df_orig2$ihme_loc_id, location_id], 
                               year_id = unique(df_orig2$year_id), 
                               gbd_round_id = 7,
                               decomp_step = "iterative")
df_orig2[, super_region_name := NULL]
df_orig2 <- merge(df_orig2, locs[,.(ihme_loc_id, location_id, super_region_name)], by = "ihme_loc_id")
setnames(sdi, "mean_value", "confounders_sdi")
setnames(haq, "mean_value", "confounders_haq")
haq[, confounders_haq := confounders_haq/100]

df_orig2 <- merge(df_orig2, sdi[,.(location_id, year_id, confounders_sdi)], by = c("location_id", "year_id"))
df_orig2 <- merge(df_orig2, haq[,.(location_id, year_id, confounders_haq)], by = c("location_id", "year_id"))

##########################################################
## Create Super region dummies
##########################################################

for(c.super_reg in unique(df_orig2$super_region_name)){
  df_orig2[super_region_name == c.super_reg,paste0("dummy_", c.super_reg) := 1]
  df_orig2[is.na(get(paste0("dummy_", c.super_reg))),paste0("dummy_", c.super_reg) := 0]
  
}

df_orig2[,dummy_other_region := `dummy_Latin America and Caribbean` +
           `dummy_North Africa and Middle East` +
           `dummy_South Asia` + 
           `dummy_Southeast Asia, East Asia, and Oceania` + 
           `dummy_Central Europe, Eastern Europe, and Central Asia` + 
           `dummy_High-income`]

df_orig2[is.na(confounders_otherparenteducation), confounders_otherparenteducation := 0]

#df_orig2 <- df_orig2[!(obs_slope >0 & ((upper > 1 & lower > 1) |( upper < 1 & lower < 1)))|is.na(obs_slope)] 
#df_orig2 <- df_orig2[!is.na(obs_slope)]
#df_orig2 <- df_orig2[!is.infinite(obs_slope)]
df_orig2[, cov_row_sum := rowSums(.SD, na.rm = T), 
         .SDcols = names(df_orig2)[names(df_orig2) %like% "confounders" & !names(df_orig2) %like%"sdi|haq|other"]]
df_orig2[, max_cov_row_sum := max(cov_row_sum, na.rm= T), by = .(age_interval, Mother_Father_education, cohort_unexp_def_mid, cohort_exp_def_mid)]


df_orig2[age_start == 99, age_start_temp := tstrsplit(int, " to ", keep = 1, type.convert = T)]
df_orig2[age_start == 99, age_start := as.numeric(age_start_temp)/12]
df_orig2[age_end == 99, age_end_temp := tstrsplit(int, " to ", keep = 2)]
df_orig2[age_end == 99, age_end := as.numeric(gsub( " Months", "", age_end_temp))/12]

df_orig2[, age_midpoint := ((age_start + age_end)/2)]
df_orig2[, logage_midpoint := log(age_midpoint)]
df_orig2 <- df_orig2[!is.na(age_midpoint)]

########################################################################
## Create a per-unit-of-education effect size for interactive terms
########################################################################

df_orig2[, denom_int := ((cohort_exp_def_lower + cohort_exp_def_upper)/2) - ((cohort_unexp_def_lower + cohort_unexp_def_upper)/2)]
df_orig2[, std_effect := log(effect_size)/denom_int]

########################################################################
## Age dummies for interactive term
########################################################################



#create age group dummies
for(c.age in unique(df_orig2$age_interval_std)){
  df_orig2[age_interval_std == c.age,paste0("dummy_", c.age) := 1]
  df_orig2[is.na(get(paste0("dummy_", c.age))),paste0("dummy_", c.age) := 0]
  
}
df_orig2 <- df_orig2[!is.na(age_interval_std)]
unique(df_orig2$age_interval_std) %>% paste0("dummy_", .) -> age_dummies
age_dummies <- age_dummies[age_dummies != "dummy_0 to 5, real"]






##########################################################
## Set up Model characteristics for initial crosswalking model
##########################################################
## Toggle for multiplicative models
multiplicative_model <- T

#set up covariate list

#covariate_list = ["confounders_age", "confounders_sex"]

# covariate_list = c("confounders_age", "confounders_sex", 
#                   "confounders_parity", "confounders_rural_urban_residence",
#                   "confounders_mother_marital_status", "confounders_wealth",
#                   "confounders_otherparenteducation",
#                   "confounders_water", "confounders_perinatal_care_received",
#                   'dummy_Sub-Saharan Africa','dummy_South Asia',
#                   'dummy_other_regions')

covariate_list = c("confounders_rural_urban_residence",
                   "confounders_wealth", "confounders_otherparenteducation", "age_midpoint")

covariate_list = c("confounders_rural_urban_residence",
                   "confounders_wealth", "confounders_otherparenteducation","confounders_sex", "confounders_age", age_dummies)
########################################################################
## Create Interactive terms for model, natated as "__covariate__int"
########################################################################
for(c.cov in covariate_list){
  df_orig2[, paste0(c.cov, "int") := get(c.cov) * denom_int]
}

#create seq id to merge on
df_orig2[, seq_id := 1:nrow(df_orig2)]


#############################################################################
## Run one mat and one pat model on all data combined
## This is the crosswalking model. Because data availability varies 
## by subset, this model standardizes all data using the assumption that the 
## effect sizes that vary by model design will vary consistently (in relative space)
## across all age groups.
#############################################################################


c.mat <- 2

xwalker <- function(c.mat){
  data1 <- MRData()
  c.covariate_list <- copy(covariate_list)
  if(multiplicative_model){
    c.covariate_list <- c(paste0(covariate_list, "int"))
  }
  
  data1$load_df(
    data = df_orig2[ Mother_Father_education == c.mat & age_end <= 5], 
    col_obs = 'ln_effect_size', 
    col_obs_se = 'log_se', 
    col_covs = c(c.covariate_list, covariate_list, "seq_id", "cohort_exp_def_lower", 
                 "cohort_exp_def_upper","cohort_unexp_def_lower", "cohort_unexp_def_upper", "age_midpoint", "seq_id"),
    col_study_id = 'field_citation_value'
  )
  #add list of cov objects
  linear_covs <- lapply(c.covariate_list, LinearCovModel)
  
  # prior_spline_monotonicity='decreasing')
  cov_main <- LinearCovModel(
    alt_cov = list("cohort_exp_def_lower", "cohort_exp_def_upper"),
    ref_cov = list("cohort_unexp_def_lower", "cohort_unexp_def_upper"),
    name = "exposure",
    use_re = TRUE
  )
  # 
  # covfinder <- CovFinder(
  #   data = data1, 
  #   covs = as.list(c.covariate_list),
  #   pre_selected_covs = list("cohort_exp_def_lower", "cohort_exp_def_upper",
  #                            "cohort_unexp_def_lower", "cohort_unexp_def_upper"), 
  #   num_samples = 100L, 
  #   power_range = list(3, 6), 
  #   power_step_size = 1, 
  #   laplace_threshold = 1e-5
  # )
  # covfinder$select_covs(verbose = FALSE)
  # covfinder$selected_covs
  # 
  # define the model object
  mr = MRBRT(
    data = data1, 
    # cov_models=list(cov_intercept, cov1, cov2, cov3),
    #cov_models=list.append(cov_intercept,cov_main,linear_covs),
    cov_models=list.append(cov_main,linear_covs),
    inlier_pct = inpct
  )
  
  # mr$fit_model(inner_print_level = 5L, inner_max_iter = 100L)
  mr$fit_model(inner_print_level = 5L, inner_max_iter = 100L)
  
  
  ########################################################################
  ## Create exposure frame and predict
  ########################################################################
  
  exposure <- seq(0,19, .25)
  exposure0 <- rep(0, length(exposure))
  
  
  dt = data.table(expand.grid(intercept = 1,cohort_exp_def_lower = exposure, cohort_unexp_def_lower = c(0), cohort_unexp_def_upper = c(0)))
  dt[, cohort_unexp_def_upper := cohort_unexp_def_lower]
  dt[, cohort_exp_def_upper := cohort_exp_def_lower]
  
  ## All simulated RRs are exposure = n compared to exposure = 0, except there's one thats 12 compared to 6.
  dt <- dt[cohort_unexp_def_upper == 0|(cohort_unexp_def_upper == 6 & cohort_exp_def_upper == 12)|(cohort_unexp_def_upper == 6 & cohort_exp_def_upper == 6)]
  df_pred <- unique(dt)
  
  
  ## This creates all combos of covariates, minimally and maximally controlled, with no in between except for minimally controlled except for each covariate at a time.
  dummies::dummy(age_dummies) %>% data.table() %>% setnames(., age_dummies)-> temp_dum_merge
  temp_dum_merge <- rbind(temp_dum_merge, data.table(t(c(0,0,0,0,0))), use.names = F)
  temp_dum_merge[, intercept := 1]
  
  cover <- function(c.cov){
    df_temp <- df_pred %>% copy()
    df_temp[, (c.cov) := 1]
    df_temp2 <- df_pred %>% copy()
    df_temp2[, (c.cov) := 0]
    #do it separately by ages
    if(!c.cov %like% "dummy"){
      df_temp3 <- df_pred %>% copy()
      df_temp3[, (c.cov) := 1]
      df_temp3 <- merge(df_temp3, temp_dum_merge, by = "intercept", allow.cartesian = T)
      df_temp4 <- df_pred %>% copy()
      df_temp4[, (c.cov) := 0]
      df_temp4 <- merge(df_temp4, temp_dum_merge, by = "intercept", allow.cartesian = T)
      return(rbind(df_temp, df_temp2, df_temp3, df_temp4, fill = T))
    }else{
      return(rbind(df_temp, df_temp2))
    }
  }
  
  if(length(covariate_list) > 0){
    rbindlist(lapply(covariate_list[covariate_list %like% "confounders|dummy" & !covariate_list %like% "int|midpoint"], cover), fill = T) -> df_pred
  }
  
  df_pred1 <- na.replace(df_pred,  0)
  
  df_pred2 <- na.replace(df_pred,  1)
  
  
  
  
  df_pred <- unique(rbind(df_pred1, df_pred2))
  
  age <- data.table(intercept = rep(1,length(c(.041665, .5,.5416667, 2.5, 2.541667,3))), age_midpoint = c(.041665, .5,.5416667, 2.5,2.541667, 3))
  age[, logage_midpoint := log(age_midpoint)]
  df_pred <- merge(df_pred, age, by = "intercept", allow.cartesian = T)

  
  # Create multiplicative variable exposures
  for(c.var in names(df_pred)[ (!names(df_pred) %like% "int" & names(df_pred) %like% "confound|dummy")|names(df_pred) %like% "point"]){
    df_pred[, (paste0(c.var, "int")) := get(c.var) * cohort_exp_def_upper]
  }
  
  data_pred = MRData()
  
  # Creatd data object for prediction
  data_pred$load_df(df_pred,
                    col_covs = list.append("cohort_exp_def_lower", "cohort_exp_def_upper", 
                                           "cohort_unexp_def_lower", "cohort_unexp_def_upper", c.covariate_list, "age_midpoint", "logage_midpoint"))
  
  
  
  # Predict samples
  print(c('beta_soln :', mr$beta_soln))
  print(c('gamma_soln:', mr$gamma_soln))
  # sample solution
  # beta_samples, gamma_samples = mr.sample_soln(sample_size=10)
  # samples <- mr$sample_soln(sample_size = 10L)
  samples <- mr$sample_soln(sample_size = c.sample_size)
  names(samples) <- c("beta", "gamma")
  
  # create draws
  samples2 <- samples
  samples2[["beta"]][1, ] <- mr$beta_soln
  samples2[["gamma"]][1,1] <- mr$gamma_soln
  log_rr_draws = mr$create_draws(data = data_pred,
                                 # sample_size=10L, 
                                 # beta_samples=samples[["beta"]],
                                 # gamma_samples=samples[["gamma"]],
                                 beta_samples=samples2[["beta"]],
                                 gamma_samples=samples2[["gamma"]],
                                 random_study =FALSE)
  
  preds <- cbind(df_pred, log_rr_draws)
  setnames(preds, paste0("V", 1:c.sample_size), paste0("rr", 1:c.sample_size), skip_absent = T)
  preds[, maternal_education := cohort_exp_def_lower]
  
  names(samples) <- c("beta", "gamma")
  gammas <- samples$gamma %>% data.table()
  samples <- samples$beta %>% data.table()
  if(length(c.covariate_list) > 0){
    setnames(samples, c("exposure", mr$cov_names[5:length(mr$cov_names)]))
  }else{
    setnames(samples, "exposure")
  }
  samples[, draw := 1:c.sample_size]
  samples[, mother_father := c.mat]
  gammas[, draw := 1:c.sample_size]
  gammas[, mother_father := c.mat]
  
  preds[, mother_father := c.mat]
  preds
  
  outliers = data.table(data1$to_df())
  outliers <- merge(outliers, df_orig2, by = c("seq_id",c.covariate_list, covariate_list))
  outliers[, inlier := round(mr$w_soln)]
  outliers[, mother_father := c.mat]
  outliers
  
  return(list(preds, samples, outliers, gammas))
}

lapply(1:2, xwalker) -> xwalked_list

rbindlist(list(xwalked_list[[1]][[1]], xwalked_list[[2]][[1]])) -> all_preds
rbindlist(list(xwalked_list[[1]][[2]], xwalked_list[[2]][[2]])) -> samples
rbindlist(list(xwalked_list[[1]][[3]], xwalked_list[[2]][[3]])) -> outliers

rbindlist(list(xwalked_list[[1]][[4]], xwalked_list[[2]][[4]])) -> gammas

# ##merge on age interval
# ints <- c(.041665, .5,.5416667, 2.5,2.541667, 3)
# int_dt <- data.table(age_midpoint = ints,  age_interval_std = c("0 to 0.1, real", "0 to 1, real", "0.1 to 1, real", "0 to 5, real", "1 to 5, real"))

#

all_preds[, rowsum_dummies := rowSums(.SD),  .SDcols = age_dummies]
all_preds <- all_preds[rowsum_dummies %in% 0:1]

for(c.age in age_dummies){
  print(c.age)
  all_preds[get(c.age) ==1, age_interval_std := gsub("dummy_", "", c.age)]
}
all_preds[is.na(age_interval_std), age_interval_std := "0 to 5, real"]


#all_preds <- merge(all_preds, int_dt)
outliers[, age_midpoint := (age_start + age_end) / 2]


# 
dir.create(paste0("<<<<< filepath redacted >>>>>mrbrt_output/", c.model_version), recursive = T)
write.csv(outliers, paste0("<<<<< filepath redacted >>>>>mrbrt_output/", c.model_version, "/r_mrbrt_outliers.csv"))
write.csv(all_preds, paste0("<<<<< filepath redacted >>>>>mrbrt_output/", c.model_version, "/r_mrbrt_preds.csv"))
write.csv(samples, paste0("<<<<< filepath redacted >>>>>mrbrt_output/", c.model_version, "/r_mrbrt_samples.csv"))
write.csv(gammas, paste0("<<<<< filepath redacted >>>>>mrbrt_output/", c.model_version, "/r_mrbrt_gammas.csv"))
