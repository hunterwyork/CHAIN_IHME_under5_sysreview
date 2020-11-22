library(data.table)
library(survival)
library(coxme, lib.loc = "<<<<< filepath redacted >>>>>rlibs")
library(magrittr)
library(haven)
library(foreign)
library(dplyr)
library(stringr)
library(ggplot2)
library(parallel)

library(sf)


source("/<<<<< filepath redacted >>>>>r/get_location_metadata.R")
locs <- get_location_metadata(location_set_id = 22, gbd_round_id = 7, decomp_step = "iterative")

#RR Estimation of Childhood mortality from Complete birth histories
##Hunter York
#Arguments
jpath <- ifelse(Sys.info()[1]=="Windows", "J:/", "/home/j/")
cores <- 6
source(paste0(jpath,"Project<<<<< filepath redacted >>>>>.R"))
#install.packages("lmerTest", lib = "<<<<< filepath redacted >>>>>rlibs")

###load cbh files
#files <- list.files(paste0(jpath,"<<<<< filepath redacted >>>>>_2018/maternal_education/data/ubcov_output_7102019"),full.names=T)
files <- list.files(paste0(jpath,"<<<<< filepath redacted >>>>>_2018/maternal_education/data/ubcov_output"),full.names=T)

# data.list <- mclapply(files, function(file) {
#   data <- readstata13::read.dta13(file)
# },mc.cores=ifelse(Sys.info()[1]=="Windows", 1, cores))

data.list <- lapply(files, function(file) {
  print(file)
  data <- readstata13::read.dta13(file)
})


merged_data <- data.table(rbindlist(data.list,fill=T))


merged_data[age_of_death_units == 1, child_age_at_death_months := age_of_death_number /30]

###
merged_data <- merged_data[,.(nid, hh_id, strata, psu, admin_1,
                              ihme_loc_id, year_start, 
                              age_of_death_number, age_of_death_units, 
                              child_age_at_death_months, child_age_at_death_raw,
                              child_alive, child_dob_cmc, child_id, child_sex,
                              maternal_ed_yrs, mother_id, wealth_index_dhs,
                              wealth_index_dhs_continuous,
                              age_month, pweight, urban, mother_age_years,
                              paternal_ed_yrs, int_month, int_year,
                              mother_height_cm_1d, interview_date_cmc, contraceptive_type,
                              geospatial_id, height_for_age_sd,	weight_for_age_sd,	weight_for_height_sd,
                              bcg, dpt_1, dpt_2, dpt_3, polio_0, polio_1, polio_2, polio_3, measles)]

#############################################
#create outcome identifiers#
#############################################
merged_data[child_age_at_death_months < 180, death_under_15 := 1]
merged_data[!(child_age_at_death_months < 180), death_under_15 := 0]
merged_data[child_age_at_death_months < 120, death_under_10 := 1]
merged_data[!(child_age_at_death_months < 120), death_under_10 := 0]
merged_data[child_age_at_death_months < 60, death_under_5 := 1]
merged_data[!(child_age_at_death_months < 60), death_under_5 := 0]
merged_data[child_age_at_death_months < 12, death_under_1 := 1]
merged_data[!(child_age_at_death_months < 12), death_under_1 := 0]

merged_data[child_age_at_death_months < 180 & child_age_at_death_months >= 60, mort10q5 := 1]
merged_data[!(child_age_at_death_months < 180 & child_age_at_death_months >= 60), mort10q5 := 0]
merged_data[child_age_at_death_months < 60, mort5q0 := 1]
merged_data[!(child_age_at_death_months < 60), mort5q0 := 0]
merged_data[mort5q0 == 1, mort10q5 := NA]
merged_data[is.na(child_age_at_death_months), mort10q5 := 0]
merged_data[is.na(child_age_at_death_months), mort5q0 := 0]


####################################
#create demographic identifiers
####################################
merged_data[, age_year := floor(age_month / 12)]
merged_data[,interview_year := 1900+ floor((interview_date_cmc -1)/12)]
merged_data[, year_of_birth := 1900 + floor((child_dob_cmc -1)/12)]
merged_data[,mother_age_at_birth := mother_age_years - age_year]

merged_data[mother_age_at_birth < 20, mother_age_at_birth_binned := "< 20"]
merged_data[mother_age_at_birth >= 20 & mother_age_at_birth < 35, mother_age_at_birth_binned := "20 to 34"]
merged_data[mother_age_at_birth >= 35, mother_age_at_birth_binned := "35+"]
merged_data[, mother_age_at_birth_binned := factor(mother_age_at_birth_binned, levels = c("20 to 34", "< 20", "35+"))]

merged_data[child_age_at_death_months == 6000, child_age_at_death_months := NA]
merged_data[maternal_ed_yrs > 25, maternal_ed_yrs := NA]
merged_data[maternal_ed_yrs > 18, maternal_ed_yrs := 18]
merged_data[paternal_ed_yrs > 25, paternal_ed_yrs := NA]
merged_data[paternal_ed_yrs > 18, paternal_ed_yrs := 18]

######################################################################
#tabulate total person years (to death or to interview, depending)
######################################################################
merged_data[,birthtointerview_cmc := interview_date_cmc - child_dob_cmc]
merged_data[child_alive == "Yes", person_months := birthtointerview_cmc]
merged_data[child_alive == "No", person_months := child_age_at_death_months]
merged_data[, person_years := person_months/12]

#create specific person year caps for different demographic bins
##assume all babies with person years 0 lived to be a day  old
merged_data[person_years == 0, person_years := .002739]

merged_data[, person_years_5q0 := person_years]
merged_data[, person_years_5q10 := person_years]
merged_data[person_years < 10, person_years_5q10 := NA]
merged_data[, person_years_10q5 := person_years]
merged_data[person_years < 5, person_years_10q5 := NA]
merged_data[, person_years_5q5 := person_years]
merged_data[person_years < 5, person_years_5q5 := NA]
merged_data[, person_years_4q1 := person_years]
merged_data[person_years < 1, person_years_4q1 := NA]
merged_data[, person_years_1q0 := person_years]

##create dummy vars to allow for correct survival objects to be created
merged_data[, dummy0 := 0]
merged_data[, dummy1 := 1]
merged_data[, dummy5 := 5]
merged_data[, dummy10 := 10]
merged_data[mort5q0 == 1, dummy5 := NA]

######################################################################
#create dummy cascade wealth variable
######################################################################
##lowercase wealth var
merged_data[, wealth_index_dhs := tolower(wealth_index_dhs)]

#cascade wealth var
merged_data[wealth_index_dhs == "", wealth_index_dhs := NA]
merged_data[!is.na(wealth_index_dhs), wealth_index_dhs_low := 1]
merged_data[is.na(wealth_index_dhs), wealth_index_dhs_low := NA]
merged_data[wealth_index_dhs %in% c("2", "poorer", "3", "middle", "4", "richer", "5", "richest"), wealth_index_dhs_middle_low := 1]
merged_data[!wealth_index_dhs %in% c("2", "poorer", "3", "middle", "4", "richer", "5", "richest"), wealth_index_dhs_middle_low := 0]
merged_data[is.na(wealth_index_dhs), wealth_index_dhs_middle_low := NA]
merged_data[wealth_index_dhs %in% c("3", "middle", "4", "richer", "5", "richest"), wealth_index_dhs_middle := 1]
merged_data[!wealth_index_dhs %in% c("3", "middle", "4", "richer", "5", "richest"), wealth_index_dhs_middle := 0]
merged_data[is.na(wealth_index_dhs), wealth_index_dhs_middle := NA]
merged_data[wealth_index_dhs %in% c("4", "richer", "5", "richest"), wealth_index_dhs_middle_high := 1]
merged_data[!wealth_index_dhs %in% c( "4", "richer", "5", "richest"), wealth_index_dhs_middle_high := 0]
merged_data[is.na(wealth_index_dhs), wealth_index_dhs_middle_high := NA]
merged_data[wealth_index_dhs %in% c("5", "richest"), wealth_index_dhs_high := 1]
merged_data[!wealth_index_dhs %in% c("5", "richest"), wealth_index_dhs_high := 0]
merged_data[is.na(wealth_index_dhs), wealth_index_dhs_high := NA]


####################################################################
##decade demarcate child year born, limit to between 1970 and 2000##
####################################################################
merged_data[,child_year_born := 1900 + floor((child_dob_cmc -1)/12)]
merged_data[,decade_child_born := floor(child_year_born/10)*10]
merged_data[decade_child_born < 1970, decade_child_born := 1970]
merged_data[decade_child_born > 2000, decade_child_born := 2000]

#do the same for mother decade age at birth
merged_data[, mother_decade_age_at_birth := floor(mother_age_at_birth/10)*10]


#mother_id_var
merged_data[, mother_id_var := paste(ihme_loc_id, year_start, hh_id, psu, strata,mother_id, sep = "_")]
merged_data[, mother_id_var := as.numeric(as.factor(mother_id_var))]

#mother_height_var
merged_data[,mother_height_cm := as.numeric(mother_height_cm_1d / 10)]
merged_data[mother_height_cm < 60|mother_height_cm > 240 , mother_height_cm := NA]

#create height z score - this is done from only the data provided and may not be comparable to global averages
#basically 1 = mother is in bottom 5% of heights
qcut = function(x, n) {
  quantiles = seq(0, 1, length.out = n+1)
  cutpoints = unname(unique(quantile(x, quantiles, na.rm = TRUE)))
  as.character(cut(x, cutpoints, include.lowest = TRUE, labels = F))
}

merged_data[, zscoreheight := qcut(mother_height_cm, 20)]
merged_data[zscoreheight > 1, zscoreheight := 0]
merged_data[, zscoreheight := as.numeric(zscoreheight)]

#create simpler categorical variable based on citation
merged_data[mother_height_cm < 145, mother_height_binned := "<145"]
merged_data[mother_height_cm >= 145 & mother_height_cm < 150, mother_height_binned := "145-149.9"]
merged_data[mother_height_cm >= 150 & mother_height_cm < 155, mother_height_binned := "150-154.9"]
merged_data[mother_height_cm >= 155 & mother_height_cm < 160, mother_height_binned := "155-159.9"]
merged_data[mother_height_cm >= 160, mother_height_binned := ">=160"]
merged_data$mother_height_binned <- factor(merged_data$mother_height_binned, levels = c(">=160", "155-159.9", "150-154.9", "145-149.9", "<145"))

#create survey id
merged_data[, survey_id := paste0(ihme_loc_id, year_start)]

###delete implausible things
merged_data[age_month > child_age_at_death_months,age_month := child_age_at_death_months]
merged_data[,age_year := floor(age_month/12)]

#create 
merged_data[, adj_age := age_year - 5]

#fix wealth variable
merged_data[wealth_index_dhs == 1, wealth_index_dhs := "poorest"]
merged_data[wealth_index_dhs == 2, wealth_index_dhs := "poorer"]
merged_data[wealth_index_dhs == 3, wealth_index_dhs := "middle"]
merged_data[wealth_index_dhs == 4, wealth_index_dhs := "richer"]
merged_data[wealth_index_dhs == 5, wealth_index_dhs := "richest"]
merged_data[, wealth_index_dhs := relevel(as.factor(wealth_index_dhs), ref = "middle")]

#create indicator for if other children died
merged_data[, first_child_death := min(child_dob_cmc[!is.na(age_of_death_number)]), by = mother_id_var]
merged_data[is.infinite(first_child_death), first_child_death := NA]
merged_data[first_child_death < child_dob_cmc, older_sibling_dead := 1]
merged_data[is.na(older_sibling_dead), older_sibling_dead := 0]

merged_data[,vacc_scale_self_report :=rowSums(.SD),.SDcols = names(merged_data)[names(merged_data) %like% "self_report" & !names(merged_data) %like% "scale" ]]
merged_data[,vacc_scale_card :=rowSums(.SD),.SDcols = names(merged_data)[names(merged_data) %like% "card" & !names(merged_data) %like% "scale" ]]

merged_data[, decade_child_born := decade_child_born - 2000]
merged_data <- merged_data[complete.cases(merged_data[,.(mother_height_binned, paternal_ed_yrs, wealth_index_dhs_continuous)])]

#create table

####
for(c.ihme_loc_id in unique(merged_data$ihme_loc_id)){
  print(c.ihme_loc_id)
  merged_data[ihme_loc_id == c.ihme_loc_id] %>% fwrite(., paste0("/<<<<< filepath redacted >>>>>education<<<<< filepath redacted >>>>>under_5_mort/data/input_data/", c.ihme_loc_id, ".csv"))
}
####
for(c.survey_id in unique(merged_data$survey_id)){
  print(c.survey_id)
  merged_data[survey_id == c.survey_id] %>% fwrite(., paste0("/<<<<< filepath redacted >>>>>education/under_5_mort/data/input_data/", c.survey_id, ".csv"))
}


template <- data.table(expand.grid(survey_id = unique(merged_data$survey_id), 
                                   new_var = c("",
                                               "wealth_index_dhs_continuous", 
                                               # "child_sex", 
                                               # "mother_age_at_birth_binned",
                                               # "decade_child_born",
                                               # "mother_father_higher_ed",
                                               "wealth_index_dhs_continuous child_sex mother_age_at_birth_binned std_year_born")))
# "maternal_ed_yrs:avg_mat_ed paternal_ed_yrs:avg_pat_ed",
# "wealth_index_dhs_continuous child_sex mother_age_at_birth_binned decade_child_born maternal_ed_yrs:avg_mat_ed paternal_ed_yrs:avg_pat_ed")))
fwrite(template, "/<<<<< filepath redacted >>>>>/education/under_5_mort/code/array_template.csv")
#launch location-specific models
qsub_root <- "<<<<< filepath redacted >>>>>/education/update_2019/code/"
code_root <- "/<<<<< filepath redacted >>>>>/education/under_5_mort/code/"
n_jobs <- length(template[,c.iso])
n_jobs <- 342
c.proj <- "proj_team"



system(paste0("qsub -N educ_ineq_gbd -P ",c.proj, " -t 1:", n_jobs,
              " -l fthread=2 -l m_mem_free=20G -q long.q -l h_rt=1:00:00 -l archive=TRUE ",
              qsub_root,"shells/r_shell_35107.sh ",code_root,"analyze_under_5.R"), intern = T)->job_string


