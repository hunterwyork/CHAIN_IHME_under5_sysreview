#################################################################################
###  Take Risk Extraction Sheet for child mortality and clean it for analysis ###
#################################################################################
###  Hunter York, hyork@uw.edu
###  07/30/2019
#################################################################################
###  Description:
###
###  Takes all Sheets in inputs folder, appends them, and standardizes columns
###  creates simple graphs of raw data to use for vetting
#################################################################################


#################################################################################
## Setup ##
#################################################################################

# Set up root directories
root <- "<<<<< filepath redacted >>>>>Desktop/child_mort_dir" ## replace with directory on host computer
input_dir <- paste0(root, "/inputs")
code_dir <- paste0(root, "/code")

# 
locs <- fread(paste0("<<<<< filepath redacted >>>>>gbd_2020_locs.csv"))
locs <- locs[level == 3]

# install libraries and load them
list.of.packages <- c("data.table", "plyr", "dplyr", "ggplot2", "readxl", "grid", "scales", "stringr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
lapply(list.of.packages, require, character.only = TRUE)

#source fxns script
source(paste0(code_dir, "/cleaning_fxns.R"))
#################################################################################
## Load data ##
#################################################################################

#make an object of all input files if more than one
data.files <- list.files(input_dir, full.names = T)

#read them and bind them into one file
lapply(data.files, FUN = function(x){
  temp <- read_excel(x, sheet = "extraction") 
  temp <- data.frame(temp)
  temp <- data.table(temp)
  #remove descriptions rows (1-2), ensure you're not accidentally deleting data
  temp <- temp[-(1:2),]
  return(temp)
}) %>% 
  rbindlist(fill = T) -> input_data



#################################################################################
## Clean and Shape ##
#################################################################################

#remove breaks between entries
input_data <- input_data[!is.na(field_citation_value)]

#remove names of extractors
input_data <- input_data[!field_citation_value %like% "extraction"]

#add partner's education as paternal education
input_data[tolower(note_sr) %like% "partner" & Mother_Father_education == 0, Mother_Father_education := 1]
input_data[tolower(note_sr) %like% "spouse" & Mother_Father_education == 0, Mother_Father_education := 1]
input_data[tolower(note_sr) %like% "head" & Mother_Father_education == 0, Mother_Father_education := 1]


#fix backwards exposed and unexposed data for two extractions
input_data[field_citation_value == "Maternal education and child survival: A comparative study of survey data from 17 countries", cohort_exposed_def1_arch := cohort_exposed_def1]
input_data[field_citation_value == "Maternal education and child survival: A comparative study of survey data from 17 countries", cohort_exposed_def1 := cohort_unexp_def1]
input_data[field_citation_value == "Maternal education and child survival: A comparative study of survey data from 17 countries", cohort_unexp_def1 := cohort_exposed_def1_arch]
input_data[field_citation_value == "Maternal education and child survival: A comparative study of survey data from 17 countries", cohort_exposed_def1_arch := NULL]


#count dhs sources
input_data[tolower(field_citation_value) %like% "dhs|demographic" | tolower(note_sr) %like% "dhs|demographic"  | tolower(study_name) %like% "dhs|demographic",
           str_sub(tolower(field_citation_value),1, -4)] %>% unique() %>% length()


input_data <- input_data[!tolower(field_citation_value) %like% "dhs|demographic" & !tolower(note_sr) %like% "dhs|demographic"  & !tolower(study_name) %like% "dhs|demographic"]

#drop duplicated files
input_data <- input_data[is.na(Duplicate_Merged) | Duplicate_Merged == "2"]

#replace fieldcitationvalue with study id
input_data[, new_field_citation_value := paste0("study_", Indiv_study_id)]
input_data[, .(new_field_citation_value, field_citation_value)] %>% 
  .[,.(unique(field_citation_value)[1]), by = new_field_citation_value] -> citations_map
input_data[, field_citation_value := new_field_citation_value]
write.csv(citations_map, paste0(root, "/outputs/citations_map.csv"), row.names = F)


#slim down working dataset to not include metadata
working_data <- input_data[,.(field_citation_value, location_name, rep_geography, 
                              year_start_study, year_end_study, age_start,
                              age_end, design, effect_size_measure, 
                              effect_size, lower, upper,
                              CI_uncertainty_type_value,nonCI_uncertainty_type, nonCI_uncertainty_value,
                              Mother_Father_education,
                              cc_cases,
                              cc_control,
                              
                              cohort_sample_size_exp, cohort_sample_size_unexp, cohort_sample_size_total,
                              cohort_exposed_def1, cohort_exposed_def2, cohort_unexp_def1,cohort_unexp_def2,
                              Absolute_sample_size, Absolute_number_of_death,
                              Mortality_rate_exposed.group, Mortality_rate_unexposed, cc_exposed_def1, cc_exposed_def2, 
                              cc_exp_unit_rr, cc_unexposed_def1, cc_unexposed_def2)]


#add covariates
covs <- input_data[,.SD,.SDcols =names(input_data)[names(input_data) %like% "confounders"]]
other_covs <- covs$confounders_other %>%
  strsplit(split = ",") %>% 
  unlist %>% tolower() %>% 
  gsub("[^a-zA-Z0-9 ., ]", "", .) %>% 
  gsub(" ", "", .) %>% 
  unique() %>% 
  .[!is.na(.)]


#add other covariates
working_data <- cbind(working_data, covs)

#
working_data <- create_covariates(working_data)

#create a year midpoint variable
working_data[, year_midpoint := (as.numeric(year_end_study) + as.numeric(year_start_study)) / 2]

#create location- year label
working_data[, location_year := paste0(location_name,"_", year_start_study)]

#create age interval label
working_data[, age_interval := paste0(substr(as.numeric(age_start), 1,5), " to ", substr(as.numeric(age_end),1,5), " years of age")]

#create number of data points contributed per study
working_data[, field_citation_value_substr := field_citation_value]
working_data[, study_size := length(field_citation_value), by = .(field_citation_value_substr, location_name)]

#create ihme_loc_id indicator and merge on location hierarchy
working_data[, ihme_loc_id := tstrsplit(location_name, "|", keep = 2, fixed = T)]
working_data <- merge(working_data, locs[,.(ihme_loc_id, region_name, super_region_name)], by = "ihme_loc_id")

#create dummy vars for superregion
for(c.sup_reg_name in unique(working_data[super_region_name != "High-income",super_region_name])){
  working_data[super_region_name == c.sup_reg_name, paste0("dummy_", c.sup_reg_name) := 1]
  working_data[is.na(get(paste0("dummy_", c.sup_reg_name))),  paste0("dummy_", c.sup_reg_name) := 0]
}

#creat variable for mother's, father's or combined ed
working_data[Mother_Father_education ==2, parent_gender:= "Female"]
working_data[Mother_Father_education ==1, parent_gender:= "Male"]
working_data[Mother_Father_education ==0, parent_gender:= "Other"]

#################################################################################
## Create Standardize Exposure Intervals ##
#################################################################################

#for locations that are missing a numeric exposure interval, fill with standard vars
# THIS SHOULD NOT BE NECESSARY, BUT IS BEING DONE TEMPORARILY UNTIL THESE VALUES ARE REPLACED
# working_data[is.na(cohort_exposed_def1) & !is.na(cohort_exposed_def2) & cohort_exposed_def2 %like% "Illiterate|illiterate|No|no|None|none", cohort_exposed_def2 := "0 to 0"]
# working_data[is.na(cohort_unexp_def1) & !is.na(cohort_unexp_def2) & cohort_unexp_def2 %like% "Illiterate|illiterate|No|no|None|none", cohort_unexp_def2 := "0 to 0"]
# working_data[is.na(cohort_exposed_def1) & !is.na(cohort_exposed_def2) & cohort_exposed_def2 %like% "Primary|primary", cohort_exposed_def2 := "1 to 6"]
# working_data[is.na(cohort_unexp_def1) & !is.na(cohort_unexp_def2) & cohort_unexp_def2 %like% "Primary|primary", cohort_unexp_def2 := "1 to 6"]
# working_data[is.na(cohort_exposed_def1) & !is.na(cohort_exposed_def2) & cohort_exposed_def2 %like% "Secondary|secondary|High school|high school", cohort_exposed_def2 := "7 to 12"]
# working_data[is.na(cohort_unexp_def1) & !is.na(cohort_unexp_def2) & cohort_unexp_def2 %like% "Secondary|secondary|High school|high school", cohort_unexp_def2 := "7 to 12"]
# working_data[is.na(cohort_exposed_def1) & !is.na(cohort_exposed_def2) & cohort_exposed_def2 %like% "Tertiary|tertiary|college|College|vocation|Vocation|professional|Professional|university|University", cohort_exposed_def2 := "13 to 18"]
# working_data[is.na(cohort_unexp_def1) & !is.na(cohort_unexp_def2) & cohort_unexp_def2 %like% "Tertiary|tertiary|college|College|vocation|Vocation|professional|Professional|university|University", cohort_unexp_def2 := "13 to 18"]

#copy case controls vars into cohort vars for computational purposes
working_data[tolower(design) %like% "control", cohort_exposed_def1 := cc_exposed_def1]
working_data[tolower(design) %like% "control", cohort_unexp_def1 := cc_unexposed_def1]

#remove question marks and equal signs
working_data[,cohort_unexp_def1 := gsub("\\?|=", "", cohort_unexp_def1)]
working_data[,cohort_exposed_def1 := gsub("\\?|=", "", cohort_exposed_def1)]

#change one fringe case
working_data[ cohort_unexp_def1 == "9 years and above", cohort_unexp_def_1 := ">8"]


#run create bins fxn
working_data <- create_bins(working_data, "cohort_exposed_def1", "cohort_unexp_def1" )

#print unstandardized vals
working_data[,.( cohort_unexp_var_arch ,clean_cohort_unexp_def)] %>% unique() %>% .[clean_cohort_unexp_def %like% "NA"]
working_data[,.( cohort_exposed_var_arch ,clean_cohort_exp_def)] %>% unique() %>% .[clean_cohort_exp_def %like% "NA"]

#coerce effect size, upper and lower to numeric
working_data[, effect_size := as.numeric(effect_size)] %>% .[, upper := as.numeric(upper)] %>% .[, lower := as.numeric(lower)]

#################################################################################
## outlier non-usable data ##
#################################################################################

working_data <- working_data[!effect_size_measure %like% "Concentration"]
working_data[, cohort_sample_size_total := as.numeric(gsub("[^0-9.-]", "", cohort_sample_size_total))]


#################################################################################
## Create confidence intervals for data missing them ##
#################################################################################

## Create indicator for non-imputed CIs
working_data[!is.na(upper)|!is.na(nonCI_uncertainty_value), CI_orig := 1]
working_data[is.na(CI_orig), CI_orig := 0]

#use midpoint of CI when possible for effect size
working_data[is.na(effect_size), effect_size := (upper + lower)/2]

#add sample size for Case controls
working_data[design %like% "ase" & is.na(cohort_sample_size_total), cohort_sample_size_total := as.numeric(cc_cases) + as.numeric(cc_control)]
working_data[design %like% "ase" & is.na(cohort_sample_size_total) & (!is.na(cc_cases)&is.na(cc_control)), cohort_sample_size_total := as.numeric(cc_cases)]
working_data[design %like% "ase" & is.na(cohort_sample_size_total) & (is.na(cc_cases)&is.na(cc_control)), cohort_sample_size_total := Absolute_sample_size]

create_conf_ints()

#################################################################################
## Create study sizes variable ##
#################################################################################

working_data[, study_sizes := .N, by = .(field_citation_value)]

#################################################################################
## Identify problem extractions ##
#################################################################################
missing_exp_ranges <- working_data[!complete.cases(working_data[,.(
  cohort_exp_def_lower, cohort_unexp_def_lower,
  cohort_exp_def_upper, cohort_unexp_def_upper)]), unique(field_citation_value)]

overlapping_exposure_ref <- working_data[(cohort_unexp_def_lower >= cohort_exp_def_lower & cohort_unexp_def_lower <= cohort_exp_def_upper)|
                                           (cohort_unexp_def_upper >= cohort_exp_def_lower & cohort_unexp_def_upper <= cohort_exp_def_upper)|
                                           (cohort_exp_def_lower >= cohort_unexp_def_lower & cohort_exp_def_lower <= cohort_unexp_def_upper)|
                                           (cohort_exp_def_upper >= cohort_unexp_def_lower & cohort_exp_def_upper <= cohort_unexp_def_upper), unique(field_citation_value)]
CI_off <- working_data[!(((upper - effect_size )) >= 0), unique(field_citation_value)]
negative_effect_sizes <- working_data[effect_size < 0, unique(field_citation_value)]
too_large_effect_sizes <-working_data[effect_size > 20, unique(field_citation_value)]
non_RR_OR <- working_data[!tolower(effect_size_measure) %like% "risk|odds|ratio", unique(field_citation_value)]
exp_ref_same <- working_data[cohort_exp_def_mid == cohort_unexp_def_mid, unique(field_citation_value)]
reversed_signs <- working_data[((cohort_exp_def_mid > cohort_unexp_def_mid & lower > 1)|
                                  (cohort_exp_def_mid < cohort_unexp_def_mid & upper < 1)), unique(field_citation_value)]
prob_list <- c("missing_exp_ranges", "overlapping_exposure_ref", "CI_off", 
               "negative_effect_sizes", "too_large_effect_sizes", "non_RR_OR", 
               "exp_ref_same", "reversed_signs")
problems <- data.table(field_citation_value = unique(working_data$field_citation_value))
problems <- problems[!duplicated(field_citation_value)]

for(c.prob in prob_list){
  print(c.prob)
  c.prob_eval <- get(c.prob)
  problems[field_citation_value %in% c.prob_eval, paste0(c.prob) := 1]
  problems[is.na(get(c.prob)),paste0(c.prob) := 0]
}

write.csv(problems, paste0(root, "/outputs/problem_extraction.csv"))

#################################################################################
## Save cleaned and usable dataset ##
#################################################################################

working_data <- working_data[complete.cases(working_data[,.(effect_size, upper, lower, cohort_exp_def_lower, cohort_unexp_def_lower, cohort_exp_def_upper, cohort_unexp_def_upper, cohort_sample_size_total)])]
working_data <- working_data[effect_size > 0]
working_data <- working_data[(upper - effect_size)/1.96 >= 0]



###
working_data[, log_effect := log(effect_size)]
working_data[, obs_slope := log_effect/(cohort_exp_def_mid - cohort_unexp_def_mid)]
#working_data <- working_data[obs_slope < 0]

###
working_data[design %like% "cohort|section", cross_sectional := 1]
working_data[is.na(cross_sectional), cross_sectional := 0]

###
working_data[confounders_perinatal_care_received == 1 | confounders_assisted_delivery == 1, confounders_perinatal_care_received := 1]
###

working_data[inrange(as.numeric(age_start),0,.05), age_start_round := 0]
working_data[inrange(as.numeric(age_start),.06,.75), age_start_round := .05]
working_data[inrange(as.numeric(age_start),.76,4.5), age_start_round := 1]
working_data[inrange(as.numeric(age_start),4.5,9.5), age_start_round := 5]
working_data[inrange(as.numeric(age_start),9.5,15), age_start_round := 15]

working_data[inrange(as.numeric(age_end),0,.75), age_end_round := .75]
working_data[inrange(as.numeric(age_end),.75,1.1), age_end_round := 1]
working_data[inrange(as.numeric(age_end),1,5.5), age_end_round := 5]
working_data[inrange(as.numeric(age_end),5.5,15), age_end_round := 15]
working_data[, age_interval_std := paste0(age_start_round, " to ", age_end_round)]

working_data[is.na(confounders_age), confounders_age := 0]
working_data[is.na(confounders_sex), confounders_sex := 0]
working_data[is.na(confounders_wealth), confounders_wealth := 0]

age_xwalk <- fread("<<<<< filepath redacted >>>>>xwalk_ages_rrs.csv")
setnames(age_xwalk, c("int", "variable"), c("age_interval_std", "Mother_Father_education"))
age_xwalk[Mother_Father_education == "maternal", Mother_Father_education := "2"]
age_xwalk[Mother_Father_education == "paternal", Mother_Father_education := "1"]
working_data <- merge(working_data, age_xwalk, by = c("age_interval_std", "Mother_Father_education"), all.x = T)
working_data[, age_start := as.numeric(age_start)]
working_data[, age_end := as.numeric(age_end)]

temp3 <- copy(working_data)
temp3[,age_interval_std := NULL]
temp3 <- temp3[!tolower(field_citation_value) %like% "dhs|demographic"]
temp3[age_start < .022 & age_end <= .09, age_interval_std := "0 to 0.1, real"]
temp3[age_start < .022 & age_end <= 1.1 & age_end >.09, age_interval_std := "0 to 1, real"]
temp3[age_start < .022 & age_end >1.1 & age_end <= 5, age_interval_std := "0 to 5, real"]
temp3[age_start > .076 & age_end <= 1.1, age_interval_std := "0.1 to 1, real"]
temp3[age_start > .076 & age_end > 1.1 & age_end <= 5, age_interval_std := "0.1 to 5, real"]
temp3[age_start > .9 & age_start < 1.1 & age_end > 4.5 & age_end <= 5.1, age_interval_std := "1 to 5, real"]

working_data <- copy(temp3)

working_data[effect_size > 0 & lower < 0, lower := .01]
working_data[, log_effect := log(effect_size)]
working_data[, obs_slope := log_effect/(cohort_exp_def_mid - cohort_unexp_def_mid)]
working_data[, exp_int := (cohort_exp_def_mid + cohort_unexp_def_mid)/2]
working_data[, obs_std := (log(upper) - log(effect_size))/1.96]

working_data  %>%
  filter(cohort_unexp_def_lower < cohort_unexp_def_upper) %>%
  filter(cohort_exp_def_lower < cohort_exp_def_upper) %>% 
  data.table() -> working_data

working_data <- working_data[!cohort_exposed_var_arch %like% "NA|and" & !cohort_unexp_var_arch %like% "NA|and"]


dir.create(paste0(root, "/outputs"))
write.csv(working_data, paste0(root, "/outputs/cleaned_dataset.csv"))
write.csv(working_data, "<<<<< filepath redacted >>>>>update_2019/code/child_mort_dir/outputs/cleaned_dataset.csv")
write.csv(working_data, "<<<<< filepath redacted >>>>>cleaned_dataset.csv")


#################################################################################
## Generate table of data availability by age range ##
#################################################################################

working_data[!is.na(age_interval_std) & age_interval_std %like% "real"] %>% .[,.(mapvar = .N), by = .(location_name,Mother_Father_education, age_interval_std, field_citation_value_substr)] %>% 
  .[,ihme_loc_id := tstrsplit(location_name, "|", keep = 2, fixed = T)] -> temp

table1 <- temp[, .(`Total Observations` = sum(mapvar),
`Total Unique Countries` = length(unique(location_name)),
`Total Unique Studies` = length(unique(field_citation_value_substr))), by = .(age_interval_std, Mother_Father_education)]

table1 <- table1[Mother_Father_education > 0]

table1[age_interval_std == "0 to 0.1, real", `Age Interval` := "0 to 1 Month"]
table1[age_interval_std == "0 to 1, real", `Age Interval` := "0 to 1 Year"]
table1[age_interval_std == "0 to 5, real", `Age Interval` := "0 to 5 Years"]
table1[age_interval_std == "0.1 to 1, real", `Age Interval` := "1 to 12 Months"]
table1[age_interval_std == "0.1 to 5, real", `Age Interval` := "1 Month to 5 Years"]
table1[age_interval_std == "1 to 5, real", `Age Interval` := "1 to 5 Years"]

table1[Mother_Father_education == 1, Variable := "Paternal Education"]
table1[Mother_Father_education == 2, Variable := "Maternal Education"]

table1 <- table1[,.SD, .SDcols = c("Age Interval", "Variable","Total Unique Studies", "Total Unique Countries", "Total Observations")]
table1[order(`Age Interval`, Variable)]

table1_totals <- table1[,lapply(.SD, sum), .SDcols = names(table1)[!names(table1) %in% c("Age Interval", "Variable")]]
table1_totals <- cbind(data.table(Variable = length(unique(table1$Variable)),
                                  `Age Interval` = length(unique(table1$`Age Interval`))), table1_totals)
table1 <- rbind(table1, table1_totals, fill = T)

htmlTable::htmlTable(table1,
                     align = paste(c("l", "l", rep("r", ncol(table1)-2)), collapse =""),
                     rnames = F,
                     rgroup = c("", "Totals"), 
                     n.rgroup = c(10,1), 
                     caption = "Summary Characteristics of Systematic Review",
                     total = T)
#################################################################################
## Generate plot of data availability by age range ##
#################################################################################
source("<<<<< filepath redacted >>>>>master/GBD_2016/inset_maps/noSubs/GBD_WITH_INSETS_MAPPING_FUNCTION.R")

working_data[!is.na(age_interval_mdpt) & age_interval_mdpt <= 4] %>% .[,.(mapvar = .N), by = .(location_name, age_interval_std)] %>% 
  .[,ihme_loc_id := tstrsplit(location_name, "|", keep = 2, fixed = T)] -> temp

pdf(paste0(root, "/visuals/map_by_exposure_var.pdf"), onefile = T, width = 10, height = 8)
for(c.age in c("Neonatal", "Under-1", "Under-5")){
  gbd_map(temp[age_interval_std == c.age], limits = c(1,2,3,4,5,10,20,50), title = c.age)
}
dev.off()


#################################################################################
## Generate plot of data availability by age range ##
#################################################################################


working_data[!is.na(age_interval_mdpt)& age_interval_mdpt <=4] %>% ggplot() + geom_histogram(aes(x = as.character(round(age_interval_mdpt,3))), bins = 10,
                                                                      stat = "count") + coord_flip() +
  geom_vline(xintercept = .25) + ggtitle("Distribution of Midpoints of Exposure Ranges of Studies") + xlab("Midpoint")

#################################################################################
## Generate plot of all the raw data ##
#################################################################################

#for all reference variables, create a plot of all raw effect sizes
dir.create(paste0(root, "/visuals"))
pdf(paste0(root, "/visuals/raw_data_post_clean_by_exposure_interval.pdf"), onefile = T, width = 10, height = 8)
working_data[, seq := 1:nrow(working_data)]
for(c.ref_cat in unique(working_data$clean_cohort_unexp_def)){
  for(c.age_range in unique(working_data[clean_cohort_unexp_def == c.ref_cat]$age_interval)){
    print(c(c.ref_cat, c.age_range))
    c.ref_lower <- as.numeric(strsplit(c.ref_cat, " to ")[[1]][1])
    c.ref_upper <- as.numeric(strsplit(c.ref_cat, " to ")[[1]][2])
    c.ref_mid <- (c.ref_lower + c.ref_upper)/2
    gg <- ggplot(working_data[clean_cohort_unexp_def == c.ref_cat & age_interval == c.age_range], group = seq)+
      geom_errorbarh(aes(xmax = cohort_exp_def_upper,
                         xmin = cohort_exp_def_lower,
                         y = effect_size, 
                         x = cohort_exp_def_mid, 
                         color = location_name, alpha = .5), height = 0) +
      geom_errorbar(aes(x = cohort_exp_def_mid, 
                        ymax = upper,
                        ymin = lower,
                        color = location_name,
                        group = seq,
                        linetype = factor(CI_orig, levels = c(1,0)),alpha = .5),
                    width = 0,
                    position = position_dodge(width = .5) ) +
      labs(title = paste0("Reference Category (Unexposed Group):\n", c.ref_cat, " years of Education\n", c.age_range), x = "Exposure Interval", y = "Effect Size") + 
      geom_abline(slope = 0, 
                  intercept = 1,
                  linetype = "dashed") +
      geom_vline(xintercept = c.ref_mid, color = "red", linetype = "dotted", size = 1)+
      theme_bw() + 
      xlim(0,20) 
    print(gg)
  } 
}
dev.off()

#for all citations and age ranges variables, create a plot of all raw effect sizes
dir.create(paste0(root, "/visuals"))
pdf(paste0(root, "/visuals/raw_data_post_clean_by_citation.pdf"), onefile = T, width = 10, height = 8)
working_data[, seq := 1:nrow(working_data)]
working_data[, field_citation_value_substr := field_citation_value]
for(c.citation in unique(working_data$field_citation_value_substr)){
  for(c.age_range in unique(working_data[field_citation_value_substr == c.citation]$age_interval)){
    for(c.ref_cat in  unique(working_data[field_citation_value_substr == c.citation & age_interval == c.age_range]$clean_cohort_unexp_def))
      print(c(c.citation, c.age_range))
    c.ref_cat
    c.ref_lower <- working_data[field_citation_value_substr == c.citation & age_interval == c.age_range & clean_cohort_unexp_def == c.ref_cat][1][,cohort_unexp_def_lower]
    c.ref_upper <- working_data[field_citation_value_substr == c.citation& age_interval == c.age_range& clean_cohort_unexp_def == c.ref_cat][1][,cohort_unexp_def_upper]
    c.ref_mid <- (c.ref_lower + c.ref_upper)/2
    c.full_citation <- unique(working_data[field_citation_value_substr == c.citation, field_citation_value])
    gg <- ggplot(working_data[field_citation_value_substr == c.citation & age_interval == c.age_range& clean_cohort_unexp_def == c.ref_cat], group = seq)+
      geom_errorbarh(aes(xmax = cohort_exp_def_upper,
                         xmin = cohort_exp_def_lower,
                         y = effect_size, 
                         x = cohort_exp_def_mid,
                         color = parent_gender), height = 0) +
      geom_errorbar(aes(x = cohort_exp_def_mid, 
                        ymax = upper,
                        ymin = lower,
                        group = seq,
                        color = parent_gender,
                        linetype = factor(CI_orig, levels = c(1,0))),
                    width = 0,
                    position = position_dodge(width = .5)) +
      labs(title = paste0("Reference Category (Unexposed Group):\n", c.ref_cat, " years of Education\n", c.age_range, "\n", c.full_citation), x = "Exposure Interval", y = "Effect Size") + 
      geom_abline(slope = 0, 
                  intercept = 1,
                  linetype = "dashed") +
      geom_vline(xintercept = c.ref_mid, color = "red", linetype = "dotted", size = 1)+
      theme_bw() + 
      xlim(0,20) + 
      annotate("rect",xmin=c.ref_lower, xmax=c.ref_upper, ymin=-Inf, ymax=Inf ,fill = "red", alpha = .05)+
      scale_color_brewer(palette="Dark2")
    print(gg)
  } 
}
dev.off()


#for all citations and age ranges variables, create a plot of all raw effect sizes

# add input data
working_data[, log_effect := log(effect_size)]
working_data[, obs_slope := log_effect/(cohort_exp_def_mid - cohort_unexp_def_mid)]
working_data[, exp_int := (cohort_exp_def_mid + cohort_unexp_def_mid)/2]
working_data[, obs_std := (log(upper) - log(effect_size))/1.96]

# graph slopes superimposed wtih data
ggplot()+
  geom_point(data = working_data,aes(x = exp_int, y = obs_slope, size = 1/obs_std), alpha = .25, color = "blue") +
  ylim(-.2,.2) + xlim(0,18) + theme_bw() + 
  geom_abline(slope = 0, intercept = 0, linetype = 2) + 
  ggtitle("First Derivative of Relative Risk Curve from Raw Data") + 
  facet_wrap(~super_region_name)

ggplot()+
  geom_point(data = working_data,aes(x = exp_int, y = obs_slope, size = 1/obs_std), alpha = .25, color = "blue") +
  ylim(-.2,.2) + xlim(0,18) + theme_bw() + 
  geom_abline(slope = 0, intercept = 0, linetype = 2) + 
  ggtitle("First Derivative of Relative Risk Curve from Raw Data") 

ggplot()+
  geom_point(data = working_data,aes(x = exp_int, y = obs_slope, size = 1/obs_std), alpha = .25, color = "blue") +
  ylim(-.2,.2) + xlim(0,18) + theme_bw() + 
  geom_smooth(data = working_data,aes(x = exp_int, y = obs_slope), alpha = .25, color = "red",method = "loess")+
  geom_abline(slope = 0, intercept = 0, linetype = 2) + 
  ggtitle("First Derivative of Relative Risk Curve from Raw Data")  + 
  facet_wrap(~parent_gender)

#################################################################################
## crosswalk unstandardized data ##
#################################################################################
# 
# source(paste0(code_dir, "/dhs_crosswalk.R"))
# 
# 
# 
# #apply function to all unstandardized data
# working_data[,xwalk_unit := paste(field_citation_value, age_start, age_end, sep = "_")]
# 
# data_to_xwalk <- working_data[cohort_unexp_def_lower != 0 & cohort_unexp_def_upper != 0]
# 
# xwalked_data_list <- lapply(unique(data_to_xwalk$xwalk_unit), dhs_standardizr, c.data = data_to_xwalk, c.dhs_data= dhs_cbh_subset)
# xwalked_data <- rbindlist(xwalked_data_list)
# xwalked_data[,xwalked_mid_point := (xwalked_exposure_upper + xwalked_exposure_lower)/2]
# 
# #################################################################################
# 
# 
# #for all citations and age ranges variables, create a plot of all raw effect sizes
# xwalked_data[, seq := 1:nrow(xwalked_data)]
# xwalked_data[,effect_size := as.numeric(effect_size)]
# xwalked_data[,upper := as.numeric(upper)]
# xwalked_data[,lower := as.numeric(lower)]
# 
# ##rbind xwalked data and data that didn't need to be
# gold_std_data <- working_data[cohort_unexp_def_lower == 0 & cohort_unexp_def_upper == 0]
# gold_std_data[,xwalked_exposure_upper := cohort_exp_def_upper]
# gold_std_data[,xwalked_exposure_lower := cohort_exp_def_lower]
# gold_std_data[,xwalked_mid_point := (xwalked_exposure_upper + xwalked_exposure_lower)/2]
# gold_std_data[,crosswalked_upper :=  as.numeric(upper)]
# gold_std_data[,crosswalked_lower :=   as.numeric(lower)]
# gold_std_data[,crosswalked_effect :=   as.numeric(effect_size)]
# 
# processed_data <- rbind(gold_std_data,xwalked_data, fill = T)
# ggplot(processed_data) + 
#   geom_errorbar(aes(x = xwalked_mid_point, ymax = crosswalked_upper, ymin = crosswalked_lower)) + 
#   geom_errorbarh(aes(xmin = xwalked_exposure_lower, xmax = xwalked_exposure_upper, y = crosswalked_effect)) + 
#   xlim(0,20) + ylim(0,1)
# 
# 
# dir.create(paste0(root, "/visuals"))
# pdf(paste0(root, "/visuals/xwalked_data_post_clean_by_citation.pdf"), onefile = T, width = 10, height = 8)
# 
# for(c.citation in unique(xwalked_data$field_citation_value)){
#   for(c.age_range in unique(xwalked_data[field_citation_value == c.citation]$age_interval)){
#     print(c(c.citation, c.age_range))
#     c.ref_cat <- xwalked_data[field_citation_value == c.citation & age_interval == c.age_range][1][,clean_cohort_unexp_def]
#     c.ref_lower <- xwalked_data[field_citation_value == c.citation & age_interval == c.age_range][1][,cohort_unexp_def_lower]
#     c.ref_upper <- xwalked_data[field_citation_value == c.citation & age_interval == c.age_range][1][,cohort_unexp_def_upper]
#     c.ref_mid <- (c.ref_lower + c.ref_upper)/2
#     gg <- ggplot(xwalked_data[field_citation_value == c.citation & age_interval == c.age_range], group = seq)+
#       geom_errorbarh(aes(xmax = cohort_exp_def_upper,
#                          xmin = cohort_exp_def_lower,
#                          y = effect_size, 
#                          x = cohort_exp_def_mid), height = 0, color = "blue") +
#       geom_errorbar(aes(x = cohort_exp_def_mid, 
#                         ymax = upper,
#                         ymin = lower,
#                         group = seq),
#                     width = 0,
#                     position = position_dodge(width = .5), color = "blue") +
#       geom_errorbarh(aes(xmax = xwalked_exposure_upper,
#                          xmin = xwalked_exposure_lower,
#                          y = crosswalked_effect, 
#                          x = xwalked_mid_point), height = 0, color = "red") +
#       geom_errorbar(aes(x = xwalked_mid_point, 
#                         ymax = crosswalked_upper,
#                         ymin = crosswalked_lower,
#                         group = seq),
#                     width = 0,
#                     position = position_dodge(width = .5), color = "red") +
#       labs(title = paste0("Reference Category (Unexposed Group):\n", c.ref_cat, " years of Education\n", c.age_range, "\n", c.citation), x = "Exposure Interval", y = "Effect Size") + 
#       geom_abline(slope = 0, 
#                   intercept = 1,
#                   linetype = "dashed") +
#       geom_vline(xintercept = c.ref_mid, color = "red", linetype = "dotted", size = 1)+
#       theme_bw() + 
#       xlim(0,20) + 
#       annotate("rect",xmin=c.ref_lower, xmax=c.ref_upper, ymin=-Inf, ymax=Inf ,fill = "red", alpha = .05)+
#       scale_color_brewer(palette="Dark2")
#     print(gg)
#   } 
# }
# dev.off()
# 
# ##create map
# unique_locs <- unique(working_data[,.(substr(field_citation_value,1,20), location_name)])
# unique_locs[,ihme_loc_id := tstrsplit(location_name, "|", keep = 2, fixed = T)]
# unique_locs[, ihme_loc_id := substr(ihme_loc_id, 1,3)]
# counts_by_ihme_loc_id <- unique_locs[,.N, by = ihme_loc_id]
# setnames(counts_by_ihme_loc_id, "N", "mapvar")
# source("<<<<< filepath redacted >>>>>master/GBD_2016/inset_maps/noSubs/GBD_WITH_INSETS_MAPPING_FUNCTION.R")
# pdf(paste0(root, "/visuals/data_map.pdf"), onefile = T, width = 10, height = 8)
# 
# gbd_map(counts_by_ihme_loc_id[ihme_loc_id %in% locs$ihme_loc_id], limits = c(1,2,3,4,5,10,20))
# dev.off()
