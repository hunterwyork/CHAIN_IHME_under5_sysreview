#################################################################################
###  Compare NTNU extractions from 2019 to Hunter's extractions from 2017     ###
#################################################################################
###  Hunter York, hyork@uw.edu
###  10/07/2019
#################################################################################
###  Description:
###
###  
###  
#################################################################################


#################################################################################
## Setup ##
#################################################################################

# install libraries and load them
list.of.packages <- c("data.table", "plyr", "dplyr", "ggplot2", "readxl", "grid")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
lapply(list.of.packages, require, character.only = TRUE)

#
root <- "J:/WORK/01_covariates/02_inputs/education/update_2019/code/child_mort_dir" ## replace with directory on host computer
input_dir <- paste0(root, "/inputs")
code_dir <- paste0(root, "/CHAIN_IHME_under5_sysreview")
output_dir <- paste0(root, "/outputs/")

# read in ntnu extractions
temp <- read_excel("C:/Users/hyork/Desktop/child_mort_dir/inputs/appende_all_20191007.xlsm", sheet = "extraction") 
temp <- data.frame(temp)
temp <- data.table(temp)
#remove descriptions rows (1-2), ensure you're not accidentally deleting data
ntnu_extractions <- temp[-(1:2),]
#subset to only a few columns
# ntnu_extractions <- ntnu_extractions[,.(field_citation_value, location_name, location_id, age_start, age_end, 
#                                         year_start_study, outcome_def, design, effect_size, upper, lower,cohort_exposed_def1,
#                                         cohort_unexp_def1)]

setnames(ntnu_extractions, c("cohort_exposed_def1", "cohort_unexp_def1"),c("cohort_exposed_def", "cohort_unexp_def")  )


# read in hunter's 2017 extractions 
temp <- read_excel("C:/Users/hyork/Desktop/archive desktop 20190902/archive_desktop/archive_1_2019/children_all_cause_lit_review.xlsx", sheet = "cc_education")
temp <- data.frame(temp)
temp <- data.table(temp)
#remove descriptions rows (1-2), ensure you're not accidentally deleting data
hwy_extractions <- temp[-(1:2),]
# hwy_extractions <- hwy_extractions[,.(field_citation_value, location_name, location_id, age_start, age_end, 
#                                       year_start_study, outcome_def, design, effect_size, upper, lower,cohort_exposed_def, cohort_unexp_def )]

#run create bins fxn
create_bins(ntnu_extractions, "cohort_exposed_def", "cohort_unexp_def")
create_bins(hwy_extractions, "cohort_exposed_def", "cohort_unexp_def")

# create abbreviated field_citation_value and location_name var to do exploratory merge on
ntnu_extractions[,merge_var := paste(tolower(substr(field_citation_value,1,20)), year_start_study, location_id, sep = "_")]
hwy_extractions[,merge_var := paste(tolower(substr(field_citation_value,1,20)), year_start_study, location_id, sep = "_")]

#change names of important comparative variables
# for(c.var in c("effect_size", "upper", "lower", "cohort_exposed_def", "cohort_unexp_def")){
#   print(c.var)
#   setnames(ntnu_extractions, c.var, paste0(c.var, "_ntnu"))
#   setnames(hwy_extractions, c.var, paste0(c.var, "_hwy"))
# }

# intersect unique titles 
ntnu_extractions[!is.na(merge_var)]$merge_var[ ntnu_extractions[!is.na(merge_var)]$merge_var %in% hwy_extractions$merge_var] %>% 
  unique() -> matched_field_cit_vals

matched_field_cit_vals <- matched_field_cit_vals[!matched_field_cit_vals %like% "NA"]

merge <- hwy_extractions[merge_var %in% matched_field_cit_vals]
# output
dir.create(paste0(output_dir, "/lit_extract_comparisons/"), recursive = T)
for(c.citation_location_year in merge[,unique(paste(merge_var, location_id, year_start_study, sep = "_"))]){
  temp_hwy <- copy(hwy_extractions[paste(merge_var, location_id, year_start_study, sep = "_") == c.citation_location_year])
  temp_hwy[, extractor := "hwy"]
  temp_ntnu <- copy(ntnu_extractions[paste(merge_var, location_id, year_start_study, sep = "_") == c.citation_location_year])
  temp_ntnu[, extractor := "ntnu"]
  file_name <- temp_ntnu[1,paste0(substr(field_citation_value,1,30), "_", tstrsplit(location_name, "|", keep = 1, fixed = T), "_", year_start_study)]
  temp_bind <- rbind(temp_hwy, temp_ntnu, fill = T)
  temp_bind <- temp_bind[,.SD, .SDcols = c("extractor", names(temp_bind)[!names(temp_bind) == "extractor"])]
  
  
  write.csv(temp_bind, paste0(output_dir, "/lit_extract_comparisons/", file_name,".csv"))
}
