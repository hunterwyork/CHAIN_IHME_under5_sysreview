
all_fits <- paste0("/<<<<< filepath redacted >>>>>education/under_5_mort/data/output_data/") %>% list.files(., full.names = T) %>% 
  lapply(., fread) %>% rbindlist(fill = T)

all_fits[, survey_id := factor(survey_id, levels = sort(unique(all_fits$survey_id)))]

all_means <- all_fits[,(lapply(.SD, mean, na.rm = T)), .SDcols = names(all_fits)[!names(all_fits) %in% c("vars", "int","subset", "ihme_loc_id", "draw", "survey_id")], by= .(vars, int, ihme_loc_id, subset, survey_id)]
all_means[, measure := "mean"]
all_lower <- all_fits[,(lapply(.SD, quantile, .025, na.rm = T)), .SDcols = names(all_fits)[!names(all_fits) %in% c("vars","subset", "int", "ihme_loc_id", "draw", "survey_id")], by= .(vars, int, ihme_loc_id, subset, survey_id)]
all_lower[, measure := "lower"]
all_upper <- all_fits[,(lapply(.SD, quantile, .975, na.rm = T)), .SDcols = names(all_fits)[!names(all_fits) %in% c("vars","subset", "int", "ihme_loc_id", "draw", "survey_id")], by= .(vars, int, ihme_loc_id, subset, survey_id)]
all_upper[, measure := "upper"]
all_summary <- rbind(all_means, all_upper, all_lower)
all_summary <- melt(all_summary, id.vars = c('measure','subset', 'vars', 'int', 'ihme_loc_id', "survey_id"))
all_summary <- dcast(all_summary, ...~measure)
for(c.var in unique(unlist(tstrsplit(all_summary$vars, ", ")))){
  if(!c.var %in% c("maternal_ed_yrs", "paternal_ed_yrs","parental_ed", "other_parent_ed", NA)){
    all_summary[vars %like% c.var, (c.var) := 1]
    all_summary[!vars %like% c.var, (c.var) := 0]
  }
}

all_summary[, vars := gsub("maternal_ed_yrs, paternal_ed_yrs", "base", vars)]
all_summary[, vars := gsub("parental_ed, other_parent_ed", "base", vars)]
all_summary[, vars := gsub("child_sex, mother_age_at_birth_binned, decade_child_born", "all_covariates, ", vars)]

all_summary <- all_summary[!is.na(subset)]

all_summary[, survey_order := rank(as.numeric(str_sub(survey_id, 4, -1))), by = .(ihme_loc_id, vars, subset, int, variable)]
all_summary[,seq_id := 1:nrow(all_summary)]
all_summary[,seq_id := factor(seq_id, levels = all_summary[order(mean), seq_id])]
all_summary[variable %like% "matern", mat_pat := "Mother's Education"]
all_summary[variable %like% "patern", mat_pat := "Father's Education"]
all_summary[, int := paste0(int, " Months")]

#export dhs to run in mrbrt
all_export <- copy(all_summary[subset == "none" & variable %like% "atern"])
all_export[int %like% "0 to 1 M", age_interval_std := "0 to 0.1, real"]
all_export[int %like% "1 to 12", age_interval_std := "0.1 to 1, real"]
all_export[int %like% "0 to 12", age_interval_std := "0 to 1, real"]
all_export[int %like% "0 to 60", age_interval_std := "0 to 5, real"]
all_export[int %like% "1 to 60", age_interval_std := "0.1 to 5, real"]

#read in lit review data
input_data <-fread('<<<<< filepath redacted >>>>>/education/update_2019/code/child_mort_dir/outputs/outliers.csv')
all_export[, names(input_data) [names(input_data)%like% "confound"] := 0]
all_export[, confounders_sex := 1]
all_export[, confounders_wealth :=1]
all_export[, confounders_otherparenteducation := 1]
all_export[mat_pat == "Father's Education", Mother_Father_education := 1]
all_export[mat_pat == "Mother's Education", Mother_Father_education := 2]
all_export[, effect_size := exp(mean)]
all_export[, upper := exp(upper)]
all_export[, lower := exp(lower)]
all_export[, cohort_exp_def_lower := 1]
all_export[, cohort_exp_def_upper := 1.99]
all_export[, cohort_unexp_def_lower := 0]
all_export[, cohort_unexp_def_upper:= 0.99]
all_export[, cohort_sample_size_total := 1000]
all_export[, age_start := 99]
all_export[, age_end := 99]
all_export[, field_citation_value := survey_id]

write.csv(all_export, "<<<<< filepath redacted >>>>>dhs_effect_sizes.csv")
