###model fit###
library(data.table)
library(survival)
library(coxme, lib.loc = "<<<<< filepath redacted >>>>>rlibs")
library(magrittr)
library(dplyr)
library(lme4)
library(ggplot2)
library(MASS)
library(Jmisc, lib.loc = "<<<<< filepath redacted >>>>>rlibs")
library(stargazer,  lib.loc = "<<<<< filepath redacted >>>>>rlibs")
library(merTools,  lib.loc = "<<<<< filepath redacted >>>>>rlibs")
library(stringr)
library(boot)
library(rlist)

##parameters
task_id <- as.integer(Sys.getenv("SGE_TASK_ID"))
parameters <- fread("/<<<<< filepath redacted >>>>>/education/under_5_mort/code/array_template.csv")
c.survey_id <- parameters[task_id, survey_id]
c.iso <- substr(c.survey_id, 1,3)
new_var <- tstrsplit(parameters[task_id, new_var], " ") %>% unlist

#load data
data <- fread(paste0("/<<<<< filepath redacted >>>>>education/under_5_mort/data/input_data/", c.survey_id, ".csv"))
data[child_age_at_death_months == 0, child_age_at_death_months := .0333]
#create new var
data[maternal_ed_yrs >= paternal_ed_yrs & maternal_ed_yrs > 0, mother_father_higher_ed := 1]
data[(maternal_ed_yrs < paternal_ed_yrs) | maternal_ed_yrs == 0, mother_father_higher_ed := 0]
data[maternal_ed_yrs >= paternal_ed_yrs, parental_ed := maternal_ed_yrs]
data[paternal_ed_yrs >= maternal_ed_yrs, parental_ed := paternal_ed_yrs]
data[maternal_ed_yrs == parental_ed, other_parent_ed := paternal_ed_yrs]
data[paternal_ed_yrs == parental_ed, other_parent_ed := maternal_ed_yrs]

#compute mother birth cohort
data[, mother_birth_cohort := (floor((year_of_birth - mother_age_at_birth)/10)) * 10]
data[, mother_birth_year := (floor((year_of_birth - mother_age_at_birth)))]

#make average ed per birth cohort year
data[, avg_mat_ed := mean(maternal_ed_yrs, na.rm = T), by = year_of_birth]
data[, avg_pat_ed := mean(paternal_ed_yrs, na.rm = T), by = year_of_birth]

#make std_child year born
data[,std_year_born := child_year_born - 1990]

c.re <- "(1|survey_id/admin_1/mother_birth_cohort)"
if(length(unique(data$survey_id)) == 1){c.re <- gsub("survey_id/", "", c.re)}


betar <- function(c.fit){
  pred <- mvrnorm(1000, fixef(c.fit), vcov(c.fit)) %>%
    data.table %>%
    .[, draw:=0:999]
  return(pred)
}


fittr <- function(c.vars, c.int, d.data){
  c.data <- copy(d.data)
  print(c.int)
  print(c.vars)
  c.int_begin <- as.numeric(tstrsplit(c.int, " to ", keep = 1)[[1]])
  c.int_end <- as.numeric(tstrsplit(c.int, " to ", keep = 2)[[1]])
  if(c.int_begin < c.int_end){
    c.data[child_age_at_death_months > c.int_begin & child_age_at_death_months <= c.int_end, child_died_int := 1]
    c.data[is.na(child_died_int), child_died_int := 0]
    c.data[child_age_at_death_months < c.int_begin, child_died_int := NA]
    c.data[child_age_at_death_months > c.int_begin & child_age_at_death_months <= c.int_end, person_years_int := child_age_at_death_months - c.int_begin]
    c.data[child_alive == 1 & age_month < c.int_end, person_years_int := age_month - c.int_begin]
    c.data[child_alive == 1 & age_month >= c.int_end, person_years_int := c.int_end - c.int_begin]
    c.data[is.na(child_died_int), person_years_int := NA]
    
    c.data_temp <- copy(c.data)
    
    formula <- paste0("Surv(c.data$person_years_int,c.data$child_died_int) ~", paste0(c.vars, collapse = " + "), " + ", c.re)
    print(formula)
    
    model_fit <- coxme(as.formula(formula), data = c.data)
    preds <- betar(model_fit)
    preds[, vars := paste0(c.vars, collapse = ", ")]
    preds[, int := c.int]
    preds[, subset := "none"]
    
    c.data <- c.data_temp[maternal_ed_yrs >= 3]
    
    model_fit2 <- coxme(as.formula(formula), data = c.data)
    preds2 <- betar(model_fit2)
    preds2[, vars := paste0(c.vars, collapse = ", ")]
    preds2[, int := c.int]
    preds2[, subset := "mat greater than 3"]
    
    c.data <-  c.data_temp[wealth_index_dhs_continuous >= median(wealth_index_dhs_continuous)]
    
    model_fit3 <- coxme(as.formula(formula), data =c.data)
    preds3 <- betar(model_fit3)
    preds3[, vars := paste0(c.vars, collapse = ", ")]
    preds3[, int := c.int]
    preds3[, subset := "wealth greater than median"]
    
    c.data <-  c.data_temp[maternal_ed_yrs > paternal_ed_yrs]
    
    model_fit4 <- coxme(as.formula(formula), data =c.data)
    preds4 <- betar(model_fit4)
    preds4[, vars := paste0(c.vars, collapse = ", ")]
    preds4[, int := c.int]
    preds4[, subset := "maternal ed higher"]
    c.data <-  c.data_temp[paternal_ed_yrs > maternal_ed_yrs]
    
    model_fit5 <- coxme(as.formula(formula), data =c.data)
    preds5 <- betar(model_fit5)
    preds5[, vars := paste0(c.vars, collapse = ", ")]
    preds5[, int := c.int]
    preds5[, subset := "paternal ed higher"]
    
    c.data <-  c.data_temp[year_of_birth >= median(c.data_temp$year_of_birth, na.rm = T)]
    
    model_fit6 <- coxme(as.formula(formula), data =c.data)
    preds6 <- betar(model_fit6)
    preds6[, vars := paste0(c.vars, collapse = ", ")]
    preds6[, int := c.int]
    preds6[, subset := "last half year of birth"]
    
    c.data <-  c.data_temp[year_of_birth < median(c.data_temp$year_of_birth, na.rm = T)]
    
    
    model_fit7 <- coxme(as.formula(formula), data =c.data)
    preds7 <- betar(model_fit3)
    preds7[, vars := paste0(c.vars, collapse = ", ")]
    preds7[, int := c.int]
    preds7[, subset := "first half year of birth"]
    preds <- rbindlist(list(preds, preds2, preds3, preds4, preds5, preds6, preds7))
    return(preds)
  }
}

base_vars <- c("maternal_ed_yrs", "paternal_ed_yrs")
fit_list <- lapply(X = list(c(base_vars, new_var)),
                    FUN = fittr, c.int= "0 to 1", d.data = data)
fit_list2 <- lapply(X = list(c(base_vars, new_var)),
                  FUN = fittr, c.int= "1 to 12", d.data = data)
fit_list3 <- lapply(X = list(c(base_vars, new_var)),
       FUN = fittr, c.int= "12 to 60", d.data = data)
fit_list4 <- lapply(X = list(c(base_vars, new_var)),
                     FUN = fittr, c.int= "0 to 12", d.data = data)
fit_list5 <- lapply(X = list(c(base_vars, new_var)),
                     FUN = fittr, c.int= "0 to 60", d.data = data)
fit_list6 <- lapply(X = list(c(base_vars, new_var)),
                     FUN = fittr, c.int= "1 to 60", d.data = data)
# fit_list7 <- lapply(X = list(c("parental_ed","other_parent_ed", new_var)),
#                    FUN = fittr, c.int= "0 to 1", d.data = data)
# fit_list8 <- lapply(X = list(c("parental_ed","other_parent_ed", new_var)),
#                     FUN = fittr, c.int= "1 to 12", d.data = data)
# fit_list9 <- lapply(X = list(c("parental_ed","other_parent_ed", new_var)),
#                     FUN = fittr, c.int= "12 to 60", d.data = data)
# fits <- rbindlist(lapply(list(fit_list, fit_list2, fit_list3, fit_list4, fit_list5, fit_list6, fit_list7, fit_list8, fit_list9), rbindlist, fill = T), fill = T)
fits <- rbindlist(lapply(list(fit_list, fit_list2, fit_list3, fit_list4, fit_list5, fit_list6), rbindlist, fill = T), fill = T)

fits[, ihme_loc_id := c.iso]
fits[,survey_id := c.survey_id]

fwrite(fits, paste0("/<<<<< filepath redacted >>>>>education/under_5_mort/data/output_data/", c.survey_id, "_",task_id, ".csv"))
