### Takes predictions from mr-brt and visualizes them ###
# Hunter York 12/8/2019
# hyork@uw.edu
###

library(data.table)
library(ggplot2)
library(stringr)
library(dplyr)
library(stargazer)
library(gtools)

# load data
c.model_version <-  "prod20201012"
data <- fread(paste0("<<<<< filepath redacted >>>>>mrbrt_output/", c.model_version, "/r_mrbrt_preds.csv"))
input_data <- fread(paste0("<<<<< filepath redacted >>>>>mrbrt_output/", c.model_version, "/r_mrbrt_outliers.csv"))
input_data[, V1 := NULL]
input_data <- input_data[,.SD, .SDcols = names(input_data)[!str_sub(names(input_data),-2,-1) == ".y"]]
names(input_data)[str_sub(names(input_data),-2,-1) == ".x"] <- str_sub(names(input_data)[str_sub(names(input_data),-2,-1) == ".x"], 1, -3)


data <- data[age_interval_std %like% "real"]
data <- unique(data[,.SD,.SDcols = names(data)[!names(data) %like% "age_midpoint"]])
input_data <- input_data[age_interval_std %like% "real"]
setnames(data, "Mother_Father_education", "mother_father")



data[, V1 := NULL]
input_data[, V1 := NULL]
input_data[, mother_father := as.character(mother_father)]

data[, mother_father := as.character(mother_father)]
data[mother_father == 1, mother_father := "Paternal Education"]
data[mother_father == 2, mother_father := "Maternal Education"]

# cast long by draw
data_long <- melt(data, measure.vars = patterns("rr"))
setnames(data_long, c("variable", "value"), c("draw", "rr"))
data_long[, draw := as.numeric(str_sub(draw, 3, -1))]
data_long <- unique(data_long)
#take out commas from cols
names(data_long) <- gsub(",", "", names(data_long))

# rename covariates
confounders <- names(data_long)[!names(data_long) %like% "rr|draw|maternal_education|cohort|intercept|dummy|V1|age_midpoint" & ! names(data_long) %in% paste0(names(data_long), "int")]

# standardize by dividing by 0 exposure value for RRs greater than 1
data_long[, rr:= as.numeric(rr)]
exp_0s <- data_long[maternal_education == cohort_unexp_def_lower ,.(exp_0 = exp(rr)), by = c(confounders, "draw", "cohort_unexp_def_lower")]
data_long <- merge(unique(exp_0s), data_long, by =c(confounders, "draw", "cohort_unexp_def_lower") )
data_long[is.infinite(rr), rr:= NA]

data_long[, rr_std := exp(rr)/exp_0]
data_long[, rowsum := rowSums(.SD), .SDcols = confounders[confounders %like% "cohort|case|cross|confounders|dummy" & !confounders %like% "int"]]

# collapse to summaries
data_summary <- data_long[!is.nan(rr_std),.(rr = median(rr_std, na.rm = T),
                                            upper = quantile(rr_std, .975, na.rm = T), 
                                            lower = quantile(rr_std, .025, na.rm = T)), by = c("maternal_education", confounders, "cohort_unexp_def_lower", "rowsum")]

draw_summary_save <- copy(data_summary)
draw_summary_save[, rowsum := rowSums(.SD), .SDcols = confounders[confounders %like% "cohort|case|cross|confounders|dummy"]]
saveRDS(draw_summary_save, "<<<<< filepath redacted >>>>>draw_summary_save_04242020.rds")
# plots


data_long[,rowsum := rowSums(.SD), .SDcols = names(data_long)[names(data_long) %like% "case|cross|confounders" & !(names(data_long) %like% "^confounder" & names(data_long) %like% "int$")]]


betar <- function(c.var, c.data_long){
  betas <-  c.data_long[(get(c.var) ==1 & rowsum ==1), .(mother_father, age_interval_std, draw, rr, rowsum)]
  betas_summary <- betas[,.(beta = mean(rr),
                            upper = quantile(rr, .975), 
                            lower = quantile(rr, .025)), by = .(mother_father, age_interval_std)]
  betas_summary[, var := c.var]
  return(betas_summary)
}


betas_all <- rbindlist(lapply(names(data_long)[names(data_long) %like% "case|cohort|cross|confounders|dummy|maternal"], betar, data_long))
betas_all[(lower > 0 & beta >0)|(upper < 0 & beta < 0), Significance := "Significant"]
betas_all[is.na(Significance), Significance := "Not Significant"]
betas_all[, beta := exp(beta)]
betas_all[, upper := exp(upper)]
betas_all[, lower := exp(lower)]

gg3 <- ggplot(betas_all[var != "maternal_education"]) + 
  geom_point(aes(x = var, y = beta, color = Significance)) + 
  geom_errorbar(aes(x = var, ymin = lower, ymax = upper, color = Significance)) + 
  facet_wrap(~ mother_father + age_interval_std, nrow = 1) + 
  geom_hline(yintercept = 1, linetype = "dotted") +
  scale_y_continuous(breaks = seq(.2, 1.8, .4), limits = c(.2, 1.8))+
  theme_bw() +ylab("exp(beta)")+
  coord_flip()



data_long[, l1_rr := lag(rr_std), by =  c(confounders, "cohort_unexp_def_lower")]
data_long[, l1_exp := lag(maternal_education), by =  c(confounders, "cohort_unexp_def_lower")]

data_long[, slope := (log(rr_std) - log(l1_rr))/(maternal_education - l1_exp), by = c(confounders, "cohort_unexp_def_lower")]

slope_summary <- data_long[cohort_unexp_def_lower == 0,.(slope = median(slope, na.rm = T),
                                                         upper = quantile(slope, .975, na.rm = T), 
                                                         lower = quantile(slope, .025, na.rm = T)), by =  c(confounders, "maternal_education", "rowsum", "cohort_unexp_def_lower")]

# add input data
input_data[, log_effect := log(effect_size)]
input_data[, cohort_exp_def_mid := (cohort_exp_def_lower + cohort_exp_def_upper)/2]
input_data[, cohort_unexp_def_mid := (cohort_unexp_def_lower + cohort_unexp_def_upper)/2]
input_data[, obs_slope := log_effect/(cohort_exp_def_mid - cohort_unexp_def_mid)]
input_data[, exp_int := (cohort_exp_def_mid + cohort_unexp_def_mid)/2]
input_data[, obs_std := (log(upper) - log(effect_size))/1.96]
input_data[(lower > 1 & effect_size > 1) | (upper < 1 & effect_size < 1), significant := "significant"]
input_data[is.na(significant), significant := "not significant"]
input_data <- input_data[!is.na(age_start) & !is.na(age_end)]
input_data[,mother_father := as.character(mother_father)]
input_data[mother_father == "2", mother_father := "Maternal Education"]
input_data[mother_father == "1", mother_father := "Paternal Education"]

input_data[, inlier := as.character(inlier)]
input_data[inlier == 1, inlier := "Inlier"]
input_data[inlier == 0, inlier := "Outlier"]
input_data[, V1 := NULL]

slope_summary %>% na.replace(x= .,replace =  0) -> slope_summary
slope_summary[, max_rowsum := max(rowsum), by = .(age_interval_std, mother_father)]

graph_dat <- merge(input_data[, .SD, .SDcols = names(input_data)[!duplicated(names(input_data))]], slope_summary[((rowsum == 4 & confounders_age == 0)|(rowsum == 0)) & maternal_education != 0 & cohort_unexp_def_lower == 0], all = T)
graph_dat <- graph_dat[!age_interval_std %like% "NA" & !is.na(mother_father)]

graph_resid <- copy(graph_dat)
graph_resid[is.infinite(slope), slope := NA]
graph_resid[,fit_slope:=median(slope, na.rm = T), by = c(confounders[confounders %like% "mother|age_in" & !confounders %like% "haq"])]
graph_resid[, resid := obs_slope - fit_slope]

pdf(paste0('<<<<< filepath redacted >>>>>update_2019/code/child_mort_dir/visuals/preds_rr_and_slopes.pdf'), width = 12, height = 8)
# graph slopes superimposed wtih data
gg <- ggplot(graph_dat[!tolower(age_interval_std) %like% "std|stand" & (nchar(field_citation_value) != 7|is.na(field_citation_value))])+
  geom_jitter(aes(x = exp_int, y = obs_slope, size = 1/obs_std,color = as.factor(confounders_wealth), shape = significant, alpha = .25)) +
  geom_line(aes(x = maternal_education-.5, y = slope), color = "black") +
  geom_ribbon(aes(x = maternal_education - .5, ymax = upper, ymin = lower), alpha= .25, fill = "gray30") + 
  theme_bw() + facet_grid(~age_interval_std ~ mother_father) + geom_abline(intercept = 0, slope = 0, linetype = "dotted") + 
  ggtitle("First Derivative of RR Curve Plotted Over\nNormalized Relative Risk Derivatives") + xlab("Exposure Interval Midpoint") + ylab("Slope of log(RRs)") + 
  labs(size = "1/Standard Deviation of RR") + scale_shape_manual(values = c(13, 16),na.translate = FALSE) + scale_color_discrete(na.translate = FALSE) +
  guides(alpha = FALSE) +
  coord_cartesian(ylim=c(-.2,.2), xlim=c(0,18))
print(gg)
gg <- ggplot(graph_resid[!tolower(age_interval_std) %like% "std|stand" & is_dhs != "DHS"])+
  geom_jitter(aes(x = exp_int, y = resid, size = 1/obs_std,color = inlier, shape = significant, alpha = .25)) +
  theme_bw() + facet_grid(~age_interval_std ~ mother_father) + geom_abline(intercept = 0, slope = 0, linetype = "dotted") + 
  ggtitle("First Derivative of RR Curve Plotted Over\nNormalized Relative Risk Derivatives Residuals") + xlab("Exposure Interval Midpoint") + ylab("Residuals of Slope of log(RRs) from Fitted Value") + 
  labs(size = "1/Standard Deviation of RR") + scale_shape_manual(values = c(13, 16),na.translate = FALSE) + scale_color_discrete(na.translate = FALSE) +
  guides(alpha = FALSE) +
  coord_cartesian(ylim=c(-.2,.2), xlim=c(0,18))
print(gg)
print(gg3)
x <- lapply(rev(confounders), graphr, c.data_summary = data_summary)

data_summary[cohort_unexp_def_lower == 0 & rowsum==4 & confounders_age == 0]%>% 
  .[age_interval_std == "0 to 0.1, real", int := "0 to 1 Months"] %>% 
  .[age_interval_std == "0.1 to 1, real", int := "1 to 12 Months"] %>% 
  .[age_interval_std == "1 to 5, real", int := "12 to 60 Months"] %>% 
  .[age_interval_std == "0 to 5, real", int := "0 to 60 Months"] %>% 
  .[maternal_education >= 0 & maternal_education <= 6., grouping := "Primary"] %>% 
  .[maternal_education > 6 & maternal_education <= 12, grouping := "Secondary"] %>% 
  .[maternal_education > 12 & maternal_education <= 18, grouping := "Tertiary"] %>% 
  .[, int := factor(int, levels = c("0 to 1 Months", "1 to 12 Months", "12 to 60 Months", "0 to 60 Months", "0 to 12 Months", "1 to 60 Months"))] %>% 
  .[!is.na(int) & !int %like% "0 to 60" & maternal_education <= 18 & mother_father == "Maternal Education" & round(maternal_education) == maternal_education] %>% 
  ggplot() + 
  # geom_line(aes(x = maternal_education, y = rr, fill = int)) +
  # geom_ribbon(aes(x = maternal_education, ymin = lower, ymax = upper, fill = int, color = int), alpha = .2) +
  # theme_bw() + 
  geom_point(aes(x = maternal_education, y = rr, color = int), position = position_dodge(width = .8), size = 2) +
  geom_errorbar(aes(x = maternal_education, ymin = lower, ymax = upper, color = int), position = position_dodge(width = .8)) +
  ylim(.35, 1.1)+
  labs(fill="Age Inteval") + 
  geom_hline(yintercept = 1, linetype = "solid")+
  ggtitle("Maternal Education, Disaggregated Ages") + 
  #facet_wrap( ~ mother_father) + ylim(0,1.05) +
  xlab("Education (In Years Completed)\n") +
  scale_fill_viridis_d(option = "D", begin = 0, end = .5)+
  scale_color_viridis_d(option = "D", begin = 0, end = .5)+
  scale_x_continuous(breaks = seq(0,18))+
  scale_y_continuous(breaks = c(.4,.6,.8,1,1.1), limits = c(.4, 1.1))+
  theme(legend.position = "bottom")  +
  facet_grid(mother_father~ grouping , space = 'free_x', scales = 'free_x', switch = 'x') +
  theme_bw(base_size = 16)+
  ylab("Relative Risk") +
  theme(panel.grid.minor.x = element_blank()) + 
  # remove facet spacing on x-direction
  theme(panel.spacing.x = unit(0,"line")) +
  # switch the facet strip label to outside 
  # remove background color
  theme(strip.placement = 'outside',
        strip.background.x = element_blank(),
        legend.position = "none",
        strip.text.y = element_blank()) +
  theme(panel.spacing.x = unit(.5,"line"))-> gg4_mat
data_summary[cohort_unexp_def_lower == 0 & rowsum==4 & confounders_age == 0]%>% 
  .[age_interval_std == "0 to 0.1, real", int := "0 to 1 Months"] %>% 
  .[age_interval_std == "0.1 to 1, real", int := "1 to 12 Months"] %>% 
  .[age_interval_std == "1 to 5, real", int := "12 to 60 Months"] %>% 
  .[age_interval_std == "0 to 5, real", int := "0 to 60 Months"] %>% 
  .[maternal_education >= 0 & maternal_education <= 6, grouping := "Primary"] %>% 
  .[maternal_education > 6 & maternal_education <= 12, grouping := "Secondary"] %>% 
  .[maternal_education > 12 & maternal_education <= 18, grouping := "Tertiary"] %>% 
  .[, int := factor(int, levels = c("0 to 1 Months", "1 to 12 Months", "12 to 60 Months", "0 to 60 Months", "0 to 12 Months", "1 to 60 Months"))] %>% 
  .[!is.na(int) & !int %like% "0 to 60" & maternal_education <= 18 & mother_father == "Paternal Education" & round(maternal_education) == maternal_education] %>% 
  ggplot() + 
  ylim(.35, 1.1) +
  #geom_line(aes(x = maternal_education, y = rr, color = int)) +
  # geom_ribbon(aes(x = maternal_education, ymin = lower, ymax = upper, fill = int, color = int), alpha = .2) +
  # theme_bw() + 
  geom_point(aes(x = maternal_education, y = rr, color = int), position = position_dodge(width = .8), size = 2) +
  geom_errorbar(aes(x = maternal_education, ymin = lower, ymax = upper, color = int), position = position_dodge(width = .8)) +
  labs(fill="Age Inteval") + 
  geom_hline(yintercept = 1, linetype = "solid")+
  ggtitle("Paternal Education, Disaggregated Ages") + 
  #facet_wrap( ~ mother_father) + ylim(0,1.05) +
  xlab("Education (In Years Completed)\n") +
  scale_fill_viridis_d(option = "D", begin = 0, end = .5)+
  scale_color_viridis_d(option = "D", begin = 0, end = .5)+
  scale_x_continuous(breaks = seq(0,18))+  
  scale_y_continuous(breaks = c(.4,.6,.8,1,1.1), limits = c(.4, 1.1))+
  theme(legend.position = "bottom")  +
  facet_grid(mother_father~ grouping ,shrink = T, space = 'free_x', scales = 'free_x', switch = 'x') +
  theme_bw(base_size = 16)+
  ylab("Relative Risk") +
  theme(panel.grid.minor.x = element_blank()) + 
  # remove facet spacing on x-direction
  theme(panel.spacing.x = unit(.5,"line")) +
  # switch the facet strip label to outside 
  # remove background color
  theme(strip.placement = 'outside',
        strip.background.x = element_blank(),
        legend.position = "none",
        strip.text.y = element_blank()
  )-> gg4_pat

data_summary[cohort_unexp_def_lower == 0 & rowsum==4 & confounders_age == 0]%>% 
  .[age_interval_std == "0 to 0.1, real", int := "0 to 1 Months"] %>% 
  .[age_interval_std == "0.1 to 1, real", int := "1 to 12 Months"] %>% 
  .[age_interval_std == "1 to 5, real", int := "12 to 60 Months"] %>% 
  .[age_interval_std == "0 to 5, real", int := "0 to 60 Months"] %>% 
  .[maternal_education >= 0 & maternal_education <= 6, grouping := "Primary"] %>% 
  .[maternal_education > 6 & maternal_education <= 12, grouping := "Secondary"] %>% 
  .[maternal_education > 12 & maternal_education <= 18, grouping := "Tertiary"] %>% 
  .[, int := factor(int, levels = c("0 to 1 Months", "1 to 12 Months", "12 to 60 Months", "0 to 60 Months", "0 to 12 Months", "1 to 60 Months"))] %>% 
  .[!is.na(int) & int %like% "0 to 60" & maternal_education <= 18 & mother_father == "Maternal Education" & round(maternal_education) == maternal_education] %>% 
  ggplot() + 
  ylim(.35, 1.1) +
  geom_rect(data = data.table(maternal_education = 0:18), aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = "red", alpha = .05/18)+
  geom_line(aes(x = maternal_education, y = rr, color = int)) +
  geom_ribbon(aes(x = maternal_education, ymin = lower, ymax = upper, fill = int, color = int), alpha = .2) +
  # theme_bw() + 
  # geom_point(aes(x = maternal_education, y = rr, color = int), position = position_dodge(width = .6), size = 2) +
  # geom_errorbar(aes(x = maternal_education, ymin = lower, ymax = upper, color = int), position = position_dodge(width = .8)) +
  labs(fill="Age Inteval") + 
  geom_hline(yintercept = 1, linetype = "solid")+
  ggtitle("Maternal Education, Aggregated Ages") + 
  #facet_wrap( ~ mother_father) + ylim(0,1.05) +
  xlab("Education (In Years Completed)\n") +
  scale_fill_viridis_d(option = "D", begin = .75, end = .75)+
  scale_color_viridis_d(option = "D", begin = .75, end = .75)+
  scale_x_continuous(breaks = seq(0,18))+  
  scale_y_continuous(breaks = c(.4,.6,.8,1,1.1), limits = c(.4, 1.1))+
  theme(legend.position = "bottom")  +
  facet_grid(mother_father~ grouping ,shrink = T, space = 'free_x', scales = 'free_x', switch = 'x') +
  theme_bw(base_size = 16)+
  ylab("Relative Risk") +
  theme(panel.grid.minor.x = element_blank()) + 
  # remove facet spacing on x-direction
  theme(panel.spacing.x = unit(.5,"line")) +
  # switch the facet strip label to outside 
  # remove background color
  theme(strip.placement = 'outside',
        strip.background.x = element_blank(),
        legend.position = "none",
        strip.text.y = element_blank()
  )-> gg4_mat2


data_summary[cohort_unexp_def_lower == 0 & rowsum==4 & confounders_age == 0]%>% 
  .[age_interval_std == "0 to 5, real", cum_int := "yes"] %>% 
  .[age_interval_std != "0 to 5, real" |is.na(age_interval_std), cum_int := "no"] %>% 
  .[age_interval_std == "0 to 0.1, real", int := "0 to 1 Months"] %>% 
  .[age_interval_std == "0.1 to 1, real", int := "1 to 12 Months"] %>% 
  .[age_interval_std == "1 to 5, real", int := "12 to 60 Months"] %>% 
  .[age_interval_std == "0 to 5, real", int := "0 to 60 Months"] %>% 
  .[maternal_education >= 0 & maternal_education <= 6, grouping := "Primary"] %>% 
  .[maternal_education > 6 & maternal_education <= 12, grouping := "Secondary"] %>% 
  .[maternal_education > 12 & maternal_education <= 18, grouping := "Tertiary"] %>% 
  .[, int := factor(int, levels = c("0 to 1 Months", "1 to 12 Months", "12 to 60 Months", "0 to 60 Months", "0 to 12 Months", "1 to 60 Months"))] %>% 
  .[!is.na(int) & int %like% "0 to 60" & maternal_education <= 18 & mother_father == "Paternal Education"& round(maternal_education) == maternal_education] %>% 
  ggplot() + 
  ylim(.35, 1.1) +
  geom_rect(data = data.table(maternal_education = 0:18), aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = "red", alpha = .05/18)+
  geom_line(aes(x = maternal_education, y = rr, color = int)) +
  geom_ribbon(aes(x = maternal_education, ymin = lower, ymax = upper, fill = int, color = int), alpha = .2) +
  # theme_bw() + 
  # geom_point(aes(x = maternal_education, y = rr, color = int), position = position_dodge(width = .6), size = 2) +
  # geom_errorbar(aes(x = maternal_education, ymin = lower, ymax = upper, color = int), position = position_dodge(width = .8)) +
  labs(fill="Age Inteval") + 
  geom_hline(yintercept = 1, linetype = "solid")+
  ggtitle("Paternal Education, Aggregated Ages") + 
  #facet_wrap( ~ mother_father) + ylim(0,1.05) +
  xlab("Education (In Years Completed)\n") +
  scale_fill_viridis_d(option = "D", begin = .75, end = .75)+
  scale_color_viridis_d(option = "D", begin = .75, end = .75)+
  scale_x_continuous(breaks = seq(0,18))+  
  scale_y_continuous(breaks = c(.4,.6,.8,1,1.1), limits = c(.4, 1.1))+
  theme(legend.position = "bottom")  +
  facet_grid(mother_father~ grouping ,shrink = T, space = 'free_x', scales = 'free_x', switch = 'x') +
  theme_bw(base_size = 16)+
  ylab("Relative Risk") +
  theme(panel.grid.minor.x = element_blank()) + 
  # remove facet spacing on x-direction
  theme(panel.spacing.x = unit(.5,"line")) +
  # switch the facet strip label to outside 
  # remove background color
  theme(strip.placement = 'outside',
        strip.background.x = element_blank(),
        legend.position = "none",
        strip.text.y = element_blank()
  )-> gg4_pat2

data_summary[cohort_unexp_def_lower == 0] %>%  
  .[age_interval_std == "0 to 0.1, real", int := "0 to 1 Months"] %>% 
  .[age_interval_std == "0.1 to 1, real", int := "1 to 12 Months"] %>% 
  .[age_interval_std == "1 to 5, real", int := "12 to 60 Months"] %>% 
  .[age_interval_std == "0 to 5, real", int := "0 to 60 Months"] %>% 
  .[, int := factor(int, levels = c("0 to 1 Months", "1 to 12 Months", "12 to 60 Months", "0 to 60 Months", "0 to 12 Months", "1 to 60 Months"))] %>% 
  .[!is.na(int)] %>% 
  
  ggplot() + 
  geom_point(aes(x = maternal_education, y = rr, color = int)) + labs(color = "Child Age Range") +
  scale_color_viridis_d(option = "D", begin = 0, end = .75)+
  guides(color = guide_legend(override.aes = list(size=5))) + 
  theme_bw(base_size = 18)+ theme(legend.position = "bottom")-> gglegend

gglegendr <- function(x){
  tmp <- ggplot_gtable(ggplot_build(x))
  leg <- which(sapply(tmp$grobs, function(y) y$name) == "guide-box")
  tmp$grobs[[leg]]
}

gglegendr(gglegend) -> leg

grid.arrange(gg4_mat, gg4_pat,gg4_mat2, gg4_pat2,leg, layout_matrix = rbind(c(1,1,1,2,2,2), 
                                                                            c(1,1,1,2,2,2),
                                                                            c(1,1,1,2,2,2),
                                                                            c(1,1,1,2,2,2),
                                                                            c(3,3,3,4,4,4),
                                                                            c(3,3,3,4,4,4),
                                                                            c(3,3,3,4,4,4),
                                                                            c(3,3,3,4,4,4),
                                                                            c(5,5,5,5,5,5)), top =  grid::textGrob("Figure 2a: Relative Risk of Under-5 Mortality, by Age Group and Parents' Education\n", x = 0.01, hjust = 0,gp = gpar(fontsize = 20, font = 1)))


dev.off()


pdf('<<<<< filepath redacted >>>>>update_2019/code/child_mort_dir/visuals/figure_curves.pdf', width = 12, height = 8)
library(gpar)
pdf('<<<<< filepath redacted >>>>>/Desktop/figure_curves.pdf', width = 12, height = 10)

grid.arrange(gg4_mat, gg4_pat,gg4_mat2, gg4_pat2,leg, layout_matrix = rbind(c(1,1,1,2,2,2), 
                                                                            c(1,1,1,2,2,2),
                                                                            c(1,1,1,2,2,2),
                                                                            c(1,1,1,2,2,2),
                                                                            c(3,3,3,4,4,4),
                                                                            c(3,3,3,4,4,4),
                                                                            c(3,3,3,4,4,4),
                                                                            c(3,3,3,4,4,4),
                                                                            c(5,5,5,5,5,5)), top =  grid::textGrob("Figure 3a: Relative Risk of Under-5 Mortality, by Age Group and Parents' Education\n", x = 0.01, hjust = 0,gp = gpar(fontsize = 20, font = 1)))

dev.off()

###figure 2

library(RColorBrewer)

#set up controls variable
# graph_dat[confounders_otherparenteducation ==1, control := "Partner's Ed."]
# graph_dat[confounders_otherparenteducation ==1 & (confounders_wealth ==1 |confounders_wealth_index ==1 | confounders_income == 1), control := "Partner's Ed. & Wealth/Income"]
graph_dat[rowsum == 4 & confounders_age == 0, control := "Optimally Adjusted"]
graph_dat[rowsum == 0 & confounders_age == 0, control := "Unadjusted"]

#graph_dat[is.na(control), control := "Neither"]
graph_dat[, control := factor(control)]
graph_dat[, control := factor(control, levels = rev(levels(graph_dat$control)))]
#graph_dat[rowsum == max_rowsum, control := "Partner's Ed. & Wealth/Income"]
#graph_dat[rowsum == 0, control := "Neither"]
#graph_dat[rowsum == 1 & confounders_otherparenteducation==1, control := "Partner's Ed."]

hline_df <- data.frame(name = c('a', 'b'), y = c(1, 2))

graph_dat[,.(age_interval_std, mother_father)] %>%
  unique() %>% .[, dum_per_facet := 1] %>% 
  .[age_interval_std == "0 to 5, real", cum_int := "yes"] %>% 
  .[age_interval_std != "0 to 5, real" |is.na(age_interval_std), cum_int := "no"] %>% 
  rbind(graph_dat, ., fill = T) %>% 
  .[!tolower(age_interval_std) %like% "std|stand" & (nchar(field_citation_value) != 7|is.na(field_citation_value))] %>% 
  .[age_interval_std == "0 to 0.1, real", int := "0 to 1 Months"] %>% 
  .[age_interval_std == "0.1 to 1, real", int := "1 to 12 Months"] %>% 
  .[age_interval_std == "1 to 5, real", int := "12 to 60 Months"] %>% 
  .[age_interval_std == "0 to 5, real", int := "0 to 60 Months"] %>% 
  .[, int := factor(int, levels = c("0 to 1 Months", "1 to 12 Months", "12 to 60 Months", "0 to 60 Months", "0 to 12 Months", "1 to 60 Months"))] %>% 
  
  
  .[!is.na(int)] %>% 
  ggplot()+
  geom_rect(data  = function(x) subset(x,dum_per_facet == 1), aes(fill = cum_int),xmin = -Inf,xmax = Inf,
            ymin = -Inf,ymax = Inf, alpha = .05, show.legend = F)+
  facet_grid(~int ~ mother_father)   +
  geom_jitter(aes(x = exp_int, y = obs_slope, size = 1/obs_std,shape = inlier, alpha = .25),color = "gray50", width = .5) +
  
  geom_ribbon(aes(x = maternal_education - .5, ymax = upper, ymin = lower, fill = control), alpha= .5, show.legend =T) + 
  geom_line(aes(x = maternal_education-.5, y = slope, color = control), show.legend = T, alpha = 1) +
  theme_bw() +
  geom_hline(data=hline_df,aes(yintercept=y,linetype=name), color='red',show_guide=F) + 
  
  geom_abline(intercept = 0, slope = 0) + 
  ggtitle("Figure 4: Systematic Review Data:\nEstimates of Log(RR) Per Additional Year of Parental Education\nPlotted Over Normalized Input Data") +
  xlab("Years of Maternal or Paternal Education") + 
  ylab("Log(RR) Per Additional Year of Parental Education") + 
  labs(size = expression(frac("1","Standard Error of Log(RR)")), fill = "Study-Level\nControls",color = "Study-Level\nControls", shape = "") + 
  scale_shape_manual(values = c(16, 13),na.translate = FALSE) +
  scale_color_manual(values = brewer.pal(2, "Set2")[c(1,2)], na.translate = FALSE) + 
  scale_fill_manual(values = c(NA,brewer.pal(2, "Set2")[c(2)], brewer.pal(2, "Set2")[c(1)], "red"), na.translate = FALSE, breaks = c("Neither", "Other Parent's Ed.", "Other Parent's Ed. & Wealth/Income")) + 
  scale_radius()+
  guides(color = guide_legend(override.aes = list(size=4, linetype = 1, fill = NA)),
         shape = guide_legend(override.aes = list(size = 4,
                                                  linetype = 0, fill = NA)),
         fill = guide_legend(override.aes = list(size = 4, linetype = 0)),
         size = guide_legend(override.aes = list(size = 4, linetype = 0, fill = NA))) + 
  guides(alpha = FALSE, linetype = FALSE) + theme(legend.position = "bottom", legend.box = "vertical")+
  coord_cartesian(ylim=c(-.2,.1), xlim=c(0,18)) ->gg
#scale_fill_manual(values = c("white", "red"))-> gg
print(gg)


# graph_dat[!tolower(age_interval_std) %like% "std|stand" & (nchar(field_citation_value) != 7|is.na(field_citation_value))] %>% 
#   .[age_interval_std == "0 to 0.1, real", int := "0 to 1 Months"] %>% 
#   .[age_interval_std == "0.1 to 1, real", int := "1 to 12 Months"] %>% 
#   .[age_interval_std == "1 to 5, real", int := "12 to 60 Months"] %>% 
#   .[age_interval_std == "0 to 5, real", int := "0 to 60 Months"] %>% 
#   .[, int := factor(int, levels = c("0 to 1 Months", "1 to 12 Months", "12 to 60 Months", "0 to 60 Months", "0 to 12 Months", "1 to 60 Months"))] %>% 
#   
#   
#   .[!is.na(int)] %>% 
#   ggplot()+
#   geom_jitter(aes(x = exp_int, y = exp(obs_slope)^exp_int, size = 1/obs_std,color = inlier, alpha = .25)) +
#   geom_line(aes(x = maternal_education-.5, y = exp(slope)^(maternal_education-.5)), color = "black") +
#   geom_ribbon(aes(x = maternal_education - .5, ymax = exp(upper)^(maternal_education-.5), ymin = exp(lower)^(maternal_education-.5)), alpha= .25, fill = "gray30") + 
#   theme_bw() + facet_grid(~int ~ mother_father) + geom_abline(intercept = 1, slope = 0, linetype = "dotted") + 
#   ggtitle("Systematic Review Data:\nFirst Derivative of RR Curve Plotted Over\nNormalized Relative Risk Derivatives") + xlab("Midpoint of Exposure and Referent Intervals") + 
#   ylab("Normalize log(RR)/Year of Education") + 
#   labs(size = "1/Standard Deviation of RR", color = "Inlier") + 
#   scale_shape_manual(values = c(13, 16),na.translate = FALSE) +
#   scale_color_manual(values = brewer.pal(3, "Set2")[1:2], na.translate = FALSE) + 
#   guides(alpha = FALSE) + theme(legend.position = "bottom")+
#   guides(color = guide_legend(override.aes = list(size=4)), shape = guide_legend(override.aes = list(size = 4))) + 
#   coord_cartesian(ylim=c(2,0), xlim=c(0,18)) -> gg
# print(gg)

pdf('<<<<< filepath redacted >>>>>update_2019/code/child_mort_dir/visuals/figure_2.pdf', width = 6, height = 10)
print(gg)
dev.off()
jpeg('<<<<< filepath redacted >>>>>update_2019/code/child_mort_dir/visuals/figure_2.jpeg', width = 6, height = 10, units = "in", res = 250)
print(gg)
dev.off()
pdf('<<<<< filepath redacted >>>>>update_2019/code/child_mort_dir/visuals/figure_2.pdf', width = 6, height = 10)
print(gg)
dev.off()





####################
## Betas Table #####
####################
betas <- fread(paste0("<<<<< filepath redacted >>>>>mrbrt_output/", c.model_version, "/r_mrbrt_samples.csv"))
betas[, V1 := NULL]
gammas <- fread(paste0("<<<<< filepath redacted >>>>>mrbrt_output/", c.model_version, "/r_mrbrt_gammas.csv"))
setnames(gammas, c("V1", "value", "draw", "mother_father"))
betas_melt <- melt(betas, id.var = c("draw", "mother_father"))
gammas[, variable := "gamma"]
gammas[, value := sqrt(value)]
betas_melt <- rbind(betas_melt, gammas, fill = T)

betas_summary <- betas_melt[,.(median = median(value, na.rm = T),
                               upper = quantile(value, .975, na.rm = T),
                               lower = quantile(value, .025, na.rm = T)), by = .(variable, mother_father)]

betas_summary[, `Beta Value` := paste0(formatC(median, digits =3, format = "f", flag = "+"), " (", formatC(lower, digits =3, format = "f", flag = "+"), " to ", formatC(upper, digits =3, format = "f", flag = "+"), ")")]
betas_summary[, `Beta Value` := gsub("+", "&nbsp", `Beta Value`, fixed = T)]
betas_summary <- dcast(betas_summary, variable ~ mother_father, value.var = "Beta Value")
setnames(betas_summary, c("Variable", 
                          "&nbsp&nbsp&nbsp&nbspPaternal Coefficients&nbsp&nbsp&nbsp&nbsp", 
                          "&nbsp&nbsp&nbsp&nbspMaternal Coefficients&nbsp&nbsp&nbsp&nbsp"))
temp <- data.table(Variable = betas_summary$Variable, new_var = c("Education (in Years)", 
                                                                  "Rural/Urban:Education",
                                                                  "Wealth/Income:Education",
                                                                  "Partner's Education:Education",
                                                                  "Child Sex:Education",
                                                                  "Mother's Age:Education",
                                                                  "0 to 12 Months:Education",
                                                                  "0 to 1 Month:Education",
                                                                  "1 to 12 Months:Education",
                                                                  "1 to 60 Months:Education",
                                                                  "12 to 60 Months:Education",
                                                                  "Standard Deviation/Year of Ed"))
betas_summary <- merge(betas_summary, temp, by = "Variable")
betas_summary[, Variable := new_var] %>% .[,new_var := NULL]

# stargazer(betas_summary, type = "html", out = "<<<<< filepath redacted >>>>>betas_summary.html",
#           summary = FALSE, rownames = F)

htmlTable::htmlTable(betas_summary,
                     align = c("l", "c", "c"),
                     rnames = F,
                     rgroup = c("Exposure", "Study-Level Covariates", "Child Age Dummy Variables", "Between-Study Hetergeneity"), 
                     n.rgroup = c(1,5,5,1), 
                     col.rgroup = c("none", "#E6E6F0"),
                     total = F
)





####################
## Funnel Plot #####
####################

graph_dat[!tolower(age_interval_std) %like% "std|stand" & is.na(field_citation_value) ] %>% 
  .[,.(age_interval_std, mother_father, slope, upper, lower)] %>% 
  .[,.(slope = mean(slope), upper = mean(upper), lower = mean(lower)), by = .(age_interval_std, mother_father)] -> summary_slope
summary_slope[, se := (upper - slope) / 1.96]

ggplot(graph_dat[!tolower(age_interval_std) %like% "std|stand"])+
  geom_jitter(aes(x = obs_std, y = obs_slope,color = as.factor(inlier), shape = as.factor(inlier), alpha = .1), width = .01) +
  theme_bw() + 
  facet_grid(age_interval_std ~ mother_father) + 
  geom_hline(data = summary_slope, aes(yintercept = slope), color = "black") +
  geom_segment(data = summary_slope, aes(x = 0, xend = 1, y = slope, yend = 1.96), color = "black", linetype = "dashed") +
  geom_segment(data = summary_slope, aes(x = 0, xend = 1, y = slope, yend = -1.96), color = "black", linetype = "dashed") +
  
  geom_rect(data = summary_slope,aes(xmin = -Inf, xmax = Inf, ymax = upper, ymin = lower), alpha= .25, fill = "gray30") + 
  geom_abline(aes(intercept = 0, slope = 0), linetype = "dotted") + 
  ggtitle("First Derivative of RR Curve Plotted Over\nNormalized Relative Risk Derivatives") +
  ylab("Normalized log(RR)") + 
  xlab("Standard Error of Observation") + 
  labs(size = "1/Standard Error of RR") +
  #scale_x_continuous(trans = scales::pseudo_log_trans(sigma = .05))+
  #scale_y_continuous(trans = scales::pseudo_log_trans(sigma = .05))+
  scale_x_reverse() +
  scale_shape_manual(values = c(1, 13),na.translate = FALSE) + 
  scale_color_discrete(na.translate = FALSE) +
  guides(alpha = FALSE) +
  coord_flip(xlim=c(0, 1), ylim=c(-1, 1))


##now do it as residuals
summary_slope_merge <- copy(summary_slope)
setnames(summary_slope_merge, c("slope", "upper", "lower"), c("slope_fit", "upper_fit", "lower_fit"))
graph_dat_funnel_resid <- merge(graph_dat,summary_slope_merge, by = c("age_interval_std", "mother_father") )
graph_dat_funnel_resid[, resid := obs_slope - slope_fit]

pseudo_log_trans_reverse <- function (sigma = 1, base = exp(1)) 
{
  trans_new("pseudo_log_reverse", function(x) -asinh(x/(2 * sigma))/log(base), 
            function(x) -2 * sigma * sinh(x * log(base)))
}

pdf("<<<<< filepath redacted >>>>>/Desktop/funnel_plot.pdf", width = 10, height = 8)
gg <- graph_dat_funnel_resid[!tolower(age_interval_std) %like% "std|stand" & !is.infinite(resid)] %>% 
  .[age_interval_std == "0 to 0.1, real", int := "0 to 1 Months"] %>%
  .[age_interval_std == "0.1 to 1, real", int := "1 to 12 Months"] %>%
  .[age_interval_std == "1 to 5, real", int := "12 to 60 Months"] %>%
  .[age_interval_std == "0 to 5, real", int := "0 to 60 Months"] %>%
  .[, int := factor(int, levels = c("0 to 1 Months", "1 to 12 Months", "12 to 60 Months", "0 to 60 Months", "0 to 12 Months", "1 to 60 Months"))] %>%
  ggplot(.)+
  geom_point(aes(x = obs_std, y = resid,color = as.factor(int), shape = as.factor(inlier))) +
  theme_bw() + 
  facet_grid( mother_father~.) + 
  geom_segment(data = summary_slope, aes(x = 0, xend = 3, y = 0, yend = 5.88), color = "black", linetype = "dashed") +
  geom_segment(data = summary_slope, aes(x = 0, xend = 3, y = 0, yend = -5.88), color = "black", linetype = "dashed") +
  geom_abline(aes(intercept = 0, slope = 0), linetype = "dotted") + 
  ggtitle("Figure 5: Residuals of Normalized RR Per 1 Year of Parental Education and Fitted Model") +
  ylab("Residual:\nNormalized log(RR) - Age Group-Specific Fit") + 
  xlab("Standard Error of Observation") + 
  labs(size = "1/Standard Error of RR") +
  scale_x_continuous(trans = pseudo_log_trans_reverse(sigma = .05), breaks = c(0, .05, .1, .25, .5, 1,2,3,4))+
  scale_y_continuous(trans = scales::pseudo_log_trans(sigma = .05), breaks = c(-2, -1, -.5, -.25,-.1, 0,.1, .25, .5, 1, 2))+
  #scale_x_reverse(trans = scales::pseudo_log_trans(sigma = .05)) +
  scale_shape_manual(values = c(1, 13),na.translate = FALSE) + 
  scale_color_discrete(na.translate = FALSE) +
  guides(alpha = FALSE) +
  labs(color = "Child Age Interval", shape = "")+
  coord_flip(xlim=c(0, 3.5), ylim=c(-2.5, 2.5))

print(gg)
dev.off()
######################################################3
## Summary chars table
########################################################
input_data[nchar(field_citation_value) > 16, field_citation_value_substr := str_sub(field_citation_value, 1, -8)]

input_data <- input_data %>% 
  .[,ihme_loc_id := tstrsplit(location_name, "|", keep = 2, fixed = T)]  %>% 
  .[(confounders_wealth == 1|confounders_income == 1) & confounders_otherparenteducation == 1, minimally_controlled := 1] %>% 
  .[!((confounders_wealth == 1|confounders_income == 1) & confounders_otherparenteducation == 1), minimally_controlled := 0]
input_data_temp <- input_data %>% 
  .[!is.na(age_interval_std) & age_interval_std %like% "real" & field_citation_value %like% "study_" & !is.na(effect_size)]
table1 <- input_data_temp %>% 
  .[,.(`Total\nObservations` = length(effect_size),
       `Total\nUnique Countries` = length(unique(ihme_loc_id)),
       `Total\nUnique Studies` =  length(unique(field_citation_value)),
       `Median Sample\nSize` = median(cohort_sample_size_total, na.rm = T),
       `Sample Size\nIQR`= IQR(cohort_sample_size_total, na.rm = T),
       `Percent\nMinimally\nControlled` = paste0(formatC(round((mean(minimally_controlled))*100, 2), digits = 2, format = "f"), " %")), by = .(age_interval_std, Mother_Father_education)]

table1 <- table1[Mother_Father_education > 0]

table1[age_interval_std == "0 to 0.1, real", `Age Interval` := "0 to 1"]
table1[age_interval_std == "0 to 1, real", `Age Interval` := "0 to 12"]
table1[age_interval_std == "0 to 5, real", `Age Interval` := "0 to 60"]
table1[age_interval_std == "0.1 to 1, real", `Age Interval` := "1 to 12"]
table1[age_interval_std == "0.1 to 5, real", `Age Interval` := "1 to 60"]
table1[age_interval_std == "1 to 5, real", `Age Interval` := "12 to 60"]

table1[Mother_Father_education == 1, Variable := "Paternal Education"]
table1[Mother_Father_education == 2, Variable := "Maternal Education"]

table1 <- table1[,.SD, .SDcols = c("Age Interval", "Variable","Total\nUnique Studies", "Total\nUnique Countries", "Total\nObservations", "Median Sample\nSize", "Sample Size\nIQR", "Percent\nMinimally\nControlled")]
table1 <- table1[order(`Age Interval`, Variable)]

table1_totals <- table1[,lapply(.SD, sum), .SDcols = names(table1)[!names(table1) %in% c("Age Interval", "Variable", "Median Sample\nSize", "Sample Size\nIQR", "Percent\nMinimally\nControlled")]]
table1_totals <- cbind(data.table(Variable = length(unique(table1$Variable)),
                                  `Age Interval` = length(unique(table1$`Age Interval`)), 
                                  `Median Sample\nSize` = input_data_temp[,median(cohort_sample_size_total, na.rm = T)],
                                  `Sample Size\nIQR` = input_data_temp[,IQR(cohort_sample_size_total, na.rm = T)],
                                  `Percent\nMinimally\nControlled` = input_data_temp[,paste0(formatC(round((mean(minimally_controlled))*100, 2), digits = 2, format = "f"), " %")]), table1_totals)
table1 <- rbind(table1, table1_totals, fill = T)
setnames(table1, "Age Interval", "Age Interval\n(In Months)")
htmlTable::htmlTable(table1,
                     align = paste(c("c", "c", rep("r", ncol(table1)-2)), collapse =""),
                     rnames = F,
                     rgroup = c(rep("", 12), "Totals"), 
                     n.rgroup = c(rep(1,13)), 
                     caption = "Summary Characteristics of Systematic Review",
                     col.rgroup = c("none", "#E6E6F0"),
                     total = T)
stargazer::stargazer(table1, type = "html", out = "<<<<< filepath redacted >>>>>table105292020.html", summary = F)
###try again
input_data_temp <- input_data[is_dhs == "Literature Review"]
input_data_temp[, confounders_haq_quintile := as.numeric(cut(confounders_haq, seq(0,1,.2), ordered_result = T))]
input_data_temp[cohort_sample_size_total > 1000 & cohort_sample_size_total!= 1889, three_star := 1]
input_data_temp[is.na(three_star), three_star := 0]
input_data_temp[three_star ==1 & CI_orig == 1, four_star := 1]
input_data_temp[is.na(four_star), four_star := 0]
input_data_temp[four_star == 1 & minimally_controlled==1, five_star := 1]
input_data_temp[is.na(five_star), five_star := 0]
input_data_temp[(confounders_wealth ==1 | confounders_income ==1 | confounders_wealth_index ==1), confounders_wealth := 1]
input_data_temp[(confounders_wealth ==1 | confounders_income ==1 | confounders_wealth_index ==1) & confounders_otherparenteducation == 1, minimally_controlled := 1]
input_data_temp[is.na(minimally_controlled), minimally_controlled := 0]
input_data_temp[rep_geography > 1 | is.na(rep_geography), rep_geography := 0]
input_data_temp[,super_region_name := factor(super_region_name, levels = c("High-income",
                                                                           "South Asia",
                                                                           "Latin America and Caribbean",
                                                                           "Sub-Saharan Africa",
                                                                           "Central Europe, Eastern Europe, and Central Asia",
                                                                           "Southeast Asia, East Asia, and Oceania",
                                                                           "North Africa and Middle East"
))]
input_data_temp[design == "Cross sectional", design := "Cross-sectional"]
input_data_temp[, design := factor(design, levels = c("Retrospective cohort", 
                                                      "Cross-sectional",
                                                      "Prospective cohort",
                                                      "Case-control",
                                                      "Nested case-control",
                                                      "Randomized controlled trial"))]
input_data_temp[tolower(design) %like% "case", type_case_control := 1] %>% 
  .[is.na(type_case_control), type_case_control := 0]
input_data_temp[cross_sectional  ==1, type_cross_sectional := 1] %>% 
  .[is.na(type_cross_sectional), type_cross_sectional := 0]

input_data_temp[, decade := floor(as.numeric(year_end_study)/10) * 10]
input_data_temp[is.na(decade)|decade > 2010, decade := floor(as.numeric(year_start_study)/10) * 10]
input_data_temp[nchar(year_end_study)>4, decade := floor(as.numeric(str_sub(year_start_study,7,-1))/10) * 10]
input_data_temp[is.na(decade), decade := floor(year_id/10) * 10]
input_data_temp[, decade := paste0(decade, " - ", decade + 9)]

table1 <- input_data_temp[,.N, by = .(age_interval_std, Mother_Father_education)]
table2 <- input_data_temp[,.N, by = .(super_region_name,Mother_Father_education )]
table6 <- input_data_temp[,.N, by = .(confounders_age,Mother_Father_education )]
table7 <- input_data_temp[,.N, by = .(confounders_sex,Mother_Father_education )]
table5 <- input_data_temp[,.N, by = .(confounders_otherparenteducation,Mother_Father_education )]
table4 <- input_data_temp[,.N, by = .(confounders_wealth,Mother_Father_education )]
table3 <- input_data_temp[,.N, by = .(minimally_controlled,Mother_Father_education )]
table8 <- input_data_temp[,.N, by = .(confounders_haq_quintile,Mother_Father_education )]
table9 <- input_data_temp[,.N, by = .(three_star,Mother_Father_education )]
table10 <- input_data_temp[,.N, by = .(four_star,Mother_Father_education )]
table11 <- input_data_temp[,.N, by = .(five_star,Mother_Father_education )]
table12 <- input_data_temp[,.N, by = .(rep_geography,Mother_Father_education )]
table13 <- input_data_temp[,.N, by = .(design,Mother_Father_education )]
table14 <- input_data_temp[,.N, by = .(confounders_rural_urban_residence,Mother_Father_education )]
table15 <- input_data_temp[,.N, by = .(decade,Mother_Father_education )]

formattr <- function(c.table){
  print(c.table)
  dt <- dcast(get(c.table), ... ~ Mother_Father_education, value.var = "N")
  dt[is.na(`2`), `2` := 0]
  dt[is.na(`1`), `1` := 0]
  dt[, sum1 := sum(`1`)]
  dt[, sum2 := sum(`2`)]
  dt[, `Paternal Education` := `1`/sum1]
  dt[, `Maternal Education` := `2`/sum2]
  dt[, variable := names(dt)[1]]
  setnames(dt, 1, "var_val")
  return(dt)
}

table2 <- rbindlist(lapply(paste0("table", c(1,13,12,15,2, 6,7,4,14,5,3)), formattr), fill = T)
table2 <- table2[var_val != 0]
table2[var_val == 1 & !variable %like% "haq", var_val := variable]
table2[!variable %in% c("super_region_name", "design", "decade"), var_val := c("0 to 1 Months",
                                                                               "0 to 12 Months",
                                                                               "0 to 60 Months",
                                                                               "1 to 12 Months",
                                                                               "1 to 60 Months",
                                                                               "12 to 60 Months",
                                                                               "Representative of National or Subnational Unit",
                                                                               "Controlled for Age of Mother",
                                                                               "Controlled for Sex of Child",
                                                                               "Controlled for Wealth or Income",
                                                                               "Controlled for Urbanicity",
                                                                               "Controlled for Partner's Education",
                                                                               
                                                                               "Controlled for Both Partner's Ed. & Wealth/Income"
)]



table2 <- table2[,.(var_val, `Maternal Education`, `Paternal Education`)]
setnames(table2, c(" ", "  Maternal Education  ", "  Paternal Education  "))
table2[, `  Paternal Education  ` := paste0(formatC(`  Paternal Education  `* 100, digits = 2, format = "f"), " %")]
table2[, `  Maternal Education  ` := paste0(formatC(`  Maternal Education  `* 100, digits = 2, format = "f"), " %")]


table1 <- input_data_temp %>% 
  .[,.(`Total Observations` = length(effect_size),
       `Total Unique Countries` = length(unique(ihme_loc_id)),
       `Total Unique Studies` =  length(unique(field_citation_value))),
    by = .(Mother_Father_education)]
table1 <- melt(table1, id.vars = "Mother_Father_education")
table1 <- dcast(table1, variable ~ Mother_Father_education)
setnames(table1, names(table2)[c(1,3,2)])
table2 <- rbindlist(list(table1, table2), use.names = TRUE)
table2 <- table2[,.SD, .SDcols = names(table2)[c(1,3,2)]]

setnames(table2, c(" ", "&nbspMaternal Education&nbsp","&nbspPaternal Education&nbsp"))

htmlTable::htmlTable(table2,
                     align = c("r", "r", "r"),
                     rnames = F,
                     rgroup = c(rep("Number of Observations", 1),
                                rep("Age Interval", 1),
                                "Study Design",
                                "Study-Level Characteristics",
                                "Year Study Was Published",
                                "GBD Super-Region",
                                "Study-Level Controls"),
                     n.rgroup = c(3,6,6,1,5,7,6), 
                     cgroup = c("", "Outcome Variable"),
                     n.cgroup = c(1,2),
                     caption = "Summary Characteristics of Systematic Review",
                     col.rgroup = c("none", "#E6E6F0"),
                     total = F)


4##############################################33
#########now make maps
###############################################
unique_locs <- unique(input_data_temp[,.(study_id, location_name)])
unique_locs[,ihme_loc_id := tstrsplit(location_name, "|", keep = 2, fixed = T)]
unique_locs[, ihme_loc_id := substr(ihme_loc_id, 1,3)]
counts_by_ihme_loc_id <- unique_locs[,.N, by = ihme_loc_id]
setnames(counts_by_ihme_loc_id, "N", "mapvar")
source("<<<<< filepath redacted >>>>>master/GBD_2016/inset_maps/noSubs/GBD_WITH_INSETS_MAPPING_FUNCTION.R")
pdf("<<<<< filepath redacted >>>>>/Desktop/data_map.pdf", onefile = T, width = 8, height = 6)

gbd_map(counts_by_ihme_loc_id,
        limits = c(1,2,3,4,5,10,20),
        col = "YlGnBu",
        title = "Figure 2 - Systematic Review Sources: Number of Extracted Studies by Location",
        labels = c("1", "2", "3", "4", "5 to 9", "10 to 20"))

dev.off()

unique_locs <- unique(input_data[,.(field_citation_value, location_name)])
unique_locs[,ihme_loc_id := tstrsplit(location_name, "|", keep = 2, fixed = T)]
unique_locs[is.na(ihme_loc_id), ihme_loc_id := substr(field_citation_value,1,3)]
unique_locs[, ihme_loc_id := substr(ihme_loc_id, 1,3)]
counts_by_ihme_loc_id <- unique_locs[,.(N = .N,
                                        DHS = length(field_citation_value[!field_citation_value %like% "study_"]),
                                        lit = length(field_citation_value[field_citation_value %like% "study_"])),
                                     by = ihme_loc_id]
setnames(counts_by_ihme_loc_id, "N", "mapvar")
write.csv(counts_by_ihme_loc_id,"<<<<< filepath redacted >>>>>/Desktop/data_map_counts.csv", row.names = F)

counts_by_ihme_loc_id[DHS > 0 & lit == 0, "DHS Sources Exclusively" := DHS]
counts_by_ihme_loc_id[DHS == 0 & lit > 0, "Literature Sources Exclusively" := lit]
counts_by_ihme_loc_id[DHS > 0 & lit > 0, "Mixed DHS & Literature Sources" := N]

library(sf)
shp <- read_sf("<<<<< filepath redacted >>>>>/Desktop/child_mort_dir/ref/ne_50m_admin_0_countries.shp")
map <- merge(counts_by_ihme_loc_id,shp, by.x = "ihme_loc_id", by.y = "ADM0_A3", all.y = T)

gg_map <- ggplot(map) + 
  geom_sf(aes(geometry = geometry), fill = "white") + 
  geom_sf(data = map[!is.na(map$`Literature Sources Exclusively`)],
          aes(geometry = geometry, alpha =`Literature Sources Exclusively`), na.rm = T,
          fill = "#1E88E5") + 
  geom_sf(data = map[!is.na(map$`Mixed DHS & Literature Sources`)],
          aes(geometry = geometry, alpha = `Mixed DHS & Literature Sources`), na.rm = T,
          fill = "#FFC107") + 
  geom_sf(data = map[!is.na(map$`DHS Sources Exclusively`)],
          aes(geometry = geometry, alpha = `DHS Sources Exclusively` ), na.rm = T, 
          fill = "#D81B60") + 
  theme_bw() + 
  theme(legend.position = "none")+
  ggtitle("Figure 2 - Number of Included Studies by Location")+
  scale_alpha_continuous(range = c(.1,1), breaks = c(0,1,2,5,10,20), labels = c(0,1,2,5,10,20))


g_legend <- function(a.gplot){ 
  tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
  legend <- tmp$grobs[[leg]] 
  return(legend)} 



leg_1 <- g_legend(ggplot(map) + 
                    geom_sf(data = map[!is.na(map$`Literature Sources Exclusively`)],
                            aes(geometry = geometry, alpha =`Literature Sources Exclusively`), na.rm = T,
                            fill = "#1E88E5") + 
                    theme_bw() + 
                    scale_alpha_continuous(range = c(.1,1), breaks = c(0,1,2,5,10,20), labels = c(0,1,"2 to 4","5 to 9","10 to 20",20))
)
leg_2 <- g_legend(ggplot(map) + 
                    geom_sf(data = map[!is.na(map$`Literature Sources Exclusively`)],
                            aes(geometry = geometry, alpha =`Literature Sources Exclusively`), na.rm = T,
                            fill = "#FFC107") + 
                    theme_bw() + 
                    labs(alpha = "Mixed DHS & Literature Sources")+
                    scale_alpha_continuous(range = c(.1,1), breaks = c(0,1,2,5,10,20), labels = c(0,1,"2 to 4","5 to 9","10 to 20",20))
)
leg_3 <- g_legend(ggplot(map) + 
                    geom_sf(data = map[!is.na(map$`Literature Sources Exclusively`)],
                            aes(geometry = geometry, alpha =`Literature Sources Exclusively`), na.rm = T,
                            fill = "#D81B60") + 
                    theme_bw() + 
                    labs(alpha = "DHS Sources Exclusively")+
                    scale_alpha_continuous(range = c(.1,1), breaks = c(0,1,2,5,10,20), labels = c(0,1,"2 to 4","5 to 9","10 to 20",20))
)

pdf("<<<<< filepath redacted >>>>>/Desktop/data_map.pdf", onefile = T, width = 13, height = 10)


grid.arrange(gg_map, leg_1, leg_2, leg_3,  layout_matrix = rbind(c(1,1,1,1,1,1,1,1,1),
                                                                 c(1,1,1,1,1,1,1,1,1),
                                                                 c(1,1,1,1,1,1,1,1,1),                                                               
                                                                 c(1,1,1,1,1,1,1,1,1),
                                                                 c(1,1,1,1,1,1,1,1,1),
                                                                 c(1,1,1,1,1,1,1,1,1),
                                                                 c(NA,NA,2,NA,3,NA,4,NA, NA)) )
dev.off()
##############################################



####number plugging#####

# On average, each additional year of maternal education 
# as compared to a mother with no education resulted in a
# relative risk of mortality for children aged 0 - 5 years
# of 0.948 (0.942 to 0.952). This translates to a relative
# risk death under the age of 5 of 0.724 (0.699 to 0.744)
# for children born to mothers with 6 years of education 
# compared to 0 years of education and 0.525 (0.489 to 0.553) 
# for children born to mothers with 12 years of education versus
# 0 years of education. These results represent 65 countries,
# years 1974 to 2014, and 311 unique combinations of covariates,
# and 34 studies (Figure 1).

#make pretty number function for mean lower and upper
pretty_mean <- function(mean, lower, upper, digits = 3){
  paste0(formatC(mean, digits), " (", formatC(lower, digits), " to ", formatC(upper, digits), ")")
}

data_summary[, rowsum := rowSums(.SD), .SDcols = confounders[confounders %like% "cohort|case|cross|confounders|dummy"]]
input_data[is.na(ihme_loc_id) & nchar(field_citation_value) == 7, ihme_loc_id := substr(field_citation_value,1,3)]
input_data[year_start_study == "NA" & nchar(field_citation_value) == 7, year_start_study := as.numeric(substr(field_citation_value,4,7))]

##1#

data_summary[age_interval_std == "0 to 5, real" &
               mother_father %like% "Mat" &
               maternal_education == 1 &
               cohort_unexp_def_lower == 0 &rowsum == 4 & confounders_age == 0,
             pretty_mean(rr, lower, upper, 3)]
##2##

data_summary[age_interval_std == "0 to 5, real" &
               mother_father %like% "Mat" &
               maternal_education == 6 &
               cohort_unexp_def_lower == 0 &rowsum == 4 & confounders_age == 0,
             pretty_mean(rr, lower, upper, 3)]


##3##
data_summary[age_interval_std == "0 to 5, real" &
               mother_father %like% "Mat" &
               maternal_education == 12 &
               cohort_unexp_def_lower == 0 &rowsum == 4 & confounders_age == 0,
             pretty_mean(rr, lower, upper, 3)]


data_summary[age_interval_std == "0 to 5, real" &
               mother_father %like% "Mat" &
               maternal_education == 16 &
               cohort_unexp_def_lower == 0 &rowsum == 4 & confounders_age == 0,
             pretty_mean(rr, lower, upper, 3)]

##4##
input_data[age_interval_std == "0 to 5, real" &
             mother_father %like% "Mat" & !is.na(field_citation_value), 
           paste(length(unique(ihme_loc_id)),
                 min(as.numeric(year_start_study), na.rm = T), 
                 max(as.numeric(year_start_study), na.rm = T), 
                 .N, 
                 length(unique(field_citation_value_substr)))]





# Although there is a broad range in the magnitude of the 
# dose response relationship, the bulk of study effect sizes
# -xX percent-sourced from both the systematic review and 
# primary analyses of DHS data, found a significant and
# protetective effect of maternal education on under-5 mortality. 

##5##
(input_data[age_interval_std == "0 to 5, real" &
              mother_father %like% "Mat" &
              !is.na(obs_slope), length(effect_size[obs_slope < 0 & ((effect_size < 1 & upper < 1)|(effect_size > 1 & lower > 1))])/length(effect_size)] * 100) %>% 
  round(3)


# Paternal education showed similar, but muted effects as compared
# to maternal education with respect to its preventative effect on 
# under-5 mortality with a relative risk per one additional year of
# paternal education of XX.XX (XXx to XXX). This translates to a 
# relative risk of death before age 5 of XX.XX (XXX to XXX) for  
# children born to fathers with 6 years of education compared to 
# 0 years of education and XX.XX (XXX to XXX) for children born to
# fathers with 12 years of education versus 0  years of education

##6##

data_summary[age_interval_std == "0 to 5, real" &
               mother_father %like% "Pat" &
               maternal_education == 1 &
               cohort_unexp_def_lower == 0 & rowsum == 4 & confounders_age == 0,
             pretty_mean(rr, lower, upper, 3)]
##7##

data_summary[age_interval_std == "0 to 5, real" &
               mother_father %like% "Pat" &
               maternal_education == 6 &
               cohort_unexp_def_lower == 0 &rowsum == 4 & confounders_age == 0,
             pretty_mean(rr, lower, upper, 3)]


##8##
data_summary[age_interval_std == "0 to 5, real" &
               mother_father %like% "Pat" &
               maternal_education == 12 &
               cohort_unexp_def_lower == 0 & rowsum == 4 & confounders_age == 0,
             pretty_mean(rr, lower, upper, 3)]

data_summary[age_interval_std == "0 to 5, real" &
               mother_father %like% "Pat" &
               maternal_education == 16 &
               cohort_unexp_def_lower == 0 & rowsum == 4 & confounders_age == 0,
             pretty_mean(rr, lower, upper, 3)]

# The range in effect sizes for paternal education was
# more mixed with only XX percent of systematic review
# and DHS data analyses effect sizes showing significantly 
# protective effects and XX showing detrimental effects of
# paternal education on under-5 mortality. 

##9##



(input_data[age_interval_std == "0 to 5, real" &
              mother_father %like% "Pat" &
              !is.na(obs_slope), length(effect_size[obs_slope < 0 & ((effect_size < 1 & upper < 1)|(effect_size > 1 & lower > 1))])/length(effect_size)] * 100) %>% 
  round(3)

##10##
# (input_data[age_interval_std == "0 to 5, real" &
#               mother_father %like% "Pat" &
#               !is.na(obs_slope), length(effect_size[obs_slope > 0 & ((effect_size < 1 & upper < 1)|(effect_size > 1 & lower > 1))])/length(effect_size)] * 100) %>% 
#   round(3)



# Studies on under-5 mortality, neonatal mortality, and post-neonatal
# infant mortality were prevalent, but studies on the age range 1-year
# to 4-years were rare, with only X sources analyzing this age group. 
# Therefore


##11##
input_data[age_interval_std == "1 to 5, real" &
             mother_father %like% "Mat|Pat" &
             !is.na(obs_slope), length(unique(field_citation_value_substr))]

# On average, each additional year of maternal education
# resulted in a relative risk of mortality for neonates 
# (1-28 days), as compared to a mother with no education,
# of XX.XX (XXX to XXX). For post-neonatal infants 
# (28 days - 1 year), the relative risk was XX.XX (XXX to XXX),
# and for young children (1 year - 5 years), the relative risk 
# was XX.XX (XXX to XXX). (Figure 1).

##12##
data_summary[age_interval_std == "0 to 0.1, real" &
               mother_father %like% "Mat" &
               maternal_education == 1 &
               cohort_unexp_def_lower == 0 &rowsum == 4 & confounders_age == 0,
             pretty_mean(rr, lower, upper, 3)]

##13##
data_summary[age_interval_std == "0.1 to 1, real" &
               mother_father %like% "Mat" &
               maternal_education == 1 &
               cohort_unexp_def_lower == 0 & rowsum == 4 & confounders_age == 0,
             pretty_mean(rr, lower, upper, 3)]
##14##
data_summary[age_interval_std == "1 to 5, real" &
               mother_father %like% "Mat" &
               maternal_education == 1 &
               cohort_unexp_def_lower == 0 & rowsum == 4 & confounders_age == 0,
             pretty_mean(rr, lower, upper, 3)]


##12b##
data_summary[age_interval_std == "0 to 0.1, real" &
               mother_father %like% "Mat" &
               maternal_education == 12 &
               cohort_unexp_def_lower == 0 &rowsum == 4 & confounders_age == 0,
             pretty_mean(rr, lower, upper, 3)]

##13b##
data_summary[age_interval_std == "0.1 to 1, real" &
               mother_father %like% "Mat" &
               maternal_education == 12 &
               cohort_unexp_def_lower == 0 & rowsum == 4 & confounders_age == 0,
             pretty_mean(rr, lower, upper, 3)]
##14b##
data_summary[age_interval_std == "1 to 5, real" &
               mother_father %like% "Mat" &
               maternal_education == 12 &
               cohort_unexp_def_lower == 0 & rowsum == 4 & confounders_age == 0,
             pretty_mean(rr, lower, upper, 3)]

##15##
data_summary[age_interval_std == "0 to 0.1, real" &
               mother_father %like% "Pat" &
               maternal_education == 1 &
               cohort_unexp_def_lower == 0 & rowsum == 4 & confounders_age == 0,
             pretty_mean(rr, lower, upper, 3)]

##16##
data_summary[age_interval_std == "0.1 to 1, real" &
               mother_father %like% "Pat" &
               maternal_education == 1 &
               cohort_unexp_def_lower == 0 &rowsum == 4 & confounders_age == 0,
             pretty_mean(rr, lower, upper, 3)]
##17##
data_summary[age_interval_std == "1 to 5, real" &
               mother_father %like% "Pat" &
               maternal_education == 1 &
               cohort_unexp_def_lower == 0 &rowsum == 4 & confounders_age == 0,
             pretty_mean(rr, lower, upper, 3)]

##15b##
data_summary[age_interval_std == "0 to 0.1, real" &
               mother_father %like% "Pat" &
               maternal_education == 12 &
               cohort_unexp_def_lower == 0 & rowsum == 4 & confounders_age == 0,
             pretty_mean(rr, lower, upper, 3)]

##16b##
data_summary[age_interval_std == "0.1 to 1, real" &
               mother_father %like% "Pat" &
               maternal_education == 12 &
               cohort_unexp_def_lower == 0 &rowsum == 4 & confounders_age == 0,
             pretty_mean(rr, lower, upper, 3)]
##17b##
data_summary[age_interval_std == "1 to 5, real" &
               mother_father %like% "Pat" &
               maternal_education == 12 &
               cohort_unexp_def_lower == 0 &rowsum == 4 & confounders_age == 0,
             pretty_mean(rr, lower, upper, 3)]

##18##
gammas <- fread(paste0("<<<<< filepath redacted >>>>>mrbrt_output/", c.model_version, "/r_mrbrt_gammas.csv"))
setnames(gammas, c("draw1", "value", "draw2", "mother_father"))
gammas <- gammas[, sd := sqrt(value)]
gammas_melt <- gammas[,.(mean = mean(sd),
                         upper = quantile(sd, .975),
                         lower = quantile(sd, .025)),
                      by = .(mother_father)]


gammas_melt[mother_father == 2, pretty_mean(mean, lower, upper, 3)]

##19##
gammas_melt[mother_father == 1, pretty_mean(mean, lower, upper, 3)]


#20##
gammas_melt[mother_father == 2, pretty_mean(mean*12, lower*12, upper*12, 3)]


#20##
gammas_melt[mother_father == 1, pretty_mean(mean*12, lower*12, upper*12, 3)]


##21##
data_summary[age_interval_std == "0 to 5, real" &
               mother_father %like% "Mat" &
               maternal_education == 12 &
               cohort_unexp_def_lower == 0 &rowsum == 4 & confounders_age == 0,
             pretty_mean(rr, lower, upper, 3)]

##22##
mat_05_rr <- data_summary[age_interval_std == "0 to 5, real" &
                            mother_father %like% "Mat" &
                            maternal_education == 12 &
                            cohort_unexp_def_lower == 0 &rowsum == 4 & confounders_age == 0, rr]

paste0("(",
       round(mat_05_rr + gammas_melt[mother_father == 1, mean]*12,3),
       " to " ,
       round(mat_05_rr - gammas_melt[mother_father == 1, mean]*12,3),
       ")")



### output for kam
# maternal 0 to 1 month, 1 to 12, and 12 to 60 months
temp <- data_summary[age_interval_std %in% c("0.1 to 1, real", "0 to 0.1, real", "1 to 5, real") &
               mother_father %like% "Mat|Pat" &
               maternal_education %in% c(6,12,16) &
               cohort_unexp_def_lower == 0 &rowsum == 4 & confounders_age == 0,
             pretty_mean(rr, lower, upper, 3), by = .(mother_father, maternal_education, age_interval_std)]
write.csv(temp, "<<<<< filepath redacted >>>>>/Desktop/rrs_for_fig_kam.csv")




##########################################################################
###########################################################################
##########################################################################

###################################################
### Copy over input data and plot similar graphs
###################################################
input_data <-fread('<<<<< filepath redacted >>>>>outliers_mrbrt.csv')

input_data <- input_data[,.(year_end_study, location_name,field_citation_value_substr,field_citation_value, age_interval_std, Mother_Father_education, inlier, age_start, age_end, effect_size,cohort_exp_def_lower,cohort_exp_def_upper,cohort_unexp_def_lower, cohort_unexp_def_upper, cohort_exp_def_mid, cohort_unexp_def_mid, upper, lower, log_effect)]
input_data[is.na(cohort_exp_def_mid), cohort_exp_def_mid := (cohort_exp_def_upper+cohort_exp_def_lower)/2]
input_data[is.na(cohort_unexp_def_mid), cohort_unexp_def_mid := (cohort_unexp_def_upper+cohort_unexp_def_lower)/2]

input_data[, exp_int := (cohort_exp_def_mid + cohort_unexp_def_mid)/2]
input_data[, exp_width := abs(cohort_exp_def_mid - cohort_unexp_def_mid)]
input_data[, obs_slope := (log(effect_size)/(cohort_exp_def_mid - cohort_unexp_def_mid))]
input_data[, obs_std := (((log(upper) - log(effect_size)))/1.96)/exp_width]
input_data[, obs_slope_reg := effect_size/(cohort_exp_def_mid - cohort_unexp_def_mid)]
input_data[, obs_slope_reg_se := abs(upper - effect_size)/1.96]
input_data[age_interval_std == "0 to 0.1, real", int := "0 to 1 months"]
input_data[age_interval_std == "0 to 1, real", int := "0 to 12 months"]
input_data[age_interval_std == "0 to 5, real", int := "0 to 60 months"]
input_data[age_interval_std == "0.1 to 1, real", int := "1 to 12 months"]
input_data[age_interval_std == "0.1 to 5, real", int := "1 to 60 months"]
input_data[age_interval_std == "1 to 5, real", int := "12 to 60 months"]

#input_data <- input_data[!duplicated(input_data[,.(field_citation_value_substr, age_start, age_end, Mother_Father_education)])]
# drawr <- function(i, dt, value.var, sd.var, draws){
#   print(i)
#   temp <- data.table(value_draws = rnorm(draws, dt[i,get(value.var)], dt[i, get(sd.var)]), id = "fill")
#   temp_dt <- dt[i]
#   temp_dt[, id := "fill"]
#   out <- temp_dt[temp, on = "id"]
#   out[, row_id := i]
# }
# 
# input_draws <- rbindlist(mclapply(1:nrow(input_data), drawr, dt = input_data,
#                                   value.var = "obs_slope", sd.var = "obs_std", draws = 100, mc.cores = 4))
# input_draws[, value_draws := exp(value_draws)]
# temp3 <- input_draws[, .(value_draws = median(value_draws, na.rm = T)), by = .( age_interval_std, Mother_Father_education)]
# input_draws[, row_id := factor(row_id, levels = sample(unique(input_draws$row_id)))]
# gg1 <- ggplot(input_draws) +
#   geom_density(aes(x = value_draws, y = ..density.., fill = (row_id)),color = NA, alpha = .2) +
#   geom_density(aes(x = value_draws, y = ..density..), color = "gray20")+ 
#   geom_vline(data = temp3, aes(xintercept = value_draws), color = "blue")+
#   theme_bw() + facet_grid(age_interval_std~Mother_Father_education) + theme(legend.position = "none") +
#   geom_vline(xintercept = 0, linetype = "dotted", color = "red") + 
#   geom_text(data = temp3, aes(x = value_draws-.05, y = 75, label = round(value_draws, 3))) + 
#   ylim(0,100)+ xlim(0.5, 1.5)
# print(gg1)
# gg1 <- ggplot(input_draws[inlier==1]) +
#   geom_density(aes(x = value_draws, y = ..density.., fill = (row_id)),color = NA, alpha = .2) +
#   geom_density(aes(x = value_draws, y = ..density..), color = "gray20")+ 
#   geom_vline(data = temp3, aes(xintercept = value_draws), color = "blue")+
#   theme_bw() + facet_grid(age_interval_std~Mother_Father_education) + theme(legend.position = "none") +
#   geom_vline(xintercept = 0, linetype = "dotted", color = "red") + 
#   geom_text(data = temp2, aes(x = value-.05, y = 75, label = round(value, 3))) + 
#   ylim(0,15)+ xlim(0.5, 1.5)
# print(gg1)
###################################################
### maps of dhs results by country
###################################################
all_summary
source(paste0("/home/j/DATA/SHAPE_FILES/GBD_geographies/master/GBD_2016/inset_maps/noSubs/GBD_WITH_INSETS_MAPPING_FUNCTION.R"))
dir.create("<<<<< filepath redacted >>>>>maps_dhs_rrs_05012020/")

shp <- read_sf(paste0("/home/j/DATA/SHAPE_FILES/GBD_geographies/master/GBD_2016/inset_maps/noSubs/GBD_WITH_INSETS_NOSUBS.shp"))


for(c.mat in unique(all_summary$mat_pat)){
  for(c.int in unique(all_summary$int)){
    print(c.mat)
    print(c.int)
    map_data <- copy(all_summary[mat_pat == c.mat & int == c.int & subset == "none" & vars == "base", .(mapvar = mean(mean), upper = mean(upper)), by = ihme_loc_id])
    map_data[, iso3 := ihme_loc_id]
    map_data <- merge(map_data, shp, by = "iso3", all = T)
    jpeg(gsub(" |'", "_", paste0("<<<<< filepath redacted >>>>>maps_dhs_rrs_05012020/", "all_", c.mat, "_", c.int,".jpeg")), height = 700, width = 1000)
    gg1 <- ggplot(map_data) +
      geom_sf(aes(geometry = geometry), fill = "white") +
      geom_sf(data = map_data[!is.na(map_data$mapvar)], aes(fill = mapvar, geometry = geometry), na.rm = T) +
      ylim (-48, 50) +xlim(-100, 130)+ theme_bw() + scale_fill_continuous(limits = c(-.2, 0), breaks = seq(-.2, 0, .05), type = "viridis") + 
      ggtitle(paste0(c.mat, ", ", c.int, ", All RRs"))
    print(gg1)
    dev.off()
    
    jpeg(gsub(" |'", "_", paste0("<<<<< filepath redacted >>>>>maps_dhs_rrs_05012020/", "sig_", c.mat, "_", c.int,".jpeg")), height = 700, width = 1000)
    gg1 <- ggplot(map_data) +
      geom_sf(aes(geometry = geometry), fill = "white") +
      geom_sf(data = map_data[!is.na(map_data$mapvar) & map_data$upper < 0], aes(fill = mapvar, geometry = geometry), na.rm = T) +
      ylim (-48, 50) +xlim(-100, 130)+ theme_bw() + scale_fill_continuous(limits = c(-.2, 0), breaks = seq(-.2, 0, .05), type = "viridis") + 
      ggtitle(paste0(c.mat, ", ", c.int, ", Only Significant RRs"))
    print(gg1)
    dev.off()
  }
}

source("/ihme/cc_resources/libraries/current/r/get_covariate_estimates.R")
source("/ihme/cc_resources/libraries/current/r/get_location_metadata.R")
locs <- get_location_metadata(location_set_id = 22)

map_data <- copy(all_summary[subset == "none" & vars == "base", .(mapvar = mean(mean), upper = mean(upper)), by = .(ihme_loc_id, int, mat_pat)])
map_data <- map_data[!is.na(mat_pat) & !is.na(int)]

haq <- get_covariate_estimates(covariate_id = c(1099),
                               location_id = locs[ihme_loc_id %in% map_data$ihme_loc_id, location_id], 
                               gbd_round_id = 7,
                               decomp_step = "iterative")

sdi <- get_covariate_estimates(covariate_id = c(881),
                               location_id = locs[ihme_loc_id %in% map_data$ihme_loc_id, location_id], 
                               gbd_round_id = 7,
                               decomp_step = "iterative")

map_data <- merge(map_data, locs[,.(ihme_loc_id, location_id)], by = "ihme_loc_id")
setnames(sdi, "mean_value", "sdi")
setnames(haq, "mean_value", "haq")

map_data <- merge(map_data, sdi[year_id == 2000], by = "location_id")
map_data <- merge(map_data, haq[year_id == 2000], by = "location_id")

ggplot(map_data[upper < 0]) + 
  geom_point(aes(x = sdi, y = mapvar)) + 
  geom_smooth(aes(x = sdi, y = mapvar), method = "lm")+
  facet_grid(int ~mat_pat)

ggplot(map_data[upper < 0]) + 
  geom_point(aes(x = haq, y = mapvar)) + 
  geom_smooth(aes(x = haq, y = mapvar), method = "lm")+
  facet_grid(int ~mat_pat)




###################################################
### Caterpillar plots
###################################################
input_data[, obs_slope_upper := (log(upper)/(cohort_exp_def_mid - cohort_unexp_def_mid))]
input_data[, obs_slope_lower := (log(lower)/(cohort_exp_def_mid - cohort_unexp_def_mid))]
input_data[is.infinite(obs_slope_lower), obs_slope_lower := -1]
input_data[is.nan(obs_slope_lower), obs_slope_lower := -1]

input_data[Mother_Father_education==2, mat_pat := "Mother's Education"]
input_data[Mother_Father_education==1, mat_pat := "Father's Education"]
input_data <- input_data[cohort_exp_def_mid != cohort_unexp_def_mid]

mrbrt_effect <- readRDS("<<<<< filepath redacted >>>>>draw_summary_save_04242020.rds")
avg_effects <- mrbrt_effect[rowsum == 0 & maternal_education < 5, .(obs_slope = log(mean(rr^(1/maternal_education))), 
                                           obs_slope_upper = log(mean(upper^(1/maternal_education))), 
                                           obs_slope_lower = log(mean(lower^(1/maternal_education)))), by = .(mother_father, age_interval_std)]
avg_effects[mother_father == "Maternal Education", mat_pat := "Mother's Education"]
avg_effects[mother_father == "Paternal Education", mat_pat := "Father's Education"]
avg_effects[age_interval_std == "0 to 0.1, real", int := "0 to 1 months"]
avg_effects[age_interval_std == "1 to 5, real", int := "12 to 60 months"]
avg_effects[age_interval_std == "0 to 1, real", int := "0 to 12 months"]
avg_effects[age_interval_std == "0 to 5, real", int := "0 to 60 months"]
avg_effects[age_interval_std == "0.1 to 1, real", int := "1 to 12 months"]
avg_effects[age_interval_std == "0.1 to 5, real", int := "1 to 60 months"]
avg_effects[,row_id := "RE Est."]
input_data[, size_dummy := "data"]
avg_effects[,size_dummy := "est"]

graph_dat <- rbind(avg_effects, input_data, fill = T)

graph_dat[,ordered_row_id := factor(row_id, levels = c("RE Est.", input_data[order(obs_slope), row_id]))]

# ggplot(graph_dat) + 
#   geom_errorbar(aes(x = as.factor(row_id), ymin = obs_slope_lower, ymax = obs_slope_upper, color = size_dummy)) + 
#   geom_point(aes(x = as.factor(row_id), y = obs_slope, color = size_dummy, size = size_dummy)) + 
#   facet_wrap(~mat_pat + int, ncol = 4, scales = "free_y") + ylim(-1, 1)+
#   coord_flip() + geom_hline(yintercept = 0, linetype = "dotted")+
#   theme_bw()  + theme(legend.position = "bottom")+xlab("Normalized Log(RR)")+
#   scale_size_discrete(range = c(.5,1))+
#   theme(axis.text.y=element_blank()) + 
#   scale_x_discrete(expand = expand_scale(add = 10))

input_data[tolower(field_citation_value) %like% "dhs|demog"|(!is.na(as.numeric(str_sub(field_citation_value,-4,-1))) & nchar(field_citation_value)==7), is_dhs := "DHS"]
input_data[is.na(is_dhs), is_dhs := "Literature Review"]

input_data[,ordered_row_id := order(obs_slope), by = .(int, is_dhs, mat_pat)]
input_data <- input_data[order(int, is_dhs, mat_pat,obs_slope)]

input_data[,row_id := NULL]
input_data[,row_id := 1:nrow(input_data)]
input_data[,row_id := factor(row_id, levels = c(input_data[, row_id]))]
input_data <- input_data[!is.na(obs_slope)] 
input_data[, is_dhs := factor(is_dhs, levels = rev(unique(input_data$is_dhs)))]
weird <- scales::trans_new("signed_log",
                           transform=function(x) sign(x)*(abs(x)^.5),
                           inverse=function(x) sign(x)*(abs(x))^2)
input_data <- input_data[inlier %in% 0:1]
input_data[, inlier := as.character(inlier)]
input_data[inlier == 1, inlier := "Inlier"]
input_data[inlier == 0, inlier := "Outlier"]

grob_list <- list()
i <- 0
for(c.mat in unique(input_data$mat_pat)){
  for(c.int in unique(input_data$int)){
    print(i)
    i <- i + 1
    gg1 <- ggplot(input_data[mat_pat == c.mat & int == c.int]) + 
      geom_errorbar(aes(x = as.factor(row_id), ymin = obs_slope_lower, ymax = obs_slope_upper, color = as.factor(inlier)), size = .25, width = 0) + 
      geom_point(aes(x = as.factor(row_id), y = obs_slope, color = as.factor(inlier)), size = .5)+
      coord_flip(ylim = c(-1,1)) + geom_hline(yintercept = 0, linetype = "dotted")+
      theme_bw()  + theme(legend.position = "none")+
      theme(axis.text.y=element_blank(), axis.title.y=element_blank(),
            axis.title.x = element_blank(),panel.grid.major.y = element_blank()) + 
      facet_grid(is_dhs~.,  scales="free_y") +
      scale_x_discrete(expand = expand_scale(add = 10)) + scale_y_continuous(trans = weird, limits = c(-5,5), breaks = c( -1, -.25, 0, .25, 1))
    
    
    glabel <-avg_effects[mat_pat == c.mat & int == c.int, paste0(round(obs_slope, 3), " (",round(obs_slope_lower, 3), " to ",round(obs_slope_upper, 3),")")]
    gg2 <- ggplot(avg_effects[mat_pat == c.mat & int == c.int]) + 
      geom_errorbar(aes(x = as.factor(row_id), ymin = obs_slope_lower, ymax = obs_slope_upper)) + 
      geom_point(aes(x = as.factor(row_id), y = obs_slope)) + 
      coord_flip() + geom_hline(yintercept = 0, linetype = "dotted")+
      theme_bw()  + theme(legend.position = "none")+
      ylab("Normalized Log(RR)")+
      theme(axis.text.y=element_blank(), axis.title.y=element_blank()) + 
      scale_x_discrete(expand = expand_scale(add = 10)) +
      geom_text(aes(x = "RE Est.", y = -.5, label = "Avg. Effect"), size = 5) + 
      geom_text(aes(x = "RE Est.", y = .27, label = glabel) ,size = 3.5) + scale_y_continuous(trans = weird, limits = c(-1,1), breaks = c(-1, -.25, 0, .25, 1))
    
    
    
    
    grob_list[[i]]<- arrangeGrob(gg1, gg2, layout_matrix = matrix(c(1,1,1,1,1,2)), top = paste0(c.mat, ", ", c.int))
    
  }
}

g_legend <- function(a.gplot){ 
  tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
  legend <- tmp$grobs[[leg]] 
  return(legend)} 


gg_legend <- g_legend(ggplot(input_data[mat_pat == c.mat & int == c.int]) + 
                        geom_errorbar(aes(x = as.factor(row_id), ymin = obs_slope_lower, ymax = obs_slope_upper, color = as.factor(inlier))) + 
                        theme(legend.position = "bottom") + labs(color = ""))
pdf("<<<<< filepath redacted >>>>>forest_plots_04242020b.pdf", width = 19, height = 18)
grid.arrange(grobs = list.append(grob_list, gg_legend), layout_matrix = rbind(c(1,2,3,4,5,6), c(1,2,3,4,5,6),
                                                                              c(1,2,3,4,5,6), c(1,2,3,4,5,6),c(1,2,3,4,5,6),
                                                                              c(7,8,9,10,11,12),c(7,8,9,10,11,12),
                                                                              c(7,8,9,10,11,12),
                                                                              c(7,8,9,10,11,12), c(7,8,9,10,11,12), 
                                                                              c(13,13,13,13,13,13)))
dev.off()


###################################################
### Beeswarm plots
###################################################
library(ggbeeswarm, lib.loc = "<<<<< filepath redacted >>>>>rlibs")

pdf(paste0("<<<<< filepath redacted >>>>>beeswarm_plots_05052020.pdf"),width = 9.5, height = 7)
gg1 <- ggplot(input_data[int %in% c("0 to 1 months", "1 to 12 months", "12 to 60 months")]) + 
  geom_beeswarm(aes(x = int, y = obs_slope, color = is_dhs), alpha = .5, size = .5, cex = .75) +
  facet_grid(~mat_pat) + 
  theme_bw() + 
  ylim(-.25, .25) + 
  geom_hline(yintercept = 0, linetype = "dotted") + 
  geom_point(data = avg_effects[int %in% c("0 to 1 months", "1 to 12 months", "12 to 60 months")], 
             aes(x = int, y = obs_slope)) + 
  # geom_line(data = avg_effects[int %in% c("0 to 1 months", "1 to 12 months", "12 to 60 months")], 
  #            aes(x = int, y = obs_slope, group = mat_pat)) +
  geom_errorbar(data = avg_effects[int %in% c("0 to 1 months", "1 to 12 months", "12 to 60 months")], 
                 aes(x = int, ymax = obs_slope_upper, ymin = obs_slope_lower), width = .2) + 
  labs(x = "Age Interval", y = "Normalized Log(RR)", color = "Data Source")+ 
  ggtitle("Regularized Log Relative Risks, Superimposed With\nAverage Effect Sizes")+
  guides(color = guide_legend(override.aes = list(size=2)), alpha = guide_legend(override.aes = list(alpha = 1)))
print(gg1)
dev.off()
  


###################################################
### Compare the ratios of the RRs of different age intervals
###################################################
effect_sizes <- dcast(all_summary[!is.na(mean) & variable %like% "matern|patern|parental|other_p",
                                  .(subset, vars, int, ihme_loc_id, variable,mean)], ... ~ int,
                      value.var = "mean")

unique(all_summary$int) %>% as.factor() -> fac_vec
pdf('<<<<< filepath redacted >>>>>under_5_int_ratios.pdf', height=11, width = 11)
for(c.int in unique(all_summary$int)){
  for(c.int2 in unique(all_summary$int)){
    if(c.int != c.int2){
      if(as.numeric(fac_vec[fac_vec == c.int]) < as.numeric(fac_vec[fac_vec == c.int2])){
        gg1 <- ggplot(effect_sizes[vars %in% c("base", "base, all covariates") & subset == "none" & variable %like% "atern"]) + 
          geom_point(aes(x = get(c.int), y = get(c.int2))) + facet_grid(variable ~ vars) + 
          xlim(-.2, 0) + ylim(-.2, 0) + geom_abline(slope = 1, intercept = 0, linetype = "dotted", color = "red") + 
          theme_bw() + xlab(c.int) + ylab(c.int2)
        sum <- effect_sizes[vars %in% c("base", "base, all covariates") & subset == "none", .(ratio = get(c.int)/get(c.int2)), by = .(subset, vars, ihme_loc_id, variable)]
        gg2 <- ggplot(sum[ variable %like% "atern"]) + 
          geom_histogram(aes(x = ratio, y = ..density..), binwidth = .1) + 
          geom_density(aes(x = ratio), adjust = 1, color = 'red') +
          xlim(0,2) +
          geom_vline(xintercept = 1, linetype = "dotted", color = "blue")+
          facet_grid(variable ~ vars)  +
          theme_bw()
        grid.arrange(gg1, gg2, nrow = 1,top = paste0(c.int, " and ", c.int2))
      }
    }
  }
}
for(c.int in unique(all_summary$int)){
  for(c.int2 in unique(all_summary$int)){
    if(c.int != c.int2){
      if(as.numeric(fac_vec[fac_vec == c.int]) < as.numeric(fac_vec[fac_vec == c.int2])){
        if(!c.int %in% c('0 to 12', '0 to 60', '60 to 180') & !c.int2 %in% c('0 to 12', '0 to 60', '60 to 180')){
          gg1 <- ggplot(effect_sizes[vars %in% c("base", "base, all covariates") & subset == "none" & !variable %like% "atern"]) + 
            geom_point(aes(x = get(c.int), y = get(c.int2))) + facet_grid(variable ~ vars) + 
            xlim(-.2, 0) + ylim(-.2, 0) + geom_abline(slope = 1, intercept = 0, linetype = "dotted", color = "red") + 
            theme_bw() + xlab(c.int) + ylab(c.int2)
          sum <- effect_sizes[vars %in% c("base", "base, all covariates") & subset == "none", .(ratio = get(c.int)/get(c.int2)), by = .(subset, vars, ihme_loc_id, variable)]
          gg2 <- ggplot(sum[!variable %like% "atern"]) + 
            geom_histogram(aes(x = ratio, y = ..density..), binwidth = .1) + 
            geom_density(aes(x = ratio), adjust = 1, color = 'red') +
            xlim(0,2) +
            geom_vline(xintercept = 1, linetype = "dotted", color = "blue")+
            facet_grid(variable ~ vars)  +
            theme_bw()
          grid.arrange(gg1, gg2, nrow = 1,top = paste0(c.int, " and ", c.int2))
        }
      }
    }
  }
}
dev.off()




for(c.vars in unique(all_summary$vars)[-1]){
  # test <- all_summary[variable %like% "mat" & int == c.int & (vars == "base" | vars == c.vars)] %>% .[vars == c.vars,upper<0] %>% sum
  # control <- all_summary[variable %like% "mat" & int == c.int & (vars == "base" | vars == c.vars)] %>% .[vars == "base",upper<0] %>% sum
  # testp <- all_summary[variable %like% "pat" & int == c.int & (vars == "base" | vars == c.vars)] %>% .[vars == c.vars,upper<0] %>% sum
  # controlp <- all_summary[variable %like% "pat" & int == c.int & (vars == "base" | vars == c.vars)] %>% .[vars == "base",upper<0] %>% sum
  # 
  gg1 <- ggplot(all_summary[variable %like% "mat" & (vars == "base" | vars == c.vars) & subset == "none"], aes(group = vars)) + 
    geom_point(aes(x = ihme_loc_id, y = mean,color = as.factor(vars)), position = position_dodge(width = 1)) + 
    geom_errorbar(aes(x = ihme_loc_id, ymin = lower, ymax = upper, color = as.factor(vars)), position = position_dodge(width = 1)) + 
    facet_wrap(~int, ncol = 1) + ylim(-.4, .2)+
    coord_flip() + geom_hline(yintercept = 0, linetype = "dotted")+
    theme_bw()  + theme(legend.position = "bottom") #+ 
  # annotate(geom="text", x=40, y=.18, label=paste0(control, " Sig."),color="red")+
  # annotate(geom="text", x=35, y=.18, label=paste0(test, " Sig."),color="blue")
  gg2 <- ggplot(all_summary[variable %like% "pat" & (vars == "base" | vars == c.vars)], aes(group = vars)) + 
    geom_point(aes(x = ihme_loc_id, y = mean,color = as.factor(vars)), position = position_dodge(width = 1)) + 
    geom_errorbar(aes(x = ihme_loc_id, ymin = lower, ymax = upper, color = as.factor(vars)), position = position_dodge(width = 1)) + 
    facet_grid(variable~subset) + ylim(-.4, .2)+
    coord_flip() + geom_hline(yintercept = 0, linetype = "dotted")+
    theme_bw()  + theme(legend.position = "bottom") #+ 
  # annotate(geom="text", x=40, y=.18, label=paste0(controlp, " Sig."),color="red")+
  # annotate(geom="text", x=35, y=.18, label=paste0(testp, " Sig."),color="blue")
  gg3 <- ggplot(all_summary[variable %like% "parental" &  (vars == "base" | vars == c.vars)], aes(group = vars)) + 
    geom_point(aes(x = ihme_loc_id, y = mean,color = as.factor(vars)), position = position_dodge(width = 1)) + 
    geom_errorbar(aes(x = ihme_loc_id, ymin = lower, ymax = upper, color = as.factor(vars)), position = position_dodge(width = 1)) + 
    facet_grid(variable~subset) + ylim(-.4, .2)+
    coord_flip() + geom_hline(yintercept = 0, linetype = "dotted")+
    theme_bw()  + theme(legend.position = "bottom") #+ 
  # annotate(geom="text", x=40, y=.18, label=paste0(control, " Sig."),color="red")+
  # annotate(geom="text", x=35, y=.18, label=paste0(test, " Sig."),color="blue")
  gg4 <- ggplot(all_summary[variable %like% "other_p" & (vars == "base" | vars == c.vars)], aes(group = vars)) + 
    geom_point(aes(x = ihme_loc_id, y = mean,color = as.factor(vars)), position = position_dodge(width = 1)) + 
    geom_errorbar(aes(x = ihme_loc_id, ymin = lower, ymax = upper, color = as.factor(vars)), position = position_dodge(width = 1)) + 
    facet_grid(variable~subset) + ylim(-.4, .2)+
    coord_flip() + geom_hline(yintercept = 0, linetype = "dotted")+
    theme_bw()  + theme(legend.position = "bottom") #+ 
  # annotate(geom="text", x=40, y=.18, label=paste0(control, " Sig."),color="red")+
  # annotate(geom="text", x=35, y=.18, label=paste0(test, " Sig."),color="blue")
  grid.arrange(gg1, gg2,gg3, gg4, top = gsub(", ", "\n", paste0(c.int, ", ",gsub( "base, ", "", c.vars))), nrow = 4)
  
}


fwrite(all_summary, '<<<<< filepath redacted >>>>>all_summary.csv')



###############################################################################
###############################################################################
###############################################################################



source("/ihme/cc_resources/libraries/current/r/get_location_metadata.R")
locs <- get_location_metadata(location_set_id = 22, gbd_round_id = 7, decomp_step = "iterative")

list.files(paste0("/ihme/covariates/education/under_5_mort/data/input_data/"), full.names = T)[list.files(paste0("/ihme/covariates/education/under_5_mort/data/input_data/"), full.names = F) %like% "20"] %>% 
  lapply(., fread) %>% rbindlist -> merged_data


comp_cases_table <- merged_data[,.(`Number of Surveys  ` = length(unique(survey_id)),
                                   `  Total Mothers  ` = length(unique(mother_id_var)),
                                   `  Total Live Births  ` = .N,
                                   `  Deaths 0-1 Month  ` = length(ihme_loc_id[child_alive == 0 & child_age_at_death_months <= 1]),
                                   `  Deaths 1-12 Months  ` = length(ihme_loc_id[child_alive == 0 & child_age_at_death_months > 1 & child_age_at_death_months <= 12]),
                                   `  Deaths 12-60 Months  ` = length(ihme_loc_id[child_alive == 0 & child_age_at_death_months > 12 & child_age_at_death_months <= 60])), by = .(ihme_loc_id)]
comp_cases_table <- merge(locs[,.(ihme_loc_id, lancet_label)], comp_cases_table, by = "ihme_loc_id")
comp_cases_table$ihme_loc_id <- NULL
comp_cases_table %>% setnames("lancet_label", "Location")

#append totals to the bottom
comp_cases_table_totals <- comp_cases_table[,lapply(.SD, sum), .SDcols = names(comp_cases_table)[names(comp_cases_table) != "Location"]]
comp_cases_table_totals <- cbind(data.table(Location = length(comp_cases_table$Location)), comp_cases_table_totals)
comp_cases_table <- rbind(comp_cases_table, comp_cases_table_totals, fill = T)
comp_cases_table[, names(comp_cases_table)[-1] := lapply(.SD, function(x){formatC(x, big.mark = ",")}), .SDcols = names(comp_cases_table)[-1]]


pdf('<<<<< filepath redacted >>>>>comp_cases_table_04220202.pdf', width = 10, height = 8)
knitr::knit_print(htmlTable::htmlTable(comp_cases_table,
                                       align = paste(c("c", "c", rep("r", ncol(comp_cases_table)-2)), collapse =""),
                                       rnames = F,
                                       rgroup = c("", "Totals"), 
                                       n.rgroup = c(58,1), 
                                       caption = "Live Births and Deaths by Country",
                                       total = T))
dev.off()
###############################################################################
###############################################################################
###############################################################################

model_ref <- coxph(Surv(merged_data$person_years_5q0,merged_data$mort5q0) ~
                     maternal_ed_yrs+
                     paternal_ed_yrs +
                     ihme_loc_id, data = merged_data)

int_standardizr <- function(c.int, data){
  print(int)
  c.int_begin <- 12*as.numeric(tstrsplit(c.int, " to ", keep = 1)[[1]])
  c.int_end <- 12*as.numeric(tstrsplit(c.int, " to ", keep = 2)[[1]])
  if(c.int_begin < c.int_end){
    data[child_age_at_death_months >= c.int_begin & child_age_at_death_months < c.int_end, child_died_int := 1]
    data[is.na(child_died_int), child_died_int := 0]
    data[child_age_at_death_months < c.int_begin, child_died_int := NA]
    data[child_age_at_death_months >= c.int_begin & child_age_at_death_months < c.int_end, person_years_int := child_age_at_death_months - c.int_begin]
    data[child_alive == "Yes", person_years_int := c.int_end - c.int_begin]
    data[is.na(child_died_int), person_years_int := NA]
    
    model_fit <- coxph(Surv(data$person_years_int,data$child_died_int) ~
                         maternal_ed_yrs+
                         paternal_ed_yrs +
                         ihme_loc_id, data = data)
    dt <- data.table(int_begin = c.int_begin,
                     int_end = c.int_end,
                     int = c.int,
                     maternal = exp(coef(model_fit)['maternal_ed_yrs']),
                     paternal = exp(coef(model_fit)['paternal_ed_yrs']))
    return(dt)
    
  }
}

model_ref <- int_standardizr( merged_data)

c_ints <- c("0 to 5",
            "0.05 to 1",
            "1 to 5",
            "0 to 0.75",
            "0.05 to 0.75",
            "0.05 to 5",
            "0 to 1",
            "1 to 15",
            "0 to 15", 
            "5 to 15")

xwalked_ints <- rbindlist(lapply(c_ints, int_standardizr, data = merged_data), fill = T)

ints_long <- melt(xwalked_ints, id.vars = c("int_begin", "int_end", "int"))
ints_long[, val_0_5 := value[int == "0 to 5"], by = variable]
ints_long[, ratio := value/val_0_5]
output <- ints_long[,.(int, variable, ratio)]     
write.csv(output, "<<<<< filepath redacted >>>>>xwalk_ages_rrs.csv", row.names = F)
