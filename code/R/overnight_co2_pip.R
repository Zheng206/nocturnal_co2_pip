.libPaths(c("/misc/appl/R-4.1/lib64/R/library","/home/zhengren/Desktop/cluster_set_up/r_packages"))
library(dplyr)
library(stringr)
library(tidyverse)
library(readxl)
library(ggplot2)
library(rlang)
library(DT)
library(broom)
library(gtsummary)
library(ggfortify)
library(kableExtra)
library(corrplot)
library(car)
library(openxlsx)
library(lme4)
library(broom.mixed)
library(glmmLasso)
library(kableExtra)
library(wavethresh)
library(survival)
library(ggsurvfit)
source("./code/R/co2_function.R")

# Data Management
total_df = read_csv("./data/processed_data/total_df_20240311.csv")
subject_summary = read_csv("./data/processed_data/subject_summary_20240311.csv")
features_df = read_csv("./data/processed_data/extracted_wavelet_features_20240311.csv")
input_index_df = read_csv("./data/missing/inpute_missing_data_index_20240311.csv")
df_info = read_csv("./data/data_info/info_0311.csv")
frequency_table = read_csv("./data/processed_data/frequency_table_20240311.csv")
used_record = df_info %>% filter(DC == "DC", period == ">= 4 hours") %>% dplyr::select(subject, day)
total_df = total_df %>% semi_join(used_record, by = c("subject", "day"))
subject_summary = subject_summary %>% semi_join(used_record, by = c("subject", "day"))
subject_summary$year = sapply(subject_summary$day, function(x) str_split(x, "-")[[1]][1], USE.NAMES = FALSE)
features_df = features_df %>% semi_join(used_record, by = c("subject", "day"))

write_csv(total_df, "./code/co2_app/data/data_stream/total_df.csv")
write_csv(features_df, "./code/co2_app/data/wavelet/features_df.csv")
write_csv(frequency_table, "./code/co2_app/data/wavelet/frequency_table.csv")
write_csv(input_index_df, "./code/co2_app/data/input_index/input_index.csv")

mda = read_excel("./data/raw_data/raw_MDA/MDA_DATA_FINAL.xls")
summary_df = mda %>% dplyr::select("PDF_Filename", "date_visit", "fvc", "onset_region___1", "onset_region___2", "onset_region___3", "alsfrs_10", "alsfrs_total", "niv_date", "trach_date", "death_date", "visit_age", "sex", "Days since onset at visit", "home_niv", "home_notes", "Analysis Period") %>% filter(grepl("HYP|MDA", PDF_Filename))
colnames(summary_df) = c("filename", "date_visit", "fvc", "onset_region___1", "onset_region___2", "onset_region___3", "alsfrs_10", "alsfrs_total", "niv_date", "trach_date", "death_date", "visit_age", "sex", "diagnosis_delay", "home_niv", "home_notes", "Analysis.Period")
summary_df$date_visit = sapply(summary_df$Analysis.Period, function(x) as.character(as.Date(str_split(x, " ")[[1]][1], format = "%d-%b-%Y")), USE.NAMES = FALSE)

summary_df$subject = sapply(summary_df$filename, function(x) {if(grepl("HYP", x)){str_split(x,"--")[[1]][1]}else{
  item = str_split(x,"_")[[1]]
  upenn = item[which(grepl("UPENN", item))]
  upenn = str_sub(upenn, start = 1, end = 7)
  return(upenn)
}}, USE.NAMES = FALSE)
summary_df$visit = sapply(summary_df$date_visit, function(x) str_split(x, "-")[[1]][2], USE.NAMES = FALSE)
summary_df$year = sapply(summary_df$date_visit, function(x) str_split(x, "-")[[1]][1], USE.NAMES = FALSE)

onset_region = summary_df %>% group_by(subject) %>% summarize(onset_region_1 = sum(na.omit(onset_region___1)),
                                                              onset_region_2 = sum(na.omit(onset_region___2)),
                                                              onset_region_3 = sum(na.omit(onset_region___3))) %>% 
  mutate(onset_region = case_when(onset_region_1 == 1 ~ "limb",
                                  onset_region_2 == 1 ~ "bulbar",
                                  onset_region_3 == 1 ~ "limb")) %>% mutate(onset_region_limb = case_when(onset_region == "limb" ~ 1,
                                                                                                          .default = 0)) %>% dplyr::select(subject, onset_region_limb)

summary_df = summary_df %>% left_join(onset_region, by = "subject") %>% dplyr::select("subject", "year", "visit", "onset_region_limb", "alsfrs_10", "alsfrs_total", "fvc", "niv_date", "trach_date", "death_date", "diagnosis_delay", "visit_age", "sex", "home_niv", "home_notes")
subject = unique(summary_df$subject)
age = sapply(subject, function(x) summary_df %>% filter(subject == x) %>% head(1) %>% pull(visit_age), USE.NAMES = FALSE)
age_df = data.frame(cbind(subject, age))
age_df$age = as.numeric(age_df$age)
summary_df = summary_df %>% left_join(age_df, by = "subject") %>% dplyr::select("subject", "year", "visit", "onset_region_limb", "alsfrs_10", "alsfrs_total", "fvc", "diagnosis_delay", "age", "sex", "home_niv", "home_notes")
summary_used = subject_summary
subject_used = unique(summary_used$subject)
summary_df = summary_df %>% filter(subject %in% subject_used)

niv_df = summary_df[c("subject", "year", "visit", "home_niv", "home_notes")]
cpap_note = niv_df %>% filter(home_niv == 1) %>% pull(home_notes) %>% unique() 
cpap_note = cpap_note[which(grepl("CPAP|C-PAP|cpap", cpap_note))]
niv_df = niv_df %>% mutate(home_niv = case_when(home_notes %in% cpap_note ~ 0,
                                                .default = home_niv))

niv_df = niv_df %>% filter(!is.na(home_niv)) %>% dplyr::select(subject, year, visit, home_niv) %>% group_by(subject, year, visit) %>% summarize(niv = sum(home_niv)) %>% mutate(niv = case_when(niv > 0 ~ 1,
                                                                                                                                                                                                .default = 0))
summary_df_unique = summary_df %>% dplyr::select(subject, year, visit, onset_region_limb, age, diagnosis_delay) %>% distinct() %>% na.omit()

summary_used = summary_used %>% left_join(summary_df_unique, by = c("subject", "year", "visit"))
sex_df = summary_df %>% dplyr::select(subject, sex) %>% distinct() %>% na.omit()
summary_used = summary_used %>% left_join(sex_df, by = c("subject"))
summary_used = summary_used %>% left_join(niv_df, by = c("subject", "year", "visit"))
write_csv(summary_used, "./code/co2_app/data/wavelet/subject_summary.csv")


# Wavelet Module Data
wave_features = colnames(features_df)[-c(1:2)]
wave_features = sapply(wave_features, function(x) gsub(" ", "_", x), USE.NAMES = FALSE)
colnames(features_df) = c(colnames(features_df)[1:2], wave_features)

# Mixed Model Data
subject_summary = summary_used
mix_df = subject_summary %>% left_join(features_df, by = c("subject", "day")) %>% na.omit() %>% group_by(subject, year, visit) %>% summarise_if(is.numeric, mean, na.rm = TRUE) %>% ungroup() 
mix_df= mix_df[,apply(mix_df, 2, function(col) { length(unique(col)) > 1 })]
wave_features = colnames(mix_df)[grepl("_level_", colnames(mix_df))]
wave_features = wave_features[which(!grepl("*_13$|*_12$|*_11$|*_10$", wave_features))]
mix_df[wave_features] = scale(mix_df[wave_features], center = TRUE, scale = TRUE)
mix_df$subject = as.factor(mix_df$subject)
write_csv(mix_df, "./code/co2_app/data/wavelet/mix_df.csv")

# Hierarchical Clustering

# Night-level Label
k_means_df = subject_summary %>% left_join(features_df, by = c("subject", "day")) %>% na.omit()
mat = as.matrix(k_means_df[wave_features]) 
hclust.out = hclust(dist(mat))
k_means_df$cluster = cutree(hclust.out, k = 4)
k_means_df$cluster_night = sapply(k_means_df$cluster, function(x){case_when(x == 1 ~ 1, 
                                                                            x == 2 ~ 1,
                                                                            x == 3 ~ 1,
                                                                            x == 4 ~ 0)}, USE.NAMES = FALSE)

event_summary = k_means_df %>% group_by(as.character(cluster_night)) %>% summarize(event_45 = mean(event_45),
                                                                   event_50 = mean(event_50),
                                                                   event_45.10 = mean(event_45.10),
                                                                   event_50.10 = mean(event_50.10),
                                                                   event_spo2 = mean(event_spo2),
                                                                   delta_pco2_event = mean(delta_pco2_event),
                                                                   delta_pco2_50_event = mean(delta_pco2_50_event)) %>% t() %>% data.frame() 
event_summary = event_summary[2:nrow(event_summary),]
colnames(event_summary) = c("normal", "abnormal")
event_summary = event_summary %>% mutate(normal = sprintf("%.3f", as.numeric(normal)),
                                       abnormal = sprintf("%.3f", as.numeric(abnormal)))
event_summary = as_tibble(event_summary, rownames = "event")
time_summary = k_means_df %>% group_by(as.character(cluster_night)) %>% summarize(PCO2.45 = mean(PCO2.45),
                                                                                  PCO2.50 = mean(PCO2.50),
                                                                                  SpO2.88 = mean(SpO2.88),
                                                                                  PCO2.45.10 = mean(PCO2.45.10),
                                                                                  PCO2.50.10 = mean(PCO2.50.10),
                                                                                  delta_pco2_time = mean(delta_pco2_time),
                                                                                  delta_pco2_50_time = mean(delta_pco2_50_time)) %>% t() %>% data.frame()
time_summary = time_summary[2:nrow(time_summary),]
colnames(time_summary) = c("normal", "abnormal")
time_summary = time_summary %>% mutate(normal = sprintf("%.3f", as.numeric(normal)),
                                       abnormal = sprintf("%.3f", as.numeric(abnormal)))
time_summary = as_tibble(time_summary, rownames = "period")

write_csv(event_summary, "./code/co2_app/data/abnormal_summary/event_summary.csv")
write_csv(time_summary, "./code/co2_app/data/abnormal_summary/time_summary.csv")

# Visit-level Label
visit_cluster = k_means_df %>% dplyr::select(subject, year, visit, day, cluster_night) %>% group_by(subject, year, visit) %>% summarize(cluster_visit = mean(cluster_night)) %>% ungroup()
visit_cluster$cluster_visit = sapply(visit_cluster$cluster_visit, function(x) case_when(x>=0.5~1,
                                                                       .default = 0), USE.NAMES = FALSE)
visit_cluster = visit_cluster %>% left_join(k_means_df %>% dplyr::select(subject, year, visit, mip, fvc), by = c("subject", "year", "visit")) %>% distinct()

k_means_df  = k_means_df %>% left_join(visit_cluster %>% dplyr::select(subject, year, visit, cluster_visit), by = c("subject", "year", "visit"))

# Patient-level Label

patient_cluster = k_means_df %>% dplyr::select(subject, year, visit, day, cluster_night) %>% group_by(subject) %>% summarize(cluster_patient = mean(cluster_night)) %>% ungroup()
patient_cluster$cluster_patient = sapply(patient_cluster$cluster_patient, function(x) case_when(x>=0.5~1,
                                                                                        .default = 0), USE.NAMES = FALSE)
patient_cluster =patient_cluster %>% left_join(k_means_df %>% dplyr::select(subject, year, visit, mip, fvc) %>% group_by(subject) %>% summarize(mip = mean(mip), fvc = mean(fvc)), by = c("subject")) %>% distinct()
k_means_df  = k_means_df %>% left_join(patient_cluster %>% dplyr::select(subject, cluster_patient), by = c("subject"))

k_means_df = k_means_df %>% mutate(cluster_night = case_when(cluster_night == 1~"Abnormal",
                                                             .default = "Normal"),
                                   cluster_visit = case_when(cluster_visit == 1~"Abnormal",
                                                             .default = "Normal"),
                                   cluster_patient = case_when(cluster_patient == 1~"Abnormal",
                                                               .default = "Normal"))
write_csv(k_means_df, "./code/co2_app/data/clustering/k_means_df.csv")

# Kaplan Meier Curve
######## try first visit as the starting point

km_df = k_means_df 
km_df$day = as_datetime(km_df$day)
visit_time_df = lapply(1:nrow(visit_cluster), function(i){
  sub_df = k_means_df %>% filter(subject == visit_cluster[[i, "subject"]], year == visit_cluster[[i, "year"]], visit == visit_cluster[[i, "visit"]]) %>% arrange(day) %>% head(1) %>% dplyr::select(subject, year, visit, day)
  colnames(sub_df) = c("subject", "year", "visit", "period")
  return(sub_df)
}) %>% bind_rows()
first_visit_date = lapply(unique(visit_time_df$subject), function(x){
  sub_df = visit_time_df %>% filter(subject == x) %>% arrange(period) %>% head(1) %>% dplyr::select(subject, year, visit, period)
  colnames(sub_df) = c("subject", "year", "visit", "period")
  return(sub_df)
}) %>% bind_rows()
colnames(first_visit_date) = c("subject", "year", "visit", "first_visit")
km_df = km_df %>% dplyr::select(subject, year, visit, event_45, event_50.10, event_spo2, SpO2.88, delta_pco2_event, delta_pco2_50_event) %>% group_by(subject, year, visit) %>% summarise_if(is.numeric, mean, na.rm = TRUE) %>% ungroup() %>% 
  left_join(visit_cluster, by = c("subject", "year", "visit")) %>% left_join(first_visit_date %>% dplyr::select(subject, first_visit), by = c("subject")) %>% left_join(visit_time_df, by = c("subject", "year", "visit")) %>% 
  mutate(fvc_50 = case_when(fvc < 50 ~ 1,
                            .default = 0),
         mip_60 = case_when(mip > -60 ~ 1,
                            .default = 0),
         pco2_45 = case_when(event_45 > 0 ~ 1,
                             .default = 0),
         spo2_88 = case_when(event_spo2 > 0 ~ 1,
                             .default = 0),
         pco2_50.10 = case_when(event_50.10 > 0 ~ 1,
                                .default = 0),
         delta_pco2_event = case_when(delta_pco2_event > 0 ~ 1,
                                      .default = 0),
         delta_pco2_50_event = case_when(delta_pco2_50_event > 0 ~ 1,
                                         .default = 0),
         nocturnal_test = case_when(cluster_visit == 1 ~ 1,
                                    .default = 0),
         time_sd = period - first_visit,
         time_co2 = period - first_visit)

sd_df = km_df %>% dplyr::select(subject, fvc, mip, fvc_50, mip_60, nocturnal_test, time_sd, cluster_visit) %>% distinct()
sd_df = sd_df %>% dplyr::select(subject, fvc_50, mip_60, nocturnal_test, time_sd, cluster_visit) %>% pivot_longer(c(2:4), names_to = "test", values_to = "event") %>% rename(time = time_sd)

pco2_df = km_df %>% dplyr::select(subject, pco2_45, spo2_88, pco2_50.10, delta_pco2_event, delta_pco2_50_event, time_co2, cluster_visit) %>% distinct()
pco2_df = pco2_df %>% pivot_longer(c(2:6), names_to = "test", values_to = "event") %>% rename(time = time_co2)
test_df =rbind(sd_df, pco2_df) 

write_csv(test_df, "./code/co2_app/data/kaplan_mierer/kaplan_mierer.csv")

# Cox Regression Model
cox_df = k_means_df 
cox_df$day = as_datetime(cox_df$day)
visit_time_df = lapply(1:nrow(visit_cluster), function(i){
  sub_df = k_means_df %>% filter(subject == visit_cluster[[i, "subject"]], year == visit_cluster[[i, "year"]], visit == visit_cluster[[i, "visit"]]) %>% arrange(day) %>% head(1) %>% dplyr::select(subject, year, visit, day)
  colnames(sub_df) = c("subject", "year", "visit", "period")
  return(sub_df)
}) %>% bind_rows()
first_visit_date = lapply(unique(visit_time_df$subject), function(x){
  sub_df = visit_time_df %>% filter(subject == x) %>% arrange(period) %>% head(1) %>% dplyr::select(subject, year, visit, period)
  colnames(sub_df) = c("subject", "year", "visit", "period")
  return(sub_df)
}) %>% bind_rows()
colnames(first_visit_date) = c("subject", "year", "visit", "first_visit")
raw_features = colnames(cox_df)[c(4:5, 11:16, 18:23, 32:35)]
raw_features = raw_features[c(1:6, 11:13, 17:18)]
cox_df = cox_df %>% dplyr::select(subject, year, visit, any_of(raw_features)) %>% group_by(subject, year, visit) %>% summarise_if(is.numeric, mean, na.rm = TRUE) %>% ungroup() %>% 
  left_join(visit_cluster, by = c("subject", "year", "visit")) %>% left_join(first_visit_date %>% dplyr::select(subject, first_visit), by = c("subject")) %>% left_join(visit_time_df, by = c("subject", "year", "visit")) %>% 
  mutate(fvc_50 = case_when(fvc < 50 ~ 1,
                            .default = 0),
         mip_60 = case_when(mip > -60 ~ 1,
                            .default = 0),
         nocturnal_test = case_when(cluster_visit == 1 ~ 1,
                                    .default = 0),
         time = period - first_visit)

cox_data_pre = function(cox_df, v, outcome){
  subj = unique(cox_df$subject)
  sub_df = cox_df %>% dplyr::select(subject, time, all_of(v), all_of(outcome)) 
  gen_df = lapply(subj, function(x){
    subj_df = sub_df %>% filter(subject == x) %>% arrange(time)
    subj_df = subj_df %>% mutate(tstart = lag(time), tstop = time) %>% filter(time > 0) %>% dplyr::select(subject, tstart, tstop, any_of(v), any_of(outcome))
    return(subj_df)
  }) %>% bind_rows()
  return(gen_df)
}

fvc_cox_result = lapply(raw_features, function(x){
  df1 = cox_data_pre(cox_df, x, "fvc_50")
  a = coxph(
    Surv(time = tstart, time2 = tstop, event = fvc_50) ~ eval(parse(text = x)), 
    data = df1
  ) %>% 
    tbl_regression(exp = TRUE)
  result = a$table_body %>% dplyr::select(variable, estimate, ci, p.value)
  result[1, "variable"] = x
  return(result)
}) %>% bind_rows() %>% mutate(outcome = "fvc_50")

mip_cox_result = lapply(raw_features, function(x){
  df1 = cox_data_pre(cox_df, x, "mip_60")
  a = coxph(
    Surv(time = tstart, time2 = tstop, event = mip_60) ~ eval(parse(text = x)), 
    data = df1
  ) %>% 
    tbl_regression(exp = TRUE)
  result = a$table_body %>% dplyr::select(variable, estimate, ci, p.value)
  result[1, "variable"] = x
  return(result)
}) %>% bind_rows() %>% mutate(outcome = "mip_60")

cox_result = rbind(fvc_cox_result, mip_cox_result) %>% arrange(p.value)
write_csv(cox_result, "./data/kaplan-meier/cox_result_20240311.csv")

