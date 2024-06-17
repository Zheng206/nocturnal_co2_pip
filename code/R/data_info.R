library(dplyr)
library(tidyr)
library(stringr)
library(tidyverse)
library(readxl)
source("./code/R/co2_function.R")

path = list.files("./data/raw_data/raw_data_stream/20240311", recursive = TRUE, full.names = TRUE)

# HYP
hyp_path = path[which(grepl("HYP", path))]
file_size = sapply(hyp_path, file.size, USE.NAMES = FALSE)
hyp_path = hyp_path[which(file_size > 80000)]
subject = sapply(hyp_path, info_get, info = "subject", USE.NAMES = FALSE)
visit = sapply(hyp_path, info_get, info = "visit", USE.NAMES = FALSE)
date = sapply(hyp_path, info_get, info = "date", USE.NAMES = FALSE)
df = data.frame(cbind(subject, visit, date, hyp_path))
df$DC = sapply(df$hyp_path, drift_correction, USE.NAMES = FALSE)
df$PR = sapply(df$hyp_path, PR, USE.NAMES = FALSE)
df$period = sapply(df$hyp_path, time_length, USE.NAMES = FALSE)
colnames(df) = c("subject", "visit", "date", "path", "DC", "PR", "period")

# MDA
MDA_path = path[which(grepl("MDA", path))]
subject = sapply(MDA_path, info_get, info = "subject", USE.NAMES = FALSE)
visit = sapply(MDA_path, info_get, info = "visit", USE.NAMES = FALSE)
date = sapply(MDA_path, info_get, info = "date", USE.NAMES = FALSE)
mda_df = data.frame(cbind(subject, visit, date, MDA_path))
mda_df$DC = sapply(mda_df$MDA_path, drift_correction, USE.NAMES = FALSE)
mda_df$PR = sapply(mda_df$MDA_path, PR, USE.NAMES = FALSE)
mda_df$period = sapply(mda_df$MDA_path, time_length, USE.NAMES = FALSE)
colnames(mda_df) = c("subject", "visit", "date", "path", "DC", "PR", "period")
df = rbind(df, mda_df)
df$day = sapply(df$path, function(x){
  sub_df = read_excel(x)
  t =convertToDate(sub_df$Time[nrow(sub_df)])
  return(as.character(t))}, USE.NAMES = FALSE)

df = df %>% mutate(path = basename(path)) %>% dplyr::select("subject", "visit", "day", "path", "DC", "period") %>% rename(file = path)
df[which(df$subject == "HYP09" & df$visit == "07"), "visit"] = "06"
df[which(df$subject == "HYP11" & df$visit == "04"), "visit"] = "03"
df[which(df$subject == "UPENN02" & df$visit == "10"), "visit"] = "09"
df$year = sapply(df$day, function(x) str_split(x, "-")[[1]][1], USE.NAMES = FALSE)

# Sample Size Summary
N = nrow(df)
N_visits = df %>% group_by(subject, year, visit) %>% summarize(count = n()) %>% ungroup() %>% nrow()
n_dc_4 = nrow(df %>% filter(DC == "DC", period == ">= 4 hours"))
n_dc_4_visit = df %>% filter(DC == "DC", period == ">= 4 hours") %>% group_by(subject, year, visit) %>% summarize(count = n()) %>% ungroup() %>% nrow() 

subject_summary = read_csv("./data/processed_data/subject_summary_20240311.csv")
subject_summary$day = as.character(subject_summary$day)
subject_summary$year = sapply(subject_summary$day, function(x) str_split(x, "-")[[1]][1], USE.NAMES = FALSE)
df_info = df %>% dplyr::select(subject, year, visit, day, file, DC, period) %>% left_join(subject_summary, by = c("subject", "year", "visit", "day"))
write_csv(df_info, "./data/data_info/info_0311.csv")

sample_size_summary = sample_info(df_info)
write_csv(sample_size_summary, "./data/data_info/data_summary_20240311.csv")






# Prelim Data Table
mda = read_excel("./data/MDA_DATA/MDA_DATA_FINAL.xls")
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
summary_df = summary_df %>% mutate(fvc_outcome = case_when(fvc < 50 ~ 1,
                                                           .default = 0))
subject = unique(summary_df$subject)
age = sapply(subject, function(x) summary_df %>% filter(subject == x) %>% head(1) %>% pull(visit_age), USE.NAMES = FALSE)
age_df = data.frame(cbind(subject, age))
age_df$age = as.numeric(age_df$age)
summary_df = summary_df %>% left_join(age_df, by = "subject") %>% dplyr::select("subject", "year", "visit", "onset_region_limb", "alsfrs_10", "alsfrs_total", "fvc", "niv_date", "trach_date", "death_date", "diagnosis_delay", "age", "fvc_outcome", "sex", "home_niv", "home_notes")
summary_used = read_csv("./code/co2_app/data/wavelet/subject_summary.csv") 
summary_used$year = sapply(summary_used$day, function(x) str_split(x, "-")[[1]][1], USE.NAMES = FALSE)
subject_used = unique(summary_used$subject)
summary_df = summary_df %>% filter(subject %in% subject_used)

niv_df = summary_df[c("subject", "year", "visit", "home_niv", "home_notes")]
cpap_note = niv_df %>% filter(home_niv == 1) %>% pull(home_notes) %>% unique() 
cpap_note = cpap_note[which(grepl("CPAP|C-PAP|cpap", cpap_note))]
niv_df = niv_df %>% mutate(home_niv = case_when(home_notes %in% cpap_note ~ 0,
                                                .default = home_niv))

niv_df = niv_df %>% filter(!is.na(home_niv)) %>% dplyr::select(subject, year, visit, home_niv) %>% group_by(subject, year, visit) %>% summarize(niv = sum(home_niv)) %>% mutate(niv = case_when(niv > 0 ~ 1,
                                                                                                                                                                                    .default = 0))
write_csv(niv_df, "./data/data_management/data_info/niv.csv")  

summary_df_unique = summary_df %>% dplyr::select(subject, year, visit, onset_region_limb, age, diagnosis_delay) %>% distinct() %>% na.omit()

summary_used = summary_used %>% left_join(summary_df_unique, by = c("subject", "year", "visit"))
sex_df = summary_df %>% dplyr::select(subject, sex) %>% distinct() %>% na.omit()
summary_used = summary_used %>% left_join(sex_df, by = c("subject"))
summary_used = summary_used %>% left_join(niv_df, by = c("subject", "year", "visit"))

age = paste0(round(summary_used %>% dplyr::select(subject, age) %>% distinct() %>% pull(age) %>% mean()),  " +/- ", round(summary_used %>% dplyr::select(subject, age) %>% distinct() %>% pull(age) %>% sd()))
sex = paste0(summary_used %>% dplyr::select(subject, sex) %>% na.omit() %>% distinct() %>% filter(sex == "M") %>% nrow(),  "(", round(100*(summary_used %>% dplyr::select(subject, sex) %>% na.omit() %>% distinct() %>% filter(sex == "M") %>% nrow()/14)), ")")

diag_delay = sapply(subject_used, function(x) summary_used %>% filter(subject == x) %>% head(1) %>% pull(diagnosis_delay)/365, USE.NAMES = FALSE)
diag_df = data.frame(cbind(subject_used, diag_delay))
summary_used = summary_used %>% left_join(diag_df, by = c("subject" = "subject_used"))
diag_delay_iqr = paste0(sprintf("%.1f", median(diag_delay)),"(", sprintf("%.0f", quantile(diag_delay)["25%"]), ",", sprintf("%.0f", quantile(diag_delay)["75%"]), ")")
alsfrs_total = paste0(round(summary_used %>% dplyr::select(subject, year, visit, alsfrs_total) %>% distinct() %>% pull(alsfrs_total) %>% mean()),  " +/- ", round(summary_used %>% dplyr::select(subject, year, visit, alsfrs_total) %>% distinct() %>% pull(alsfrs_total) %>% sd()))
alsfrs_10 = paste0(sprintf("%.0f", summary_used %>% dplyr::select(subject, year, visit, alsfrs_10) %>% distinct() %>% pull(alsfrs_10) %>% median()),"(", sprintf("%.0f", (summary_used %>% dplyr::select(subject, year, visit, alsfrs_10) %>% distinct() %>% pull(alsfrs_10) %>% quantile())["25%"]), ",", sprintf("%.0f", (summary_used %>% dplyr::select(subject, visit, alsfrs_10) %>% distinct() %>% pull(alsfrs_10) %>% quantile())["75%"]), ")")
alsfrs_11 = paste0(sprintf("%.0f", summary_used %>% dplyr::select(subject, year, visit, alsfrs_11) %>% distinct() %>% pull(alsfrs_11) %>% median()),"(", sprintf("%.0f", (summary_used %>% dplyr::select(subject, year, visit, alsfrs_11) %>% distinct() %>% pull(alsfrs_11) %>% quantile())["25%"]), ",", sprintf("%.0f", (summary_used %>% dplyr::select(subject, visit, alsfrs_11) %>% distinct() %>% pull(alsfrs_11) %>% quantile())["75%"]), ")")
sym_limb = paste0(summary_used %>% dplyr::select(subject, onset_region_limb) %>% na.omit() %>% distinct() %>% pull(onset_region_limb) %>% sum(),  "(", round(100*(summary_used %>% dplyr::select(subject, onset_region_limb) %>% na.omit() %>% distinct() %>% pull(onset_region_limb) %>% sum())/14), ")")
sym_bulbar = paste0(14 - (summary_used %>% dplyr::select(subject, onset_region_limb) %>% na.omit() %>% distinct() %>% pull(onset_region_limb) %>% sum()),  "(", 
                  round(100*((14 - (summary_used %>% dplyr::select(subject, onset_region_limb) %>% na.omit() %>% distinct() %>% pull(onset_region_limb) %>% sum()))/14)), ")")
fvc = paste0(round(summary_used %>% dplyr::select(subject, year, visit, fvc) %>% distinct() %>% pull(fvc) %>% mean()),  " +/- ", round(summary_used %>% dplyr::select(subject, year, visit, fvc) %>% distinct() %>% pull(fvc) %>% sd()))
mip = paste0(round(summary_used %>% dplyr::select(subject, year, visit, mip) %>% distinct() %>% pull(mip) %>% mean()),  " +/- ", round(summary_used %>% dplyr::select(subject, year, visit, mip) %>% distinct() %>% pull(mip) %>% sd()))
spot_co2 = paste0(round(summary_used %>% dplyr::select(subject, year, visit, spot_co2) %>% distinct() %>% pull(spot_co2) %>% mean()),  " +/- ", round(summary_used %>% dplyr::select(subject, year, visit, spot_co2) %>% distinct() %>% pull(spot_co2) %>% sd()))

## Table 1
table_8 = summary_used %>% dplyr::select(subject, year, visit, event_45, spot_co2, fvc, event_spo2_less_5, event_45.10, delta_pco2_50_event, delta_pco2_event, mean_PCO2, niv) 
table_8 = table_8 %>% mutate(mean_PCO2_50 = case_when(mean_PCO2 > 50 ~ 1,
                                                      .default = 0)) %>% filter(niv == 0)
row_print = function(event, table_8){
  event_45_visit = paste0(table_8 %>% filter(.data[[event]] > 0) %>% group_by(subject, year, visit) %>% summarize(count = n()) %>% nrow(), "(",
                          round(100 * (table_8 %>% filter(.data[[event]] > 0) %>% group_by(subject, year, visit) %>% summarize(count = n()) %>% nrow()/table_8 %>% group_by(subject, year, visit) %>% summarize(count = n()) %>% nrow())), ")")
  event_45_spot = paste0(table_8 %>% filter(.data[[event]] > 0, spot_co2 < 45) %>% group_by(subject, year, visit) %>% summarize(count = n()) %>% nrow(), "/", table_8 %>% filter(.data[[event]] > 0) %>% group_by(subject, year, visit) %>% summarize(count = n()) %>% nrow(), "(",
                         round(100 * (table_8 %>% filter(.data[[event]] > 0, spot_co2 < 45) %>% group_by(subject, year, visit) %>% summarize(count = n()) %>% nrow()/table_8 %>% filter(.data[[event]] > 0) %>% group_by(subject, year, visit) %>% summarize(count = n()) %>% nrow())), ")")
  event_45_fvc = paste0(table_8 %>% filter(.data[[event]] > 0, fvc > 45) %>% group_by(subject, year, visit) %>% summarize(count = n()) %>% nrow(), "/", table_8 %>% filter(.data[[event]] > 0) %>% group_by(subject, year, visit) %>% summarize(count = n()) %>% nrow(), "(",
                        round(100 * (table_8 %>% filter(.data[[event]] > 0, fvc > 45) %>% group_by(subject, year, visit) %>% summarize(count = n()) %>% nrow()/table_8 %>% filter(.data[[event]] > 0) %>% group_by(subject, year, visit) %>% summarize(count = n()) %>% nrow())), ")")
  event_45_spo2 = paste0(table_8 %>% filter(.data[[event]] > 0, event_spo2_less_5 > 0) %>% group_by(subject, year, visit) %>% summarize(count = n()) %>% nrow(), "/", table_8 %>% filter(.data[[event]] > 0) %>% group_by(subject, year, visit) %>% summarize(count = n()) %>% nrow(), "(",
                         round(100 * (table_8 %>% filter(.data[[event]] > 0, event_spo2_less_5 > 0) %>% group_by(subject, year, visit) %>% summarize(count = n()) %>% nrow()/table_8 %>% filter(.data[[event]] > 0) %>% group_by(subject, year, visit) %>% summarize(count = n()) %>% nrow())), ")")
  print(paste0(event_45_visit, "; ", event_45_spot, "; ", event_45_fvc, "; ", event_45_spo2))
}

### Row 1
row_print("event_45",  table_8)
### Row 2
row_print("event_45.10",  table_8)
### Row 3
row_print("delta_pco2_50_event",  table_8)
### Row 4
row_print("delta_pco2_event",  table_8)
### Row 5
row_print("mean_PCO2_50",  table_8)

## Patient Table
summary_used = summary_used %>% mutate(fvc_event = case_when(fvc < 50 ~ 1, .default = 0),
                                       mip_event = case_when(mip > -60 ~ 1, .default = 0),
                                       spo2_event = case_when(SpO2.88 > 0 ~ 1, .default = 0))
first_date_event = function(s, test, summary_used){
  sub_df = summary_used %>% filter(subject == s & eval(parse(text = test)) > 0) %>% head(1)
  if(nrow(sub_df) > 0){
    date = sub_df %>% pull(day) %>% as.Date()
  }else{date = NA}
  return(date)
}

last_date_event = function(s, summary_used){
  sub_df = summary_used %>% filter(subject == s) %>% tail(1) 
  if(nrow(sub_df) > 0){
    date = sub_df %>% pull(day) %>% as.Date()
  }else{date = NA}
  return(date)
}

event_occur = function(subject_used, standard, test, summary_used, niv_ex = TRUE){
  if(niv_ex){summary_used = summary_used %>% filter(niv == 0)}
  test_occur = sapply(subject_used, function(x){
    date_test = first_date_event(x, test, summary_used)
    return(as.character(date_test))
  }, USE.NAMES = FALSE)
  
  standard_occur = sapply(subject_used, function(x){
    date_standard = first_date_event(x, standard, summary_used)
    return(as.character(date_standard))
  }, USE.NAMES = FALSE)
  
  before_standard = sapply(1:length(subject_used), function(i){
    diff = as.numeric(as.Date(standard_occur[i], "%Y-%m-%d") - as.Date(test_occur[i], "%Y-%m-%d"))
    if(!is.na(test_occur[i])){
      if(!is.na(diff)){if(diff <= 0){return("No")}else{return("Yes")}}else{return("Yes")}
    }else{return("No")}
  }, USE.NAMES = FALSE)
  
  test_occur = sapply(test_occur, function(x){
    if(is.na(x)){return("did not occur")}else{return("occurred")}
  }, USE.NAMES = FALSE)
  
  standard_occur = sapply(standard_occur, function(x){
    if(is.na(x)){return("did not occur")}else{return("occurred")}
  }, USE.NAMES = FALSE)
  result_df = data.frame(cbind(subject = subject_used, test = rep(test, length(subject_used)), test_occur = test_occur, standard = rep(standard, length(subject_used)), standard_occur = standard_occur, before_standard = before_standard))
  return(result_df)
}

Before_test = function(subject_sub, standard, test, summary_used, niv_ex = TRUE){
  if(niv_ex){summary_used = summary_used %>% filter(niv == 0)}
  before_standard = sapply(subject_sub, function(x){
    date_test = first_date_event(x, test, summary_used)
    date_standard = first_date_event(x, standard, summary_used)
    date_last = last_date_event(x, summary_used)
    if(!is.na(date_test)){
      if(!is.na(date_standard)){
        diff = as.numeric(date_standard - date_test)
      }else{
        #diff = as.numeric(date_last - date_test) + 365
        diff = Inf
        }
    }else{
      if(!is.na(date_standard)){
        #diff = as.numeric(date_standard - date_last) - 365
        diff = -Inf
      }else{diff = NA}
    }
    return(diff)
  }, USE.NAMES = FALSE)
  count = paste0(length(before_standard[which(before_standard > 0)]), " (", sprintf("%.2f", 100*length(before_standard[which(before_standard > 0)])/length(subject_sub)), ")")
  #quantile_range = unname(quantile(na.omit(before_standard)[which(na.omit(before_standard) > 0)])[2:4])
  #median_date = paste0(quantile_range[2], " (", quantile_range[1], ",", quantile_range[3], ")")
  return(list("count" = count,  "before_standard" = before_standard))
}

patient_event_table = function(test, summary_used, niv_ex = TRUE){
  if(niv_ex){summary_used = summary_used %>% filter(niv == 0)}
  co2_test = test
  test_summary = summary_used %>% group_by(subject) %>% summarize(event = case_when(sum(eval(parse(text = test))) > 0 ~ 1,
                                                                                    .default = 0))
  count = paste0(test_summary %>% filter(event == 1) %>% nrow(), " (", sprintf("%.2f", 100*(test_summary %>% filter(event == 1) %>% nrow())/nrow(test_summary)), ")")
  subject_sub = unique(test_summary %>% filter(event == 1) %>% pull(subject))
  fvc_compare = Before_test(subject_sub, "fvc_event", test, summary_used)
  before_fvc = fvc_compare$count
  before_fvc_time = paste0(sort(fvc_compare$before_standard), collapse = ",")
  mip_compare = Before_test(subject_sub, "mip_event", test, summary_used)
  before_mip = mip_compare$count
  before_mip_time = paste0(sort(mip_compare$before_standard), collapse = ",")
  spo2_compare = Before_test(subject_sub, "spo2_event", test, summary_used)
  before_spo2 = spo2_compare$count
  before_spo2_time = paste0(sort(spo2_compare$before_standard), collapse = ",")
  count_table = data.frame(cbind(co2_test, count, before_fvc, before_fvc_time, before_mip, before_mip_time, before_spo2, before_spo2_time))
  day_table = data.frame(cbind(subject = subject_sub, before_fvc = fvc_compare$before_standard, before_mip = mip_compare$before_standard, before_spo2 = spo2_compare$before_standard, test = rep(test, length(subject_sub))))
  return(list("count_table"= count_table, "day_table"= day_table))
}


participant_table = lapply(c("event_45.10", "event_50.10", "delta_pco2_50_event"), function(x) patient_event_table(x, summary_used)) 
participant_count_table = lapply(1:length(participant_table), function(i) participant_table[[i]][["count_table"]]) %>% bind_rows()
participant_day_table = lapply(1:length(participant_table), function(i) participant_table[[i]][["day_table"]]) %>% bind_rows()

info_table = lapply(c("event_45.10", "event_50.10", "delta_pco2_50_event"), function(x){
  result_df = lapply(c("fvc_event", "mip_event", "spo2_event"), function(y){
  event_occur(subject_used = subject_used, standard = y, test = x, summary_used = summary_used)
  }) %>% bind_rows()
  return(result_df)
  }) %>% bind_rows()

info_table %>% filter(test_occur == "occurred") %>% group_by(test, standard) %>% summarize(Inf_rate = sum(standard_occur == "did not occur") / sum(before_standard == "Yes"))

####STOP HERE######

name = readDICOM(path = "~/Desktop/s0201")$hdr[[1]] %>%
  filter(name == "ProtocolName") %>%
  pull(value)


# Prelim Data Table
subject_summary = read_csv("./data/cluster/hierarchical_clustering_results.csv")
info = read_excel("./data/MDA_DATA/MDA_DATA_V3_05.10.23.xls")
info = info %>% dplyr::select("PDF_Filename", "date_visit", "fvc", "mip", "spot_co2",	"spot_o2", "alsfrs_10", "alsfrs_11", "alsfrs_total", "sex", "visit_age") %>% 
  filter(grepl("HYP|MDA", PDF_Filename)) %>% na.omit()
colnames(info) = c("filename", "period", "fvc", "mip",  "spot_co2",	"spot_o2", "alsfrs_10", "alsfrs_11", "alsfrs_total", "sex", "age")

info$subject = sapply(info$filename, function(x) {if(grepl("HYP", x)){str_split(x,"--")[[1]][1]}else{
  item = str_split(x,"_")[[1]]
  upenn = item[which(grepl("UPENN", item))]
  upenn = str_sub(upenn, start = 1, end = 7)
  return(upenn)
}}, USE.NAMES = FALSE)
info$day  = as.character(info$period)

subject_summary = subject_summary %>% left_join(info %>% dplyr::select(subject, sex, age), by = "subject")

# Visit-level Table 2
visit_summary = subject_summary %>% dplyr::select(subject, day, visit, event_45, event_45.10, delta_pco2_50_event, fvc, event_spo2_less_5) %>% 
  mutate(fvc_event = case_when(fvc > 50 ~ 1,
                               .default = 0)) %>% 
  group_by(subject, year, visit) %>% 
  summarize(event_45 = sum(event_45),
            event_45.10 = sum(event_45.10),
            delta_pco2_50_event = sum(delta_pco2_50_event), 
            fvc_event = sum(fvc_event),
            event_spo2_less_5 = sum(event_spo2_less_5))

# Follow up time
info = read_excel("./data/MDA_DATA/MDA_DATA_V3_08.04.23.xls")
info = read_excel("./data/MDA_DATA/MDA_DATA_V3_05.10.23.xls")
info = info %>% dplyr::select("PDF_Filename", "date_visit", "Analysis Period") %>% filter(grepl("HYP|MDA", PDF_Filename)) 
colnames(info) = c("filename", "period", "ap")

info$subject = sapply(info$filename, function(x) {if(grepl("HYP", x)){str_split(x,"--")[[1]][1]}else{
  item = str_split(x,"_")[[1]]
  upenn = item[which(grepl("UPENN", item))]
  upenn = str_sub(upenn, start = 1, end = 7)
  return(upenn)
}}, USE.NAMES = FALSE)

info_start = na.omit(info[c("subject", "period")])
info_start$period = as.Date(info_start$period)
info_end = na.omit(info[c("subject", "ap")])
info_end$ap = sapply(info_end$ap, function(x) str_split(str_split(x, " - ")[[1]][2], " ")[[1]][1], USE.NAMES = FALSE)
info_end$ap = sapply(info_end$ap, function(x) as.character(as.Date(x, format = "%d-%b-%Y")), USE.NAMES = FALSE)
info_end$ap = as.Date(info_end$ap) 

end = info_end %>% filter(subject %in% unique(subject_summary$subject)) %>% group_by(subject) %>% summarize(end_date = tail(ap, n = 1)) %>% ungroup()
start = info_start %>% filter(subject %in% unique(subject_summary$subject)) %>% group_by(subject) %>% summarize(start_date = head(period, n = 1)) %>% ungroup()
date_df = start %>% left_join(end, by = "subject") %>% mutate(follow_up_time = end_date - start_date)

median(date_df$follow_up_time)
quantile(date_df$follow_up_time)

date_df_end = subject_summary %>% group_by(subject) %>% summarize(end_date = head(day, n = 1)) %>% ungroup()
date_df_tail = subject_summary %>% group_by(subject) %>% summarize(start_date = tail(day, n = 1)) %>% ungroup()
date_df %>% left_join(date_df_tail, by = c("subject")) %>% mutate(follow_up_time = end_date - start_date)
follow_up_time = lapply(unique(date_df$subject), function(x){
  sub_df = date_df %>% filter(subject == x) %>% mutate(time = date - lag(date)) %>% na.omit()
}) %>% bind_rows()

median(follow_up_time$time)
IQR(follow_up_time$time)