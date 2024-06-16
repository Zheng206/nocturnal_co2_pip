library(dplyr)
library(stringr)
library(tidyverse)
library(glue)
library(readxl)
library(ggplot2)
library(rlang)
library(DT)
library(broom)
library(ggfortify)
library(kableExtra)
library(corrplot)
library(car)
library(openxlsx)
library(lme4)
library(broom.mixed)

source("./code/R/co2_function.R")

## Compile total_df


new_files = list.files("./data/raw_data/raw_data_stream/20240311", recursive = TRUE, full.names = TRUE)

total_df_gen = function(path, day){
  hyp_path = path[which(grepl("HYP", path))]
  file_size = sapply(hyp_path, file.size, USE.NAMES = FALSE)
  hyp_path = hyp_path[which(file_size > 80000)]
  subject = sapply(hyp_path, info_get, info = "subject", USE.NAMES = FALSE)
  visit = sapply(hyp_path, info_get, info = "visit", USE.NAMES = FALSE)
  date = sapply(hyp_path, info_get, info = "date", USE.NAMES = FALSE)
  
  df = data.frame(cbind(subject, visit, date, hyp_path))
  
  df$DC = sapply(df$hyp_path, drift_correction, USE.NAMES = FALSE)
  df$PR = sapply(df$hyp_path, PR, USE.NAMES = FALSE)
  df = df %>% filter(DC == "DC")
  input_hy = df %>% group_by(subject, visit) %>% count() %>% dplyr::select(1:2)
  total_df = lapply(1:nrow(input_hy), subject_com_df, input = input_hy, df = df) %>% bind_rows()
  
  ##MDA_path = list.files("./Overnight data streams/All subjects", pattern = "MDA", recursive = TRUE, full.names = TRUE)
  MDA_path = path[which(grepl("MDA", path))]
  subject = sapply(MDA_path, info_get, info = "subject", USE.NAMES = FALSE)
  visit = sapply(MDA_path, info_get, info = "visit", USE.NAMES = FALSE)
  date = sapply(MDA_path, info_get, info = "date", USE.NAMES = FALSE)
  
  df_mda = data.frame(cbind(subject, visit, date, MDA_path))
  df_mda$DC = sapply(df_mda$MDA_path, drift_correction, USE.NAMES = FALSE)
  df_mda$PR = sapply(df_mda$MDA_path, PR, USE.NAMES = FALSE)
  df_mda = df_mda %>% filter(DC == "DC")
  input_mda = df_mda %>% group_by(subject, visit) %>% count() %>% dplyr::select(1:2)
  
  total_df_mda = lapply(1:nrow(input_mda), subject_com_df, input = input_mda, df = df_mda) %>% bind_rows()
  
  total_df = rbind(total_df, total_df_mda)
  write_csv(total_df, paste0("./data/raw_data/raw_total_df/overnight_data_all_subjects_", day, ".csv"))
  return(total_df)
}

total_df = total_df_gen(new_files, "20240311")

## Check missing data
missing_summary_gen = function(total_df, day){
  input = total_df %>% group_by(file, subject, visit, day) %>% summarize(count = n()) %>% dplyr::select(file, subject, visit, day) %>% ungroup()
  
  input[["PCO2_status"]] = sapply(1:nrow(input), function(i) missing_check(df = total_df, s = input[[i, "subject"]], d = input[[i, "day"]], m = "PCO2_DC", type = "status"), USE.NAMES = FALSE)
  input[["SpO2_status"]] = sapply(1:nrow(input), function(i) missing_check(df = total_df, s = input[[i, "subject"]], d = input[[i, "day"]], m = "SpO2", type = "status"), USE.NAMES = FALSE)
  input[["PR_status"]] = sapply(1:nrow(input), function(i) missing_check(df = total_df, s = input[[i, "subject"]], d = input[[i, "day"]], m = "PR", type = "status"), USE.NAMES = FALSE)
  input[["PCO2_miss_n"]] = sapply(1:nrow(input), function(i) missing_check(df = total_df, s = input[[i, "subject"]], d = input[[i, "day"]], m = "PCO2_DC", type = "times"), USE.NAMES = FALSE)
  input[["SpO2_miss_n"]] = sapply(1:nrow(input), function(i) missing_check(df = total_df, s = input[[i, "subject"]], d = input[[i, "day"]], m = "SpO2", type = "times"), USE.NAMES = FALSE)
  input[["PR_miss_n"]] = sapply(1:nrow(input), function(i) missing_check(df = total_df, s = input[[i, "subject"]], d = input[[i, "day"]], m = "PR", type = "times"), USE.NAMES = FALSE)
  write_csv(input, paste0("./data/missing/missing_summary_", day, ".csv"))
  return(input)
}
missing_summary_df = missing_summary_gen(total_df = total_df, day = "20240311")

  


