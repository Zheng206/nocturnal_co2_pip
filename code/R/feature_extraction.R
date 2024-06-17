library(wavethresh)
library(parallel)
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
setwd("/home/zhengren/Desktop/Project/nocturnal_co2_pip")
source("/home/zhengren/Desktop/Project/nocturnal_co2_pip/code/R/co2_function.R")

# Data Management
total_df = read_csv("./data/raw_total_df/overnight_data_all_subjects_20240311.csv")

# Subject Summary Info
summary_info = read_excel("./data/raw_MDA/MDA_DATA_FINAL.xls")
summary_info = summary_info %>% dplyr::select("PDF_Filename", "date_visit", "fvc", "mip", "visit_age", "height", "weight", "spot_co2",	"spot_o2",	"spot_hr", "alsfrs_10", "alsfrs_11", "alsfrs_total", "date_onset") %>% filter(grepl("HYP|MDA", PDF_Filename))
colnames(summary_info) = c("filename", "period", "fvc", "mip", "visit_age", "height", "weight", "spot_co2",	"spot_o2",	"spot_hr", "alsfrs_10", "alsfrs_11", "alsfrs_total", "date_onset")

summary_info$subject = sapply(summary_info$filename, function(x) {if(grepl("HYP", x)){str_split(x,"--")[[1]][1]}else{
  item = str_split(x,"_")[[1]]
  upenn = item[which(grepl("UPENN", item))]
  upenn = str_sub(upenn, start = 1, end = 7)
  return(upenn)
  }}, USE.NAMES = FALSE)
summary_info$day  = as.character(summary_info$period)

other_info = summary_info %>% dplyr::select(subject, day, fvc, mip, spot_co2, alsfrs_10, alsfrs_11, alsfrs_total) %>% na.omit()
other_info$visit = sapply(other_info$day, function(x) str_split(x, "-")[[1]][2], USE.NAMES = FALSE)

# Raw Feature Extraction
## readin data
miss_summary = read_csv("./data/missing/missing_summary_20240311.csv")
non_miss = miss_summary %>% filter(PCO2_status == "No") %>% dplyr::select(subject, day)
non_miss_df = lapply(1:nrow(non_miss), function(i){
  sub_df = total_df %>% filter(subject == non_miss[[i, "subject"]], day == non_miss[[i, "day"]])
  return(sub_df)
}) %>% bind_rows()
non_miss_df$day = as.character(non_miss_df$day)
#write_csv(non_miss_df, "./data/qc_data/non_missing_data/non_missing_df_20231206.csv")

#non_miss_summary = subject_summary_gen(df = non_miss_df, summary_df = other_info)
#write_csv(non_miss_summary, "./data/data_management/non_missing_summary_sen.csv")

missing_inpute = read_csv("./data/missing/inpute_missing_data_20240311.csv")
missing_inpute$day = as.character(missing_inpute$day)
total_subject = rbind(non_miss_df, missing_inpute)
write_csv(total_subject, "./data/processed_data/total_df_20240311.csv")
subject_summary = subject_summary_gen(total_subject, other_info)
write_csv(subject_summary, "./data/processed_data/subject_summary_20240311.csv")


# Wavelet Feature Extraction

# Read-in data
total_subject_df = total_subject
total_subject_df$day = as.character(total_subject_df$day)
total_subject_input = total_subject_df %>% group_by(subject, day) %>% summarize(count = n()) %>% ungroup()
total_subject_input$power = sapply(total_subject_input$count, function(x) floor(log2(x)), USE.NAMES = FALSE)
total_subject_input = total_subject_input %>% filter(power >= 13)

feature_df = lapply(seq_len(nrow(total_subject_input)), function(i){
    feature_extraction(total_subject_df, total_subject_input, i)
}) %>% bind_rows()

write_csv(feature_df, "./data/processed_data/extracted_wavelet_features_20240311.csv")

# Frequency Conversion
feature_input = feature_df %>% dplyr::select(subject, day)
n_level = seq(1,13,1)

for (n in n_level){
  cols = sapply(1:nrow(feature_input), function(i){ 
    a = frequency_conversion(total_df, feature_input[[i, "subject"]], feature_input[[i, "day"]], n)
    return(a)}, USE.NAMES = FALSE)
  feature_input = cbind(feature_input, cols)
}
colnames(feature_input) = c("subject", "day", paste0("level_", n_level))
write_csv(feature_input, "./data/processed_data/frequency_table_20240311.csv")

# Period Conversion
n_level = seq(1,9,1)

for (n in n_level){
  cols = sapply(1:nrow(feature_input), function(i){ 
    a = period_conversion(total_df, feature_input[[i, "subject"]], feature_input[[i, "day"]], n)
    return(a)}, USE.NAMES = FALSE)
  feature_input = cbind(feature_input, cols)
}
colnames(feature_input) = c("subject", "day", paste0("level_", n_level))
write_csv(feature_input, "./data/processed_data/period_table.csv")

period = sapply(n_level, function(x) names(sort(table(feature_input[paste0("level_", x)]), decreasing = TRUE))[1], USE.NAMES = FALSE)
period_table = data.frame(cbind(n_level, period))
write_csv(period_table, "./data/processed_data/period_list.csv")
