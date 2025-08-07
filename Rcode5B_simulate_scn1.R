# Simulate Sales ($ per year) under Scenario 1
# save: "Sales_simulate_scn1.rds"
code_dir = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(code_dir)
getwd()

library(tidyverse)
library(data.table)
library(plm) # panel data
library(fastDummies)
library(fixest) # fixed-effect regression
library(ggpubr) # plot
library(beepr)

rm(list=ls())
gc()

# import RDS file from running Rcode5A
dt_county = readRDS("County_Selection_Panel.rds")

# import combined spend-outage data =================================
# Note: A 1% data sample is provided just for completeness of the code, 
#      not for replication of the results. See "Data Availability" statement 
#      for how to get the data for this study.
dt_sample = readRDS("Data_combined_county_sample.rds")
dt = dt_sample



#####################################################################
# ==================== GLM (log link) Estimation ===================#
#####################################################################
fe_list = c("fips_code", "local_date", "county_week")
# create outage bins
dt <- dt %>% mutate(outBin = case_when(
  (SAIDI <= 0) ~ "0",
  (SAIDI >0 & SAIDI <= 2) ~ "2",
  (SAIDI >2 & SAIDI <= 4) ~ "4",
  (SAIDI >4 & SAIDI <= 6) ~ "6",
  (SAIDI >6 & SAIDI <= 8) ~ "8",
  .default = "9"
))
dt <- dummy_cols(dt, select_columns = "outBin")
outbin = c("outBin_2", "outBin_4","outBin_6","outBin_8","outBin_9")

f_res = paste0("Daily_Spend ~ ", 
               paste(outbin, collapse = "+"),
               "|", paste(fe_list, collapse = "+"))
f_res = f_res %>% str_replace_all(" ","") %>% as.formula()
print(f_res)
res_bin = feglm(f_res, 
                data = dt %>% filter(Daily_Spend > 0),
                family = Gamma(link = "log"), 
                cluster=~fips_code,
                glm.iter = 100)

# add fitted values of sales from the model ------------------------
dt$Daily_Spend_fit = predict(res_bin, newdata = dt, type = "response")

#####################################################################
# =================== Scenario Development: 1A =====================#
#####################################################################
# select candidate counties
candidate_fips = dt_county$fips_code[dt_county$scn1A == 1]

maxItr = 200 # set max iteration number
sim_output_list = list()
set.seed(147)
for (n in 1:maxItr) {
  # randomly select 100 counties from the candidates
  select_fips = sample(candidate_fips, size = 100, replace = FALSE)
  
  # reduce daily SAIDI by 5% for the selected counties
  dt_new = dt %>% 
    select(fips_code, local_date, Daily_Spend_fit, SAIDI,
           county_week, data_year) %>% 
    rename(true_SAIDI = SAIDI) %>% 
    mutate(SAIDI = ifelse(fips_code %in% select_fips,
                          0.95*true_SAIDI, true_SAIDI))
  
  # create outage bins based on new SAIDI values
  dt_new <- dt_new %>% mutate(outBin = case_when(
    (SAIDI <= 0) ~ "0",
    (SAIDI >0 & SAIDI <= 2) ~ "2",
    (SAIDI >2 & SAIDI <= 4) ~ "4",
    (SAIDI >4 & SAIDI <= 6) ~ "6",
    (SAIDI >6 & SAIDI <= 8) ~ "8",
    .default = "9"
  ))
  dt_new$outBin = factor(dt_new$outBin, levels = c("0","2","4","6","8","9"))
  dt_new <- dummy_cols(dt_new, select_columns = "outBin")
  
  # predict Sales based on new SAIDI values 
  dt_new$iter_num = n
  dt_new$Spend_hat <- predict(res_bin, newdata = dt_new, type = "response")
  
  # compute average annual sales for each county 
  sales_sim = dt_new %>% 
    group_by(fips_code, data_year, iter_num) %>% 
    summarise(Spend_null = sum(Daily_Spend_fit, na.rm = TRUE),
              Spend_hat = sum(Spend_hat, na.rm = TRUE), 
              .groups = "drop")
  
  sim_output_list[[n]] = sales_sim 
  cat("\rFinished", n, "of", maxItr, "iterations")
  
  rm(dt_new, select_fips, sales_sim)
}


sim_output_1A = rbindlist(sim_output_list)
sim_output_1A <- sim_output_1A %>% mutate(scenario = "1A")
rm(sim_output_list, candidate_fips)
#####################################################################
# =================== Scenario Development: 1B =====================#
#####################################################################
# select candidate counties
candidate_fips = dt_county$fips_code[dt_county$scn1B == 1]

maxItr = 200 # set max iteration number
sim_output_list = list()
set.seed(147)
for (n in 1:maxItr) {
  # randomly select 100 counties from the candidates
  select_fips = sample(candidate_fips, size = 100, replace = FALSE)
  
  # reduce daily SAIDI by 5% for the selected counties
  dt_new = dt %>% 
    select(fips_code, local_date, Daily_Spend_fit, SAIDI,
           county_week, data_year) %>% 
    rename(true_SAIDI = SAIDI) %>% 
    mutate(SAIDI = ifelse(fips_code %in% select_fips,
                          0.95*true_SAIDI, true_SAIDI))
  
  # create outage bins based on new SAIDI values
  dt_new <- dt_new %>% mutate(outBin = case_when(
    (SAIDI <= 0) ~ "0",
    (SAIDI >0 & SAIDI <= 2) ~ "2",
    (SAIDI >2 & SAIDI <= 4) ~ "4",
    (SAIDI >4 & SAIDI <= 6) ~ "6",
    (SAIDI >6 & SAIDI <= 8) ~ "8",
    .default = "9"
  ))
  dt_new$outBin = factor(dt_new$outBin, levels = c("0","2","4","6","8","9"))
  dt_new <- dummy_cols(dt_new, select_columns = "outBin")
  
  # predict Sales based on new SAIDI values 
  dt_new$iter_num = n
  dt_new$Spend_hat <- predict(res_bin, newdata = dt_new, type = "response")
  
  # compute average annual sales for each county 
  sales_sim = dt_new %>% 
    group_by(fips_code, data_year, iter_num) %>% 
    summarise(Spend_null = sum(Daily_Spend_fit, na.rm = TRUE),
              Spend_hat = sum(Spend_hat, na.rm = TRUE), 
              .groups = "drop")
  
  sim_output_list[[n]] = sales_sim 
  cat("\rFinished", n, "of", maxItr, "iterations")
  
  rm(dt_new, select_fips, sales_sim)
}

sim_output_1B = rbindlist(sim_output_list) %>% mutate(scenario = "1B")
rm(sim_output_list, candidate_fips)

#####################################################################
# =============== Scenario Development: Random =====================#
#####################################################################
# use all counties as candidate counties
candidate_fips = dt_county$fips_code

maxItr = 200 # set max iteration number
sim_output_list = list()
set.seed(147)
for (n in 1:maxItr) {
  # randomly select 100 counties from the candidates
  select_fips = sample(candidate_fips, size = 100, replace = FALSE)
  
  # reduce daily SAIDI by 5% for the selected counties
  dt_new = dt %>% 
    select(fips_code, local_date, Daily_Spend_fit, SAIDI,
           county_week, data_year) %>% 
    rename(true_SAIDI = SAIDI) %>% 
    mutate(SAIDI = ifelse(fips_code %in% select_fips,
                          0.95*true_SAIDI, true_SAIDI))
  
  # create outage bins based on new SAIDI values
  dt_new <- dt_new %>% mutate(outBin = case_when(
    (SAIDI <= 0) ~ "0",
    (SAIDI >0 & SAIDI <= 2) ~ "2",
    (SAIDI >2 & SAIDI <= 4) ~ "4",
    (SAIDI >4 & SAIDI <= 6) ~ "6",
    (SAIDI >6 & SAIDI <= 8) ~ "8",
    .default = "9"
  ))
  dt_new$outBin = factor(dt_new$outBin, levels = c("0","2","4","6","8","9"))
  dt_new <- dummy_cols(dt_new, select_columns = "outBin")
  
  # predict Sales based on new SAIDI values 
  dt_new$iter_num = n
  dt_new$Spend_hat <- predict(res_bin, newdata = dt_new, type = "response")
  
  # compute average annual sales for each county 
  sales_sim = dt_new %>% 
    group_by(fips_code, data_year, iter_num) %>% 
    summarise(Spend_null = sum(Daily_Spend_fit, na.rm = TRUE),
              Spend_hat = sum(Spend_hat, na.rm = TRUE), 
              .groups = "drop")
  
  sim_output_list[[n]] = sales_sim 
  cat("\rFinished", n, "of", maxItr, "iterations")
  
  rm(dt_new, select_fips, sales_sim)
}


sim_output_1Rand = rbindlist(sim_output_list) %>% mutate(scenario = "1Random")
rm(sim_output_list, candidate_fips)

# save simulated sales ($ per year) ----------------------
scn1_output = rbind(sim_output_1A, sim_output_1B, sim_output_1Rand)
saveRDS(scn1_output, "Sales_simulate_scn1.rds")
# summary(scn1_output)
