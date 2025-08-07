# Simulate Sales ($ per year) under Scenario 2 & 3
# save: "Sales_simulate_scn2/3.rds"
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

# import socioeconomic deciles
county_attr = readRDS("county_socioecon_deciles.rds")
county_attr <- county_attr %>% select(fips_code, income_decile, share_minority_decile)


#####################################################################
# ==================== GLM (log link) Estimation ===================#
#####################################################################
fe_list = c("fips_code", "local_date", "county_week")

# Regression 1: x Income Decile -------------------------------------
dt_attr = county_attr[,c(1,2)]
print(colnames(dt_attr)[2])
colnames(dt_attr)[2] = "var_ptl"

dt1 = left_join(dt, dt_attr, by = "fips_code")
dt1 <- dummy_cols(dt1, select_columns = "var_ptl")
ptlbin = c("var_ptl_1", "var_ptl_2", "var_ptl_3", "var_ptl_4",
           "var_ptl_6", "var_ptl_7", "var_ptl_8", "var_ptl_9", "var_ptl_10")

f_res = paste0("Daily_Spend ~ SAIDI + ", 
               paste(paste("SAIDI",ptlbin,sep = ":"), collapse = "+"),
               "|", paste(fe_list, collapse = "+"))
f_res = f_res %>% str_replace_all(" ","") %>% as.formula()
print(f_res)

res_Income = feglm(f_res, 
                data = dt1 %>% filter(Daily_Spend > 0),
                family = Gamma(link = "log"), 
                cluster=~fips_code,
                glm.iter = 100)

# add fitted values of sales from the model 
dt1$Daily_Spend_fit = predict(res_Income, newdata = dt1, type = "response")

# Regression 2: x Minority Decile -------------------------------------
dt_attr = county_attr[,c(1,3)]
print(colnames(dt_attr)[2])
colnames(dt_attr)[2] = "var_ptl"

dt2 = left_join(dt, dt_attr, by = "fips_code")
dt2 <- dummy_cols(dt2, select_columns = "var_ptl")
ptlbin = c("var_ptl_1", "var_ptl_2", "var_ptl_3", "var_ptl_4",
           "var_ptl_6", "var_ptl_7", "var_ptl_8", "var_ptl_9", "var_ptl_10")

f_res = paste0("Daily_Spend ~ SAIDI + ", 
               paste(paste("SAIDI",ptlbin,sep = ":"), collapse = "+"),
               "|", paste(fe_list, collapse = "+"))
f_res = f_res %>% str_replace_all(" ","") %>% as.formula()
print(f_res)

res_Minority = feglm(f_res, 
                   data = dt2 %>% filter(Daily_Spend > 0),
                   family = Gamma(link = "log"), 
                   cluster=~fips_code,
                   glm.iter = 100)

# add fitted values of sales from the model 
dt2$Daily_Spend_fit = predict(res_Minority, newdata = dt2, type = "response")

#####################################################################
# =================== Scenario Development: 2A =====================#
#####################################################################
# select candidate counties
candidate_fips = dt_county$fips_code[dt_county$scn2A == 1]

maxItr = 200 # set max iteration number
sim_output_list = list()
set.seed(147)
for (n in 1:maxItr) {
  # randomly select 100 counties from the candidates
  select_fips = sample(candidate_fips, size = 100, replace = FALSE)
  
  # reduce daily SAIDI by 5% for the selected counties
  dt_new = dt1 %>% 
    rename(true_SAIDI = SAIDI) %>% 
    mutate(SAIDI = ifelse(fips_code %in% select_fips,
                          0.95*true_SAIDI, true_SAIDI))
  
  # predict Sales based on new SAIDI values 
  dt_new$iter_num = n
  dt_new$Spend_hat <- predict(res_Income, newdata = dt_new, type = "response")
  
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

sim_output_2A = rbindlist(sim_output_list) %>% mutate(scenario = "2A")
rm(sim_output_list, candidate_fips)
#####################################################################
# =================== Scenario Development: 2B =====================#
#####################################################################
# select candidate counties
candidate_fips = dt_county$fips_code[dt_county$scn2B == 1]

maxItr = 200 # set max iteration number
sim_output_list = list()
set.seed(147)
for (n in 1:maxItr) {
  # randomly select 100 counties from the candidates
  select_fips = sample(candidate_fips, size = 100, replace = FALSE)
  
  # reduce daily SAIDI by 5% for the selected counties
  dt_new = dt1 %>% 
    rename(true_SAIDI = SAIDI) %>% 
    mutate(SAIDI = ifelse(fips_code %in% select_fips,
                          0.95*true_SAIDI, true_SAIDI))
  
  # predict Sales based on new SAIDI values 
  dt_new$iter_num = n
  dt_new$Spend_hat <- predict(res_Income, newdata = dt_new, type = "response")
  
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


sim_output_2B = rbindlist(sim_output_list) %>% mutate(scenario = "2B")
rm(sim_output_list, candidate_fips)
#####################################################################
# =============== Scenario Development: 2 Random ===================#
#####################################################################
# select candidate counties
candidate_fips = dt_county$fips_code

maxItr = 200 # set max iteration number
sim_output_list = list()
set.seed(147)
for (n in 1:maxItr) {
  # randomly select 100 counties from the candidates
  select_fips = sample(candidate_fips, size = 100, replace = FALSE)
  
  # reduce daily SAIDI by 5% for the selected counties
  dt_new = dt1 %>% 
    rename(true_SAIDI = SAIDI) %>% 
    mutate(SAIDI = ifelse(fips_code %in% select_fips,
                          0.95*true_SAIDI, true_SAIDI))
  
  # predict Sales based on new SAIDI values 
  dt_new$iter_num = n
  dt_new$Spend_hat <- predict(res_Income, newdata = dt_new, type = "response")
  
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


sim_output_2Rand = rbindlist(sim_output_list) %>% mutate(scenario = "2Random")
rm(sim_output_list, candidate_fips)
# save simulated sales ($ per year) ----------------------
scn2_output = rbind(sim_output_2A, sim_output_2B, sim_output_2Rand)
saveRDS(scn2_output, "Sales_simulate_scn2.rds")

summary(scn2_output)
rm(sim_output_2A, sim_output_2B, dt1)
#####################################################################
# =================== Scenario Development: 3A =====================#
#####################################################################
# select candidate counties
candidate_fips = dt_county$fips_code[dt_county$scn3A == 1]

maxItr = 200 # set max iteration number
sim_output_list = list()
set.seed(147)
for (n in 1:maxItr) {
  # randomly select 100 counties from the candidates
  select_fips = sample(candidate_fips, size = 100, replace = FALSE)
  
  # reduce daily SAIDI by 5% for the selected counties
  dt_new = dt2 %>% 
    rename(true_SAIDI = SAIDI) %>% 
    mutate(SAIDI = ifelse(fips_code %in% select_fips,
                          0.95*true_SAIDI, true_SAIDI))
  
  # predict Sales based on new SAIDI values 
  dt_new$iter_num = n
  dt_new$Spend_hat <- predict(res_Minority, newdata = dt_new, type = "response")
  
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


sim_output_3A = rbindlist(sim_output_list) %>% mutate(scenario = "3A")
rm(sim_output_list, candidate_fips)
#####################################################################
# =================== Scenario Development: 3B =====================#
#####################################################################
# select candidate counties
candidate_fips = dt_county$fips_code[dt_county$scn3B == 1]

maxItr = 200 # set max iteration number
sim_output_list = list()
set.seed(147)
for (n in 1:maxItr) {
  # randomly select 100 counties from the candidates
  select_fips = sample(candidate_fips, size = 100, replace = FALSE)
  
  # reduce daily SAIDI by 5% for the selected counties
  dt_new = dt2 %>% 
    rename(true_SAIDI = SAIDI) %>% 
    mutate(SAIDI = ifelse(fips_code %in% select_fips,
                          0.95*true_SAIDI, true_SAIDI))
  
  # predict Sales based on new SAIDI values 
  dt_new$iter_num = n
  dt_new$Spend_hat <- predict(res_Minority, newdata = dt_new, type = "response")
  
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


sim_output_3B = rbindlist(sim_output_list) %>% mutate(scenario = "3B")
rm(sim_output_list, candidate_fips)
#####################################################################
# ============= Scenario Development: 3 Random =====================#
#####################################################################
# select candidate counties
candidate_fips = dt_county$fips_code

maxItr = 200 # set max iteration number
sim_output_list = list()
set.seed(147)
for (n in 1:maxItr) {
  # randomly select 100 counties from the candidates
  select_fips = sample(candidate_fips, size = 100, replace = FALSE)
  
  # reduce daily SAIDI by 5% for the selected counties
  dt_new = dt2 %>% 
    rename(true_SAIDI = SAIDI) %>% 
    mutate(SAIDI = ifelse(fips_code %in% select_fips,
                          0.95*true_SAIDI, true_SAIDI))
  
  # predict Sales based on new SAIDI values 
  dt_new$iter_num = n
  dt_new$Spend_hat <- predict(res_Minority, newdata = dt_new, type = "response")
  
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


sim_output_3Rand = rbindlist(sim_output_list) %>% mutate(scenario = "3Random")
rm(sim_output_list, candidate_fips)
# save simulated sales ($ per year) ----------------------
scn3_output = rbind(sim_output_3A, sim_output_3B, sim_output_3Rand)
saveRDS(scn3_output, "Sales_simulate_scn3.rds")
# summary(scn3_output)
