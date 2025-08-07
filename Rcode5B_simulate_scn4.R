# Simulate Sales ($thousand per year) under Scenario 4
# save: "Sales_simulate_scn4*.rds" 
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

# import NRI Rating categories 
dt_NRI = readRDS("County_NRI_EAL_Rating.rds")
# summary(dt_NRI)

Hazard_Type = c("Heat Wave", "Winter Weather",
                "Wildfire", "Hurricane",
                "Tornado", "Hail",
                "Riverine Flooding")

dt_NRI <- dt_NRI %>% filter(data_type %in% Hazard_Type) %>% 
  filter(!is.na(EAL_SCORE), EAL_SCORE > 0) %>% 
  filter(fips_code %in% unique(dt$fips_code))

rating_levels = c("Very High", "Relatively High", "Relatively Moderate",
                  "Relatively Low", "Very Low")
rt_levels = c("VeryHigh", "RelHigh", "Moderate", "RelLow", "VeryLow")

# define short rating name ********
dt_NRI <- dt_NRI %>% mutate(
  RT = case_when(
    (EAL_RATNG == "Very High") ~ "VeryHigh",
    (EAL_RATNG == "Relatively High") ~ "RelHigh",
    (EAL_RATNG == "Relatively Moderate") ~ "Moderate",
    (EAL_RATNG == "Relatively Low") ~ "RelLow",
    .default = "VeryLow"
  )
)

dt_NRI$EAL_RATNG = factor(dt_NRI$EAL_RATNG, levels = rating_levels)
dt_NRI$RT = factor(dt_NRI$RT, levels = rt_levels)

#####################################################################
# ==================== GLM (log link) Estimation ===================#
#####################################################################
fe_list = c("fips_code", "local_date", "county_week")

# m = 1 # Select regression to run -----------------------------------
for (m in 1:7) {
  print(Hazard_Type[m])
  
  dt_attr = dt_NRI %>% filter(data_type == Hazard_Type[m]) %>% 
    select(fips_code, EAL_RATNG, RT)

  dt_run = left_join(dt, dt_attr, by = "fips_code")
  dt_run <- dt_run %>% filter(!is.na(RT)) # remove NA Rating
  dt_run <- dummy_cols(dt_run, select_columns = "RT")
  RatingBin = c("RT_VeryHigh","RT_RelHigh","RT_Moderate","RT_RelLow")
  
  f_res = paste0("Daily_Spend ~ SAIDI + ", 
               paste(paste("SAIDI",RatingBin,sep = ":"), collapse = "+"),
               "|", paste(fe_list, collapse = "+"))
  f_res = f_res %>% str_replace_all(" ","") %>% as.formula()
  
  res_RTNG = feglm(f_res, 
                 data = dt_run %>% filter(Daily_Spend > 0),
                 family = Gamma(link = "log"), 
                 cluster=~fips_code,
                 glm.iter = 100)
  
  dt_run$Daily_Spend_fit = predict(res_RTNG, newdata = dt_run, type = "response")
  
  
#####################################################################
# ================= Scenario Development: 4A =======================#
#####################################################################
if (m == 1) {
  # select candidate counties
  candidate_fips = dt_county$fips_code[dt_county$scn4A == 1]
  
  maxItr = 200 # set max iteration number
  sim_output_list = list()
  set.seed(147)
  for (n in 1:maxItr) {
    # randomly select 100 counties from the candidates
    select_fips = sample(candidate_fips, size = 100, replace = FALSE)
    
    # reduce daily SAIDI by 5% for the selected counties
    dt_new = dt_run %>% 
      rename(true_SAIDI = SAIDI) %>% 
      mutate(SAIDI = ifelse(fips_code %in% select_fips,
                            0.95*true_SAIDI, true_SAIDI))
    
    # predict Sales based on new SAIDI values 
    dt_new$iter_num = n
    dt_new$Spend_hat <- predict(res_RTNG, newdata = dt_new, type = "response")
    
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
  sim_output_4 = rbindlist(sim_output_list) %>% mutate(scenario = "4A")
  rm(sim_output_list, candidate_fips)
  
  # select counties randomly ----------------------------------------
  candidate_fips = dt_county$fips_code
  
  maxItr = 200 # set max iteration number
  sim_output_list = list()
  set.seed(147)
  for (n in 1:maxItr) {
    # randomly select 100 counties from the candidates
    select_fips = sample(candidate_fips, size = 100, replace = FALSE)
    
    # reduce daily SAIDI by 5% for the selected counties
    dt_new = dt_run %>% 
      rename(true_SAIDI = SAIDI) %>% 
      mutate(SAIDI = ifelse(fips_code %in% select_fips,
                            0.95*true_SAIDI, true_SAIDI))
    
    # predict Sales based on new SAIDI values 
    dt_new$iter_num = n
    dt_new$Spend_hat <- predict(res_RTNG, newdata = dt_new, type = "response")
    
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
  sim_output_4R = rbindlist(sim_output_list) %>% mutate(scenario = "4A_Random")
  rm(sim_output_list, candidate_fips)
  
  scn4_output = rbind(sim_output_4, sim_output_4R)
  saveRDS(scn4_output, "Sales_simulate_scn4A.rds")
  print("Simulation 4A completed.")
}

#####################################################################
# ================= Scenario Development: 4B =======================#
#####################################################################
if (m == 2) {
  # select candidate counties
  candidate_fips = dt_county$fips_code[dt_county$scn4B == 1]
  
  maxItr = 200 # set max iteration number
  sim_output_list = list()
  set.seed(147)
  for (n in 1:maxItr) {
    # randomly select 100 counties from the candidates
    select_fips = sample(candidate_fips, size = 100, replace = FALSE)
    
    # reduce daily SAIDI by 5% for the selected counties
    dt_new = dt_run %>% 
      rename(true_SAIDI = SAIDI) %>% 
      mutate(SAIDI = ifelse(fips_code %in% select_fips,
                            0.95*true_SAIDI, true_SAIDI))
    
    # predict Sales based on new SAIDI values 
    dt_new$iter_num = n
    dt_new$Spend_hat <- predict(res_RTNG, newdata = dt_new, type = "response")
    
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
  sim_output_4 = rbindlist(sim_output_list) %>% mutate(scenario = "4B")
  rm(sim_output_list, candidate_fips)
  
  # select counties randomly ----------------------------------------
  candidate_fips = dt_county$fips_code
  
  maxItr = 200 # set max iteration number
  sim_output_list = list()
  set.seed(147)
  for (n in 1:maxItr) {
    # randomly select 100 counties from the candidates
    select_fips = sample(candidate_fips, size = 100, replace = FALSE)
    
    # reduce daily SAIDI by 5% for the selected counties
    dt_new = dt_run %>% 
      rename(true_SAIDI = SAIDI) %>% 
      mutate(SAIDI = ifelse(fips_code %in% select_fips,
                            0.95*true_SAIDI, true_SAIDI))
    
    # predict Sales based on new SAIDI values 
    dt_new$iter_num = n
    dt_new$Spend_hat <- predict(res_RTNG, newdata = dt_new, type = "response")
    
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
  sim_output_4R = rbindlist(sim_output_list) %>% mutate(scenario = "4B_Random")
  rm(sim_output_list, candidate_fips)
  
  scn4_output = rbind(sim_output_4, sim_output_4R)
  saveRDS(scn4_output, "Sales_simulate_scn4B.rds")
  print("Simulation 4B completed.")
}

#####################################################################
# ================= Scenario Development: 4C =======================#
#####################################################################
if (m == 3) {
  # select candidate counties
  candidate_fips = dt_county$fips_code[dt_county$scn4C == 1]
  
  maxItr = 200 # set max iteration number
  sim_output_list = list()
  set.seed(147)
  for (n in 1:maxItr) {
    # randomly select 100 counties from the candidates
    select_fips = sample(candidate_fips, size = 100, replace = FALSE)
    
    # reduce daily SAIDI by 5% for the selected counties
    dt_new = dt_run %>% 
      rename(true_SAIDI = SAIDI) %>% 
      mutate(SAIDI = ifelse(fips_code %in% select_fips,
                            0.95*true_SAIDI, true_SAIDI))
    
    # predict Sales based on new SAIDI values 
    dt_new$iter_num = n
    dt_new$Spend_hat <- predict(res_RTNG, newdata = dt_new, type = "response")
    
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
  sim_output_4 = rbindlist(sim_output_list) %>% mutate(scenario = "4C")
  rm(sim_output_list, candidate_fips)
  
  # select counties randomly ----------------------------------------
  candidate_fips = dt_county$fips_code
  
  maxItr = 200 # set max iteration number
  sim_output_list = list()
  set.seed(147)
  for (n in 1:maxItr) {
    # randomly select 100 counties from the candidates
    select_fips = sample(candidate_fips, size = 100, replace = FALSE)
    
    # reduce daily SAIDI by 5% for the selected counties
    dt_new = dt_run %>% 
      rename(true_SAIDI = SAIDI) %>% 
      mutate(SAIDI = ifelse(fips_code %in% select_fips,
                            0.95*true_SAIDI, true_SAIDI))
    
    # predict Sales based on new SAIDI values 
    dt_new$iter_num = n
    dt_new$Spend_hat <- predict(res_RTNG, newdata = dt_new, type = "response")
    
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
  sim_output_4R = rbindlist(sim_output_list) %>% mutate(scenario = "4C_Random")
  rm(sim_output_list, candidate_fips)
  
  scn4_output = rbind(sim_output_4, sim_output_4R)
  saveRDS(scn4_output, "Sales_simulate_scn4C.rds")
  print("Simulation 4C completed.")
}

#####################################################################
# ================= Scenario Development: 4D =======================#
#####################################################################
if (m == 4) {
  # select candidate counties
  candidate_fips = dt_county$fips_code[dt_county$scn4D == 1]
  
  maxItr = 200 # set max iteration number
  sim_output_list = list()
  set.seed(147)
  for (n in 1:maxItr) {
    # randomly select 100 counties from the candidates
    select_fips = sample(candidate_fips, size = 100, replace = FALSE)
    
    # reduce daily SAIDI by 5% for the selected counties
    dt_new = dt_run %>% 
      rename(true_SAIDI = SAIDI) %>% 
      mutate(SAIDI = ifelse(fips_code %in% select_fips,
                            0.95*true_SAIDI, true_SAIDI))
    
    # predict Sales based on new SAIDI values 
    dt_new$iter_num = n
    dt_new$Spend_hat <- predict(res_RTNG, newdata = dt_new, type = "response")
    
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
  sim_output_4 = rbindlist(sim_output_list) %>% mutate(scenario = "4D")
  rm(sim_output_list, candidate_fips)
  
  # select counties randomly ----------------------------------------
  candidate_fips = dt_county$fips_code
  
  maxItr = 200 # set max iteration number
  sim_output_list = list()
  set.seed(147)
  for (n in 1:maxItr) {
    # randomly select 100 counties from the candidates
    select_fips = sample(candidate_fips, size = 100, replace = FALSE)
    
    # reduce daily SAIDI by 5% for the selected counties
    dt_new = dt_run %>% 
      rename(true_SAIDI = SAIDI) %>% 
      mutate(SAIDI = ifelse(fips_code %in% select_fips,
                            0.95*true_SAIDI, true_SAIDI))
    
    # predict Sales based on new SAIDI values 
    dt_new$iter_num = n
    dt_new$Spend_hat <- predict(res_RTNG, newdata = dt_new, type = "response")
    
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
  sim_output_4R = rbindlist(sim_output_list) %>% mutate(scenario = "4D_Random")
  rm(sim_output_list, candidate_fips)
  
  scn4_output = rbind(sim_output_4, sim_output_4R)
  saveRDS(scn4_output, "Sales_simulate_scn4D.rds")
  print("Simulation 4D completed.")
}

#####################################################################
# ================= Scenario Development: 4E =======================#
#####################################################################
if (m == 5) {
  # select candidate counties
  candidate_fips = dt_county$fips_code[dt_county$scn4E == 1]
  
  maxItr = 200 # set max iteration number
  sim_output_list = list()
  set.seed(147)
  for (n in 1:maxItr) {
    # randomly select 100 counties from the candidates
    select_fips = sample(candidate_fips, size = 100, replace = FALSE)
    
    # reduce daily SAIDI by 5% for the selected counties
    dt_new = dt_run %>% 
      rename(true_SAIDI = SAIDI) %>% 
      mutate(SAIDI = ifelse(fips_code %in% select_fips,
                            0.95*true_SAIDI, true_SAIDI))
    
    # predict Sales based on new SAIDI values 
    dt_new$iter_num = n
    dt_new$Spend_hat <- predict(res_RTNG, newdata = dt_new, type = "response")
    
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
  sim_output_4 = rbindlist(sim_output_list) %>% mutate(scenario = "4E")
  rm(sim_output_list, candidate_fips)
  
  # select counties randomly ----------------------------------------
  candidate_fips = dt_county$fips_code
  
  maxItr = 200 # set max iteration number
  sim_output_list = list()
  set.seed(147)
  for (n in 1:maxItr) {
    # randomly select 100 counties from the candidates
    select_fips = sample(candidate_fips, size = 100, replace = FALSE)
    
    # reduce daily SAIDI by 5% for the selected counties
    dt_new = dt_run %>% 
      rename(true_SAIDI = SAIDI) %>% 
      mutate(SAIDI = ifelse(fips_code %in% select_fips,
                            0.95*true_SAIDI, true_SAIDI))
    
    # predict Sales based on new SAIDI values 
    dt_new$iter_num = n
    dt_new$Spend_hat <- predict(res_RTNG, newdata = dt_new, type = "response")
    
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
  sim_output_4R = rbindlist(sim_output_list) %>% mutate(scenario = "4E_Random")
  rm(sim_output_list, candidate_fips)
  
  scn4_output = rbind(sim_output_4, sim_output_4R)
  saveRDS(scn4_output, "Sales_simulate_scn4E.rds")
  print("Simulation 4E completed.")
}

#####################################################################
# ================= Scenario Development: 4F =======================#
#####################################################################
if (m == 6) {
  # select candidate counties
  candidate_fips = dt_county$fips_code[dt_county$scn4F == 1]
  
  maxItr = 200 # set max iteration number
  sim_output_list = list()
  set.seed(147)
  for (n in 1:maxItr) {
    # randomly select 100 counties from the candidates
    select_fips = sample(candidate_fips, size = 100, replace = FALSE)
    
    # reduce daily SAIDI by 5% for the selected counties
    dt_new = dt_run %>% 
      rename(true_SAIDI = SAIDI) %>% 
      mutate(SAIDI = ifelse(fips_code %in% select_fips,
                            0.95*true_SAIDI, true_SAIDI))
    
    # predict Sales based on new SAIDI values 
    dt_new$iter_num = n
    dt_new$Spend_hat <- predict(res_RTNG, newdata = dt_new, type = "response")
    
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
  sim_output_4 = rbindlist(sim_output_list) %>% mutate(scenario = "4F")
  rm(sim_output_list, candidate_fips)
  
  # select counties randomly ----------------------------------------
  candidate_fips = dt_county$fips_code
  
  maxItr = 200 # set max iteration number
  sim_output_list = list()
  set.seed(147)
  for (n in 1:maxItr) {
    # randomly select 100 counties from the candidates
    select_fips = sample(candidate_fips, size = 100, replace = FALSE)
    
    # reduce daily SAIDI by 5% for the selected counties
    dt_new = dt_run %>% 
      rename(true_SAIDI = SAIDI) %>% 
      mutate(SAIDI = ifelse(fips_code %in% select_fips,
                            0.95*true_SAIDI, true_SAIDI))
    
    # predict Sales based on new SAIDI values 
    dt_new$iter_num = n
    dt_new$Spend_hat <- predict(res_RTNG, newdata = dt_new, type = "response")
    
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
  sim_output_4R = rbindlist(sim_output_list) %>% mutate(scenario = "4F_Random")
  rm(sim_output_list, candidate_fips)
  
  scn4_output = rbind(sim_output_4, sim_output_4R)
  saveRDS(scn4_output, "Sales_simulate_scn4F.rds")
  print("Simulation 4F completed.")
}

#####################################################################
# ================= Scenario Development: 4G =======================#
#####################################################################
if (m == 7) {
  # select candidate counties
  candidate_fips = dt_county$fips_code[dt_county$scn4G == 1]
  
  maxItr = 200 # set max iteration number
  sim_output_list = list()
  set.seed(147)
  for (n in 1:maxItr) {
    # randomly select 100 counties from the candidates
    select_fips = sample(candidate_fips, size = 100, replace = FALSE)
    
    # reduce daily SAIDI by 5% for the selected counties
    dt_new = dt_run %>% 
      rename(true_SAIDI = SAIDI) %>% 
      mutate(SAIDI = ifelse(fips_code %in% select_fips,
                            0.95*true_SAIDI, true_SAIDI))
    
    # predict Sales based on new SAIDI values 
    dt_new$iter_num = n
    dt_new$Spend_hat <- predict(res_RTNG, newdata = dt_new, type = "response")
    
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
  sim_output_4 = rbindlist(sim_output_list) %>% mutate(scenario = "4G")
  rm(sim_output_list, candidate_fips)
  
  # select counties randomly ----------------------------------------
  candidate_fips = dt_county$fips_code
  
  maxItr = 200 # set max iteration number
  sim_output_list = list()
  set.seed(147)
  for (n in 1:maxItr) {
    # randomly select 100 counties from the candidates
    select_fips = sample(candidate_fips, size = 100, replace = FALSE)
    
    # reduce daily SAIDI by 5% for the selected counties
    dt_new = dt_run %>% 
      rename(true_SAIDI = SAIDI) %>% 
      mutate(SAIDI = ifelse(fips_code %in% select_fips,
                            0.95*true_SAIDI, true_SAIDI))
    
    # predict Sales based on new SAIDI values 
    dt_new$iter_num = n
    dt_new$Spend_hat <- predict(res_RTNG, newdata = dt_new, type = "response")
    
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
  sim_output_4R = rbindlist(sim_output_list) %>% mutate(scenario = "4G_Random")
  rm(sim_output_list, candidate_fips)
  
  scn4_output = rbind(sim_output_4, sim_output_4R)
  saveRDS(scn4_output, "Sales_simulate_scn4G.rds")
  print("Simulation 4G completed.")
}

rm(dt_run, res_RTNG, dt_attr)
gc()
}
