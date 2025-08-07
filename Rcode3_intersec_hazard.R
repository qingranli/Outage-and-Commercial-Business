# This R code generates regression results for the heterogeneity analysis,
# with SAIDI x climate hazard rating category 
code_dir = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(code_dir)
getwd()

library(tidyverse)
library(data.table)
library(tigris)
library(sf)
library(fastDummies)
library(fixest) # fixed-effect regression
library(ggpubr) 

library(modelsummary) # print regression table to word
library(flextable)

rm(list=ls())
gc()

# import combined spend-outage data =================================
# Note: A 1% data sample is provided just for completeness of the code, 
#      not for replication of the results. See "Data Availability" statement 
#      for how to get the data for this study.
dt_sample = readRDS("Data_combined_county_sample.rds")
dt1 = dt_sample

# import NRI Rating categories 
dt_NRI = readRDS("County_NRI_EAL_Rating.rds")
summary(dt_NRI)

Hazard_Type = c("Heat Wave", "Cold Wave", "Winter Weather",
                "Ice Storm", "Wildfire", "Hurricane",
                "Tornado", "Strong Wind", "Drought", "Hail",
                "Riverine Flooding", "Coastal Flooding")

dt_NRI <- dt_NRI %>% filter(data_type %in% Hazard_Type) %>% 
  filter(!is.na(EAL_SCORE), EAL_SCORE > 0) %>% 
  filter(fips_code %in% unique(dt1$fips_code))

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
# Regression: GLM (log link)
#####################################################################
fe_list = c("fips_code", "local_date", "county_week") # fixed effects

resList = list() # save regression results

for (n in 1:length(Hazard_Type)) {
  dt_attr = dt_NRI %>% filter(data_type == Hazard_Type[n]) %>% 
    select(fips_code, EAL_RATNG, RT)
  print(Hazard_Type[n])
  
  dt = left_join(dt1, dt_attr, by = "fips_code")
  dt <- dt %>% filter(!is.na(RT)) # remove NA Rating
  
  dt <- dummy_cols(dt, select_columns = "RT")
  RatingBin = c("RT_VeryHigh","RT_RelHigh","RT_Moderate","RT_RelLow")
  
  
  f_res = paste0("Daily_Spend ~ SAIDI + ", 
                 paste(paste("SAIDI",RatingBin,sep = ":"), collapse = "+"),
                 "|", paste(fe_list, collapse = "+"))
  f_res = f_res %>% str_replace_all(" ","") %>% as.formula()
  
  res_RTNG = feglm(f_res, 
                  data = dt %>% filter(Daily_Spend > 0),
                  family = Gamma(link = "log"), 
                  cluster=~fips_code,
                  glm.iter = 100)
  resList[[n]] = res_RTNG # save result
}

etable(resList)


#####################################################################
# plot coefficient of intersection terms & 95% CI
#####################################################################
plotList = list()
for (n in 1:length(Hazard_Type)) {
  res_save = resList[[n]]
  res.tab = as.data.table(res_save$coeftable) %>% 
    mutate(var_names = rownames(res_save$coeftable))
  colnames(res.tab)[1:4] <- c("est", "se", "t_val", "p_val")
  
  res.tab = res.tab %>% filter(var_names != "SAIDI") %>% 
    mutate(RT = str_remove(var_names, "^SAIDI:RT_"))
  
  res.tab <- res.tab %>% mutate(
    EAL_RATNG = case_when(
      (RT == "VeryHigh") ~ "Very High",
      (RT == "RelHigh") ~ "Relatively High",
      (RT == "Moderate") ~ "Relatively Moderate",
      (RT == "RelLow") ~ "Relatively Low"
    )
  )
  
  res.tab$EAL_RATNG = factor(res.tab$EAL_RATNG, levels = rating_levels)
  
  plotList[[n]] = ggplot(res.tab, aes(x=EAL_RATNG)) + 
    geom_hline(yintercept = 0, lty = "dashed") +
    geom_errorbar(aes(ymin = est-1.96*se, ymax = est+1.96*se), width = 0.3) +
    geom_point(aes(y=est)) + 
    labs(x = "", y = "", title = Hazard_Type[n]) +
    coord_flip() +
    theme_light() + theme(panel.grid = element_blank(), 
                          text = element_text(size = 10))
}

ggarrange(plotlist = plotList, nrow = 4, ncol = 3,
          labels = c("a", "b", "c", "d", "e", "f",
                     "g", "h", "i", "j", "k", "l"),
          label.y = 1, label.x = 0.01)


# ggsave("fig_saved/fig_result5_EAL_Rating.jpg", 
#        dpi = 300, width = 11, height = 6)
