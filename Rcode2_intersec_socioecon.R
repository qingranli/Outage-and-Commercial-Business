# This R code generates regression results for the heterogeneity analysis,
# with SAIDI x socioeconomic deciles
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

# import socioeconomic deciles
county_attr = readRDS("county_socioecon_deciles.rds")

#####################################################################
# Regression: GLM (log link)
#####################################################################
fe_list = c("fips_code", "local_date", "county_week") # fixed effects

resList = list() # save regression results

colnames(county_attr)[2:6]
# select attribute decile variable "var_ptl" ---------------------------
for (n in 2:6) {
  dt_attr = county_attr[,c(1,n)]
  print(colnames(dt_attr)[2])
  colnames(dt_attr)[2] = "var_ptl"
  
  dt = left_join(dt1, dt_attr, by = "fips_code")
  dt <- dummy_cols(dt, select_columns = "var_ptl")
  ptlbin = c("var_ptl_1", "var_ptl_2", "var_ptl_3", "var_ptl_4",
             "var_ptl_6", "var_ptl_7", "var_ptl_8", "var_ptl_9", "var_ptl_10")
  
  
  f_res = paste0("Daily_Spend ~ SAIDI + ", 
                 paste(paste("SAIDI",ptlbin,sep = ":"), collapse = "+"),
                 "|", paste(fe_list, collapse = "+"))
  f_res = f_res %>% str_replace_all(" ","") %>% as.formula()
  # print(f_res)
  
  res_ptl = feglm(f_res, 
                  data = dt %>% filter(Daily_Spend > 0),
                  family = Gamma(link = "log"), 
                  cluster=~fips_code,
                  glm.iter = 100)
  resList[[n-1]] = res_ptl # save result
}

etable(resList)


#####################################################################
# plot coefficient of intersection terms & 95% CI
#####################################################################
colnames(county_attr)[2:6]
xlabels = c("Population Density", 
            "Median Household Income",
            "Unemployment rate",
            "Share of Renter-Occupied Housing Units", 
            "Minority population share")

plotList = list()
for (n in 1:length(xlabels)) {
  res_save = resList[[n]]
  res.tab = as.data.table(res_save$coeftable) %>% 
    mutate(var_names = rownames(res_save$coeftable))
  colnames(res.tab)[1:4] <- c("est", "se", "t_val", "p_val")
  
  res.tab = res.tab %>% filter(var_names != "SAIDI") %>% 
    mutate(decile = as.numeric(str_remove(var_names, "^SAIDI:var_ptl_")))
  res.tab <- rbind(res.tab, data.table(est = 0, se=0, t_val = 0, p_val = 0,
                                       var_names = "SAIDI:var_ptl_5", decile = 5))
  
  plotList[[n]] = ggplot(res.tab %>% filter(var_names != "SAIDI"), aes(x=decile)) + 
    geom_hline(yintercept = 0, lty = "dashed") +
    geom_errorbar(aes(ymin = est-1.96*se, ymax = est+1.96*se), width = 0.3) +
    geom_point(aes(y=est)) + 
    # scale_y_continuous(breaks = seq(-0.04, 0.04, 0.02), 
    #                    limits = c(-0.04, 0.04)) +
    scale_x_continuous(breaks = 1:10) +
    labs(x = xlabels[n], y = "", title = "") +
    theme_light() + theme(panel.grid = element_blank(), 
                          text = element_text(size = 14))
}

ggarrange(plotlist = plotList, 
          labels = c("a", "b", "c", "d", "e"),
          label.y = 1, label.x = 0.01)

# ggsave("fig_saved/fig_result4_decile.jpg", 
#        dpi = 300, width = 14, height = 6)
