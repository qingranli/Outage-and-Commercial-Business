# This R code generates regression results for the baseline model with ...
# SAIDI, timing-dependent SAIDI, and SAIDI bins. And code for sectoral variation.
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
dt = dt_sample


#####################################################################
# Regression: GLM (log link)
#####################################################################
fe_list = c("fips_code", "local_date", "county_week") # fixed effects

resList = list() # save regression results

# Model 1: Sales ~ SAIDI ============================================
f_res = paste0("Daily_Spend ~ SAIDI ",
               "|", paste(fe_list, collapse = "+"))
f_res = f_res %>% str_replace_all(" ","") %>% as.formula()
res_main = feglm(f_res, 
                 data = dt %>% filter(Daily_Spend > 0),
                 family = Gamma(link = "log"), 
                 cluster=~fips_code,
                 glm.iter = 100)
resList[[1]] = res_main # save result to list

# Model 2: Sales ~ timing-dependent SAIDI ===========================
f_res = paste0("Daily_Spend ~ ",
               paste(c("SAIDI_morning",
                       "SAIDI_afternoon",
                       "SAIDI_evening",
                       "SAIDI_night"), collapse = "+"), 
               "|", paste(fe_list, collapse = "+"))
f_res = f_res %>% str_replace_all(" ","") %>% as.formula()
res_main = feglm(f_res, 
                 data = dt %>% filter(Daily_Spend > 0),
                 family = Gamma(link = "log"), 
                 cluster=~fips_code,
                 glm.iter = 100)
resList[[2]] = res_main # save result


# Model 3: Sales ~ SAIDI bins =======================================
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
res_main = feglm(f_res, 
                data = dt %>% filter(Daily_Spend > 0),
                family = Gamma(link = "log"), 
                cluster=~fips_code,
                glm.iter = 100)
resList[[3]] = res_main # save result

etable(resList)

# modelsummary(resList, fmt = 3, 
#              stars = c('*' = .1, '**' = .05, '***' = .01), 
#              output = "flextable") %>% 
#   save_as_docx(path = "save_reg_table.docx")

#####################################################################
# Plot regression results
#####################################################################
res_save1 = resList[[1]]
res_save2 = resList[[2]]

res.tab1 = as.data.table(res_save1$coeftable) %>% 
  mutate(var_names = rownames(res_save1$coeftable))
res.tab2 = as.data.table(res_save2$coeftable) %>% 
  mutate(var_names = rownames(res_save2$coeftable))
res.tab = rbind(res.tab1, res.tab2)
colnames(res.tab)[1:4] <- c("est", "se", "t_val", "p_val")
res.tab = res.tab %>%  
  mutate(var_outage = case_when(
    (var_names == "SAIDI_morning") ~ "Morning",
    (var_names == "SAIDI_afternoon") ~ "Afternoon",
    (var_names == "SAIDI_evening") ~ "Evening",
    (var_names == "SAIDI_night") ~ "Night",
    .default = "SAIDI (average)"
  ))

res.tab$var_outage = factor(res.tab$var_outage, 
                            levels = c("SAIDI (average)",
                                       "Morning",
                                       "Afternoon",
                                       "Evening",
                                       "Night"))

p1 = ggplot(res.tab, aes(x=var_outage)) + 
  geom_hline(yintercept = 0, lty = "dashed") +
  geom_errorbar(aes(ymin = est-1.96*se, ymax = est+1.96*se), 
                width = 0.3,
                position = position_dodge(width = 0.5)) +
  geom_point(aes(y=est), position = position_dodge(width = 0.5)) +
  # scale_y_continuous(breaks = seq(-1,0,0.01)) +
  labs(x = "", y = "") +
  theme_light() + theme(panel.grid = element_blank())


# plot bin-regression result ===========================================
res_save3 = resList[[3]]
res.tab = as.data.table(res_save3$coeftable) %>% 
  mutate(var_names = rownames(res_save3$coeftable))
colnames(res.tab)[1:4] <- c("est", "se", "t_val", "p_val")

res.tab = res.tab %>%  
  mutate(var_outage = case_when(
    (var_names == "outBin_2") ~ "+0-2 hours",
    (var_names == "outBin_4") ~ "+2-4 hours",
    (var_names == "outBin_6") ~ "+4-6 hours",
    (var_names == "outBin_8") ~ "+6-8 hours",
    .default = "> 8 hours"
  ))

res.tab$var_outage = factor(res.tab$var_outage, 
                            levels = c("+0-2 hours",
                                       "+2-4 hours",
                                       "+4-6 hours",
                                       "+6-8 hours",
                                       "> 8 hours"))

p2 = ggplot(res.tab, aes(x=var_outage)) + 
  geom_hline(yintercept = 0, lty = "dashed") +
  geom_errorbar(aes(ymin = est-1.96*se, ymax = est+1.96*se), 
                width = 0.3,
                position = position_dodge(width = 0.5)) +
  geom_point(aes(y=est), position = position_dodge(width = 0.5)) +
  # scale_y_continuous(breaks = seq(-1,0,0.05)) +
  labs(x = "", y = "") +
  theme_light() + theme(panel.grid = element_blank())

# combine plots =====================================================
p1 <- p1 + theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 10))
p2 <- p2 + theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 10))
ggarrange(p1, p2, align = "h", #widths = c(0.55, 0.45),
          labels = c("a", "b"), label.x = 0, label.y = 1.01)

# ggsave("fig_saved/fig_result1_save.jpg", 
#        dpi = 300, width = 8, height = 4)


#####################################################################
# R Code for sectoral variation results
#####################################################################
# import combined spend-outage data (not included) ------------------
dt0 = readRDS("Daily_data_combined_county_9sector.rds")
# set up the list (level) of sectors **************************
place_type_list = c("Food and Beverage Stores",
                    "General Merchandise Stores",
                    "Gasoline Stations",
                    "Building Material and Supplies",
                    "Clothing Stores",
                    "Health and Personal Care Supplies",
                    "Other Retail Trade",
                    "Accommodation",
                    "Restaurants and Drinking Places",
                    "Others")

fe_list = c("fips_code", "local_date", "county_week")
f_res = paste0("Daily_Spend ~ SAIDI ",
               "|", paste(fe_list, collapse = "+"))
f_res = f_res %>% str_replace_all(" ","") %>% as.formula()

resList1 = list() # record regression output
for (s in 1:length(place_type_list)) {
  sector_select = place_type_list[s]
  dt = dt1 %>% filter(sector_name == sector_select)
  
  res_main = feglm(f_res, 
                   data = dt %>% filter(Daily_Spend > 0),
                   family = Gamma(link = "log"), 
                   cluster=~fips_code,
                   glm.iter = 500)
  resList1[[s]] = res_main # save result
  print(sector_select)
  print(summary(dt$noSpend))
  rm(dt, res_main)
  
}


# Plot result by sector ==================================
dt.plot1.list = list()
for (s in 1:length(place_type_list)) {
  sector_name = place_type_list[s]
  res_save = resList1[[s]]
  
  res.tab = as.data.table(res_save$coeftable) %>% 
    mutate(var_names = rownames(res_save$coeftable),
           place_type = sector_name)
  colnames(res.tab)[1:4] <- c("est", "se", "t_val", "p_val")
  dt.plot1.list[[s]] = res.tab
  print(s)
}

dt.plot1 = rbindlist(dt.plot1.list)
dt.plot1$place_type = factor(dt.plot1$place_type, 
                             levels = rev(place_type_list))

p3 = ggplot(dt.plot1 %>% filter(place_type != "Others"), 
            aes(x=reorder(place_type, est))) + 
  geom_hline(yintercept = 0, lty = "dashed") +
  geom_errorbar(aes(ymin = est-1.96*se, ymax = est+1.96*se), 
                width = 0.3,
                position = position_dodge(width = 0.5)) +
  geom_point(aes(y=est), position = position_dodge(width = 0.5)) +
  scale_y_continuous(breaks = seq(-1,0,0.01), limits = c(-0.06,0)) +
  coord_flip() +
  labs(x = "", y = "") +
  theme_light() + theme(panel.grid.minor.x = element_blank(),
                        panel.grid.major.x = element_blank(),
                        text = element_text(size=10))
p3
# ggsave("fig_saved/fig2_mainRes_sector.jpg", plot = p1, 
#        dpi = 300, width = 5, height = 4)
