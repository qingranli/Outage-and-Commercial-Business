# Simulate Total Costs of Power Outages by county ($ per outage hour)
#
code_dir = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(code_dir)
getwd()

library(tidyverse)
library(data.table)
library(tigris)
library(sf)
library(fixest) # fixed-effect regression
library(ggpubr) # plot

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
f_res = paste0("Daily_Spend ~ SAIDI ",
               "|", paste(fe_list, collapse = "+"))
f_res = f_res %>% str_replace_all(" ","") %>% as.formula()
print(f_res)
res_main = feglm(f_res, 
                 data = dt %>% filter(Daily_Spend > 0),
                 family = Gamma(link = "log"), 
                 cluster=~fips_code,
                 glm.iter = 100)

#####################################################################
# ==================== Simulate Cost of Outage  ====================#
#####################################################################
dt_new = dt %>% rename(true_SAIDI = SAIDI)
# predict Sales with no outage
dt_new$Spend_null <- predict(res_main, 
                             newdata = dt_new %>% mutate(SAIDI = 0), 
                             type = "response")

# predict Sales with observed outage
dt_new$Spend_hat <- predict(res_main, 
                            newdata = dt_new %>% mutate(SAIDI = true_SAIDI), 
                            type = "response")

# compute cost of outage (Null - Prediction)
dt_new = dt_new %>% mutate(Cost = Spend_null - Spend_hat)

# aggregate annual cost by county by year ---------------------------
cost_sum = dt_new %>% mutate(data_year = year(local_date)) %>% 
  group_by(fips_code, data_year) %>% 
  summarise(Total_Cost = sum(Cost, na.rm = TRUE),
            Total_Spend = sum(Daily_Spend, na.rm = TRUE),
            Total_Outage = sum(true_SAIDI, na.rm = TRUE),
            .groups = "drop") %>% 
  mutate(cost_percent_sales = 100*Total_Cost/Total_Spend,
         cost_per_hour = ifelse(Total_Outage >0, Total_Cost/Total_Outage, 0))

cost_2022 <- cost_sum %>% filter(data_year == 2022)
summary(cost_2022)

#####################################################################
# ================ Plot Cost of Outage ($ per hour) ================#
#####################################################################
# import US county map (50 states + DC)
state_list = fips_codes %>% dplyr::select(state, state_code) %>% 
  unique() %>% filter(state_code < 60)
US_county = counties(year = 2019)
US_county <- US_county %>% 
  filter(STATEFP %in% state_list$state_code) %>% 
  select(STATEFP, GEOID) %>% mutate(fips_code = as.numeric(GEOID)) %>% 
  st_transform(crs = 4326) %>% 
  shift_geometry(position = "below")

# add cost to map
cost_map = left_join(US_county, cost_2022, by = "fips_code")

p_out = ggplot(cost_map) + geom_sf(aes(fill = cost_per_hour)) + 
  scale_fill_viridis_b(option = "E",
                       na.value = "white",
                       trans = "sqrt",
                       breaks = c(0, 10, 100, 500, 1000, 5000, 10000), 
                       labels = scales::comma,
                       name = "$ per hour") +
  theme_void()

# ggsave("fig_saved/map_cost_per_hour.jpg",plot = p_out, 
#        dpi = 300, width = 8, height = 4)
