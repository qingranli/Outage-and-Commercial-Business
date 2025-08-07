# analyze benefits of place-based reliability improvement
code_dir = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(code_dir)
getwd()

library(tidyverse)
library(data.table)
library(ggpubr) # plot

rm(list=ls())
gc()

# import simulated sales ============================================
dt1 = readRDS("Sales_simulate_scn1.rds")
dt2 = readRDS("Sales_simulate_scn2.rds")
dt3 = readRDS("Sales_simulate_scn3.rds")

dt4A = readRDS("Sales_simulate_scn4A.rds")
dt4B = readRDS("Sales_simulate_scn4B.rds")
dt4C = readRDS("Sales_simulate_scn4C.rds")
dt4D = readRDS("Sales_simulate_scn4D.rds")
dt4E = readRDS("Sales_simulate_scn4E.rds")
dt4F = readRDS("Sales_simulate_scn4F.rds")
dt4G = readRDS("Sales_simulate_scn4G.rds")

# aggregate sales by iteration and scenario -------------------------
dt1_sum = dt1 %>% 
  group_by(data_year, iter_num, scenario) %>% 
  summarise(Sales_null = sum(Spend_null, na.rm = TRUE),
            Sales_hat = sum(Spend_hat, na.rm = TRUE), 
            .groups = "drop") %>% 
  mutate(Cost_save = Sales_hat - Sales_null) %>% 
  filter(data_year == 2022)

dt2_sum = dt2 %>% 
  group_by(data_year, iter_num, scenario) %>% 
  summarise(Sales_null = sum(Spend_null, na.rm = TRUE),
            Sales_hat = sum(Spend_hat, na.rm = TRUE), 
            .groups = "drop") %>% 
  mutate(Cost_save = Sales_hat - Sales_null) %>% 
  filter(data_year == 2022)

dt3_sum = dt3 %>% 
  group_by(data_year, iter_num, scenario) %>% 
  summarise(Sales_null = sum(Spend_null, na.rm = TRUE),
            Sales_hat = sum(Spend_hat, na.rm = TRUE), 
            .groups = "drop") %>% 
  mutate(Cost_save = Sales_hat - Sales_null) %>% 
  filter(data_year == 2022)


rm(dt1, dt2, dt3)

dt4A_sum = dt4A %>% 
  group_by(data_year, iter_num, scenario) %>% 
  summarise(Sales_null = sum(Spend_null, na.rm = TRUE),
            Sales_hat = sum(Spend_hat, na.rm = TRUE), 
            .groups = "drop") %>% 
  mutate(Cost_save = Sales_hat - Sales_null) %>% 
  filter(data_year == 2022)

dt4B_sum = dt4B %>% 
  group_by(data_year, iter_num, scenario) %>% 
  summarise(Sales_null = sum(Spend_null, na.rm = TRUE),
            Sales_hat = sum(Spend_hat, na.rm = TRUE), 
            .groups = "drop") %>% 
  mutate(Cost_save = Sales_hat - Sales_null) %>% 
  filter(data_year == 2022)

dt4C_sum = dt4C %>% 
  group_by(data_year, iter_num, scenario) %>% 
  summarise(Sales_null = sum(Spend_null, na.rm = TRUE),
            Sales_hat = sum(Spend_hat, na.rm = TRUE), 
            .groups = "drop") %>% 
  mutate(Cost_save = Sales_hat - Sales_null) %>% 
  filter(data_year == 2022)

dt4D_sum = dt4D %>% 
  group_by(data_year, iter_num, scenario) %>% 
  summarise(Sales_null = sum(Spend_null, na.rm = TRUE),
            Sales_hat = sum(Spend_hat, na.rm = TRUE), 
            .groups = "drop") %>% 
  mutate(Cost_save = Sales_hat - Sales_null) %>% 
  filter(data_year == 2022)

dt4E_sum = dt4E %>% 
  group_by(data_year, iter_num, scenario) %>% 
  summarise(Sales_null = sum(Spend_null, na.rm = TRUE),
            Sales_hat = sum(Spend_hat, na.rm = TRUE), 
            .groups = "drop") %>% 
  mutate(Cost_save = Sales_hat - Sales_null) %>% 
  filter(data_year == 2022)

dt4F_sum = dt4F %>% 
  group_by(data_year, iter_num, scenario) %>% 
  summarise(Sales_null = sum(Spend_null, na.rm = TRUE),
            Sales_hat = sum(Spend_hat, na.rm = TRUE), 
            .groups = "drop") %>% 
  mutate(Cost_save = Sales_hat - Sales_null) %>% 
  filter(data_year == 2022)

dt4G_sum = dt4G %>% 
  group_by(data_year, iter_num, scenario) %>% 
  summarise(Sales_null = sum(Spend_null, na.rm = TRUE),
            Sales_hat = sum(Spend_hat, na.rm = TRUE), 
            .groups = "drop") %>% 
  mutate(Cost_save = Sales_hat - Sales_null) %>% 
  filter(data_year == 2022)

rm(dt4A, dt4B, dt4C, dt4D, dt4E, dt4F, dt4G)

# change long to wide data table ------------------------------------
dt1 <- dcast(dt1_sum %>% data.table(), formula = iter_num ~ scenario,
             value.var = "Cost_save") 
dt2 <- dcast(dt2_sum %>% data.table(), formula = iter_num ~ scenario,
             value.var = "Cost_save")
dt3 <- dcast(dt3_sum %>% data.table(), formula = iter_num ~ scenario,
             value.var = "Cost_save")
rm(dt1_sum, dt2_sum, dt3_sum)

dt4A <- dcast(dt4A_sum %>% data.table(), formula = iter_num ~ scenario,
             value.var = "Cost_save")
dt4B <- dcast(dt4B_sum %>% data.table(), formula = iter_num ~ scenario,
              value.var = "Cost_save")
dt4C <- dcast(dt4C_sum %>% data.table(), formula = iter_num ~ scenario,
              value.var = "Cost_save")
dt4D <- dcast(dt4D_sum %>% data.table(), formula = iter_num ~ scenario,
              value.var = "Cost_save")
dt4E <- dcast(dt4E_sum %>% data.table(), formula = iter_num ~ scenario,
              value.var = "Cost_save")
dt4F <- dcast(dt4F_sum %>% data.table(), formula = iter_num ~ scenario,
              value.var = "Cost_save")
dt4G <- dcast(dt4G_sum %>% data.table(), formula = iter_num ~ scenario,
              value.var = "Cost_save")
rm(dt4A_sum, dt4B_sum, dt4C_sum, dt4D_sum, dt4E_sum, dt4F_sum, dt4G_sum)

# compute benefit (scn - random) ====================================
dt1 <- dt1 %>% mutate(benefit_1A = `1A` - `1Random`,
                      benefit_1B = `1B` - `1Random`)
dt2 <- dt2 %>% mutate(benefit_2A = `2A` - `2Random`,
                      benefit_2B = `2B` - `2Random`)
dt3 <- dt3 %>% mutate(benefit_3A = `3A` - `3Random`,
                      benefit_3B = `3B` - `3Random`)

dt4A <- dt4A %>% mutate(benefit_4A = `4A` - `4A_Random`)
dt4B <- dt4B %>% mutate(benefit_4B = `4B` - `4B_Random`)
dt4C <- dt4C %>% mutate(benefit_4C = `4C` - `4C_Random`)
dt4D <- dt4D %>% mutate(benefit_4D = `4D` - `4D_Random`)
dt4E <- dt4E %>% mutate(benefit_4E = `4E` - `4E_Random`)
dt4F <- dt4F %>% mutate(benefit_4F = `4F` - `4F_Random`)
dt4G <- dt4G %>% mutate(benefit_4G = `4G` - `4G_Random`)

#####################################################################
# ============ Summary: Reliability Benefits =======================#
#####################################################################
# combine benefit columns ===========================================
dt_combine = dt1 %>% select(iter_num, benefit_1A, benefit_1B) %>% 
  left_join(dt2 %>% select(iter_num, benefit_2A, benefit_2B), by = "iter_num") %>% 
  left_join(dt3 %>% select(iter_num, benefit_3A, benefit_3B), by = "iter_num") %>% 
  left_join(dt4A %>% select(iter_num, benefit_4A), by = "iter_num") %>% 
  left_join(dt4B %>% select(iter_num, benefit_4B), by = "iter_num") %>% 
  left_join(dt4C %>% select(iter_num, benefit_4C), by = "iter_num") %>% 
  left_join(dt4D %>% select(iter_num, benefit_4D), by = "iter_num") %>% 
  left_join(dt4E %>% select(iter_num, benefit_4E), by = "iter_num") %>% 
  left_join(dt4F %>% select(iter_num, benefit_4F), by = "iter_num") %>% 
  left_join(dt4G %>% select(iter_num, benefit_4G), by = "iter_num")

dt_long = melt(dt_combine, id.vars = "iter_num", 
               variable.name = "scn",
               value.name = "benefit")
dt_long <- dt_long %>% mutate(scenario = substr(scn, 9,10))

# summary relative policy benefits (1000 $)
summary_tbl = dt_long %>% group_by(scenario) %>% 
  summarise(benefit_mean = (1e-3)*mean(benefit),
            benefit_sd = (1e-3)*sd(benefit),
            .groups = "drop")
summary_tbl$scenario = factor(summary_tbl$scenario,
                              levels = rev(unique(dt_long$scenario)))

# Plot 1 ============================================================
ggplot(summary_tbl, aes(x=scenario)) + 
  geom_hline(yintercept = 0, lty = "dashed", color = "gray") +
  geom_linerange(aes(ymin = 0, ymax = benefit_mean)) + 
  geom_point(aes(y = benefit_mean)) +
  geom_label(aes(y = benefit_mean, 
                 label = sprintf("%.1f", benefit_mean),
                 fill = benefit_mean)) +
  scale_fill_gradient2(low = "#ef8a62", mid = "#f7f7f7", high = "#67a9cf",
                       midpoint = 0) +
  scale_y_continuous(breaks = seq(-100,400,50)) +
  labs(x = "", y = "Policy benefits relative to baseline (in year 2022)",
       fill = "thousand $") +
  coord_flip() +
  theme_bw() + theme(panel.grid = element_blank(),
                     legend.position = c(0.9, 0.75))

# ggsave("fig_saved/fig_result6_Policy_benefits.jpg", dpi = 300,
#        width = 8, height = 4.5)


# Plot 2 ============================================================
ggplot(summary_tbl, aes(x=scenario)) +
  geom_hline(yintercept = 0, lty = "dashed", color = "gray") +
  geom_errorbar(aes(ymin = CIlow, ymax = CIhigh), width = 0.3) +
  geom_point(aes(y = benefit_mean)) +
  geom_label(aes(y = benefit_mean,
                 label = sprintf("%.1f", benefit_mean),
                 fill = benefit_mean)) +
  scale_fill_gradient2(low = "#ef8a62", mid = "#f7f7f7", high = "#67a9cf",
                      midpoint = 0) +
  scale_y_continuous(breaks = seq(-1000,4000,100)) +
  labs(x = "", y = "Policy benefits relative to baseline (in year 2022)",
       fill = "thousand $") +
  coord_flip() +
  theme_bw() + theme(panel.grid = element_blank())
