# Construct policy scenarios - county Selection
# save: County_Selection_Panel.rds
code_dir = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(code_dir)
getwd()

library(tidyverse)
library(data.table)
library(tigris)
library(sf)
library(ggpubr) # plot

rm(list=ls())
gc()


# prep for county map -----------------------------------------------
state_list = fips_codes %>% dplyr::select(state, state_code) %>% 
  unique() %>% filter(state_code < 60)
US_county = counties(year = 2019)
US_county <- US_county %>% 
  filter(STATEFP %in% state_list$state_code) %>% 
  select(STATEFP, GEOID) %>% mutate(fips_code = as.numeric(GEOID)) %>% 
  st_transform(crs = 4326) %>% 
  shift_geometry(position = "below")

# import combined spend-outage data =================================
# Note: A 1% data sample is provided just for completeness of the code, 
#      not for replication of the results. See "Data Availability" statement 
#      for how to get the data for this study.
dt_sample = readRDS("Data_combined_county_sample.rds")
dt = dt_sample

# get SAIDI data for all US counties 
dt0 <- dt %>% select(fips_code, state, local_date, SAIDI, data_year)

#####################################################################
# ============= Scenario 1 based on SAIDI decile ===================#
#####################################################################
# Compute average Daily SAIDI for each county (2022) ===========
saidi_county = dt0 %>% filter(data_year == 2022) %>% 
  group_by(fips_code) %>% 
  summarise(SAIDI_mu = mean(SAIDI), .groups = "drop")

# generate decile values based on average *******************
saidi_county <- saidi_county %>% 
  mutate(SAIDI_decile = ntile(SAIDI_mu, 10))

plot_map = US_county %>% left_join(saidi_county, by = "fips_code")
p_output = ggplot(plot_map) +
  geom_sf(aes(fill = factor(SAIDI_decile))) +
  scale_fill_viridis_d(name = "Average Daily SAIDI (Decile)") +
  theme_void() +
  theme(legend.position = "top") +
  guides(fill=guide_legend(nrow=2,byrow=TRUE))

# ggsave("fig_saved/map_SAIDI_decile.jpg", plot = p_output,
#        dpi = 300, width = 8, height = 4)

scn1_select = saidi_county %>% select(fips_code, SAIDI_decile) %>% 
  mutate(scn1A = ifelse(SAIDI_decile == 10, 1, 0),
         scn1B = ifelse(SAIDI_decile == 1, 1, 0))

#####################################################################
# ====== Scenario 2 & 3 based on income & racial decile ============#
#####################################################################
# import socioeconomic deciles
county_attr = readRDS("county_socioecon_deciles.rds")
county_attr <- county_attr %>% select(fips_code, income_decile, share_minority_decile)

scn2_select = county_attr %>% 
  mutate(scn2A = ifelse(income_decile == 10, 1, 0),
         scn2B = ifelse(income_decile == 1, 1, 0),
         scn3A = ifelse(share_minority_decile == 10, 1, 0),
         scn3B = ifelse(share_minority_decile == 1, 1, 0)) %>% 
  filter(fips_code %in% unique(dt0$fips_code)) %>% 
  data.table()

summary(scn2_select)
scn2_select[is.na(scn2A), scn2A := 0]
scn2_select[is.na(scn2B), scn2B := 0]

#####################################################################
# ============= Scenario 4 based on hazard EAL rating ==============#
#####################################################################
dt_NRI = readRDS("County_NRI_EAL_Rating.rds")
Hazard_Type = c("Heat Wave", "Winter Weather",
                "Wildfire", "Hurricane",
                "Tornado", "Hail",
                "Riverine Flooding")

dt_NRI <- dt_NRI %>% filter(data_type %in% Hazard_Type) %>% 
  filter(!is.na(EAL_SCORE), EAL_SCORE > 0) %>% 
  filter(fips_code %in% unique(dt0$fips_code))

unique(dt_NRI$EAL_RATNG)

county_rating = dt_NRI %>% group_by(fips_code, data_type) %>% 
  summarise(RiskHigh = ifelse(EAL_RATNG == "Very High" |
                               EAL_RATNG == "Relatively High"|
                                EAL_RATNG == "Relatively Moderate", 1, 0),
            .groups = "drop") %>% data.table()

# county_rating %>% group_by(data_type) %>% summarise(n=sum(RiskHigh))
scn4_select = dcast(county_rating, fips_code ~ data_type, value.var = "RiskHigh")
scn4_select <- scn4_select %>% 
  select(fips_code, `Heat Wave`, `Winter Weather`,
         Wildfire, Hurricane, Tornado, Hail, `Riverine Flooding`)

colnames(scn4_select)[2:8] = c("scn4A", "scn4B", "scn4C", "scn4D",
                               "scn4E", "scn4F", "scn4G")
summary(scn4_select)
scn4_select[is.na(scn4A), scn4A := 0]
scn4_select[is.na(scn4B), scn4B := 0]
scn4_select[is.na(scn4D), scn4D := 0]
scn4_select[is.na(scn4G), scn4G := 0]

#####################################################################
# combine county selection data 
dt_county = scn1_select %>% 
  left_join(scn2_select, by = "fips_code") %>% 
  left_join(scn4_select, by = "fips_code") %>% 
  data.table()

saveRDS(dt_county, "County_Selection_Panel_sample.rds")
