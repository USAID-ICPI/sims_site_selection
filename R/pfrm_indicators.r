# SETUP -------------------------------------------------------------------

library(tidyverse)
library(ICPIutilities)
library(RearWindow)

path <- "~/ICPI/Data"
ou <- "Kenya"

# IMPORT DATASET ----------------------------------------------------------

#strucutre filepath from inputs
filepath <- file.path(path, paste0("MER_Structured_Dataset_SITE_IM_FY17-18_20180921_v2_2_", ou, ".rds"))
#open file
df_site <- read_rds(filepath)
rm(filepath)


# PERFORMANCE -------------------------------------------------------------

prfm <- df_site %>% 
  filter(indicator %in% c("HTS_TST_POS", "TX_NEW", "PMTCT_STAT", "OVC_SERV", "KP_PREV"),
         standardizeddisaggregate == "Total Numerator",
         typemilitary == "N") %>% 
  add_cumulative() %>% 
  group_by(operatingunit, psnu,sitename, orgunituid, indicator) %>% 
  summarise_at(vars(fy2018cum, fy2018_targets), ~ sum(., na.rm = TRUE)) %>%
  ungroup() %>% 
  filter(fy2018_targets !=0) %>% 
  mutate(achievement = round(fy2018cum/fy2018_targets, 3)) %>% 
  select(-fy2018cum) %>% 
  mutate(indicator = paste0("prfm.", str_to_lower(indicator)))

thres_med <- rw_addthresholds(df_site) %>% .["med"]

prfm <- prfm %>% 
  group_by(indicator) %>% 
  mutate(score = case_when(achievement < thres_med & 
                             fy2018_targets > median(fy2018_targets, na.rm = TRUE) ~ 2,
                           achievement < thres_med                                 ~ 1,
                           TRUE                                                    ~ 0)) %>% 
  ungroup() %>% 
  rename(value1 = fy2018_targets,
         value2 = achievement) %>% 
  gather(type, val, value1, value2, score, na.rm = TRUE) %>% 
  unite(ind, indicator, type, sep = ".") %>% 
  spread(ind, val) 


  
df_ovcstat <- df_site %>% 
  filter(indicator == "OVC_HIVSTAT",
         (standardizeddisaggregate == "Total Numerator" |
          (standardizeddisaggregate == "ReportedStatus" & otherdisaggregate ==  "No HIV Status"))) %>% 
  add_cumulative() %>% 
  group_by(operatingunit, psnu, sitename, orgunituid, indicator, standardizeddisaggregate) %>% 
  summarise_at(vars(fy2018cum), ~ sum(., na.rm = TRUE)) %>%
  ungroup() %>% 
  spread(standardizeddisaggregate, fy2018cum) %>% 
  mutate(knownstatus = round(1-(ReportedStatus/`Total Numerator`), 2)) %>% 
  select(-c(indicator, ReportedStatus, `Total Numerator`))

df_ovc <- df_site %>% 
  filter(indicator == "OVC_SERV",
         standardizeddisaggregate == "Total Numerator") %>% 
  add_cumulative() %>% 
  group_by(operatingunit, psnu, sitename, orgunituid, indicator, standardizeddisaggregate) %>% 
  summarise_at(vars(fy2018cum), ~ sum(., na.rm = TRUE)) %>%
  ungroup() %>% 
  select(-c(indicator, standardizeddisaggregate)) %>% 
  rename(ovc_serv = fy2018cum)

df_ovc_comb <- full_join(df_ovc, df_ovcstat)
  
# prfm2 <- df_ovc_comb %>% 
#   group_by(indicator) %>% 
#   mutate(score = case_when(achievement < thres_med & 
#                              fy2018_targets > median(fy2018_targets, na.rm = TRUE) ~ 2,
#                            achievement < thres_med                                 ~ 1,
#                            TRUE                                                    ~ 0)) %>% 
#   ungroup() %>% 
#   rename(value1 = fy2018_targets,
#          value2 = achievement) %>% 
#   gather(type, val, value1, value2, score, na.rm = TRUE) %>% 
#   unite(ind, indicator, type, sep = ".") %>% 
#   spread(ind, val) 