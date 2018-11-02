
# PERFORMANCE -------------------------------------------------------------

prfm_ind <- df_site %>% 
  filter(indicator %in% c("HTS_TST_POS", "TX_NEW", "PMTCT_STAT", "OVC_SERV", "KP_PREV", "VMMC_CIRC"),
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

prfm_ind <- prfm_ind %>% 
  group_by(indicator) %>% 
  mutate(score = case_when(achievement < thres_med & 
                             fy2018_targets > median(fy2018_targets, na.rm = TRUE) ~ 2,
                           achievement < thres_med                                 ~ 1,
                           TRUE                                                    ~ 0)) %>% 
  ungroup() %>% 
  rename(value = achievement) %>%
  select(-fy2018_targets) %>% 
  gather(type, val, value, score, na.rm = TRUE) %>% 
  unite(ind, indicator, type, sep = ".") %>% 
  spread(ind, val) 
