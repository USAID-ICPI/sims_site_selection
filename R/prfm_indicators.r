
# PERFORMANCE -------------------------------------------------------------

prfm_ind <- df_site %>% 
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
  select(-c(indicator, ReportedStatus, `Total Numerator`)) %>% 
  filter(is.finite(knownstatus))

df_ovc <- df_site %>% 
  filter(indicator == "OVC_SERV",
         standardizeddisaggregate == "Total Numerator") %>% 
  add_cumulative() %>% 
  group_by(operatingunit, psnu, sitename, orgunituid, indicator, standardizeddisaggregate) %>% 
  summarise_at(vars(fy2018cum), ~ sum(., na.rm = TRUE)) %>%
  ungroup() %>% 
  select(-c(indicator, standardizeddisaggregate)) %>%
  filter(fy2018cum != 0) %>% 
  rename(ovc_serv = fy2018cum)

prfm_ovc <- full_join(df_ovc, df_ovcstat)
  rm(df_ovc, df_ovcstat)

prfm_ovc <- prfm_ovc %>%
  mutate(prfm.ovc_status.score = case_when(ovc_serv > median(ovc_serv, na.rm = TRUE) &
                                              knownstatus < median(knownstatus, na.rm = TRUE)   ~ 2,
                                            knownstatus < median(knownstatus, na.rm = TRUE)     ~ 1,
                                            TRUE                                                ~ 0)) %>%
  rename(prfm.ovc_status.value = knownstatus) %>% 
  select(-ovc_serv)
  