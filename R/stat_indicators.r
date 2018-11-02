
# HIV KNOWN STATUS INDICATORS ---------------------------------------------

#1. OVC Known Status
  stat_ovc <- df_site %>% 
    filter(indicator %in% c("OVC_HIVSTAT", "OVC_SERV"),
           (standardizeddisaggregate == "Total Numerator" |
              (standardizeddisaggregate == "ReportedStatus" & otherdisaggregate ==  "No HIV Status"))) %>% 
    add_cumulative() %>% 
    mutate(standardizeddisaggregate = str_remove(standardizeddisaggregate, " ")) %>% 
    unite(ind, indicator, standardizeddisaggregate) %>% 
    group_by(operatingunit, psnu, sitename, orgunituid, ind) %>% 
    summarise_at(vars(fy2018cum), ~ sum(., na.rm = TRUE)) %>%
    ungroup() %>% 
    spread(ind, fy2018cum) %>% 
    filter_at(vars(contains("OVC")), any_vars(!is.na(.) & .!=0)) %>% 
    mutate(knownstatus = round(1-(OVC_HIVSTAT_ReportedStatus/OVC_HIVSTAT_TotalNumerator), 2)) %>% 
    select(operatingunit, psnu, sitename, orgunituid, ovc_serv = OVC_SERV_TotalNumerator, knownstatus) 
    #filter(is.finite(knownstatus))
  
  stat_ovc <- stat_ovc %>%
    mutate(stat.ovc.score = case_when(ovc_serv > median(ovc_serv, na.rm = TRUE) &
                                      knownstatus < median(knownstatus, na.rm = TRUE)     ~ 2,
                                      knownstatus < median(knownstatus, na.rm = TRUE)     ~ 1,
                                      TRUE                                                ~ 0)) %>%
    rename(stat.ovc.value = knownstatus) %>% 
    select(-ovc_serv)

#2. & 3. PMTCT & TB Known Status
  
  stat_oth <- df_site %>% 
    filter(indicator %in% c("PMTCT_STAT", "TB_STAT"),
           str_detect(standardizeddisaggregate, "Total")) %>% 
    add_cumulative() %>% 
    group_by(operatingunit, psnu, sitename, orgunituid, indicator, numeratordenom) %>% 
    summarise_at(vars(fy2018cum), sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    filter(fy2018cum != 0) %>% 
    spread(numeratordenom, fy2018cum) %>% 
    mutate(knownstatus = round(N / D, 2)) %>% 
    select(-N) 
    
  stat_oth <- stat_oth %>%
    mutate(score = case_when(D > median(D, na.rm = TRUE) &
                             knownstatus < median(knownstatus, na.rm = TRUE)     ~ 2,
                             knownstatus < median(knownstatus, na.rm = TRUE)     ~ 1,
                             TRUE                                                ~ 0)) %>%
    rename(value = knownstatus) %>% 
    select(-D) %>% 
    mutate(indicator = str_extract(indicator, "(PMTCT|TB)") %>% paste0("stat.", .) %>% tolower()) %>% 
    gather(type, val, value, score) %>% 
    unite(ind, indicator, type, sep = ".") %>% 
    spread(ind, val)
