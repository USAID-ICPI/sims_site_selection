

# CASE IDENTIFICATION -----------------------------------------------------

#1.&2. HTS_TST_POS Position relative to OU-/PSNU-level median (median last three quarters)
  
  #pull headers to figure out last three quarters to keep  
    headers <- names(df_site) 
    pds <- headers[stringr::str_detect(headers, "q(?=[:digit:])")] %>% 
      tail(., n =3)
  #narrow down to data needed for indicator creation
    ci_hts_pos <- df_site %>% 
      filter(indicator == "HTS_TST_POS", 
             standardizeddisaggregate == "Total Numerator",
             typemilitary == "N") %>% 
      select(operatingunit, psnu,sitename, orgunituid, pds) %>% 
      filter_at(vars(contains("q")), any_vars(!is.na(.) & .!=0)) #remove if all quarters are missing
  #site sum over last 3 pds
    ci_hts_pos <- ci_hts_pos %>% 
      gather(pd, val, starts_with("fy")) %>% 
      select(-pd) %>% 
      group_by_if(is.character) %>% 
      summarise_if(is.numeric, ~ sum(., na.rm = TRUE)) %>% 
      ungroup()
  #Calculate percentile grouping
    ci_hts_pos <- ci_hts_pos %>%
      group_by(operatingunit) %>% 
      mutate(ci.hts_pos_ou.score = case_when(val > quantile(val, .75) ~ 2,
                                             val > quantile(val, .50) ~ 1,
                                             TRUE                     ~ 0)) %>% 
      ungroup() %>%
      group_by(operatingunit, psnu) %>% 
      mutate(ci.hts_pos_psnu.score = case_when(val > quantile(val, .75) ~ 2,
                                               val > quantile(val, .50) ~ 1,
                                               TRUE                     ~ 0)) %>% 
      ungroup() %>% 
      mutate(ci.hts_pos_ou.value = val) %>% 
      rename(ci.hts_pos_psnu.value = val)
    
#3. Year on Year change in volume
    
    qtr <- identifypd(df_site, "quarter")
      
    qtr_fltr <- case_when(qtr == 1 ~ "1",
                          qtr == 2 ~ "(1|2)",
                          qtr == 3 ~ "(1|2|3)",
                          qtr == 4 ~ "(?=[:digit:])")
    
    pds <- headers[stringr::str_detect(headers, paste0("q", qtr_fltr))]
    
    ci_hts_pos_yoy <- df_site %>% 
      filter(indicator == "HTS_TST_POS", 
             standardizeddisaggregate == "Total Numerator",
             typemilitary == "N") %>% 
      select(operatingunit, psnu,sitename, orgunituid, pds) 
    
    ci_hts_pos_yoy <- ci_hts_pos_yoy %>% 
      gather(pd, val, starts_with("fy"), na.rm = TRUE) %>% 
      mutate(pd = str_remove_all(pd, "q[:digit:]")) %>% 
      group_by_if(is.character) %>% 
      summarise_if(is.numeric, ~ sum(., na.rm = TRUE)) %>% 
      #ungroup() %>% 
      #group_by_if(is.character) %>% 
      mutate(ci.hts_pos_yoyd =val - dplyr::lag(val),
             ci.hts_pos_yoyc = (val - dplyr::lag(val))/dplyr::lag(val)) %>% 
      ungroup() %>% 
      filter(pd == "fy2018", is.finite(ci.hts_pos_yoyc))

  #Calculate percentile grouping
    ci_hts_pos_yoy <- ci_hts_pos_yoy %>%
      group_by(operatingunit) %>% 
      mutate(ci.hts_pos_yoyd.score = case_when(ci.hts_pos_yoyd > quantile(ci.hts_pos_yoyd, .75) ~ 2,
                                               ci.hts_pos_yoyd > quantile(ci.hts_pos_yoyd, .50) ~ 1,
                                               TRUE                                ~ 0),
             ci.hts_pos_yoyc.score = case_when(ci.hts_pos_yoyc > quantile(ci.hts_pos_yoyc, .75) ~ 2,
                                               ci.hts_pos_yoyc > quantile(ci.hts_pos_yoyc, .50) ~ 1,
                                               TRUE                                ~ 0)) %>% 
      ungroup() %>% 
      rename(ci.hts_pos_yoyd.value = ci.hts_pos_yoyd,
             ci.hts_pos_yoyc.value = ci.hts_pos_yoyc,
             ci.hts_pos_yoy.value = val) %>% 
      select(-pd, -ci.hts_pos_yoy.value)
   
#4. Share Index testing
  
  #narrow down to data needed for indicator creation
    ci_index <- df_site %>% 
      filter(indicator == "HTS_TST", 
             standardizeddisaggregate %in% c("Modality/Age Aggregated/Sex/Result",
                                             "Modality/Age/Sex/Result"),
             typemilitary == "N") %>% 
      select(operatingunit, psnu,sitename, orgunituid, pds, modality) %>% 
      filter_at(vars(contains("q")), any_vars(!is.na(.) & .!=0)) #remove if all quarters are missing
  #site sum over last 3 pds
    ci_index <- ci_index %>% 
      gather(pd, val, starts_with("fy"), na.rm = TRUE) %>% 
      select(-pd) %>% 
      mutate(modality = ifelse(modality %in% c("Index", "IndexMod"), "ALL Index", modality)) %>% 
      group_by_if(is.character) %>% 
      summarise_if(is.numeric, ~ sum(., na.rm = TRUE)) %>% 
      ungroup()
  #index share
    ci_index <- ci_index %>% 
      group_by(operatingunit, psnu, sitename, orgunituid) %>% 
      mutate(indexshare = round(val / sum(val), 2)) %>% 
      ungroup() %>% 
      filter(modality == "ALL Index") %>% 
      select(-modality)
  
  #Calculate percentile grouping
    ci_index <- ci_index %>%
      group_by(operatingunit, psnu) %>% 
      mutate(ci.hts_index.score = case_when(indexshare < quantile(indexshare, .25) ~ 2,
                                            indexshare < quantile(indexshare, .50) ~ 1,
                                            TRUE                                   ~ 0)) %>% 
      ungroup() %>% 
      rename(ci.hts_index.value = indexshare) %>% 
      select(-val)
    
  rm(headers, pds, qtr, qtr_fltr)
    