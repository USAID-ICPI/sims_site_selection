

# CASE IDENTIFICATION -----------------------------------------------------

#1.&2. TX_NEW Position relative to OU-/PSNU-level median (median last three quarters)
  
  #pull headers to figure out last three quarters to keep  
    headers <- names(df_site) 
    pds <- headers[stringr::str_detect(headers, "q(?=[:digit:])")] %>% 
      tail(., n =3)
  #narrow down to data needed for indicator creation
    init_tx_new <- df_site %>% 
      filter(indicator == "TX_NEW", 
             standardizeddisaggregate == "Total Numerator",
             typemilitary == "N") %>% 
      select(operatingunit, psnu,sitename, orgunituid, pds) %>% 
      filter_if(is.numeric, any_vars(!is.na(.) & .!=0)) #remove if all quarters are missing
  #site sum over last 3 pds
    init_tx_new <- init_tx_new %>% 
      gather(pd, val, starts_with("fy")) %>% 
      select(-pd) %>% 
      group_by_if(is.character) %>% 
      summarise_if(is.numeric, ~ sum(., na.rm = TRUE)) %>% 
      ungroup()
  #Calculate percentile grouping
    init_tx_new <- init_tx_new %>%
      group_by(operatingunit) %>% 
      mutate(init.tx_new_ou.score = case_when(val > quantile(val, .75) ~ 2,
                                              val > quantile(val, .50) ~ 1,
                                              TRUE                     ~ 0)) %>% 
      ungroup() %>%
      group_by(operatingunit, psnu) %>% 
      mutate(init.tx_new_psnu.score = case_when(val > quantile(val, .75) ~ 2,
                                                val > quantile(val, .50) ~ 1,
                                                TRUE                     ~ 0)) %>% 
      ungroup() %>% 
      mutate(init.tx_new_ou.value = val) %>% 
      rename(init.tx_new_psnu.value = val)
    
#3. Year on Year change in volume
    
    qtr <- identifypd(df_site, "quarter")
      
    qtr_fltr <- case_when(qtr == 1 ~ "1",
                          qtr == 2 ~ "(1|2)",
                          qtr == 3 ~ "(1|2|3)",
                          qtr == 4 ~ "(?=[:digit:])")
    
    pds <- headers[stringr::str_detect(headers, paste0("q", qtr_fltr))]
    
    init_tx_new_yoy <- df_site %>% 
      filter(indicator == "TX_NEW", 
             standardizeddisaggregate == "Total Numerator",
             typemilitary == "N") %>% 
      select(operatingunit, psnu,sitename, orgunituid, pds) 
    
    init_tx_new_yoy <- init_tx_new_yoy %>% 
      gather(pd, val, starts_with("fy"), na.rm = TRUE) %>% 
      mutate(pd = str_remove_all(pd, "q[:digit:]")) %>% 
      group_by_if(is.character) %>% 
      summarise_if(is.numeric, ~ sum(., na.rm = TRUE)) %>% 
      #ungroup() %>% 
      #group_by_if(is.character) %>% 
      mutate(init.tx_new_yoyd =val - dplyr::lag(val),
             init.tx_new_yoyc = (val - dplyr::lag(val))/dplyr::lag(val)) %>% 
      ungroup() %>% 
      filter(pd == "fy2018", is.finite(init.tx_new_yoyc))

  #Calculate percentile grouping
    init_tx_new_yoy <- init_tx_new_yoy %>%
      group_by(operatingunit) %>% 
      mutate(init.tx_new_yoyd.score = case_when(init.tx_new_yoyd > quantile(init.tx_new_yoyd, .75) ~ 2,
                                                init.tx_new_yoyd > quantile(init.tx_new_yoyd, .50) ~ 1,
                                                TRUE                                ~ 0),
             init.tx_new_yoyc.score = case_when(init.tx_new_yoyc > quantile(init.tx_new_yoyc, .75) ~ 2,
                                                init.tx_new_yoyc > quantile(init.tx_new_yoyc, .50) ~ 1,
                                                TRUE                                ~ 0)) %>% 
      ungroup() %>% 
      rename(init.tx_new_yoyd.value = init.tx_new_yoyd,
             init.tx_new_yoyc.value = init.tx_new_yoyc,
             init.tx_new_yoy.value = val) %>% 
      select(-pd, -init.tx_new_yoy.value)
     
#4. NET NEW change
    
    init_tx_netnew_yoy <- df_site %>% 
      filter(indicator == "TX_NET_NEW", 
             standardizeddisaggregate == "Total Numerator",
             typemilitary == "N") %>% 
      select(operatingunit, psnu,sitename, orgunituid, pds) 
    
    init_tx_netnew_yoy <- init_tx_netnew_yoy %>% 
      gather(pd, val, starts_with("fy"), na.rm = TRUE) %>% 
      mutate(pd = str_remove_all(pd, "q[:digit:]")) %>% 
      group_by_if(is.character) %>% 
      summarise_if(is.numeric, ~ sum(., na.rm = TRUE)) %>% 
      #ungroup() %>% 
      #group_by_if(is.character) %>% 
      mutate(init.tx_netnew_yoyd =  val - dplyr::lag(val),
             init.tx_netnew_yoyc = (val - dplyr::lag(val))/dplyr::lag(val)) %>% 
      ungroup() %>% 
      filter(pd == "fy2018", is.finite(init.tx_netnew_yoyc))
    
    #Calculate percentile grouping
    init_tx_netnew_yoy <- init_tx_netnew_yoy %>%
      group_by(operatingunit) %>% 
      mutate(init.tx_netnew_yoyd.score = case_when(init.tx_netnew_yoyd > quantile(init.tx_netnew_yoyd, .75) ~ 2,
                                                init.tx_netnew_yoyd > quantile(init.tx_netnew_yoyd, .50) ~ 1,
                                                TRUE                                ~ 0),
             init.tx_netnew_yoyc.score = case_when(init.tx_netnew_yoyc > quantile(init.tx_netnew_yoyc, .75) ~ 2,
                                                init.tx_netnew_yoyc > quantile(init.tx_netnew_yoyc, .50) ~ 1,
                                                TRUE                                ~ 0)) %>% 
      ungroup() %>% 
      rename(init.tx_netnew_yoyd.value = init.tx_netnew_yoyd,
             init.tx_netnew_yoyc.value = init.tx_netnew_yoyc,
             init.tx_netnew_yoy.value = val) %>% 
      select(-pd, -init.tx_netnew_yoy.value)
    
  rm(headers, pds, qtr, qtr_fltr)
  