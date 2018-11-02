
# LINKAGE -----------------------------------------------------------------

#1. Proxy Linkage
  
  #pull headers to figure out last three quarters to keep  
    headers <- names(df_site) 
    pds <- headers[stringr::str_detect(headers, "q(?=[:digit:])")] %>% 
      tail(., n =3)
    
  #narrow down to data needed for indicator creation
    lnk_val <- df_site %>% 
      filter(indicator %in% c("HTS_TST_POS", "TX_NEW"),
             standardizeddisaggregate == "Total Numerator",
             typemilitary == "N") %>% 
      select(operatingunit, psnu,sitename, orgunituid, indicator, pds) %>% 
      filter_at(vars(contains("q")), any_vars(!is.na(.) & .!=0)) #remove if all quarters are missing
    
  #create linkage value
    lnk_val <- lnk_val %>% 
      gather(pd, val, starts_with("fy")) %>% 
      select(-pd) %>% 
      group_by_if(is.character) %>% 
      summarise_if(is.numeric, ~ sum(., na.rm = TRUE)) %>% 
      ungroup() %>% 
      spread(indicator, val) %>% 
      mutate(linkage = TX_NEW / HTS_TST_POS,
             linkage = ifelse(!is.finite(linkage), 0, linkage))
    
  #Calculate percentile grouping
    lnk_val <- lnk_val %>%
      group_by(operatingunit) %>% 
      mutate(lnk.proxylinkage.score = case_when(linkage < quantile(linkage, .25) ~ 2,
                                                linkage < quantile(linkage, .50) ~ 1,
                                                TRUE                             ~ 0)) %>% 
      ungroup() %>%
      rename(lnk.proxylinkage.value = linkage) %>% 
      select(-HTS_TST_POS, -TX_NEW)
    
      
#4. & 5. Number of quarters without decrease/increase
  #narrow down to data needed for indicator creation
    lnk_chng <- df_site %>% 
      filter(indicator %in% c("HTS_TST_POS", "TX_NEW"),
             standardizeddisaggregate == "Total Numerator",
             typemilitary == "N") %>% 
      select(operatingunit, psnu,sitename, orgunituid, indicator, contains("q")) %>% 
      filter_at(vars(contains("q")), any_vars(!is.na(.) & .!=0)) #remove if all quarters are missing
  
  #create linkage value
    lnk_chng <- lnk_chng %>% 
      #get all indicator values on same rows
      group_by_if(is.character) %>% 
      summarise_if(is.numeric, ~ sum(., na.rm = TRUE)) %>% 
      ungroup %>% 
      gather(pd, val, starts_with("fy")) %>% 
      spread(indicator, val) %>% 
      mutate(linkage = TX_NEW / HTS_TST_POS,
             linkage = ifelse(!is.finite(linkage), 0, linkage))
  #change
    lnk_chng <- lnk_chng %>% 
      group_by(operatingunit, psnu, sitename, orgunituid) %>% 
      mutate(link_qtrdiff_pos = ifelse(linkage - lag(linkage) > 0, 1, 0),
             link_qtrdiff_neg = ifelse(linkage - lag(linkage) < 0, 1, 0))%>% 
      ungroup()
  
    lnk_chng <- lnk_chng %>% 
      group_by(operatingunit, psnu, sitename, orgunituid) %>% 
      summarise_at(vars(contains("link")), ~ sum(., na.rm = TRUE)) %>% 
      ungroup()
    
    lnk_chng <- lnk_chng %>%
      group_by(operatingunit) %>% 
      mutate(lnk.lnk_pos_chng.score = case_when(link_qtrdiff_pos > quantile(link_qtrdiff_pos, .75) ~ 2,
                                                link_qtrdiff_pos > quantile(link_qtrdiff_pos, .50) ~ 1,
                                                TRUE                                               ~ 0),
             lnk.lnk_neg_chng.score = case_when(link_qtrdiff_neg > quantile(link_qtrdiff_neg, .75) ~ 2,
                                                link_qtrdiff_neg > quantile(link_qtrdiff_neg, .50) ~ 1,
                                                TRUE                                               ~ 0)) %>% 
      ungroup() %>%
      rename(lnk.lnk_pos_chng.value = link_qtrdiff_pos,
             lnk.lnk_neg_chng.value = link_qtrdiff_neg) %>% 
      select(-linkage)
    
    rm(headers, pds)
    