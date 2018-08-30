

# SETUP -------------------------------------------------------------------

library(tidyverse)

path <- "~/ICPI/Data"
ou <- "Malawi"

# IMPORT DATASET ----------------------------------------------------------
  
  #strucutre filepath from inputs
    filepath <- file.path(path, paste0("MER_Structured_Dataset_SITE_IM_FY17-18_20180815_v1_1_", ou, ".Rds"))
  #open file
    df_site <- read_rds(filepath)


# CASE IDENTIFICATION -----------------------------------------------------

#1.&2. HTS_TST_POS Position relative to OU-/PSNU-level median (median last three quarters)
  
  #pull headers to figure out last three quarters to keep  
    headers <- names(df_site) 
    pds <- headers[stringr::str_detect(headers, "q(?=[:digit:])")] %>% 
      tail(., n =3)
  #narrow down to data needed for indicator creation
    ci_hts_pos <- df_site %>% 
      filter(indicator == "HTS_TST_POS", standardizeddisaggregate == "Total Numerator",
             is.na(typemilitary)) %>% 
      select(operatingunit, psnu,sitename, orgunituid, pds)
  #site sum over last 3 pds
    ci_hts_pos <- ci_hts_pos %>% 
      gather(pd, val, starts_with("fy"), na.rm = TRUE) %>% 
      select(-pd) %>% 
      group_by_if(is.character) %>% 
      summarise_if(is.numeric, ~ sum(., na.rm = TRUE)) %>% 
      ungroup()
  #Calculate percentile grouping
    ci_hts_pos <- ci_hts_pos %>%
      group_by(operatingunit) %>% 
      mutate(ci.hts_pos_ou_score = case_when(val > quantile(val, .75) ~ 2,
                                             val > quantile(val, .50) ~ 1,
                                             TRUE                     ~ 0)) %>% 
      ungroup() %>%
      group_by(operatingunit, psnu) %>% 
      mutate(ci.hts_pos_psnu_score = case_when(val > quantile(val, .75) ~ 2,
                                               val > quantile(val, .50) ~ 1,
                                               TRUE                     ~ 0)) %>% 
      ungroup() %>% 
      rename(ci_hts_pos_vol = val)
    
#3. Year on Year change in volume
    
    qtr <- ICPIutilities::identifypd(df_site, "quarter")
    # curr_yr <- paste0("fy", ICPIutilities::identifypd(df_site, "year"))
    # prior_yr <- paste0("fy", ICPIutilities::identifypd(df_site, "year")-1)
      
    qtr_fltr <- case_when(qtr == 1 ~ "1",
                          qtr == 2 ~ "(1|2)",
                          qtr == 3 ~ "(1|2|3)",
                          qtr == 4 ~ "(?=[:digit:])")
    
    pds <- headers[stringr::str_detect(headers, paste0("q", qtr_fltr))]
    
    ci_hts_pos_yoy <- df_site %>% 
      filter(indicator == "HTS_TST_POS", standardizeddisaggregate == "Total Numerator",
             is.na(typemilitary)) %>% 
      select(operatingunit, psnu,sitename, orgunituid, pds) 
    
    ci_hts_pos_yoy <- ci_hts_pos_yoy %>% 
      gather(pd, val, starts_with("fy"), na.rm = TRUE) %>% 
      mutate(pd = str_remove_all(pd, "q[:digit:]")) %>% 
      group_by_if(is.character) %>% 
      summarise_if(is.numeric, ~ sum(., na.rm = TRUE)) %>% 
      ungroup() %>% 
      spread(pd, val, fill = 0)
          
    ci_hts_pos_yoy <- ci_hts_pos_yoy %>% 
      mutate(ci_hts_pos_yoy = (!!curr_yr - !!prior_yr) / !!prior_yr)
      
      
    
    ci_hts_pos_yoy <- df_site %>% 
      filter(indicator == "HTS_TST_POS", standardizeddisaggregate == "Total Numerator",
             is.na(typemilitary)) %>% 
      select(operatingunit, psnu,sitename, orgunituid, pds) 
    
    ci_hts_pos_yoy <- ci_hts_pos_yoy %>% 
      gather(pd, val, starts_with("fy")) %>% 
      mutate(pd = str_remove_all(pd, "q[:digit:]")) %>% 
      group_by_if(is.character) %>% 
      summarise_if(is.numeric, ~ sum(., na.rm = TRUE)) %>% 
      #ungroup() %>% 
      #group_by_if(is.character) %>% 
      mutate(ci_hts_pos_yoy = (val - dplyr::lag(val))/dplyr::lag(val)) %>% 
      ungroup() %>% 
      filter(pd == "fy2018", is.finite(ci_hts_pos_yoy))

  #Calculate percentile grouping
    ci_hts_pos_yoy <- ci_hts_pos_yoy %>%
      group_by(operatingunit) %>% 
      mutate(ci_hts_pos_yoy_score = case_when(val > quantile(ci_hts_pos_yoy, .75) ~ 2,
                                              val > quantile(ci_hts_pos_yoy, .50) ~ 1,
                                              TRUE                                ~ 0)) %>% 
      ungroup()
     
      
      
      