

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

#HTS_TST_POS Position relative to OU-leve median (median last three quarters)
  
  #pull headers to figure out last three quarters to keep  
    headers <- names(df_site) 
    pds <- headers[stringr::str_detect(headers, "q(?=[:digit:])")] %>% 
      tail(., n =3)
  #
    ci_hts_pos_ou_comp <- df_site %>% 
      filter(indicator == "HTS_TST_POS", standardizeddisaggregate == "Total Numerator") %>% 
      select(operatingunit, psnu,sitename, orgunituid, pds)
    ci_hts_pos_ou_comp <- ci_hts_pos_ou_comp %>% 
      gather(pd, val, starts_with("fy"), na.rm = TRUE) %>% 
      select(-pd) %>% 
      group_by_if(is.character) %>% 
      summarise_if(is.numeric, ~ sum(., na.rm = TRUE)) %>% 
      ungroup()
    