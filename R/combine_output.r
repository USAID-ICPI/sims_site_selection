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
  rm(path, ou, filepath)


# CREATE SCORES -----------------------------------------------------------

  source("R/ci_indicators.r")
  source("R/init_indicators.r")
  source("R/lnk_indicators.r")
  source("R/prfm_indicators.r")
  source("R/stat_indicators.r")


# COMBINE -----------------------------------------------------------------

#pull site info
  sites <- df_site %>% 
    distinct(sitename, sitetype, operatingunit, psnu, snuprioritization, orgunituid)

#join
  combo <- 
    left_join(sites, ci_hts_pos) %>% 
    left_join(., ci_hts_pos_yoy) %>% 
    left_join(., ci_index) %>% 
    left_join(., init_tx_new) %>% 
    left_join(., init_tx_new_yoy) %>% 
    left_join(., init_tx_netnew_yoy) %>% 
    left_join(., lnk_val) %>% 
    left_join(., lnk_chng) %>% 
    left_join(., prfm_ind) %>% 
    left_join(., stat_ovc) %>% 
    left_join(., stat_oth)
  
  rm(sites, ci_hts_pos, ci_hts_pos_yoy, ci_index, init_tx_new, init_tx_new_yoy, init_tx_netnew_yoy, lnk_val, lnk_chng, prfm_ind, stat_oth, stat_ovc)

#remove blank rows & arrange by TX_NEW volumne
  combo <- combo %>% 
    filter_at(vars(matches("(ci|init|lnk|stat|prfm)")), any_vars(!is.na(.) & .!=0))
    arrange(desc(init.tx_new_ou.value))

#covert to wide
  combo_w <- combo %>% 
    gather(metric, val, -c(sitename, operatingunit, psnu, snuprioritization, sitetype, orgunituid), na.rm = TRUE) %>% 
    separate(metric, c("grp", "metric", "type"), sep = "\\.") %>% 
    unite(metric, grp, metric, sep = ".") %>% 
    spread(metric, val)

#export wide
combo_w %>% 
  filter(type == "value") %>% 
  write_csv("Output/sims_selection_KEN_DEMO_wide.csv", na = "")

# combo_l <- combo %>% 
#   gather(metric, val, -c(sitename, operatingunit, psnu, snuprioritization, sitetype, orgunituid), na.rm = TRUE) %>% 
#   separate(metric, c("grp", "metric", "type"), sep = "\\.") %>% 
#   unite(metric, grp, metric, sep = ".") %>% 
#   spread(type, val)
# 
# write_csv(combo_l, "Output/sims_selection_KEN_DEMO_long.csv", na = "")
# 
# combo <- read_csv("C:/Users/achafetz/Downloads/sims_selection_KEN_DEMO_long.csv")

