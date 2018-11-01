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


# CREATE SCORES -----------------------------------------------------------

  source("R/ci_indicators.r")
  source("R/init_indicators.r")
  source("R/lnk_indicators.r")
  source("R/prfm_indicators.r")


# COMBINE -----------------------------------------------------------------

sites <- df_site %>% 
  distinct(sitename, sitetype, operatingunit, psnu, snuprioritization)

combo <- left_join(sites, ci_hts_pos)
combo <- left_join(combo, ci_hts_pos_yoy)
combo <- left_join(combo, init_tx_new)
combo <- left_join(combo, init_tx_new_yoy)
combo <- left_join(combo, lnk_val)
combo <- left_join(combo, lnk_chng)
combo <- left_join(combo, prfm_ind)
combo <- left_join(combo, prfm_ovc)

rm(sites, ci_hts_pos, ci_hts_pos_yoy, init_tx_new, init_tx_new_yoy, lnk_val, lnk_chng, prfm_ind, prfm_ovc)

combo <- combo %>% 
  filter_at(vars(starts_with("ci"), starts_with("init"), starts_with("lnk"), starts_with("prfm")), any_vars(!is.na(.) & .!=0))

combo <- combo %>% 
  arrange(desc(init.tx_new_psnu.value))

combo_w <- combo %>% 
  gather(metric, val, -c(sitename, operatingunit, psnu, snuprioritization, sitetype, orgunituid), na.rm = TRUE) %>% 
  separate(metric, c("grp", "metric", "type"), sep = "\\.") %>% 
  unite(metric, grp, metric, sep = ".") %>% 
  spread(metric, val)

combo_w <- combo_w %>% filter(type == "value")

write_csv(combo_w, "Output/sims_selection_KEN_DEMO_wide.csv", na = "")

# combo_l <- combo %>% 
#   gather(metric, val, -c(sitename, operatingunit, psnu, snuprioritization, sitetype, orgunituid), na.rm = TRUE) %>% 
#   separate(metric, c("grp", "metric", "type"), sep = "\\.") %>% 
#   unite(metric, grp, metric, sep = ".") %>% 
#   spread(type, val)
# 
# write_csv(combo_l, "Output/sims_selection_KEN_DEMO_long.csv", na = "")
# 
# combo <- read_csv("C:/Users/achafetz/Downloads/sims_selection_KEN_DEMO_long.csv")

