sites <- df_site %>% 
  distinct(sitename, sitetype, operatingunit, psnu, snuprioritization)

combo <- left_join(sites, ci_hts_pos)
combo <- left_join(combo, ci_hts_pos_yoy)
combo <- left_join(combo, init_tx_new)
combo <- left_join(combo, init_tx_new_yoy)
combo <- left_join(combo, lnk_val)
combo <- left_join(combo, lnk_chng)

rm(sites, ci_hts_pos, ci_hts_pos_yoy, init_tx_new, init_tx_new_yoy, lnk_val, lnk_chng)

combo <- combo %>% 
  filter_at(vars(starts_with("ci"), starts_with("init"), starts_with("lnk")), any_vars(!is.na(.) & .!=0))

combo_w <- combo %>% 
  gather(metric, val, -c(sitename, operatingunit, psnu, snuprioritization, sitetype, orgunituid), na.rm = TRUE) %>% 
  separate(metric, c("grp", "metric", "type"), sep = "\\.") %>% 
  unite(metric, grp, metric, sep = ".") %>% 
  spread(metric, val)

combo_w <- combo_w %>% filter(type == "value")

write_csv(combo_w, "Output/sims_selection_KEN_DEMO_wide.csv", na = "")

combo_l <- combo %>% 
  gather(metric, val, -c(sitename, operatingunit, psnu, snuprioritization, sitetype, orgunituid), na.rm = TRUE) %>% 
  separate(metric, c("grp", "metric", "type"), sep = "\\.") %>% 
  unite(metric, grp, metric, sep = ".") %>% 
  spread(type, val)

write_csv(combo_l, "Output/sims_selection_KEN_DEMO_long.csv", na = "")

combo <- read_csv("C:/Users/achafetz/Downloads/sims_selection_KEN_DEMO_long.csv")

