sites <- df_site %>% 
  distinct(sitename, operatingunit, psnu, snuprioritization)

combo <- left_join(sites, ci_hts_pos)
combo <- left_join(combo, ci_hts_pos_yoy)
combo <- left_join(combo, init_tx_new)
combo <- left_join(combo, init_tx_new_yoy)
combo <- left_join(combo, lnk)
combo <- left_join(combo, lnk_val)
combo <- left_join(combo, lnk_chng)

combo <- combo %>% 
  filter_at(vars(starts_with("ci"), starts_with("init"), starts_with("lnk")), any_vars(!is.na(.) & .!=0))
