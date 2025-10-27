summarise_IndicatorDist <- function(indSim,
                                    ind_id,
                                    dataset_name,
                                    savePath){
  
  ## Rename columns if required
  if("value_sc" %in% colnames(indSim)){
    indSim <- indSim %>%
      dplyr::rename(value = value_sc,
                    value_trunc = value_sc_trunc)
  }
  
  ## Summarise indicator value distributions per area and year and save summaries
  
  # Without truncation
  indSim_sum <- indSim %>%
    dplyr::group_by(ICunitId, ICunitName, year) %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::summarise(mean = mean(value),
                     median = median(value),
                     sd = sd(value),
                     rel_sd = sd/mean,
                     q025 = quantile(value, probs = 0.025),
                     q05 = quantile(value, probs = 0.05),
                     q25 = quantile(value, probs = 0.25),
                     q75 = quantile(value, probs = 0.75),
                     q95 = quantile(value, probs = 0.95),
                     q975 = quantile(value, probs = 0.975),
                     .groups = "keep") %>%
    dplyr::ungroup()
  
  # With truncation post-summary
  indSim_sum2 <- indSim_sum %>%
    dplyr::mutate(across(c(mean, median, q025, q05, q25, q75, q95, q975), ~ ifelse(.x > 1, 1, .x))) %>%
    rowwise() %>%
    mutate(sd_adj = ifelse(all(c_across(c(mean, median, q025, q05, q25, q75, q95, q975)) == 1), 0, sd),
           rel_sd_adj = ifelse(all(c_across(c(mean, median, q025, q05, q25, q75, q95, q975)) == 1), 0, rel_sd)) %>%
    ungroup()

  # Save
  saveRDS(indSim_sum, file = paste0(savePath, "statSummary_id_", ind_id, "_", dataset_name, ".rds"))
  saveRDS(indSim_sum2, file = paste0(savePath, "statSummary_id_", ind_id, "_", dataset_name, "truncPostSum.rds"))
  
  options(scipen = 999)
  
  indSim_sum <- indSim_sum %>%
    dplyr::mutate(across(c(mean, median, sd, rel_sd, q025, q05, q25, q75, q95, q975), ~ as.character(round(.x, digits = 5))))

  indSim_sum2 <- indSim_sum2 %>%
    dplyr::mutate(across(c(mean, median, sd, rel_sd, q025, q05, q25, q75, q95, q975), ~ as.character(round(.x, digits = 5))))
  
  readr::write_excel_csv(indSim_sum, file = paste0(savePath, "statSummary_id_", ind_id, "_", dataset_name, ".csv"))
  readr::write_excel_csv(indSim_sum2, file = paste0(savePath, "statSummary_id_", ind_id, "_", dataset_name, "truncPostSum.csv"))
  
  ## If present, summarise second set of values (truncated set) and save summaries
  if("value_trunc" %in% colnames(indSim)){
    
    indSim_sum3 <- indSim %>%
      dplyr::group_by(ICunitId, ICunitName, year) %>%
      dplyr::filter(!is.na(value_trunc)) %>%
      dplyr::summarise(mean = mean(value_trunc),
                       median = median(value_trunc),
                       sd = sd(value_trunc),
                       rel_sd = sd/mean,
                       q025 = quantile(value_trunc, probs = 0.025),
                       q05 = quantile(value_trunc, probs = 0.05),
                       q25 = quantile(value_trunc, probs = 0.25),
                       q75 = quantile(value_trunc, probs = 0.75),
                       q95 = quantile(value_trunc, probs = 0.95),
                       q975 = quantile(value_trunc, probs = 0.975),
                       .groups = "keep") %>%
      dplyr::ungroup()
    
    saveRDS(indSim_sum3, file = paste0(savePath, "statSummary_id_", ind_id, "_", dataset_name, "_truncPreSum.rds"))
    
    indSim_sum3 <- indSim_sum3 %>%
      dplyr::mutate(across(c(mean, median, sd, rel_sd, q025, q05, q25, q75, q95, q975), ~ as.character(round(.x, digits = 5))))
    
    readr::write_excel_csv(indSim_sum3, file = paste0(savePath, "statSummary_id_", ind_id, "_", dataset_name, "_truncPreSum.csv"))
  }
  
  message("Values summarised and saved for indicator Id: ", ind_id)
  
  ## Count NA scaled values that had to be removed
  na_count <- indSim %>%
    dplyr::filter(is.na(value)) %>%
    nrow()
  na_perc <- round((na_count/nrow(indSim))*100, digits = 3)
  
  if(na_count > 0){
    message(paste0(na_count, " NA values removed (sampled reference value = 0). This corresponds to ", na_perc, " % of values."))
  }
}
