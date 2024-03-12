# Script to calculate VE values according to variant 


calc_ve_by_variant <- function(lagdose1 = weeks.lag.1, 
                               lagdose2 = weeks.lag.2, 
                               lagdose3 = weeks.lag.3, 
                               lagdose4 = weeks.lag.4, 
                               lagdose5 = weeks.lag.5, 
                               ve_variant_data, 
                               weekly_waning) {
  
  ve_by_variant <- ve_variant_data
  
  if (lagdose1 > 0) {
    for (i in 1:lagdose1) {
      ve_by_variant[[glue("ve_fd_{i}")]] <- 0
    }
  }
  if (lagdose2 > 0) {
    for (i in 1:lagdose2) {
      ve_by_variant[[glue("ve_sd_{i}")]] <- ve_by_variant$ve_fd
    }
  }
  if (lagdose3 > 0) {
    for (i in 1:lagdose3) {
      ve_by_variant[[glue("ve_ad1_{i}")]] <- ve_by_variant$ve_sd
    }
  }
  if (lagdose4 > 0) {
    for (i in 1:lagdose4) {
      ve_by_variant[[glue("ve_ad2_{i}")]] <- ve_by_variant$ve_ad1
    }
  }
  if (lagdose5 > 0) {
    for (i in 1:lagdose5) {
      ve_by_variant[[glue("ve_ad3_{i}")]] <- ve_by_variant$ve_ad2
    }
  }
  
  ve_by_variant[[glue("ve_fd_{lagdose1 + 1}")]] <- ve_by_variant$ve_fd
  ve_by_variant[[glue("ve_sd_{lagdose2 + 1}")]] <- ve_by_variant$ve_sd
  ve_by_variant[[glue("ve_ad1_{lagdose3 + 1}")]] <- ve_by_variant$ve_ad1
  ve_by_variant[[glue("ve_ad2_{lagdose4 + 1}")]] <- ve_by_variant$ve_ad2
  ve_by_variant[[glue("ve_ad3_{lagdose5 + 1}")]] <- ve_by_variant$ve_ad3
  
  for (i in (lagdose1+2):208) {
    ve_by_variant[[glue("ve_fd_{i}")]] <- ve_by_variant[[glue("ve_fd_{i-1}")]] * weekly_waning
  }
  for (i in (lagdose2+2):208) {
    ve_by_variant[[glue("ve_sd_{i}")]] <- ve_by_variant[[glue("ve_sd_{i-1}")]] * weekly_waning
  }
  for (i in (lagdose3+2):208) {
    ve_by_variant[[glue("ve_ad1_{i}")]] <- ve_by_variant[[glue("ve_ad1_{i-1}")]] * weekly_waning
  }
  for (i in (lagdose4+2):208) {
    ve_by_variant[[glue("ve_ad2_{i}")]] <- ve_by_variant[[glue("ve_ad2_{i-1}")]] * weekly_waning
  }
  for (i in (lagdose5+2):208) {
    ve_by_variant[[glue("ve_ad3_{i}")]] <- ve_by_variant[[glue("ve_ad3_{i-1}")]] * weekly_waning
  }
  
  ve_by_variant <- ve_by_variant %>% select(Variant, starts_with(c("ve_fd_", "ve_sd_", "ve_ad1_", "ve_ad2_", "ve_ad3_")))
  return(ve_by_variant)
}