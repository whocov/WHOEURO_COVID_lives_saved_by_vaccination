# Script to assign VE initial values, lag times and vaccine waning times depending on which scenario is run

calc.ve.by.variant.sensitivity <- function(sensitivity, VE_values) {
  
  source("R/0_calc_ve_by_variant.R")
  
  
  ## Reshape VE values table to match code
  # Original analysis
  VE_values_original <- VE_values %>% 
    select(Variant, Dose, VE) %>% 
    pivot_wider(names_from = Variant, 
                values_from = VE) %>% 
    mutate(Dose = trimws(Dose),
           Dose = case_when(Dose == "1" ~ "ve_fd",
                            Dose == "2" ~ "ve_sd",
                            Dose == "3" ~ "ve_ad1",
                            Dose == "4" ~ "ve_ad2")) %>% 
    add_row(Dose = "ve_ad3") %>% 
    mutate(Omicron = ifelse(Dose == "ve_ad3", Omicron[4], Omicron),
           across(c(Alpha, Delta), ~ ifelse(is.na(.), 0, .)))

  VE_values_original <- VE_values_original %>% 
    remove_rownames %>% 
    column_to_rownames(var = "Dose")
  
  
  # Sensitivity analysis 1: high VE values
  VE_values_SA1 <- VE_values %>% 
    select(Variant, Dose, Upper_CI) %>% 
    pivot_wider(names_from = Variant, 
                values_from = Upper_CI) %>% 
    mutate(Dose = trimws(Dose),
           Dose = case_when(Dose == "1" ~ "ve_fd",
                            Dose == "2" ~ "ve_sd",
                            Dose == "3" ~ "ve_ad1",
                            Dose == "4" ~ "ve_ad2")) %>% 
    add_row(Dose = "ve_ad3") %>% 
    mutate(Omicron = ifelse(Dose == "ve_ad3", Omicron[4], Omicron),
           across(c(Alpha, Delta), ~ ifelse(is.na(.), 0, .)))
  
  VE_values_SA1 <- VE_values_SA1 %>% 
    remove_rownames %>% 
    column_to_rownames(var = "Dose")
  
  # Sensitivity analysis 2: low VE values
  VE_values_SA2 <- VE_values %>% 
    select(Variant, Dose, Lower_CI) %>% 
    pivot_wider(names_from = Variant, 
                values_from = Lower_CI) %>% 
    mutate(Dose = trimws(Dose),
           Dose = case_when(Dose == "1" ~ "ve_fd",
                            Dose == "2" ~ "ve_sd",
                            Dose == "3" ~ "ve_ad1",
                            Dose == "4" ~ "ve_ad2")) %>% 
    add_row(Dose = "ve_ad3") %>% 
    mutate(Omicron = ifelse(Dose == "ve_ad3", Omicron[4], Omicron),
           across(c(Alpha, Delta), ~ ifelse(is.na(.), 0, .)))
  
  VE_values_SA2 <- VE_values_SA2 %>% 
    remove_rownames %>% 
    column_to_rownames(var = "Dose")
  
  
  if(sensitivity == "no" | str_detect(sensitivity, "^yes_prior_immunity")){
    weeks.lag.1 = 2 # after first dose
    weeks.lag.2 = 1 # after second dose
    weeks.lag.3 = 1 # after first additional dose
    weeks.lag.4 = 1 # after second additional dose
    weeks.lag.5 = 1 # after third additional dose
    ve_variant_data <- tibble(Variant = c("Index", "Alpha", "Delta", "Omicron"),
                              ve_fd   = c(0, VE_values_original$Alpha[1], VE_values_original$Delta[1], VE_values_original$Omicron[1]),
                              ve_sd   = c(0, VE_values_original$Alpha[2], VE_values_original$Delta[2], VE_values_original$Omicron[2]),
                              ve_ad1  = c(0, VE_values_original$Alpha[2], VE_values_original$Delta[3], VE_values_original$Omicron[3]),
                              ve_ad2  = c(0, VE_values_original$Alpha[2], VE_values_original$Delta[3], VE_values_original$Omicron[4]),
                              ve_ad3  = c(0, VE_values_original$Alpha[2], VE_values_original$Delta[3], VE_values_original$Omicron[5]))
    weekly_waning = 0.9976 # Waning of each vaccination dose

  } else {
    
    if(sensitivity == "yes_VE_high") {
      ve_variant_data <- tibble(Variant = c("Index", "Alpha", "Delta", "Omicron"),
                                ve_fd   = c(0, VE_values_SA1$Alpha[1], VE_values_SA1$Delta[1], VE_values_SA1$Omicron[1]),
                                ve_sd   = c(0, VE_values_SA1$Alpha[2], VE_values_SA1$Delta[2], VE_values_SA1$Omicron[2]),
                                ve_ad1  = c(0, VE_values_SA1$Alpha[2], VE_values_SA1$Delta[3], VE_values_SA1$Omicron[3]),
                                ve_ad2  = c(0, VE_values_SA1$Alpha[2], VE_values_SA1$Delta[3], VE_values_SA1$Omicron[4]),
                                ve_ad3  = c(0, VE_values_SA1$Alpha[2], VE_values_SA1$Delta[3], VE_values_SA1$Omicron[5]))
      weeks.lag.1 = 2 # after first dose
      weeks.lag.2 = 1 # after second dose
      weeks.lag.3 = 1 # after first additional dose
      weeks.lag.4 = 1 # after second additional dose
      weeks.lag.5 = 1 # after third additional dose
      weekly_waning = 0.9976 # Waning of each vaccination dose

    }
    
    if(sensitivity == "yes_VE_low") {
      ve_variant_data <- tibble(Variant = c("Index", "Alpha", "Delta", "Omicron"),
                                ve_fd   = c(0, VE_values_SA2$Alpha[1], VE_values_SA2$Delta[1], VE_values_SA2$Omicron[1]),
                                ve_sd   = c(0, VE_values_SA2$Alpha[2], VE_values_SA2$Delta[2], VE_values_SA2$Omicron[2]),
                                ve_ad1  = c(0, VE_values_SA2$Alpha[2], VE_values_SA2$Delta[3], VE_values_SA2$Omicron[3]),
                                ve_ad2  = c(0, VE_values_SA2$Alpha[2], VE_values_SA2$Delta[3], VE_values_SA2$Omicron[4]),
                                ve_ad3  = c(0, VE_values_SA2$Alpha[2], VE_values_SA2$Delta[3], VE_values_SA2$Omicron[5]))
      weeks.lag.1 = 2 # after first dose
      weeks.lag.2 = 1 # after second dose
      weeks.lag.3 = 1 # after first additional dose
      weeks.lag.4 = 1 # after second additional dose
      weeks.lag.5 = 1 # after third additional dose
      weekly_waning = 0.9976 # Waning of each vaccination dose

    }
    
    if(sensitivity == "yes_lag_short"){
      ve_variant_data <- tibble(Variant = c("Index", "Alpha", "Delta", "Omicron"),
                                ve_fd   = c(0, VE_values_original$Alpha[1], VE_values_original$Delta[1], VE_values_original$Omicron[1]),
                                ve_sd   = c(0, VE_values_original$Alpha[2], VE_values_original$Delta[2], VE_values_original$Omicron[2]),
                                ve_ad1  = c(0, VE_values_original$Alpha[2], VE_values_original$Delta[3], VE_values_original$Omicron[3]),
                                ve_ad2  = c(0, VE_values_original$Alpha[2], VE_values_original$Delta[3], VE_values_original$Omicron[4]),
                                ve_ad3  = c(0, VE_values_original$Alpha[2], VE_values_original$Delta[3], VE_values_original$Omicron[5]))
      weeks.lag.1 = 1# after first dose
      weeks.lag.2 = 1# after second dose
      weeks.lag.3 = 1 # after first additional dose
      weeks.lag.4 = 1 # after second additional dose
      weeks.lag.5 = 1 # after third additional dose
      weekly_waning = 0.9976 # Waning of each vaccination dose

    }
    
    if(sensitivity == "yes_lag_long"){
      ve_variant_data <- ve_variant_data
      ve_variant_data <- tibble(Variant = c("Index", "Alpha", "Delta", "Omicron"),
                                ve_fd   = c(0, VE_values_original$Alpha[1], VE_values_original$Delta[1], VE_values_original$Omicron[1]),
                                ve_sd   = c(0, VE_values_original$Alpha[2], VE_values_original$Delta[2], VE_values_original$Omicron[2]),
                                ve_ad1  = c(0, VE_values_original$Alpha[2], VE_values_original$Delta[3], VE_values_original$Omicron[3]),
                                ve_ad2  = c(0, VE_values_original$Alpha[2], VE_values_original$Delta[3], VE_values_original$Omicron[4]),
                                ve_ad3  = c(0, VE_values_original$Alpha[2], VE_values_original$Delta[3], VE_values_original$Omicron[5]))
      weeks.lag.1 = 3# after first dose
      weeks.lag.2 = 2# after second dose
      weeks.lag.3 = 2# after first additional dose
      weeks.lag.4 = 2# after second additional dose
      weeks.lag.5 = 2# after third additional dose
      weekly_waning = 0.9976 # Waning of each vaccination dose

    }
    
    if(sensitivity == "yes_waning_fast"){
      ve_variant_data <- ve_variant_data
      ve_variant_data <- tibble(Variant = c("Index", "Alpha", "Delta", "Omicron"),
                                ve_fd   = c(0, VE_values_original$Alpha[1], VE_values_original$Delta[1], VE_values_original$Omicron[1]),
                                ve_sd   = c(0, VE_values_original$Alpha[2], VE_values_original$Delta[2], VE_values_original$Omicron[2]),
                                ve_ad1  = c(0, VE_values_original$Alpha[2], VE_values_original$Delta[3], VE_values_original$Omicron[3]),
                                ve_ad2  = c(0, VE_values_original$Alpha[2], VE_values_original$Delta[3], VE_values_original$Omicron[4]),
                                ve_ad3  = c(0, VE_values_original$Alpha[2], VE_values_original$Delta[3], VE_values_original$Omicron[5]))
      weeks.lag.1 = 2# after first dose
      weeks.lag.2 = 2# after second dose
      weeks.lag.3 = 2# after first additional dose
      weeks.lag.4 = 1 # after second additional dose
      weeks.lag.5 = 1 # after third additional dose
      weekly_waning = 0.993 # Waning of each vaccination dose

    }
    
    if(sensitivity == "yes_waning_slow"){
      ve_variant_data <- ve_variant_data
      ve_variant_data <- tibble(Variant = c("Index", "Alpha", "Delta", "Omicron"),
                                ve_fd   = c(0, VE_values_original$Alpha[1], VE_values_original$Delta[1], VE_values_original$Omicron[1]),
                                ve_sd   = c(0, VE_values_original$Alpha[2], VE_values_original$Delta[2], VE_values_original$Omicron[2]),
                                ve_ad1  = c(0, VE_values_original$Alpha[2], VE_values_original$Delta[3], VE_values_original$Omicron[3]),
                                ve_ad2  = c(0, VE_values_original$Alpha[2], VE_values_original$Delta[3], VE_values_original$Omicron[4]),
                                ve_ad3  = c(0, VE_values_original$Alpha[2], VE_values_original$Delta[3], VE_values_original$Omicron[5]))
      weeks.lag.1 = 2 # after first dose
      weeks.lag.2 = 1 # after second dose
      weeks.lag.3 = 1 # after first additional dose
      weeks.lag.4 = 1 # after second additional dose
      weeks.lag.5 = 1 # after third additional dose
      weekly_waning = 0.999 # Waning of each vaccination dose

    }
    
    if(sensitivity == "no_waning"){
      ve_variant_data <- ve_variant_data
      ve_variant_data <- tibble(Variant = c("Index", "Alpha", "Delta", "Omicron"),
                                ve_fd   = c(0, VE_values_original$Alpha[1], VE_values_original$Delta[1], VE_values_original$Omicron[1]),
                                ve_sd   = c(0, VE_values_original$Alpha[2], VE_values_original$Delta[2], VE_values_original$Omicron[2]),
                                ve_ad1  = c(0, VE_values_original$Alpha[2], VE_values_original$Delta[3], VE_values_original$Omicron[3]),
                                ve_ad2  = c(0, VE_values_original$Alpha[2], VE_values_original$Delta[3], VE_values_original$Omicron[4]),
                                ve_ad3  = c(0, VE_values_original$Alpha[2], VE_values_original$Delta[3], VE_values_original$Omicron[5]))
      weeks.lag.1 = 2 # after first dose
      weeks.lag.2 = 1 # after second dose
      weeks.lag.3 = 1 # after first additional dose
      weeks.lag.4 = 1 # after second additional dose
      weeks.lag.5 = 1 # after third additional dose
      weekly_waning = 1 # Waning of each vaccination dose

    }
    
    if(sensitivity == "no_waning_original_ve"){
      ve_variant_data <- ve_variant_data
      ve_variant_data <- tibble(Variant = c("Index", "Alpha", "Delta", "Omicron"),
                                ve_fd   = c(0, VE_values_original$Alpha[1], VE_values_original$Delta[1], VE_values_original$Omicron[1]),
                                ve_sd   = c(0, VE_values_original$Alpha[2], VE_values_original$Delta[2], VE_values_original$Omicron[2]),
                                ve_ad1  = c(0, VE_values_original$Alpha[2], VE_values_original$Delta[3], VE_values_original$Omicron[3]),
                                ve_ad2  = c(0, VE_values_original$Alpha[2], VE_values_original$Delta[3], VE_values_original$Omicron[4]),
                                ve_ad3  = c(0, VE_values_original$Alpha[2], VE_values_original$Delta[3], VE_values_original$Omicron[5]))
      weeks.lag.1 = 2 # after first dose
      weeks.lag.2 = 1 # after second dose
      weeks.lag.3 = 1 # after first additional dose
      weeks.lag.4 = 1 # after second additional dose
      weeks.lag.5 = 1 # after third additional dose
      weekly_waning = 1 # Waning of each vaccination dose

    }
    

  }
    # Calculate VE by variant
    ve_by_variant <- calc_ve_by_variant(weeks.lag.1, weeks.lag.2, weeks.lag.3, weeks.lag.4, weeks.lag.5, ve_variant_data, weekly_waning)
    
    return(ve_by_variant)
}




