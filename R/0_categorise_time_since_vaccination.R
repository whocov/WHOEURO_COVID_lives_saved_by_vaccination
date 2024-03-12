

categorise.time.since.vaccination <- function(vacc_data) {
  
  # initial data preparation
  categorised_data <- vacc_data %>%
    mutate(across(c(DoseAdditional1, DoseAdditional2, DoseAdditional3, FirstDose, SecondDose), ~ replace_na(.x, 0))) %>% 
    arrange(ReportCountry, TargetGroup, year_week) %>%
    group_by(ReportCountry, TargetGroup) %>%
    mutate(total_ad3 = cumsum(DoseAdditional3),
           total_ad2 = pmax(cumsum(DoseAdditional2), cumsum(DoseAdditional3)),
           total_ad1 = pmax(cumsum(DoseAdditional1), cumsum(DoseAdditional2), cumsum(DoseAdditional3)),
           total_sd = pmax(cumsum(SecondDose), cumsum(DoseAdditional1), cumsum(DoseAdditional2), cumsum(DoseAdditional3)))

  # categorise time since additional dose 3
  for (i in 208:1) {
    week_start <- i-1
    week_end <- i
    categorised_data <- categorised_data %>% 
      mutate("ad3_{i}" := cumsum(lag(DoseAdditional3, week_start, default = 0)) - cumsum(lag(DoseAdditional3, week_end, default = 0)))
  }
  
  # categorise time since additional dose 2 subtracting additional dose 1
  for (i in 208:1) {
    week_start <- i-1
    week_end <- i
    categorised_data <- categorised_data %>% 
      mutate(ad2 = cumsum(lag(DoseAdditional2, week_start, default = 0)) - cumsum(lag(DoseAdditional2, week_end, default = 0)),
             "ad2_{i}" := pmax(ad2 - total_ad3, 0),
             total_ad3 = pmax(total_ad3 - ad2, 0)
      )
  }

  # categorise time since additional dose 1 subtracting additional dose 2
  for (i in 208:1) {
    week_start <- i-1
    week_end <- i
    categorised_data <- categorised_data %>% 
      mutate(ad1 = cumsum(lag(DoseAdditional1, week_start, default = 0)) - cumsum(lag(DoseAdditional1, week_end, default = 0)),
             "ad1_{i}" := pmax(ad1 - total_ad2, 0),
             total_ad2 = pmax(total_ad2 - ad1, 0)
             )
  }
  
  # categorise time since second dose subtracting those receiving additional dose 1
  for (i in 208:1) {
    week_start <- i-1
    week_end <- i
    categorised_data <- categorised_data %>% 
      mutate(sd = cumsum(lag(SecondDose, week_start, default = 0)) - cumsum(lag(SecondDose, week_end, default = 0)),
             "sd_{i}" := pmax(sd - total_ad1, 0),
             total_ad1 = pmax(total_ad1 - sd, 0))
  }
  
  # categorise time since first dose subtracting those receiving a second dose
  for (i in 208:1) {
    week_start <- i-1
    week_end <- i
    categorised_data <- categorised_data %>% 
      mutate(fd = cumsum(lag(FirstDose, week_start, default = 0)) - cumsum(lag(FirstDose, week_end, default = 0)),
             "fd_{i}" := pmax(fd - total_sd, 0),
             total_sd = pmax(total_sd - fd, 0))
  }
  
  categorised_data <- categorised_data %>%     
    mutate(total_ad3 = cumsum(DoseAdditional3),
           total_ad2 = pmax(cumsum(DoseAdditional2),cumsum(DoseAdditional3)),
           total_ad1 = pmax(cumsum(DoseAdditional1), cumsum(DoseAdditional2), cumsum(DoseAdditional3)),
           total_sd = pmax(cumsum(SecondDose), cumsum(DoseAdditional1), cumsum(DoseAdditional2), cumsum(DoseAdditional3)),
           total_fd = pmax(cumsum(FirstDose), cumsum(SecondDose), cumsum(DoseAdditional1), cumsum(DoseAdditional2), cumsum(DoseAdditional3))) %>%
    ungroup()

  # check that sum of additional dose categories is equal to cumulative sum of additional dose
  categorised_data %>% mutate(sum_ad3 = rowSums(select(., starts_with("ad3_")))) %>% 
    filter(total_ad3 != sum_ad3) %>% 
    {assert_that(nrow(.) == 0)}
  
  categorised_data %>% mutate(sum_ad2 = rowSums(select(., starts_with(c("ad3_", "ad2_"))))) %>% 
    filter(total_ad2 != sum_ad2) %>%
    {assert_that(nrow(.) == 0)}
  
  categorised_data %>% mutate(sum_ad1 = rowSums(select(., starts_with(c("ad3_", "ad2_", "ad1_"))))) %>% 
    filter(total_ad1 != sum_ad1) %>%
    {assert_that(nrow(.) == 0)}
  
  
  # check that sum of second dose and additional dose categories is equal to cumulative sum
  # of second dose
  categorised_data %>% mutate(sum_sd = rowSums(select(., starts_with(c("ad3_", "ad2_", "ad1_", "sd_"))))) %>% 
    filter(total_sd != sum_sd) %>%
    {assert_that(nrow(.) == 0)}
  
  # check that sum of first, second, and additional dose categories is equal to cumulative sum
  # of first dose
  categorised_data %>% mutate(sum_fd = rowSums(select(., starts_with(c("ad3_", "ad2_","ad1_","sd_", "fd_"))))) %>% 
    filter(total_fd != sum_fd) %>%
    {assert_that(nrow(.) == 0)}
  
  # check that all data is within 4 year period
  assert_that(max(categorised_data$fd_208, categorised_data$sd_208, categorised_data$ad1_208, categorised_data$ad2_208, categorised_data$ad3_208) == 0)
  
  categorised_data <- categorised_data %>% ungroup() %>% select(-c(total_ad3, total_ad2, total_ad1, total_sd, sd, fd, ad1))
  
  return(categorised_data)
}







