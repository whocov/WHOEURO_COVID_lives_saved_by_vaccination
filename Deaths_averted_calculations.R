# Functions to calculate number of deaths averted from COVID vaccinations
# Margaux Mesle - meslem@who.int
# First created: June 2021
# Latest update: June 2023


  
calculate.expected.deaths <- function(deaths_age, 
                                     vax_clean, 
                                     variant_waves, 
                                     ve_by_variant, 
                                     seroprevalence, 
                                     countries_included,
                                     age.group, 
                                     save.data = TRUE, 
                                     prior_immunity_discount, 
                                     time_since_vacc_cats = TRUE) {
  
  # Make sure the data considers the correct weeks
  deaths_age <- deaths_age %>% 
    filter(year_week %in% reporting.weeks)
  
  vax_clean <- vax_clean %>% 
    filter(year_week %in% reporting.weeks)
  
  if(age.group == "Include finer older only"){
    # Make sure the data considers the correct weeks
    deaths_age <- deaths_age %>% 
      filter(year_week %in% reporting.weeks) %>% 
      filter(!TargetGroup == "Age 60+")
    
    vax_clean <- vax_clean %>% 
      filter(year_week %in% reporting.weeks) %>% 
      filter(!TargetGroup == "Age 60+")
    
  }
  
  # Merge vaccination and mortality data
  observed_data <- vax_clean %>%
    select(ReportCountry, year_week, FirstDose, SecondDose, DoseAdditional1, DoseAdditional2, DoseAdditional3, 
           nFirstDose, nSecondDose, nDoseAdditional1, nDoseAdditional2, nDoseAdditional3, Denominator, TargetGroup) %>%
    arrange(ReportCountry, TargetGroup, year_week) %>%
    group_by(ReportCountry, TargetGroup) %>% 
    mutate(FirstDose = ifelse(nFirstDose > Denominator, 0, FirstDose),
           nFirstDose = cumsum(FirstDose),
           SecondDose = ifelse(nSecondDose > Denominator, 0, SecondDose),
           nSecondDose = cumsum(SecondDose),
           DoseAdditional1 = ifelse(nDoseAdditional1 > Denominator, 0, DoseAdditional1),
           nDoseAdditional1 = cumsum(DoseAdditional1),
           DoseAdditional2 = ifelse(nDoseAdditional2 > Denominator, 0, DoseAdditional2),
           nDoseAdditional2 = cumsum(DoseAdditional2)) %>% 
    ungroup() %>% 
    full_join(deaths_age) %>%
    distinct() %>%
    filter(ReportCountry %in% unique(countries_included$CountryName)) %>% 
    select(ReportCountry, year_week, FirstDose, SecondDose, DoseAdditional1, DoseAdditional2, DoseAdditional3,
           nFirstDose, nSecondDose, nDoseAdditional1, nDoseAdditional2, nDoseAdditional3, 
           Denominator, DeathsObserved, TargetGroup) %>%
    arrange(ReportCountry, TargetGroup, year_week) %>%
    group_by(ReportCountry, TargetGroup) %>%
    fill(Denominator, .direction = "updown") %>%
    # remove rows with missing vaccination data occuring after start of vaccination
    mutate(include_indicator = if_else(!is.na(nFirstDose), 1L, NA_integer_)) %>%
    fill(include_indicator, .direction = "up") %>% 
    filter(!is.na(include_indicator)) %>% 
    select(!include_indicator) %>% 
    mutate(no_prior_nonmissing = if_else(!is.na(FirstDose), 1L, NA_integer_)) %>% 
    fill(no_prior_nonmissing, .direction = "down") %>% 
    # fill rows with missing vaccination data before start of vaccination
    mutate(across(c(nFirstDose, nSecondDose, nDoseAdditional1, nDoseAdditional2, nDoseAdditional3), ~ if_else(is.na(no_prior_nonmissing) & is.na(.x), 0, .x))) %>% select(!no_prior_nonmissing) %>%
    # fill other rows with missing vaccination data
    fill(nFirstDose, .direction = "down") %>%
    fill(nSecondDose, .direction = "down") %>%
    fill(nDoseAdditional1, .direction = "down") %>%
    fill(nDoseAdditional2, .direction = "down") %>%
    fill(nDoseAdditional3, .direction = "down") %>%
    ungroup()

  
  # expand variant waves to all age groups
  variant_waves_all_ages <- expand_grid(variant_waves %>% select(ReportCountry, Variant), vax_clean %>% select(TargetGroup) %>% unique()) %>% 
    full_join(variant_waves)

  # add variant based on variant waves
  observed_data <- full_join(observed_data %>% mutate(data_marker = 1), 
                             variant_waves_all_ages, 
                             by = c("year_week" = "start", "ReportCountry" = "ReportCountry", "TargetGroup" = "TargetGroup")) %>% 
    arrange(ReportCountry, TargetGroup, year_week) %>% 
    group_by(ReportCountry, TargetGroup) %>% 
    tidyr::fill(Variant, .direction = c("down")) %>%
    mutate(Variant = ifelse(is.na(Variant), "Index", as.character(Variant)),
           Variant = as.factor(Variant)) %>% 
    #mutate(Variant = replace_na("WT")) %>%
    filter(data_marker == 1) %>% 
    select(!c(data_marker, end)) %>% 
    ungroup()

  # add vaccine_effectiveness
  observed_data <- observed_data %>%
    left_join(ve_by_variant, by = c("Variant"))
  
  # categorise number of people by time since vaccination 
  observed_data <- categorise.time.since.vaccination(observed_data)

  # add in seroprevalence
  observed_data$yearmonth <- year_months_template$yearmonth[match(observed_data$year_week, year_months_template$year_week)]
  observed_data$seroprevalence <- seroprevalence$seroprevalence[match(observed_data$yearmonth, seroprevalence$yearmonth)]
  
  observed_data <- observed_data %>%
    group_by(yearmonth) %>% 
    fill(seroprevalence, .direction = "updown") %>% 
    ungroup()
  
  assert_that(all(!is.na(observed_data$seroprevalence)), msg = "Missing seroprevalence for certain year-months present in data")
  

    # estimate number of deaths averted by updated calculation (with accounting for time since vaccination or variant)
    Expected_cases_time_since_vacc <- observed_data %>%
      arrange(ReportCountry, TargetGroup, year_week) %>%
      group_by(ReportCountry, TargetGroup) %>%
      mutate(Denominator = pmax(Denominator, nFirstDose)) %>% 
      mutate(DeathsObserved = replace_na(DeathsObserved, 0),
             DeathsObserved.roll = rollmean(DeathsObserved, k = 3, align = "center", fill = NA),
             DeathsObserved.roll = if_else(is.na(DeathsObserved.roll), as.double(DeathsObserved), DeathsObserved.roll),
             across(starts_with("fd_"), ~ .x / Denominator),
             across(starts_with("sd_"), ~ .x / Denominator),
             across(starts_with("ad1_"), ~ .x / Denominator),
             across(starts_with("ad2_"), ~ .x / Denominator),
             across(starts_with("ad3_"), ~ .x / Denominator)) %>% 
      ungroup() %>% 
      mutate(across(starts_with("ve_"), ~ seroprevalence * prior_immunity_discount * .x + (1 - seroprevalence)*.x))
    
    vu_categories <- observed_data %>% 
      ungroup() %>% 
      select(starts_with(c("ad3_", "ad2_", "ad1_", "fd_", "sd_"))) %>% 
      names()
    
    for (vu_category in vu_categories) {
      Expected_cases_time_since_vacc[[paste0("vevu_", vu_category)]] <- Expected_cases_time_since_vacc[[vu_category]] * Expected_cases_time_since_vacc[[paste0("ve_", vu_category)]]
    }
   
    Expected_cases_time_since_vacc <- Expected_cases_time_since_vacc %>% 
      mutate(ad2_intermediate = rowSums(select(., starts_with("vevu_ad2_"))),
             fd_intermediate = rowSums(select(., starts_with("vevu_fd_"))),
             sd_intermediate = rowSums(select(., starts_with("vevu_sd_"))),
             ad1_intermediate = rowSums(select(., starts_with("vevu_ad1_"))),
             ad2_intermediate = rowSums(select(., starts_with("vevu_ad2_"))),
             ad3_intermediate = rowSums(select(., starts_with("vevu_ad3_"))),
             DeathsAvertedDose1 = round(DeathsObserved.roll * (fd_intermediate/(1 - fd_intermediate - sd_intermediate - ad1_intermediate - ad2_intermediate - ad3_intermediate))),
             DeathsAvertedDose2 = round(DeathsObserved.roll * (sd_intermediate/(1 - fd_intermediate - sd_intermediate - ad1_intermediate - ad2_intermediate - ad3_intermediate))),
             DeathsAvertedDose3 = round(DeathsObserved.roll * (ad1_intermediate/(1 - fd_intermediate - sd_intermediate - ad1_intermediate - ad2_intermediate - ad3_intermediate))),
             DeathsAvertedDose4 = round(DeathsObserved.roll * (ad2_intermediate/(1 - fd_intermediate - sd_intermediate - ad1_intermediate - ad2_intermediate - ad3_intermediate))),
             DeathsAvertedDose5 = round(DeathsObserved.roll * (ad3_intermediate/(1 - fd_intermediate - sd_intermediate - ad1_intermediate - ad2_intermediate - ad3_intermediate))),
             TotalAverted = DeathsAvertedDose1 + DeathsAvertedDose2 + DeathsAvertedDose3  + DeathsAvertedDose4 + DeathsAvertedDose5,
             DeathsExpected = TotalAverted + DeathsObserved) %>%
      select(!starts_with(c("vevu", "ve_", "fd_", "sd_", "ad1_", "ad2_", "ad3_"))) %>% 
      distinct() %>% 
      filter(year_week %in% reporting.weeks)

    assert_that(all(!is.na(Expected_cases_time_since_vacc$TotalAverted)), msg = "Unexpected missing values in TotalAverted")
   
    if (age.group == "25+") {
      age.grouping <- "Age 25-49"
    } else if (age.group == "50+") {
      age.grouping <- "Age 50-59"
    } else if (age.group == "60+") {
      age.grouping <- "Age 60+"
    }
      
    
    if (age.group == "ALL") {

      Expected_cases_time_since_vacc <- Expected_cases_time_since_vacc %>% 
        filter(TargetGroup %in% c("Age 25-49", "Age 50-59", "Age 60+")) %>% 
        group_by(ReportCountry, year_week) 
      
    }  else if (age.group == "Include finer older only") {
      
      Expected_cases_time_since_vacc <- Expected_cases_time_since_vacc %>% 
        filter(TargetGroup %in% c("Age 25-49", "Age 50-59", "Age 60-69", "Age 70-79", "Age 80+")) %>% 
        group_by(ReportCountry, year_week) 
      
    } else {
      
      Expected_cases_time_since_vacc <- Expected_cases_time_since_vacc %>% 
        filter(TargetGroup %in% c("Age 25-49", "Age 50-59", "Age 60+", "Age 60-69", "Age 70-79", "Age 80+"))  
    }

  if(save.data == TRUE & Period == "Vaccination") {
    if(sensitivity == "no" & !age.group %in% c("Include finer older", "Include finer older only")){
      
      write.csv(Expected_cases_time_since_vacc, 
                paste0("Outputs/Sensitivity_analyses/Expected_cases_", reporting.week, ".csv"),
                row.names = FALSE)
    } else if(sensitivity == "no" & age.group %in% c("Include finer older", "Include finer older only")){
      
      write.csv(Expected_cases_time_since_vacc, 
                paste0("Outputs/Sensitivity_analyses/Expected_cases_finer_older_", reporting.week, ".csv"),
                row.names = FALSE)
      
    } else {
      
      expected_summary <- Expected_cases_time_since_vacc %>% 
        select(ReportCountry, year_week, TargetGroup, Variant, DeathsObserved, Denominator, DeathsExpected, TotalAverted)
      
      write.csv(expected_summary, 
                paste0("Outputs/Sensitivity_analyses/Expected_cases_", sensitivity, "_", reporting.week, ".csv"),
                row.names = FALSE)
    }
  } else {
    
    if(sensitivity == "no"){
      
      write.csv(Expected_cases_time_since_vacc, 
                paste0("Outputs/Sensitivity_analyses/Expected_cases_pre_omicron_", reporting.week, ".csv"),
                row.names = FALSE)
    } else {
      
      expected_summary <- Expected_cases_time_since_vacc %>% 
        select(ReportCountry, year_week, TargetGroup, Variant, DeathsObserved, Denominator, DeathsExpected, TotalAverted)
      
      write.csv(expected_summary, 
                paste0("Outputs/Sensitivity_analyses/Expected_cases_pre_omicron_", sensitivity, "_", reporting.week, ".csv"),
                row.names = FALSE)
    }
    
    
  }
  
    return(Expected_cases_time_since_vacc)
}
