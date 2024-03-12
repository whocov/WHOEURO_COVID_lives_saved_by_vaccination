

  # Mortality data -------------------------------------------------------------
  weekly_summary_60 <- Expected_cases_all_ages %>% 
    ungroup() %>% 
    select(ReportCountry, year_week, TargetGroup, TotalAverted) %>%
    filter(TargetGroup %in% c("Age 60-69", "Age 70-79", "Age 80+", "Age 60+")) %>% 
    mutate(TargetGroup = "Age 60+") %>% 
    group_by(ReportCountry, TargetGroup, year_week) %>% 
    summarise(TotalAverted = sum(TotalAverted)) %>% 
    ungroup() %>% 
    group_by(TargetGroup, year_week) %>% 
    summarise(TotalAverted = sum(TotalAverted)) %>% 
    ungroup() 
    
  
  weekly_summary_other_gps <- Expected_cases_all_ages %>% 
    ungroup() %>% 
    select(ReportCountry, year_week, TargetGroup, TotalAverted) %>%
    group_by(ReportCountry, TargetGroup, year_week) %>% 
    summarise(TotalAverted = sum(TotalAverted)) %>% 
    ungroup() %>% 
    filter(!TargetGroup == "Age 60+") %>% 
    group_by(TargetGroup, year_week) %>% 
    summarise(TotalAverted = sum(TotalAverted)) %>% 
    ungroup()  %>% 
    full_join(weekly_summary_60) %>% 
    rename(LivesSaved = TotalAverted) %>% 
    filter(TargetGroup %in% c("Age 25-49", "Age 50-59", "Age 60+"))

  
  # Sense check
  # x <- weekly_summary_other_gps %>% 
  #   group_by(TargetGroup) %>% 
  #   summarise(TotalAverted = sum(TotalAverted)) %>% 
  #   ungroup()
  
  
  # Vaccination data -----------------------------------------------------------
  vaccine_totals_by_country <- vax_clean %>% 
    select(ReportCountry, year_week, TargetGroup, 
           FirstDose, SecondDose, DoseAdditional1, DoseAdditional2, DoseAdditional3,
           Denominator) %>%
    arrange(ReportCountry, TargetGroup, year_week) 
  
  vaccine_totals_by_country_60 <- vaccine_totals_by_country %>% 
    filter(TargetGroup %in% c("Age 60-69", "Age 70-79", "Age 80+", "Age 60+")) %>% 
    mutate(TargetGroup = "Age 60+") %>% 
    group_by(ReportCountry, TargetGroup, year_week) %>% 
    summarise(FirstDose = sum(FirstDose),
              SecondDose = sum(SecondDose),
              DoseAdditional1 = sum(DoseAdditional1),
              DoseAdditional2 = sum(DoseAdditional2),
              DoseAdditional3 = sum(DoseAdditional3),
              Denominator = sum(unique(Denominator))) %>% 
    ungroup() 
  
  vaccine_totals_by_country <- vaccine_totals_by_country %>% 
    full_join(vaccine_totals_by_country_60) %>% 
    ungroup() %>% 
    mutate(pcFirstDose = round((FirstDose/Denominator)*100),
           pcSecondDose = round((SecondDose/Denominator)*100),
           pcDoseAdditional1 = round((DoseAdditional1/Denominator)*100),
           pcDoseAdditional2 = round((DoseAdditional2/Denominator)*100),
           pcDoseAdditional3 = round((DoseAdditional3/Denominator)*100)) %>% 
    mutate(pcFirstDose = ifelse(pcFirstDose>100, 100, pcFirstDose),
           pcSecondDose = ifelse(pcSecondDose>100, 100, pcSecondDose),
           pcDoseAdditional1 = ifelse(pcDoseAdditional1 > 100, 100, pcDoseAdditional1),
           pcDoseAdditional2 = ifelse(pcDoseAdditional2 > 100, 100, pcDoseAdditional2),
           pcDoseAdditional3 = ifelse(pcDoseAdditional3 > 100, 100, pcDoseAdditional3)) 
  
  
  temp <- vaccine_totals_by_country %>% 
    filter(TargetGroup %in% c("Age 25-49", "Age 50-59", "Age 60+")) %>% 
    group_by(ReportCountry, TargetGroup, year_week) %>% 
    summarise(FirstDose = sum(FirstDose),
              SecondDose = sum(SecondDose),
              DoseAdditional1 = sum(DoseAdditional1),
              DoseAdditional2 = sum(DoseAdditional2),
              DoseAdditional3 = sum(DoseAdditional3),
              TotalDoses = sum(FirstDose, SecondDose, DoseAdditional1, DoseAdditional2, DoseAdditional3),
              Denominator = sum(unique(Denominator))) %>% 
    ungroup() %>% 
    group_by(TargetGroup, year_week) %>% 
    summarise(FirstDose = sum(FirstDose),
              SecondDose = sum(SecondDose),
              DoseAdditional1 = sum(DoseAdditional1),
              DoseAdditional2 = sum(DoseAdditional2),
              DoseAdditional3 = sum(DoseAdditional3),
              TotalDoses = sum(FirstDose, SecondDose, DoseAdditional1, DoseAdditional2, DoseAdditional3),
              Denominator = sum(unique(Denominator))) %>% 
    ungroup() %>% 
    select(TargetGroup, year_week, TotalDoses)
  
  
  
  weekly_summary <- weekly_summary_other_gps %>% 
    full_join(temp) %>% 
    adorn_totals("row")
  
  write.csv(weekly_summary, "Lives_saved_total_doses.csv", row.names = F)
  
  