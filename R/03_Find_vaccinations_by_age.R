# Script to aggregate vaccination data by age groups (25+, 50+, 60+)
# Margaux Mesle - meslem@who.int
# July 2021


vaccination.by.age <- function(vax_clean, Country_population, geo) {
  

  if(geo == "Country"){
    
    vaccine_totals_by_country <- vax_clean %>% 
      select(ReportCountry, year_week, TargetGroup, 
             nFirstDose, nSecondDose, nDoseAdditional1, nDoseAdditional2, nDoseAdditional3, 
             Denominator) %>%
      arrange(TargetGroup, year_week) %>% 
      group_by(ReportCountry, TargetGroup) %>% 
      slice(tail(row_number(), 1)) 
    
    
    vaccine_totals_by_country_60 <- vaccine_totals_by_country %>% 
      filter(TargetGroup %in% c("Age 60-69", "Age 70-79", "Age 80+", "Age 60+")) %>% 
      mutate(TargetGroup = "Age 60+") %>% 
      group_by(ReportCountry, TargetGroup) %>% 
      summarise(nFirstDose = sum(nFirstDose),
                nSecondDose = sum(nSecondDose),
                nDoseAdditional1 = sum(nDoseAdditional1),
                nDoseAdditional2 = sum(nDoseAdditional2),
                nDoseAdditional3 = sum(nDoseAdditional3),
                Denominator = sum(unique(Denominator))) %>% 
      ungroup() 
    
    
    vaccine_totals_by_country <- full_join(vaccine_totals_by_country, vaccine_totals_by_country_60) %>% 
      select(-year_week) %>% 
      mutate(pcFirstDose = round((nFirstDose/Denominator)*100),
             pcSecondDose = round((nSecondDose/Denominator)*100),
             pcDoseAdditional1 = round((nDoseAdditional1/Denominator)*100),
             pcDoseAdditional2 = round((nDoseAdditional2/Denominator)*100),
             pcDoseAdditional3 = round((nDoseAdditional3/Denominator)*100)) %>% 
      mutate(pcFirstDose = ifelse(pcFirstDose>100, 100, pcFirstDose),
             pcSecondDose = ifelse(pcSecondDose>100, 100, pcSecondDose),
             pcDoseAdditional1 = ifelse(pcDoseAdditional1 > 100, 100, pcDoseAdditional1),
             pcDoseAdditional2 = ifelse(pcDoseAdditional2 > 100, 100, pcDoseAdditional2),
             pcDoseAdditional3 = ifelse(pcDoseAdditional3 > 100, 100, pcDoseAdditional3)) 
    

    return(vaccine_totals_by_country)
  } 
  
  if(geo == "Country by finer age groups"){
    
    vaccine_totals_by_country <- vax_clean %>% 
      select(ReportCountry, year_week, TargetGroup, nFirstDose, nSecondDose, nDoseAdditional1, nDoseAdditional2, nDoseAdditional3, 
             Denominator) %>%
      arrange(TargetGroup, year_week) %>% 
      group_by(ReportCountry, TargetGroup) %>% 
      slice(tail(row_number(), 1)) %>% 
      ungroup() %>% 
      mutate(pcFirstDose = round((nFirstDose/Denominator)*100),
             pcSecondDose = round((nSecondDose/Denominator)*100),
             pcDoseAdditional1 = round((nDoseAdditional1/Denominator)*100),
             pcDoseAdditional2 = round((nDoseAdditional2/Denominator)*100),
             pcDoseAdditional3 = round((nDoseAdditional3/Denominator)*100)) %>% 
      mutate(pcFirstDose = ifelse(pcFirstDose>100, 100, pcFirstDose),
             pcSecondDose = ifelse(pcSecondDose>100, 100, pcSecondDose),
             pcDoseAdditional1 = ifelse(pcDoseAdditional1 > 100, 100, pcDoseAdditional1),
             pcDoseAdditional2 = ifelse(pcDoseAdditional2 > 100, 100, pcDoseAdditional2),
             pcDoseAdditional3 = ifelse(pcDoseAdditional3 > 100, 100, pcDoseAdditional3)) 
    
    vaccine_totals_60 <- vaccine_totals_by_country %>% 
      filter(TargetGroup %in% c("Age 60-69", "Age 70-79", "Age 80+", "Age 60+")) %>% 
      mutate(TargetGroup = "Age 60+") %>% 
      select(ReportCountry, year_week, TargetGroup, nFirstDose, nSecondDose, nDoseAdditional1, nDoseAdditional2, nDoseAdditional3, 
             Denominator) %>%
      arrange(ReportCountry, TargetGroup, year_week) %>% 
      group_by(ReportCountry) %>% 
      summarise(nFirstDose = sum(nFirstDose),
                nSecondDose = sum(nSecondDose),
                nDoseAdditional1 = sum(nDoseAdditional1),
                nDoseAdditional2 = sum(nDoseAdditional2),
                nDoseAdditional3 = sum(nDoseAdditional3),
                Denominator = sum(unique(Denominator))) %>% 
      ungroup() %>% 
      mutate(pcFirstDose = round((nFirstDose/Denominator)*100),
             pcSecondDose = round((nSecondDose/Denominator)*100),
             pcDoseAdditional1 = round((nDoseAdditional1/Denominator)*100),
             pcDoseAdditional2 = round((nDoseAdditional2/Denominator)*100),
             pcDoseAdditional3 = round((nDoseAdditional3/Denominator)*100)) 
      
    vaccine_totals_by_country <- full_join(vaccine_totals_by_country, vaccine_totals_60) %>% 
      select(-year_week)
    
    
    return(vaccine_totals_by_country)
  } 
  
  if(geo == "Country by week"){
    
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
    
    vaccine_totals_by_country <- full_join(vaccine_totals_by_country, vaccine_totals_by_country_60) %>% 
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
    
    
    return(vaccine_totals_by_country)
  } 
  
  if(geo == "Region"){

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
    
    vaccine_region <- full_join(vaccine_totals_by_country, vaccine_totals_by_country_60) %>% 
      ungroup() %>% 
      group_by(TargetGroup, year_week) %>% 
      summarise(FirstDose = sum(FirstDose),
                SecondDose = sum(SecondDose),
                DoseAdditional1 = sum(DoseAdditional1),
                DoseAdditional2 = sum(DoseAdditional2),
                DoseAdditional3 = sum(DoseAdditional3),
                Denominator = sum(unique(Denominator))) %>% 
      ungroup() %>% 
      group_by(TargetGroup) %>% 
      summarise(FirstDose = sum(FirstDose),
                SecondDose = sum(SecondDose),
                DoseAdditional1 = sum(DoseAdditional1),
                DoseAdditional2 = sum(DoseAdditional2),
                DoseAdditional3 = sum(DoseAdditional3),
                TotalDoses = sum(FirstDose, SecondDose, DoseAdditional1, DoseAdditional2, DoseAdditional3),
                Denominator = sum(unique(last(Denominator)))) %>% 
      ungroup() 
    
    
    vaccine_totals_region <- vaccine_region %>% 
      filter(TargetGroup %in% c("Age 25-49", "Age 50-59", "Age 60+")) %>% 
      adorn_totals("row")
    
    vaccine_region <- vaccine_region %>% 
      full_join(vaccine_totals_region) %>% 
      mutate(pcFirstDose = round((FirstDose/Denominator)*100),
             pcSecondDose = round((SecondDose/Denominator)*100),
             pcDoseAdditional1 = round((DoseAdditional1/Denominator)*100),
             pcDoseAdditional2 = round((DoseAdditional2/Denominator)*100),
             pcDoseAdditional3 = round((DoseAdditional3/Denominator)*100)) 
      
 
    return(vaccine_region)
  }
  

 }
