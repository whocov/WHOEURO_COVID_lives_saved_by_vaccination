# Script to create Table 1 for Deaths Averted analysis
# Margaux Mesle - meslem@who.int
# September 2021
# Update April 2022


create.table.3 <- function(Expected_cases_all_ages, variant_waves, resolution) {


  if(resolution == "Country by variant"){
    
    summary_temp <- Expected_cases_all_ages %>%
      select(ReportCountry, year_week, Variant, TargetGroup, 
             nFirstDose, nSecondDose, nDoseAdditional1, nDoseAdditional2, nDoseAdditional3,
             Denominator, 
             DeathsObserved, DeathsAvertedDose1, DeathsAvertedDose2, DeathsAvertedDose3, DeathsAvertedDose4, DeathsAvertedDose5, 
             TotalAverted, DeathsExpected) %>%
      arrange(ReportCountry, year_week) %>% 
      distinct() %>%
      group_by(ReportCountry, Variant) %>%
      summarise(FirstDose = max(nFirstDose, na.rm = TRUE),
                SecondDose = max(nSecondDose, na.rm = TRUE),
                DoseAdditional1 = max(nDoseAdditional1, na.rm = TRUE),
                DoseAdditional2 = max(nDoseAdditional2, na.rm = TRUE),
                DoseAdditional3 = max(nDoseAdditional3, na.rm = TRUE),
                denominator = sum(unique(last(Denominator))),
                DeathsObserved = sum(DeathsObserved, na.rm=TRUE),
                DeathsAvertedDose1 = sum(DeathsAvertedDose1, na.rm = TRUE),
                DeathsAvertedDose2 = sum(DeathsAvertedDose2, na.rm = TRUE),
                DeathsAvertedDose3 = sum(DeathsAvertedDose3, na.rm = TRUE),
                DeathsAvertedDose4 = sum(DeathsAvertedDose4, na.rm = TRUE),
                DeathsAvertedDose5 = sum(DeathsAvertedDose5, na.rm = TRUE),
                TotalAverted = sum(TotalAverted, na.rm = TRUE),
                DeathsExpected = sum(DeathsExpected, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(VaxUptake2 = round((SecondDose/denominator)*100),
             VaxUptake3 = round((DoseAdditional1/denominator)*100),
             VaxUptake4 = round((DoseAdditional2/denominator)*100),
             VaxUptake5 = round((DoseAdditional3/denominator)*100),
             VaxUptake2 = ifelse(VaxUptake2>100, 100, VaxUptake2),
             VaxUptake3 = ifelse(VaxUptake3>100, 100, VaxUptake3),
             VaxUptake4 = ifelse(VaxUptake4>100, 100, VaxUptake4),
             VaxUptake5 = ifelse(VaxUptake5>100, 100, VaxUptake5),
             MortalityRateObserved = round((DeathsObserved/denominator)*100000),
             MortalityRateExpected = round((DeathsExpected/denominator)*100000),
             pc.Expected.deaths = round((1-(MortalityRateObserved/MortalityRateExpected))*100)) %>%
      distinct() %>%
      group_by(ReportCountry) %>% 
      arrange(-pc.Expected.deaths) %>% 
      ungroup()
  
    
    summary.total <- summary_temp %>% 
      group_by(Variant) %>% 
      summarise(SecondDose = sum(SecondDose),
                DoseAdditional1 = sum(DoseAdditional1),
                DoseAdditional2 = sum(DoseAdditional2),
                DoseAdditional3 = sum(DoseAdditional3),
                denominator = sum(unique(denominator)),
                VaxUptake2 = round((SecondDose/denominator)*100),
                VaxUptake3 = round((DoseAdditional1/denominator)*100),
                VaxUptake4 = round((DoseAdditional2/denominator)*100),
                VaxUptake2 = ifelse(VaxUptake2>100, 100, VaxUptake2),
                VaxUptake3 = ifelse(VaxUptake3>100, 100, VaxUptake3),
                VaxUptake4 = ifelse(VaxUptake4>100, 100, VaxUptake4),
                VaxUptake5 = ifelse(VaxUptake5>100, 100, VaxUptake5),
                DeathsAvertedDose1 = sum(DeathsAvertedDose1, na.rm=TRUE),
                DeathsAvertedDose2 = sum(DeathsAvertedDose2, na.rm=TRUE),
                DeathsAvertedDose3 = sum(DeathsAvertedDose3, na.rm=TRUE),
                DeathsAvertedDose4 = sum(DeathsAvertedDose4, na.rm=TRUE),
                DeathsAvertedDose5 = sum(DeathsAvertedDose5, na.rm=TRUE),
                DeathsObserved = sum(DeathsObserved),
                DeathsExpected = sum(DeathsExpected),
                TotalAverted = sum(TotalAverted),
                MortalityRateObserved = round((DeathsObserved/denominator)*100000),
                MortalityRateExpected = round((DeathsExpected/denominator)*100000),
                pc.Expected.deaths = round((1-(MortalityRateObserved/MortalityRateExpected))*100)) %>%
      ungroup() %>% 
      select(Variant, VaxUptake2, VaxUptake3, VaxUptake4, DeathsObserved, DeathsExpected, 
             DeathsAvertedDose1, DeathsAvertedDose2, DeathsAvertedDose3, DeathsAvertedDose4, DeathsAvertedDose5,
             TotalAverted, MortalityRateObserved, MortalityRateExpected, pc.Expected.deaths) %>%
      add_column(ReportCountry = "Total", .before = "Variant")
    
    
    summary_temp <- summary_temp %>%
     # full_join(summary.total) %>% 
      mutate(ReportCountry = ifelse(str_detect(ReportCountry, "Kosovo"), "Kosovo*", ReportCountry)) %>% 
      mutate(Variant = factor(Variant, levels = variant.order)) %>% 
      group_by(ReportCountry) %>% 
      ungroup()

    
    Table.3 <- summary_temp %>%
      select(ReportCountry, Variant, VaxUptake2, VaxUptake3, VaxUptake4, VaxUptake5, DeathsObserved, 
             DeathsAvertedDose1, DeathsAvertedDose2, DeathsAvertedDose3, DeathsAvertedDose4, DeathsAvertedDose5,
             TotalAverted, MortalityRateObserved, MortalityRateExpected, pc.Expected.deaths) %>%
      rename(Country = ReportCountry,
             `Vaccination uptake Full` = VaxUptake2,
             `Vaccination uptake Additional 1` = VaxUptake3,
             `Vaccination uptake Additional 2` = VaxUptake4,
             `Vaccination uptake Additional 3` = VaxUptake5,
             `Number of deaths observed` = DeathsObserved,
             `Averted after one dose` = DeathsAvertedDose1,
             `Averted after two doses` = DeathsAvertedDose2,
             `Averted after first additional dose` = DeathsAvertedDose3,
             `Averted after second additional dose` = DeathsAvertedDose4,
             `Averted after third additional dose` = DeathsAvertedDose5,
             `Averted total` = TotalAverted,
             `Observed mortality rate` = MortalityRateObserved,
             `Total Expected` = MortalityRateExpected,
             `% expected deaths averted by vaccination` = pc.Expected.deaths)
    
    return(Table.3) 
    
  
  }
  
  if(resolution == "Country by variant and pre-Omicron"){
    
    pre_omicron <- read.csv("Outputs/Sensitivity_analyses/Expected_cases_pre_omicron2023-W12.csv")
    
    summary_temp <- pre_omicron %>%
      select(ReportCountry, year_week, Variant, TargetGroup, 
             nFirstDose, nSecondDose, nDoseAdditional1, nDoseAdditional2, nDoseAdditional3,
             Denominator, 
             DeathsObserved, DeathsAvertedDose1, DeathsAvertedDose2, DeathsAvertedDose3, DeathsAvertedDose4, DeathsAvertedDose5, 
             TotalAverted, DeathsExpected) %>%
      arrange(ReportCountry, year_week) %>% 
      distinct() %>%
      group_by(ReportCountry, Variant) %>%
      summarise(FirstDose = max(nFirstDose, na.rm = TRUE),
                SecondDose = max(nSecondDose, na.rm = TRUE),
                DoseAdditional1 = max(nDoseAdditional1, na.rm = TRUE),
                DoseAdditional2 = max(nDoseAdditional2, na.rm = TRUE),
                DoseAdditional3 = max(nDoseAdditional3, na.rm = TRUE),
                denominator = sum(unique(last(Denominator))),
                DeathsObserved = sum(DeathsObserved, na.rm=TRUE),
                DeathsAvertedDose1 = sum(DeathsAvertedDose1, na.rm = TRUE),
                DeathsAvertedDose2 = sum(DeathsAvertedDose2, na.rm = TRUE),
                DeathsAvertedDose3 = sum(DeathsAvertedDose3, na.rm = TRUE),
                DeathsAvertedDose4 = sum(DeathsAvertedDose4, na.rm = TRUE),
                DeathsAvertedDose5 = sum(DeathsAvertedDose5, na.rm = TRUE),
                TotalAverted = sum(TotalAverted, na.rm = TRUE),
                DeathsExpected = sum(DeathsExpected, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(VaxUptake2 = round((SecondDose/denominator)*100),
             VaxUptake3 = round((DoseAdditional1/denominator)*100),
             VaxUptake4 = round((DoseAdditional2/denominator)*100),
             VaxUptake5 = round((DoseAdditional3/denominator)*100),
             VaxUptake2 = ifelse(VaxUptake2>100, 100, VaxUptake2),
             VaxUptake3 = ifelse(VaxUptake3>100, 100, VaxUptake3),
             VaxUptake4 = ifelse(VaxUptake4>100, 100, VaxUptake4),
             VaxUptake5 = ifelse(VaxUptake5>100, 100, VaxUptake5),
             MortalityRateObserved = round((DeathsObserved/denominator)*100000),
             MortalityRateExpected = round((DeathsExpected/denominator)*100000),
             pc.Expected.deaths = round((1-(MortalityRateObserved/MortalityRateExpected))*100)) %>%
      distinct() %>%
      group_by(ReportCountry) %>% 
      arrange(-pc.Expected.deaths) %>% 
      ungroup()
    
    
    summary.total <- summary_temp %>% 
      group_by(Variant) %>% 
      summarise(SecondDose = sum(SecondDose),
                DoseAdditional1 = sum(DoseAdditional1),
                DoseAdditional2 = sum(DoseAdditional2),
                DoseAdditional3 = sum(DoseAdditional3),
                denominator = sum(unique(denominator)),
                VaxUptake2 = round((SecondDose/denominator)*100),
                VaxUptake3 = round((DoseAdditional1/denominator)*100),
                VaxUptake4 = round((DoseAdditional2/denominator)*100),
                VaxUptake2 = ifelse(VaxUptake2 > 100, 100, VaxUptake2),
                VaxUptake3 = ifelse(VaxUptake3 > 100, 100, VaxUptake3),
                VaxUptake4 = ifelse(VaxUptake4 > 100, 100, VaxUptake4),
                VaxUptake5 = ifelse(VaxUptake5 > 100, 100, VaxUptake5),
                DeathsAvertedDose1 = sum(DeathsAvertedDose1, na.rm=TRUE),
                DeathsAvertedDose2 = sum(DeathsAvertedDose2, na.rm=TRUE),
                DeathsAvertedDose3 = sum(DeathsAvertedDose3, na.rm=TRUE),
                DeathsAvertedDose4 = sum(DeathsAvertedDose4, na.rm=TRUE),
                DeathsAvertedDose5 = sum(DeathsAvertedDose5, na.rm=TRUE),
                DeathsObserved = sum(DeathsObserved),
                DeathsExpected = sum(DeathsExpected),
                TotalAverted = sum(TotalAverted),
                MortalityRateObserved = round((DeathsObserved/denominator)*100000),
                MortalityRateExpected = round((DeathsExpected/denominator)*100000),
                pc.Expected.deaths = round((1-(MortalityRateObserved/MortalityRateExpected))*100)) %>%
      ungroup() %>% 
      mutate(pc.Expected.deaths = ifelse(is.na(pc.Expected.deaths), 0, pc.Expected.deaths)) %>% 
      select(Variant, VaxUptake2, VaxUptake3, VaxUptake4, DeathsObserved, DeathsExpected, 
             DeathsAvertedDose1, DeathsAvertedDose2, DeathsAvertedDose3, DeathsAvertedDose4, DeathsAvertedDose5,
             TotalAverted, MortalityRateObserved, MortalityRateExpected, pc.Expected.deaths) %>%
      add_column(ReportCountry = "Total", .before = "Variant")
    
    
    summary_temp <- summary_temp %>%
      # full_join(summary.total) %>% 
      mutate(ReportCountry = ifelse(str_detect(ReportCountry, "Kosovo"), "Kosovo*", ReportCountry)) %>% 
      mutate(Variant = factor(Variant, levels = variant.order)) %>% 
      group_by(ReportCountry) %>% 
      ungroup()
    
    
    Table.3.pre.omicron <- summary_temp %>%
      select(ReportCountry, Variant, VaxUptake2, VaxUptake3, VaxUptake4, VaxUptake5, DeathsObserved, 
             DeathsAvertedDose1, DeathsAvertedDose2, DeathsAvertedDose3, DeathsAvertedDose4, DeathsAvertedDose5,
             TotalAverted, MortalityRateObserved, MortalityRateExpected, pc.Expected.deaths) %>%
      rename(Country = ReportCountry,
             `Vaccination uptake Full` = VaxUptake2,
             `Vaccination uptake Additional 1` = VaxUptake3,
             `Vaccination uptake Additional 2` = VaxUptake4,
             `Vaccination uptake Additional 3` = VaxUptake5,
             `Number of deaths observed` = DeathsObserved,
             `Averted after one dose` = DeathsAvertedDose1,
             `Averted after two doses` = DeathsAvertedDose2,
             `Averted after first additional dose` = DeathsAvertedDose3,
             `Averted after second additional dose` = DeathsAvertedDose4,
             `Averted after third additional dose` = DeathsAvertedDose5,
             `Averted total` = TotalAverted,
             `Observed mortality rate` = MortalityRateObserved,
             `Total Expected` = MortalityRateExpected,
             `% expected deaths averted by vaccination` = pc.Expected.deaths)
    
    return(Table.3.pre.omicron) 
    
    
  }
  
  if(resolution == "Country by age"){
    
    # Summary vaccination ------------------------------------------------------
    summary_vacc <- vaccinations_by_age_country %>% 
      group_by(ReportCountry, TargetGroup) %>% 
      summarise(SecondDose = sum(nSecondDose),
                DoseAdditional1 = sum(nDoseAdditional1),
                DoseAdditional2 = sum(nDoseAdditional2),
                DoseAdditional3 = sum(nDoseAdditional3),
                denominator = sum(Denominator)) %>% 
      ungroup() %>% 
      adorn_totals("row")
    
 
    summary_vacc <- summary_vacc %>% 
      #full_join(summary_vacc_60) %>% 
      mutate(ReportCountry = ifelse(ReportCountry == "Total", "zTotal", ReportCountry)) %>% 
      arrange(ReportCountry, TargetGroup) %>% 
      mutate(ReportCountry = ifelse(ReportCountry == "zTotal", "Total", ReportCountry)) 
      
    # Summary mortality --------------------------------------------------------
    summary_mort <- Expected_cases_all_ages %>%
      select(ReportCountry, year_week, TargetGroup, 
             DeathsObserved, DeathsAvertedDose1, DeathsAvertedDose2, DeathsAvertedDose3, DeathsAvertedDose4, DeathsAvertedDose5, 
             TotalAverted, DeathsExpected) %>%
      arrange(ReportCountry, year_week) %>% 
      distinct() %>%
      group_by(ReportCountry, TargetGroup) %>%
      summarise(DeathsObserved = sum(DeathsObserved, na.rm=TRUE),
                DeathsAvertedDose1 = sum(DeathsAvertedDose1, na.rm = TRUE),
                DeathsAvertedDose2 = sum(DeathsAvertedDose2, na.rm = TRUE),
                DeathsAvertedDose3 = sum(DeathsAvertedDose3, na.rm = TRUE),
                DeathsAvertedDose4 = sum(DeathsAvertedDose4, na.rm = TRUE),
                DeathsAvertedDose5 = sum(DeathsAvertedDose5, na.rm = TRUE),
                TotalAverted = sum(TotalAverted, na.rm = TRUE),
                DeathsExpected = sum(DeathsExpected, na.rm = TRUE)) %>%
      ungroup() %>%
      adorn_totals("row") 
    
    
    summary_mort_60 <- summary_mort %>% 
      filter(TargetGroup %in% c("Age 60-69", "Age 70-79", "Age 80+", "Age 60+")) %>% 
      mutate(TargetGroup = "Age 60+") %>% 
      group_by(ReportCountry, TargetGroup) %>% 
      summarise(DeathsObserved = sum(DeathsObserved),
                DeathsAvertedDose1 = sum(DeathsAvertedDose1),
                DeathsAvertedDose2 = sum(DeathsAvertedDose2), 
                DeathsAvertedDose3 = sum(DeathsAvertedDose3),
                DeathsAvertedDose4 = sum(DeathsAvertedDose4),
                DeathsAvertedDose5 = sum(DeathsAvertedDose5),
                TotalAverted = sum(TotalAverted),
                DeathsExpected = sum(DeathsExpected)) %>% 
      ungroup() 
    
    summary_mort <- summary_mort %>% 
      full_join(summary_mort_60) %>% 
      mutate(ReportCountry = ifelse(ReportCountry == "Total", "zTotal", ReportCountry)) %>% 
      arrange(ReportCountry, TargetGroup) %>% 
      mutate(ReportCountry = ifelse(ReportCountry == "zTotal", "Total", ReportCountry)) 
    
    
    # Join tables --------------------------------------------------------------
    summary_temp <- full_join(summary_mort, summary_vacc) %>% 
      group_by(ReportCountry, TargetGroup) %>% 
      mutate(VaxUptake2 = round((SecondDose / denominator) * 100),
             VaxUptake3 = round((DoseAdditional1 / denominator) * 100),
             VaxUptake4 = round((DoseAdditional2 / denominator) * 100),
             VaxUptake5 = round((DoseAdditional3 / denominator) * 100),
             VaxUptake2 = ifelse(VaxUptake2 > 100, 100, VaxUptake2),
             VaxUptake3 = ifelse(VaxUptake3 > 100, 100, VaxUptake3),
             VaxUptake4 = ifelse(VaxUptake4 > 100, 100, VaxUptake4),
             VaxUptake5 = ifelse(VaxUptake5 > 100, 100, VaxUptake5),
             MortalityRateObserved = round((DeathsObserved / denominator) * 100000),
             MortalityRateExpected = round((DeathsExpected / denominator) * 100000),
             pc.Expected.deaths = round((1 - (MortalityRateObserved / MortalityRateExpected)) * 100)) %>%
      ungroup()
    
    # Tidy up
    summary_temp <- summary_temp %>%
      select(ReportCountry, TargetGroup, VaxUptake2, VaxUptake3, VaxUptake4, VaxUptake5, DeathsObserved, 
             DeathsAvertedDose1, DeathsAvertedDose2, DeathsAvertedDose3, DeathsAvertedDose4, DeathsAvertedDose5,
             TotalAverted, MortalityRateObserved, MortalityRateExpected, pc.Expected.deaths) %>% 
      mutate(ReportCountry = ifelse(str_detect(ReportCountry, "Kosovo"), "Kosovo*", ReportCountry),
             ReportCountry = ifelse(str_detect(ReportCountry, "Netherlands"), "Netherlands (Kingdom of)", ReportCountry)) 
    
    
    Table.3 <- summary_temp %>%
      mutate(VaxUptake5 = ifelse(VaxUptake5 == 0, "<1%", VaxUptake5)) %>%
      rename(Country = ReportCountry,
             `Vaccination uptake Full` = VaxUptake2,
             `Vaccination uptake Additional 1` = VaxUptake3,
             `Vaccination uptake Additional 2` = VaxUptake4,
             `Vaccination uptake Additional 3` = VaxUptake5,
             `Number of deaths observed` = DeathsObserved,
             `Averted after one dose` = DeathsAvertedDose1,
             `Averted after two doses` = DeathsAvertedDose2,
             `Averted after first additional dose` = DeathsAvertedDose3,
             `Averted after second additional dose` = DeathsAvertedDose4,
             `Averted after third additional dose` = DeathsAvertedDose5,
             `Averted total` = TotalAverted,
             `Observed mortality rate` = MortalityRateObserved,
             `Total Expected` = MortalityRateExpected,
             `% expected deaths averted by vaccination` = pc.Expected.deaths) 
    
    return(Table.3) 
    
    
  }
  
  if(resolution == "Country"){
    
    # Summary mortality
    summary_mort <- Expected_cases_all_ages %>%
      select(ReportCountry, year_week, TargetGroup, 
             DeathsObserved, DeathsAvertedDose1, DeathsAvertedDose2, DeathsAvertedDose3, DeathsAvertedDose4, DeathsAvertedDose5, 
             TotalAverted, DeathsExpected) %>%
      arrange(ReportCountry, year_week) %>% 
      distinct() %>%
      group_by(ReportCountry) %>%
      summarise(DeathsObserved = sum(DeathsObserved, na.rm=TRUE),
                DeathsAvertedDose1 = sum(DeathsAvertedDose1, na.rm = TRUE),
                DeathsAvertedDose2 = sum(DeathsAvertedDose2, na.rm = TRUE),
                DeathsAvertedDose3 = sum(DeathsAvertedDose3, na.rm = TRUE),
                DeathsAvertedDose4 = sum(DeathsAvertedDose4, na.rm = TRUE),
                DeathsAvertedDose5 = sum(DeathsAvertedDose5, na.rm = TRUE),
                TotalAverted = sum(TotalAverted, na.rm = TRUE),
                DeathsExpected = sum(DeathsExpected, na.rm = TRUE)) %>%
      ungroup() %>%
      adorn_totals("row") 
    
    # Summary vaccination 
    summary_vacc <- vaccinations_by_age_country %>% 
      group_by(ReportCountry) %>% 
      summarise(SecondDose = sum(nSecondDose),
                DoseAdditional1 = sum(nDoseAdditional1),
                DoseAdditional2 = sum(nDoseAdditional2),
                DoseAdditional3 = sum(nDoseAdditional3),
                denominator = sum(Denominator)) %>% 
      ungroup() %>% 
      adorn_totals("row")

        
    # Join tables
    summary_temp <- full_join(summary_mort, summary_vacc) %>% 
      group_by(ReportCountry) %>% 
      mutate(VaxUptake2 = round((SecondDose / denominator) * 100),
             VaxUptake3 = round((DoseAdditional1 / denominator) * 100),
             VaxUptake4 = round((DoseAdditional2 / denominator) * 100),
             VaxUptake5 = round((DoseAdditional3 / denominator) * 100),
             VaxUptake2 = ifelse(VaxUptake2 > 100, 100, VaxUptake2),
             VaxUptake3 = ifelse(VaxUptake3 > 100, 100, VaxUptake3),
             VaxUptake4 = ifelse(VaxUptake4 > 100, 100, VaxUptake4),
             VaxUptake5 = ifelse(VaxUptake5 > 100, 100, VaxUptake5),
             MortalityRateObserved = round((DeathsObserved / denominator) * 100000),
             MortalityRateExpected = round((DeathsExpected / denominator) * 100000),
             pc.Expected.deaths = round((1 - (MortalityRateObserved / MortalityRateExpected)) * 100)) %>%
      ungroup() %>% 
      group_by(ReportCountry) %>% 
      arrange(-pc.Expected.deaths) %>% 
      ungroup()
    
    # Tidy up
    summary_temp <- summary_temp %>%
      select(ReportCountry, VaxUptake2, VaxUptake3, VaxUptake4, VaxUptake5, DeathsObserved, 
             DeathsAvertedDose1, DeathsAvertedDose2, DeathsAvertedDose3, DeathsAvertedDose4, DeathsAvertedDose5,
             TotalAverted, MortalityRateObserved, MortalityRateExpected, pc.Expected.deaths) %>% 
      mutate(ReportCountry = ifelse(str_detect(ReportCountry, "Kosovo"), "Kosovo*", ReportCountry)) 
    
    # Filter out totals to be added at bottom of table
    summary_total <-  summary_temp %>% 
      filter(ReportCountry == "Total")

    Table.3 <- summary_temp %>%
      filter(!ReportCountry == "Total") %>% 
      add_row(summary_total) %>% 
      mutate(VaxUptake5 = ifelse(VaxUptake5 == 0, "<1%", VaxUptake5)) %>%
      rename(Country = ReportCountry,
             `Vaccination uptake Full` = VaxUptake2,
             `Vaccination uptake Additional 1` = VaxUptake3,
             `Vaccination uptake Additional 2` = VaxUptake4,
             `Vaccination uptake Additional 3` = VaxUptake5,
             `Number of deaths observed` = DeathsObserved,
             `Averted after one dose` = DeathsAvertedDose1,
             `Averted after two doses` = DeathsAvertedDose2,
             `Averted after first additional dose` = DeathsAvertedDose3,
             `Averted after second additional dose` = DeathsAvertedDose4,
             `Averted after third additional dose` = DeathsAvertedDose5,
             `Averted total` = TotalAverted,
             `Observed mortality rate` = MortalityRateObserved,
             `Total Expected` = MortalityRateExpected,
             `% expected deaths averted by vaccination` = pc.Expected.deaths) 
    
    return(Table.3) 
    
    
  }
  
  if(resolution == "Country pre-Omicron"){
    
    pre_omicron <- read.csv("Outputs/Sensitivity_analyses/Expected_cases_pre_omicron2023-W12.csv")
    
    summary_temp <- pre_omicron %>%
      select(ReportCountry, year_week, Variant, TargetGroup, 
             nFirstDose, nSecondDose, nDoseAdditional1, nDoseAdditional2, nDoseAdditional3,
             Denominator, 
             DeathsObserved, DeathsAvertedDose1, DeathsAvertedDose2, DeathsAvertedDose3, DeathsAvertedDose4, DeathsAvertedDose5, 
             TotalAverted, DeathsExpected) %>%
      arrange(ReportCountry, year_week) %>% 
      distinct() %>%
      group_by(ReportCountry) %>%
      summarise(FirstDose = max(nFirstDose, na.rm = TRUE),
                SecondDose = max(nSecondDose, na.rm = TRUE),
                DoseAdditional1 = max(nDoseAdditional1, na.rm = TRUE),
                DoseAdditional2 = max(nDoseAdditional2, na.rm = TRUE),
                DoseAdditional3 = max(nDoseAdditional3, na.rm = TRUE),
                denominator = sum(unique(last(Denominator))),
                DeathsObserved = sum(DeathsObserved, na.rm=TRUE),
                DeathsAvertedDose1 = sum(DeathsAvertedDose1, na.rm = TRUE),
                DeathsAvertedDose2 = sum(DeathsAvertedDose2, na.rm = TRUE),
                DeathsAvertedDose3 = sum(DeathsAvertedDose3, na.rm = TRUE),
                DeathsAvertedDose4 = sum(DeathsAvertedDose4, na.rm = TRUE),
                DeathsAvertedDose5 = sum(DeathsAvertedDose5, na.rm = TRUE),
                TotalAverted = sum(TotalAverted, na.rm = TRUE),
                DeathsExpected = sum(DeathsExpected, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(VaxUptake2 = round((SecondDose/denominator)*100),
             VaxUptake3 = round((DoseAdditional1/denominator)*100),
             VaxUptake4 = round((DoseAdditional2/denominator)*100),
             VaxUptake5 = round((DoseAdditional3/denominator)*100),
             VaxUptake2 = ifelse(VaxUptake2>100, 100, VaxUptake2),
             VaxUptake3 = ifelse(VaxUptake3>100, 100, VaxUptake3),
             VaxUptake4 = ifelse(VaxUptake4>100, 100, VaxUptake4),
             VaxUptake5 = ifelse(VaxUptake5>100, 100, VaxUptake5),
             MortalityRateObserved = round((DeathsObserved/denominator)*100000),
             MortalityRateExpected = round((DeathsExpected/denominator)*100000),
             pc.Expected.deaths = round((1-(MortalityRateObserved/MortalityRateExpected))*100)) %>%
      distinct() %>%
      group_by(ReportCountry) %>% 
      arrange(-pc.Expected.deaths) %>% 
      ungroup()
    
 
    Table.3 <- summary_temp %>%
      select(ReportCountry, VaxUptake2, VaxUptake3, VaxUptake4, VaxUptake5, DeathsObserved, 
             DeathsAvertedDose1, DeathsAvertedDose2, DeathsAvertedDose3, DeathsAvertedDose4, DeathsAvertedDose5,
             TotalAverted, MortalityRateObserved, MortalityRateExpected, pc.Expected.deaths) %>%
      rename(Country = ReportCountry,
             `Vaccination uptake Full` = VaxUptake2,
             `Vaccination uptake Additional 1` = VaxUptake3,
             `Vaccination uptake Additional 2` = VaxUptake4,
             `Vaccination uptake Additional 3` = VaxUptake5,
             `Number of deaths observed` = DeathsObserved,
             `Averted after one dose` = DeathsAvertedDose1,
             `Averted after two doses` = DeathsAvertedDose2,
             `Averted after first additional dose` = DeathsAvertedDose3,
             `Averted after second additional dose` = DeathsAvertedDose4,
             `Averted after third additional dose` = DeathsAvertedDose5,
             `Averted total` = TotalAverted,
             `Observed mortality rate` = MortalityRateObserved,
             `Total Expected` = MortalityRateExpected,
             `% expected deaths averted by vaccination` = pc.Expected.deaths)
    
    return(Table.3) 
    
    
  }
  
  if(resolution == "Regional"){
    
    summary_temp <- Expected_cases_all_ages %>%
      select(ReportCountry, year_week, Variant, TargetGroup, 
             nFirstDose, nSecondDose, nDoseAdditional1, nDoseAdditional2, nDoseAdditional3,
             Denominator, 
             DeathsObserved, 
             DeathsAvertedDose1, DeathsAvertedDose2, DeathsAvertedDose3, DeathsAvertedDose4, DeathsAvertedDose5,
             TotalAverted, DeathsExpected) %>%
      arrange(ReportCountry, year_week) %>% 
      distinct() %>%
      group_by(Variant) %>%
      summarise(FirstDose = max(nFirstDose, na.rm = TRUE),
                SecondDose = max(nSecondDose, na.rm = TRUE),
                DoseAdditional1 = max(nDoseAdditional1, na.rm = TRUE),
                DoseAdditional2 = max(nDoseAdditional2, na.rm = TRUE),
                DoseAdditional3 = max(nDoseAdditional3, na.rm = TRUE),
                denominator = sum(unique(last(Denominator))),
                DeathsObserved = sum(DeathsObserved, na.rm=TRUE),
                DeathsAvertedDose1 = sum(DeathsAvertedDose1, na.rm = TRUE),
                DeathsAvertedDose2 = sum(DeathsAvertedDose2, na.rm = TRUE),
                DeathsAvertedDose3 = sum(DeathsAvertedDose3, na.rm = TRUE),
                DeathsAvertedDose4 = sum(DeathsAvertedDose4, na.rm = TRUE),
                DeathsAvertedDose5 = sum(DeathsAvertedDose5, na.rm = TRUE),
                TotalAverted = sum(TotalAverted, na.rm=TRUE),
                DeathsExpected = sum(DeathsExpected, na.rm=TRUE)) %>%
      ungroup() %>%
      mutate(VaxUptake2 = round((SecondDose/denominator)*100),
             VaxUptake3 = round((DoseAdditional1/denominator)*100),
             VaxUptake4 = round((DoseAdditional2/denominator)*100),
             VaxUptake5 = round((DoseAdditional3/denominator)*100),
             VaxUptake2 = ifelse(VaxUptake2>100, 100, VaxUptake2),
             VaxUptake3 = ifelse(VaxUptake3>100, 100, VaxUptake3),
             VaxUptake4 = ifelse(VaxUptake4>100, 100, VaxUptake4),
             VaxUptake5 = ifelse(VaxUptake5>100, 100, VaxUptake5),
             MortalityRateObserved = round((DeathsObserved/denominator)*100000),
             MortalityRateExpected = round((DeathsExpected/denominator)*100000),
             pc.Expected.deaths = round((1-(MortalityRateObserved/MortalityRateExpected))*100)) %>%
      distinct() %>%
      arrange(-pc.Expected.deaths) %>% 
      ungroup()
    
    
    summary.total <- summary_temp %>% 
      group_by(Variant) %>% 
      summarise(SecondDose = sum(SecondDose),
                DoseAdditional1 = sum(DoseAdditional1),
                DoseAdditional2 = sum(DoseAdditional2),
                DoseAdditional3 = sum(DoseAdditional3),
                denominator = sum(unique(denominator)),
                VaxUptake2 = round((SecondDose/denominator)*100),
                VaxUptake3 = round((DoseAdditional1/denominator)*100),
                VaxUptake4 = round((DoseAdditional2/denominator)*100),
                VaxUptake5 = round((DoseAdditional3/denominator)*100),
                VaxUptake2 = ifelse(VaxUptake2 > 100, 100, VaxUptake2),
                VaxUptake3 = ifelse(VaxUptake3 > 100, 100, VaxUptake3),
                VaxUptake4 = ifelse(VaxUptake4 > 100, 100, VaxUptake4),
                VaxUptake5 = ifelse(VaxUptake5 > 100, 100, VaxUptake5),
                DeathsAvertedDose1 = sum(DeathsAvertedDose1, na.rm=TRUE),
                DeathsAvertedDose2 = sum(DeathsAvertedDose2, na.rm=TRUE),
                DeathsAvertedDose3 = sum(DeathsAvertedDose3, na.rm=TRUE),
                DeathsAvertedDose4 = sum(DeathsAvertedDose4, na.rm=TRUE),
                DeathsAvertedDose5 = sum(DeathsAvertedDose5, na.rm=TRUE),
                DeathsObserved = sum(DeathsObserved),
                DeathsExpected = sum(DeathsExpected),
                TotalAverted = sum(TotalAverted),
                MortalityRateObserved = round((DeathsObserved/denominator)*100000),
                MortalityRateExpected = round((DeathsExpected/denominator)*100000),
                pc.Expected.deaths = round((1-(MortalityRateObserved/MortalityRateExpected))*100)) %>%
      ungroup() %>% 
      select(Variant, VaxUptake2, VaxUptake3, VaxUptake4, VaxUptake5, 
             DeathsObserved, DeathsExpected, 
             DeathsAvertedDose1, DeathsAvertedDose2, DeathsAvertedDose3, DeathsAvertedDose4, DeathsAvertedDose5, 
             TotalAverted, MortalityRateObserved, MortalityRateExpected, pc.Expected.deaths) 
    
    
    summary_temp <- summary_temp %>%
      full_join(summary.total) %>% 
      mutate(Variant = factor(Variant, levels = variant.order)) 
    
    
    Table.3 <- summary_temp %>%
      select(Variant, VaxUptake2, VaxUptake3, VaxUptake4, VaxUptake5, 
             DeathsObserved, DeathsAvertedDose1, DeathsAvertedDose2, DeathsAvertedDose3, DeathsAvertedDose4, DeathsAvertedDose5,
             TotalAverted, MortalityRateObserved, MortalityRateExpected, pc.Expected.deaths) %>%
      rename(`Vaccination uptake Full` = VaxUptake2,
             `Vaccination uptake Additional 1` = VaxUptake3,
             `Vaccination uptake Additional 2` = VaxUptake4,
             `Vaccination uptake Additional 3` = VaxUptake5,
             `Number of deaths observed` = DeathsObserved,
             `Averted after one dose` = DeathsAvertedDose1,
             `Averted after two doses` = DeathsAvertedDose2,
             `Averted after first additional dose` = DeathsAvertedDose3,
             `Averted after second additional dose` = DeathsAvertedDose4,
             `Averted after third additional dose` = DeathsAvertedDose5,
             `Averted total` = TotalAverted,
             `Observed mortality rate` = MortalityRateObserved,
             `Total Expected` = MortalityRateExpected,
             `% expected deaths averted by vaccination` = pc.Expected.deaths)
    
    return(Table.3) 
    
    
  }
  
}
