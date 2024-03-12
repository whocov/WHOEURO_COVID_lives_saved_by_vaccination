# Script to create Table 2 for Deaths Averted analysis
# Margaux Mesle - meslem@who.int
# September 2021


create.table.4 <- function(Expected_cases_all_ages, method) {
  
 if(method == "by age variant"){ 
   
   summary_temp <- Expected_cases_all_ages %>%
    select(ReportCountry, Variant, TargetGroup, year_week, FirstDose, SecondDose, DoseAdditional1, DoseAdditional2, DoseAdditional3,
           Denominator, 
           DeathsObserved, DeathsAvertedDose1, DeathsAvertedDose2, DeathsAvertedDose3, DeathsAvertedDose4, DeathsAvertedDose5,
           TotalAverted, DeathsExpected) %>%
    distinct() %>%
    group_by(ReportCountry, TargetGroup, Variant) %>%
    summarise(FirstDose = sum(FirstDose, na.rm = TRUE),
              SecondDose = sum(SecondDose, na.rm = TRUE),
              DoseAdditional1 = sum(DoseAdditional1, na.rm = TRUE),
              DoseAdditional2 = sum(DoseAdditional2, na.rm = TRUE),
              DoseAdditional3 = sum(DoseAdditional3, na.rm = TRUE),
              denominator = sum(unique(last(Denominator))),
              DeathsObserved = sum(DeathsObserved, na.rm = TRUE),
              DeathsAvertedDose1 = sum(DeathsAvertedDose1, na.rm = TRUE),
              DeathsAvertedDose2 = sum(DeathsAvertedDose2, na.rm = TRUE),
              DeathsAvertedDose3 = sum(DeathsAvertedDose3, na.rm = TRUE),
              DeathsAvertedDose4 = sum(DeathsAvertedDose4, na.rm = TRUE),
              DeathsAvertedDose5 = sum(DeathsAvertedDose5, na.rm = TRUE),
              TotalAverted = sum(TotalAverted, na.rm = TRUE),
              DeathsExpected = sum(DeathsExpected, na.rm = TRUE)) %>%
    ungroup() %>% 
     pivot_wider()
  
  summary_total <- summary_temp %>% 
    group_by(Variant, TargetGroup) %>% 
    summarise(SecondDose = sum(SecondDose),
              DoseAdditional1 = sum(DoseAdditional1),
              DoseAdditional2 = sum(DoseAdditional2),
              DoseAdditional3 = sum(DoseAdditional3),
              denominator = sum(unique(last(denominator))),
              VaxUptake2 = round((SecondDose/denominator)*100),
              VaxUptake3 = round((DoseAdditional1/denominator)*100),
              VaxUptake4 = round((DoseAdditional2/denominator)*100),
              VaxUptake5 = round((DoseAdditional3/denominator)*100),
              VaxUptake2 = ifelse(VaxUptake2 > 100, 100, VaxUptake2),
              VaxUptake3 = ifelse(VaxUptake3 > 100, 100, VaxUptake3),
              VaxUptake4 = ifelse(VaxUptake4 > 100, 100, VaxUptake4),
              VaxUptake5 = ifelse(VaxUptake5 > 100, 100, VaxUptake5),
              DeathsObserved = sum(DeathsObserved),
              DeathsExpected = sum(DeathsExpected),
              TotalAverted = sum(TotalAverted),
              MortalityRateObserved = round((DeathsObserved/denominator)*100000),
              MortalityRateExpected = round((DeathsExpected/denominator)*100000),
              pc.Expected.deaths = round((1-(MortalityRateObserved/MortalityRateExpected))*100)) %>%
    ungroup() %>% 
    select(Variant, TargetGroup, SecondDose, DoseAdditional1, VaxUptake2, VaxUptake3, VaxUptake4, VaxUptake5, denominator, 
           DeathsObserved, TotalAverted, 
           MortalityRateObserved, MortalityRateExpected, pc.Expected.deaths) 
  
  # Calculate summaries for each age group
  # 25 - 49 years
  summary.total.25 <- summary_total %>% 
    filter(TargetGroup == "Age 25-49") %>% 
    select(-TargetGroup)
  
  colnames(summary.total.25) <- paste0(colnames(summary.total.25),"_25")
  summary.total.25 <- summary.total.25 %>% 
    rename(Variant = Variant_25) %>% 
    add_column(ReportCountry = "Total", .before = "Variant")
  
  # 50 - 59 years 
  summary.total.50 <- summary_total %>% 
    filter(TargetGroup == "Age 50-59") %>% 
    select(-TargetGroup) 
  
  colnames(summary.total.50) <- paste0(colnames(summary.total.50),"_50")
  summary.total.50 <- summary.total.50 %>% 
    rename(Variant = Variant_50) %>% 
    add_column(ReportCountry = "Total", .before = "Variant")

  # 60 + years
  summary.total.60 <- summary_total %>% 
    filter(TargetGroup == "Age 60+") %>% 
    select(-TargetGroup)
  
  colnames(summary.total.60) <- paste0(colnames(summary.total.60),"_60")
  summary.total.60 <- summary.total.60 %>% 
    rename(Variant = Variant_60) %>% 
    add_column(ReportCountry ="Total", .before = "Variant")
  
  summary.total.full <- full_join(summary.total.25, summary.total.50) %>%  
    full_join(summary.total.60)
  
  
  summary.25 <- summary_temp %>% 
    filter(TargetGroup == "Age 25-49") %>%
    group_by(ReportCountry) %>% 
    group_modify(~ adorn_totals(.x, where = "row")) %>% 
    ungroup() %>% 
    mutate(Variant = ifelse(Variant == "-", "Total", Variant),
           Variant = factor(Variant, levels = c("Index", "Alpha", "Delta", "Omicron", "Total"))) %>% 
    arrange(ReportCountry, TargetGroup, Variant) %>% 
    mutate(denominator = ifelse(TargetGroup == "Total", lag(denominator), denominator)) %>% 
    mutate(VaxUptake2 = round((SecondDose/denominator)*100),
           VaxUptake3 = round((DoseAdditional1/denominator)*100),
           VaxUptake4 = round((DoseAdditional2/denominator)*100),
           VaxUptake5 = round((DoseAdditional3/denominator)*100),
           VaxUptake2 = ifelse(VaxUptake2 > 100, 100, VaxUptake2),
           VaxUptake3 = ifelse(VaxUptake3 > 100, 100, VaxUptake3),
           VaxUptake4 = ifelse(VaxUptake4 > 100, 100, VaxUptake4),
           VaxUptake5 = ifelse(VaxUptake5 > 100, 100, VaxUptake5),
           MortalityRateObserved = round((DeathsObserved/denominator)*100000),
           MortalityRateExpected = round((DeathsExpected/denominator)*100000),
           pc.Expected.deaths = round((1-(MortalityRateObserved/MortalityRateExpected))*100)) %>%
    select(ReportCountry, Variant, SecondDose, DoseAdditional1, VaxUptake2, VaxUptake3, VaxUptake4, VaxUptake5,
           denominator, DeathsObserved, TotalAverted, 
           MortalityRateObserved, MortalityRateExpected, pc.Expected.deaths) 
    
  colnames(summary.25) <- paste0(colnames(summary.25),"_25")
  summary.25 <- summary.25 %>% 
    rename(ReportCountry = ReportCountry_25,
           Variant = Variant_25) 
  
  
  summary.50 <- summary_temp %>% 
    filter(TargetGroup == "Age 50-59") %>% 
    group_by(ReportCountry) %>% 
    group_modify(~ adorn_totals(.x, where = "row")) %>% 
    ungroup() %>% 
    mutate(Variant = ifelse(Variant == "-", "Total", Variant),
           Variant = factor(Variant, levels = c("Index", "Alpha", "Delta", "Omicron", "Total"))) %>% 
    arrange(ReportCountry, TargetGroup, Variant) %>% 
    mutate(denominator = ifelse(TargetGroup == "Total", lag(denominator), denominator)) %>% 
    mutate(VaxUptake2 = round((SecondDose/denominator)*100),
           VaxUptake3 = round((DoseAdditional1/denominator)*100),
           VaxUptake4 = round((DoseAdditional2/denominator)*100),
           VaxUptake5 = round((DoseAdditional3/denominator)*100),
           VaxUptake2 = ifelse(VaxUptake2 > 100, 100, VaxUptake2),
           VaxUptake3 = ifelse(VaxUptake3 > 100, 100, VaxUptake3),
           VaxUptake4 = ifelse(VaxUptake4 > 100, 100, VaxUptake4),
           VaxUptake5 = ifelse(VaxUptake5 > 100, 100, VaxUptake5),
           MortalityRateObserved = round((DeathsObserved/denominator)*100000),
           MortalityRateExpected = round((DeathsExpected/denominator)*100000),
           pc.Expected.deaths = round((1-(MortalityRateObserved/MortalityRateExpected))*100)) %>%
    select(ReportCountry, Variant, SecondDose, DoseAdditional1, VaxUptake2, VaxUptake3, VaxUptake4, VaxUptake5,
           denominator, DeathsObserved, TotalAverted, 
           MortalityRateObserved, MortalityRateExpected, pc.Expected.deaths) 
  
  colnames(summary.50) <- paste0(colnames(summary.50),"_50")
  summary.50 <- summary.50 %>% 
    rename(ReportCountry = ReportCountry_50,
           Variant = Variant_50) 
  
  
  summary.60 <- summary_temp %>% 
    filter(TargetGroup == "Age 60+") %>% 
    group_by(ReportCountry) %>% 
    group_modify(~ adorn_totals(.x, where = "row")) %>% 
    ungroup() %>% 
    mutate(Variant = ifelse(Variant == "-", "Total", Variant),
           Variant = factor(Variant, levels = c("Index", "Alpha", "Delta", "Omicron", "Total"))) %>% 
    arrange(ReportCountry, TargetGroup, Variant) %>% 
    mutate(denominator = ifelse(TargetGroup == "Total", lag(denominator), denominator)) %>% 
    mutate(VaxUptake2 = round((SecondDose/denominator)*100),
           VaxUptake3 = round((DoseAdditional1/denominator)*100),
           VaxUptake4 = round((DoseAdditional2/denominator)*100),
           VaxUptake5 = round((DoseAdditional3/denominator)*100),
           VaxUptake2 = ifelse(VaxUptake2 > 100, 100, VaxUptake2),
           VaxUptake3 = ifelse(VaxUptake3 > 100, 100, VaxUptake3),
           VaxUptake4 = ifelse(VaxUptake4 > 100, 100, VaxUptake4),
           VaxUptake5 = ifelse(VaxUptake5 > 100, 100, VaxUptake5),
           MortalityRateObserved = round((DeathsObserved/denominator)*100000),
           MortalityRateExpected = round((DeathsExpected/denominator)*100000),
           pc.Expected.deaths = round((1-(MortalityRateObserved/MortalityRateExpected))*100)) %>%
    select(ReportCountry, Variant, SecondDose, DoseAdditional1, VaxUptake2, VaxUptake3, VaxUptake4, VaxUptake5,
           denominator, DeathsObserved, TotalAverted, 
           MortalityRateObserved, MortalityRateExpected, pc.Expected.deaths) 
  
  colnames(summary.60) <- paste0(colnames(summary.60),"_60")
  summary.60 <- summary.60 %>% 
    rename(ReportCountry = ReportCountry_60,
           Variant = Variant_60) 
  
  

  Table.4 <- full_join(summary.25, summary.50) %>% 
    full_join(summary.60) %>% 
    full_join(summary.total.full) %>% 
    distinct() %>% 
    mutate(ReportCountry = ifelse(ReportCountry == "Kosovo", "Kosovo*", ReportCountry)) %>% 
    mutate(ReportCountry = factor(ReportCountry, levels = c("Albania", "Austria", "Belgium", "Croatia", "Cyprus", "Czechia", "Denmark",
                                                            "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Iceland",
                                                            "Ireland", "Israel", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", 
                                                            "Montenegro", "Netherlands", "North Macedonia", "Norway", "Poland", "Portugal", 
                                                            "Republic of Moldova", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden", 
                                                            "Switzerland", "Ukraine", "United Kingdom (England)",
                                                            "United Kingdom (Scotland)", "United Kingdom (Wales)", "Kosovo*", "Total"))) %>% 
    mutate(Variant = ifelse(Variant == "-", "Total", Variant),
           Variant = factor(Variant, levels = c("Index", "Alpha", "Delta", "Omicron", "Total"))) %>% 
    arrange(ReportCountry, Variant)
  }
  
  if(method == "by age"){
    
    summary_temp <- Expected_cases_all_ages %>%
      select(ReportCountry, TargetGroup, year_week, FirstDose, SecondDose, DoseAdditional1, DoseAdditional2, DoseAdditional3,
             Denominator, 
             DeathsObserved, DeathsAvertedDose1, DeathsAvertedDose2, DeathsAvertedDose3, DeathsAvertedDose4, DeathsAvertedDose5,
             TotalAverted, DeathsExpected) %>%
      distinct() %>%
      group_by(ReportCountry, TargetGroup) %>%
      summarise(FirstDose = sum(FirstDose, na.rm = TRUE),
                SecondDose = sum(SecondDose, na.rm = TRUE),
                DoseAdditional1 = sum(DoseAdditional1, na.rm = TRUE),
                DoseAdditional2 = sum(DoseAdditional2, na.rm = TRUE),
                DoseAdditional3 = sum(DoseAdditional3, na.rm = TRUE),
                denominator = sum(unique(last(Denominator))),
                DeathsObserved = sum(DeathsObserved, na.rm = TRUE),
                DeathsAvertedDose1 = sum(DeathsAvertedDose1, na.rm = TRUE),
                DeathsAvertedDose2 = sum(DeathsAvertedDose2, na.rm = TRUE),
                DeathsAvertedDose3 = sum(DeathsAvertedDose3, na.rm = TRUE),
                DeathsAvertedDose4 = sum(DeathsAvertedDose4, na.rm = TRUE),
                DeathsAvertedDose5 = sum(DeathsAvertedDose5, na.rm = TRUE),
                TotalAverted = sum(TotalAverted, na.rm = TRUE),
                DeathsExpected = sum(DeathsExpected, na.rm = TRUE)) %>%
      ungroup() 
    
    summary.total <- summary_temp %>% 
      group_by(TargetGroup) %>% 
      summarise(SecondDose = sum(SecondDose),
                DoseAdditional1 = sum(DoseAdditional1),
                DoseAdditional2 = sum(DoseAdditional2),
                DoseAdditional3 = sum(DoseAdditional3),
                denominator = sum(unique(last(denominator))),
                VaxUptake2 = round((SecondDose/denominator)*100),
                VaxUptake3 = round((DoseAdditional1/denominator)*100),
                VaxUptake4 = round((DoseAdditional2/denominator)*100),
                VaxUptake5 = round((DoseAdditional3/denominator)*100),
                VaxUptake2 = ifelse(VaxUptake2 > 100, 100, VaxUptake2),
                VaxUptake3 = ifelse(VaxUptake3 > 100, 100, VaxUptake3),
                VaxUptake4 = ifelse(VaxUptake4 > 100, 100, VaxUptake4),
                VaxUptake5 = ifelse(VaxUptake5 > 100, 100, VaxUptake5),
                DeathsObserved = sum(DeathsObserved),
                DeathsExpected = sum(DeathsExpected),
                TotalAverted = sum(TotalAverted),
                MortalityRateObserved = round((DeathsObserved/denominator)*100000),
                MortalityRateExpected = round((DeathsExpected/denominator)*100000),
                pc.Expected.deaths = round((1-(MortalityRateObserved/MortalityRateExpected))*100)) %>%
      ungroup() %>% 
      select(TargetGroup, SecondDose, DoseAdditional1, VaxUptake2, VaxUptake3, VaxUptake4, VaxUptake5, denominator, 
             DeathsObserved, TotalAverted, 
             MortalityRateObserved, MortalityRateExpected, pc.Expected.deaths) 
    
    # Calculate summaries for each age group
    # 25 - 49 years
    summary.total.25 <- summary.total %>% 
      filter(TargetGroup == "Age 25-49") %>% 
      select(-TargetGroup)
    
    colnames(summary.total.25) <- paste0(colnames(summary.total.25),"_25")
    summary.total.25 <- summary.total.25 %>% 
      add_column(ReportCountry = "Total")
    
    # 50 - 59 years 
    summary.total.50 <- summary.total %>% 
      filter(TargetGroup == "Age 50-59") %>% 
      select(-TargetGroup) 
    
    colnames(summary.total.50) <- paste0(colnames(summary.total.50),"_50")
    summary.total.50 <- summary.total.50 %>% 
      add_column(ReportCountry = "Total")
    
    # 60 + years
    summary.total.60 <- summary.total %>% 
      filter(TargetGroup == "Age 60+") %>% 
      select(-TargetGroup)
    
    colnames(summary.total.60) <- paste0(colnames(summary.total.60),"_60")
    summary.total.60 <- summary.total.60 %>% 
      add_column(ReportCountry ="Total")
    
    summary.total.full <- full_join(summary.total.25, summary.total.50) %>%  
      full_join(summary.total.60)
    
    
    summary.25 <- summary_temp %>% 
      filter(TargetGroup == "Age 25-49") %>%
      group_by(ReportCountry) %>% 
      group_modify(~ adorn_totals(.x, where = "row")) %>% 
      ungroup() %>% 
      arrange(ReportCountry, TargetGroup) %>% 
      mutate(denominator = ifelse(TargetGroup == "Total", lag(denominator), denominator)) %>% 
      mutate(VaxUptake2 = round((SecondDose/denominator)*100),
             VaxUptake3 = round((DoseAdditional1/denominator)*100),
             VaxUptake4 = round((DoseAdditional2/denominator)*100),
             VaxUptake5 = round((DoseAdditional3/denominator)*100),
             VaxUptake2 = ifelse(VaxUptake2 > 100, 100, VaxUptake2),
             VaxUptake3 = ifelse(VaxUptake3 > 100, 100, VaxUptake3),
             VaxUptake4 = ifelse(VaxUptake4 > 100, 100, VaxUptake4),
             VaxUptake5 = ifelse(VaxUptake5 > 100, 100, VaxUptake5),
             MortalityRateObserved = round((DeathsObserved/denominator)*100000),
             MortalityRateExpected = round((DeathsExpected/denominator)*100000),
             pc.Expected.deaths = round((1-(MortalityRateObserved/MortalityRateExpected))*100)) %>%
      select(ReportCountry, SecondDose, DoseAdditional1, VaxUptake2, VaxUptake3, VaxUptake4, VaxUptake5,
             denominator, DeathsObserved, TotalAverted, 
             MortalityRateObserved, MortalityRateExpected, pc.Expected.deaths) 
    
    colnames(summary.25) <- paste0(colnames(summary.25),"_25")
    summary.25 <- summary.25 %>% 
      rename(ReportCountry = ReportCountry_25) 
    
    
    summary.50 <- summary_temp %>% 
      filter(TargetGroup == "Age 50-59") %>% 
      group_by(ReportCountry) %>% 
      group_modify(~ adorn_totals(.x, where = "row")) %>% 
      ungroup() %>% 
      arrange(ReportCountry, TargetGroup) %>% 
      mutate(denominator = ifelse(TargetGroup == "Total", lag(denominator), denominator)) %>% 
      mutate(VaxUptake2 = round((SecondDose/denominator)*100),
             VaxUptake3 = round((DoseAdditional1/denominator)*100),
             VaxUptake4 = round((DoseAdditional2/denominator)*100),
             VaxUptake5 = round((DoseAdditional3/denominator)*100),
             VaxUptake2 = ifelse(VaxUptake2 > 100, 100, VaxUptake2),
             VaxUptake3 = ifelse(VaxUptake3 > 100, 100, VaxUptake3),
             VaxUptake4 = ifelse(VaxUptake4 > 100, 100, VaxUptake4),
             VaxUptake5 = ifelse(VaxUptake5 > 100, 100, VaxUptake5),
             MortalityRateObserved = round((DeathsObserved/denominator)*100000),
             MortalityRateExpected = round((DeathsExpected/denominator)*100000),
             pc.Expected.deaths = round((1-(MortalityRateObserved/MortalityRateExpected))*100)) %>%
      select(ReportCountry, SecondDose, DoseAdditional1, VaxUptake2, VaxUptake3, VaxUptake4, VaxUptake5,
             denominator, DeathsObserved, TotalAverted, 
             MortalityRateObserved, MortalityRateExpected, pc.Expected.deaths) 
    
    colnames(summary.50) <- paste0(colnames(summary.50),"_50")
    summary.50 <- summary.50 %>% 
      rename(ReportCountry = ReportCountry_50) 
    
    
    summary.60 <- summary_temp %>% 
      filter(TargetGroup == "Age 60+") %>% 
      group_by(ReportCountry) %>% 
      group_modify(~ adorn_totals(.x, where = "row")) %>% 
      ungroup() %>% 
      arrange(ReportCountry, TargetGroup) %>% 
      mutate(denominator = ifelse(TargetGroup == "Total", lag(denominator), denominator)) %>% 
      mutate(VaxUptake2 = round((SecondDose/denominator)*100),
             VaxUptake3 = round((DoseAdditional1/denominator)*100),
             VaxUptake4 = round((DoseAdditional2/denominator)*100),
             VaxUptake5 = round((DoseAdditional3/denominator)*100),
             VaxUptake2 = ifelse(VaxUptake2 > 100, 100, VaxUptake2),
             VaxUptake3 = ifelse(VaxUptake3 > 100, 100, VaxUptake3),
             VaxUptake4 = ifelse(VaxUptake4 > 100, 100, VaxUptake4),
             VaxUptake5 = ifelse(VaxUptake5 > 100, 100, VaxUptake5),
             MortalityRateObserved = round((DeathsObserved/denominator)*100000),
             MortalityRateExpected = round((DeathsExpected/denominator)*100000),
             pc.Expected.deaths = round((1-(MortalityRateObserved/MortalityRateExpected))*100)) %>%
      select(ReportCountry,  SecondDose, DoseAdditional1, VaxUptake2, VaxUptake3, VaxUptake4, VaxUptake5,
             denominator, DeathsObserved, TotalAverted, 
             MortalityRateObserved, MortalityRateExpected, pc.Expected.deaths) 
    
    colnames(summary.60) <- paste0(colnames(summary.60),"_60")
    summary.60 <- summary.60 %>% 
      rename(ReportCountry = ReportCountry_60) 
    
    
    
    Table.4 <- full_join(summary.25, summary.50) %>% 
      full_join(summary.60) %>% 
      full_join(summary.total.full) %>% 
      distinct() %>% 
      mutate(ReportCountry = ifelse(ReportCountry == "Kosovo", "Kosovo*", ReportCountry)) %>% 
      mutate(ReportCountry = factor(ReportCountry, levels = c("Albania", "Austria", "Belgium", "Croatia", "Cyprus", "Czechia", "Denmark",
                                                              "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Iceland",
                                                              "Ireland", "Israel", "Italy", "Latvia", "Lithuania", "Luxembourg", 
                                                              "Malta", "Montenegro", "Netherlands", "North Macedonia", "Norway", "Poland", 
                                                              "Portugal", "Republic of Moldova", "Romania", "Slovakia", "Slovenia", 
                                                              "Spain", "Sweden", "Switzerland", "Ukraine", "United Kingdom (England)",
                                                              "United Kingdom (Scotland)", "United Kingdom (Wales)", "Kosovo*", "Total"))) %>% 
      arrange(ReportCountry)
    
  }

  if(method == "by age older finer"){
    
    # Summary by country and age group -----------------------------------------
    summary_country_age <- Expected_cases_all_ages %>%
      select(ReportCountry, TargetGroup, year_week, 
             #FirstDose, SecondDose, DoseAdditional1, DoseAdditional2, DoseAdditional3, 
             Denominator, 
             DeathsObserved, DeathsAvertedDose1, DeathsAvertedDose2, DeathsAvertedDose3, DeathsAvertedDose4, DeathsAvertedDose5,
             TotalAverted, DeathsExpected) %>%
      distinct() 
    
    
    summary_country_age_60 <- summary_country_age %>% 
      filter(TargetGroup %in% c("Age 60-69", "Age 70-79", "Age 80+", "Age 60+")) %>% 
      mutate(TargetGroup = "Age 60+") %>% 
      group_by(ReportCountry, TargetGroup, year_week) %>% 
      summarise(Denominator = sum(Denominator), 
                DeathsObserved = sum(DeathsObserved), 
                DeathsAvertedDose1 = sum(DeathsAvertedDose1), 
                DeathsAvertedDose2 = sum(DeathsAvertedDose2), 
                DeathsAvertedDose3 = sum(DeathsAvertedDose3), 
                DeathsAvertedDose4 = sum(DeathsAvertedDose4), 
                DeathsAvertedDose5 = sum(DeathsAvertedDose5),
                TotalAverted = sum(TotalAverted), 
                DeathsExpected = sum(DeathsExpected)) %>% 
      ungroup() 

    summary_country_age <- summary_country_age  %>%
      full_join(summary_country_age_60) %>% 
      group_by(ReportCountry, TargetGroup) %>%
      summarise(denominator = sum(unique(last(Denominator))),
                DeathsObserved = sum(DeathsObserved, na.rm = TRUE),
                DeathsAvertedDose1 = sum(DeathsAvertedDose1, na.rm = TRUE),
                DeathsAvertedDose2 = sum(DeathsAvertedDose2, na.rm = TRUE),
                DeathsAvertedDose3 = sum(DeathsAvertedDose3, na.rm = TRUE),
                DeathsAvertedDose4 = sum(DeathsAvertedDose4, na.rm = TRUE),
                DeathsAvertedDose5 = sum(DeathsAvertedDose5, na.rm = TRUE),
                TotalAverted = sum(TotalAverted, na.rm = TRUE),
                DeathsExpected = sum(DeathsExpected, na.rm = TRUE),
                ObservedRate = round((DeathsObserved / denominator)*100000),
                ExpectedRate = round((DeathsExpected / denominator)*100000),
                pc_change = round((1-(DeathsObserved/DeathsExpected))*100)) %>%
      ungroup() %>% 
      select(ReportCountry, TargetGroup, DeathsObserved, TotalAverted, ObservedRate, ExpectedRate, pc_change)
    
    # Summary by age group -----------------------------------------------------
    summary_age <- Expected_cases_all_ages %>%
      select(TargetGroup, year_week, 
             Denominator, 
             DeathsObserved, DeathsAvertedDose1, DeathsAvertedDose2, DeathsAvertedDose3, DeathsAvertedDose4, DeathsAvertedDose5,
             TotalAverted, DeathsExpected) %>%
      distinct() 
    
    
    summary_age_60 <- summary_age %>% 
      filter(TargetGroup %in% c("Age 60-69", "Age 70-79", "Age 80+", "Age 60+")) %>% 
      mutate(TargetGroup = "Age 60+") %>% 
      group_by(TargetGroup, year_week) %>% 
      summarise(Denominator = sum(Denominator), 
                DeathsObserved = sum(DeathsObserved), 
                DeathsAvertedDose1 = sum(DeathsAvertedDose1), 
                DeathsAvertedDose2 = sum(DeathsAvertedDose2), 
                DeathsAvertedDose3 = sum(DeathsAvertedDose3), 
                DeathsAvertedDose4 = sum(DeathsAvertedDose4), 
                DeathsAvertedDose5 = sum(DeathsAvertedDose5),
                TotalAverted = sum(TotalAverted), 
                DeathsExpected = sum(DeathsExpected)) %>% 
      ungroup() 
    
    
    summary_age <- summary_age %>%
      full_join(summary_age_60) %>% 
      group_by(TargetGroup) %>%
      summarise(denominator = sum(unique(last(Denominator))),
                DeathsObserved = sum(DeathsObserved, na.rm = TRUE),
                DeathsAvertedDose1 = sum(DeathsAvertedDose1, na.rm = TRUE),
                DeathsAvertedDose2 = sum(DeathsAvertedDose2, na.rm = TRUE),
                DeathsAvertedDose3 = sum(DeathsAvertedDose3, na.rm = TRUE),
                DeathsAvertedDose4 = sum(DeathsAvertedDose4, na.rm = TRUE),
                DeathsAvertedDose5 = sum(DeathsAvertedDose5, na.rm = TRUE),
                TotalAverted = sum(TotalAverted, na.rm = TRUE),
                DeathsExpected = sum(DeathsExpected, na.rm = TRUE),
                ObservedRate = round((DeathsObserved / denominator)*100000),
                ExpectedRate = round((DeathsExpected / denominator)*100000),
                pc_change = round((1-(DeathsObserved/DeathsExpected))*100)) %>%
      ungroup() %>% 
      select(TargetGroup, DeathsObserved, TotalAverted, ObservedRate, ExpectedRate, pc_change) %>% 
      add_column(ReportCountry = "Total", .before = "TargetGroup")
    
    # Join previous tables
    summary_mortality <- full_join(summary_country_age, summary_age)
    
    # Summary of vaccination data
    summary_vaccination <- full_join(vaccinations_by_age_country, vaccinations_by_age) %>% 
      select(ReportCountry, TargetGroup, pcSecondDose, pcDoseAdditional1, pcDoseAdditional2, pcDoseAdditional3) %>% 
      filter(!TargetGroup == "Total") %>% 
      mutate(ReportCountry = ifelse(is.na(ReportCountry), "Total", ReportCountry))
    
    

    Table.4 <- full_join(summary_vaccination, summary_mortality) %>% 
      distinct() %>% 
      mutate(ReportCountry = ifelse(ReportCountry == "Kosovo", "Kosovo*", ReportCountry)) %>% 
      mutate(ReportCountry = factor(ReportCountry, levels = c("Albania", "Austria", "Belgium", "Croatia", "Cyprus", "Czechia", "Denmark",
                                                              "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Iceland",
                                                              "Ireland", "Israel", "Italy", "Latvia", "Lithuania", "Luxembourg", 
                                                              "Malta", "Montenegro", "Netherlands", "North Macedonia", "Norway", "Poland",
                                                              "Portugal", "Republic of Moldova", "Romania", "Slovakia", "Slovenia", 
                                                              "Spain", "Sweden", "Switzerland", "Ukraine", "United Kingdom (England)",
                                                              "United Kingdom (Scotland)", "United Kingdom (Wales)", "Kosovo*", "Total"))) %>% 
      arrange(ReportCountry)
    
  }
  
  if(method == "by age older finer variant"){
    
    # Summary by country and age group -----------------------------------------
    summary_country_age <- Expected_cases_all_ages %>%
      select(ReportCountry, TargetGroup, year_week, Variant,
             Denominator, 
             DeathsObserved, DeathsAvertedDose1, DeathsAvertedDose2, DeathsAvertedDose3, DeathsAvertedDose4, DeathsAvertedDose5,
             TotalAverted, DeathsExpected) %>%
      distinct()  
    
    # Summary by country for 60+ age group -------------------------------------
    summary_country_age_60 <- Expected_cases_all_ages %>%
      select(ReportCountry, TargetGroup, year_week, Variant,
             Denominator, 
             DeathsObserved, DeathsAvertedDose1, DeathsAvertedDose2, DeathsAvertedDose3, DeathsAvertedDose4, DeathsAvertedDose5,
             TotalAverted, DeathsExpected) %>%
      distinct() %>% 
      filter(TargetGroup %in% c("Age 60-69", "Age 70-79", "Age 80+", "Age 60+")) %>% 
      mutate(TargetGroup = "Age 60+") %>% 
      group_by(ReportCountry, TargetGroup, year_week, Variant) %>% 
      summarise(Denominator = sum(Denominator), 
                DeathsObserved = sum(DeathsObserved), 
                DeathsAvertedDose1 = sum(DeathsAvertedDose1), 
                DeathsAvertedDose2 = sum(DeathsAvertedDose2), 
                DeathsAvertedDose3 = sum(DeathsAvertedDose3), 
                DeathsAvertedDose4 = sum(DeathsAvertedDose4), 
                DeathsAvertedDose5 = sum(DeathsAvertedDose5),
                TotalAverted = sum(TotalAverted), 
                DeathsExpected = sum(DeathsExpected)) %>% 
      ungroup() 
    
    
    summary_country_age <- summary_country_age %>% 
      full_join(summary_country_age_60)
    
    # Summary for Alpha
    summary_alpha <- summary_country_age %>%
      filter(Variant == "Alpha") %>% 
      group_by(ReportCountry, TargetGroup) %>%
      summarise(denominator = sum(unique(last(Denominator))),
                DeathsObserved = sum(DeathsObserved, na.rm = TRUE),
                DeathsAvertedDose1 = sum(DeathsAvertedDose1, na.rm = TRUE),
                DeathsAvertedDose2 = sum(DeathsAvertedDose2, na.rm = TRUE),
                DeathsAvertedDose3 = sum(DeathsAvertedDose3, na.rm = TRUE),
                DeathsAvertedDose4 = sum(DeathsAvertedDose4, na.rm = TRUE),
                DeathsAvertedDose5 = sum(DeathsAvertedDose5, na.rm = TRUE),
                TotalAverted = sum(TotalAverted, na.rm = TRUE),
                DeathsExpected = sum(DeathsExpected, na.rm = TRUE),
                ObservedRate = round((DeathsObserved / denominator)*100000),
                ExpectedRate = round((DeathsExpected / denominator)*100000),
                pc_change = round((1-(DeathsObserved/DeathsExpected))*100)) %>%
      ungroup() %>% 
      select(ReportCountry, TargetGroup, DeathsObserved, TotalAverted, ObservedRate, ExpectedRate, pc_change) %>% 
      rename(ADeathsObserved = DeathsObserved,
             ATotalAverted = TotalAverted,
             AObservedRate = ObservedRate,
             AExpectedRate = ExpectedRate,
             Apc_change = pc_change) %>% 
      mutate(Apc_change = ifelse(is.na(Apc_change), "-", Apc_change))

    # Summary for Delta
    summary_delta <- summary_country_age %>%
      filter(Variant == "Delta") %>% 
      group_by(ReportCountry, TargetGroup) %>%
      summarise(denominator = sum(unique(last(Denominator))),
                DeathsObserved = sum(DeathsObserved, na.rm = TRUE),
                DeathsAvertedDose1 = sum(DeathsAvertedDose1, na.rm = TRUE),
                DeathsAvertedDose2 = sum(DeathsAvertedDose2, na.rm = TRUE),
                DeathsAvertedDose3 = sum(DeathsAvertedDose3, na.rm = TRUE),
                DeathsAvertedDose4 = sum(DeathsAvertedDose4, na.rm = TRUE),
                DeathsAvertedDose5 = sum(DeathsAvertedDose5, na.rm = TRUE),
                TotalAverted = sum(TotalAverted, na.rm = TRUE),
                DeathsExpected = sum(DeathsExpected, na.rm = TRUE),
                ObservedRate = round((DeathsObserved / denominator)*100000),
                ExpectedRate = round((DeathsExpected / denominator)*100000),
                pc_change = round((1-(DeathsObserved/DeathsExpected))*100)) %>%
      ungroup() %>% 
      select(ReportCountry, TargetGroup, DeathsObserved, TotalAverted, ObservedRate, ExpectedRate, pc_change) %>% 
      rename(DDeathsObserved = DeathsObserved,
             DTotalAverted = TotalAverted,
             DObservedRate = ObservedRate,
             DExpectedRate = ExpectedRate,
             Dpc_change = pc_change) %>% 
      mutate(Dpc_change = ifelse(is.na(Dpc_change), "-", Dpc_change))
    
    
    # Summary for Omicron
    summary_omicron <- summary_country_age %>%
      filter(Variant == "Omicron") %>% 
      group_by(ReportCountry, TargetGroup) %>%
      summarise(denominator = sum(unique(last(Denominator))),
                DeathsObserved = sum(DeathsObserved, na.rm = TRUE),
                DeathsAvertedDose1 = sum(DeathsAvertedDose1, na.rm = TRUE),
                DeathsAvertedDose2 = sum(DeathsAvertedDose2, na.rm = TRUE),
                DeathsAvertedDose3 = sum(DeathsAvertedDose3, na.rm = TRUE),
                DeathsAvertedDose4 = sum(DeathsAvertedDose4, na.rm = TRUE),
                DeathsAvertedDose5 = sum(DeathsAvertedDose5, na.rm = TRUE),
                TotalAverted = sum(TotalAverted, na.rm = TRUE),
                DeathsExpected = sum(DeathsExpected, na.rm = TRUE),
                ObservedRate = round((DeathsObserved / denominator)*100000),
                ExpectedRate = round((DeathsExpected / denominator)*100000),
                pc_change = round((1-(DeathsObserved/DeathsExpected))*100)) %>%
      ungroup() %>% 
      select(ReportCountry, TargetGroup, DeathsObserved, TotalAverted, ObservedRate, ExpectedRate, pc_change) %>% 
      rename(ODeathsObserved = DeathsObserved,
             OTotalAverted = TotalAverted,
             OObservedRate = ObservedRate,
             OExpectedRate = ExpectedRate,
             Opc_change = pc_change) %>% 
      mutate(Opc_change = ifelse(is.na(Opc_change), "-", Opc_change))
    
    
  # Join tables together
    Table.4 <- summary_alpha %>% 
      full_join(summary_delta) %>% 
      full_join(summary_omicron) %>% 
      distinct() %>% 
      mutate(ReportCountry = ifelse(ReportCountry == "Kosovo", "Kosovo*", ReportCountry)) %>% 
      mutate(ReportCountry = factor(ReportCountry, levels = c("Albania", "Austria", "Belgium", "Croatia", "Cyprus", "Czechia", "Denmark",
                                                              "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Iceland",
                                                              "Ireland", "Israel", "Italy", "Latvia", "Lithuania", "Luxembourg", 
                                                              "Malta", "Montenegro", "Netherlands", "North Macedonia", "Norway", "Poland",
                                                              "Portugal", "Republic of Moldova", "Romania", "Slovakia", "Slovenia", 
                                                              "Spain", "Sweden", "Switzerland", "Ukraine", "United Kingdom (England)",
                                                              "United Kingdom (Scotland)", "United Kingdom (Wales)", "Kosovo*", "Total"))) %>% 
      arrange(ReportCountry)
    
  }
  
    return(Table.4) 
}
