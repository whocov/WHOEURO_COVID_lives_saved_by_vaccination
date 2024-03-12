

create.expected.observed.by.variant.table <- function(Expected_cases_all_ages,
                                                      variant_months_region,
                                                      version){
  

  summary_table <- Expected_cases_all_ages %>% 
    ungroup() %>% 
    select(ReportCountry, TargetGroup, Variant, year_week, DeathsObserved, DeathsExpected, Denominator) %>%
    arrange(ReportCountry, TargetGroup, year_week) %>% 
    group_by(ReportCountry, TargetGroup, Variant) %>% 
    summarise(ObservedDeaths = sum_keep_na(DeathsObserved),
              ExpectedDeaths = sum_keep_na(DeathsExpected),
              LivesSaved = ExpectedDeaths - ObservedDeaths,
              Population = sum(unique(last(Denominator)))) %>% 
    ungroup() 
  
  
  # Adding 60+ age group for ease of calculation
  summary_temp <- Expected_cases_all_ages %>% 
    filter(TargetGroup %in% c("Age 60-69", "Age 70-79", "Age 80+", "Age 60+")) %>% 
    mutate(TargetGroup = "Age 60+") %>% 
    group_by(ReportCountry, TargetGroup, Variant) %>% 
    summarise(ObservedDeaths = sum_keep_na(DeathsObserved),
              ExpectedDeaths = sum_keep_na(DeathsExpected),
              LivesSaved = ExpectedDeaths - ObservedDeaths,
              Population = sum(unique(last(Denominator)))) %>% 
    ungroup() 
  
  summary_table <- full_join(summary_table, summary_temp)
  
  if(version == "Don't include monthly breakdown"){
    # Calculate deaths observed, expected by variant and age groups --------------
    summary_table1 <- summary_table %>% 
      full_join(summary_temp) %>% 
      group_by(TargetGroup, Variant) %>% 
      summarise(ObservedDeaths = sum_keep_na(ObservedDeaths),
                ExpectedDeaths = sum_keep_na(ExpectedDeaths),
                LivesSaved = ExpectedDeaths - ObservedDeaths,
                Population = sum(Population)) %>% 
      ungroup() 
    
    
    summary_by_age_variants <- summary_table1 %>% 
      group_by(TargetGroup, Variant) %>% 
      mutate(ObservedMortalityRate = round((ObservedDeaths / Population) *100000),
             ExpectedMortalityRate = round((ExpectedDeaths / Population) *100000)) %>% 
      ungroup() %>% 
      group_by(TargetGroup) %>% 
      mutate(ObservedDeaths = paste0(trimws(format(ObservedDeaths, big.mark = ",")), " (", 
                                     trimws(format(ObservedMortalityRate, big.mark = ",")), ")"),
             ExpectedDeaths = paste0(trimws(format(ExpectedDeaths, big.mark = ",")), " (", 
                                     trimws(format(ExpectedMortalityRate, big.mark = ",")), ")"),
             LivesSaved = trimws(format(LivesSaved, big.mark = ","))) %>% 
      ungroup() %>% 
      select(-c(Population, ObservedMortalityRate, ExpectedMortalityRate)) %>% 
      pivot_longer(cols = c(ObservedDeaths : LivesSaved),
                   names_to = "Mortality",
                   values_to = "Counts") %>% 
      pivot_wider(names_from = "Variant",
                  values_from = "Counts")
    
    summary_by_age <- summary_table1 %>% 
      group_by(TargetGroup) %>% 
      summarise(ObservedDeaths = sum(ObservedDeaths),
                ExpectedDeaths = sum(ExpectedDeaths),
                LivesSaved = ExpectedDeaths - ObservedDeaths,
                Population = sum(unique(Population)),
                ObservedMortalityRate = round((ObservedDeaths / Population) *100000),
                ExpectedMortalityRate = round((ExpectedDeaths / Population) *100000)) %>% 
      ungroup() %>% 
      group_by(TargetGroup) %>% 
      mutate(ObservedDeaths = paste0(trimws(format(ObservedDeaths, big.mark = ",")), " (", 
                                     trimws(format(ObservedMortalityRate, big.mark = ",")), ")"),
             ExpectedDeaths = paste0(trimws(format(ExpectedDeaths, big.mark = ",")), " (", 
                                     trimws(format(ExpectedMortalityRate, big.mark = ",")), ")"),
             LivesSaved = trimws(format(LivesSaved, big.mark = ","))) %>% 
      ungroup() %>% 
      select(-c(Population, ObservedMortalityRate, ExpectedMortalityRate)) %>% 
      pivot_longer(cols = c(ObservedDeaths : LivesSaved),
                   names_to = "Mortality",
                   values_to = "Total") 
    
  
      summary_table_main <- full_join(summary_by_age_variants, summary_by_age) %>% 
        mutate(Mortality = str_remove(Mortality, "Deaths"),
               Mortality = str_replace(Mortality, "Observed", "Reported"))
        
    
  
    
    # Calculate totals by variant ------------------------------------------------
      totals_variants <- summary_table %>% 
        filter(TargetGroup %in% c("Age 25-49", "Age 50-59", "Age 60+")) %>% 
        group_by(Variant) %>% 
        summarise(ObservedDeaths = sum_keep_na(ObservedDeaths),
                  ExpectedDeaths = sum_keep_na(ExpectedDeaths),
                  LivesSaved = ExpectedDeaths - ObservedDeaths,
                  Population = sum(unique(Population))) %>% 
        ungroup() %>% 
        adorn_totals("row") %>% 
        mutate(LivesSaved = trimws(format(LivesSaved, big.mark = ",")),
               ObservedMortalityRate = round((ObservedDeaths / Population) *100000),
               ExpectedMortalityRate = round((ExpectedDeaths / Population) *100000),
               ObservedDeaths = paste0(trimws(format(ObservedDeaths, big.mark = ",")), " (", 
                                       trimws(format(ObservedMortalityRate, big.mark = ",")), ")"),
               ExpectedDeaths = paste0(trimws(format(ExpectedDeaths, big.mark = ",")), " (", 
                                       trimws(format(ExpectedMortalityRate, big.mark = ",")), ")"),
               Variant = factor(Variant, levels = c("Index", "Alpha", "Delta", "Omicron", "Total"))) %>%
        arrange(Variant) %>% 
        select(-c(Population, ObservedMortalityRate, ExpectedMortalityRate)) %>% 
        pivot_longer(cols = ObservedDeaths : LivesSaved,
                     names_to = "Mortality",
                     values_to = "Counts") %>% 
        pivot_wider(names_from = "Variant",
                    values_from = "Counts") %>% 
        mutate(Mortality = str_remove(Mortality, "Deaths"),
               Mortality = str_replace(Mortality, "Observed", "Reported")) %>% 
        add_column(TargetGroup = "Total", .before = "Mortality")
      
  
    # Calculate totals by age ----------------------------------------------------
      totals_ages <- summary_table %>% 
        filter(TargetGroup %in% c("Age 25-49", "Age 50-59", "Age 60+")) %>% 
        group_by(TargetGroup) %>% 
        summarise(ObservedDeaths = sum_keep_na(ObservedDeaths),
                  ExpectedDeaths = sum_keep_na(ExpectedDeaths),
                  LivesSaved = ExpectedDeaths - ObservedDeaths,
                  Population = sum(unique(Population))) %>% 
        ungroup() %>% 
        adorn_totals("row") %>% 
        mutate(LivesSaved = trimws(format(LivesSaved, big.mark = ",")),
               ObservedMortalityRate = round((ObservedDeaths / Population) *100000),
               ExpectedMortalityRate = round((ExpectedDeaths / Population) *100000),
               ObservedDeaths = paste0(trimws(format(ObservedDeaths, big.mark = ",")), " (", 
                                       trimws(format(ObservedMortalityRate, big.mark = ",")), ")"),
               ExpectedDeaths = paste0(trimws(format(ExpectedDeaths, big.mark = ",")), " (", 
                                       trimws(format(ExpectedMortalityRate, big.mark = ",")), ")")) %>%
        select(-c(Population, ObservedMortalityRate, ExpectedMortalityRate)) %>% 
        pivot_longer(cols = ObservedDeaths : LivesSaved,
                     names_to = "Mortality",
                     values_to = "Total") %>% 
        mutate(Mortality = str_remove(Mortality, "Deaths"),
               Mortality = str_replace(Mortality, "Observed", "Reported")) 
      
      
  # Create final table
    observed_expected_table <- summary_table_main %>% 
      #full_join(totals_ages) %>% 
      full_join(totals_variants) %>% 
      select(TargetGroup, Mortality, Index, Alpha, Delta, Omicron, Total) %>% 
      mutate(TargetGroup = str_remove(TargetGroup, "Age "),
             TargetGroup = ifelse(TargetGroup == "60+", "≥60", TargetGroup),
             TargetGroup = ifelse(TargetGroup == "80+", "≥80", TargetGroup)) %>% 
      rename(`Age group\n(years)` = TargetGroup)
    
    return(observed_expected_table)
    
  }
  
  if(version == "Include monthly breakdown"){
    
    # Find number of months for dominant VOC
    variant_months_region_tm <- variant_months_region %>% 
      select(Variant, n_months) %>% 
      distinct() 

    
    summary_table1 <- summary_table %>% 
      group_by(TargetGroup, Variant) %>% 
      summarise(ObservedDeaths = sum_keep_na(ObservedDeaths),
                ExpectedDeaths = sum_keep_na(ExpectedDeaths),
                LivesSaved = ExpectedDeaths - ObservedDeaths,
                Population = sum(unique(Population))) %>% 
      ungroup() %>% 
      full_join(variant_months_region_tm)
    
  # Calculate deaths observed, expected by variant and age groups --------------
    summary_by_age_variants <- summary_table1 %>% 
      group_by(TargetGroup, Variant) %>% 
      mutate(ObservedDeathsPVM = trimws(format(round(ObservedDeaths / n_months), big.mark = ",")),
             ExpectedDeathsPVM = trimws(format(round(ExpectedDeaths / n_months), big.mark = ",")),
             LivesSavedPVM = trimws(format(round(LivesSaved / n_months), big.mark = ",")),
             ObservedMortalityRate = round((ObservedDeaths / Population) *100000),
             ExpectedMortalityRate = round((ExpectedDeaths / Population) *100000)) %>% 
      ungroup() %>% 
      group_by(TargetGroup) %>% 
      mutate(ObservedDeaths = paste0(trimws(format(ObservedDeaths, big.mark = ",")), " (", 
                                     trimws(format(ObservedMortalityRate, big.mark = ",")), ")"),
             ExpectedDeaths = paste0(trimws(format(ExpectedDeaths, big.mark = ",")), " (", 
                                     trimws(format(ExpectedMortalityRate, big.mark = ",")), ")"),
             LivesSaved = trimws(format(LivesSaved, big.mark = ","))) %>% 
      ungroup() %>% 
      select(-c(Population, n_months, ObservedMortalityRate, ExpectedMortalityRate)) %>% 
      pivot_longer(cols = c(ObservedDeaths : LivesSavedPVM),
                   names_to = "Mortality",
                   values_to = "Counts") %>% 
      pivot_wider(names_from = "Variant",
                  values_from = "Counts") 

    
    summary_by_age <- summary_table1 %>% 
      group_by(TargetGroup) %>% 
      summarise(ObservedDeaths = sum(ObservedDeaths),
                ExpectedDeaths = sum(ExpectedDeaths),
                LivesSaved = ExpectedDeaths - ObservedDeaths,
                Population = sum(unique(Population)),
                ObservedMortalityRate = round((ObservedDeaths / Population) *100000),
                ExpectedMortalityRate = round((ExpectedDeaths / Population) *100000)) %>% 
      ungroup() %>% 
      group_by(TargetGroup) %>% 
      mutate(ObservedDeaths = paste0(trimws(format(ObservedDeaths, big.mark = ",")), " (", 
                                     trimws(format(ObservedMortalityRate, big.mark = ",")), ")"),
             ExpectedDeaths = paste0(trimws(format(ExpectedDeaths, big.mark = ",")), " (", 
                                     trimws(format(ExpectedMortalityRate, big.mark = ",")), ")"),
             LivesSaved = trimws(format(LivesSaved, big.mark = ","))) %>% 
      ungroup() %>% 
      select(-c(Population, ObservedMortalityRate, ExpectedMortalityRate)) %>% 
      pivot_longer(cols = c(ObservedDeaths : LivesSaved),
                   names_to = "Mortality",
                   values_to = "Total") 
      
     summary_table_main <- full_join(summary_by_age_variants, summary_by_age) %>% 
       mutate(Mortality = str_replace(Mortality, "Deaths", ""),
              Mortality = str_replace(Mortality, "PVM", " PVM"),
              Mortality = str_replace(Mortality, "Observed", "Reported"),
              Mortality = str_replace(Mortality, "LivesSaved", "Lives Saved"),
              Mortality = str_replace(Mortality, "Lives SavedPVM", "Lives Saved PVM")) %>% 
       mutate(Mortality = factor(Mortality, levels = c("Reported", "Reported PVM", 
                                                       "Expected", "Expected PVM", 
                                                       "Lives Saved", "Lives Saved PVM"))) %>% 
       arrange(TargetGroup, Mortality) 
     
    
 
    # Calculate totals by variant ------------------------------------------------
    totals_variants <- summary_table1 %>% 
      filter(TargetGroup %in% c("Age 25-49", "Age 50-59", "Age 60+")) %>% 
      group_by(Variant) %>% 
      summarise(ObservedDeaths = sum_keep_na(ObservedDeaths),
                ExpectedDeaths = sum_keep_na(ExpectedDeaths),
                LivesSaved = ExpectedDeaths - ObservedDeaths,
                Population = sum(unique(Population)),
                n_months = n_months) %>% 
      ungroup() %>% 
      distinct() %>% 
      adorn_totals("row") %>% 
      mutate(ObservedDeathsPVM = trimws(format(round(ObservedDeaths / n_months), big.mark = ",")),
             ExpectedDeathsPVM = trimws(format(round(ExpectedDeaths / n_months), big.mark = ",")),
             LivesSavedPVM = trimws(format(round(LivesSaved / n_months), big.mark = ",")),
             LivesSaved = trimws(format(LivesSaved, big.mark = ",")),
             ObservedMortalityRate = round((ObservedDeaths / Population) *100000),
             ExpectedMortalityRate = round((ExpectedDeaths / Population) *100000),
             ObservedDeaths = paste0(trimws(format(ObservedDeaths, big.mark = ",")), " (", 
                                     trimws(format(ObservedMortalityRate, big.mark = ",")), ")"),
             ExpectedDeaths = paste0(trimws(format(ExpectedDeaths, big.mark = ",")), " (", 
                                     trimws(format(ExpectedMortalityRate, big.mark = ",")), ")"),
             Variant = factor(Variant, levels = c("Index", "Alpha", "Delta", "Omicron", "Total"))) %>%
      arrange(Variant) %>% 
      select(-c(Population, n_months, ObservedMortalityRate, ExpectedMortalityRate)) %>% 
      pivot_longer(cols = ObservedDeaths : LivesSavedPVM,
                   names_to = "Mortality",
                   values_to = "Counts") %>% 
      pivot_wider(names_from = "Variant",
                  values_from = "Counts") %>% 
      mutate(Mortality = str_remove(Mortality, "Deaths")) %>% 
      mutate(Mortality = str_replace(Mortality, "Deaths", ""),
             Mortality = str_replace(Mortality, "PVM", " PVM"),
             Mortality = str_replace(Mortality, "Observed", "Reported"),
             Mortality = str_replace(Mortality, "LivesSaved", "Lives Saved"),
             Mortality = str_replace(Mortality, "Lives SavedPVM", "Lives Saved PVM")) %>% 
      mutate(Mortality = factor(Mortality, levels = c("Reported", "Reported PVM", 
                                                      "Expected", "Expected PVM", 
                                                      "Lives Saved", "Lives Saved PVM"))) %>% 
      arrange(Mortality) %>%
      add_column(TargetGroup = "Total", .before = "Mortality")


    # Create final table
    observed_expected_table <- summary_table_main %>% 
      full_join(totals_variants) %>% 
      select(TargetGroup, Mortality, Index, Alpha, Delta, Omicron, Total) %>% 
      mutate(TargetGroup = str_remove(TargetGroup, "Age "),
             TargetGroup = ifelse(TargetGroup == "60+", "≥60", TargetGroup),
             TargetGroup = ifelse(TargetGroup == "80+", "≥80", TargetGroup)) %>% 
      rename(`Age group\n(years)` = TargetGroup)
    
    return(observed_expected_table)
    
  }
  
}