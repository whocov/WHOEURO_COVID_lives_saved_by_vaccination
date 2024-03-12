# Script to create summary table of lives saved by age, dose and variant


lives_saved_variant <- function(Expected_cases_all_ages, measure){
  
  if(measure == "By age and dose"){
    
    total_lives_saved_variant <- Expected_cases_all_ages %>% 
      ungroup() %>% 
      select(ReportCountry, year_week, TargetGroup, Variant, 
             DeathsAvertedDose1, DeathsAvertedDose2, DeathsAvertedDose3, DeathsAvertedDose4, DeathsAvertedDose5, TotalAverted) %>% 
      group_by(TargetGroup, Variant) %>% 
      summarise(AvertedDose1 = sum(DeathsAvertedDose1, na.rm = TRUE), 
                AvertedDose2 = sum(DeathsAvertedDose2, na.rm = TRUE),
                AvertedDose3 = sum(DeathsAvertedDose3, na.rm = TRUE),
                AvertedDose4 = sum(DeathsAvertedDose4, na.rm = TRUE),
                AvertedDose5 = sum(DeathsAvertedDose5, na.rm = TRUE),
                TotalAverted = sum(TotalAverted, na.rm = TRUE)) %>% 
      ungroup() %>% 
      group_by(TargetGroup) %>% 
      mutate(Variant = factor(Variant, levels = c("Index", "Alpha", "Delta", "Omicron"))) %>% 
      arrange(TargetGroup, Variant) %>% 
      ungroup()
    
    Age_25 <- total_lives_saved_variant %>%
      filter(TargetGroup == "Age 25-49") %>% 
      select(-TargetGroup) %>% 
      adorn_totals("row") %>% 
      t() %>% 
      as.data.frame() %>%
      row_to_names(row_number = 1) %>% 
      rownames_to_column(var = "Dosage") %>% 
      add_column(TargetGroup = "Age 25-49", .before = "Dosage")
      
    Age_50 <- total_lives_saved_variant %>%
      filter(TargetGroup == "Age 50-59") %>% 
      select(-TargetGroup) %>% 
      adorn_totals("row") %>% 
      t() %>% 
      as.data.frame() %>%
      row_to_names(row_number = 1) %>% 
      rownames_to_column(var = "Dosage") %>% 
      add_column(TargetGroup = "Age 50-59", .before = "Dosage")
    
    Age_60 <- total_lives_saved_variant %>%
      filter(TargetGroup == "Age 60+") %>% 
      select(-TargetGroup) %>% 
      adorn_totals("row") %>% 
      t() %>% 
      as.data.frame() %>%
      row_to_names(row_number = 1) %>% 
      rownames_to_column(var = "Dosage") %>% 
      add_column(TargetGroup = "Age 60+", .before = "Dosage")
    
    total_lives_saved_variant_dose <- Expected_cases_all_ages %>% 
      ungroup() %>% 
      select(ReportCountry, year_week, TargetGroup, Variant, 
             DeathsAvertedDose1, DeathsAvertedDose2, DeathsAvertedDose3, DeathsAvertedDose4, DeathsAvertedDose5, TotalAverted) %>% 
      group_by(Variant) %>% 
      summarise(AvertedDose1 = sum(DeathsAvertedDose1, na.rm = TRUE), 
                AvertedDose2 = sum(DeathsAvertedDose2, na.rm = TRUE),
                AvertedDose3 = sum(DeathsAvertedDose3, na.rm = TRUE),
                AvertedDose4 = sum(DeathsAvertedDose4, na.rm = TRUE),
                AvertedDose5 = sum(DeathsAvertedDose5, na.rm = TRUE),
                TotalAverted = sum(TotalAverted, na.rm = TRUE)) %>% 
      ungroup() %>% 
      adorn_totals("row") %>% 
      t() %>% 
      as.data.frame() %>%
      row_to_names(row_number = 1) %>% 
      rownames_to_column(var = "Dosage") %>% 
      add_column(TargetGroup = "Total", .before = "Dosage")
    
      
    
    summary_table <- Age_25 %>% 
      full_join(Age_50) %>% 
      full_join(Age_60) %>% 
      full_join(total_lives_saved_variant_dose) %>% 
      mutate(AlphaPc = round((as.numeric(Alpha) / as.numeric(Total))*100),
             DeltaPc = round((as.numeric(Delta) / as.numeric(Total))*100),
             OmicronPc = round((as.numeric(Omicron) / as.numeric(Total))*100),
             AlphaPc = ifelse(is.na(AlphaPc), "-", AlphaPc),
             DeltaPc = ifelse(is.na(DeltaPc), "-", DeltaPc),
             OmicronPc = ifelse(is.na(OmicronPc), "-", OmicronPc)) %>% 
      mutate(Index = trimws(format(as.numeric(Index), big.mark = ",")),
             Alpha = paste0(trimws(format(as.numeric(Alpha), big.mark = ",")), " (", AlphaPc, "%)"),
             Delta = paste0(trimws(format(as.numeric(Delta), big.mark = ",")), " (", DeltaPc, "%)"),
             Omicron = paste0(trimws(format(as.numeric(Omicron), big.mark = ",")), " (", OmicronPc, "%)"),
             Total = trimws(format(as.numeric(Total), big.mark = ","))) %>% 
      select(-c(AlphaPc, DeltaPc, OmicronPc)) %>% 
      mutate(Dosage = str_replace(Dosage, "Averted", ""),
             Dosage = str_replace(Dosage, "Dose", "Dose "),
             TargetGroup = str_replace(TargetGroup, "Age ", "")) %>% 
      rename(`Age group\n(years)` = TargetGroup)
    
    return(summary_table)
  }
    
  if(measure == "By finer age and dose"){
    
    total_lives_saved_variant_age <- Expected_cases_all_ages %>% 
      ungroup() %>% 
      select(ReportCountry, year_week, TargetGroup, Variant, 
             DeathsAvertedDose1, DeathsAvertedDose2, DeathsAvertedDose3, DeathsAvertedDose4, DeathsAvertedDose5, TotalAverted) 
    
    total_lives_saved_variant_age_60 <- Expected_cases_all_ages %>% 
      ungroup() %>% 
      select(ReportCountry, year_week, TargetGroup, Variant, 
             DeathsAvertedDose1, DeathsAvertedDose2, DeathsAvertedDose3, DeathsAvertedDose4, DeathsAvertedDose5, TotalAverted) %>% 
      filter(TargetGroup %in% c("Age 60-69", "Age 70-79", "Age 80+", "Age 60+")) %>% 
      mutate(TargetGroup = "Age 60+") %>% 
      group_by(Variant) %>% 
      summarise(AvertedDose1 = sum(DeathsAvertedDose1, na.rm = TRUE), 
                AvertedDose2 = sum(DeathsAvertedDose2, na.rm = TRUE),
                AvertedDose3 = sum(DeathsAvertedDose3, na.rm = TRUE),
                AvertedDose4 = sum(DeathsAvertedDose4, na.rm = TRUE),
                AvertedDose5 = sum(DeathsAvertedDose5, na.rm = TRUE),
                TotalAverted = sum(TotalAverted, na.rm = TRUE)) %>% 
      ungroup() %>% 
      add_column(TargetGroup = "Age 60+", .before = "Variant")
    
    
    total_lives_saved_variant_age <- total_lives_saved_variant_age %>% 
      filter(!TargetGroup == "Age 60+") %>% 
      group_by(TargetGroup, Variant) %>% 
      summarise(AvertedDose1 = sum(DeathsAvertedDose1, na.rm = TRUE), 
                AvertedDose2 = sum(DeathsAvertedDose2, na.rm = TRUE),
                AvertedDose3 = sum(DeathsAvertedDose3, na.rm = TRUE),
                AvertedDose4 = sum(DeathsAvertedDose4, na.rm = TRUE),
                AvertedDose5 = sum(DeathsAvertedDose5, na.rm = TRUE),
                TotalAverted = sum(TotalAverted, na.rm = TRUE)) %>% 
      ungroup() %>% 
      full_join(total_lives_saved_variant_age_60) %>% 
      group_by(TargetGroup) %>% 
      mutate(Variant = factor(Variant, levels = c("Index", "Alpha", "Delta", "Omicron"))) %>% 
      arrange(TargetGroup, Variant) %>% 
      ungroup() %>% 
      pivot_longer(cols = AvertedDose1 : TotalAverted,
                   names_to = "Dosage",
                   values_to = "Counts") %>% 
      pivot_wider(names_from = "Variant",
                  values_from = "Counts") %>% 
      group_by(TargetGroup, Dosage) %>% 
      mutate(Total = sum(Index, Alpha, Delta, Omicron)) %>% 
      ungroup()
    
    
    total_lives_saved_variant <- Expected_cases_all_ages %>% 
      ungroup() %>% 
      select(ReportCountry, year_week, TargetGroup, Variant, 
             DeathsAvertedDose1, DeathsAvertedDose2, DeathsAvertedDose3, DeathsAvertedDose4, DeathsAvertedDose5, TotalAverted) %>% 
      group_by(Variant) %>% 
      summarise(AvertedDose1 = sum(DeathsAvertedDose1, na.rm = TRUE), 
                AvertedDose2 = sum(DeathsAvertedDose2, na.rm = TRUE),
                AvertedDose3 = sum(DeathsAvertedDose3, na.rm = TRUE),
                AvertedDose4 = sum(DeathsAvertedDose4, na.rm = TRUE),
                AvertedDose5 = sum(DeathsAvertedDose5, na.rm = TRUE),
                TotalAverted = sum(TotalAverted, na.rm = TRUE)) %>% 
      ungroup() %>% 
      mutate(Variant = factor(Variant, levels = c("Index", "Alpha", "Delta", "Omicron"))) %>% 
      arrange(Variant) %>% 
      pivot_longer(cols = AvertedDose1 : TotalAverted,
                   names_to = "Dosage",
                   values_to = "Counts") %>% 
      pivot_wider(names_from = "Variant",
                  values_from = "Counts") %>% 
      group_by(Dosage) %>% 
      mutate(Total = sum(Index, Alpha, Delta, Omicron)) %>% 
      ungroup() %>% 
      add_column(TargetGroup = "Total", .before = "Dosage")
    
    summary_table <- full_join(total_lives_saved_variant_age, total_lives_saved_variant) %>% 
      select(-Index) %>% 
      mutate(AlphaPc = round((as.numeric(Alpha) / as.numeric(Total))*100),
             DeltaPc = round((as.numeric(Delta) / as.numeric(Total))*100),
             OmicronPc = round((as.numeric(Omicron) / as.numeric(Total))*100),
             AlphaPc = ifelse(is.na(AlphaPc), "-", AlphaPc),
             DeltaPc = ifelse(is.na(DeltaPc), "-", DeltaPc),
             OmicronPc = ifelse(is.na(OmicronPc), "-", OmicronPc)) %>% 
      mutate(Alpha = paste0(trimws(format(as.numeric(Alpha), big.mark = ",")), " (", AlphaPc, "%)"),
             Delta = paste0(trimws(format(as.numeric(Delta), big.mark = ",")), " (", DeltaPc, "%)"),
             Omicron = paste0(trimws(format(as.numeric(Omicron), big.mark = ",")), " (", OmicronPc, "%)"),
             Total = trimws(format(as.numeric(Total), big.mark = ","))) %>% 
      select(-c(AlphaPc, DeltaPc, OmicronPc)) %>% 
      mutate(Dosage = str_replace(Dosage, "Averted", ""),
             Dosage = str_replace(Dosage, "Dose", "Dose "),
             TargetGroup = str_replace(TargetGroup, "Age ", ""),
             TargetGroup = ifelse(TargetGroup == "60+", "≥60",
                                  ifelse(TargetGroup == "80+", "≥80", TargetGroup))) %>% 
      rename(`Age group\n(years)` = TargetGroup)
    

    return(summary_table)
  }
  
 # return()
}
