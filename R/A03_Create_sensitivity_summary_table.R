# Script to create summary tables of different sensitivity analyses
# Margaux Mesle
# May 2022




create_sensitivity_analyses_summary_table <- function(countries_included, resolution){
  

  # Create list of all file names needed
  csvfiles <- list.files(here::here("Outputs/Sensitivity_analyses"), pattern = paste0(reporting.week,'.csv'))
  
  ############################################
  # Supplementary table 1 - Regional results #
  ############################################
  
  if(resolution =="Region"){
  
    table.S1.template <- data.frame()
    
    for(i in seq_along(csvfiles)){
    
    if(i == 1){
      scenario.label <- "1. High VE values"
      csvfile <- paste0("Expected_cases_yes_VE_high_", reporting.week, ".csv")
    } else if(i == 2){
      scenario.label <- "2. Low VE values"
      csvfile <- paste0("Expected_cases_yes_VE_low_", reporting.week, ".csv")
    } else if(i == 3){
      scenario.label <- "3. Long lag time"
      csvfile <- paste0("Expected_cases_yes_lag_long_", reporting.week, ".csv")
    } else if(i == 4){
      scenario.label <- "4. Short lag time"
      csvfile <- paste0("Expected_cases_yes_lag_short_", reporting.week, ".csv")
    } else if(i == 5){
      scenario.label <- "5. Long waning time"
      csvfile <- paste0("Expected_cases_yes_waning_slow_", reporting.week, ".csv")
    } else if(i == 6){
      scenario.label <- "6. Short waning time"
      csvfile <- paste0("Expected_cases_yes_waning_fast_", reporting.week, ".csv")
    } else if(i == 7){
      scenario.label <- "7. Prior immunity high"
      csvfile <- paste0("Expected_cases_yes_prior_immunity_high_", reporting.week, ".csv")
    } else if(i == 8){
      scenario.label <- "8. Prior immunity low"
      csvfile <- paste0("Expected_cases_yes_prior_immunity_low_", reporting.week, ".csv")
    }
    
    data_in <- read.csv(here::here("Outputs/Sensitivity_analyses/", csvfile))
    
    temp <- data_in %>% 
      filter(TargetGroup %in% c("Age 60-69", "Age 70-79", "Age 80+", "Age 60+")) %>% 
      mutate(TargetGroup = "Age 60+") %>% 
      group_by(ReportCountry, TargetGroup, year_week, Variant) %>% 
      summarise(DeathsObserved = sum(DeathsObserved, na.rm = TRUE), 
                DeathsExpected = sum(DeathsExpected, na.rm = TRUE),
                TotalAverted = sum(TotalAverted, na.rm = TRUE),
                Denominator = sum(unique(Denominator))) %>% 
      ungroup() 
    
    deaths_region <- data_in %>% 
      full_join(temp) %>% 
      filter(!TargetGroup %in% c("Age 60-69", "Age 70-79", "Age 80+")) %>% 
      group_by(TargetGroup, Variant) %>% 
      summarise(TotalObserved = sum(DeathsObserved, na.rm=TRUE),
                TotalExpected = sum(DeathsExpected, na.rm=TRUE),
                TotalAverted = sum(TotalAverted, na.rm=TRUE),
                TotalPopulation = sum(unique(last(Denominator)), na.rm=TRUE)) %>% 
      mutate(MortalityRateExpected = round((TotalExpected/TotalPopulation)*100000),
             pc_Expected = round((1-(TotalObserved/TotalExpected))*100)) %>% 
      ungroup() %>% 
      mutate(Variant = factor(Variant, levels = variant.order)) %>% 
      arrange(Variant) 
    
    pop_25 <- deaths_region %>% filter(TargetGroup == "Age 25-49") %>% head(1) %>% pull(TotalPopulation)
    pop_50 <- deaths_region %>% filter(TargetGroup == "Age 50-59") %>% head(1) %>% pull(TotalPopulation)
    pop_60 <- deaths_region %>% filter(TargetGroup == "Age 60+") %>% head(1) %>% pull(TotalPopulation)
    
    temp_25 <- deaths_region %>% 
      filter(TargetGroup == "Age 25-49") %>% 
      select(-c(MortalityRateExpected, pc_Expected)) %>% 
      adorn_totals("row") %>% 
      mutate(TotalPopulation = pop_25) %>% 
      mutate(Variant = ifelse(TargetGroup == "Total", "Total", Variant),
             TargetGroup = "Age 25-49") %>% 
      pivot_wider(names_from = Variant,
                  values_from = c(TotalAverted, TotalObserved, TotalExpected)) %>% 
      mutate(TotalPopulation = pop_25)
    
    temp_50 <- deaths_region %>% 
      filter(TargetGroup == "Age 50-59") %>% 
      select(-c(MortalityRateExpected, pc_Expected)) %>% 
      adorn_totals("row") %>% 
      mutate(TotalPopulation = pop_50) %>% 
      mutate(Variant = ifelse(TargetGroup == "Total", "Total", Variant),
             TargetGroup = "Age 50-59") %>% 
      pivot_wider(names_from = Variant,
                  values_from = c(TotalAverted, TotalObserved, TotalExpected)) %>% 
      mutate(TotalPopulation = pop_50)
    
    temp_60 <- deaths_region %>% 
      filter(TargetGroup == "Age 60+") %>% 
      select(-c(MortalityRateExpected, pc_Expected)) %>% 
      adorn_totals("row") %>% 
      mutate(TotalPopulation = pop_60) %>% 
      mutate(Variant = ifelse(TargetGroup == "Total", "Total", Variant),
             TargetGroup = "Age 60+") %>% 
      pivot_wider(names_from = Variant,
                  values_from = c(TotalAverted, TotalObserved, TotalExpected)) %>% 
      mutate(TotalPopulation = pop_60)
    
    summary_deaths_variant <- rbind(temp_25, temp_50, temp_60) %>% 
      adorn_totals("row") %>% 
      mutate(Index_MR = round((TotalAverted_Index / TotalPopulation)*100000),
             Alpha_MR = round((TotalAverted_Alpha / TotalPopulation)*100000),
             Delta_MR = round((TotalAverted_Delta / TotalPopulation)*100000),
             Omicron_MR = round((TotalAverted_Omicron / TotalPopulation)*100000),
             Total_MR = round((TotalAverted_Total / TotalPopulation)*100000)) %>% 
      mutate(Index_pc = round((1-(TotalObserved_Index / TotalExpected_Index))*100),
             Alpha_pc = round((1-(TotalObserved_Alpha / TotalExpected_Alpha))*100),
             Delta_pc = round((1-(TotalObserved_Delta / TotalExpected_Delta))*100),
             Omicron_pc = round((1-(TotalObserved_Omicron / TotalExpected_Omicron))*100),
             Total_pc = round((1-(TotalObserved_Total / TotalExpected_Total))*100)) %>% 
      select(TargetGroup, TotalAverted_Index, Index_MR, Index_pc, 
             TotalAverted_Alpha, Alpha_MR, Alpha_pc,
             TotalAverted_Delta, Delta_MR, Delta_pc,
             TotalAverted_Omicron, Omicron_MR, Omicron_pc, 
             TotalAverted_Total, Total_MR, Total_pc) %>% 
      add_column(Scenario = scenario.label, .after = "TargetGroup")
    
    table.S1.template <- rbind(table.S1.template, summary_deaths_variant) %>% 
      distinct()
    
    }
    
    Table.S1 <- table.S1.template
 
    return(Table.S1)
    
  }
  
  ###########################################
  # Supplementary table 1 - Country results #
  ###########################################
  
  if(resolution =="Country"){

    # Create table template to be filled in --------------------------------------
    table.S2.template <- data.frame(expand.grid(ReportCountry = unique(countries_included$CountryName),
                                                Scenario_nb = "", 
                                                Deaths_Observed = NA, 
                                                Deaths_Averted = NA, 
                                                Mortality_rate_observed = NA,
                                                Mortality_rate_expected = NA, 
                                                pc_expected = NA))
      
    # Find numbers from each scenario ------------------------------------------
    for(i in seq_along(csvfiles)){
      

      if(i == 1){
        scenario.label <- "1. High VE values"
        csvfile <- paste0("Expected_cases_yes_VE_high_", reporting.week, ".csv")
      } else if(i == 2){
        scenario.label <- "2. Low VE values"
        csvfile <- paste0("Expected_cases_yes_VE_low_", reporting.week, ".csv")
      } else if(i == 3){
        scenario.label <- "3. Long lag time"
        csvfile <- paste0("Expected_cases_yes_lag_long_", reporting.week, ".csv")
      } else if(i == 4){
        scenario.label <- "4. Short lag time"
        csvfile <- paste0("Expected_cases_yes_lag_short_", reporting.week, ".csv")
      } else if(i == 5){
        scenario.label <- "5. Long waning time"
        csvfile <- paste0("Expected_cases_yes_waning_slow_", reporting.week, ".csv")
      } else if(i == 6){
        scenario.label <- "6. Short waning time"
        csvfile <- paste0("Expected_cases_yes_waning_fast_", reporting.week, ".csv")
      } else if(i == 7){
        scenario.label <- "7. Prior immunity high"
        csvfile <- paste0("Expected_cases_yes_prior_immunity_high_", reporting.week, ".csv")
      } else if(i == 8){
        scenario.label <- "8. Prior immunity low"
        csvfile <- paste0("Expected_cases_yes_prior_immunity_low_", reporting.week, ".csv")
      }
      
      data_in <- read.csv(here::here("Outputs/Sensitivity_analyses/", csvfile))
      
      temp <- data_in %>% 
        filter(TargetGroup %in% c("Age 60-69", "Age 70-79", "Age 80+", "Age 60+")) %>% 
        mutate(TargetGroup = "Age 60+") %>% 
        group_by(ReportCountry, TargetGroup) %>% 
        summarise(DeathsObserved = sum(DeathsObserved, na.rm = TRUE), 
                  DeathsExpected = sum(DeathsExpected, na.rm = TRUE),
                  TotalAverted = sum(TotalAverted, na.rm = TRUE),
                  Denominator = sum(unique(Denominator))) %>% 
        ungroup() 
      
      summary.scenario.x <- data_in %>% 
        full_join(temp) %>% 
        filter(!TargetGroup %in% c("Age 60-69", "Age 70-79", "Age 80+")) %>% 
        group_by(ReportCountry) %>% 
        summarise(Deaths_Observed = sum(DeathsObserved, na.rm=TRUE),
                  Deaths_Expected = sum(DeathsExpected, na.rm=TRUE),
                  Deaths_Averted = sum(TotalAverted, na.rm=TRUE),
                  denominator = sum(unique(last(Denominator), na.rm=TRUE))) %>% 
        ungroup() %>% 
        adorn_totals("row") %>% 
        mutate(Mortality_rate_observed = round((Deaths_Observed/denominator)*100000),
               Mortality_rate_expected = round((Deaths_Expected/denominator)*100000),
               pc_expected = round((1-(Mortality_rate_observed/Mortality_rate_expected))*100)) %>% 
        mutate(Scenario_nb = scenario.label) %>% 
        select(ReportCountry, Scenario_nb, Deaths_Observed, Deaths_Averted, Mortality_rate_observed, Mortality_rate_expected, pc_expected) 
      
      
      table.S2.template <- rbind(table.S2.template, summary.scenario.x)

    }
      
      Table.S2 <- table.S2.template %>% 
        mutate(ReportCountry = as.character(ReportCountry),
               ReportCountry = ifelse(ReportCountry == "Kosovo", "Kosovo*", ReportCountry)) %>% 
        mutate(ReportCountry = factor(ReportCountry, levels = c("Albania", "Austria", "Belgium", "Croatia", "Cyprus", "Czechia",
                                                                "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", 
                                                                "Hungary", "Iceland", "Ireland", "Israel", "Italy", "Latvia",
                                                                "Lithuania", "Luxembourg", "Malta", "Montenegro", "North Macedonia",
                                                                "Netherlands", "Norway", "Poland", "Portugal", "Republic of Moldova",
                                                                "Romania", "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland", 
                                                                "United Kingdom (England)", "United Kingdom (Scotland)", "Ukraine", 
                                                                "United Kingdom (Wales)", "Kosovo*", "Total"))) %>% 
        filter(!Scenario_nb == "") %>% 
        distinct() %>% 
        arrange(ReportCountry, Scenario_nb)
      
    return(Table.S2)
    
  }
    
  
}