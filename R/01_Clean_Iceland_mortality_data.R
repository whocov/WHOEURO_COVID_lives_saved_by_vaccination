# Script to read, clean and prepare Iceland mortality data as sent by country
# Margaux Mesle
# May 2023

clean.Iceland.mortality.data <- function(Iceland_mortality, 
                                         resolution){
  
    IS_2020 <- Iceland_mortality %>% 
      t() %>% 
      as.data.frame() %>% 
      select(1 : 15) %>% 
      row_to_names(row_number = 1) %>% 
      rownames_to_column(var = "year_week") %>% 
      select(year_week : `80+`) %>% 
      select(-`2020`) %>% 
      filter(!year_week == "Grand Total") %>% 
      mutate(year_week = str_remove(year_week, "Week "),
             year_week = sprintf("%02d", as.numeric(year_week)),
             year_week = paste0("2020-", year_week)) %>% 
      mutate_at(c(2 : 15), as.numeric)
      
    IS_2021 <- Iceland_mortality %>% 
      t() %>% 
      as.data.frame() %>% 
      select(16 : 30) %>% 
      row_to_names(row_number = 1) %>% 
      rownames_to_column(var = "year_week") %>% 
      select(year_week : `80+`) %>% 
      select(-`2021`) %>% 
      filter(!year_week == "Grand Total") %>% 
      mutate(year_week = str_remove(year_week, "Week "),
             year_week = sprintf("%02d", as.numeric(year_week)),
             year_week = paste0("2021-", year_week)) %>% 
      rename_with(~ sub(".1$", "", .x), everything()) %>% 
      mutate_at(c(2 : 15), as.numeric)
    
    IS_2022 <- Iceland_mortality %>% 
      t() %>% 
      as.data.frame() %>% 
      select(31 : 45) %>% 
      row_to_names(row_number = 1) %>% 
      rownames_to_column(var = "year_week") %>% 
      select(year_week : `80+`) %>% 
      select(-`2022`) %>% 
      filter(!year_week == "Grand Total") %>% 
      mutate(year_week = str_remove(year_week, "Week "),
             year_week = sprintf("%02d", as.numeric(year_week)),
             year_week = paste0("2022-", year_week)) %>% 
      rename_with(~ sub(".2$", "", .x), everything()) %>% 
      mutate_at(c(2 : 15), as.numeric)
    
    
    IS_2023 <- Iceland_mortality %>% 
      t() %>% 
      as.data.frame() %>% 
      select(46 : 60) %>% 
      row_to_names(row_number = 1) %>% 
      rownames_to_column(var = "year_week") %>% 
      select(year_week : `80+`) %>% 
      select(-`2023`) %>% 
      filter(!year_week == "Grand Total") %>% 
      mutate(year_week = str_remove(year_week, "Week "),
             year_week = sprintf("%02d", as.numeric(year_week)),
             year_week = paste0("2023-", year_week)) %>% 
      rename_with(~ sub(".3$", "", .x), everything()) %>% 
      mutate_at(c(2 : 15), as.numeric) %>% 
      filter(!is.na(`80+`))
    
    
    if(resolution == "finer ages"){
    
      IS_clean <- IS_2020 %>% 
        full_join(IS_2021) %>% 
        full_join(IS_2022) %>% 
        full_join(IS_2023) %>% 
        group_by(year_week) %>% 
        summarise(`Age 25-49`= sum(`25-29`, `30-39`, `40-49`),
                  `Age 50-59` = sum(`50-59`),
                  `Age 60-69` = sum(`60-64`, `65-69`),
                  `Age 70-79` = sum(`70-74`, `75-79`),
                  `Age 80+` = sum(`80+`)) %>% 
        pivot_longer(cols = `Age 25-49` : `Age 80+`,
                     names_to = "TargetGroup",
                     values_to = "DeathsObserved") %>% 
        add_column(ReportCountry = "Iceland", .before = "year_week") %>% 
        add_column(Period = "Omicron", .before = "year_week") %>% 
        mutate(Period = ifelse(year_week %in% omicron.weeks, "Omicron", "Pre-Omicron"))
    
    } else {
      
      IS_clean <- IS_2020 %>% 
        full_join(IS_2021) %>% 
        full_join(IS_2022) %>% 
        full_join(IS_2023) %>% 
        group_by(year_week) %>% 
        summarise(`Age 25-49`= sum(`25-29`, `30-39`, `40-49`),
                  `Age 50-59` = sum(`50-59`),
                  `Age 60+` = sum(`60-64`, `65-69`, `70-74`, `75-79`, `80+`)) %>% 
        pivot_longer(cols = `Age 25-49` : `Age 60+`,
                     names_to = "TargetGroup",
                     values_to = "DeathsObserved") %>% 
        add_column(ReportCountry = "Iceland", .before = "year_week") %>% 
        add_column(Period = "Omicron", .before = "year_week") %>% 
        mutate(Period = ifelse(year_week %in% omicron.weeks, "Omicron", "Pre-Omicron"))
      
    }
    
  return(IS_clean)
}