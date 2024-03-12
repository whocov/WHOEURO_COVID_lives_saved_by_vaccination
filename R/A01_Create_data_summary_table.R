# Script to create summary table of which countries are included in the analysis according to age group considered
# Margaux Mesle
# May 2022


create.data.summary.table <- function(deaths, 
                                      vaccine, 
                                      variants, 
                                      country_codes, 
                                      countries_partial_reporting, 
                                      countries_no_reporting){
  
  # Find out which countries have mortality data by age groups -----------------
  mortality_data <- deaths %>%
    mutate(CountryName = ifelse(grepl("^UK-ENG_*", DataSource), "United Kingdom (England)", CountryName),
           CountryName = ifelse(grepl("^UK-NI_*", DataSource), "United Kingdom (Northern Ireland)", CountryName),
           CountryName = ifelse(grepl("^UK-SCOT_*", DataSource), "United Kingdom (Scotland)", CountryName),
           CountryName = ifelse(grepl("^UK-WALES_*", DataSource), "United Kingdom (Wales)", CountryName),
           CountryName = ifelse(CountryName == "Moldova", "Republic of Moldova", CountryName),
           CountryName = ifelse(CountryName == "Russia", "Russian Federation", CountryName)) %>%
    dplyr::select(CountryName, DateUsedForStatisticsISO | (starts_with("Deaths"))) %>%
    mutate(DateUsedForStatisticsISO = str_replace(DateUsedForStatisticsISO, "W", ""),
           Period = ifelse(DateUsedForStatisticsISO %in% omicron.weeks, "Full analysis", "Pre-Omicron")) %>% 
    group_by(CountryName, Period) %>%
    summarise(`Deaths_25-49` = sum(c(Deaths25.29F, Deaths25.29M, Deaths30.39F, Deaths30.39M, Deaths40.49F, Deaths40.49M), na.rm = TRUE),
              `Deaths_50-59` = sum(c(Deaths50.59F, Deaths50.59M), na.rm = TRUE),
              `Deaths_60+` = sum(c(Deaths60.64F, Deaths60.64M, Deaths65.69F, Deaths65.69M, Deaths70.74F, Deaths70.74M, 
                                   Deaths75.79F, Deaths75.79M, Deaths80.F, Deaths80.M), na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(`Deaths_60+` = ifelse(`Deaths_60+` == 1, 0, `Deaths_60+`)) %>% 
    # melt data frame 
    group_by(CountryName, Period) %>%
    pivot_longer(cols = `Deaths_25-49`:`Deaths_60+`,
                 names_to = "Age_group",
                 values_to = "Counts") %>% 
    ungroup() %>% 
    # Tidy up element names
    mutate(Age_group = str_replace(Age_group, "Deaths_", ""),
           Mortality_data = ifelse(Counts > 0, "Yes", "No")) %>%
    select(-Counts) %>% 
    mutate(Mortality_data = ifelse(CountryName %in% countries_partial_reporting & Period == "Full analysis",
                                   "No", Mortality_data))
      

  
  # Find out which countries have vaccination data by age groups ---------------
  vaccine_data <- vaccine %>%
    full_join(England_vaccinations) %>% 
    mutate(TargetGroup = str_replace(TargetGroup, "1_Age60+", "Age60")) %>%
    dplyr::select(ReportingCountry, DateUsedForStatisticsISO, Region, TargetGroup, Vaccine, DoseFirst, DoseSecond, DoseAdditional1) %>%
    mutate(ReportingCountry = ifelse(ReportingCountry == "UK" & Region == "UKM", "UKScotland", ReportingCountry),
           ReportingCountry = ifelse(ReportingCountry == "UK" & Region == "UKL", "UKWales", ReportingCountry),
           ReportingCountry = ifelse(ReportingCountry == "UK" & Region == "UK_ENG", "UKEngland", ReportingCountry)) %>%
    filter(TargetGroup %in% c("Age25_49", "Age50_59", "Age60+")) %>%
    mutate(TargetGroup = str_replace(TargetGroup, "_", "-")) %>%
    mutate(DateUsedForStatisticsISO = str_replace(DateUsedForStatisticsISO, "W", ""),
           Period = ifelse(DateUsedForStatisticsISO %in% omicron.weeks, "Full analysis", "Pre-Omicron")) 
    

  vaccine_data$report_country <- country_codes$report_country[match(vaccine_data$ReportingCountry, country_codes$ReportingCountry)] 

  
  vaccine_data <- vaccine_data %>%
    filter(!(report_country == "Republic of Moldova" & TargetGroup %in% c("Age25-49", "Age50-59"))) %>% 
    group_by(report_country, Period, TargetGroup) %>%
    summarise(DoseFirst = sum(DoseFirst, na.rm = TRUE),
              DoseSecond = sum(DoseSecond, na.rm = TRUE),
              DoseAdditional1 = sum(DoseAdditional1, na.rm = TRUE)) %>%
    ungroup() 
  
  vaccine_data <- vaccine_data %>%
    arrange(report_country, TargetGroup) %>%
    mutate(Age_group = str_replace(TargetGroup, "Age", ""),
           Vaccination_data = ifelse(DoseFirst > 0 & DoseSecond > 0, "Yes", "No")) %>%
    select(-c(TargetGroup, DoseFirst, DoseSecond, DoseAdditional1)) %>% 
    mutate(report_country = ifelse(report_country == "United Kingdom, England", "United Kingdom (England)", report_country),
           report_country = ifelse(report_country == "United Kingdom, Scotland", "United Kingdom (Scotland)", report_country),
           report_country = ifelse(report_country == "United Kingdom, Wales", "United Kingdom (Wales)", report_country),
           report_country = ifelse(report_country == "Kosovo (in accordance with Security Council resolution 1244 (1999))",
                                 "Kosovo", report_country)) %>%
    mutate(Vaccination_data = ifelse(report_country %in% countries_partial_reporting & Period == "Full analysis", "No", Vaccination_data)) %>% 
    rename(CountryName = report_country)
  

  # Find out which countries have circulating variant information --------------
  GISAID_countries <- variants_GISAID %>% 
    select(CountryName) %>% 
    distinct()
  GISAID_countries_full <- expand.grid(CountryName = GISAID_countries$CountryName,
                                       Period = c("Pre-Omicron", "Full analysis"),
                                       stringsAsFactors = FALSE)
  
  variant_countries <- variants %>% 
    mutate(CountryName = ifelse(str_detect(DataSource, "UK-WALES"), "United Kingdom (Wales)", CountryName),
           CountryName = ifelse(str_detect(DataSource, "UK-ENG"), "United Kingdom (England)", CountryName),
           CountryName = ifelse(str_detect(DataSource, "UK-SCOT"), "United Kingdom (Scotland)", CountryName),
           CountryName = ifelse(str_detect(DataSource, "UK-NI"), "United Kingdom (Northern Ireland)", CountryName)) %>%
    mutate(DateUsedForStatisticsWeek = str_replace(DateUsedForStatisticsWeek, "^'", ""),
           Period = ifelse(DateUsedForStatisticsWeek %in% omicron.weeks, "Full analysis", "Pre-Omicron")) %>% 
    filter(NumberSequencedViruses > 0, variant_out!= "UNK") %>%
    filter(duplicate_to_drop == FALSE, sample == "Any", complete_denom == TRUE) %>% 
    select(CountryName, Period) %>% 
    distinct() %>% 
    full_join(GISAID_countries_full) %>% 
    distinct() %>% 
    mutate(Variant_data = "Yes")
    
  
  
  # Join to create final summary table and tidy up -----------------------------
  data_summary_table <- mortality_data %>% 
    full_join(vaccine_data) %>% 
    full_join(variant_countries) %>% 
    distinct() %>%
    mutate(Mortality_data = ifelse(is.na(Mortality_data), "No", Mortality_data),
           Vaccination_data = ifelse(is.na(Vaccination_data), "No", Vaccination_data),
           Variant_data = ifelse(is.na(Variant_data), "No", Variant_data),
           Country_included = ifelse((Mortality_data == "Yes" & Vaccination_data == "Yes"),  "Yes", 
                                     ifelse((Mortality_data == "Yes" & Vaccination_data == "No"),  "No",
                                            ifelse((Mortality_data == "No" & Vaccination_data == "Yes"),  "No","No")))) %>%
    mutate(Country_included = ifelse(CountryName %in% countries_partial_reporting & Period == "Full analysis", "No", Country_included),
           Country_included = ifelse(CountryName %in% countries_no_reporting, "No", Country_included),
           Mortality_data = ifelse(CountryName == "Iceland", "Yes", Mortality_data),
           Country_included = ifelse(CountryName == "Iceland", "Yes", Country_included)) %>% 
    arrange(CountryName)
    
  
  # Find which countries have finer older age groups ---------------------------
    # Specifically looking for countries reporting 60-69, 70-79 and 80+
  
  # Mortality data
  mortality_data <- deaths %>%
    mutate(CountryName = ifelse(grepl("^UK-ENG_*", DataSource), "United Kingdom (England)", CountryName),
           CountryName = ifelse(grepl("^UK-NI_*", DataSource), "United Kingdom (Northern Ireland)", CountryName),
           CountryName = ifelse(grepl("^UK-SCOT_*", DataSource), "United Kingdom (Scotland)", CountryName),
           CountryName = ifelse(grepl("^UK-WALES_*", DataSource), "United Kingdom (Wales)", CountryName),
           CountryName = ifelse(CountryName == "Moldova", "Republic of Moldova", CountryName),
           CountryName = ifelse(CountryName == "Russia", "Russian Federation", CountryName)) %>%
    dplyr::select(CountryName, DateUsedForStatisticsISO | (starts_with("Deaths"))) %>%
    mutate(DateUsedForStatisticsISO = str_replace(DateUsedForStatisticsISO, "W", ""),
           Period = ifelse(DateUsedForStatisticsISO %in% omicron.weeks, "Full analysis", "Pre-Omicron")) %>% 
    group_by(CountryName, Period) %>%
    summarise(`Deaths_60-69` = sum(c(Deaths60.64F, Deaths60.64M, Deaths65.69F, Deaths65.69M), na.rm = TRUE),
              `Deaths_70-79` = sum(c(Deaths70.74F, Deaths70.74M, Deaths75.79F, Deaths75.79M, Deaths80.F, Deaths80.M), na.rm = TRUE),
              `Deaths_80+` = sum(c(Deaths80.F, Deaths80.M), na.rm = TRUE)) %>%
    ungroup() %>%
    # melt data frame 
    group_by(CountryName, Period) %>%
    pivot_longer(cols = `Deaths_60-69`:`Deaths_80+`,
                 names_to = "Age_group",
                 values_to = "Counts") %>% 
    ungroup() %>% 
    # Tidy up element names
    mutate(Age_group = str_replace(Age_group, "Deaths_", ""),
           Mortality_data = ifelse(Counts > 0, "Yes", "No")) %>%
    select(-Counts) %>% 
    mutate(Mortality_data = ifelse(CountryName %in% countries_partial_reporting & Period == "Full analysis",
                                   "No", Mortality_data))
  
  # Vaccination data
  vaccine_data <- vaccine %>%
    full_join(England_vaccinations) %>% 
    mutate(TargetGroup = str_replace(TargetGroup, "1_Age60+", "Age60")) %>%
    dplyr::select(ReportingCountry, DateUsedForStatisticsISO, Region, TargetGroup, Vaccine, DoseFirst, DoseSecond, DoseAdditional1) %>%
    mutate(ReportingCountry = ifelse(ReportingCountry == "UK" & Region == "UKM", "UKScotland", ReportingCountry),
           ReportingCountry = ifelse(ReportingCountry == "UK" & Region == "UKL", "UKWales", ReportingCountry),
           ReportingCountry = ifelse(ReportingCountry == "UK" & Region == "UK_ENG", "UKEngland", ReportingCountry)) %>%
    # mutate(DoseSecond = ifelse(Vaccine == "JANSS", DoseFirst, DoseSecond)) %>%
    filter(TargetGroup %in% c("Age60_69", "Age70_79", "Age80+")) %>%
    mutate(TargetGroup = str_replace(TargetGroup, "_", "-"),
           DateUsedForStatisticsISO = str_replace(DateUsedForStatisticsISO, "W", ""),
           Period = ifelse(DateUsedForStatisticsISO %in% omicron.weeks, "Full analysis", "Pre-Omicron")) 
  
  # Assign country names
  vaccine_data$report_country <- country_codes$report_country[match(vaccine_data$ReportingCountry, country_codes$ReportingCountry)] 
  
  # Tidy up
  vaccine_data <- vaccine_data %>%
    group_by(report_country, Period, TargetGroup) %>%
    summarise(DoseFirst = sum(DoseFirst, na.rm = TRUE),
              DoseSecond = sum(DoseSecond, na.rm = TRUE),
              DoseAdditional1 = sum(DoseAdditional1, na.rm = TRUE)) %>%
    ungroup() 
  
  vaccine_data <- vaccine_data %>%
    arrange(report_country, TargetGroup) %>%
    mutate(Age_group = str_replace(TargetGroup, "Age", ""),
           Vaccination_data = ifelse(DoseFirst > 0 & DoseSecond > 0, "Yes", "No")) %>%
    select(-c(TargetGroup, DoseFirst, DoseSecond, DoseAdditional1)) %>% 
    mutate(report_country = ifelse(report_country == "United Kingdom, England", "United Kingdom (England)", report_country),
           report_country = ifelse(report_country == "United Kingdom, Scotland", "United Kingdom (Scotland)", report_country),
           report_country = ifelse(report_country == "United Kingdom, Wales", "United Kingdom (Wales)", report_country),
           report_country = ifelse(report_country == "Kosovo (in accordance with Security Council resolution 1244 (1999))",
                                   "Kosovo", report_country)) %>%
    mutate(Vaccination_data = ifelse(report_country %in% countries_partial_reporting & Period == "Full analysis", "No", Vaccination_data)) %>% 
    rename(CountryName = report_country)
  
  # Create summary table
  data_summary_table_short <- mortality_data %>% 
    full_join(vaccine_data) %>% 
    distinct() %>% 
    mutate(Mortality_data = ifelse(Mortality_data == "Yes", 1, 0),
           Vaccination_data = ifelse(Vaccination_data == "Yes", 1, 0)) %>% 
    group_by(CountryName, Period) %>% 
    summarise(Mortality_data = sum(Mortality_data, na.rm = TRUE),
              Vaccination_data = sum(Vaccination_data, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(Mortality_data = ifelse(Mortality_data == 3, "Yes", "No"),
           Vaccination_data = ifelse(Vaccination_data == 3, "Yes", "No")) %>% 
    add_column(Age_group = "Finer older", .after = "Period") %>% 
    mutate(Country_included = ifelse((Mortality_data == "Yes" & Vaccination_data == "Yes"),  "Yes", 
                                     ifelse((Mortality_data == "Yes" & Vaccination_data == "No"),  "No",
                                            ifelse((Mortality_data == "No" & Vaccination_data == "Yes"),  "No","No"))),
           Mortality_data = ifelse(CountryName == "Iceland", "Yes", Mortality_data),
           Country_included = ifelse(CountryName == "Iceland", "Yes", Country_included),
           Mortality_data = ifelse(CountryName == "Israel", "No", Mortality_data),
           Vaccination_data = ifelse(CountryName == "Israel", "No", Vaccination_data),
           Country_included = ifelse(CountryName == "Israel", "No", Country_included))
  
  
  # Combine with previous summary table
  data_summary_table_final <- full_join(data_summary_table, data_summary_table_short) %>% 
    arrange(CountryName, Period, Age_group) %>% 
    fill(Variant_data, .direction = "down")
  
  
  return(data_summary_table_final)
}