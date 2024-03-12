# Script to clean vaccination data
# Margaux Mesle - meslem@who.int
# July 2021


clean.vaccination.data.finer.ages <- function(vaccine, 
                                              country_codes, 
                                              population_fine,
                                              England_vaccinations,
                                              Israel_vaccination,
                                              Scotland_vaccination,
                                              Ukraine_vaccinations,
                                              Netherlands_vaccination) {
  
  # Tidy and prepare some country data -----------------------------------------
  Israel_vaccination_clean <- Israel_vaccination %>% 
    mutate(report_country = "Israel",
           age_group = paste0("Age ", age_group),
           Manufacturer_Desc = ifelse(Manufacturer_Desc == "Pfizer", "COM",
                                      ifelse(Manufacturer_Desc == "Moderna", "MOD",
                                             ifelse(Manufacturer_Desc == "AstraZeneca", "AZ", "NVXD"))),
           year_week = str_replace(DateUsedForStatistics, "-W", "-")) %>% 
    filter(!age_group == "Age Unknown") %>% 
    rename(DoseFirst = first_dose, 
           DoseSecond = second_dose,
           DoseAdditional1 = third_dose,
           DoseAdditional2 = fourth_dose,
           TargetGroup = age_group,
           Vaccine = Manufacturer_Desc) %>% 
    select(- DateUsedForStatistics)
  colnames(Israel_vaccination_clean) <- tolower(colnames(Israel_vaccination_clean))
  
  England_vaccinations_clean <- England_vaccinations %>% 
    filter(TargetGroup %in% c("Age25_49", "Age50_59", "Age60_69", "Age70_79", "Age80+")) %>% 
    rename(nDoseFirst = DoseFirst,
           nDoseSecond = DoseSecond,
           nDoseAdditional1 = DoseAdditional1,
           nDoseAdditional2 = DoseAdditional2,
           nDoseAdditional3 = DoseAdditional3) %>% 
    arrange(TargetGroup, DateUsedForStatisticsISO) %>% 
    group_by(TargetGroup) %>% 
    mutate(dosefirst = nDoseFirst - lag(nDoseFirst, default = 0),
           dosesecond = nDoseSecond - lag(nDoseSecond, default = 0),
           doseadditional1 = nDoseAdditional1 - lag(nDoseAdditional1, default = 0),
           doseadditional2 = nDoseAdditional2 - lag(nDoseAdditional2, default = 0),
           doseadditional3 = nDoseAdditional3 - lag(nDoseAdditional3, default = 0)) %>% 
    ungroup() %>% 
    mutate(ReportCountry = "United Kingdom (England)",
           TargetGroup = str_replace(TargetGroup, "Age", "Age "),
           TargetGroup = str_replace(TargetGroup, "_", "-"),
           year_week = str_replace(DateUsedForStatisticsISO, "W", ""),
           Year = as.numeric(substr(year_week, 1, 4))) %>% 
    select(ReportCountry, year_week, TargetGroup, dosefirst, dosesecond, doseadditional1, doseadditional2, doseadditional3, Vaccine, Year) %>% 
    rename(pretty_vaccine = Vaccine)
  
  Scotland_vaccination_clean <- Scotland_vaccination %>% 
    filter(TargetGroup %in% c("Age25_49", "Age50_59", "Age60_69", "Age70_79", "Age80+")) %>% 
    rename(DateUsedForStatisticsISO = DateUsedForStatistics) %>% 
    select(DateUsedForStatisticsISO, Region, ReportingCountry, TargetGroup, 
           DoseFirst, DoseSecond, DoseAdditional1, DoseAdditional2, DoseAdditional3,
           Vaccine) %>% 
    mutate(TargetGroup = ifelse(TargetGroup %in% c("Age60_69", "Age70_79", "Age80+"), "Age60+", TargetGroup)) %>% 
    group_by(DateUsedForStatisticsISO, Region, ReportingCountry, TargetGroup, Vaccine) %>% 
    summarise(DoseFirst = sum(DoseFirst),
              DoseSecond = sum(DoseSecond),
              DoseAdditional1 = sum(DoseAdditional1),
              DoseAdditional2 = sum(DoseAdditional2),
              DoseAdditional3 = sum(DoseAdditional3)) %>% 
    ungroup()
  
  Ukraine_vaccinations_clean <- Ukraine_vaccinations %>% 
    rename_all(., .funs = tolower) %>% 
    rename(year_week = dateusedforstatisticsiso,
           region = reportingcountry) %>% 
    add_column(report_country = "Ukraine", .before = "region") %>% 
    add_column(doseadditional2 = NA, .before = "targetgroup") %>% 
    add_column(doseadditional3 = NA, .before = "targetgroup") %>% 
    mutate(targetgroup = str_replace(targetgroup, "1_", ""),
           targetgroup = str_replace(targetgroup, "Age", "Age ")) %>% 
    mutate(dosefirst = as.numeric(dosefirst),
           dosesecond = as.numeric(dosesecond),
           doseadditional1 = as.numeric(doseadditional1),
           doseadditional2 = as.numeric(doseadditional2),
           doseadditional3 = as.numeric(doseadditional3))
  
  
  # Data cleaning --------------------------------------------------------------
  selected_data <- vaccine %>% 
    filter(!Region %in% c("IL", "UK_ENG", "UKM", "NL")) %>% 
    full_join(Netherlands_vaccination) %>% 
    select(DateUsedForStatisticsISO, Region, ReportingCountry, TargetGroup, 
           DoseFirst, DoseSecond, DoseAdditional1, DoseAdditional2, DoseAdditional3, Vaccine) %>% 
    full_join(Scotland_vaccination_clean)
  
  
  # code was developed from data provided with lower case column names - more recently data provided in camelcase
  colnames(selected_data) <- tolower(colnames(selected_data))
  
  # keep only national level data
  selected_data <- selected_data %>%
    mutate(reportingcountry = ifelse(reportingcountry == "UK", region, reportingcountry)) %>% 
    filter(reportingcountry == region) %>% 
    mutate(reportingcountry = ifelse(region == "UK_ENG", "UKEngland",
                                     ifelse(region == "UKM", "UKScotland",
                                            ifelse(region == "UKN", "UKNI",
                                                   ifelse(region == "UKL", "UKWales", reportingcountry))))) 
  
  # Add country names 
  selected_data <- left_join(selected_data, select(country_codes, ReportingCountry, report_country), 
                             by = c("reportingcountry" = "ReportingCountry"))
  
  selected_data <- selected_data %>% 
    full_join(Israel_vaccination_clean) %>% 
    full_join(Ukraine_vaccinations_clean)
  
  # recode age category
  selected_data <- selected_data %>%
    filter(targetgroup %in% c("Age25_49", "Age50_59", "Age60_69", "Age70_79", "Age80+")) %>% 
    mutate(pretty_targetgroup = str_replace(targetgroup, "1_", ""),
           pretty_targetgroup = str_replace(pretty_targetgroup, "_", "-"),
           pretty_targetgroup = str_replace(pretty_targetgroup, "Age", "Age ")) %>% 
    mutate(targetgroup = pretty_targetgroup) %>% 
    select(-pretty_targetgroup) %>% 
    mutate(year_week = str_replace(dateusedforstatisticsiso, "-W", "-"))
  
  
  # recode vaccine brand
  selected_data <- selected_data %>%
    select(report_country, year_week, targetgroup, dosefirst, dosesecond, doseadditional1, doseadditional2, doseadditional3, vaccine) %>% 
    full_join(Israel_vaccination_clean) %>% 
    mutate(pretty_vaccine = recode(vaccine, 
                                   COM = "Pfizer BioNTech - Comirnaty",
                                   MOD = "Moderna - mRNA-1273", 
                                   UNK="Unknown product", 
                                   AZ = "AstraZeneca - Vaxzevria", 
                                   CN = "BBIBV-CorV - CNBG",
                                   WUCNBG = "Wuhan CNBG - Inactivated", 
                                   SIICOV = "SII - Covishield", # AstraZeneca
                                   BECNBG = "Beijing CNBG - BBIBP-CorV", 
                                   JANSS = "Janssen - Ad26.COV 2-S", 
                                   QAZ = "RIBSP - QazVac",
                                   SIN = "Sinovac - CoronaVac", 
                                   SPU = "Gamaleya - Gam-Covid-Vac", 
                                   SRCVB = "SRCVB - EpiVacCorona",
                                   ZFUZ = "Sino-Uzbek", 
                                   HAYATVAX = "Julphar - Hayat-Vax", 
                                   QAZVAQ = "RIBSP - QazVac", 
                                   BHACOV = "Covaxin - Bharat",
                                   NVXD = "Novavax - Nuvaxovid", 
                                   SPUL = "Sputnik LigComirnaty – Pfizer/BioNTech Original/Omicron BA.4-5ht - Gamaleya", 
                                   VLA  = "Valneva",  #COVID-19 Vaccine (inactivated, adjuvanted) Valneva – Valneva
                                   COMBA.1 = "Comirnaty – Pfizer/BioNTech Original/Omicron BA.1", # Comirnaty – Pfizer/BioNTech Original/Omicron BA.1
                                   "COMBA.4-5" = "Comirnaty – Pfizer/BioNTech Original/Omicron BA.4-5", # Comirnaty – Pfizer/BioNTech Original/Omicron BA.4-5
                                   "MODBA.1" = "Moderna bivalent Original",  # Spikevax Moderna bivalent Original/Omicron BA.1
                                   "MODBA.4-5" = "Moderna bivalent Original",
                                   SGSK = "Sanofi–GSK",
                                   COMBIV = "Comirnaty – Pfizer/BioNTech Original/Omicron BA.4-5",
                                   MODBIV = "Moderna Spikevax")) %>% 
    #rename(vaccine = pretty_vaccine) %>% 
    select(-vaccine) %>% 
    rename(ReportCountry = report_country,
           TargetGroup = targetgroup) %>% 
    mutate(ReportCountry = ifelse(ReportCountry == "United Kingdom, England", "United Kingdom (England)",
                                  ifelse(ReportCountry == "United Kingdom, Northern Ireland", "United Kingdom (Northern Ireland)",
                                         ifelse(ReportCountry == "United Kingdom, Scotland", "United Kingdom (Scotland)",
                                                ifelse(ReportCountry == "United Kingdom, Wales", "United Kingdom (Wales)", ReportCountry))))) %>% 
    mutate(Year = as.numeric(substr(year_week, 1, 4)))
  
  
  selected_data <- selected_data %>% 
    full_join(England_vaccinations_clean)
  
  # Find denominators for each country and age group ---------------------------
  
  # EU/EEA countries
  vaccine_denominators <- population_fine %>% 
    mutate(TargetGroup = str_replace(TargetGroup, "_", "-"),
           TargetGroup = str_replace(TargetGroup, "Age", "Age ")) %>% 
    rename(ReportCountry = CountryName)
  
  # Join vaccination data to denominators --------------------------------------
  vaccine_data_clean <- selected_data %>% 
    full_join(vaccine_denominators, 
              by = c("ReportCountry" = "ReportCountry", "Year" = "Year", "TargetGroup" = "TargetGroup"),
              relationship = "many-to-many") %>% 
    filter(!is.na(year_week)) %>% 
    group_by(ReportCountry, TargetGroup) %>% 
    arrange(ReportCountry, TargetGroup) %>% 
    fill(Denominator, .direction = "downup") %>% 
    ungroup() %>% 
    filter(!ReportCountry %in% c("Republic of Moldova", "Germany", "Ukraine"))
  
    #filter(!(ReportCountry == "Republic of Moldova" & TargetGroup %in% c("Age 25-49", "Age 50-59")))
  
  # Tidy up Albania week 2022/47 -----------------------------------------------
  vaccine_data_clean <- vaccine_data_clean %>% 
    mutate(doseadditional1 = ifelse(ReportCountry == "Albania" & year_week == "2022-47" & doseadditional3 > 0,
                                    doseadditional2, doseadditional1),
           doseadditional2 = ifelse(ReportCountry == "Albania" & year_week == "2022-47" & doseadditional3 > 0,
                                    doseadditional3, doseadditional2),
           doseadditional3 = ifelse(ReportCountry == "Albania" & year_week == "2022-47" & doseadditional3 > 0,
                                    0, doseadditional3) )
  
  
  # Calculate percentage coverage ----------------------------------------------
  vaccine_data_clean <- vaccine_data_clean %>% 
    mutate(FirstDose = ifelse(is.na(dosefirst), 0, dosefirst),
           SecondDose = ifelse(is.na(dosesecond), 0, dosesecond),
           DoseAdditional1 = ifelse(is.na(doseadditional1), 0, doseadditional1),
           DoseAdditional2 = ifelse(is.na(doseadditional2), 0, doseadditional2),
           DoseAdditional3 = ifelse(is.na(doseadditional3), 0, doseadditional3)) %>%
    group_by(ReportCountry, TargetGroup, year_week, Denominator) %>%
    summarise(FirstDose = sum(FirstDose),
              SecondDose = sum(SecondDose),
              DoseAdditional1 = sum(DoseAdditional1),
              DoseAdditional2 = sum(DoseAdditional2),
              DoseAdditional3 = sum(DoseAdditional3)) %>%
    ungroup() %>%
    arrange(ReportCountry, TargetGroup, year_week) %>% 
    group_by(ReportCountry, TargetGroup) %>%
    mutate(nFirstDose = cumsum(FirstDose),
           nSecondDose = cumsum(SecondDose),
           nDoseAdditional1 = cumsum(DoseAdditional1),
           nDoseAdditional2 = cumsum(DoseAdditional2),
           nDoseAdditional3 = cumsum(DoseAdditional3),
           pcFirstDose = round((cumsum(FirstDose)/Denominator)*100),
           pcSecondDose = round((cumsum(SecondDose)/Denominator)*100),
           pcDoseAdditional1 = round((cumsum(DoseAdditional1)/Denominator)*100),
           pcDoseAdditional2 = round((cumsum(DoseAdditional2)/Denominator)*100),
           pcDoseAdditional3 = round((cumsum(DoseAdditional3)/Denominator)*100)) %>%
    ungroup() %>%
    group_by(ReportCountry, TargetGroup, year_week) %>%
    mutate(pcFirstDose = ifelse(pcFirstDose>100, 100, pcFirstDose),
           pcSecondDose = ifelse(pcSecondDose>100, 100, pcSecondDose),
           pcDoseAdditional1 = ifelse(pcDoseAdditional1 > 100, 100, pcDoseAdditional1),
           pcDoseAdditional2 = ifelse(pcDoseAdditional2 > 100, 100, pcDoseAdditional2),
           pcDoseAdditional3 = ifelse(pcDoseAdditional3 > 100, 100, pcDoseAdditional3)) %>%
    ungroup() %>% 
    filter(year_week %in% reporting.weeks)
  
  
  # Making sure all countries have data have doses 4 and 5 ---------------------
  
  # Find mean percentage change of vaccination coverage in countries without missing dose 4 and/or 5 data
  mean_pc_change_values <- vaccine_data_clean %>% 
    group_by(ReportCountry, TargetGroup) %>% 
    # Remove countries not included in full analysis
    filter(!(ReportCountry %in% countries_no_reporting | ReportCountry %in% countries_partial_reporting | is.na(Denominator))) %>% 
    filter(row_number() == n()) %>% 
    ungroup() %>% 
    filter(!nDoseAdditional3 == 0) %>% 
    group_by(TargetGroup) %>% 
    mutate(pcChange3To4 = ((pcDoseAdditional1 - pcDoseAdditional2)/ pcDoseAdditional1)*100,
           pcChange4To5 = ((pcDoseAdditional2 - pcDoseAdditional3)/ pcDoseAdditional2)*100) %>% 
    summarise(meanpcChange3To4 = mean(pcChange3To4),
              meanpcChange4To5 = mean(pcChange4To5),
              ValueChange3To4 = (100 - meanpcChange3To4) / 100,
              ValueChange4To5 = (100 - meanpcChange4To5) / 100) %>% 
    ungroup() 
  
  # Find mean time difference of vaccination coverage in countries without missing dose 4 and/or 5 data
  x <- vaccine_data_clean %>%
    filter(ReportCountry %in% countries_included$CountryName) %>% 
    dplyr::select(ReportCountry, TargetGroup, year_week, DoseAdditional1) %>%
    filter(DoseAdditional1 > 0) %>%
    arrange(ReportCountry, year_week) %>%
    group_by(ReportCountry, TargetGroup) %>%
    slice(1) %>% 
    mutate(Dose = "booster 1") %>% 
    select(-DoseAdditional1)
  
  y <- vaccine_data_clean %>%
    filter(ReportCountry %in% countries_included$CountryName) %>% 
    dplyr::select(ReportCountry, TargetGroup, year_week, DoseAdditional2) %>%
    filter(DoseAdditional2 > 0) %>%
    arrange(ReportCountry, year_week) %>%
    group_by(ReportCountry, TargetGroup) %>%
    slice(1) %>% 
    mutate(Dose = "booster 2")%>% 
    select(-DoseAdditional2)
  
  z <- vaccine_data_clean %>%
    filter(ReportCountry %in% countries_included$CountryName) %>% 
    dplyr::select(ReportCountry, TargetGroup, year_week, DoseAdditional3) %>%
    filter(DoseAdditional3 > 0) %>%
    arrange(ReportCountry, year_week) %>%
    group_by(ReportCountry, TargetGroup) %>%
    slice(1) %>% 
    mutate(Dose = "booster 3")%>% 
    select(-DoseAdditional3)
  
  # Convert year_weeks into numerical values from 1 to n - to count week differences
  year_months_template1 <- year_months_template %>% 
    select(-yearmonth) %>% 
    distinct() %>% 
    rowid_to_column()
  
  mean_week_difference <- x %>% full_join(y) %>% full_join(z) %>% 
    arrange(ReportCountry, TargetGroup) %>% 
    full_join(year_months_template1) %>% 
    distinct() %>% 
    mutate(weeks = as.numeric(substr(year_week, 6, 7))) %>% 
    group_by(ReportCountry, TargetGroup) %>%
    mutate(diff_weeks = rowid - lag(rowid, default = rowid[1])) %>% 
    ungroup() %>% 
    filter(!is.na(ReportCountry)) %>% 
    group_by(TargetGroup, Dose) %>% 
    summarise(MeanWeeks = round(mean(diff_weeks))) %>% 
    ungroup() %>% 
    filter(!Dose == "booster 1")  
  
  # Find which countries are missing data for doses 4 and 5
  countries_missing_boosters <- vaccine_data_clean %>% 
    group_by(ReportCountry, TargetGroup) %>% 
    filter(year_week == "2023-12") %>% 
    filter(!is.na(Denominator)) %>% 
    filter(row_number() == n()) %>% 
    filter(nDoseAdditional3 == 0) %>% 
    ungroup() 
  
  # For clarity of coding, pull values calculated above
  # Reduction of doses
  change_booster2_age25 <- mean_pc_change_values %>% filter(TargetGroup == "Age 25-49") %>% pull(ValueChange3To4)
  change_booster3_age25 <- mean_pc_change_values %>% filter(TargetGroup == "Age 25-49") %>% pull(ValueChange4To5)
  
  change_booster2_age50 <- mean_pc_change_values %>% filter(TargetGroup == "Age 50-59") %>% pull(ValueChange3To4)
  change_booster3_age50 <- mean_pc_change_values %>% filter(TargetGroup == "Age 50-59") %>% pull(ValueChange4To5)
  
  change_booster2_age60 <- mean_pc_change_values %>% filter(TargetGroup == "Age 60-69") %>% pull(ValueChange3To4)
  change_booster3_age60 <- mean_pc_change_values %>% filter(TargetGroup == "Age 60-69") %>% pull(ValueChange4To5)
  
  change_booster2_age70 <- mean_pc_change_values %>% filter(TargetGroup == "Age 70-79") %>% pull(ValueChange3To4)
  change_booster3_age70 <- mean_pc_change_values %>% filter(TargetGroup == "Age 70-79") %>% pull(ValueChange4To5)
  
  change_booster2_age80 <- mean_pc_change_values %>% filter(TargetGroup == "Age 80+") %>% pull(ValueChange3To4)
  change_booster3_age80 <- mean_pc_change_values %>% filter(TargetGroup == "Age 80+") %>% pull(ValueChange4To5)
  
  
  # Lag time for vaccine doses
  lag_booster2_age25 <- mean_week_difference %>% filter(TargetGroup == "Age 25-49" & Dose == "booster 2") %>% pull(MeanWeeks)
  lag_booster3_age25 <- mean_week_difference %>% filter(TargetGroup == "Age 25-49" & Dose == "booster 3") %>% pull(MeanWeeks)
  
  lag_booster2_age50 <- mean_week_difference %>% filter(TargetGroup == "Age 50-59" & Dose == "booster 2") %>% pull(MeanWeeks)
  lag_booster3_age50 <- mean_week_difference %>% filter(TargetGroup == "Age 50-59" & Dose == "booster 3") %>% pull(MeanWeeks)
  
  lag_booster2_age60 <- mean_week_difference %>% filter(TargetGroup == "Age 60-69" & Dose == "booster 2") %>% pull(MeanWeeks)
  lag_booster3_age60 <- mean_week_difference %>% filter(TargetGroup == "Age 60-69" & Dose == "booster 3") %>% pull(MeanWeeks)
  
  lag_booster2_age70 <- mean_week_difference %>% filter(TargetGroup == "Age 70-79" & Dose == "booster 2") %>% pull(MeanWeeks)
  lag_booster3_age70 <- mean_week_difference %>% filter(TargetGroup == "Age 70-79" & Dose == "booster 3") %>% pull(MeanWeeks)
  
  lag_booster2_age80 <- mean_week_difference %>% filter(TargetGroup == "Age 80+" & Dose == "booster 2") %>% pull(MeanWeeks)
  lag_booster3_age80 <- mean_week_difference %>% filter(TargetGroup == "Age 80+" & Dose == "booster 3") %>% pull(MeanWeeks)
  
  # Run calculations -----------------------------------------------------------
  
  # Booster 2 
  
  # England
  ENG_booster_2 <- vaccine_data_clean %>% 
    filter(ReportCountry == "United Kingdom (England)"  & TargetGroup == "Age 25-49") %>% 
    group_by(TargetGroup) %>% 
    #filter(DoseAdditional1 > 0) %>%
    ungroup() %>% 
    slice(lag_booster2_age25  : n()) %>% 
    #slice(1 : lag_booster2_age25) %>% 
    mutate(DoseAdditional2 = round(DoseAdditional1 * change_booster2_age25))
  
  # Switzerland
  CH_25_B2 <- vaccine_data_clean %>% 
    filter(ReportCountry == "Switzerland" & TargetGroup == "Age 25-49") %>% 
    group_by(TargetGroup) %>% 
    filter(DoseAdditional1 > 10) %>%
    ungroup() %>% 
    slice(lag_booster2_age25 : n()) %>% 
    #slice(1 : (lag_booster2_age25 - 1)) %>% 
    mutate(DoseAdditional2 = round(DoseAdditional1 * change_booster2_age25))
  
  CH_50_B2 <- vaccine_data_clean %>% 
    filter(ReportCountry == "Switzerland"  & TargetGroup == "Age 50-59") %>% 
    group_by(TargetGroup) %>% 
    filter(DoseAdditional1 > 10) %>%
    ungroup() %>% 
    slice(lag_booster2_age50 : n()) %>% 
    #slice(1 : lag_booster2_age50) %>% 
    mutate(DoseAdditional2 = round(DoseAdditional1 * change_booster2_age50))
  
  CH_60_B2 <- vaccine_data_clean %>% 
    filter(ReportCountry == "Switzerland"  & TargetGroup == "Age 60-69") %>% 
    group_by(TargetGroup) %>% 
    filter(DoseAdditional1 > 10) %>%
    ungroup() %>% 
    slice(lag_booster2_age60 : n()) %>% 
    mutate(DoseAdditional2 = round(DoseAdditional1 * change_booster2_age60))
  
  CH_70_B2 <- vaccine_data_clean %>% 
    filter(ReportCountry == "Switzerland"  & TargetGroup == "Age 70-79") %>% 
    group_by(TargetGroup) %>% 
    filter(DoseAdditional1 > 10) %>%
    ungroup() %>% 
    slice(lag_booster2_age70 : n()) %>% 
    mutate(DoseAdditional2 = round(DoseAdditional1 * change_booster2_age70))
  
  CH_80_B2 <- vaccine_data_clean %>% 
    filter(ReportCountry == "Switzerland"  & TargetGroup == "Age 80+") %>% 
    group_by(TargetGroup) %>% 
    filter(DoseAdditional1 > 10) %>%
    ungroup() %>% 
    slice(lag_booster2_age80 : n()) %>% 
    mutate(DoseAdditional2 = round(DoseAdditional1 * change_booster2_age80))
  
  CH_booster_2 <- CH_25_B2 %>% full_join(CH_50_B2) %>% full_join(CH_60_B2) %>% full_join(CH_70_B2) %>% full_join(CH_80_B2) 
  

  booster_2_data <- CH_booster_2 %>% full_join(ENG_booster_2) %>% mutate(id = paste0(ReportCountry, "-", TargetGroup, "_", year_week))
  
  
  temp <- vaccine_data_clean %>% 
    mutate(id = paste0(ReportCountry, "-", TargetGroup, "_", year_week)) %>% 
    filter(!id %in% booster_2_data$id) %>% 
    full_join(booster_2_data) %>% 
    select(-id)
  
  # Booster 3 
  
  # England
  ENG_25_B3 <- temp %>% 
    filter(ReportCountry == "United Kingdom (England)" & TargetGroup == "Age 25-49") %>% 
    group_by(TargetGroup) %>% 
    filter(DoseAdditional2 > 0) %>%
    ungroup() %>% 
    slice(lag_booster3_age25 : n()) %>% 
    mutate(DoseAdditional3 = round(DoseAdditional2 * change_booster3_age25))
  
  ENG_50_B3 <- temp %>% 
    filter(ReportCountry == "United Kingdom (England)"  & TargetGroup == "Age 50-59") %>% 
    group_by(TargetGroup) %>% 
    filter(DoseAdditional2 > 0) %>%
    ungroup() %>% 
    slice(lag_booster3_age50 : n()) %>% 
    mutate(DoseAdditional3 = round(DoseAdditional2 * change_booster3_age50))
  
  ENG_booster_3 <- ENG_25_B3 %>% full_join(ENG_50_B3) 
  
  # Croatia
  HR_25_B3 <- temp %>% 
    filter(ReportCountry == "Croatia" & TargetGroup == "Age 25-49") %>% 
    group_by(TargetGroup) %>% 
    filter(DoseAdditional2 > 0) %>%
    ungroup() %>%
    slice(lag_booster3_age25 : n()) %>% 
    mutate(DoseAdditional3 = round(DoseAdditional2 * change_booster3_age25))
  
  HR_50_B3 <- temp %>% 
    filter(ReportCountry == "Croatia"  & TargetGroup == "Age 50-59") %>% 
    group_by(TargetGroup) %>% 
    filter(DoseAdditional2 > 0) %>%
    ungroup() %>% 
    slice(lag_booster3_age50 : n()) %>% 
    mutate(DoseAdditional3 = round(DoseAdditional2 * change_booster3_age50))
  
  HR_60_B3 <- temp %>% 
    filter(ReportCountry == "Croatia"  & TargetGroup == "Age 60-69") %>% 
    group_by(TargetGroup) %>% 
    filter(DoseAdditional2 > 0) %>%
    ungroup() %>% 
    slice(lag_booster3_age60 : n()) %>% 
    mutate(DoseAdditional3 = round(DoseAdditional2 * change_booster3_age60))
  
  HR_70_B3 <- temp %>% 
    filter(ReportCountry == "Croatia"  & TargetGroup == "Age 70-79") %>% 
    group_by(TargetGroup) %>% 
    filter(DoseAdditional2 > 0) %>%
    ungroup() %>% 
    slice(lag_booster3_age70 : n()) %>% 
    mutate(DoseAdditional3 = round(DoseAdditional2 * change_booster3_age70))
  
  HR_80_B3 <- temp %>% 
    filter(ReportCountry == "Croatia"  & TargetGroup == "Age 80+") %>% 
    group_by(TargetGroup) %>% 
    filter(DoseAdditional2 > 0) %>%
    ungroup() %>% 
    slice(lag_booster3_age80 : n()) %>% 
    mutate(DoseAdditional3 = round(DoseAdditional2 * change_booster3_age80))
  
  HR_booster_3 <- HR_25_B3 %>% full_join(HR_50_B3) %>% full_join(HR_60_B3) %>% full_join(HR_70_B3) %>% full_join(HR_80_B3) 
  

  # Israel - not reporting by smaller age groups
  IL_25_B3 <- temp %>% 
    filter(ReportCountry == "Israel" & TargetGroup == "Age 25-49") %>% 
    group_by(TargetGroup) %>% 
    filter(DoseAdditional2 > 0) %>%
    ungroup() %>% 
    slice(lag_booster3_age25 : n()) %>% 
    #slice(1 : lag_booster3_age25) %>% 
    mutate(DoseAdditional3 = round(DoseAdditional2 * change_booster3_age25))
  
  IL_50_B3 <- temp %>% 
    filter(ReportCountry == "Israel"  & TargetGroup == "Age 50-59") %>% 
    group_by(TargetGroup) %>% 
    filter(DoseAdditional2 > 0) %>%
    ungroup() %>% 
    slice(lag_booster3_age50 : n()) %>% 
    #slice(1 : lag_booster3_age50) %>% 
    mutate(DoseAdditional3 = round(DoseAdditional2 * change_booster3_age50))
  
  IL_booster_3 <- IL_25_B3 %>% full_join(IL_50_B3)
  
  # Latvia
  LV_25_B3 <- temp %>% 
    filter(ReportCountry == "Latvia" & TargetGroup == "Age 25-49") %>% 
    group_by(TargetGroup) %>% 
    filter(DoseAdditional2 > 0) %>%
    ungroup() %>% 
    slice(lag_booster3_age25 : n()) %>% 
    mutate(DoseAdditional3 = round(DoseAdditional2 * change_booster3_age25))
  
  LV_50_B3 <- temp %>% 
    filter(ReportCountry == "Latvia"  & TargetGroup == "Age 50-59") %>% 
    group_by(TargetGroup) %>% 
    filter(DoseAdditional2 > 0) %>%
    ungroup() %>% 
    slice(lag_booster3_age50 : n()) %>% 
    mutate(DoseAdditional3 = round(DoseAdditional2 * change_booster3_age50))
  
  LV_60_B3 <- temp %>% 
    filter(ReportCountry == "Latvia"  & TargetGroup == "Age 60-69") %>% 
    group_by(TargetGroup) %>% 
    filter(DoseAdditional2 > 0) %>%
    ungroup() %>% 
    slice(lag_booster3_age60 : n()) %>% 
    mutate(DoseAdditional3 = round(DoseAdditional2 * change_booster3_age60))
  
  LV_70_B3 <- temp %>% 
    filter(ReportCountry == "Latvia"  & TargetGroup == "Age 70-79") %>% 
    group_by(TargetGroup) %>% 
    filter(DoseAdditional2 > 0) %>%
    ungroup() %>% 
    slice(lag_booster3_age70 : n()) %>% 
    mutate(DoseAdditional3 = round(DoseAdditional2 * change_booster3_age70))
  
  LV_80_B3 <- temp %>% 
    filter(ReportCountry == "Latvia"  & TargetGroup == "Age 80+") %>% 
    group_by(TargetGroup) %>% 
    filter(DoseAdditional2 > 0) %>%
    ungroup() %>% 
    slice(lag_booster3_age80 : n()) %>% 
    mutate(DoseAdditional3 = round(DoseAdditional2 * change_booster3_age80))
  
  LV_booster_3 <- LV_25_B3 %>% full_join(LV_50_B3) %>% full_join(LV_60_B3) %>% full_join(LV_70_B3) %>% full_join(LV_80_B3) 
  
  # Lithuania
  LT_25_B3 <- temp %>% 
    filter(ReportCountry == "Lithuania" & TargetGroup == "Age 25-49") %>% 
    group_by(TargetGroup) %>% 
    filter(DoseAdditional2 > 0) %>%
    ungroup() %>% 
    slice(lag_booster3_age25 : n()) %>% 
    mutate(DoseAdditional3 = round(DoseAdditional2 * change_booster3_age25))
  
  LT_50_B3 <- temp %>% 
    filter(ReportCountry == "Lithuania"  & TargetGroup == "Age 50-59") %>% 
    group_by(TargetGroup) %>% 
    filter(DoseAdditional2 > 0) %>%
    ungroup() %>% 
    slice(lag_booster3_age50 : n()) %>% 
    mutate(DoseAdditional3 = round(DoseAdditional2 * change_booster3_age50))
  
  LT_60_B3 <- temp %>% 
    filter(ReportCountry == "Lithuania"  & TargetGroup == "Age 60-69") %>% 
    group_by(TargetGroup) %>% 
    filter(DoseAdditional2 > 0) %>%
    ungroup() %>% 
    slice(lag_booster3_age60 : n()) %>% 
    mutate(DoseAdditional3 = round(DoseAdditional2 * change_booster3_age60))
  
  LT_70_B3 <- temp %>% 
    filter(ReportCountry == "Lithuania"  & TargetGroup == "Age 70-79") %>% 
    group_by(TargetGroup) %>% 
    filter(DoseAdditional2 > 0) %>%
    ungroup() %>% 
    slice(lag_booster3_age70 : n()) %>% 
    mutate(DoseAdditional3 = round(DoseAdditional2 * change_booster3_age70))
  
  LT_80_B3 <- temp %>% 
    filter(ReportCountry == "Lithuania"  & TargetGroup == "Age 80+") %>% 
    group_by(TargetGroup) %>% 
    filter(DoseAdditional2 > 0) %>%
    ungroup() %>% 
    slice(lag_booster3_age80 : n()) %>% 
    mutate(DoseAdditional3 = round(DoseAdditional2 * change_booster3_age80))
  
  LT_booster_3 <- LT_25_B3 %>% full_join(LT_50_B3) %>% full_join(LT_60_B3) %>% full_join(LT_70_B3) %>% full_join(LT_80_B3) 
  
  
  # Switzerland
  CH_25_B3 <- temp %>% 
    filter(ReportCountry == "Switzerland" & TargetGroup == "Age 25-49") %>% 
    group_by(TargetGroup) %>% 
    filter(DoseAdditional2 > 0) %>%
    ungroup() %>% 
    slice(lag_booster3_age25 : n()) %>% 
    mutate(DoseAdditional3 = round(DoseAdditional2 * change_booster3_age25))
  
  CH_50_B3 <- temp %>% 
    filter(ReportCountry == "Switzerland"  & TargetGroup == "Age 50-59") %>% 
    group_by(TargetGroup) %>% 
    filter(DoseAdditional2 > 0) %>%
    ungroup() %>% 
    slice(lag_booster3_age50 : n()) %>% 
    mutate(DoseAdditional3 = round(DoseAdditional2 * change_booster3_age50))
  
  CH_60_B3 <- temp %>% 
    filter(ReportCountry == "Switzerland"  & TargetGroup == "Age 60-69") %>% 
    group_by(TargetGroup) %>% 
    filter(DoseAdditional2 > 0) %>%
    ungroup() %>% 
    slice(lag_booster3_age60 : n()) %>% 
    mutate(DoseAdditional3 = round(DoseAdditional2 * change_booster3_age60))
  
  CH_70_B3 <- temp %>% 
    filter(ReportCountry == "Switzerland"  & TargetGroup == "Age 70-79") %>% 
    group_by(TargetGroup) %>% 
    filter(DoseAdditional2 > 0) %>%
    ungroup() %>% 
    slice(lag_booster3_age70 : n()) %>% 
    mutate(DoseAdditional3 = round(DoseAdditional2 * change_booster3_age70))
  
  CH_80_B3 <- temp %>% 
    filter(ReportCountry == "Switzerland"  & TargetGroup == "Age 80+") %>% 
    group_by(TargetGroup) %>% 
    filter(DoseAdditional2 > 0) %>%
    ungroup() %>% 
    slice(lag_booster3_age80 : n()) %>% 
    mutate(DoseAdditional3 = round(DoseAdditional2 * change_booster3_age80))
  
  CH_booster_3 <- CH_25_B3 %>% full_join(CH_50_B3) %>% full_join(CH_60_B3) %>% full_join(CH_70_B3) %>% full_join(CH_80_B3) 
  
  # Sweden
  SE_25_B3 <- temp %>% 
    filter(ReportCountry == "Sweden" & TargetGroup == "Age 25-49") %>% 
    group_by(TargetGroup) %>% 
    filter(DoseAdditional2 > 0) %>%
    ungroup() %>% 
    slice(lag_booster3_age25 : n()) %>% 
    mutate(DoseAdditional3 = round(DoseAdditional2 * change_booster3_age25))
  
  SE_50_B3 <- temp %>% 
    filter(ReportCountry == "Sweden"  & TargetGroup == "Age 50-59") %>% 
    group_by(TargetGroup) %>% 
    filter(DoseAdditional2 > 0) %>%
    ungroup() %>% 
    slice(lag_booster3_age50 : n()) %>% 
    mutate(DoseAdditional3 = round(DoseAdditional2 * change_booster3_age50))
  
  SE_60_B3 <- temp %>% 
    filter(ReportCountry == "Sweden"  & TargetGroup == "Age 60-69") %>% 
    group_by(TargetGroup) %>% 
    filter(DoseAdditional2 > 0) %>%
    ungroup() %>% 
    slice(lag_booster3_age60 : n()) %>% 
    mutate(DoseAdditional3 = round(DoseAdditional2 * change_booster3_age60))
  
  SE_70_B3 <- temp %>% 
    filter(ReportCountry == "Sweden"  & TargetGroup == "Age 70-79") %>% 
    group_by(TargetGroup) %>% 
    filter(DoseAdditional2 > 0) %>%
    ungroup() %>% 
    slice(lag_booster3_age70 : n()) %>% 
    mutate(DoseAdditional3 = round(DoseAdditional2 * change_booster3_age70))
  
  SE_80_B3 <- temp %>% 
    filter(ReportCountry == "Sweden"  & TargetGroup == "Age 80+") %>% 
    group_by(TargetGroup) %>% 
    filter(DoseAdditional2 > 0) %>%
    ungroup() %>% 
    slice(lag_booster3_age80 : n()) %>% 
    mutate(DoseAdditional3 = round(DoseAdditional2 * change_booster3_age80))
  
  SE_booster_3 <- SE_25_B3 %>% full_join(SE_50_B3) %>% full_join(SE_60_B3) %>% full_join(SE_70_B3) %>% full_join(SE_80_B3) 
  
  # Slovenia
  SL_25_B3 <- temp %>% 
    filter(ReportCountry == "Slovenia" & TargetGroup == "Age 25-49") %>% 
    group_by(TargetGroup) %>% 
    filter(DoseAdditional2 > 0) %>%
    ungroup() %>% 
    slice(lag_booster3_age25 : n()) %>% 
    mutate(DoseAdditional3 = round(DoseAdditional2 * change_booster3_age25))
  
  SL_50_B3 <- temp %>% 
    filter(ReportCountry == "Slovenia"  & TargetGroup == "Age 50-59") %>% 
    group_by(TargetGroup) %>% 
    filter(DoseAdditional2 > 0) %>%
    ungroup() %>% 
    slice(lag_booster3_age50 : n()) %>% 
    mutate(DoseAdditional3 = round(DoseAdditional2 * change_booster3_age50))
  
  SL_60_B3 <- temp %>% 
    filter(ReportCountry == "Slovenia"  & TargetGroup == "Age 60-69") %>% 
    group_by(TargetGroup) %>% 
    filter(DoseAdditional2 > 0) %>%
    ungroup() %>% 
    slice(lag_booster3_age60 : n()) %>% 
    mutate(DoseAdditional3 = round(DoseAdditional2 * change_booster3_age60))

  SL_70_B3 <- temp %>% 
    filter(ReportCountry == "Slovenia"  & TargetGroup == "Age 70-79") %>% 
    group_by(TargetGroup) %>% 
    filter(DoseAdditional2 > 0) %>%
    ungroup() %>% 
    slice(lag_booster3_age70 : n()) %>% 
    mutate(DoseAdditional3 = round(DoseAdditional2 * change_booster3_age70))

  SL_80_B3 <- temp %>% 
    filter(ReportCountry == "Slovenia"  & TargetGroup == "Age 80+") %>% 
    group_by(TargetGroup) %>% 
    filter(DoseAdditional2 > 0) %>%
    ungroup() %>% 
    slice(lag_booster3_age80 : n()) %>% 
    mutate(DoseAdditional3 = round(DoseAdditional2 * change_booster3_age80))
  
  SL_booster_3 <- SL_25_B3 %>% full_join(SL_50_B3) %>% full_join(SL_60_B3) %>% full_join(SL_70_B3) %>% full_join(SL_80_B3) 
  
  # Romania
  RO_25_B3 <- temp %>% 
    filter(ReportCountry == "Romania" & TargetGroup == "Age 25-49") %>% 
    group_by(TargetGroup) %>% 
    filter(DoseAdditional2 > 0) %>%
    ungroup() %>% 
    slice(lag_booster3_age25 : n()) %>% 
    mutate(DoseAdditional3 = round(DoseAdditional2 * change_booster3_age25))
  
  RO_50_B3 <- temp %>% 
    filter(ReportCountry == "Romania"  & TargetGroup == "Age 50-59") %>% 
    group_by(TargetGroup) %>% 
    filter(DoseAdditional2 > 0) %>%
    ungroup() %>%
    slice(lag_booster3_age50 : n()) %>% 
    mutate(DoseAdditional3 = round(DoseAdditional2 * change_booster3_age50))
  
  RO_60_B3 <- temp %>% 
    filter(ReportCountry == "Romania"  & TargetGroup == "Age 60-69") %>% 
    group_by(TargetGroup) %>% 
    filter(DoseAdditional2 > 0) %>%
    ungroup() %>% 
    slice(lag_booster3_age60 : n()) %>% 
    mutate(DoseAdditional3 = round(DoseAdditional2 * change_booster3_age60))
  
  RO_70_B3 <- temp %>% 
    filter(ReportCountry == "Romania"  & TargetGroup == "Age 70-79") %>% 
    group_by(TargetGroup) %>% 
    filter(DoseAdditional2 > 0) %>%
    ungroup() %>% 
    slice(lag_booster3_age70 : n()) %>% 
    mutate(DoseAdditional3 = round(DoseAdditional2 * change_booster3_age70))
  
  RO_80_B3 <- temp %>% 
    filter(ReportCountry == "Romania"  & TargetGroup == "Age 80+") %>% 
    group_by(TargetGroup) %>% 
    filter(DoseAdditional2 > 0) %>%
    ungroup() %>% 
    slice(lag_booster3_age80 : n()) %>% 
    mutate(DoseAdditional3 = round(DoseAdditional2 * change_booster3_age80))
  
  RO_booster_3 <- RO_25_B3 %>% full_join(RO_50_B3) %>% full_join(RO_60_B3) %>% full_join(RO_70_B3) %>% full_join(RO_80_B3) 
  
  
  booster_3_data <- IL_booster_3 %>% 
    full_join(RO_booster_3) %>% 
    full_join(LV_booster_3) %>% 
    full_join(LT_booster_3) %>% 
    full_join(SL_booster_3) %>% 
    full_join(CH_booster_3) %>% 
    full_join(HR_booster_3) %>% 
    full_join(ENG_booster_3) %>% 
    full_join(SE_booster_3) %>% 
    mutate(id = paste0(ReportCountry, "-", TargetGroup, "_", year_week))
  
  
  vaccine_data_clean_complete <- temp %>% 
    mutate(id = paste0(ReportCountry, "-", TargetGroup, "_", year_week)) %>% 
    filter(!id %in% booster_3_data$id) %>% 
    full_join(booster_3_data) %>% 
    select(-id)
  
  
  vaccine_data_clean_complete <- vaccine_data_clean_complete %>% 
    group_by(ReportCountry, TargetGroup, year_week, Denominator) %>%
    summarise(FirstDose = sum(FirstDose),
              SecondDose = sum(SecondDose),
              DoseAdditional1 = sum(DoseAdditional1),
              DoseAdditional2 = sum(DoseAdditional2),
              DoseAdditional3 = sum(DoseAdditional3)) %>%
    ungroup() %>%
    arrange(ReportCountry, TargetGroup, year_week) %>% 
    group_by(ReportCountry, TargetGroup) %>%
    mutate(nFirstDose = cumsum(FirstDose),
           nSecondDose = cumsum(SecondDose),
           nDoseAdditional1 = cumsum(DoseAdditional1),
           nDoseAdditional2 = cumsum(DoseAdditional2),
           nDoseAdditional3 = cumsum(DoseAdditional3),
           pcFirstDose = round((cumsum(FirstDose)/Denominator)*100),
           pcSecondDose = round((cumsum(SecondDose)/Denominator)*100),
           pcDoseAdditional1 = round((cumsum(DoseAdditional1)/Denominator)*100),
           pcDoseAdditional2 = round((cumsum(DoseAdditional2)/Denominator)*100),
           pcDoseAdditional3 = round((cumsum(DoseAdditional3)/Denominator)*100)) %>%
    ungroup() %>%
    group_by(ReportCountry, TargetGroup, year_week) %>%
    mutate(pcFirstDose = ifelse(pcFirstDose>100, 100, pcFirstDose),
           pcSecondDose = ifelse(pcSecondDose>100, 100, pcSecondDose),
           pcDoseAdditional1 = ifelse(pcDoseAdditional1 > 100, 100, pcDoseAdditional1),
           pcDoseAdditional2 = ifelse(pcDoseAdditional2 > 100, 100, pcDoseAdditional2),
           pcDoseAdditional3 = ifelse(pcDoseAdditional3 > 100, 100, pcDoseAdditional3)) %>%
    ungroup() %>% 
    filter(year_week %in% reporting.weeks) %>% 
    filter(!ReportCountry %in% c("Republic of Moldova", "United Kingdom (Scotland)", "Ukraine"))
  
  
  
  return(vaccine_data_clean_complete)
  
}
