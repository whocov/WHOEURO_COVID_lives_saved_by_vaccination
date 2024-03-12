# Script to clean mortality data
# Margaux Mesle - meslem@who.int
# Version1: July 2021
# Updated: March 2022


calculate.mortality.by.age.update <- function(deaths, 
                                              Croatia_mortality,
                                              Iceland_mortality, 
                                              Israel_mortality, 
                                              Kosovo_mortality, 
                                              Lithuania_mortality,
                                              Scotland_motality,
                                              Romania_mortality,
                                              year_months_template) {
  
     age.cols <- c("Deaths25.29F", "Deaths25.29M", "Deaths30.39F", "Deaths30.39M", 
                   "Deaths40.49F", "Deaths40.49M", "Deaths50.59M", "Deaths50.59F", "Deaths60.64M", "Deaths60.64F", 
                   "Deaths65.69M", "Deaths65.69F", "Deaths70.74M", "Deaths70.74F", "Deaths75.79M", "Deaths75.79F",
                   "Deaths80.M", "Deaths80.F")

     # Tidy up additional countries --------------------------------------------
     # Croatia
     Croatia_mortality_clean <- Croatia_mortality %>% 
       mutate(yearweek_temp = paste0(substr(yearweek, 4, 7), "-", substr(yearweek, 1, 2)),
              year_week = yearweek_temp,
              Period = ifelse(year_week %in% omicron.weeks, "Omicron", "Pre-Omicron")) %>% 
       pivot_longer(cols = `25-49` : `80+`,
                    names_to = "age_group",
                    values_to = "DeathsObserved") %>% 
       add_column(CountryName = "Croatia") %>% 
       mutate(age_group = paste0("Age", age_group)) %>% 
       dplyr::select(CountryName, Period, year_week, age_group, DeathsObserved) %>% 
       filter(year_week %in% reporting.weeks) 

     # Iceland
     Iceland_mortality_clean <- clean.Iceland.mortality.data(Iceland_mortality, resolution = "")
     
     # Israel
     Israel_mortality_clean <- Israel_mortality %>% 
       rename(DeathsObserved = count_cases) %>% 
       filter(!is.na(DateUsedForStatistics)) %>% 
       mutate(year_week = str_replace(DateUsedForStatistics, "W", ""),
              Period = ifelse(year_week %in% omicron.weeks, "Omicron", "Pre-Omicron"),
              TargetGroup = ifelse(TargetGroup == "Age25-49", "Age 25-49",
                                   ifelse(TargetGroup == "Age50-59", "Age 50-59", "Age 60+"))) %>% 
       select(ReportCountry, Period, year_week, TargetGroup, DeathsObserved) %>% 
       filter(year_week %in% reporting.weeks) 
     
     Kosovo_mortality_clean <- Kosovo_mortality %>% 
       select(c(ReportingCountry, DateUsedForStatisticsISO) | (starts_with("Deaths"))) %>% 
       mutate(DateUsedForStatisticsISO = str_replace(DateUsedForStatisticsISO, "W", ""),
              Period = ifelse(DateUsedForStatisticsISO %in% omicron.weeks, "Omicron", "Pre-Omicron"),
              CountryName = "Kosovo") %>% 
       # melt data frame   
       group_by(CountryName, Period) %>% 
       pivot_longer(cols = age.cols,
                    names_to = "age_group",
                    values_to = "DeathsObserved") %>% 
       rename(year_week = DateUsedForStatisticsISO) %>%
       dplyr::select(CountryName, Period, year_week, age_group, DeathsObserved) %>% 
       mutate(year_week = str_replace(year_week, "'", ""),
              age_group = str_replace(age_group, "Deaths", "Age"),
              age_group = str_replace(age_group, "\\D$", ""),
              age_group = str_replace(age_group, "\\.", "-"),
              age_group = str_replace(age_group, "80", "80+")) %>% 
       filter(year_week %in% reporting.weeks)
     
     # Lithuania
     Lithuania_mortality_clean <- Lithuania_mortality %>% 
       mutate(Period = "Omicron")
       
     # Scotland
     Scotland_mortality_clean <- Scotland_mortality %>% 
       rename(DeathsObserved = X25.49) %>% 
       mutate(year_week = str_replace(year_week, "W", "-"),
              DeathsObserved = as.numeric(DeathsObserved),
              DeathsObserved = ifelse(is.na(DeathsObserved), 0, DeathsObserved),
              TargetGroup = str_replace(TargetGroup, "Age", "Age "),
              TargetGroup = str_replace(TargetGroup, "_", "-")) %>% 
       add_column(ReportCountry = "United Kingdom (Scotland)", .before = "year_week") %>% 
       add_column(Period = "Omicron", .before = "year_week")
     
     # Romania
     Romania_mortality_clean <- Romania_mortality %>% 
       select(c(ReportingCountry, DateUsedForStatisticsISO) | (starts_with("Deaths"))) %>% 
       mutate(DateUsedForStatisticsISO = str_replace(DateUsedForStatisticsISO, "W", ""),
              Period = ifelse(DateUsedForStatisticsISO %in% omicron.weeks, "Omicron", "Pre-Omicron"),
              CountryName = "Romania") %>% 
       # melt data frame   
       group_by(CountryName, Period) %>% 
       pivot_longer(cols = c("Deaths25-29F", "Deaths25-29M", "Deaths30-39F", "Deaths30-39M", 
                             "Deaths40-49F", "Deaths40-49M", "Deaths50-59M", "Deaths50-59F", "Deaths60-64M", "Deaths60-64F", 
                             "Deaths65-69M", "Deaths65-69F", "Deaths70-74M", "Deaths70-74F", "Deaths75-79M", "Deaths75-79F",
                             "Deaths80+M", "Deaths80+F"),
                    names_to = "age_group",
                    values_to = "DeathsObserved") %>% 
       rename(year_week = DateUsedForStatisticsISO) %>%
       dplyr::select(CountryName, Period, year_week, age_group, DeathsObserved) %>% 
       mutate(year_week = str_replace(year_week, "'", ""),
              age_group = str_replace(age_group, "Deaths", "Age"),
              age_group = str_replace(age_group, "\\D$", ""),
              age_group = str_replace(age_group, "\\.", "-")) %>% 
       filter(year_week %in% reporting.weeks)
     
     
  # Data cleaning --------------------------------------------------------------
  deaths_age <- deaths %>%
    filter(!CountryName == "Kosovo") %>% 
    mutate(CountryName = ifelse(grepl("^UK-ENG_*", DataSource), "United Kingdom (England)", CountryName)) %>%
    mutate(CountryName = ifelse(grepl("^UK-NI_*", DataSource), "United Kingdom (Northern Ireland)", CountryName)) %>%
    mutate(CountryName = ifelse(grepl("^UK-SCOT_*", DataSource), "United Kingdom (Scotland)", CountryName)) %>%
    mutate(CountryName = ifelse(grepl("^UK-WALES_*", DataSource), "United Kingdom (Wales)", CountryName)) %>% 
    mutate(CountryName = ifelse(CountryName == "Moldova", "Republic of Moldova", CountryName)) %>%
    # Filter data down to needed columns
    dplyr::select(c(CountryName, ReportingCountry, DateUsedForStatisticsISO) | (starts_with("Deaths"))) %>%
    mutate(DateUsedForStatisticsISO = str_replace(DateUsedForStatisticsISO, "W", ""),
           Period = ifelse(DateUsedForStatisticsISO %in% omicron.weeks, "Omicron", "Pre-Omicron")) %>% 
    # melt data frame   
    group_by(CountryName, Period) %>% 
    pivot_longer(cols = age.cols,
                 names_to = "age_group",
                 values_to = "DeathsObserved") %>% 
    rename(year_week = DateUsedForStatisticsISO) %>%
    dplyr::select(CountryName, Period, year_week, age_group, DeathsObserved) %>%
    # Tidy up element names
    mutate(year_week = str_replace(year_week, "'", ""),
           age_group = str_replace(age_group, "Deaths", "Age"),
           age_group = str_replace(age_group, "\\D$", ""),
           age_group = str_replace(age_group, "\\.", "-"),
           age_group = str_replace(age_group, "80", "80+")) 
  
  deaths_age <- deaths_age %>% 
    full_join(Kosovo_mortality_clean) %>% 
    full_join(Croatia_mortality_clean)
  
  deaths_age <- deaths_age %>%
      mutate(age_group = ifelse(age_group %in% c("Age25-29","Age30-39", "Age40-49"), "Age25-49", age_group),
             age_group = ifelse(age_group %in% c("Age60-64", "Age65-69","Age70-74", "Age75-79", "Age80+-"), "Age60+", age_group)) %>% 
      group_by(CountryName, Period, year_week, age_group) %>%
      summarise(DeathsObserved = sum_keep_na(DeathsObserved)) %>% 
      ungroup() %>% 
      mutate(DeathsObserved = ifelse(CountryName %in% c("Belgium", "Hungary") & is.na(DeathsObserved), 0, DeathsObserved)) %>% 
      ungroup()

   deaths_age <- deaths_age %>% 
     filter(!(CountryName %in% c("Germany", "Republic of Moldova") & age_group %in% c("Age25-49", "Age50-59", "Age60-69", "Age70-79", "Age80+"))) %>% 
     filter(!(CountryName == "Ukraine" & age_group %in% c("Age25-49", "Age50-59"))) %>% 
     filter(!(CountryName == "Israel" & str_detect(year_week, "^2022") | CountryName == "Israel" & str_detect(year_week, "^2023"))) %>% 
     mutate(TargetGroup = ifelse(age_group == "Age25-49", "Age 25-49",
                                 ifelse(age_group == "Age50-59", "Age 50-59", "Age 60+"))) %>% 
     rename(ReportCountry = CountryName) %>% 
     select(ReportCountry, Period, year_week, TargetGroup, DeathsObserved)
    
   deaths_age <- deaths_age %>% 
     full_join(Iceland_mortality_clean) %>% 
     filter(!(ReportCountry == "Israel" & year_week %in% Israel_mortality_clean$year_week)) %>% 
     full_join(Israel_mortality_clean) 
   
   deaths_age <- deaths_age %>% 
     filter(!(ReportCountry == "Lithuania" & year_week %in% Lithuania_mortality$year_week)) %>% 
     full_join(Lithuania_mortality_clean)
   
   deaths_age <- deaths_age %>% 
     filter(!(ReportCountry == "United Kingdom (Scotland)" & year_week %in% Scotland_mortality_clean$year_week)) %>% 
     full_join(Scotland_mortality_clean)
   
   # Make sure all countries have all weeks per age group
   full_template <- expand.grid(ReportCountry = unique(deaths_age$ReportCountry),
                                year_week = unique(deaths_age$year_week),
                                TargetGroup = unique(deaths_age$TargetGroup),
                                stringsAsFactors = FALSE) %>% 
     distinct() %>% 
     filter(!(ReportCountry %in% c("Germany", "Republic of Moldova") & 
              TargetGroup %in% c("Age 25-49", "Age 50-59", "Age 60-69", "Age 70-79", "Age 80+")))
   
   
   deaths_age <- deaths_age %>% 
     ungroup() %>% 
     group_by(ReportCountry) %>% 
     full_join(full_template, by = c("ReportCountry", "year_week", "TargetGroup")) %>% 
     ungroup() %>% 
     arrange(ReportCountry, TargetGroup, year_week) %>% 
     fill(ReportCountry, .direction = "down") %>% 
     fill(Period, .direction = "down")
   
   
  return(deaths_age)
}
