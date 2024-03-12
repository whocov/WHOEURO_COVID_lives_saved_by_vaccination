

run.data.completeness.heatmap <- function(deaths_age, vaccine){
  
  template <- expand.grid(CountryName = unique(country_codes$report_country),
                          year_week = reporting.weeks,
                          age_group = c("Age 25_49", "Age 50_59", "Age 60+"),
                          stringsAsFactors = FALSE) %>% 
    mutate(CountryName = ifelse(str_detect(CountryName, "^Kosovo"), "Kosovo", CountryName),
           CountryName = ifelse(CountryName == "Türkiye", "Turkey", CountryName))
  
  
  # Mortality data ---------------------------------------------------------------
  age.cols <- c("Deaths25.49", "Deaths50.59M", "Deaths50.59F", "Deaths60.64M", "Deaths60.64F", "Deaths65.69M", "Deaths65.69F", 
             "Deaths70.74M", "Deaths70.74F", "Deaths75.79M", "Deaths75.79F",
             "Deaths80.M", "Deaths80.F")
  

  age.cols = c("Deaths20.24F", "Deaths20.24M", "Deaths25.29F", "Deaths25.29M", "Deaths30.39F", "Deaths30.39M", 
               "Deaths40.49F", "Deaths40.49M", "Deaths50.59M", "Deaths50.59F", "Deaths60.64M", "Deaths60.64F", 
               "Deaths65.69M", "Deaths65.69F", "Deaths70.74M", "Deaths70.74F", "Deaths75.79M", "Deaths75.79F",
               "Deaths80.M", "Deaths80.F")
  
  # Data cleaning before aggregation by age group step
  deaths_age <- deaths %>%
    mutate(CountryName = ifelse(grepl("^UK-ENG_*", DataSource), "United Kingdom (England)", CountryName)) %>%
    mutate(CountryName = ifelse(grepl("^UK-NI_*", DataSource), "United Kingdom (Northern Ireland)", CountryName)) %>%
    mutate(CountryName = ifelse(grepl("^UK-SCOT_*", DataSource), "United Kingdom (Scotland)", CountryName)) %>%
    mutate(CountryName = ifelse(grepl("^UK-WALES_*", DataSource), "United Kingdom (Wales)", CountryName)) %>% 
    mutate(CountryName = ifelse(CountryName == "Moldova", "Republic of Moldova", CountryName)) %>%
    # Filter data down to needed columns
    dplyr::select(c(CountryName, ReportingCountry, DateUsedForStatisticsISO) | (starts_with("Deaths"))) %>%
    mutate(DateUsedForStatisticsISO = str_replace(DateUsedForStatisticsISO, "W", "")) %>% 
    # melt data frame   
    group_by(CountryName) %>% 
    pivot_longer(cols = age.cols,
                 names_to = "age_group",
                 values_to = "DeathsObserved") %>% 
    rename(year_week = DateUsedForStatisticsISO) %>%
    dplyr::select(CountryName, year_week, age_group, DeathsObserved) %>%
    # Tidy up element names
    mutate(year_week = str_replace(year_week, "'", ""),
           age_group = str_replace(age_group, "Deaths", "Age"),
           age_group = str_replace(age_group, "\\D$", ""),
           age_group = str_replace(age_group, "\\.", "-"),
           age_group = str_replace(age_group, "80", "80+")) 
  
  mortality_summary <- deaths_age %>%
    mutate(age_group = ifelse(age_group %in% c("Age20-24", "Age25-29","Age30-39", "Age40-49"), "Age25-49", age_group),
           age_group = ifelse(age_group %in% c("Age60-64", "Age65-69","Age70-74", "Age75-79", "Age80+-"), "Age60+", age_group),
           TargetGroup = str_replace(age_group, "Age", "Age ")) %>% 
    group_by(CountryName, year_week, TargetGroup) %>%
    summarise(DeathsObserved = sum_keep_na(DeathsObserved)) %>% 
    ungroup() %>% 
    mutate(DeathsObserved = ifelse(CountryName %in% c("Belgium", "Hungary") & is.na(DeathsObserved), 0, DeathsObserved)) %>% 
    filter(year_week %in% reporting.weeks) %>% 
    mutate(DeathsObserved = ifelse(is.na(DeathsObserved), "0", "1")) %>% 
    rename(ReportCountry = CountryName)
  
  mortality_plot <- ggplot(mortality_summary, aes(x = year_week, y = ReportCountry, fill = DeathsObserved)) +
    geom_tile() +
    facet_grid(. ~ TargetGroup) +
    scale_fill_manual(name = "Data reported",
                      values = c("0" = "white",
                                 "1" = "#1f497d"),
                      labels = c("0" = "No",
                                 "1" = "Yes")) +
    scale_y_discrete(limits = rev) +
    scale_x_discrete(breaks = week_breaks_long, labels = week_labels_long) +
    theme_bw() +
    labs(subtile = "Mortality data summary",
         x = "Calendar week",
         y = "Country name")
  
  
  
  # Vaccination data -----------------------------------------------------------

  vaccination_summary <- vax_clean %>% 
    mutate(FirstDose = ifelse(is.na(FirstDose), "0", "1")) %>% 
   select(ReportCountry, TargetGroup, year_week, FirstDose)
  
  
 vaccination_plot <- ggplot(vaccination_summary, aes(x = year_week, y = ReportCountry, fill = FirstDose)) +
    geom_tile() +
    facet_grid(. ~ TargetGroup) +
    scale_fill_manual(name = "Data reported",
                      na.value = "white",
                      values = c("0" = "white",
                                 "1" = "#1f497d"),
                      labels = c("0" = "No",
                                 "1" = "Yes")) +
    scale_y_discrete(limits = rev) +
    scale_x_discrete(breaks = week_breaks_long, labels = week_labels_long) +
    theme_bw() +
    labs(subtile = "Vaccination data summary",
         x = "Calendar week",
         y = "Country name")
  
 
 # Summary data ----------------------------------------------------------------
 
 summary_data <- mortality_summary %>% 
   full_join(vaccination_summary) %>% 
   filter(ReportCountry %in% countries_included$CountryName) %>% 
   filter(!ReportCountry %in% countries_no_reporting)
 
 
  ggplot(summary_data, aes(x = year_week, y = ReportCountry, fill = FirstDose)) +
   geom_tile() +
   facet_grid(. ~ TargetGroup) +
   scale_fill_manual(name = "Data reported",
                     na.value = "white",
                     values = c("0" = "white",
                                "1" = "#1f497d"),
                     labels = c("0" = "No",
                                "1" = "Yes")) +
   scale_y_discrete(limits = rev) +
   theme_bw() +
   labs(subtile = "Vaccination data summary",
        x = "Calendar week",
        y = "Country name")
 
 
  summary_weeks <- summary_data %>% 
    group_by(ReportCountry, TargetGroup) %>% 
    summarise(n_weeks = length(year_week),
              n_weeks_deaths = sum(as.numeric(DeathsObserved), na.rm = TRUE),
              n_weeks_vaccs = sum(as.numeric(FirstDose), na.rm = TRUE)) %>% 
    mutate(pc_deaths = round((n_weeks_deaths / n_weeks)*100),
           pc_vaccs = round((n_weeks_vaccs / n_weeks)*100)) %>% 
    ungroup() %>% 
    filter(!(n_weeks_deaths == 0 & n_weeks_vaccs == 0)) %>% 
    filter(!(n_weeks_deaths == 0 | n_weeks_vaccs == 0))
  
  
 
 
 ggarrange(mortality_plot, vaccination_plot, common.legend = TRUE)
  
  summary_data <- full_join(mortality_summary, vaccination_summary)  %>% 
    mutate(ReportCountry = ifelse(str_detect(ReportCountry, "^Kosovo"), "Kosovo", ReportCountry)) %>% 
    mutate(Variable = factor(Variable, levels = c("DoseFirst", "DoseSecond", "DoseAdditional1", "DoseAdditional2", "Mortality"))) %>% 
    mutate(Count_labels = ifelse(is.na(Counts), "No", "Yes"))
  
  
  ggplot(summary_data, aes(x = year_week, y = CountryName, fill = Count_labels)) +
    geom_tile() +
    facet_grid(age_group ~ Variable) +
    scale_fill_manual(values = c("No" = "white",
                                 "Yes" = "darkblue"),
                      name = "data available?") +
    scale_x_discrete(breaks = week_breaks_long,
                     labels = week_labels_long) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  
  # Table summary ----------------------------------------------------------------
  
  mortality_summary <- deaths %>% 
    mutate(CountryName = ifelse(grepl("^UK-ENG_*", DataSource), "United Kingdom, England", CountryName),
           CountryName = ifelse(grepl("^UK-NI_*", DataSource), "United Kingdom, Northern Ireland", CountryName),
           CountryName = ifelse(grepl("^UK-SCOT_*", DataSource), "United Kingdom, Scotland", CountryName),
           CountryName = ifelse(grepl("^UK-WALES_*", DataSource), "United Kingdom, Wales", CountryName),
           CountryName = ifelse(CountryName == "Moldova", "Republic of Moldova", CountryName),
           CountryName = ifelse(CountryName == "Russia", "Russian Federation", CountryName)) %>%
    # Filter data down to needed columns
    dplyr::select(c(CountryName, ReportingCountry, DateUsedForStatisticsWeek) | (starts_with("Deaths"))) %>%
    pivot_longer(cols = age.cols) %>% 
    rename(year_week = DateUsedForStatisticsWeek,
           age_group = name,
           DeathsObserved = value) %>%
    dplyr::select(CountryName, year_week, age_group, DeathsObserved) %>%
    # Tidy up element names
    mutate(year_week = str_replace(year_week, "'", ""),
           age_group = str_replace(age_group, "Deaths", "Age"),
           age_group = str_replace(age_group, "\\D$", ""),
           age_group = str_replace(age_group, "\\.", "-"),
           age_group = str_replace(age_group, "80", "80+")) %>% 
    mutate(age_group = ifelse(age_group %in% c("Age60-64", "Age65-69","Age70-74", "Age75-79", "Age80+-"), "Age60+", age_group),
           age_group = str_replace(age_group, "-", "_")) %>% 
    full_join(template) %>% 
    group_by(CountryName, year_week, age_group) %>%
    summarise(Counts = sum_keep_na(DeathsObserved)) %>%
    ungroup() %>% 
    group_by(CountryName, year_week) %>%
    mutate(Total_counts = sum_keep_na(Counts),
           Counts = ifelse(is.na(Counts) & !is.na(Total_counts), 0, Counts)) %>% 
    ungroup() %>% 
    filter(year_week %in% reporting.weeks) %>% 
    select(-Total_counts)
  
  vaccination_data <- vaccine %>% select(DateUsedForStatisticsISO, Region, ReportingCountry, 
                                         TargetGroup, Denominator, NumberDosesReceived, DoseFirst, DoseFirstRefused, 
                                         DoseSecond, DoseAdditional1, DoseAdditional2, DoseUnk, Vaccine) %>% 
    mutate(ReportingCountry = ifelse(Region == 'UK_ENG', "UKEngland",
                                     ifelse(Region == 'UKM', "UKScotland",
                                            ifelse(Region == 'UKL', "UKWales", ReportingCountry))))
  
  vaccination_data <- left_join(vaccination_data, select(country_codes, ReportingCountry, report_country), by=c("ReportingCountry" = "ReportingCountry"))
  
  vaccination_data <- vaccination_data %>% 
    filter(TargetGroup %in% c("1_Age60+", "Age25_49", "Age50_59")) %>% 
    mutate(TargetGroup = ifelse(TargetGroup == "1_Age60+", "Age60+", TargetGroup),
           year_week = str_replace(DateUsedForStatisticsISO, "W", ""),
           report_country = ifelse(str_detect(report_country, "^Kosovo"), "Kosovo", report_country),
           report_country = ifelse(report_country == "Moldova", "Republic of Moldova", report_country),
           report_country = ifelse(report_country == "Russia", "Russian Federation", report_country)) %>% 
    rename(CountryName = report_country,
           age_group = TargetGroup) %>% 
    full_join(template) %>% 
    filter(year_week %in% reporting.weeks) %>% 
    group_by(CountryName, year_week, age_group) %>% 
    summarise(DoseFirst = sum_keep_na(DoseFirst),
              DoseSecond = sum_keep_na(DoseSecond),
              DoseAdditional1 = sum_keep_na(DoseAdditional1),
              DoseAdditional2 = sum_keep_na(DoseAdditional2)) %>% 
    ungroup() 
  
  
  summary_data_table <- full_join(mortality_summary, vaccination_data)
  #write.csv(summary_data_table, "DA_data_summary_countries.csv", row.names = FALSE)

  return()
}
