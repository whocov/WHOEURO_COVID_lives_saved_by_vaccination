# Summary panel plots

create.summary.panel.plots <- function(Country_population, 
                                       variant_waves_region, 
                                       cases,
                                       deaths_age, 
                                       vax_clean, 
                                       Expected_cases_all_ages){

  age_group_factor_levels <- c("25 to 49 years", "50 to 59 years", "≥ 60 years")
  
  
  # Variant timings ------------------------------------------------------------
  
  variant_waves_region <- variant_waves_region %>% 
    filter(!Variant == "Index")
  
  variant_plot_timings <- variant_waves_region %>% 
    mutate(Position = c("2021-11", "2021-39", "2022-40"),
           Variant = ifelse(Variant == "Index", "", as.character(Variant))) 
  
  
  
  # Row 1: Cases with variant timings ------------------------------------------
  
  rates_cases_age <- cases %>%
    mutate(CountryName = ifelse(grepl("^UK-ENG_*", DataSource), "United Kingdom (England)", CountryName)) %>%
    mutate(CountryName = ifelse(grepl("^UK-NI_*", DataSource), "United Kingdom (Northern Ireland)", CountryName)) %>%
    mutate(CountryName = ifelse(grepl("^UK-SCOT_*", DataSource), "United Kingdom (Scotland)", CountryName)) %>%
    mutate(CountryName = ifelse(grepl("^UK-WALES_*", DataSource), "United Kingdom (Wales)", CountryName)) %>%
    filter(CountryName %in% countries_included$CountryName) %>%
    # Filter data down to needed columns
    dplyr::select(c(CountryName, ReportingCountry, DateUsedForStatisticsWeek) | (starts_with("Age"))) %>%
    filter(CountryName %in% countries_included$CountryName) %>% 
    mutate(year_week = str_replace(DateUsedForStatisticsWeek, "'", "")) %>%
    group_by(CountryName, year_week) %>%
    summarise(`Age_25-49` = sum(c(Age25.29F, Age25.29M, Age30.39F, Age30.39M, Age40.49F, Age40.49M), na.rm = TRUE),
              `Age_50-59` = sum(c(Age50.59F, Age50.59M), na.rm = TRUE),
              `Age_60+` = sum(c(Age60.64F, Age60.64M, Age65.69F, Age65.69M, Age70.74F, Age70.74M, 
                                Age75.79F, Age75.79M, Age80.F, Age80.M), na.rm = TRUE)) %>%
    ungroup() %>%
    # melt data frame    
    pivot_longer(cols = `Age_25-49`:`Age_60+`,
                 names_to = "TargetGroup",
                 values_to = "Counts") %>% 
    # Tidy up element names
    mutate(TargetGroup = str_replace(TargetGroup, "Age_", "Age "),
           Year = as.numeric(substr(year_week, 1, 4))) %>%
    arrange(CountryName, TargetGroup, year_week) %>% 
    full_join(Country_population, 
              by = c("CountryName" = "CountryName", "TargetGroup" = "TargetGroup", "Year" = "Year")) %>% 
    group_by(CountryName, TargetGroup) %>% 
    fill(Denominator, .direction = "updown") %>% 
    ungroup() %>% 
    group_by(TargetGroup, year_week) %>% 
    summarise(Counts = sum_keep_na(Counts)) %>% 
    ungroup() %>% 
    filter(year_week %in% reporting.weeks) %>% 
    mutate(TargetGroup = ifelse(TargetGroup == "Age 25-49", "25 to 49 years",
                                ifelse(TargetGroup == "Age 50-59", "50 to 59 years", "≥ 60 years")),
           TargetGroup = factor(TargetGroup, levels = age_group_factor_levels))
  
  

  # Row 2: Mortality with variant timings --------------------------------------
  
  mortality_data_plot <- deaths_age %>% 
    ungroup() %>% 
    group_by(ReportCountry, year_week, TargetGroup) %>% 
    summarise(DeathsObserved = sum(DeathsObserved)) %>% 
    ungroup() %>% 
    mutate(Year = as.numeric(substr(year_week, 1, 4))) %>% 
    arrange(ReportCountry, TargetGroup, year_week) %>%
    full_join(Country_population, 
              by = c("ReportCountry" = "CountryName", "TargetGroup" = "TargetGroup", "Year" = "Year")) %>% 
    filter(ReportCountry %in% countries_included$CountryName) %>% 
    filter(year_week %in% reporting.weeks) %>% 
    group_by(ReportCountry, TargetGroup) %>% 
    fill(Denominator, .direction = "updown") %>% 
    ungroup() %>% 
    group_by(TargetGroup, year_week) %>% 
    summarise(DeathsObserved = sum_keep_na(DeathsObserved),
              Denominator = sum(unique(Denominator)),
              Observed_MR = round((DeathsObserved / Denominator) *100000, 1)) %>% 
    ungroup() %>% 
    mutate(TargetGroup = ifelse(TargetGroup == "Age 25-49", "25 to 49 years",
                              ifelse(TargetGroup == "Age 50-59", "50 to 59 years", "≥ 60 years")),
           TargetGroup = factor(TargetGroup, levels = age_group_factor_levels))
  
  
  row.2A <- ggplot() +
    geom_segment(data = variant_waves_region, aes(x = start, xend = end, y = 920, yend = 920)) +
    geom_segment(data = variant_waves_region, aes(x = start, xend = start, y = 905, yend = 935)) +
    geom_text(data = variant_plot_timings, aes(x = Position, y = 970, label = Variant), size = 4) +
    geom_line(data = mortality_data_plot, aes(x = year_week, y = Observed_MR, group = 1, linetype = "Observed_MR"), size = 1) +
    facet_wrap(.~ TargetGroup) +
    scale_linetype_manual(values = c("Observed_MR" = "solid"),
                          labels = c("Observed_MR" = "Observed\nmortality"),
                          name = "") +
    scale_fill_manual(values = c("Observed_MR" = "black"),
                      labels = c("Observed_MR" = "Observed\nmortality"),
                      name = "") +
    scale_x_discrete(breaks = week_breaks_long, labels = week_labels_long) +
    scale_y_continuous(limits = c(0, 1050)) +
    theme_bw() +
    theme(legend.text = element_text(size = text_size, colour = "black"),
          legend.title = element_text(size = text_size, colour = "black"),
          legend.position = "right",
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size = text_size, colour = "black"),
          axis.title.y = element_text(size = text_size, colour = "black"),
          strip.background = element_rect(fill = "white"),
          strip.text = element_text(size = text_size, colour = "black")) +
    guides(colour = guide_legend(nrow = 2),
           fill = guide_legend(nrow = 2)) +
    labs(subtitle = "B) Observed mortality rates",
         x = "Week",
         y = "Observed mortality rate")
  
  
  # Combining cases and deaths rates into a single plot
  C_M_combined <- full_join(mortality_data_plot, rates_cases_age) %>% 
    group_by(TargetGroup, year_week) %>% 
    mutate(Observed_IR = round((Counts / Denominator) *100000, 1)) %>% 
    ungroup() %>% 
    select(TargetGroup, year_week, Observed_MR, Observed_IR) %>% 
    pivot_longer(cols = c(Observed_MR, Observed_IR),
                 names_to = "Variable",
                 values_to = "Rates")
  
  
  row.2B <- ggplot() +
    geom_segment(data = variant_waves_region, aes(x = start, xend = end, y = 2400, yend = 2400)) +
    geom_segment(data = variant_waves_region, aes(x = start, xend = start, y = 2350, yend = 2450)) +
    geom_text(data = variant_plot_timings, aes(x = Position, y = 2510, label = Variant), size = 4) +
    geom_line(data = C_M_combined, aes(x = year_week, y = Rates, group = Variable, colour = Variable), size = 1) +
    facet_wrap(.~ TargetGroup) +
    scale_colour_manual(name = "Observed rates",
                        values = c("Observed_MR" = "#D6852B",
                                   "Observed_IR" = "#003C50"),
                        labels = c("Observed_MR" = "Mortality",
                                   "Observed_IR" = "Infections")) +
    scale_x_discrete(breaks = week_breaks_long, labels = week_labels_long) +
    scale_y_continuous(limits = c(0, 2600),
                       breaks = c(500, 1000, 1500, 2000)) +
    theme_bw() +
    theme(legend.text = element_text(size = text_size, colour = "black"),
          legend.title = element_text(size = text_size, colour = "black"),
          legend.position = "right",
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size = text_size, colour = "black"),
          axis.title.y = element_text(size = text_size, colour = "black"),
          strip.background = element_rect(fill = "white"),
          strip.text = element_text(size = text_size, colour = "black")) +
    guides(colour = guide_legend(nrow = 2),
           fill = guide_legend(nrow = 2)) +
    labs(subtitle = "A) Observed infection and mortality rates",
         x = "Week",
         y = "Observed rates (log 10)")
  
  # Row 3: Vaccinations --------------------------------------------------------
  legend.order <- c("RateFirstDose", "RateSecondDose", "RateDoseAdditional1", "RateDoseAdditional2", "RateDoseAdditional3")
  pc.legend.order <- c("pcFirstDose", "pcSecondDose", "pcDoseAdditional1", "pcDoseAdditional2", "pcDoseAdditional3")

  vaccination_data_plot_total <- vax_clean %>%
    filter(ReportCountry %in% countries_included$CountryName) %>% 
    group_by(TargetGroup, year_week) %>%
    summarise(FirstDose = sum(FirstDose),
              SecondDose = sum(SecondDose),
              DoseAdditional1 = sum(DoseAdditional1),
              DoseAdditional2 = sum(DoseAdditional2),
              DoseAdditional3 = sum(DoseAdditional3),
              denominator = sum(unique(last(Denominator))),
              RateFirstDose = round((FirstDose/denominator*100000)),
              RateSecondDose = round((SecondDose/denominator*100000)),
              RateDoseAdditional1 = round((DoseAdditional1/denominator*100000)),
              RateDoseAdditional2 = round((DoseAdditional2/denominator*100000)),
              RateDoseAdditional3 = round((DoseAdditional3/denominator*100000))) %>%
    ungroup() %>% 
    pivot_longer(cols = "RateFirstDose":"RateDoseAdditional3",
                 names_to = "Dosage",
                 values_to = "Rate") %>% 
    select(year_week, TargetGroup, Dosage, Rate) %>% 
    mutate(Dosage = factor(Dosage, levels = legend.order),
           Rate = (Rate / 1000), 
           TargetGroup = ifelse(TargetGroup == "Age 25-49", "25 to 49 years", TargetGroup),
           TargetGroup = ifelse(TargetGroup == "Age 50-59", "50 to 59 years", TargetGroup),
           TargetGroup = ifelse(TargetGroup == "Age 60+", "≥ 60 years", TargetGroup),
           TargetGroup = factor(TargetGroup, levels = age_group_factor_levels))
  
  
  row.3A <- ggplot() +
    geom_segment(data = variant_waves_region, aes(x = start, xend = end, y = 850, yend = 850)) +
    geom_segment(data = variant_waves_region, aes(x = start, xend = start, y = 855, yend = 860)) +
    geom_text(data = variant_plot_timings, aes(x = Position, y = 920, label = Variant), size = 4) +
    geom_line(data = vaccination_data_plot_total, aes(x = year_week, y = Rate, group = Dosage, colour = Dosage), size = 1) +
    facet_wrap(.~ TargetGroup) +
    scale_colour_manual(name = "Dosage",
                        values = c("RateFirstDose" = "#a63022",
                                   "RateSecondDose" = "#D6852B",
                                   "RateDoseAdditional1" = "#1f497d",
                                   "RateDoseAdditional2" = "#3C8EA2",
                                   "RateDoseAdditional3" = "#93C7CF"), 
                        labels = c("RateFirstDose" = "First dose", 
                                   "RateSecondDose" = "Second dose",
                                   "RateDoseAdditional1" = "First booster",
                                   "RateDoseAdditional2" = "Second booster",
                                   "RateDoseAdditional3" = "Third booster")) +
    scale_fill_manual(name = "Dosage",
                      values = c("RateFirstDose" = "#a63022",
                                 "RateSecondDose" = "#D6852B",
                                 "RateDoseAdditional1" = "#1f497d",
                                 "RateDoseAdditional2" = "#3C8EA2",
                                 "RateDoseAdditional3" = "#93C7CF"), 
                      labels = c("RateFirstDose" = "First dose", 
                                 "RateSecondDose" = "Second dose",
                                 "RateDoseAdditional1" = "First booster",
                                 "RateDoseAdditional2" = "Second booster",
                                 "RateDoseAdditional3" = "Third booster")) +
    scale_x_discrete(breaks = week_breaks_long, labels = week_labels_long) +
    scale_y_continuous(labels = comma_format(big.mark = ",", accuracy = 1), limits = c(0, 950)) +
    theme_bw() +
    theme(legend.text = element_text(size = text_size, colour = "black"),
          legend.title = element_text(size = text_size, colour = "black"),
          legend.position = "right",
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size = text_size, colour = "black"),
          axis.title.y = element_text(size = text_size, colour = "black"),
          strip.background = element_rect(fill="white"),
          strip.text = element_text(size = text_size, colour = "black")) +
    labs(subtitle = "B) Vaccination rates",
         x = "Week",
         y = "Rate (x1,000)")
  
  # Cumulative coverage

  vaccination_data_plot_cumul <- vax_clean %>%
    filter(ReportCountry %in% countries_included$CountryName) %>% 
    arrange(ReportCountry, TargetGroup, year_week) %>% 
    group_by(TargetGroup, year_week) %>%
    summarise(FirstDose = sum(FirstDose),
              SecondDose = sum(SecondDose),
              DoseAdditional1 = sum(DoseAdditional1),
              DoseAdditional2 = sum(DoseAdditional2),
              DoseAdditional3 = sum(DoseAdditional3),
              denominator = sum(unique(Denominator))) %>% 
    ungroup() %>% 
    group_by(TargetGroup) %>% 
    mutate(nFirstDose = cumsum(FirstDose),
           nSecondDose = cumsum(SecondDose),
           nDoseAdditional1 = cumsum(DoseAdditional1),
           nDoseAdditional2 = cumsum(DoseAdditional2),
           nDoseAdditional3 = cumsum(DoseAdditional3),
           denominator = last(unique(denominator)),
           pcFirstDose = round((nFirstDose/denominator*100)),
           pcSecondDose = round((nSecondDose/denominator*100)),
           pcDoseAdditional1 = round((nDoseAdditional1/denominator*100)),
           pcDoseAdditional2 = round((nDoseAdditional2/denominator*100)),
           pcDoseAdditional3 = round((nDoseAdditional3/denominator*100))) %>%
    ungroup() %>% 
    pivot_longer(cols = "pcFirstDose":"pcDoseAdditional3",
                 names_to = "Dosage",
                 values_to = "Percentage") %>% 
    select(year_week, TargetGroup, Dosage, Percentage) %>% 
    distinct() %>% 
    mutate(Dosage = factor(Dosage, levels = pc.legend.order),
           TargetGroup = ifelse(TargetGroup == "Age 25-49", "25 to 49 years", TargetGroup),
           TargetGroup = ifelse(TargetGroup == "Age 50-59", "50 to 59 years", TargetGroup),
           TargetGroup = ifelse(TargetGroup == "Age 60+", "≥ 60 years", TargetGroup),
           TargetGroup = factor(TargetGroup, levels = age_group_factor_levels))
  
  

  row.3B <- ggplot() +
    geom_segment(data = variant_waves_region, aes(x = start, xend = end, y = 100, yend = 100)) +
    geom_segment(data = variant_waves_region, aes(x = start, xend = start, y = 97, yend = 102)) +
    geom_text(data = variant_plot_timings, aes(x = Position, y = 105, label = Variant), size = 4) +
    geom_line(data = vaccination_data_plot_cumul, aes(x = year_week, y = Percentage, group = Dosage, colour = Dosage), size = 1) +
    facet_grid(.~ TargetGroup) +
    scale_colour_manual(values = c("pcFirstDose" = "#a63022",
                                   "pcSecondDose" = "#D6852B",
                                   "pcDoseAdditional1" = "#1f497d",
                                   "pcDoseAdditional2" = "#3C8EA2",
                                   "pcDoseAdditional3" = "#93C7CF"),
                        labels = c("pcFirstDose" = "First dose",
                                   "pcSecondDose" = "Second dose",
                                   "pcDoseAdditional1" = "First booster",
                                   "pcDoseAdditional2" = "Second booster",
                                   "pcDoseAdditional3" = "Third booster")) +
    scale_x_discrete(breaks = week_breaks_long, labels = week_labels_long) +
    scale_y_continuous(limits = c(0, 110), 
                       breaks = c(0, 20, 40, 60, 80, 100)) +
    theme_bw() +
    theme(legend.text = element_text(size = text_size, colour = "black"),
          legend.title = element_text(size = text_size, colour = "black"),
          legend.position = "right",
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size = text_size, colour = "black"),
          axis.title.y = element_text(size = text_size, colour = "black"),
          panel.grid.minor = element_blank(),
          strip.background = element_rect(fill="white"),
          strip.text = element_text(size = text_size, colour = "black")) +
    labs(subtitle = "C) Cumulative vaccination coverage",
         x = "Week",
         y = "Percentage (%)")
  
  
  
  # Row 4: Lives saved ---------------------------------------------------------

  expected_plot_total <- Expected_cases_all_ages %>%
    filter(ReportCountry %in% countries_included$CountryName) %>% 
    select(year_week, TargetGroup, DeathsObserved, DeathsExpected, Denominator) %>%
    group_by(TargetGroup, year_week) %>% 
    summarise(DeathsExpected = sum(DeathsExpected, na.rm = TRUE),
              DeathsObserved = sum(DeathsObserved, na.rm = TRUE),
              denominator = sum(unique(Denominator), na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(Expected_MR = round((DeathsExpected / denominator) * 100000,2),
           Observed_MR = round((DeathsObserved / denominator) * 100000)) %>%
    pivot_longer(cols = c("Observed_MR", "Expected_MR"),
                 names_to = "Mortality",
                 values_to = "Rates") %>%
    mutate(TargetGroup = ifelse(TargetGroup == "Age 25-49", "25 to 49 years",
                                ifelse(TargetGroup == "Age 50-59", "50 to 59 years", "≥ 60 years")),
           TargetGroup = factor(TargetGroup, levels = age_group_factor_levels))
  
  
  row.4 <- ggplot() +
    geom_segment(data = variant_waves_region, aes(x = start, xend = end, y = 45, yend = 45)) +
    geom_segment(data = variant_waves_region, aes(x = start, xend = start, y = 43, yend = 47)) +
    geom_text(data = variant_plot_timings, aes(x = Position, y = 47, label = Variant), size = 4) +
    geom_line(data = expected_plot_total, aes(x = year_week, y = Rates, group = Mortality, colour = Mortality), size = 1) +
    facet_wrap(.~ TargetGroup) +
    scale_color_manual(name = "Mortality rates",
                       values = c("Expected_MR" = "#D6852B", 
                                  "Observed_MR" = "#003C50"),
                       labels = c("Expected_MR" = "Expected", 
                                  "Observed_MR" = "Observed")) +
    scale_fill_manual(name = "",
                      values = c("Expected_MR" = "#D6852B", 
                                 "Observed_MR" = "#003C50"),
                      labels = c("Expected_MR" = "Expected", 
                                 "Observed_MR" = "Observed")) +
    scale_x_discrete(breaks = week_breaks_long, labels = week_labels_long) +
    scale_y_continuous(limits = c(0, 50)) +
    theme_bw() +
    theme(legend.text = element_text(size = text_size, colour = "black"),
          legend.title = element_text(size = text_size, colour = "black"),
          legend.position = "right",
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size = text_size, colour = "black"),
          axis.title.y = element_text(size = text_size, colour = "black"),
          strip.background = element_rect(fill = "white"),
          strip.text = element_text(size = text_size, colour = "black")) +
    labs(subtitle = "D) Rates of lives saved and expected mortality",
         x = "Week",
         y = "Mortality rates")
  
  # Row 5: Cumulative expected deaths ------------------------------------------
  
  expected_plot_cum <- Expected_cases_all_ages %>%
    filter(ReportCountry %in% countries_included$CountryName) %>% 
    select(year_week, TargetGroup, DeathsObserved, DeathsExpected, Denominator) %>%
    group_by(TargetGroup, year_week) %>% 
    summarise(DeathsExpected = sum(DeathsExpected, na.rm = TRUE),
              DeathsObserved = sum(DeathsObserved, na.rm = TRUE),
              denominator = sum(unique(Denominator), na.rm = TRUE)) %>% 
    ungroup() %>% 
    group_by(TargetGroup) %>% 
    mutate(DeathsExpectedCum = cumsum(DeathsExpected),
           DeathsObservedCum = cumsum(DeathsObserved)) %>% 
    ungroup() %>% 
    mutate(Expected_MR = round((DeathsExpectedCum / denominator) * 100000,2),
           Observed_MR = round((DeathsObservedCum / denominator) * 100000)) %>%
    pivot_longer(cols = c("Observed_MR", "Expected_MR"),
                 names_to = "Mortality",
                 values_to = "Rates") %>%
    mutate(TargetGroup = ifelse(TargetGroup == "Age 25-49", "25 to 49 years",
                                ifelse(TargetGroup == "Age 50-59", "50 to 59 years", "≥ 60 years")),
           TargetGroup = factor(TargetGroup, levels = age_group_factor_levels))
  
  
  
  row.5 <- ggplot() +
    geom_segment(data = variant_waves_region, aes(x = start, xend = end, y = 1730, yend = 1730)) +
    geom_segment(data = variant_waves_region, aes(x = start, xend = start, y = 1720, yend = 1740)) +
    geom_text(data = variant_plot_timings, aes(x = Position, y = 1800, label = Variant), size = 4) +
    geom_line(data = expected_plot_cum, aes(x = year_week, y = Rates, group = Mortality, colour = Mortality), size = 1) +
    facet_wrap(.~ TargetGroup) +
    scale_color_manual(name = "Mortality rates",
                       values = c("Expected_MR" = "#D6852B", 
                                  "Observed_MR" = "#003C50"),
                       labels = c("Expected_MR" = "Expected", 
                                  "Observed_MR" = "Observed")) +
    scale_fill_manual(name = "",
                      values = c("Expected_MR" = "#D6852B", 
                                 "Observed_MR" = "#003C50"),
                      labels = c("Expected_MR" = "Expected", 
                                 "Observed_MR" = "Observed")) +
    scale_x_discrete(breaks = week_breaks_long, labels = week_labels_long) +
    scale_y_continuous(limits = c(0, 1840)) +
    theme_bw() +
    theme(legend.text = element_text(size = text_size, colour = "black"),
          legend.title = element_text(size = text_size, colour = "black"),
          legend.position = "right",
          axis.text = element_text(size = text_size, colour = "black"),
          axis.title = element_text(size = text_size, colour = "black"),
          strip.background = element_rect(fill="white"),
          strip.text = element_text(size = text_size, colour = "black")) +
    labs(subtitle = "E) Cumulative rates of lives saved and expected mortality",
         x = "Week",
         y = "Mortality rates")
  
  
  
  # Collate plots together -----------------------------------------------------
  
  summary_panel <- (row.2B / row.3A/ row.3B / row.4 / row.5) + plot_layout(ncol = 1)

  #ggsave("Outputs/Deaths_averted_Figure_1_new.png", dpi = 300, width = 13, height = 13)
  
  return(summary_panel)

}


