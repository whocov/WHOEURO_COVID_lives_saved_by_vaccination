# Script to plot results of sensitivity analyses against original scenario and observed mortality
# Margaux Mesle - meslem@who.int
# May 2023


plot.regional.sensitivity.curves <- function(to.save = TRUE){
  
  # Create list of all file names needed
  csvfiles <- list.files(here::here("Outputs/Sensitivity_analyses"), pattern = paste0(reporting.week,'.csv'))
  table.S1.template <- data.frame()
  
  for(i in seq_along(csvfiles)){
    
    # Read in sensitivity analyses results ---------------------------------------
    
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
      filter(!TargetGroup %in% c("Age 25-49", "Age 50-59")) %>% 
      group_by(ReportCountry, year_week) %>% 
      summarise(DeathsObserved = sum(DeathsObserved, na.rm = TRUE), 
                DeathsExpected = sum(DeathsExpected, na.rm = TRUE),
                TotalAverted = sum(TotalAverted, na.rm = TRUE),
                Denominator = sum(unique(Denominator))) %>% 
      ungroup() %>% 
      add_column(TargetGroup = "Age 60+")
    
    deaths_region <- data_in %>% 
      full_join(temp) %>% 
      filter(!TargetGroup %in% c("Age 60-69", "Age 70-79", "Age 80+")) %>% 
      select(ReportCountry, year_week, TargetGroup, Denominator,
           DeathsObserved, DeathsExpected) %>% 
      group_by(year_week, TargetGroup) %>% 
      summarise(DeathsObserved = sum(DeathsObserved),
                DeathsExpected = sum(DeathsExpected),
                Denominator = sum(Denominator)) %>%
      mutate(DeathsExpectedMR = ((DeathsExpected / Denominator) * 1000000)) %>%
      ungroup() %>% 
      add_column(Scenario = scenario.label, .before = "TargetGroup")
    
    
    
    table.S1.template <- rbind(table.S1.template, deaths_region)
      
  }
  
  
    sensitivity_summary <- table.S1.template %>% 
      filter(TargetGroup %in% c("Age 25-49", "Age 50-59", "Age 60+")) %>% 
      mutate(TargetGroup = ifelse(TargetGroup == "Age 25-49", "25 to 49 years",
                                  ifelse(TargetGroup == "Age 50-59", "50 to 59 years", "≥ 60 years")),
             TargetGroup = factor(TargetGroup, levels = c("25 to 49 years", "50 to 59 years", "≥ 60 years")))
  
    # Read in original analysis results ------------------------------------------
      
    data_original <- read.csv(paste0("Outputs/Sensitivity_analyses/Expected_cases_finer_older_", reporting.week,".csv")) %>% 
      select(ReportCountry, year_week, TargetGroup, Denominator,
             DeathsObserved, DeathsExpected) 
    
    # Calculate 60+ results
    temp <- data_original %>% 
      filter(TargetGroup %in% c("Age 60-69", "Age 70-79", "Age 80+")) %>% 
      group_by(ReportCountry, year_week) %>% 
      summarise(DeathsObserved = sum(DeathsObserved, na.rm = TRUE), 
                DeathsExpected = sum(DeathsExpected, na.rm = TRUE),
                Denominator = sum(unique(Denominator))) %>% 
      ungroup() %>% 
      add_column(TargetGroup = "Age 60+")
    
      
    data_original <- data_original %>% 
      full_join(temp) %>% 
      filter(!TargetGroup %in% c("Age 60-69", "Age 70-79", "Age 80+")) %>% 
      group_by(year_week, TargetGroup) %>% 
      summarise(DeathsObserved = sum(DeathsObserved),
                DeathsExpected = sum(DeathsExpected),
                Denominator = sum(Denominator)) %>%
      mutate(DeathsObservedMR = ((DeathsObserved / Denominator) * 1000000),
             DeathsExpectedMR = ((DeathsExpected / Denominator) * 1000000)) %>%
      ungroup() %>% 
      add_column(Scenario = "Original", .before = "TargetGroup") %>% 
      mutate(TargetGroup = ifelse(TargetGroup == "Age 25-49", "25 to 49 years",
                                  ifelse(TargetGroup == "Age 50-59", "50 to 59 years", "≥ 60 years")),
             TargetGroup = factor(TargetGroup, levels = c("25 to 49 years", "50 to 59 years", "≥ 60 years")))

    
    full_data <- data_original %>% 
      full_join(sensitivity_summary) 
    
    
    row.1 <- ggplot(data = subset(full_data, Scenario %in% c("Original", "1. High VE values", "2. Low VE values"))) +
      geom_line(aes(x = year_week, y = DeathsExpectedMR, group = Scenario, colour = Scenario, size = Scenario)) +
      geom_line(data = data_original,
                aes(x = year_week, y = DeathsObservedMR, group = 1, colour = "DeathsObservedMR", size = Scenario)) +
      facet_wrap(.~ TargetGroup, scales = "free_y") +
      scale_colour_manual(values = c("Original" = "black",
                                     "1. High VE values" = "#a63022", 
                                     "2. Low VE values" = "#3C8EA2",
                                     "DeathsObservedMR" = "grey50"),
                          labels = c("Original" = "Original scenario",
                                     "1. High VE values" = "1. High VE values", 
                                     "2. Low VE values" = "2. Low VE values",
                                     "DeathsObservedMR" = "Reported mortality")) +
      scale_size_manual(values = c("Original" = 0.6,
                                   "1. High VE values" = 0.75, 
                                   "2. Low VE values" = 0.75,
                                   "DeathsObservedMR" = 0.6)) +
      scale_x_discrete(breaks = week_breaks_long, labels = week_labels_long) +
      theme_bw() +
      theme(legend.text = element_text(size = 12, colour = "black"),
            legend.title = element_text(size = 12, colour = "black"),
            legend.position = "right",
            plot.subtitle = element_text(size = 12, colour = "black"),
            strip.text = element_text(size = 12),
            strip.background = element_rect(fill = "white"),
            axis.text = element_text(size = 12, colour = "black"),
            axis.title = element_text(size = 12, colour = "black"),
            axis.line = element_line()) +
      guides(size = "none") +
      labs(subtitle = "Sensitivity analyses: Vaccine Effectiveness",
           x = "Year-week",
           y = "Mortality rate (per 100,000)")
    
    
    row.2 <- ggplot(data = subset(full_data, Scenario %in% c("Original", "3. Long lag time", "4. Short lag time"))) +
      geom_line(aes(x = year_week, y = DeathsExpectedMR, group = Scenario, colour = Scenario, size = Scenario)) +
      geom_line(data = data_original,
                aes(x = year_week, y = DeathsObservedMR, group = 1, colour = "DeathsObservedMR", size = Scenario)) +
      facet_wrap(.~ TargetGroup, scales = "free_y") +
      scale_colour_manual(values = c("Original" = "black",
                                     "3. Long lag time" = "#3C8EA2", 
                                     "4. Short lag time" = "#a63022",
                                     "DeathsObservedMR" = "grey50"),
                          labels = c("Original" = "Original scenario",
                                     "3. Long lag time" = "3. Long lag time", 
                                     "4. Short lag time" = "4. Short lag time",
                                     "DeathsObservedMR" = "Reported mortality")) +
      scale_size_manual(values = c("Original" = 0.6,
                                 "3. Long lag time" = 0.75, 
                                 "4. Short lag time" = 0.75,
                                 "DeathsObservedMR" = 0.6)) +
      scale_x_discrete(breaks = week_breaks_long, labels = week_labels_long) +
      theme_bw() +
      theme(legend.text = element_text(size = 12, colour = "black"),
            legend.title = element_text(size = 12, colour = "black"),
            legend.position = "right",
            plot.subtitle = element_text(size = 12, colour = "black"),
            strip.text = element_text(size = 12),
            strip.background = element_rect(fill = "white"),
            axis.text = element_text(size = 12, colour = "black"),
            axis.title = element_text(size = 12, colour = "black"),
            axis.line = element_line()) +
      guides(size = "none") +
      labs(subtitle = "Sensitivity analyses: lag times",
           x = "Year-week",
           y = "Mortality rate (per 100,000)")
    
    row.3 <- ggplot(data = subset(full_data, Scenario %in% c("Original", "5. Long waning time", "6. Short waning time"))) +
      geom_line(aes(x = year_week, y = DeathsExpectedMR, group = Scenario, colour = Scenario, size = Scenario)) +
      geom_line(data = data_original,
                aes(x = year_week, y = DeathsObservedMR, group = 1, colour = "DeathsObservedMR", size = Scenario)) +
      facet_wrap(.~ TargetGroup, scales = "free_y") +
      scale_colour_manual(values = c("Original" = "black",
                                     "5. Long waning time" = "#a63022", 
                                     "6. Short waning time" = "#3C8EA2",
                                     "DeathsObservedMR" = "grey50"),
                          labels = c("Original" = "Original scenario",
                                     "5. Long waning time" = "5. Long waning time", 
                                     "6. Short waning time" = "6. Short waning time",
                                     "DeathsObservedMR" = "Reported mortality")) +
      scale_size_manual(values = c("Original" = 0.6,
                                   "5. Long waning time" = 0.75, 
                                   "6. Short waning time" = 0.75,
                                   "DeathsObservedMR" = 0.6)) +
      scale_x_discrete(breaks = week_breaks_long, labels = week_labels_long) +
      theme_bw() +
      theme(legend.text = element_text(size = 12, colour = "black"),
            legend.title = element_text(size = 12, colour = "black"),
            legend.position = "right",
            plot.subtitle = element_text(size = 12, colour = "black"),
            strip.text = element_text(size = 12),
            strip.background = element_rect(fill = "white"),
            axis.text = element_text(size = 12, colour = "black"),
            axis.title = element_text(size = 12, colour = "black"),
            axis.line = element_line()) +
      guides(size = "none") +
      labs(subtitle = "Sensitivity analyses: Vaccine waning time",
           x = "Year-week",
           y = "Mortality rate (per 100,000)")
  
    row.4 <- ggplot(data = subset(full_data, Scenario %in% c("Original", "7. Prior immunity high", "8. Prior immunity low"))) +
      geom_line(aes(x = year_week, y = DeathsExpectedMR, group = Scenario, colour = Scenario, size = Scenario)) +
      geom_line(data = data_original,
                aes(x = year_week, y = DeathsObservedMR, group = 1, colour = "DeathsObservedMR", size = Scenario)) +
      facet_wrap(.~ TargetGroup, scales = "free_y") +
      scale_colour_manual(values = c("Original" = "black",
                                     "7. Prior immunity high" = "#a63022", 
                                     "8. Prior immunity low" = "#3C8EA2",
                                     "DeathsObservedMR" = "grey50"),
                          labels = c("Original" = "Original scenario",
                                     "7. Prior immunity high" = "7. Prior immunity high", 
                                     "8. Prior immunity low" = "8. Prior immunity low",
                                     "DeathsObservedMR" = "Reported mortality")) +
      scale_size_manual(values = c("Original" = 0.6,
                                   "7. Prior immunity high" = 0.75, 
                                   "8. Prior immunity low" = 0.75, 
                                   "DeathsObservedMR" = 0.6)) +
      scale_x_discrete(breaks = week_breaks_long, labels = week_labels_long) +
      theme_bw() +
      theme(legend.text = element_text(size = 12, colour = "black"),
            legend.title = element_text(size = 12, colour = "black"),
            legend.position = "right",
            plot.subtitle = element_text(size = 12, colour = "black"),
            strip.text = element_text(size = 12),
            strip.background = element_rect(fill = "white"),
            axis.text = element_text(size = 12, colour = "black"),
            axis.title = element_text(size = 12, colour = "black"),
            axis.line = element_line()) +
      guides(size = "none") +
      labs(subtitle = "Sensitivity analyses: Prior immunity",
           x = "Year-week",
           y = "Mortality rate (per 100,000)")
    
    
    sensitivity_summary_plot <- ggarrange(row.1, row.2, row.3, row.4, ncol = 1, align = "hv")  
    
    if(to.save == TRUE){
      ggsave("Outputs/WHO_EURO_Lives_saved_COVID_vaccines_Figure_3.png", dpi = 300, height = 12, width = 12)
    }
    
    return(sensitivity_summary_plot)
  
} 
