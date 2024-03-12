# Script to plot country mortality rates
# Margaux Mesle - meslem@who.int
# March 2022


plot.country.mortality.curves <- function(deaths_age, 
                                          vax_clean, 
                                          variant_waves, 
                                          Country_population, 
                                          measure, 
                                          to.save = TRUE) {
  

  if(measure == "Count"){
    # Plot country mortality counts
    

    mortality_curves <- ggplot() +
      new_scale_fill() +
      geom_bar(data = subset(deaths_age, CountryName %in% countries.show$CountryName),
               aes(year_week, DeathsObserved, fill = age_group), stat = "identity", position = "stack") +
      geom_vline(data = subset(vax_start_dates, CountryName %in% countries.show$CountryName), 
                 aes(xintercept = year_week), col = "black") +
      facet_wrap(.~ CountryName, scales = "free_y", ncol=5) +
      scale_colour_manual(name = "Vaccination programmes\nprogress",
                          values = c("start"="black"),
                          labels = c("start"="Start of vaccination")) +
      scale_fill_manual(name = "Age groups\n(years)",
                        labels = c("Age25_49"="25 - 49",
                                  "Age50_59"="50 - 59",
                                  "Age60+"="60 and over"),
                        values = c("Age25_49" = "#a82d17", 
                                  "Age50_59" = "#e1a744", 
                                  "Age60+"=  "#1f497d")) + 
      scale_x_discrete(breaks = week_breaks_long, labels = week_labels_long) +
      scale_y_continuous(labels = comma_format(big.mark = ",", accuracy = 1)) +
      theme_bw() +
      theme(legend.text = element_text(size = 12, colour = "black"),
            legend.title = element_text(size = 12, colour = "black"),
            axis.text = element_text(size = 12, colour = "black"),
            axis.title = element_text(size = 12, colour = "black"),
            strip.background = element_rect(fill = "white")) +
      labs(subtitle = "Count of deaths per week and country, with the black vertical line representing the start of vaccination programmes.",
           x = "Week",
           y = "Count of mortality",
           caption = "[1] All references to Kosovo in this document should be understood to be in the context of the United Nations Security Council resolution 1244 (1999).")
    
  } 
  
  if(measure == "Rate"){
    

    rates_age <- deaths_age %>% 
      filter(ReportCountry %in% countries_included$CountryName) %>% 
      mutate(Year = as.numeric(substr(year_week, 1, 4))) %>% 
      full_join(Country_population, by = c("ReportCountry" = "CountryName", "TargetGroup" = "TargetGroup", "Year" = "Year")) %>% 
      group_by(ReportCountry, TargetGroup) %>% 
      arrange(ReportCountry, TargetGroup, year_week) %>% 
      fill(Denominator, .direction = "updown") %>% 
      ungroup() %>% 
      group_by(ReportCountry, TargetGroup, year_week) %>% 
      summarise(DeathsObserved = sum_keep_na(DeathsObserved),
                Denominator = sum(unique(Denominator)),
                MortalityRate = round((DeathsObserved / Denominator)*100000)) %>% 
      ungroup() %>% 
      filter(year_week %in% reporting.weeks) %>% 
      mutate(ReportCountry = ifelse(ReportCountry == "Kosovo", "Kosovo*", ReportCountry))
    

    variant_temp <- variant_waves %>%
      filter(ReportCountry %in% rates_age$ReportCountry) %>% 
      mutate(ReportCountry = ifelse(ReportCountry == "Kosovo", "Kosovo[1]", ReportCountry))
    
    vaccination_start_dates <- vaccination_start_dates %>% 
      mutate(ReportCountry = ifelse(ReportCountry == "Kosovo", "Kosovo[1]", ReportCountry))
    
    mortality_curves <- ggplot()+
      geom_rect(data = variant_temp, aes(xmin = start, xmax = end, ymin = 0,ymax = Inf, fill = Variant), alpha = 0.2) +
      scale_fill_manual(name = "Variant",
                        values = variant.colours) + 
      new_scale_fill() +
      geom_line(data = rates_age, aes(year_week, MortalityRate, group = TargetGroup, colour = TargetGroup), size = 0.7) +
      geom_vline(data = vaccination_start_dates, aes(xintercept = year_week), col="black") +
      facet_wrap(.~ ReportCountry, scales = "free", ncol = 5) +
      scale_colour_manual(name = "Age groups",
                          labels = c("Age 25-49" = "Ages 25 to 49", 
                                     "Age 50-59" = "Ages 50 to 59", 
                                     "Age 60+" = "Ages ≥60"),
                          values = c("Age 25-49" = "#a82d17", 
                                     "Age 50-59" = "#e1a744", 
                                     "Age 60+"= "#1f497d")) + 
      theme_bw() +
      theme(legend.text = element_text(size = 12, colour = "black"),
            legend.title = element_text(size = 12, colour = "black"),
            axis.text = element_text(size = 12, colour = "black"),
            axis.title = element_text(size = 12, colour = "black"),
            strip.background = element_rect(fill="white"),
            element_text(size = 13, colour = "black")) +
      scale_x_discrete(breaks = week_breaks_long, labels = week_labels_long) +
      scale_y_continuous(labels = comma_format(big.mark = ",", accuracy = 1)) +
      labs(subtitle = "Observed Mortality rate per 100,000 population per week and country, with the black vertical line representing progress of vaccination programmes.",
           x = "Week",
           y = "Mortality rate",
           caption = "[1] All references to Kosovo in this document should be understood to be in the context of the United Nations Security Council resolution 1244 (1999).")
    
    
  }
    
  if(measure == "Variant rates by age"){
    

    country_rates <- Expected_cases_all_ages %>% 
      select(ReportCountry, year_week, TargetGroup, Variant, DeathsObserved, DeathsExpected, Denominator) %>% 
      mutate(ObservedMortalityRate = (DeathsObserved / Denominator)*100000,
             ExpectedMortalityRate = (DeathsExpected / Denominator)*100000) %>% 
      mutate(ReportCountry = ifelse(ReportCountry == "Kosovo", "Kosovo[1]", ReportCountry))
    
    vaccination_start_dates <- vaccination_start_dates %>% 
      mutate(ReportCountry = ifelse(ReportCountry == "Kosovo", "Kosovo[1]", ReportCountry))
    
    
    mortality_curves <- ggplot() +
      geom_line(data = subset(country_rates, TargetGroup == "Age 60+"), 
                aes(year_week, ObservedMortalityRate, group = TargetGroup, colour = TargetGroup), size = 0.7) +
      geom_line(data = subset(country_rates, TargetGroup == "Age 60+"), 
                aes(year_week, ExpectedMortalityRate, group = TargetGroup), colour = "black", size = 0.7) +
      geom_vline(data = vaccination_start_dates, 
                 aes(xintercept = year_week, linetype = Dose), col = "black") +
      facet_wrap(.~ ReportCountry, scales = "free_y", ncol = 5) +
      scale_x_discrete(breaks=week_breaks_long, labels=week_labels_long) +
      scale_linetype_manual(name="Vaccination programmes\nprogress",
                            labels=c("start" = "Start of vaccination",
                                     "x10pp1dose" = "10% first doses",
                                     "y10pp2dose" = "10% Second dose",
                                     "z10pp3dose" = "10% additional doses"),
                            values = c("start"="solid",
                                       "x10pp1dose" = "dashed",
                                       "y10pp2dose" = "dashed",
                                       "z10pp3dose" = "dashed")) +
      theme_bw() +
      theme(legend.text = element_text(size = 12, colour = "black"),
            legend.title = element_text(size = 12, colour = "black"),
            axis.text = element_text(size = 12, colour = "black"),
            axis.title = element_text(size = 12, colour = "black"),
            strip.background = element_rect(fill="white"),
            strip.text = element_text(size = 13, colour = "black")) +
      scale_y_continuous(labels = comma_format(big.mark = ",", accuracy = 1)) +
      labs(title = "Mortality rate per 100,000 population per week and country, in context of vaccination programme roll-outs",
           x = "Week",
           y = "Mortality rates",
           caption = "[1] All references to Kosovo in this document should be understood to be in the context of the United Nations Security Council resolution 1244 (1999).")
    

  }
  
  if(measure=="Variant rates all ages"){
    
    Country_positions <- c("Austria", "Belgium", "Croatia", "Cyprus", "Czechia", "Denmark", "Estonia", "Finland",
                           "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Israel", "Italy", "Latvia",
                           "Lithuania", "Luxembourg", "Malta", "Netherlands\n(Kingdom of)", "North Macedonia", "Portugal", 
                           "Republic of Moldova", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland",
                           "Ukraine", "United Kingdom (England)", "United Kingdom (Scotland)", "Kosovo[1]")

    country_rates <- Expected_cases_all_ages %>% 
      select(ReportCountry, year_week, TargetGroup, Variant, DeathsObserved, DeathsExpected, Denominator) %>% 
      mutate(Variant = ifelse(Variant == "Index", "", Variant)) %>% 
      group_by(ReportCountry, year_week, Variant) %>% 
      summarise(ObservedDeaths = sum(DeathsObserved),
                ExpectedDeaths = sum(DeathsExpected),
                denom = sum(unique(Denominator))) %>% 
      ungroup() %>% 
      mutate(ObservedMortalityRate = (ObservedDeaths / denom)*100000,
             ExpectedMortalityRate = (ExpectedDeaths / denom)*100000) %>% 
      pivot_longer(cols = ObservedMortalityRate : ExpectedMortalityRate,
                   names_to = "Mortality Rate",
                   values_to = "Rate") %>% 
      left_join(variant_waves) %>% 
      select(ReportCountry, year_week, Variant, `Mortality Rate`, Rate, start, end) %>% 
      mutate(ReportCountry = ifelse(ReportCountry == "Kosovo", "Kosovo[1]", ReportCountry),
             ReportCountry = ifelse(ReportCountry == "Netherlands", "Netherlands\n(Kingdom of)", ReportCountry),
             ReportCountry = factor(ReportCountry, levels = Country_positions)) 
    
    vaccination_start_dates <- vaccination_start_dates %>% 
      mutate(ReportCountry = ifelse(ReportCountry == "Kosovo", "Kosovo[1]", ReportCountry),
             ReportCountry = ifelse(ReportCountry == "Netherlands", "Netherlands\n(Kingdom of)", ReportCountry),
             ReportCountry = factor(ReportCountry, levels = Country_positions))
    
    
    variant_timings <- country_rates %>% 
      left_join(variant_waves) %>% 
      distinct() %>% 
      group_by(ReportCountry) %>% 
      mutate(Variant_position = Rate +10) %>% 
      ungroup() %>% 
      group_by(ReportCountry) %>% 
      top_n(1, Rate) %>% 
      ungroup() %>% 
      select(ReportCountry, year_week, Variant, start, end, Variant_position) %>% 
      distinct() %>% 
      mutate(ReportCountry = ifelse(ReportCountry == "Kosovo", "Kosovo[1]", ReportCountry),
             ReportCountry = ifelse(ReportCountry == "Netherlands", "Netherlands\n(Kingdom of)", ReportCountry),
             ReportCountry = factor(ReportCountry, levels = Country_positions))
    
    
    
    country_rates <- country_rates %>% 
      full_join(variant_timings) %>% 
      group_by(ReportCountry) %>% 
      fill(Variant_position, .direction = "updown") %>% 
      mutate(Variant_name_position = Variant_position * 1.2) %>% 
      ungroup() %>% 
      left_join(vaccination_start_dates) %>% 
      distinct() %>% 
      group_by(ReportCountry, Variant) %>% 
      mutate(Variant_name = ifelse(first(Variant), Variant, NA)) %>% 
      ungroup() 
    
    mortality_curves <- ggplot() +
      geom_segment(data = country_rates, 
                   aes(x = start, xend = end, y = Variant_position, yend = Variant_position)) +
      geom_segment(data = country_rates, 
                   aes(x = start, xend = start, y = Variant_position-2, yend = Variant_position+2)) +
      geom_text(data = country_rates, 
                aes(x = end, y = Variant_name_position, label = Variant), hjust = 1.1, size = 4) +
      geom_line(data = country_rates, 
                aes(x = year_week, y = Rate, group = `Mortality Rate`, colour = `Mortality Rate`), linewidth = 0.8) +
      geom_segment(data = subset(country_rates, !is.na(Dose)), 
                   aes(x = year_week, xend = year_week, y = 0, yend = Variant_position-2, linetype = Dose)) +
      facet_wrap(.~ ReportCountry, scales = "free_y", ncol = 4) +
      scale_x_discrete(breaks = week_breaks_long, 
                       labels = week_labels_long) +
      scale_colour_manual(values = c("ObservedMortalityRate" = "#003C50",
                                     "ExpectedMortalityRate" = "#D6852B"),
                          labels = c("ObservedMortalityRate" = "Observed",
                                     "ExpectedMortalityRate" = "Expected")) +
      scale_linetype_manual(name = "Vaccination programmes\nprogress",
                            labels = c("start" = "Start of vaccination",
                                     "x10pp1dose" = "10% first doses",
                                     "y10pp2dose" = "10% second doses",
                                     "z10pp3dose" = "10% first booster",
                                     "z10pp4dose" = "10% second booster",
                                     "z10pp5dose" = "10% third booster"),
                            values = c("start" = "solid",
                                       "x10pp1dose" = "longdash",
                                       "y10pp2dose" = "dashed",
                                       "z10pp3dose" = "dotdash",
                                       "z10pp4dose" = "dotted",
                                       "z10pp5dose" = "dotted")) +
      theme_bw() +
      theme(legend.text = element_text(size = 12, colour = "black"),
            legend.title = element_text(size = 12, colour = "black"),
            legend.position = "bottom",
            axis.text = element_text(size = 12, colour = "black"),
            axis.title = element_text(size = 12, colour = "black"),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            strip.background = element_rect(fill = "white"),
            strip.text = element_text(size = 12, colour = "black")) +
      scale_y_continuous(labels = comma_format(big.mark = ",", accuracy = 1),
                         limits = c(0, NA), expand = expansion(mult = c(0, 0.2))) +
      labs(title = "Mortality rate per 100,000 population per week and country, in context of vaccination programme roll-outs",
           x = "Week",
           y = "Mortality rates",
           caption = "[1] All references to Kosovo in this document should be understood to be in the context of the United Nations Security Council resolution 1244 (1999).")
   }
  
  if(to.save == TRUE){
    ggsave("Outputs/WHO_EURO_Lives_saved_COVID_vaccines_Supp_Figure_1.png", dpi = 300, height = 15, width = 12)
  }
  
  return(mortality_curves)
  
}
