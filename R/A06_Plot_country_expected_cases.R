# Script to plot observed and prevented deaths per country
# Margaux Mesle - meslem@who.int
# June 2021

# Plot Figure 2:
# Number of observed and expected mortality counts, as well as vaccination uptake (VU) per week


plot.expected.mortality <- function(Expected_cases_all_ages, variant_waves, time_period) {
  

  # Reshape data to have legend
  expected_plot <- Expected_cases_all_ages %>%
    select(ReportCountry, year_week, DeathsObserved, DeathsExpected) %>%
    group_by(ReportCountry, year_week) %>% 
    summarise(DeathsObserved = sum(DeathsObserved, na.rm=TRUE),
              DeathsExpected = sum(DeathsExpected, na.rm=TRUE)) %>% 
    ungroup() %>% 
    mutate(DeathsExpected = round(DeathsExpected)) %>%
    distinct() %>%
    pivot_longer(cols = c("DeathsObserved","DeathsExpected"),
                 names_to = "Measure",
                 values_to = "Mortality.count") %>%
    mutate(ReportCountry = ifelse(ReportCountry == "United Kingdom (Scotland)", "United Kingdom\n(Scotland)", ReportCountry),
           ReportCountry = ifelse(ReportCountry == "United Kingdom (England)", "United Kingdom\n(England)", ReportCountry),
           ReportCountry = ifelse(ReportCountry == "United Kingdom (Wales)", "United Kingdom\n(Wales)", ReportCountry),
           ReportCountry = ifelse(ReportCountry == "Kosovo", "Kosovo*", ReportCountry)) %>% 
    distinct() %>% 
    filter(!is.na(Mortality.count))


  vax_start_dates <- find.start.vaccination.dates(vax_clean)
  vax_start_dates <- vax_start_dates %>%
    mutate(ReportCountry = ifelse(ReportCountry == "United Kingdom (Scotland)", "United Kingdom\n(Scotland)", ReportCountry),
           ReportCountry = ifelse(ReportCountry == "United Kingdom (England)", "United Kingdom\n(England)", ReportCountry),
           ReportCountry = ifelse(ReportCountry == "United Kingdom (Wales)", "United Kingdom\n(Wales)", ReportCountry),
           ReportCountry = ifelse(ReportCountry == "Kosovo", "Kosovo*", ReportCountry))
  
  variant_waves <- variant_waves %>%
    mutate(ReportCountry = ifelse(ReportCountry == "United Kingdom (Scotland)", "United Kingdom\n(Scotland)", ReportCountry),
           ReportCountry = ifelse(ReportCountry == "United Kingdom (England)", "United Kingdom\n(England)", ReportCountry),
           ReportCountry = ifelse(ReportCountry == "United Kingdom (Wales)", "United Kingdom\n(Wales)", ReportCountry),
           ReportCountry = ifelse(ReportCountry == "Kosovo", "Kosovo*", ReportCountry)) %>%
    filter(ReportCountry %in% unique(countries_included$CountryName)) #%>% 
    #rename(CountryName=ReportCountry)
  
  
  if(time_period == "pandemic"){
    
    deaths_plot <- ggplot(data = expected_plot ) +
      geom_rect(data = variant_waves, aes(xmin = start, xmax = end, ymin = 0, ymax = Inf, fill = Variant), alpha = 0.3) +
      scale_fill_manual(name = "Variant",
                        values = variant.colours) + 
      guides(fill = guide_legend(nrow = 2)) +
      new_scale_fill() +
      geom_line(aes(year_week, Mortality.count, group = Measure, colour = Measure)) +
      geom_vline(data = vax_start_dates, aes(xintercept = year_week, col = Dose, linetype = Dose)) +
      facet_wrap(.~ report_country, scales = "free_y", ncol=5) +
      scale_color_manual(name = "",
                         values = c("#003C50", "#558ed5", "#c3d69b"),
                         limits = c("DeathsExpected", "DeathsObserved"),
                         labels = c("DeathsExpected" = "Mortality without\nvaccination (complete series)", 
                                  "DeathsObserved" = "Observed mortality")) +
      scale_linetype_manual(name = "Vaccine doses\nadministered",
                            labels = c("start" = "Start of vaccination",
                                     "x10pp1dose" = "10% first doses",
                                     "y10pp2dose" = "10% complete series",
                                     "z10pp3dose" = "10% 1 additional dose",
                                     "z10pp4dose" = "10% 2 additional doses"),
                            values = c("start" = "solid", 
                                       "x10pp1dose" = "longdash", 
                                       "y10pp2dose" = "dashed", 
                                       "z10pp3dose" = "dotdash", 
                                       "z10pp4dose" = "dotdash")) +
      scale_x_discrete(breaks = week_breaks_long, labels = week_labels_long) +
      scale_y_continuous(labels = comma_format(big.mark = ",", accuracy = 1)) +
      theme_bw()  +
      theme(legend.text = element_text(size = text_size, colour = "black"),
            legend.title = element_text(size = text_size, colour = "black"),
            legend.position = "bottom",
            axis.text = element_text(size = text_size, colour = "black"),
            axis.title = element_text(size = text_size, colour = "black"),
            strip.background = element_rect(fill = "white")) +
      guides(colour = guide_legend(nrow = 2),
             linetype = guide_legend(nrow = 2),
             fill = guide_legend(nrow = 2)) +
      labs(x = "Week",
           y = "Number of deaths")
    
    #if(length(age.group==1)) {
    #  deaths_plot <- deaths_plot +
    #    labs(subtitle = "2. Observed and averted mortality in population aged 60 years and over ")
    #} else  if(length(age.group>1)) {
    deaths_plot <- deaths_plot +
        labs(#subtitle = paste0("Observed and averted mortality in age group: ", age.group),
             caption="* in accordance with Security Council resolution 1244 (1999).")
      
    #}
  
  
  }
  
  if(time_period == "vaccinations"){
    
    
    if(age.group == "All"){
      deaths_temp <- deaths_age %>% 
        group_by(ReportCountry, year_week) %>% 
        summarise(Mortality.count = sum(DeathsObserved, na.rm = T)) %>% 
        ungroup() %>% 
        mutate(ReportCountry = ifelse(ReportCountry == "United Kingdom (Scotland)", "United Kingdom\n(Scotland)", ReportCountry),
               ReportCountry = ifelse(ReportCountry == "United Kingdom (England)", "United Kingdom\n(England)", ReportCountry),
               ReportCountry = ifelse(ReportCountry == "United Kingdom (Wales)", "United Kingdom\n(Wales)", ReportCountry),
               ReportCountry = ifelse(ReportCountry == "Kosovo", "Kosovo*", ReportCountry)) %>%
        #filter(report_country %in% cases.plot$report_country) %>% 
        distinct()
      
    } else {
    deaths_temp <- deaths_age %>%
      filter(str_detect(year_week, "^2020")) %>%
      filter(!year_week %in% c("2020-51", "2020-52", "2020-53")) %>%
      mutate(Measure = "DeathsObserved") %>%
      rename(Mortality.count = DeathsObserved) %>%
      select(-TargetGroup) %>%
      mutate(ReportCountry = ifelse(ReportCountry == "United Kingdom (Scotland)", "United Kingdom\n(Scotland)", ReportCountry),
             ReportCountry = ifelse(ReportCountry == "United Kingdom (England)", "United Kingdom\n(England)", ReportCountry),
             ReportCountry = ifelse(ReportCountry == "United Kingdom (Wales)", "United Kingdom\n(Wales)", ReportCountry),
             ReportCountry = ifelse(ReportCountry == "Kosovo", "Kosovo*", ReportCountry)) %>%
      #filter(report_country %in% cases.plot$report_country) %>% 
      distinct()
      
    }
    
    
    cases.plot.long <- full_join(expected_plot, deaths_temp) %>%
      filter(year_week %in% reporting.weeks) %>% 
      distinct()
    
    Cases <- ggplot() +
       geom_rect(data = variant_waves, aes(xmin = start, xmax = end, ymin = 0, ymax = Inf, fill = Variant), alpha = 0.3) +
       scale_fill_manual(name = "Variant",
                         values = variant.colours) + 
      guides(fill = guide_legend(nrow = 2)) +
      new_scale_fill() +
      geom_line(data = cases.plot.long, aes(x = year_week, y = Mortality.count, group = Measure, colour = Measure)) +
      geom_vline(data = vax_start_dates, aes(xintercept = year_week, col = Dose, linetype = Dose)) +
      facet_wrap(.~ ReportCountry, scales = "free_y", ncol = 5) +
      scale_color_manual(name = "Mortality",
                         values = c("DeathsExpected" = "#D6852B", 
                                    "DeathsObserved" = "#003C50"),
                         labels = c("DeathsExpected" = "Expected", 
                                    "DeathsObserved" = "Observed")) +
      scale_linetype_manual(name = "Vaccine doses\nadministered",
                            labels = c("start" = "Start of vaccination",
                                       "x10pp1dose" = "10% first doses",
                                       "y10pp2dose"  ="10% full coverage",
                                       "z10pp3dose" = "10% 1 additional dose",
                                       "z10pp4dose" = "10% 2 additional doses",
                                       "z10pp5dose" = "10% 3 additional doses"),
                            values = c("start" = "solid",  
                                       "x10pp1dose" = "longdash", 
                                       "y10pp2dose" = "dashed", 
                                       "z10pp3dose" = "dotdash", 
                                       "z10pp4dose" = "dotdash",
                                       "z10pp5dose" = "dotdash")) +
      scale_x_discrete(breaks = week_breaks, labels = week_labels) +
      scale_y_continuous(labels = comma_format(big.mark = ",", accuracy = 1)) +
      theme_bw()  +
      theme(legend.text = element_text(size = text_size, colour = "black"),
            legend.title = element_text(size = text_size, colour = "black"),
            legend.position = "bottom",
            axis.text = element_text(size = text_size, colour = "black"),
            axis.title = element_text(size = text_size, colour = "black"),
            strip.background = element_rect(fill = "white")) +
      guides(colour = guide_legend(nrow = 2),
             linetype = guide_legend(nrow = 2),
             fill = guide_legend(nrow = 2)) +
      labs(x = "Week",
           y = "Number of deaths",
           caption = "*all references to Kosovo in this document should be understood to be\nin the context of the United Nations Security Council resolution 1244 (1999)")
    
 
  }
    
  return(Cases)
}
