# Script to plot vaccination coverage by country
# Margaux Mesle - meslem@who.int
# June 2021

# For reference: 
    # Figure A: Vaccination by age group (3 panels) and dose
    # Figure B: Vaccination by age group (3 panels) and dose: min, mean and max coverage
    # Figure C: Vaccination by age group (3 panels) and dose - by country
    # Figure D: Number of vaccination doses administered by age group in light of circulating VOCs


plot.country.vaccination.curves <- function(vax_clean, 
                                            variant_waves, 
                                            variant_waves_region, 
                                            vaccine.weeks, 
                                            reporting.weeks,
                                            figure) {
  
  legend.order <- c("pcFirstDose", "pcSecondDose", "pcDoseAdditional1", "pcDoseAdditional2", "pcDoseAdditional3")

  if(figure == "A") {
  # Total vaccination coverage by age group across the Region

  vax_data_plot_A <- vax_clean %>%
    group_by(TargetGroup, year_week) %>%
    summarise(FirstDose = sum(FirstDose),
              SecondDose = sum(SecondDose),
              DoseAdditional1 = sum(DoseAdditional1),
              DoseAdditional2 = sum(DoseAdditional2),
              denominator = sum(unique(last(Denominator)))) %>%
    ungroup() %>%
    group_by(TargetGroup) %>%
    mutate(nFirstDose = cumsum(FirstDose),
           nSecondDose = cumsum(SecondDose),
           nDoseAdditional1 = cumsum(DoseAdditional1),
           nDoseAdditional2 = cumsum(DoseAdditional2),
           pcFirstDose = round((cumsum(FirstDose)/denominator)*100),
           pcSecondDose = round((cumsum(SecondDose)/denominator)*100),
           pcDoseAdditional1 = round((cumsum(DoseAdditional1)/denominator)*100),
           pcDoseAdditional2 = round((cumsum(DoseAdditional2)/denominator)*100)) %>%
    ungroup() %>%
    group_by(TargetGroup, year_week) %>%
    mutate(pcFirstDose = ifelse(pcFirstDose>100, 100, pcFirstDose),
           pcSecondDose = ifelse(pcSecondDose>100, 100, pcSecondDose),
           pcDoseAdditional1 = ifelse(pcDoseAdditional1>100, 100, pcDoseAdditional1),
           pcDoseAdditional2 = ifelse(pcDoseAdditional2>100, 100, pcDoseAdditional2)) %>%
    ungroup() %>% 
    # IF artificial spikes are present, remove them
    # mutate(pcFirstDose = ifelse(pcFirstDose == 100 & year_week=="2021-53" & targetgroup=="Age25_49", 80, pcFirstDose),
    #        pcFirstDose = ifelse(pcFirstDose == 100 & year_week=="2021-53" & targetgroup=="Age50_59", 88, pcFirstDose),
    #        pcFirstDose = ifelse(pcFirstDose == 100 & year_week=="2021-53" & targetgroup=="Age60+", 88, pcFirstDose),
    #        pcSecondDose = ifelse(pcSecondDose == 100 & year_week=="2021-53" & targetgroup=="Age25_49", 70, pcSecondDose),
    #        pcSecondDose = ifelse(pcSecondDose == 100 & year_week=="2021-53" & targetgroup=="Age50_59", 79, pcSecondDose),
    #        pcSecondDose = ifelse(pcSecondDose == 100 & year_week=="2021-53" & targetgroup=="Age60+", 83, pcSecondDose),
    #        pcDoseAdditional1 = ifelse(pcDoseAdditional1 == 100 & year_week=="2021-53" & targetgroup=="Age25_49", 23, pcDoseAdditional1),
    #        pcDoseAdditional1 = ifelse(pcDoseAdditional1 == 100 & year_week=="2021-53" & targetgroup=="Age50_59", 45, pcDoseAdditional1),
    #        pcDoseAdditional1 = ifelse(pcDoseAdditional1 == 100 & year_week=="2021-53" & targetgroup=="Age60+", 60, pcDoseAdditional1)) %>% 
    pivot_longer(cols = "pcFirstDose":"pcDoseAdditional2",
                 names_to = "Dosage",
                 values_to = "Percentage") %>% 
    filter(TargetGroup %in% c("Age25_49", "Age50_59", "Age60+"))
  
    
   curve.A <- ggplot(data = subset(vax_data_plot_A, Dosage == "pcFirstDose")) +
     geom_line(aes(x = year_week, y = Percentage, group = targetgroup, col = targetgroup)) +
     geom_hline(yintercept = c(60, 95), col = "black", size = 0.3) +
     scale_colour_manual(name = "Age groups",
                         labels = c("Age25_49" = "25 - 49", 
                                    "Age50_59" = "50 - 59",
                                    "Age60+" = "60+"),
                         values = c("Age25_49" = "#a82d17", 
                                   "Age50_59" = "#e1a744", 
                                   "Age60+"=  "#1f497d")) + 
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25)) +
      scale_x_discrete(breaks = week_breaks_long, labels = week_labels_long) +
      theme_bw()  +
      theme(legend.text= element_text(size = text_size, colour = "black"),
            legend.title = element_text(size = text_size, colour = "black"),
            legend.position = "none",
            axis.text = element_text(size = text_size, colour = "black"),
            axis.title = element_text(size = text_size, colour = "black"),
            strip.background = element_rect(fill = "white")) +
      labs(subtitle = "I) ",
           x = "Week",
           y = "Vaccination Uptake %")


  curve.B <- ggplot(data = subset(vax_data_plot_A, Dosage == "pcSecondDose")) +
    geom_line(aes(x = year_week, y = Percentage, group = targetgroup, col = targetgroup)) +
    geom_hline(yintercept = c(60, 95), col = "black", size = 0.3) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25)) +
    scale_x_discrete(breaks = week_breaks_long, labels = week_labels_long) +
    scale_colour_manual(name = "Age groups",
                        labels = c("Age25_49" = "25 - 49", 
                                  "Age50_59" = "50 - 59",
                                  "Age60+" = "60+"),
                        values = c("Age25_49" = "#a82d17", 
                                   "Age50_59" = "#e1a744", 
                                   "Age60+"=  "#1f497d")) + 
    theme_bw()  +
    theme(legend.text= element_text(size = text_size, colour = "black"),
          legend.title = element_text(size = text_size, colour = "black"),
          legend.position = "bottom",
          axis.text = element_text(size = text_size, colour = "black"),
          axis.title = element_text(size = text_size, colour = "black"),
          strip.background = element_rect(fill = "white")) +
    labs(subtitle = "II) ",
         x = "Week",
         y = "")
  
  curve.C <- ggplot(data = subset(vax_data_plot_A, Dosage == "pcDoseAdditional1")) +
    geom_line(aes(x = year_week, y = Percentage, group = targetgroup, col = targetgroup)) +
    geom_hline(yintercept = c(60, 95), col = "black", size = 0.3) +
    scale_y_continuous(limits = c(0,100), breaks = seq(0, 100, 25)) +
    scale_x_discrete(breaks = week_breaks_long, labels = week_labels_long) +
    scale_colour_manual(name = "Age groups",
                        labels = c("Age25_49" = "25 - 49", 
                                   "Age50_59" = "50 - 59",
                                   "Age60+" = "60+"),
                        values = c("Age25_49" = "#a82d17", 
                                   "Age50_59" = "#e1a744", 
                                   "Age60+"=  "#1f497d")) + 
    theme_bw()  +
    theme(legend.text= element_text(size = text_size, colour = "black"),
          legend.title = element_text(size = text_size, colour = "black"),
          legend.position = "none",
          axis.text = element_text(size = text_size, colour = "black"),
          axis.title = element_text(size = text_size, colour = "black"),
          strip.background = element_rect(fill = "white")) +
    labs(subtitle = "III) ",
         x = "Week",
         y = "")
  
  curve.D <- ggplot(data = subset(vax_data_plot_A, Dosage == "pcDoseAdditional2")) +
    geom_line(aes(x = year_week, y = Percentage, group = targetgroup, col = targetgroup)) +
    geom_hline(yintercept = c(60, 95), col = "black", size = 0.3) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25)) +
    scale_x_discrete(breaks = week_breaks_long, labels = week_labels_long) +
    scale_colour_manual(name = "Age groups",
                        labels = c("Age25_49" = "25 - 49", 
                                   "Age50_59" = "50 - 59",
                                   "Age60+" = "60+"),
                        values=  c("Age25_49" = "#a82d17", 
                                   "Age50_59" = "#e1a744", 
                                   "Age60+"=  "#1f497d")) + 
    theme_bw()  +
    theme(legend.text= element_text(size = text_size, colour = "black"),
          legend.title = element_text(size = text_size, colour = "black"),
          legend.position = "none",
          axis.text = element_text(size = text_size, colour = "black"),
          axis.title = element_text(size = text_size, colour = "black"),
          strip.background = element_rect(fill = "white")) +
    labs(subtitle = "IV) ",
         x = "Week",
         y = "")
  
  vaccination_curves <- ggarrange(curve.A, curve.B, curve.C, curve.D, nrow=1, ncol=4, common.legend = T, legend = "bottom", align = "h")
  
  return(vaccination_curves)
  } 

  if(figure == "C") {
    # Total vaccination coverage by age group across the Region by country
    
    vax_data_plot_C <- vax_clean %>%
      filter(year_week %in% vaccine.weeks$DateUsedForStatisticsISO) %>% 
      group_by(report_country, targetgroup, year_week) %>%
      summarise(FirstDose = sum(FirstDose),
                SecondDose = sum(SecondDose),
                DoseAdditional1 = sum(DoseAdditional1),
                denominator = sum(unique(denominator))) %>%
      ungroup() %>%
      group_by(report_country, targetgroup) %>%
      mutate(nFirstDose = cumsum(FirstDose),
             nSecondDose = cumsum(SecondDose),
             nDoseAdditional1 = cumsum(DoseAdditional1),
             pcFirstDose = round((cumsum(FirstDose)/denominator)*100),
             pcSecondDose = round((cumsum(SecondDose)/denominator)*100),
             pcDoseAdditional1 = round((cumsum(DoseAdditional1)/denominator)*100)) %>%
      ungroup() %>%
      group_by(report_country, targetgroup, year_week) %>%
      mutate(pcFirstDose = ifelse(pcFirstDose>100, 100, pcFirstDose),
             pcSecondDose = ifelse(pcSecondDose>100, 100, pcSecondDose),
             pcDoseAdditional1 = ifelse(pcDoseAdditional1>100, 100, pcDoseAdditional1)) %>%
      ungroup() %>% 
      mutate(report_country = ifelse(report_country == "United Kingdom (England)", "United Kingdom\n(England)", report_country),
             report_country = ifelse(report_country == "United Kingdom (Scotland)", "United Kingdom\n(Scotland)", report_country),
             report_country = ifelse(report_country == "United Kingdom (Wales)", "United Kingdom\n(Wales)", report_country))
    
    variant_waves_cnty <- variant_waves %>% 
      mutate(report_country = ifelse(report_country == "United Kingdom (England)", "United Kingdom\n(England)", report_country),
             report_country = ifelse(report_country == "United Kingdom (Scotland)", "United Kingdom\n(Scotland)", report_country),
             report_country = ifelse(report_country == "United Kingdom (Wales)", "United Kingdom\n(Wales)", report_country))
    
    
    
    curve.A <- ggplot(data = subset(vax_data_plot_C, targetgroup == "Age25_49")) +
      geom_line(aes(x = year_week, y = pcFirstDose, group = 1, col = "#a63022"), size = 0.7) +
      geom_line(aes(x = year_week, y = pcSecondDose, group = 1, col = "#e6b45c"), size = 0.7)+
      geom_line(aes(x = year_week, y = pcDoseAdditional1, group = 1, col = "#1f497d"), size = 0.7) +
      geom_hline(yintercept = c(60, 95), col="black", size = 0.3) +
      facet_wrap(.~ report_country, ncol = 5) +
      scale_colour_manual(name = "Dosage",
                          values = colours, 
                          labels = c("#a63022" = "First dose", 
                                     "#e6b45c" = "Full coverage",
                                     "#1f497d" = "Additional dose"),
                          breaks = c("#a63022", "#e6b45c", "#1f497d")) +
      theme_bw()  +
      theme(legend.text = element_text(size = text_size, colour = "black"),
            legend.title = element_text(size = text_size, colour = "black"),
            legend.position = "none",
            axis.text = element_text(size = text_size, colour = "black"),
            axis.title = element_text(size = text_size, colour = "black"),
            strip.background = element_rect(fill = "white")) +
      scale_y_continuous(limits = c(0, 100), 
                         breaks = seq(0, 100, 25), 
                         labels = scales::number_format(accuracy = 1), decimal.mark = ',') +
      scale_x_discrete(breaks = week_breaks_long, 
                       labels = week_labels_long) +
      labs(subtitle = "A) ",
           x = "Week",
           y = "Vaccination Uptake %")
    
    
    
    curve.B <- ggplot(data = subset(vax_data_plot_C, targetgroup == "Age50_59")) +
      geom_line(aes(x = year_week, y = pcFirstDose, group = 1, col = "#a63022"), size = 0.7) +
      geom_line(aes(x = year_week, y = pcSecondDose, group = 1, col = "#e6b45c"), size = 0.7)+
      geom_line(aes(x = year_week, y = pcDoseAdditional1, group = 1, col = "#1f497d"), size = 0.7) +
      geom_hline(yintercept = c(60, 95), col = "black", size = 0.3) +
      facet_wrap(.~ report_country, ncol = 5) +
      scale_colour_manual(name = "Dosage",
                          values = colours, 
                          labels = c("#a63022" = "First dose", 
                                     "#e6b45c" = "Full coverage",
                                     "#1f497d" = "Additional dose"),
                          breaks=c("#a63022", "#e6b45c", "#1f497d")) +
      theme_bw()  +
      theme(legend.text = element_text(size = text_size, colour = "black"),
            legend.title = element_text(size = text_size, colour = "black"),
            legend.position = "bottom",
            axis.text = element_text(size = text_size, colour = "black"),
            axis.title = element_text(size = text_size, colour = "black"),
            strip.background = element_rect(fill = "white")) +
      scale_y_continuous(limits = c(0, 100), 
                         breaks = seq(0, 100, 25), 
                         labels = scales::number_format(accuracy = 1), decimal.mark = ',') +
      scale_x_discrete(breaks = week_breaks_long, 
                       labels = week_labels_long) +
      labs(subtitle = "B) ",
           x = "Week",
           y = "")
    
    curve.C <- ggplot(data = subset(vax_data_plot_C, targetgroup == "Age60+")) +
      geom_line(aes(x = year_week, y = pcFirstDose, group = 1, col = "#a63022"), size = 0.7) +
      geom_line(aes(x = year_week, y = pcSecondDose, group = 1, col = "#e6b45c"), size = 0.7)+
      geom_line(aes(x = year_week, y = pcDoseAdditional1, group = 1, col = "#1f497d"), size = 0.7) +
      geom_hline(yintercept = c(60, 95), col = "black", size = 0.3) +
      facet_wrap(.~ report_country, ncol = 5) +
      scale_colour_manual(name = "Dosage",
                          values = colours, 
                          labels = c("#a63022" = "First dose", 
                                     "#e6b45c" = "Full coverage",
                                     "#1f497d" = "Additional dose"),
                          breaks = c("#a63022", "#e6b45c", "#1f497d")) +
      theme_bw()  +
      theme(legend.text = element_text(size = text_size, colour = "black"),
            legend.title = element_text(size = text_size, colour = "black"),
            legend.position = "none",
            axis.text = element_text(size = text_size, colour = "black"),
            axis.title = element_text(size = text_size, colour = "black"),
            strip.background = element_rect(fill = "white")) +
      scale_y_continuous(limits = c(0, 100), 
                         breaks = seq(0, 100, 25), 
                         labels = scales::number_format(accuracy = 1), decimal.mark = ',') +
      scale_x_discrete(breaks = week_breaks_long, 
                       labels = week_labels_long) +
      labs(subtitle = "C) ",
           x = "Week",
           y = "")
    
    vaccination_curves <- ggarrange(curve.A, curve.B, curve.C, nrow=1, ncol=3, common.legend = T, legend = "bottom", align = "h")
    
    return(vaccination_curves)
  } 
  
  if(figure == "D") {
    # Total vaccination coverage by age group across the Region
 
    vax_data_plot_D <- vax_clean %>%
      filter(year_week %in% vaccine.weeks$DateUsedForStatisticsISO) %>% 
      group_by(targetgroup, year_week) %>%
      summarise(FirstDose = sum(FirstDose),
                SecondDose = sum(SecondDose),
                DoseAdditional1 = sum(DoseAdditional1),
                DoseAdditional2 = sum(DoseAdditional2),
                denominator = sum(unique(denominator)),
                RateFirstDose = round((FirstDose/denominator*100000)),
                RateSecondDose = round((SecondDose/denominator*100000)),
                RateDoseAdditional1 = round((DoseAdditional1/denominator*100000)),
                RateDoseAdditional2 = round((DoseAdditional2/denominator*100000))) %>%
      ungroup() %>% 
      pivot_longer(cols = "RateFirstDose":"RateDoseAdditional2",
                   names_to = "Dosage",
                   values_to = "Rate")
    

    curve.A <- ggplot(data = subset(vax_data_plot_D, targetgroup == "Age25_49")) +
      geom_rect(data = variant_waves_region, 
                aes(xmin = start, xmax = end, ymin = 0, ymax = Inf, fill = Variant), alpha = .3) +
      scale_fill_manual(name = "Variant",
                        values= variant.colours) + 
      geom_line(aes(x = year_week, y = Rate, group = Dosage, col = Dosage), size = 0.7) +
      scale_colour_manual(name = "Dosage",
                          values = c("RateFirstDose" = "#a82d17", 
                                     "RateSecondDose" = "#e1a744", 
                                     "RateDoseAdditional1" = "#1f497d",
                                     "RateDoseAdditional2" = "#4999ab"), 
                          labels = c("RateFirstDose" = "First dose", 
                                     "RateSecondDose" = "Full coverage",
                                     "RateDoseAdditional1" = "First additional dose",
                                     "RateDoseAdditional1" = "Second additional dose"),
                          breaks = c("RateFirstDose", "RateSecondDose", "RateDoseAdditional1", "RateDoseAdditional2")) +
      theme_bw()  +
      theme(legend.text = element_text(size = text_size, colour = "black"),
            legend.title = element_text(size = text_size, colour = "black"),
            legend.position = "none",
            axis.text = element_text(size = text_size, colour = "black"),
            axis.title = element_text(size = text_size, colour = "black"),
            strip.background=element_rect(fill = "white")) +
      scale_x_discrete(breaks = week_breaks_long, 
                       labels = week_labels_long) +
      scale_y_continuous(labels = scales::number_format(accuracy = 1), decimal.mark = ',') +
      labs(subtitle = "A) ",
           x = "Week",
           y = "Rate")
    
    
    
    curve.B <- ggplot(data = subset(vax_data_plot_D, targetgroup == "Age50_59")) +
      geom_rect(data = variant_waves_region, 
                aes(xmin = start, xmax = end, ymin = 0, ymax = Inf, fill = Variant), alpha = .5) +
      scale_fill_manual(name = "Variant",
                        values = c("Alpha" = "#9CC65A", 
                                   "Delta" = "#C9D971",
                                   "Omicron" = "#E7E7B9")) + 
      geom_line(aes(x = year_week, y = Rate, group = Dosage, col = Dosage), size = 0.7) +
      scale_colour_manual(name = "Dosage",
                          values = c("RateFirstDose" = "#a82d17", 
                                     "RateSecondDose" = "#e1a744", 
                                     "RateDoseAdditional1" = "#1f497d",
                                     "RateDoseAdditional2" = "#4999ab"), 
                          labels = c("RateFirstDose"="First dose", 
                                     "RateSecondDose"="Full coverage",
                                     "RateDoseAdditional1"="First additional dose",
                                     "RateDoseAdditional1"="Second additional dose"),
                          breaks = c("RateFirstDose", "RateSecondDose", "RateDoseAdditional1", "RateDoseAdditional2")) +
      theme_bw()  +
      theme(legend.text = element_text(size = text_size, colour = "black"),
            legend.title = element_text(size = text_size, colour = "black"),
            legend.position = "bottom",
            axis.text = element_text(size = text_size, colour = "black"),
            axis.title = element_text(size = text_size, colour = "black"),
            strip.background = element_rect(fill = "white")) +
      scale_x_discrete(breaks = week_breaks_long, labels = week_labels_long) +
      scale_y_continuous(labels = scales::number_format(accuracy = 1), decimal.mark = ',') +
      labs(subtitle = "B) ",
           x = "Week",
           y = "")
    
    curve.C <- ggplot(data = subset(vax_data_plot_D, targetgroup == "Age60+")) +
      geom_rect(data = variant_waves_region, 
                aes(xmin = start, xmax = end, ymin = 0, ymax = Inf, fill = Variant), alpha = .5) +
      scale_fill_manual(name = "Variant",
                        values = variant.colours) + 
      geom_line(aes(x = year_week, y = Rate, group = Dosage, col = Dosage), size = 0.7) +
      scale_colour_manual(name = "Dosage",
                          values = c("RateFirstDose" = "#a82d17", 
                                     "RateSecondDose" = "#e1a744", 
                                     "RateDoseAdditional1" = "#1f497d",
                                     "RateDoseAdditional2" = "#4999ab"), 
                          labels = c("RateFirstDose" = "First dose", 
                                   "RateSecondDose" = "Full coverage",
                                   "RateDoseAdditional1" = "First additional dose",
                                   "RateDoseAdditional1" = "Second additional dose"),
                          breaks = c("RateFirstDose", "RateSecondDose", "RateDoseAdditional1", "RateDoseAdditional2")) +
      theme_bw()  +
      theme(legend.text = element_text(size = text_size, colour = "black"),
            legend.title = element_text(size = text_size, colour = "black"),
            legend.position = "none",
            axis.text = element_text(size = text_size, colour = "black"),
            axis.title = element_text(size = text_size, colour = "black"),
            strip.background = element_rect(fill = "white")) +
      scale_x_discrete(breaks = week_breaks_long, labels = week_labels_long) +
      scale_y_continuous(labels = scales::number_format(accuracy = 1), decimal.mark = ',') +
      labs(subtitle = "C) ",
           x = "Week",
           y = "")
    
    vaccination_curves <- ggarrange(curve.A, curve.B, curve.C, nrow=1, ncol=3, common.legend = T, legend = "bottom", align = "h")
    
    return(vaccination_curves)
  } 
  
  if(figure == "E") {
    # Total vaccination coverage by age group across the Region
    
    vax_data_plot_E <- vax_clean %>%
      filter(year_week %in% reporting.weeks) %>% 
      group_by(TargetGroup, year_week) %>%
      summarise(FirstDose = sum(FirstDose),
                SecondDose = sum(SecondDose),
                DoseAdditional1 = sum(DoseAdditional1),
                DoseAdditional2 = sum(DoseAdditional2),
                denominator = sum(unique(last(Denominator))),
                RateFirstDose = round((FirstDose/denominator*100000)),
                RateSecondDose = round((SecondDose/denominator*100000)),
                RateDoseAdditional1 = round((DoseAdditional1/denominator*100000)),
                RateDoseAdditional2 = round((DoseAdditional2/denominator*100000))) %>%
      ungroup() %>% 
      pivot_longer(cols = "RateFirstDose":"RateDoseAdditional2",
                   names_to = "Dosage",
                   values_to = "Rate") 
    
    variant_waves_region <- variant_waves_region %>% filter(!Variant == "Index")
    
    max.rate <- vax_data_plot_E %>% arrange(-Rate) %>% head(1) %>% pull(Rate)
    
    curve.A <- ggplot(data = subset(vax_data_plot_E, Dosage == "RateFirstDose")) +
      geom_rect(data = variant_waves_region, 
                aes(xmin = start, xmax = end, ymin = 0, ymax = Inf, fill = Variant), alpha = .5) +
      scale_fill_manual(name = "Variant",
                        values = variant.colours) + 
      geom_line(aes(x = year_week, y = Rate, group = targetgroup, col = targetgroup), size=0.7) +
      scale_colour_manual(name = "Age groups",
                          labels = c("Age25_49" = "25 - 49", 
                                     "Age50_59" = "50 - 59",
                                     "Age60+" = "60+"),
                          values = c("Age25_49" = "#a82d17", 
                                     "Age50_59" = "#e1a744", 
                                     "Age60+" = "#1f497d")) + 
      theme_bw()  +
      theme(legend.text = element_text(size = text_size, colour = "black"),
            legend.title = element_text(size = text_size, colour = "black"),
            legend.position = "none",
            axis.text = element_text(size = text_size, colour = "black"),
            axis.title = element_text(size = text_size, colour = "black"),
            strip.background = element_rect(fill = "white")) +
      scale_x_discrete(breaks = week_breaks_long, labels = week_labels_long) +
      scale_y_continuous(limits = c(0, max.rate)) +
      labs(subtitle = "I) ",
           x = "Week",
           y = "Rate")
    
    
    
    curve.B <- ggplot(data = subset(vax_data_plot_E, Dosage == "RateSecondDose")) +
      geom_rect(data = variant_waves_region, 
                aes(xmin = start, xmax = end, ymin = 0, ymax = Inf, fill = Variant), alpha = .5) +
      scale_fill_manual(name = "Variant",
                        values = variant.colours) + 
      geom_line(aes(x = year_week, y = Rate, group = targetgroup, col = targetgroup), size = 0.7) +
      scale_colour_manual(name = "Age groups",
                          labels = c("Age25_49" = "25 - 49", 
                                     "Age50_59" = "50 - 59",
                                     "Age60+" = "60+"),
                          values = c("Age25_49" = "#a82d17", 
                                     "Age50_59" = "#e1a744", 
                                     "Age60+" = "#1f497d")) + 
      theme_bw()  +
      theme(legend.text= element_text(size = text_size, colour = "black"),
            legend.title = element_text(size = text_size, colour = "black"),
            legend.position = "bottom",
            axis.text = element_text(size = text_size, colour = "black"),
            axis.title = element_text(size = text_size, colour = "black"),
            strip.background = element_rect(fill = "white")) +
      scale_x_discrete(breaks = week_breaks_vaccination, labels = week_labels_vaccination) +
      scale_y_continuous(limits = c(0, max.rate)) +
      labs(subtitle = "II) ",
           x = "Week",
           y = "")
    
    curve.C <- ggplot(data = subset(vax_data_plot_E, Dosage == "RateDoseAdditional1")) +
      geom_rect(data = variant_waves_region, 
                aes(xmin = start, xmax = end, ymin = 0, ymax = Inf, fill = Variant), alpha = .5) +
      scale_fill_manual(name = "Variant",
                        values = variant.colours) + 
      geom_line(aes(x = year_week, y = Rate, group = targetgroup, col = targetgroup), size=0.7) +
      scale_colour_manual(name ="Age groups",
                          labels = c("Age25_49" = "25 - 49", 
                                     "Age50_59" = "50 - 59",
                                     "Age60+" = "60+"),
                          values = c("Age25_49" = "#a82d17", 
                                     "Age50_59" = "#e1a744", 
                                     "Age60+" = "#1f497d")) + 
      scale_x_discrete(breaks = week_breaks_vaccination, labels = week_labels_vaccination) +
      scale_y_continuous(limits = c(0, max.rate)) +
      theme_bw()  +
      theme(legend.text = element_text(size = text_size, colour = "black"),
            legend.title = element_text(size = text_size, colour = "black"),
            legend.position = "none",
            axis.text = element_text(size = text_size, colour = "black"),
            axis.title = element_text(size = text_size, colour = "black"),
            strip.background = element_rect(fill = "white")) +
      labs(subtitle = "III) ",
           x = "Week",
           y = "")
    
    curve.D <- ggplot(data = subset(vax_data_plot_E, Dosage == "RateDoseAdditional2")) +
      geom_rect(data = variant_waves_region, 
                aes(xmin = start, xmax = end, ymin = 0, ymax = Inf, fill = Variant), alpha = .5) +
      scale_fill_manual(name = "Variant",
                        values = variant.colours) + 
      geom_line(aes(x = year_week, y = Rate, group = targetgroup, col = targetgroup), size=0.7) +
      scale_colour_manual(name = "Age groups",
                          labels = c("Age25_49" = "25 - 49", 
                                     "Age50_59" = "50 - 59",
                                     "Age60+" = "60+"),
                          values = c("Age25_49" = "#a82d17", 
                                     "Age50_59" = "#e1a744", 
                                     "Age60+" = "#1f497d")) + 
      theme_bw()  +
      theme(legend.text = element_text(size = text_size, colour = "black"),
            legend.title = element_text(size = text_size, colour = "black"),
            legend.position = "none",
            axis.text = element_text(size = text_size, colour = "black"),
            axis.title = element_text(size = text_size, colour = "black"),
            strip.background = element_rect(fill = "white")) +
      scale_x_discrete(breaks = week_breaks_vaccination, labels = week_labels_vaccination) +
      scale_y_continuous(limits = c(0, max.rate)) +
      labs(subtitle = "IV) ",
           x = "Week",
           y = "")
    
    vaccination_curves <- ggarrange(curve.A, curve.B, curve.C, curve.D, nrow=1, ncol=4, common.legend = T, legend = "bottom", align = "h")
    
    return(vaccination_curves)
  } 
  
  if(figure == "F") {
    
    vax_data_plot_F <- vax_clean %>%
      filter(year_week %in% vaccine.weeks$DateUsedForStatisticsISO) %>% 
      group_by(report_country, targetgroup) %>%
      summarise(FirstDose = sum(FirstDose),
                SecondDose = sum(SecondDose),
                DoseAdditional1 = sum(DoseAdditional1),
                DoseAdditional2 = sum(DoseAdditional2),
                denominator = sum(unique(denominator))) %>%
      ungroup() %>%
      group_by(report_country, targetgroup) %>%
      mutate(nFirstDose=  cumsum(FirstDose),
             nSecondDose = cumsum(SecondDose),
             nDoseAdditional1 = cumsum(DoseAdditional1),
             nDoseAdditional2 = cumsum(DoseAdditional2),
             pcFirstDose = round((cumsum(FirstDose)/denominator)*100),
             pcSecondDose = round((cumsum(SecondDose)/denominator)*100),
             pcDoseAdditional1 = round((cumsum(DoseAdditional1)/denominator)*100),
             pcDoseAdditional2 = round((cumsum(DoseAdditional2)/denominator)*100)) %>%
      ungroup() %>%
      group_by(report_country, targetgroup) %>%
      summarise(pcFirstDose = ifelse(pcFirstDose>100, 100, pcFirstDose),
             pcSecondDose = ifelse(pcSecondDose>100, 100, pcSecondDose),
             pcDoseAdditional1 = ifelse(pcDoseAdditional1 > 100, 100, pcDoseAdditional1),
             pcDoseAdditional2 = ifelse(pcDoseAdditional2 > 100, 100, pcDoseAdditional2)) %>%
      ungroup() %>% 
      filter(targetgroup %in% c("Age25_49", "Age50_59", "Age60+")) %>% 
      pivot_longer(cols = "pcFirstDose":"pcDoseAdditional2",
                   names_to = "Dosage",
                   values_to = "Percentage") %>% 
      mutate(report_country = ifelse(report_country == "United Kingdom, England", "United Kingdom\n(England)", report_country),
             report_country = ifelse(report_country == "United Kingdom, Scotland", "United Kingdom\n(Scotland)", report_country),
             report_country = ifelse(report_country == "United Kingdom, Wales", "United Kingdom\n(Wales)", report_country),
             report_country = ifelse(report_country == "Kosovo", "Kosovo*", report_country))
      
    template <- expand.grid(report_country = c("Germany", "Ukraine"),
                            targetgroup = c("Age25_49", "Age50_59"),
                            Dosage = c("pcFirstDose", "pcSecondDose", "pcDoseAdditional1", "pcDoseAdditional2"),
                            Percentage = 0)
    
    vax_data_plot_F <- full_join(vax_data_plot_F, template)
    
    plot.A <- ggplot(data = subset(vax_data_plot_F, Dosage == "pcFirstDose")) +
      geom_bar(aes(x = Percentage, y = report_country, fill = targetgroup), position = position_dodge(), stat = "identity") +
      scale_fill_manual(name = "Age groups",
                          labels = c("Age25_49" = "25 - 49", 
                                     "Age50_59" = "50 - 59",
                                     "Age60+" = "60+"),
                        values = c("Age25_49" = "#a82d17", 
                                   "Age50_59" = "#e1a744", 
                                   "Age60+" = "#1f497d")) + 
      theme_bw()  +
      theme(legend.text = element_text(size = text_size, colour = "black"),
            legend.title = element_text(size = text_size, colour = "black"),
            legend.position = "bottom",
            axis.text.y = element_text(size = text_size, colour = "black"),
            axis.title = element_text(size = text_size, colour = "black"),
            strip.background = element_rect(fill = "white")) +
      scale_x_continuous(breaks = c(0, 20, 40, 60, 80, 100)) +
      scale_y_discrete(limits = rev, drop = FALSE) +
      labs(subtitle = "A) ",
           x = "",
           y = "Member State")
    
    
    plot.B <- ggplot(data = subset(vax_data_plot_F, Dosage == "pcSecondDose")) +
      geom_bar(aes(x = Percentage, y = report_country, fill = targetgroup), position = position_dodge(), stat="identity") +
      scale_fill_manual(name = "Age groups",
                          labels = c("Age25_49" = "25 - 49", 
                                     "Age50_59" = "50 - 59",
                                     "Age60+" = "60+"),
                        values = c("Age25_49" = "#a82d17", 
                                   "Age50_59" = "#e1a744", 
                                   "Age60+" = "#1f497d")) + 
      theme_bw()  +
      theme(legend.text= element_text(size = text_size, colour = "black"),
            legend.title = element_text(size = text_size, colour = "black"),
            legend.position = "bottom",
            axis.text.y = element_text(size = text_size, colour = "black"),
            axis.title = element_text(size = text_size, colour = "black"),
            strip.background = element_rect(fill = "white")) +
      scale_x_continuous(breaks = c(0, 20, 40, 60, 80, 100)) +
      scale_y_discrete(limits = rev, drop = FALSE, position = "right") +
      labs(subtitle = "B) ",
           x = "",
           y = "")
    
    plot.C <- ggplot(data = subset(vax_data_plot_F, Dosage == "pcDoseAdditional1")) +
      geom_bar(aes(x = Percentage, y = report_country, fill = targetgroup), position = "dodge", stat="identity") +
      scale_fill_manual(name = "Age groups",
                          labels = c("Age25_49" = "25 - 49", 
                                     "Age50_59" = "50 - 59",
                                     "Age60+" = "60+"),
                        values = c("Age25_49" = "#a82d17", 
                                   "Age50_59" = "#e1a744", 
                                   "Age60+" = "#1f497d")) + 
      theme_bw()  +
      theme(legend.text = element_text(size = text_size, colour = "black"),
            legend.title = element_text(size = text_size, colour = "black"),
            legend.position = "bottom",
            axis.text.y = element_text(size = text_size, colour = "black"),
            axis.title = element_text(size = text_size, colour = "black"),
            strip.background = element_rect(fill = "white")) +
      scale_x_continuous(breaks = c(0, 20, 40, 60, 80, 100)) +
      scale_y_discrete(limits = rev, drop = FALSE) +
      labs(subtitle = "C) ",
           x = "Percentage",
           y = "Member State")
    
    plot.D <- ggplot(data = subset(vax_data_plot_F, Dosage == "pcDoseAdditional2")) +
      geom_bar(aes(x = Percentage, y = report_country, fill = targetgroup), position = "dodge", stat="identity") +
      scale_fill_manual(name = "Age groups",
                        labels = c("Age25_49" = "25 - 49", 
                                   "Age50_59" = "50 - 59",
                                   "Age60+" = "60+"),
                        values = c("Age25_49" = "#a82d17", 
                                   "Age50_59" = "#e1a744", 
                                   "Age60+" = "#1f497d")) + 
      theme_bw()  +
      theme(legend.text = element_text(size = text_size, colour = "black"),
            legend.title = element_text(size = text_size, colour = "black"),
            legend.position = "bottom",
            axis.text.y = element_text(size = text_size, colour = "black"),
            axis.title = element_text(size = text_size, colour = "black"),
            strip.background = element_rect(fill = "white")) +
      scale_x_continuous(breaks = c(0, 20, 40, 60, 80, 100),
                         limits = c(0, 100)) +
      scale_y_discrete(limits = rev, drop = FALSE, position = "right") +
      labs(subtitle = "D) ",
           x = "Percentage",
           y = "")
    
    vaccination_curves <- ggarrange(plot.A, plot.B, plot.C, plot.D, nrow=2, ncol=2, common.legend = T, legend = "bottom", align = "h",
                                    widths=c(1, 1, 1, 1)) 
    
    return(vaccination_curves)
    
  }
  
}