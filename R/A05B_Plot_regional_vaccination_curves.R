# Script to plot percentage of lives saved per vaccination dose and age
# Margaux Mesle - meslem@who.int
# April 2023



plot.regional.vaccination.curves <- function(Expected_cases_all_ages, 
                                             variant_waves_region, 
                                             reporting.weeks,
                                             version,
                                             to.save){
  
   variant_waves_region <- variant_waves_region %>% 
    mutate(position = ifelse(Variant == "Alpha", "2021-10",
                             ifelse(Variant == "Delta", "2021-40",
                                    ifelse(Variant == "Omicron", "2022-35", "2020-51"))))
  
  variant_plot_timings <- variant_waves_region %>% 
    rename(Position = position) %>% 
    filter(!Variant == "Index")
  
  
  if(version == "Percentage only"){
    
    expected_data_to_plot <- Expected_cases_all_ages %>% 
      filter(year_week %in% reporting.weeks) %>% 
      select(ReportCountry, year_week, TargetGroup,
             DeathsObserved, DeathsAvertedDose1, DeathsAvertedDose2, DeathsAvertedDose3, DeathsAvertedDose4, DeathsAvertedDose5,
             DeathsExpected) %>% 
      group_by(year_week, TargetGroup) %>% 
      summarise(DeathsObserved = sum(DeathsObserved),
                DeathsExpected = sum(DeathsExpected),
                DeathsAvertedDose1 = sum(DeathsAvertedDose1),
                DeathsAvertedDose2 = sum(DeathsAvertedDose2),
                DeathsAvertedDose3 = sum(DeathsAvertedDose3),
                DeathsAvertedDose4 = sum(DeathsAvertedDose4),
                DeathsAvertedDose5 = sum(DeathsAvertedDose5),
                LivesSaved = DeathsExpected - DeathsObserved) %>%
      mutate(DeathsAvertedDose2 = ifelse(DeathsAvertedDose1 == 0 & DeathsAvertedDose2 >0, 0, DeathsAvertedDose2),
             pcDose1 = round((DeathsAvertedDose1 / LivesSaved) *100, 2),
             pcDose2 = round((DeathsAvertedDose2 / LivesSaved) *100, 2),
             pcDose3 = round((DeathsAvertedDose3 / LivesSaved) *100, 2),
             pcDose4 = round((DeathsAvertedDose4 / LivesSaved) *100, 2),
             pcDose5 = round((DeathsAvertedDose5 / LivesSaved) *100, 2)) %>%
      ungroup() %>% 
      pivot_longer(cols = "pcDose1" : "pcDose5",
                   names_to = "Dosage",
                   values_to = "Deaths")

    
    p1 <- ggplot() +
      geom_segment(data = variant_waves_region, aes(x = start, xend = end, y = 102, yend = 102)) +
      geom_segment(data = variant_waves_region, aes(x = start, xend = start, y = 100, yend = 104)) +
      geom_text(data = variant_waves_region, aes(x = position, y = 106, label = Variant), size = 4) +
      geom_bar(data = subset(expected_data_to_plot, TargetGroup == "Age 25-49"),
               aes(x = year_week, y = Deaths, fill = Dosage), stat = "identity") +
      scale_fill_manual(values = c("pcDose1" = "#a63022",
                                   "pcDose2" = "#D6852B",
                                   "pcDose3" = "#1f497d",
                                   "pcDose4" = "#3C8EA2",
                                   "pcDose5" = "#93C7CF"),
                        labels = c("pcDose1" = "First dose",
                                   "pcDose2" = "Second dose",
                                   "pcDose3" = "First booster",
                                   "pcDose4" = "Second booster",
                                   "pcDose5" = "Third booster")) +
      scale_x_discrete(breaks = week_breaks_long, labels = week_labels_long) +
      theme_bw() +
      theme(legend.text = element_text(size = text_size, colour = "black"),
            legend.title = element_text(size = text_size, colour = "black"),
            legend.position = "right",
            strip.text = element_text(size = 15),
            axis.text = element_text(size = text_size, colour = "black"),
            axis.title = element_text(size = text_size, colour = "black"),
            axis.line = element_line()) +
      labs(subtitle = "A) 25 - 49 years",
           x = "Calendar week",
           y = "Percentage (%)")
    
    p2 <- ggplot() +
      geom_segment(data = variant_waves_region, aes(x = start, xend = end, y = 102, yend = 102)) +
      geom_segment(data = variant_waves_region, aes(x = start, xend = start, y = 100, yend = 104)) +
      geom_text(data = variant_waves_region, aes(x = position, y = 106, label = Variant), size = 4) +
      geom_bar(data = subset(expected_data_to_plot, TargetGroup == "Age 50-59"),
               aes(x = year_week, y = Deaths, fill = Dosage), stat = "identity") +
      scale_fill_manual(values = c("pcDose1" = "#a63022",
                                   "pcDose2" = "#D6852B",
                                   "pcDose3" = "#1f497d",
                                   "pcDose4" = "#3C8EA2",
                                   "pcDose5" = "#93C7CF"),
                        labels = c("pcDose1" = "First dose",
                                   "pcDose2" = "Second dose",
                                   "pcDose3" = "First booster",
                                   "pcDose4" = "Second booster",
                                   "pcDose5" = "Third booster")) +
      scale_x_discrete(breaks = week_breaks_long, labels = week_labels_long) +
      theme_bw() +
      theme(legend.text = element_text(size = text_size, colour = "black"),
            legend.title = element_text(size = text_size, colour = "black"),
            legend.position = "right",
            strip.text = element_text(size = 15),
            axis.text = element_text(size = text_size, colour = "black"),
            axis.title = element_text(size = text_size, colour = "black"),
            axis.line = element_line()) +
      labs(subtitle = "B) 50 - 59 years",
           x = "Calendar week",
           y = "Percentage (%)")
    
    p3 <- ggplot() +
      geom_segment(data = variant_waves_region, aes(x = start, xend = end, y = 102, yend = 102)) +
      geom_segment(data = variant_waves_region, aes(x = start, xend = start, y = 100, yend = 104)) +
      geom_text(data = variant_waves_region, aes(x = position, y = 106, label = Variant), size = 4) +
      geom_bar(data = subset(expected_data_to_plot, TargetGroup == "Age 60+"),
               aes(x = year_week, y = Deaths, fill = Dosage), stat = "identity") +
      scale_fill_manual(values = c("pcDose1" = "#a63022",
                                   "pcDose2" = "#D6852B",
                                   "pcDose3" = "#1f497d",
                                   "pcDose4" = "#3C8EA2",
                                   "pcDose5" = "#93C7CF"),
                        labels = c("pcDose1" = "First dose",
                                   "pcDose2" = "Second dose",
                                   "pcDose3" = "First booster",
                                   "pcDose4" = "Second booster",
                                   "pcDose5" = "Third booster")) +
      scale_x_discrete(breaks = week_breaks_long, labels = week_labels_long) +
      theme_bw() +
      theme(legend.text = element_text(size = text_size, colour = "black"),
            legend.title = element_text(size = text_size, colour = "black"),
            legend.position = "right",
            strip.text = element_text(size = 15),
            axis.text = element_text(size = text_size, colour = "black"),
            axis.title = element_text(size = text_size, colour = "black"),
            axis.line = element_line()) +
      labs(subtitle = "C) 60 years or older",
           x = "Calendar week",
           y = "Percentage (%)")
    
    ggsave("Outputs/ECCMID/ECCMID_vaccine_proportions_60.png", dpi = 300, width = 9, height = 4)
    
    p_vax_proportions <- ggarrange(p1, p2, p3, ncol = 1, common.legend = TRUE, legend = "bottom")
    
    return(p_vax_proportions)
    #ggsave("Outputs/ECCMID/ECCMID_vaccine_proportions_all.png", dpi = 300, width = 8, height = 10)
    
  } 
  
  if(version == "Counts and percentages"){
    
    # Calculate percentages
    expected_data_to_plot <- Expected_cases_all_ages %>% 
      filter(year_week %in% reporting.weeks) %>% 
      select(ReportCountry, year_week, TargetGroup,
             DeathsObserved, DeathsAvertedDose1, DeathsAvertedDose2, DeathsAvertedDose3, DeathsAvertedDose4, DeathsAvertedDose5,
             DeathsExpected) %>% 
      group_by(year_week, TargetGroup) %>% 
      summarise(DeathsObserved = sum(DeathsObserved),
                DeathsExpected = sum(DeathsExpected),
                DeathsAvertedDose1 = sum(DeathsAvertedDose1),
                DeathsAvertedDose2 = sum(DeathsAvertedDose2),
                DeathsAvertedDose3 = sum(DeathsAvertedDose3),
                DeathsAvertedDose4 = sum(DeathsAvertedDose4),
                DeathsAvertedDose5 = sum(DeathsAvertedDose5),
                LivesSaved = DeathsExpected - DeathsObserved) %>%
      mutate(DeathsAvertedDose2 = ifelse(DeathsAvertedDose1 == 0 & DeathsAvertedDose2 >0, 0, DeathsAvertedDose2),
             pcDose1 = round((DeathsAvertedDose1 / LivesSaved) *100, 2),
             pcDose2 = round((DeathsAvertedDose2 / LivesSaved) *100, 2),
             pcDose3 = round((DeathsAvertedDose3 / LivesSaved) *100, 2),
             pcDose4 = round((DeathsAvertedDose4 / LivesSaved) *100, 2),
             pcDose5 = round((DeathsAvertedDose5 / LivesSaved) *100, 2)) %>%
      ungroup() %>% 
      pivot_longer(cols = "pcDose1" : "pcDose5",
                   names_to = "Dosage",
                   values_to = "Deaths") %>% 
      mutate(TargetGroup = ifelse(TargetGroup == "Age 25-49", "25 to 49 years",
                                  ifelse(TargetGroup == "Age 50-59", "50 to 59 years", "≥ 60 years")),
             TargetGroup = factor(TargetGroup, levels = c("25 to 49 years", "50 to 59 years", "≥ 60 years")))
      
    
    # Calculate counts
    lives_dose <- Expected_cases_all_ages %>% 
      select(ReportCountry, year_week, TargetGroup, DeathsAvertedDose1 : TotalAverted) %>% 
      group_by(TargetGroup, year_week) %>% 
      summarise(DeathsAvertedDose1 = sum(DeathsAvertedDose1),
                DeathsAvertedDose2 = sum(DeathsAvertedDose2),
                DeathsAvertedDose3 = sum(DeathsAvertedDose3),
                DeathsAvertedDose4 = sum(DeathsAvertedDose4),
                DeathsAvertedDose5 = sum(DeathsAvertedDose5),
                TotalAverted = sum(TotalAverted)) %>% 
      ungroup() %>% 
      mutate(TargetGroup = ifelse(TargetGroup == "Age 25-49", "25 to 49 years",
                                  ifelse(TargetGroup == "Age 50-59", "50 to 59 years", "≥ 60 years")),
             TargetGroup = factor(TargetGroup, levels = c("25 to 49 years", "50 to 59 years", "≥ 60 years"))) %>% 
      pivot_longer(cols = DeathsAvertedDose1 : TotalAverted,
                   names_to = "Dosage",
                   values_to = "Counts") 
    
    # Create plots
    p1.1 <- ggplot() +
      geom_segment(data = variant_waves_region, aes(x = start, xend = end, y = 470, yend = 470)) +
      geom_segment(data = variant_waves_region, aes(x = start, xend = start, y = 465, yend = 475)) +
      geom_text(data = subset(variant_plot_timings, Variant %in% c("Alpha", "Delta", "Omicron")), 
                aes(x = Position, y = 490, label = Variant), size = 4) +
      geom_line(data = subset(lives_dose, TargetGroup == "25 to 49 years"), 
                aes(x = year_week, y = Counts, group = Dosage, colour = Dosage), size = 1) +
      facet_wrap(.~ TargetGroup) +
      scale_color_manual(name = "Vaccine dose",
                         values = c("DeathsAvertedDose1" = "#a63022", 
                                    "DeathsAvertedDose2" = "#D6852B",
                                    "DeathsAvertedDose3" = "#1f497d", 
                                    "DeathsAvertedDose4" = "#3C8EA2", 
                                    "DeathsAvertedDose5" = "#93C7CF", 
                                    "TotalAverted" = "black"),
                         labels = c("DeathsAvertedDose1" = "First dose", 
                                    "DeathsAvertedDose2" = "Second dose",
                                    "DeathsAvertedDose3" = "First booster", 
                                    "DeathsAvertedDose4" = "Second booster", 
                                    "DeathsAvertedDose5" = "Third booster", 
                                    "TotalAverted" = "Total")) +
      scale_x_discrete(breaks = week_breaks_long, labels = week_labels_long) +
      theme_bw() +
      theme(legend.text = element_text(size = text_size, colour = "black"),
            legend.title = element_text(size = text_size, colour = "black"),
            legend.position = "right",
            axis.text.x = element_blank(),
            axis.title = element_text(size = text_size, colour = "black"),
            strip.background = element_rect(fill = "white"),
            strip.text = element_text(size = text_size, colour = "black")) +
      labs(subtitle = "",
           x = "",
           y = "Mortality counts")
    
    p2.1 <- ggplot() +
      geom_segment(data = variant_waves_region, aes(x = start, xend = end, y = 1290, yend = 1290)) +
      geom_segment(data = variant_waves_region, aes(x = start, xend = start, y = 1280, yend = 1300)) +
      geom_text(data = subset(variant_plot_timings, Variant %in% c("Alpha", "Delta", "Omicron")), 
                aes(x = Position, y = 1350, label = Variant), size = 4) +
      geom_line(data = subset(lives_dose, TargetGroup == "50 to 59 years"), 
                aes(x = year_week, y = Counts, group = Dosage, colour = Dosage), size = 1) +
      facet_wrap(.~ TargetGroup) +
      scale_color_manual(name = "Vaccine dose",
                         values = c("DeathsAvertedDose1" = "#a63022", 
                                    "DeathsAvertedDose2" = "#D6852B",
                                    "DeathsAvertedDose3" = "#1f497d", 
                                    "DeathsAvertedDose4" = "#3C8EA2", 
                                    "DeathsAvertedDose5" = "#93C7CF", 
                                    "TotalAverted" = "black"),
                         labels = c("DeathsAvertedDose1" = "First dose", 
                                    "DeathsAvertedDose2" = "Second dose",
                                    "DeathsAvertedDose3" = "First booster", 
                                    "DeathsAvertedDose4" = "Second booster", 
                                    "DeathsAvertedDose5" = "Third booster", 
                                    "TotalAverted" = "Total")) +
      scale_x_discrete(breaks = week_breaks_long, labels = week_labels_long) +
      theme_bw() +
      theme(legend.text = element_text(size = text_size, colour = "black"),
            legend.title = element_text(size = text_size, colour = "black"),
            legend.position = "right",
            axis.text.x = element_blank(),
            axis.title = element_text(size = text_size, colour = "black"),
            strip.background = element_rect(fill = "white"),
            strip.text = element_text(size = text_size, colour = "black")) +
      labs(subtitle = "",
           x = "",
           y = "")
    
    p3.1 <- ggplot() +
      geom_segment(data = variant_waves_region, aes(x = start, xend = end, y = 42000, yend = 42000)) +
      geom_segment(data = variant_waves_region, aes(x = start, xend = start, y = 42000 - 500, yend = 42000 + 500)) +
      geom_text(data = variant_plot_timings, aes(x = Position, y = 47000, label = Variant), size = 4) +
      geom_line(data = subset(lives_dose, TargetGroup == "≥ 60 years"), 
                aes(x = year_week, y = Counts, group = Dosage, colour = Dosage), size = 1) +
      facet_wrap(.~ TargetGroup) +
      scale_color_manual(name = "Vaccine dose",
                         values = c("DeathsAvertedDose1" = "#a63022", 
                                    "DeathsAvertedDose2" = "#D6852B",
                                    "DeathsAvertedDose3" = "#1f497d", 
                                    "DeathsAvertedDose4" = "#3C8EA2", 
                                    "DeathsAvertedDose5" = "#93C7CF", 
                                    "TotalAverted" = "black"),
                         labels = c("DeathsAvertedDose1" = "First dose", 
                                    "DeathsAvertedDose2" = "Second dose",
                                    "DeathsAvertedDose3" = "First booster", 
                                    "DeathsAvertedDose4" = "Second booster", 
                                    "DeathsAvertedDose5" = "Third booster", 
                                    "TotalAverted" = "Total")) +
      scale_x_discrete(breaks = week_breaks_long, labels = week_labels_long) +
      theme_bw() +
      theme(legend.text = element_text(size = text_size, colour = "black"),
            legend.title = element_text(size = text_size, colour = "black"),
            legend.position = "right",
            axis.text.x = element_blank(),
            axis.title = element_text(size = text_size, colour = "black"),
            strip.background = element_rect(fill = "white"),
            strip.text = element_text(size = text_size, colour = "black")) +
      labs(subtitle = "",
           x = "",
           y = "")
 
    p1.2 <- ggplot() +
      geom_segment(data = variant_waves_region, aes(x = start, xend = end, y = 102, yend = 102)) +
      geom_segment(data = variant_waves_region, aes(x = start, xend = start, y = 100, yend = 104)) +
      geom_text(data = variant_plot_timings, aes(x = Position, y = 106, label = Variant), size = 4) +
      geom_bar(data = subset(expected_data_to_plot, TargetGroup == "25 to 49 years"),
               aes(x = year_week, y = Deaths, fill = Dosage), stat = "identity") +
      facet_wrap(.~ TargetGroup) +
      scale_fill_manual(values = c("pcDose1" = "#a63022",
                                   "pcDose2" = "#D6852B",
                                   "pcDose3" = "#1f497d",
                                   "pcDose4" = "#3C8EA2",
                                   "pcDose5" = "#93C7CF"),
                        labels = c("pcDose1" = "First dose",
                                   "pcDose2" = "Second dose",
                                   "pcDose3" = "First booster",
                                   "pcDose4" = "Second booster",
                                   "pcDose5" = "Third booster")) +
      scale_x_discrete(breaks = week_breaks_long, labels = week_labels_long) +
      theme_bw() +
      theme(legend.text = element_text(size = text_size, colour = "black"),
            legend.title = element_text(size = text_size, colour = "black"),
            legend.position = "right",
            axis.text = element_text(size = text_size, colour = "black"),
            axis.title = element_text(size = text_size, colour = "black"),
            axis.line = element_line(),
            strip.background = element_rect(fill = "white"),
            strip.text = element_text(size = text_size, colour = "black")) +
      labs(subtitle = "",
           x = "Calendar week",
           y = "Percentage (%)")
    
    p2.2 <- ggplot() +
      geom_segment(data = variant_waves_region, aes(x = start, xend = end, y = 102, yend = 102)) +
      geom_segment(data = variant_waves_region, aes(x = start, xend = start, y = 100, yend = 104)) +
      geom_text(data = variant_plot_timings, aes(x = Position, y = 106, label = Variant), size = 4) +
      geom_bar(data = subset(expected_data_to_plot, TargetGroup == "50 to 59 years"),
               aes(x = year_week, y = Deaths, fill = Dosage), stat = "identity") +
      facet_wrap(.~ TargetGroup) +
      scale_fill_manual(values = c("pcDose1" = "#a63022",
                                   "pcDose2" = "#D6852B",
                                   "pcDose3" = "#1f497d",
                                   "pcDose4" = "#3C8EA2",
                                   "pcDose5" = "#93C7CF"),
                        labels = c("pcDose1" = "First dose",
                                   "pcDose2" = "Second dose",
                                   "pcDose3" = "First booster",
                                   "pcDose4" = "Second booster",
                                   "pcDose5" = "Third booster")) +
      scale_x_discrete(breaks = week_breaks_long, labels = week_labels_long) +
      theme_bw() +
      theme(legend.text = element_text(size = text_size, colour = "black"),
            legend.title = element_text(size = text_size, colour = "black"),
            legend.position = "right",
            axis.text = element_text(size = text_size, colour = "black"),
            axis.title = element_text(size = text_size, colour = "black"),
            axis.line = element_line(),
            strip.background = element_rect(fill = "white"),
            strip.text = element_text(size = text_size, colour = "black")) +
      labs(subtitle = "",
           x = "Calendar week",
           y = "")
    
    p3.2 <- ggplot() +
      geom_segment(data = variant_waves_region, aes(x = start, xend = end, y = 102, yend = 102)) +
      geom_segment(data = variant_waves_region, aes(x = start, xend = start, y = 100, yend = 104)) +
      geom_text(data = variant_plot_timings, aes(x = Position, y = 106, label = Variant), size = 4) +
      geom_bar(data = subset(expected_data_to_plot, TargetGroup == "≥ 60 years"),
               aes(x = year_week, y = Deaths, fill = Dosage), stat = "identity") +
      facet_wrap(.~ TargetGroup) +
      scale_fill_manual(values = c("pcDose1" = "#a63022",
                                   "pcDose2" = "#D6852B",
                                   "pcDose3" = "#1f497d",
                                   "pcDose4" = "#3C8EA2",
                                   "pcDose5" = "#93C7CF"),
                        labels = c("pcDose1" = "First dose",
                                   "pcDose2" = "Second dose",
                                   "pcDose3" = "First booster",
                                   "pcDose4" = "Second booster",
                                   "pcDose5" = "Third booster")) +
      scale_x_discrete(breaks = week_breaks_long, labels = week_labels_long) +
      theme_bw() +
      theme(legend.text = element_text(size = text_size, colour = "black"),
            legend.title = element_text(size = text_size, colour = "black"),
            legend.position = "right",
            axis.text = element_text(size = text_size, colour = "black"),
            axis.title = element_text(size = text_size, colour = "black"),
            axis.line = element_line(),
            strip.background = element_rect(fill = "white"),
            strip.text = element_text(size = text_size, colour = "black")) +
      labs(subtitle = "",
           x = "Calendar week",
           y = "")
    
    #ggsave("Outputs/Deaths_averted_Figure_2.png", dpi = 300, width = 9, height = 4)
    
    p_vax_proportions <- ggarrange(p1.1, p2.1, p3.1, p1.2, p2.2, p3.2, ncol = 3, nrow = 2,
                                   common.legend = TRUE, legend = "bottom", align = "v")
    
    return(p_vax_proportions)
    #ggsave("Outputs/Deaths_averted_Figure_2.png", dpi = 300, width = 14, height = 7)
    
  }
  
  if(version == "Counts and percentages by finer ages"){
    
    # Calculate percentages
    expected_data_to_plot <- Expected_cases_all_ages %>% 
      filter(year_week %in% reporting.weeks) %>% 
      select(ReportCountry, year_week, TargetGroup,
             DeathsObserved, DeathsAvertedDose1, DeathsAvertedDose2, DeathsAvertedDose3, DeathsAvertedDose4, DeathsAvertedDose5,
             DeathsExpected)
    
    expected_data_to_plot_60 <- Expected_cases_all_ages %>% 
      filter(year_week %in% reporting.weeks) %>% 
      select(ReportCountry, year_week, TargetGroup,
             DeathsObserved, DeathsAvertedDose1, DeathsAvertedDose2, DeathsAvertedDose3, DeathsAvertedDose4, DeathsAvertedDose5,
             DeathsExpected) %>% 
      filter(TargetGroup %in% c("Age 60-69", "Age 70-79", "Age 80+")) %>% 
      group_by(year_week) %>% 
      summarise(DeathsObserved = sum(DeathsObserved),
                DeathsExpected = sum(DeathsExpected),
                DeathsAvertedDose1 = sum(DeathsAvertedDose1),
                DeathsAvertedDose2 = sum(DeathsAvertedDose2),
                DeathsAvertedDose3 = sum(DeathsAvertedDose3),
                DeathsAvertedDose4 = sum(DeathsAvertedDose4),
                DeathsAvertedDose5 = sum(DeathsAvertedDose5),
                LivesSaved = DeathsExpected - DeathsObserved) %>%
      add_column(TargetGroup = "Age 60+", .before = "DeathsObserved")
    
    
    expected_data_to_plot <- expected_data_to_plot %>%
      full_join(expected_data_to_plot_60) %>% 
      group_by(year_week, TargetGroup) %>% 
      summarise(DeathsObserved = sum(DeathsObserved),
                DeathsExpected = sum(DeathsExpected),
                DeathsAvertedDose1 = sum(DeathsAvertedDose1),
                DeathsAvertedDose2 = sum(DeathsAvertedDose2),
                DeathsAvertedDose3 = sum(DeathsAvertedDose3),
                DeathsAvertedDose4 = sum(DeathsAvertedDose4),
                DeathsAvertedDose5 = sum(DeathsAvertedDose5),
                LivesSaved = DeathsExpected - DeathsObserved) %>%
      mutate(DeathsAvertedDose2 = ifelse(DeathsAvertedDose1 == 0 & DeathsAvertedDose2 >0, 0, DeathsAvertedDose2),
             pcDose1 = round((DeathsAvertedDose1 / LivesSaved) *100, 2),
             pcDose2 = round((DeathsAvertedDose2 / LivesSaved) *100, 2),
             pcDose3 = round((DeathsAvertedDose3 / LivesSaved) *100, 2),
             pcDose4 = round((DeathsAvertedDose4 / LivesSaved) *100, 2),
             pcDose5 = round((DeathsAvertedDose5 / LivesSaved) *100, 2)) %>%
      ungroup() %>% 
      pivot_longer(cols = "pcDose1" : "pcDose5",
                   names_to = "Dosage",
                   values_to = "Deaths") %>% 
      mutate(TargetGroup = ifelse(TargetGroup == "Age 25-49", "25 to 49 years",
                                  ifelse(TargetGroup == "Age 50-59", "50 to 59 years",
                                         ifelse(TargetGroup == "Age 60-69", "60 to 69 years", 
                                                ifelse(TargetGroup == "Age 70-79", "70 to 79 years", 
                                                       ifelse(TargetGroup == "Age 80+", "≥80 years", "≥60 years"))))),
             TargetGroup = factor(TargetGroup, levels = c("25 to 49 years", "50 to 59 years", "≥60 years", 
                                                          "60 to 69 years", "70 to 79 years", "≥80 years")))
    
    
    # Calculate counts ---------------------------------------------------------
    lives_dose <- Expected_cases_all_ages %>% 
      select(ReportCountry, year_week, TargetGroup, DeathsAvertedDose1 : TotalAverted) 
    
    lives_dose_60 <- Expected_cases_all_ages %>% 
      filter(year_week %in% reporting.weeks) %>% 
      select(ReportCountry, year_week, TargetGroup, DeathsAvertedDose1 : TotalAverted) %>% 
      filter(TargetGroup %in% c("Age 60-69", "Age 70-79", "Age 80+")) %>% 
      group_by(year_week) %>% 
      summarise(DeathsAvertedDose1 = sum(DeathsAvertedDose1),
                DeathsAvertedDose2 = sum(DeathsAvertedDose2),
                DeathsAvertedDose3 = sum(DeathsAvertedDose3),
                DeathsAvertedDose4 = sum(DeathsAvertedDose4),
                DeathsAvertedDose5 = sum(DeathsAvertedDose5),
                TotalAverted = sum(TotalAverted)) %>%
      add_column(TargetGroup = "Age 60+", .before = "DeathsAvertedDose1")
    
    
    lives_dose <- lives_dose %>% 
      full_join(lives_dose_60) %>%  
      group_by(TargetGroup, year_week) %>% 
      summarise(DeathsAvertedDose1 = sum(DeathsAvertedDose1),
                DeathsAvertedDose2 = sum(DeathsAvertedDose2),
                DeathsAvertedDose3 = sum(DeathsAvertedDose3),
                DeathsAvertedDose4 = sum(DeathsAvertedDose4),
                DeathsAvertedDose5 = sum(DeathsAvertedDose5),
                TotalAverted = sum(TotalAverted)) %>% 
      ungroup() %>% 
      mutate(TargetGroup = ifelse(TargetGroup == "Age 25-49", "25 to 49 years",
                                  ifelse(TargetGroup == "Age 50-59", "50 to 59 years",
                                         ifelse(TargetGroup == "Age 60-69", "60 to 69 years", 
                                                ifelse(TargetGroup == "Age 70-79", "70 to 79 years", 
                                                       ifelse(TargetGroup == "Age 80+", "≥80 years", "≥60 years"))))),
             TargetGroup = factor(TargetGroup, levels = c("25 to 49 years", "50 to 59 years", "≥60 years", 
                                                          "60 to 69 years", "70 to 79 years", "≥80 years"))) %>% 
    pivot_longer(cols = DeathsAvertedDose1 : TotalAverted,
                   names_to = "Dosage",
                   values_to = "Counts") 
    
    # Create plots
    p1.1 <- ggplot() +
      geom_segment(data = variant_waves_region, aes(x = start, xend = end, y = 0.470, yend = 0.470)) +
      geom_segment(data = variant_waves_region, aes(x = start, xend = start, y = 0.465, yend = 0.475)) +
      geom_text(data = subset(variant_plot_timings, Variant %in% c("Alpha", "Delta", "Omicron")), 
                aes(x = Position, y = 0.490, label = Variant), size = 4) +
      geom_line(data = subset(lives_dose, TargetGroup == "25 to 49 years"), 
                aes(x = year_week, y = Counts/1000, group = Dosage, colour = Dosage), size = 1) +
      facet_wrap(.~ TargetGroup) +
      scale_color_manual(name = "Vaccine dose",
                         values = c("DeathsAvertedDose1" = "#a63022", 
                                    "DeathsAvertedDose2" = "#D6852B",
                                    "DeathsAvertedDose3" = "#1f497d", 
                                    "DeathsAvertedDose4" = "#3C8EA2", 
                                    "DeathsAvertedDose5" = "#93C7CF", 
                                    "TotalAverted" = "black"),
                         labels = c("DeathsAvertedDose1" = "First dose", 
                                    "DeathsAvertedDose2" = "Second dose",
                                    "DeathsAvertedDose3" = "First booster", 
                                    "DeathsAvertedDose4" = "Second booster", 
                                    "DeathsAvertedDose5" = "Third booster", 
                                    "TotalAverted" = "Total")) +
      scale_x_discrete(breaks = week_breaks_long, labels = week_labels_long) +
      theme_bw() +
      theme(legend.text = element_text(size = text_size, colour = "black"),
            legend.title = element_text(size = text_size, colour = "black"),
            legend.position = "right",
            axis.text.x = element_blank(),
            axis.text = element_text(size = text_size, colour = "black"),
            axis.title = element_text(size = text_size, colour = "black"),
            strip.background = element_rect(fill = "white"),
            strip.text = element_text(size = text_size, colour = "black"),
            plot.margin = unit(c(0, 0.2, 0, 0.5), 'lines')) +
      labs(subtitle = "",
           x = "",
           y = "Mortality counts (x1,000)")
    
    p2.1 <- ggplot() +
      geom_segment(data = variant_waves_region, aes(x = start, xend = end, y = 1.290, yend = 1.290)) +
      geom_segment(data = variant_waves_region, aes(x = start, xend = start, y = 1.280, yend = 1.300)) +
      geom_text(data = subset(variant_plot_timings, Variant %in% c("Alpha", "Delta", "Omicron")), 
                aes(x = Position, y = 1.350, label = Variant), size = 4) +
      geom_line(data = subset(lives_dose, TargetGroup == "50 to 59 years"), 
                aes(x = year_week, y = Counts/1000, group = Dosage, colour = Dosage), size = 1) +
      facet_wrap(.~ TargetGroup) +
      scale_color_manual(name = "Vaccine dose",
                         values = c("DeathsAvertedDose1" = "#a63022", 
                                    "DeathsAvertedDose2" = "#D6852B",
                                    "DeathsAvertedDose3" = "#1f497d", 
                                    "DeathsAvertedDose4" = "#3C8EA2", 
                                    "DeathsAvertedDose5" = "#93C7CF", 
                                    "TotalAverted" = "black"),
                         labels = c("DeathsAvertedDose1" = "First dose", 
                                    "DeathsAvertedDose2" = "Second dose",
                                    "DeathsAvertedDose3" = "First booster", 
                                    "DeathsAvertedDose4" = "Second booster", 
                                    "DeathsAvertedDose5" = "Third booster", 
                                    "TotalAverted" = "Total")) +
      scale_x_discrete(breaks = week_breaks_long, labels = week_labels_long) +
      theme_bw() +
      theme(legend.text = element_text(size = text_size, colour = "black"),
            legend.title = element_text(size = text_size, colour = "black"),
            legend.position = "right",
            axis.text.x = element_blank(),
            axis.text = element_text(size = text_size, colour = "black"),
            axis.title = element_text(size = text_size, colour = "black"),
            strip.text = element_text(size = text_size, colour = "black"),
            strip.background = element_rect(fill = "white"),
            plot.margin = unit(c(0, 0.2, 0, 0.5), 'lines')) +
      labs(subtitle = "",
           x = "",
           y = "")
    
    p3.1 <- ggplot() +
      geom_segment(data = variant_waves_region, aes(x = start, xend = end, y = 45.000, yend = 45.000)) +
      geom_segment(data = variant_waves_region, aes(x = start, xend = start, y = 44.500, yend = 45.500)) +
      geom_text(data = variant_plot_timings, aes(x = Position, y = 46.000, label = Variant), size = 4) +
      geom_line(data = subset(lives_dose, TargetGroup == "≥60 years"), 
                aes(x = year_week, y = Counts/1000, group = Dosage, colour = Dosage), size = 1) +
      facet_wrap(.~ TargetGroup) +
      scale_color_manual(name = "Vaccine dose",
                         values = c("DeathsAvertedDose1" = "#a63022", 
                                    "DeathsAvertedDose2" = "#D6852B",
                                    "DeathsAvertedDose3" = "#1f497d", 
                                    "DeathsAvertedDose4" = "#3C8EA2", 
                                    "DeathsAvertedDose5" = "#93C7CF", 
                                    "TotalAverted" = "black"),
                         labels = c("DeathsAvertedDose1" = "First dose", 
                                    "DeathsAvertedDose2" = "Second dose",
                                    "DeathsAvertedDose3" = "First booster", 
                                    "DeathsAvertedDose4" = "Second booster", 
                                    "DeathsAvertedDose5" = "Third booster", 
                                    "TotalAverted" = "Total")) +
      scale_x_discrete(breaks = week_breaks_long, labels = week_labels_long) +
      theme_bw() +
      theme(legend.text = element_text(size = text_size, colour = "black"),
            legend.title = element_text(size = text_size, colour = "black"),
            legend.position = "right",
            axis.text.x = element_blank(),
            axis.text = element_text(size = text_size, colour = "black"),
            axis.title = element_text(size = text_size, colour = "black"),
            strip.text = element_text(size = text_size, colour = "black"),
            strip.background = element_rect(fill = "white"),
            plot.margin = unit(c(0, 0.2, 0, 0.5), 'lines')) +
      labs(subtitle = "",
           x = "",
           y = "")
    
    
    p4.1 <- ggplot() +
      geom_segment(data = variant_waves_region, aes(x = start, xend = end, y = 3.700, yend = 3.700)) +
      geom_segment(data = variant_waves_region, aes(x = start, xend = start, y = 3.600, yend = 3.800)) +
      geom_text(data = variant_plot_timings, aes(x = Position, y = 3.800, label = Variant), size = 4) +
      geom_line(data = subset(lives_dose, TargetGroup == "60 to 69 years"), 
                aes(x = year_week, y = Counts/1000, group = Dosage, colour = Dosage), size = 1) +
      facet_wrap(.~ TargetGroup) +
      scale_color_manual(name = "Vaccine dose",
                         values = c("DeathsAvertedDose1" = "#a63022", 
                                    "DeathsAvertedDose2" = "#D6852B",
                                    "DeathsAvertedDose3" = "#1f497d", 
                                    "DeathsAvertedDose4" = "#3C8EA2", 
                                    "DeathsAvertedDose5" = "#93C7CF", 
                                    "TotalAverted" = "black"),
                         labels = c("DeathsAvertedDose1" = "First dose", 
                                    "DeathsAvertedDose2" = "Second dose",
                                    "DeathsAvertedDose3" = "First booster", 
                                    "DeathsAvertedDose4" = "Second booster", 
                                    "DeathsAvertedDose5" = "Third booster", 
                                    "TotalAverted" = "Total")) +
      scale_x_discrete(breaks = week_breaks_long, labels = week_labels_long) +
      theme_bw() +
      theme(legend.text = element_text(size = text_size, colour = "black"),
            legend.title = element_text(size = text_size, colour = "black"),
            legend.position = "right",
            axis.text.x = element_blank(),
            axis.text = element_text(size = text_size, colour = "black"),
            axis.title = element_text(size = text_size, colour = "black"),
            strip.text = element_text(size = text_size, colour = "black"),
            strip.background = element_rect(fill = "white"),
            plot.margin = unit(c(0, 0.2, 0, 0.5), 'lines')) +
      labs(subtitle = "",
           x = "",
           y = "")
    
    p5.1 <- ggplot() +
      geom_segment(data = variant_waves_region, aes(x = start, xend = end, y = 8.400, yend = 8.400)) +
      geom_segment(data = variant_waves_region, aes(x = start, xend = start, y = 8.300, yend = 8.500)) +
      geom_text(data = variant_plot_timings, aes(x = Position, y = 8.900, label = Variant), size = 4) +
      geom_line(data = subset(lives_dose, TargetGroup == "70 to 79 years"), 
                aes(x = year_week, y = Counts/1000, group = Dosage, colour = Dosage), size = 1) +
      facet_wrap(.~ TargetGroup) +
      scale_color_manual(name = "Vaccine dose",
                         values = c("DeathsAvertedDose1" = "#a63022", 
                                    "DeathsAvertedDose2" = "#D6852B",
                                    "DeathsAvertedDose3" = "#1f497d", 
                                    "DeathsAvertedDose4" = "#3C8EA2", 
                                    "DeathsAvertedDose5" = "#93C7CF", 
                                    "TotalAverted" = "black"),
                         labels = c("DeathsAvertedDose1" = "First dose", 
                                    "DeathsAvertedDose2" = "Second dose",
                                    "DeathsAvertedDose3" = "First booster", 
                                    "DeathsAvertedDose4" = "Second booster", 
                                    "DeathsAvertedDose5" = "Third booster", 
                                    "TotalAverted" = "Total")) +
      scale_x_discrete(breaks = week_breaks_long, labels = week_labels_long) +
      theme_bw() +
      theme(legend.text = element_text(size = text_size, colour = "black"),
            legend.title = element_text(size = text_size, colour = "black"),
            legend.position = "right",
            axis.text.x = element_blank(),
            axis.text = element_text(size = text_size, colour = "black"),
            axis.title = element_text(size = text_size, colour = "black"),
            strip.text = element_text(size = text_size, colour = "black"),
            strip.background = element_rect(fill = "white"),
            plot.margin = unit(c(0, 0.2, 0, 0.5), 'lines')) +
      labs(subtitle = "",
           x = "",
           y = "")
    
    p6.1 <- ggplot() +
      geom_segment(data = variant_waves_region, aes(x = start, xend = end, y = 28.000, yend = 28.000)) +
      geom_segment(data = variant_waves_region, aes(x = start, xend = start, y = 27.500, yend = 28.500)) +
      geom_text(data = variant_plot_timings, aes(x = Position, y = 29.500, label = Variant), size = 4) +
      geom_line(data = subset(lives_dose, TargetGroup == "≥80 years"), 
                aes(x = year_week, y = Counts/1000, group = Dosage, colour = Dosage), size = 1) +
      facet_wrap(.~ TargetGroup) +
      scale_color_manual(name = "Vaccine dose",
                         values = c("DeathsAvertedDose1" = "#a63022", 
                                    "DeathsAvertedDose2" = "#D6852B",
                                    "DeathsAvertedDose3" = "#1f497d", 
                                    "DeathsAvertedDose4" = "#3C8EA2", 
                                    "DeathsAvertedDose5" = "#93C7CF", 
                                    "TotalAverted" = "black"),
                         labels = c("DeathsAvertedDose1" = "First dose", 
                                    "DeathsAvertedDose2" = "Second dose",
                                    "DeathsAvertedDose3" = "First booster", 
                                    "DeathsAvertedDose4" = "Second booster", 
                                    "DeathsAvertedDose5" = "Third booster", 
                                    "TotalAverted" = "Total")) +
      scale_x_discrete(breaks = week_breaks_long, labels = week_labels_long) +
      theme_bw() +
      theme(legend.text = element_text(size = text_size, colour = "black"),
            legend.title = element_text(size = text_size, colour = "black"),
            legend.position = "right",
            axis.text.x = element_blank(),
            axis.text = element_text(size = text_size, colour = "black"),
            axis.title = element_text(size = text_size, colour = "black"),
            strip.text = element_text(size = text_size, colour = "black"),
            strip.background = element_rect(fill = "white"),
            plot.margin = unit(c(0, 0.2, 0, 0.5), 'lines')) +
      labs(subtitle = "",
           x = "",
           y = "")
    
    p1.2 <- ggplot() +
      geom_segment(data = variant_waves_region, aes(x = start, xend = end, y = 102, yend = 102)) +
      geom_segment(data = variant_waves_region, aes(x = start, xend = start, y = 100, yend = 104)) +
      geom_text(data = variant_plot_timings, aes(x = Position, y = 106, label = Variant), size = 4) +
      geom_bar(data = subset(expected_data_to_plot, TargetGroup == "25 to 49 years"),
               aes(x = year_week, y = Deaths, fill = Dosage), stat = "identity") +
      facet_wrap(.~ TargetGroup) +
      scale_fill_manual(values = c("pcDose1" = "#a63022",
                                   "pcDose2" = "#D6852B",
                                   "pcDose3" = "#1f497d",
                                   "pcDose4" = "#3C8EA2",
                                   "pcDose5" = "#93C7CF"),
                        labels = c("pcDose1" = "First dose",
                                   "pcDose2" = "Second dose",
                                   "pcDose3" = "First booster",
                                   "pcDose4" = "Second booster",
                                   "pcDose5" = "Third booster")) +
      scale_x_discrete(breaks = week_breaks_long, labels = week_labels_long) +
      theme_bw() +
      theme(legend.text = element_text(size = text_size, colour = "black"),
            legend.title = element_text(size = text_size, colour = "black"),
            legend.position = "right",
            axis.text = element_text(size = text_size, colour = "black"),
            axis.title = element_text(size = text_size, colour = "black"),
            axis.line = element_line(),
            strip.text = element_text(size = text_size, colour = "black"),
            strip.background = element_rect(fill = "white"),
            plot.margin = unit(c(0, 0.2, 0, 0.5), 'lines')) +
      labs(subtitle = "",
           x = "Calendar week",
           y = "Percentage (%)")
    
    p2.2 <- ggplot() +
      geom_segment(data = variant_waves_region, aes(x = start, xend = end, y = 102, yend = 102)) +
      geom_segment(data = variant_waves_region, aes(x = start, xend = start, y = 100, yend = 104)) +
      geom_text(data = variant_plot_timings, aes(x = Position, y = 106, label = Variant), size = 4) +
      geom_bar(data = subset(expected_data_to_plot, TargetGroup == "50 to 59 years"),
               aes(x = year_week, y = Deaths, fill = Dosage), stat = "identity") +
      facet_wrap(.~ TargetGroup) +
      scale_fill_manual(values = c("pcDose1" = "#a63022",
                                   "pcDose2" = "#D6852B",
                                   "pcDose3" = "#1f497d",
                                   "pcDose4" = "#3C8EA2",
                                   "pcDose5" = "#93C7CF"),
                        labels = c("pcDose1" = "First dose",
                                   "pcDose2" = "Second dose",
                                   "pcDose3" = "First booster",
                                   "pcDose4" = "Second booster",
                                   "pcDose5" = "Third booster")) +
      scale_x_discrete(breaks = week_breaks_long, labels = week_labels_long) +
      theme_bw() +
      theme(legend.text = element_text(size = text_size, colour = "black"),
            legend.title = element_text(size = text_size, colour = "black"),
            legend.position = "right",
            axis.text = element_text(size = text_size, colour = "black"),
            axis.title = element_text(size = text_size, colour = "black"),
            axis.line = element_line(),
            strip.text = element_text(size = text_size, colour = "black"),
            strip.background = element_rect(fill = "white"),
            plot.margin = unit(c(0, 0.2, 0, 0.5), 'lines')) +
      labs(subtitle = "",
           x = "Calendar week",
           y = "")
    
    p3.2 <- ggplot() +
      geom_segment(data = variant_waves_region, aes(x = start, xend = end, y = 102, yend = 102)) +
      geom_segment(data = variant_waves_region, aes(x = start, xend = start, y = 100, yend = 104)) +
      geom_text(data = variant_plot_timings, aes(x = Position, y = 106, label = Variant), size = 4) +
      geom_bar(data = subset(expected_data_to_plot, TargetGroup == "≥60 years"),
               aes(x = year_week, y = Deaths, fill = Dosage), stat = "identity") +
      facet_wrap(.~ TargetGroup) +
      scale_fill_manual(values = c("pcDose1" = "#a63022",
                                   "pcDose2" = "#D6852B",
                                   "pcDose3" = "#1f497d",
                                   "pcDose4" = "#3C8EA2",
                                   "pcDose5" = "#93C7CF"),
                        labels = c("pcDose1" = "First dose",
                                   "pcDose2" = "Second dose",
                                   "pcDose3" = "First booster",
                                   "pcDose4" = "Second booster",
                                   "pcDose5" = "Third booster")) +
      scale_x_discrete(breaks = week_breaks_long, labels = week_labels_long) +
      theme_bw() +
      theme(legend.text = element_text(size = text_size, colour = "black"),
            legend.title = element_text(size = text_size, colour = "black"),
            legend.position = "right",
            axis.text = element_text(size = text_size, colour = "black"),
            axis.title = element_text(size = text_size, colour = "black"),
            axis.line = element_line(),
            strip.text = element_text(size = text_size, colour = "black"),
            strip.background = element_rect(fill = "white"),
            plot.margin = unit(c(0, 0.2, 0, 0.5), 'lines')) +
      labs(subtitle = "",
           x = "Calendar week",
           y = "")
    
    
    p4.2 <- ggplot() +
      geom_segment(data = variant_waves_region, aes(x = start, xend = end, y = 102, yend = 102)) +
      geom_segment(data = variant_waves_region, aes(x = start, xend = start, y = 100, yend = 104)) +
      geom_text(data = variant_plot_timings, aes(x = Position, y = 106, label = Variant), size = 4) +
      geom_bar(data = subset(expected_data_to_plot, TargetGroup == "60 to 69 years"),
               aes(x = year_week, y = Deaths, fill = Dosage), stat = "identity") +
      facet_wrap(.~ TargetGroup) +
      scale_fill_manual(values = c("pcDose1" = "#a63022",
                                   "pcDose2" = "#D6852B",
                                   "pcDose3" = "#1f497d",
                                   "pcDose4" = "#3C8EA2",
                                   "pcDose5" = "#93C7CF"),
                        labels = c("pcDose1" = "First dose",
                                   "pcDose2" = "Second dose",
                                   "pcDose3" = "First booster",
                                   "pcDose4" = "Second booster",
                                   "pcDose5" = "Third booster")) +
      scale_x_discrete(breaks = week_breaks_long, labels = week_labels_long) +
      theme_bw() +
      theme(legend.text = element_text(size = text_size, colour = "black"),
            legend.title = element_text(size = text_size, colour = "black"),
            legend.position = "right",
            axis.text = element_text(size = text_size, colour = "black"),
            axis.title = element_text(size = text_size, colour = "black"),
            axis.line = element_line(),
            strip.text = element_text(size = text_size, colour = "black"),
            strip.background = element_rect(fill = "white"),
            plot.margin = unit(c(0, 0.2, 0, 0.5), 'lines')) +
      labs(subtitle = "",
           x = "Calendar week",
           y = "")
    
    p5.2 <- ggplot() +
      geom_segment(data = variant_waves_region, aes(x = start, xend = end, y = 102, yend = 102)) +
      geom_segment(data = variant_waves_region, aes(x = start, xend = start, y = 100, yend = 104)) +
      geom_text(data = variant_plot_timings, aes(x = Position, y = 106, label = Variant), size = 4) +
      geom_bar(data = subset(expected_data_to_plot, TargetGroup == "70 to 79 years"),
               aes(x = year_week, y = Deaths, fill = Dosage), stat = "identity") +
      facet_wrap(.~ TargetGroup) +
      scale_fill_manual(values = c("pcDose1" = "#a63022",
                                   "pcDose2" = "#D6852B",
                                   "pcDose3" = "#1f497d",
                                   "pcDose4" = "#3C8EA2",
                                   "pcDose5" = "#93C7CF"),
                        labels = c("pcDose1" = "First dose",
                                   "pcDose2" = "Second dose",
                                   "pcDose3" = "First booster",
                                   "pcDose4" = "Second booster",
                                   "pcDose5" = "Third booster")) +
      scale_x_discrete(breaks = week_breaks_long, labels = week_labels_long) +
      theme_bw() +
      theme(legend.text = element_text(size = text_size, colour = "black"),
            legend.title = element_text(size = text_size, colour = "black"),
            legend.position = "right",
            axis.text = element_text(size = text_size, colour = "black"),
            axis.title = element_text(size = text_size, colour = "black"),
            axis.line = element_line(),
            strip.text = element_text(size = text_size, colour = "black"),
            strip.background = element_rect(fill = "white"),
            plot.margin = unit(c(0, 0.2, 0, 0.5), 'lines')) +
      labs(subtitle = "",
           x = "Calendar week",
           y = "")
    
    p6.2 <- ggplot() +
      geom_segment(data = variant_waves_region, aes(x = start, xend = end, y = 102, yend = 102)) +
      geom_segment(data = variant_waves_region, aes(x = start, xend = start, y = 100, yend = 104)) +
      geom_text(data = variant_plot_timings, aes(x = Position, y = 106, label = Variant), size = 4) +
      geom_bar(data = subset(expected_data_to_plot, TargetGroup == "≥80 years"),
               aes(x = year_week, y = Deaths, fill = Dosage), stat = "identity") +
      facet_wrap(.~ TargetGroup) +
      scale_fill_manual(values = c("pcDose1" = "#a63022",
                                   "pcDose2" = "#D6852B",
                                   "pcDose3" = "#1f497d",
                                   "pcDose4" = "#3C8EA2",
                                   "pcDose5" = "#93C7CF"),
                        labels = c("pcDose1" = "First dose",
                                   "pcDose2" = "Second dose",
                                   "pcDose3" = "First booster",
                                   "pcDose4" = "Second booster",
                                   "pcDose5" = "Third booster")) +
      scale_x_discrete(breaks = week_breaks_long, labels = week_labels_long) +
      theme_bw() +
      theme(legend.text = element_text(size = text_size, colour = "black"),
            legend.title = element_text(size = text_size, colour = "black"),
            legend.position = "right",
            axis.text = element_text(size = text_size, colour = "black"),
            axis.title = element_text(size = text_size, colour = "black"),
            axis.line = element_line(),
            strip.text = element_text(size = text_size, colour = "black"),
            strip.background = element_rect(fill = "white"),
            plot.margin = unit(c(0, 0.2, 0, 0.5), 'lines')) +
      labs(subtitle = "",
           x = "Calendar week",
           y = "")
    
    #ggsave("Outputs/Deaths_averted_Figure_2.png", dpi = 300, width = 9, height = 4)
    
    p_vax_proportions <- ggarrange(p1.1, p2.1, p3.1, p4.1, p5.1, p6.1, p1.2, p2.2, p3.2, p4.2, p5.2, p6.2, 
                                   ncol = 6, nrow = 2,
                                   common.legend = TRUE, legend = "bottom", align = "v") 
    
    
    if(to.save == TRUE){
      ggsave("Outputs/WHO_EURO_Lives_saved_COVID_vaccines_Figure_2.png", dpi = 300, width = 14, height = 7)
    }
    
    return(p_vax_proportions)
    
  }
  
}