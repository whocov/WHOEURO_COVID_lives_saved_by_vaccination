# Script to create country specific outputs: data summaries, visuals

create.country.specific.summaries <- function(Expected_cases_all_ages,
                                              Country,
                                              summary_part){
  
# Part 1 - Data
  if(summary_part == "Part 1"){
    summary_data <- Expected_cases_all_ages %>% 
      filter(ReportCountry == Country) %>% 
      select(ReportCountry, TargetGroup, year_week,
             FirstDose, SecondDose, DoseAdditional1, DoseAdditional2, DoseAdditional3,
             DeathsObserved, Denominator, 
             DeathsAvertedDose1, DeathsAvertedDose2, DeathsAvertedDose3, DeathsAvertedDose4, DeathsAvertedDose5,
             TotalAverted, DeathsExpected)
    
    write.csv(summary_data, paste0("Outputs/Country_summaries/", Country, "_lives_saved_summary_data.csv"), row.names = FALSE)
  
    return(summary_data)
    
  }
  
# Part 2 - Figure 1
  if(summary_part == "Part 2"){
    
    
    if(Country %in% c("Germany", "Israel", "Republic of Moldova", "Ukraine")){
      Country_population <- Country_population %>% filter(CountryName == Country)
      variant_waves_region <- variant_waves %>% filter(ReportCountry == Country)
      deaths_age <- deaths_age %>% filter(ReportCountry == Country)
      cases <- cases %>% filter(CountryName == Country)
      vax_clean <- vax_clean %>% filter(ReportCountry == Country)
      Expected_cases_all_ages <- Expected_cases_all_ages %>% filter(ReportCountry == Country)
      
      country_figure <- create.summary.panel.plots(Country_population, 
                                                   variant_waves_region, 
                                                   cases,
                                                   deaths_age, 
                                                   vax_clean, 
                                                   Expected_cases_all_ages)

    } else {

    Country_population_finer <- Country_population_finer %>% filter(CountryName == Country)
    variant_waves_region <- variant_waves %>% filter(CountryName == Country)
    cases <- cases %>% filter(CountryName == Country)
    deaths_age_finer <- deaths_age_finer %>% filter(ReportCountry == Country)
    vax_clean_finer <- vax_clean_finer %>% filter(ReportCountry == Country)
    
    country_figure <- create.summary.panel.plots.finer.older(Country_population_finer, 
                                                             variant_waves_region, 
                                                             cases, 
                                                             deaths_age_finer, 
                                                             vax_clean_finer, 
                                                             to.save = FALSE)
    }
    
    
    ggsave(paste0("Outputs/Country_summaries/", country_figure, "_lives_saved_summary_figure.png"), dpi=300, height = 12, width = 14)
    
    return(country_figure)
    
  }
  
}
