# Script for functions
# Margaux Mesle - meslem@who.int
# June 2021

# Allow cumulative counting while keeping NAs
cumsum_keep_na <- function(x) {
  if (all(is.na(x))) {
    return(NA)
  } else {
    return(cumsum(x, na.rm=TRUE))
  }
}
# Allow counting while keeping NAs
sum_keep_na <- function(x) {
  if (all(is.na(x))) {
    return(NA)
  } else {
    return(sum(x, na.rm=TRUE))
  }
}

clean.population.data <- function(EU_population, UN_population){
  
  EU_population <- EU_population %>% 
    mutate(TargetGroup = paste0("Age", TargetGroup),
           TargetGroup = str_replace(TargetGroup, "-", "_"),
           TargetGroup = str_replace(TargetGroup, "yr", ""))
  
  population <- full_join(EU_population, UN_population) %>% 
    select(-Original_updated) %>% 
    rename(Denominator = Population)
  
    return(population)
}



clean.tessy.country.name <- function(country_names, data_source) {
  
  # define functions -----------------------------------------------------------
  
  # categorise UK nation based on CountryName and DataSource columns
  categorise.UK.nation <- function(country_names, data_source) {
    
    country_ids <- c("United Kingdom, England", "United Kingdom, Scotland", "United Kingdom, Wales", 
                     "United Kingdom, Northern Ireland")
    
    country_source_terms <- c("^UK-ENG", "^UK-SCOT", "^UK-WALES", "^UK-NI")
    
    for (i in 1:length(country_ids)) {
      contains_country <- str_detect(data_source, country_source_terms[i])
      country_names <- if_else(contains_country, country_ids[i], country_names)
    }
    
    return(country_names)
  }
  
  # transform data -------------------------------------------------------------
  cleaned_country_names <- categorise.UK.nation(country_names, data_source)
  cleaned_country_names <- recode(cleaned_country_names, 
                                  "Kosovo" = "Kosovo (in accordance with Security Council resolution 1244 (1999))",
                                  "Russia" = "Russian Federation",
                                  "Republic of Moldova" = "Republic of Republic of Moldova",
                                  "Republic of North Macedonia" = "North Macedonia"
  )
  
  return(cleaned_country_names)
}


find.start.vaccination.dates <- function (vax_clean) {
  
  start_dates <- vax_clean %>%
    filter(ReportCountry %in% countries_included$CountryName) %>% 
    dplyr::select(ReportCountry, year_week, FirstDose) %>%
    mutate(FirstDose = ifelse(is.na(FirstDose), 0, FirstDose)) %>%
    filter(FirstDose > 0) %>%
    arrange(ReportCountry, year_week) %>%
    group_by(ReportCountry) %>%
    slice(1) %>% 
    mutate(Dose = "start")
  
  pp10_dates <- vax_clean %>%
    filter(ReportCountry %in% countries_included$CountryName) %>% 
    dplyr::select(ReportCountry, year_week, FirstDose, Denominator) %>%
    mutate(FirstDose = ifelse(is.na(FirstDose), 0, FirstDose)) %>%
    arrange(ReportCountry, year_week) %>%
    group_by(ReportCountry) %>%
    mutate(dose.pp = round((cumsum(FirstDose)/Denominator)*100,1)) %>%
    filter(dose.pp > 10) %>%
    slice(1) %>%
    mutate(Dose = "x10pp1dose")
  
  start_full_vax_dates <- vax_clean %>%
    filter(ReportCountry %in% countries_included$CountryName) %>% 
    dplyr::select(ReportCountry, year_week, SecondDose, Denominator) %>%
    mutate(SecondDose = ifelse(is.na(SecondDose), 0, SecondDose)) %>%
    arrange(ReportCountry, year_week) %>%
    group_by(ReportCountry) %>%
    mutate(dose.pp = round((cumsum(SecondDose)/Denominator)*100,1)) %>%
    filter(dose.pp > 10) %>%
    slice(1) %>%
    mutate(Dose = "y10pp2dose")
  
  start_additional1_dates <- vax_clean %>%
    filter(ReportCountry %in% countries_included$CountryName) %>% 
    dplyr::select(ReportCountry, year_week, DoseAdditional1, Denominator) %>%
    mutate(DoseAdditional1 = ifelse(is.na(DoseAdditional1), 0, DoseAdditional1)) %>%
    arrange(ReportCountry, year_week) %>%
    group_by(ReportCountry) %>%
    mutate(dose.pp = round((cumsum(DoseAdditional1)/Denominator)*100,1)) %>%
    filter(dose.pp > 10) %>%
    slice(1) %>%
    mutate(Dose = "z10pp3dose")
  
  start_additional2_dates <- vax_clean %>%
    filter(ReportCountry %in% countries_included$CountryName) %>% 
    dplyr::select(ReportCountry, year_week, DoseAdditional2, Denominator) %>%
    mutate(DoseAdditional2 = ifelse(is.na(DoseAdditional2), 0, DoseAdditional2)) %>%
    arrange(ReportCountry, year_week) %>%
    group_by(ReportCountry) %>%
    mutate(dose.pp = round((cumsum(DoseAdditional2)/Denominator)*100,1)) %>%
    filter(dose.pp > 10) %>%
    slice(1) %>%
    mutate(Dose = "z10pp4dose")
  
  start_additional3_dates <- vax_clean %>%
    filter(ReportCountry %in% countries_included$CountryName) %>% 
    dplyr::select(ReportCountry, year_week, DoseAdditional3, Denominator) %>%
    mutate(DoseAdditional3 = ifelse(is.na(DoseAdditional3), 0, DoseAdditional3)) %>%
    arrange(ReportCountry, year_week) %>%
    group_by(ReportCountry) %>%
    mutate(dose.pp = round((cumsum(DoseAdditional3)/Denominator)*100, 1)) %>%
    filter(dose.pp > 10) %>%
    slice(1) %>%
    mutate(Dose = "z10pp5dose")
  
  start_dates_total <- rbind(pp10_dates, start_full_vax_dates, start_additional1_dates, start_additional2_dates, start_additional3_dates) %>%
    select(ReportCountry, year_week, Dose)
  
  vax_start_dates <- full_join(start_dates, start_dates_total) %>%
    dplyr::select(-c(FirstDose))
  
  return(vax_start_dates)
}

find.start.vaccination.dates.by.dose <- function (vax_clean) {
  
  start_dates <- vax_clean %>%
    dplyr::select(report_country, year_week, FirstDose, SecondDose, DoseAdditional1, DoseAdditional2, DoseAdditional3) %>%
    mutate(FirstDose = ifelse(is.na(FirstDose), 0, FirstDose),
           SecondDose = ifelse(is.na(SecondDose), 0, SecondDose),
           DoseAdditional1 = ifelse(is.na(DoseAdditional1), 0, DoseAdditional1),
           DoseAdditional2 = ifelse(is.na(DoseAdditional2), 0, DoseAdditional2),
           DoseAdditional3 = ifelse(is.na(DoseAdditional3), 0, DoseAdditional3)) %>%
    arrange(report_country, year_week) %>%
    distinct() %>%
    pivot_longer(cols = "FirstDose" : "DoseAdditional3",
                 names_to = "Dose",
                 values_to = "Amount") %>%
    filter(Amount >0 ) %>%
    group_by(report_country, Dose) %>%
    mutate(start = first(year_week)) %>%
    ungroup() %>%
    select(report_country, Dose, start) %>%
    distinct() 
  

  return(start_dates)
}


# Number of deaths by country, age and week
calculate.percentage.deaths.over60 <- function(deahts) {
  
  under.60 <- c("00.04", "05.09", "10.14", "15.19", "20.24", "25.29", "30.39","40.49", "50.59")
  over.60 <- c("60.64", "65.69", "70.74", "75.79", "80.")
  
  deaths_over_60_pc <- deaths %>%
    mutate(CountryName=ifelse(grepl("^UK-ENG_*", DataSource), "United Kingdom (England)", CountryName)) %>%
    mutate(CountryName=ifelse(grepl("^UK-NI_*", DataSource), "United Kingdom (Northern Ireland)", CountryName)) %>%
    mutate(CountryName=ifelse(grepl("^UK-SCOT_*", DataSource), "United Kingdom (Scotland)", CountryName)) %>%
    mutate(CountryName=ifelse(grepl("^UK-WALES_*", DataSource), "United Kingdom (Wales)", CountryName)) %>%
    # Filter data down to needed columns
    dplyr::select(c(CountryName, ReportingCountry, DateUsedForStatisticsWeek) | (starts_with("Deaths"))) %>%
    # melt data frame    
    pivot_longer(cols=13:43) %>% 
    rename(year_week=DateUsedForStatisticsWeek,
           age_group=name,
           DeathsObserved=value) %>%
    dplyr::select(-c(`Deaths00.04`, `Deaths05.14`, `Deaths15.24`, `Deaths25.49`, `Deaths50.64`, `Deaths65.79`,`Deaths80.`, 
                     `DeathsHCW`, `DeathsUNK`)) %>%
    mutate(year_week=str_replace(year_week, "'", ""),
           age_group=str_replace(age_group, "Deaths", ""),
           age_group=str_replace(age_group, "\\D$", "")) %>%
    dplyr::select(-ReportingCountry) %>%
    mutate(age_group=ifelse(age_group %in% under.60, 'under60', age_group),
           age_group=ifelse(age_group %in% over.60, 'over60', age_group)) %>%
    group_by(age_group) %>%
    summarise(deaths_total=sum_keep_na(DeathsObserved)) %>%
    mutate(deaths_pc=(deaths_total/(sum(deaths_total)))*100) %>%
    ungroup()
  
  return(deaths_over_60_pc)
}

calculate.percentage.deaths.by.age <- function(deahts) {
  
  deaths_over_60 <- deaths %>%
    mutate(CountryName = ifelse(grepl("^UK-ENG_*", DataSource), "United Kingdom (England)", CountryName)) %>%
    mutate(CountryName = ifelse(grepl("^UK-NI_*", DataSource), "United Kingdom (Northern Ireland)", CountryName)) %>%
    mutate(CountryName = ifelse(grepl("^UK-SCOT_*", DataSource), "United Kingdom (Scotland)", CountryName)) %>%
    mutate(CountryName = ifelse(grepl("^UK-WALES_*", DataSource), "United Kingdom (Wales)", CountryName)) %>%
    # Filter data down to needed columns
    dplyr::select(c(CountryName, ReportingCountry, DateUsedForStatisticsWeek) | (starts_with("Deaths"))) %>%
    # melt data frame    
    pivot_longer(cols = "Deaths20.24F":"Deaths80.M") %>% 
    rename(year_week = DateUsedForStatisticsWeek,
           age_group = name,
           DeathsObserved = value) %>%
    dplyr::select(CountryName, year_week, age_group, DeathsObserved) %>%
    mutate(year_week = str_replace(year_week, "'", ""),
           age_group = str_replace(age_group, "Deaths", ""),
           age_group = str_replace(age_group, "\\D$", "")) %>%
    mutate(age_group = case_when((age_group == "20.24" | age_group == "25.29") ~'20-29',
                                 (age_group == "30.39" ) ~'30-39',
                                 (age_group == "40.49" ) ~'40-49',
                                 (age_group == "50.59" ) ~'50-59',
                                 (age_group == "60.64" | age_group == "65.69") ~'60-69',
                                 (age_group == "70.74" | age_group == "75.79") ~'70-79',
                                 (age_group == "80.") ~'80+')) %>%
    mutate(CountryName = recode(CountryName, "Republic of Moldova" = "Republic of Republic of Moldova")) %>%
    group_by(CountryName, year_week, age_group) %>%
    summarise(DeathsObserved = sum_keep_na(DeathsObserved)) %>%
    mutate(deaths_pc = round((DeathsObserved/(sum(DeathsObserved)))*100)) %>%
    ungroup()
  
  return(deaths_over_60)
}


determine.countries.included <- function(deaths_age, vax_clean) {
  
  country_list_deaths = deaths_age %>%
    summarise(unique(CountryName)) %>% 
    rename(countries = `unique(CountryName)`)
  
    # Not all countries have data refined to smaller age groups - take these out before running analysis
    countries.avoid <- c("Azerbaijan", "Georgia", "Kazakhstan", "Republic of Republic of Moldova", "Romania", "Serbia", "Ukraine", "Uzbekistan")
    country_list_deaths <- country_list_deaths %>%
      filter(!countries%in%countries.avoid) 

  country_list_vax = vax_clean %>%
    filter(!targetgroup == "ALL") %>%
    mutate(report_country = ifelse(report_country == "United Kingdom, Scotland", "United Kingdom (Scotland)", report_country),
           report_country = ifelse(report_country == "United Kingdom, England", "United Kingdom (England)", report_country),
           report_country = ifelse(report_country == "United Kingdom, Wales", "United Kingdom (Wales)", report_country),
           report_country = ifelse(report_country == "United Kingdom, Northern Ireland", "United Kingdom (Northern Ireland)", report_country)) %>%
    summarise(unique(report_country)) %>% 
    rename(countries = `unique(report_country)`)
  
  
  country_list <- inner_join(country_list_deaths, country_list_vax) %>%
    arrange(countries) 
  
  return(country_list)
}


find.vaccine.brand.per.country <- function(vaccine, country_codes, countries_included) {
  
   # Which vaccines are used in which countries - this becomes a column in Table 1
  selected_data <- vaccine %>% 
    select(DateUsedForStatisticsISO, Region, ReportingCountry, TargetGroup, Vaccine) %>% 
    mutate(year_week = str_replace(DateUsedForStatisticsISO, "-W", "-"),
           Period = ifelse(year_week %in% omicron.weeks, "Omicron", "Pre-Omicron")) %>% 
    mutate(ReportingCountry = ifelse(Region == "UK_ENG", "UKEngland",
                                     ifelse(Region == "UKM", "UKScotland",
                                            ifelse(Region == "UKN", "UKNI",
                                                   ifelse(Region == "UKL", "UKWales", ReportingCountry))))) %>% 
    select(- Region)  %>% 
    filter(TargetGroup %in% c("Age25_49", "Age50_59", "1_Age60+"))
    

  selected_data <- left_join(selected_data, select(country_codes, ReportingCountry, report_country), 
                             by = c("ReportingCountry" = "ReportingCountry"))
  
  selected_data <- selected_data %>% 
    mutate(pretty_vaccine = recode(Vaccine, 
                                   COM = "Pfizer BioNTech - Comirnaty",
                                   MOD = "Moderna - mRNA-1273", 
                                   UNK = "Unknown product", 
                                   AZ = "AstraZeneca - Vaxzevria", 
                                   CN = "BBIBV-CorV - CNBG",
                                   WUCNBG = "Wuhan CNBG - Inactivated", 
                                   SIICOV = "SII - Covishield", 
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
                                   "VLA"  = "Valneva",  #COVID-19 Vaccine (inactivated, adjuvanted) Valneva – Valneva
                                   "COMBA.1" = "Comirnaty – Pfizer/BioNTech Original/Omicron BA.1", # Comirnaty – Pfizer/BioNTech Original/Omicron BA.1
                                   "COMBA.4-5" = "Comirnaty – Pfizer/BioNTech Original/Omicron BA.4-5", # Comirnaty – Pfizer/BioNTech Original/Omicron BA.4-5
                                   "MODBA.1" = "Moderna bivalent Original",  # Spikevax Moderna bivalent Original/Omicron BA.1
                                   "MODBA.4-5" = "Moderna bivalent Original",
                                   SGSK = "Sanofi–GSK")) %>% 
    select(ReportingCountry, Period, TargetGroup, pretty_vaccine) %>% 
    distinct() %>% 
    group_by(ReportingCountry, Period, TargetGroup) %>%
    summarise(Vax = n_distinct(pretty_vaccine),
              vax_list_short = paste0(sort(unique(vaccine)), collapse = "; "),
              vax_list = paste0(sort(unique(pretty_vaccine)), collapse = "; ")) %>% 
    ungroup() %>% 
    mutate(vax_list_short = ifelse(ReportingCountry == "France", "AZ; COM; JANSS; MOD; NVXD", vax_list_short),
           vax_list = ifelse(ReportingCountry == "France", "AstraZeneca - Vaxzevria; Pfizer BioNTech - Comirnaty; Janssen - Ad26.COV 2-S; Moderna - mRNA-1273; Novavax", vax_list),
           vax_list_short = ifelse(ReportingCountry == "Germany", "AZ; COM; JANSS; MOD; NVXD", vax_list_short),
           vax_list = ifelse(ReportingCountry == "Germany", "AstraZeneca - Vaxzevria; Pfizer BioNTech - Comirnaty; Janssen - Ad26.COV 2-S; Moderna - mRNA-1273; Novavax", vax_list)) %>%
    mutate(vax_list_short = str_remove(vax_list_short, "; UNK"),
           vax_list = str_remove(vax_list, "; UNK")) %>%  # Remove 'Unknown product' if country has other vaccines listed
    mutate(ReportingCountry = ifelse(ReportingCountry == "United Kingdom, England", "United Kingdom (England)", ReportingCountry),
           ReportingCountry = ifelse(ReportingCountry == "United Kingdom, Scotland", "United Kingdom (Scotland)", ReportingCountry),
           ReportingCountry = ifelse(ReportingCountry == "United Kingdom, Wales", "United Kingdom (Wales)", ReportingCountry),
           ReportingCountry = ifelse(str_detect(ReportingCountry, "^Kosovo*"), "Kosovo", ReportingCountry)) 
  
  scotland_vaccinations <- selected_data %>% 
    filter(ReportingCountry == "United Kingdom (Scotland)") %>% 
    pull(vax_list)
    
  selected_data <- selected_data %>% 
    mutate(Vax = ifelse(ReportingCountry == "United Kingdom (England)" & Vax == 1, 3, Vax),
           vax_list = ifelse(ReportingCountry == "United Kingdom (England)", scotland_vaccinations, vax_list)) %>% 
    rename(ReportCountry = ReportingCountry)
  
  
 return(selected_data)
}


prepare.data.for.tables <- function(age) {
  # Tidy up weekly results table to calculate number of deaths averted by age and mortality rate
  # This is used to create Tables 1 and 2
  age <- age %>% 
    select(-X) %>% 
    group_by(report_country) %>%
    mutate(DeathsAverted = round(DeathsExpected-DeathsObserved)) %>%
    mutate(uptakesecond = round(uptakesecond*100)) %>%
    summarise(dosefirst = last(dosefirst),
              dosesecond = last(dosesecond),
              uptakefirst = last(uptakefirst),
              uptakesecond = last(uptakesecond),
              DeathsObserved = sum(DeathsObserved),
              DeathsAvertedDose1 = round(sum_keep_na(DeathsAvertedDose1)),
              DeathsAvertedDose2 = round(sum_keep_na(DeathsAvertedDose2)),
              DeathsAverted = sum(DeathsAvertedDose1 + DeathsAvertedDose2, na.rm = TRUE),
              denominator = unique(denominator)) %>%
    mutate(Mortality.Rate = round((DeathsObserved/denominator)*100000, 2),
           Expected.Mortality.Rate = round(((DeathsObserved + DeathsAverted)/denominator)*100000, 2))  
  
  return(age)
}


calculate.variant.waves <- function(variants, variants_GISAID, variant_lookup, weeks_template, resolution){
  
  names(variants_GISAID) <- c("CountryName", "Variant", "start", "end")
  
  omicron_weeks <- c("2021-49", "2021-50", "2021-51", "2021-52")
  delta_weeks <- c("2020-40", "2020-41", "2020-42", "2020-43", "2020-44", "2020-45", "2020-46", "2020-47", "2020-48", "2020-49",
                   "2020-50", "2020-51", "2020-52", "2020-53")

    
  if(resolution == "Country"){
    
    # Calculate variant dominance (at least 50%) per week and country from TESSy data
    variants_clean <- variants %>%
      mutate(CountryName = ifelse(str_detect(DataSource, "UK-WALES"), "United Kingdom (Wales)", CountryName),
             CountryName = ifelse(str_detect(DataSource, "UK-ENG"), "United Kingdom (England)", CountryName),
             CountryName = ifelse(str_detect(DataSource, "UK-SCOT"), "United Kingdom (Scotland)", CountryName),
             CountryName = ifelse(str_detect(DataSource, "UK-NI"), "United Kingdom (Northern Ireland)", CountryName)) %>%
      filter(complete_denom == TRUE) %>%
      mutate(year_week = str_replace(DateUsedForStatisticsWeek, "'", ""),
             year = substr(year_week, 1, 4)) %>%
      select(CountryName, year_week, year, variant_out, value, proportion_denominator)
    
    # Add country names
    variants_clean$Variant <- variant_lookup$WHOLabel[match(variants_clean$variant_out, variant_lookup$variant_out)]
    
    # Tidy up variant names
    variants_clean <- variants_clean %>%
      filter(CountryName %in% countries_included$CountryName) %>%
      mutate(Variant = ifelse(variant_out == "SGTF" & (year_week %in% omicron_weeks | year == 2022), "Omicron", Variant),
             Variant = ifelse(variant_out == "B.1.1.7" & year == 2020, "Other", Variant),
             Variant = ifelse(variant_out == "B.1.1.529" & !(year_week %in% omicron_weeks | year == 2022), "Other", Variant),
             Variant = ifelse(variant_out == "B.1.617.2" & !(year_week %in% delta_weeks | year == 2021), "Other", Variant),
             Variant = ifelse(variant_out == "Other", "Other", Variant),
             Variant = ifelse(variant_out == "B.1.1.7+E484K", "Alpha", Variant)) %>% 
      filter(!Variant == "")
    
    full_template <- expand.grid(CountryName = unique(countries_included$CountryName),
                                 year_week = weeks_template$year_week) %>% 
      filter(!CountryName %in% countries_no_reporting)
    
    # Calculate percentages
    variants_clean <- variants_clean %>%
      group_by(CountryName, Variant, proportion_denominator, year_week) %>%
      summarise(n_variants = sum_keep_na(value)) %>%
      mutate(Variant_pc = ifelse(proportion_denominator >= 10, round((n_variants/proportion_denominator)*100), 0)) %>%
      ungroup() %>% 
      full_join(full_template) %>% 
      arrange(CountryName, year_week) %>% 
      mutate(Variant = ifelse(is.na(Variant) & str_detect(year_week, "2020-*"), "Index", Variant),
             Variant_pc = ifelse(is.na(Variant_pc) & str_detect(year_week, "2020-*"), 0, Variant_pc))
    

    # Filter to data with at least 50% positivity
    variant_waves <- variants_clean %>%
      filter(Variant %in% c("Index", "Alpha", "Delta", "Omicron")) %>%
      mutate(Variant = factor(Variant, levels = c("Index", "Alpha", "Delta", "Omicron"))) %>% 
      arrange(CountryName, Variant, year_week) %>%
      filter(Variant_pc >= 50) %>% 
      rename(n_viruses = proportion_denominator)
    
    
    # Finding start week
    variant_start <- variant_waves %>%
      group_by(CountryName, Variant) %>%
      slice(1) %>% 
      ungroup() %>%
      group_by(CountryName, Variant) %>%
      mutate(start = first(year_week)) %>%
      ungroup() %>%
      select(-c(year_week, n_variants, n_viruses, Variant_pc)) %>%
      distinct() %>%
      arrange(CountryName, Variant)
    
    # Finding end week
    variant_end <- variant_waves %>%
      arrange(CountryName, Variant, year_week) %>%
      group_by(CountryName, Variant) %>%
      slice(tail(row_number(), 1)) %>% 
      ungroup() %>%
      group_by(CountryName, Variant) %>%
      mutate(end = first(year_week),
             end = ifelse(Variant == "Omicron", end.week, end)) %>%
      ungroup() %>%
      select(-c(year_week, n_variants, n_viruses, Variant_pc)) %>%
      distinct() %>%
      #rename(report_country=CountryName) %>% 
      arrange(CountryName, Variant)
    
    # Combining all together
    variant_timing <- full_join(variant_start, variant_end)
    
    # Adding missing countries (from GISAID)
    variants_GISAID_restricted <- variants_GISAID %>% 
      filter(CountryName %in% c("Czechia", "Israel", "Germany", "Italy", "Switzerland", "North Macedonia", "Republic of Moldova",
                                "United Kingdom (England)", "United Kingdom (Scotland)", "United Kingdom (Wales)", "Portugal"))
    
    add_index_GISAID <- expand.grid(CountryName = unique(variants_GISAID_restricted$CountryName),
                                    Variant = "Index",
                                    start = "2020-01")
    
    variants_GISAID_restricted <- full_join(variants_GISAID_restricted, add_index_GISAID)
    
    # Final table
    variant_waves <- variant_timing %>% 
      filter(!CountryName %in% variants_GISAID_restricted$CountryName) %>% 
      filter(!CountryName == "Albania") %>% 
      full_join(variants_GISAID_restricted) %>% 
      add_row(CountryName = "North Macedonia", Variant = "Alpha", start = "2021-01", end = "2021-33") %>% 
      add_row(CountryName = "Kosovo", Variant = "Alpha", start = "2021-01", end = "2021-27") %>%
      add_row(CountryName = "France", Variant = "Omicron", start = "2021-52", end = end.week) %>% 
      add_row(CountryName = "France", Variant = "Omicron", start = "2021-52", end = end.week) %>% 
      mutate(start = ifelse(CountryName == "Kosovo" & Variant == "Delta", "2021-28", start),
             start = ifelse(CountryName == "Austria" & Variant == "Delta", "2021-25", start),
             start = ifelse(CountryName == "Cyprus" & Variant == "Delta", "2021-24", start),
             start = ifelse(CountryName == "Spain" & Variant == "Omicron", "2021-50", start),
             start = ifelse(CountryName == "Sweden" & Variant == "Delta", "2021-26", start)) %>% 
      mutate(end = ifelse(CountryName == "Czechia" & Variant == "Index", "2021-19", end),
             end = ifelse(CountryName == "Germany" & Variant == "Index", "2020-52", end),
             end = ifelse(CountryName == "Israel" & Variant == "Index", "2021-08", end),
             end = ifelse(CountryName == "Czechia" & Variant == "Index", "2021-02", end),
             end = ifelse(CountryName == "Italy" & Variant == "Index", "2021-07", end),
             end = ifelse(CountryName == "Switzerland" & Variant == "Index", "2021-07", end),
             end = ifelse(CountryName == "Portugal" & Variant == "Index", "2021-07", end),
             end = ifelse(CountryName == "United Kingdom (England)" & Variant == "Index", "2020-51", end),
             end = ifelse(CountryName == "United Kingdom (Scotland)" & Variant == "Index", "2020-51", end),
             end = ifelse(CountryName == "United Kingdom (Wales)" & Variant == "Index", "2020-51", end),
             end = ifelse(CountryName == "Lithuania" & Variant == "Delta", "2021-50", end),
             end = ifelse(CountryName == "Estonia" & Variant == "Delta", "2021-50", end))
    
    
    
  additional_countries <- expand.grid(CountryName = c("Albania", "Republic of Moldova"), 
                                      Variant = c("Index", "Alpha", "Delta", "Omicron"),
                                      start = "",
                                      end = "") %>%
    arrange(CountryName) %>%
    mutate(start = ifelse(CountryName == "Albania" & Variant=="Index", "2020-01", start),
           end = ifelse(CountryName == "Albania" & Variant == "Index", "2021-08", end),
           start = ifelse(CountryName == "Albania" & Variant == "Alpha", "2021-09", start),
           end = ifelse(CountryName == "Albania" & Variant == "Alpha", "2021-28", end),
           start = ifelse(CountryName == "Albania" & Variant == "Delta", "2021-29", start),
           end = ifelse(CountryName == "Albania" & Variant == "Delta", "2022-03", end),
           start = ifelse(CountryName == "Albania" & Variant == "Omicron", "2022-04", start),
           end = ifelse(CountryName == "Albania" & Variant == "Omicron", end.week, end)) %>% 
    mutate(start = ifelse(CountryName == "Republic of Moldova" & Variant=="Index", "2020-01", start),
           end = ifelse(CountryName == "Republic of Moldova" & Variant == "Index", "2021-09", end),
           start = ifelse(CountryName == "Republic of Moldova" & Variant == "Alpha", "2021-10", start),
           end = ifelse(CountryName == "Republic of Moldova" & Variant == "Alpha", "2021-24", end),
           start = ifelse(CountryName == "Republic of Moldova" & Variant == "Delta", "2021-29", start),
           end = ifelse(CountryName == "Republic of Moldova" & Variant == "Delta", "2022-01", end),
           start = ifelse(CountryName == "Republic of Moldova" & Variant == "Omicron", "2022-02", start),
           end = ifelse(CountryName == "Republic of Moldova" & Variant == "Omicron", end.week, end))
  
  
  
  variant_waves <- variant_waves %>% 
    filter(!CountryName %in% additional_countries$report_country) %>% 
    full_join(additional_countries) %>% 
    mutate(end = ifelse(Variant == "Omicron" & end != end.week , end.week, end)) %>% 
    mutate(Variant = factor(Variant, levels = c("Index", "Alpha", "Delta", "Omicron"))) %>% 
    arrange(CountryName, Variant) %>% 
    rename(ReportCountry = CountryName) %>% 
    distinct()
  

  return(variant_waves)
  
  }
  
  if(resolution == "Region"){
    
    # Calculate variant dominance (at least 50%) per week and country from TESSy data
    variants_clean <- variants %>%
      mutate(CountryName = ifelse(str_detect(DataSource, "UK-WALES"), "United Kingdom (Wales)", CountryName),
             CountryName = ifelse(str_detect(DataSource, "UK-ENG"), "United Kingdom (England)", CountryName),
             CountryName = ifelse(str_detect(DataSource, "UK-SCOT"), "United Kingdom (Scotland)", CountryName),
             CountryName = ifelse(str_detect(DataSource, "UK-NI"), "United Kingdom (Northern Ireland)", CountryName)) %>%
      filter(complete_denom == TRUE) %>%
      mutate(year_week = str_replace(DateUsedForStatisticsWeek, "'", ""),
             year = substr(year_week, 1, 4)) %>%
      select(CountryName, year_week, year, variant_out, value, proportion_denominator)
    
    # Add country names
    variants_clean$Variant <- variant_lookup$WHOLabel[match(variants_clean$variant_out, variant_lookup$variant_out)]
    
    # Tidy up variant names
    variants_clean <- variants_clean %>%
      filter(CountryName %in% countries_included$CountryName) %>%
      mutate(Variant = ifelse(variant_out == "SGTF" & (year_week %in% omicron_weeks | year == 2022), "Omicron", Variant),
             Variant = ifelse(variant_out == "B.1.1.7" & year == 2020, "Other", Variant),
             Variant = ifelse(variant_out == "B.1.1.529" & !(year_week %in% omicron_weeks | year == 2022), "Other", Variant),
             Variant = ifelse(variant_out == "B.1.617.2" & !(year_week %in% delta_weeks | year == 2021), "Other", Variant),
             Variant = ifelse(variant_out == "Other", "Other", Variant),
             Variant = ifelse(variant_out == "B.1.1.7+E484K", "Alpha", Variant)) %>% 
      filter(!Variant == "")
    

    # Calculate percentages
    variants_clean <- variants_clean %>%
      group_by(Variant, year_week) %>%
      summarise(n_variants = sum_keep_na(value),
                n_viruses = sum_keep_na(proportion_denominator)) %>%
      mutate(n_viruses = ifelse(is.na(n_viruses), 0, n_viruses),
             Variant_pc = ifelse(n_viruses >= 100, round((n_variants/n_viruses)*100), 0)) %>%
      ungroup() %>% 
      arrange(year_week) %>% 
      mutate(Variant = ifelse(is.na(Variant) & str_detect(year_week, "2020-*"), "Index", Variant),
             Variant_pc = ifelse(is.na(Variant_pc) & str_detect(year_week, "2020-*"), 100, Variant_pc))
    
    # Filter to data with at least 50% positivity
    variant_waves <- variants_clean %>%
      filter(Variant %in% c("Index", "Alpha", "Delta", "Omicron")) %>%
      mutate(Variant = factor(Variant, levels = c("Index", "Alpha", "Delta", "Omicron"))) %>% 
      arrange(Variant, year_week) %>%
      filter(Variant_pc >= 50) 
    
    
    # Finding start week
    variant_start <- variant_waves %>%
      group_by(Variant) %>%
      slice(1) %>% 
      ungroup() %>%
      group_by(Variant) %>%
      mutate(start = first(year_week)) %>%
      ungroup() %>%
      select(-c(year_week, n_variants, n_viruses, Variant_pc)) %>%
      distinct() %>%
      arrange(Variant)
    
    # Finding end week
    variant_end <- variant_waves %>%
      arrange(Variant, year_week) %>%
      group_by(Variant) %>%
      slice(tail(row_number(), 1)) %>% 
      ungroup() %>%
      group_by(Variant) %>%
      mutate(end = first(year_week),
             end = ifelse(Variant == "Omicron", end.week, end)) %>%
      ungroup() %>%
      select(-c(year_week, n_variants, n_viruses, Variant_pc)) %>%
      distinct() %>%
      #rename(report_country=CountryName) %>% 
      arrange(Variant)
    
    # Combining all together
    variant_timing <- full_join(variant_start, variant_end) %>% 
      add_row(Variant = "Index", start = "2020-01", end = "2020-53") %>% 
      add_row(Variant = "Omicron", start = "2021-50", end = end.week)
    
    variant_waves <- variant_timing %>% 
      mutate(Variant = factor(Variant, levels = c("Index", "Alpha", "Delta", "Omicron"))) %>% 
      arrange(Variant) 
    
    return(variant_waves)
    
  }

  if(resolution == "By month"){
    
    variants_clean <- variants %>%
      mutate(CountryName = ifelse(str_detect(DataSource, "UK-WALES"), "United Kingdom (Wales)", CountryName),
             CountryName = ifelse(str_detect(DataSource, "UK-ENG"), "United Kingdom (England)", CountryName),
             CountryName = ifelse(str_detect(DataSource, "UK-SCOT"), "United Kingdom (Scotland)", CountryName),
             CountryName = ifelse(str_detect(DataSource, "UK-NI"), "United Kingdom (Northern Ireland)", CountryName)) %>%
      filter(complete_denom == TRUE) %>%
      mutate(year_week = str_replace(DateUsedForStatisticsWeek, "'", ""),
             year = substr(year_week, 1, 4)) %>%
      select(CountryName, year_week, year, variant_out, value, proportion_denominator)
    
    # Add country names
    variants_clean$Variant <- variant_lookup$WHOLabel[match(variants_clean$variant_out, variant_lookup$variant_out)]
    
    # Tidy up variant names
    variants_clean <- variants_clean %>%
      filter(CountryName %in% countries_included$CountryName) %>%
      mutate(Variant = ifelse(variant_out == "SGTF" & (year_week %in% omicron_weeks | year == 2022), "Omicron", Variant),
             Variant = ifelse(variant_out == "B.1.1.7" & year == 2020, "Other", Variant),
             Variant = ifelse(variant_out == "B.1.1.529" & !(year_week %in% omicron_weeks | year == 2022), "Other", Variant),
             Variant = ifelse(variant_out == "B.1.617.2" & !(year_week %in% delta_weeks | year == 2021), "Other", Variant),
             Variant = ifelse(variant_out == "Other", "Other", Variant),
             Variant = ifelse(variant_out == "B.1.1.7+E484K", "Alpha", Variant)) %>% 
      filter(!Variant == "")
    
    
    # Calculate percentages
    variants_clean <- variants_clean %>%
      group_by(Variant, year_week) %>%
      summarise(n_variants = sum_keep_na(value),
                n_viruses = sum_keep_na(proportion_denominator)) %>%
      mutate(n_viruses = ifelse(is.na(n_viruses), 0, n_viruses),
             Variant_pc = ifelse(n_viruses >= 100, round((n_variants/n_viruses)*100), 0)) %>%
      ungroup() %>% 
      arrange(year_week) %>% 
      mutate(Variant = ifelse(is.na(Variant) & str_detect(year_week, "2020-*"), "Index", Variant),
             Variant_pc = ifelse(is.na(Variant_pc) & str_detect(year_week, "2020-*"), 100, Variant_pc))
    
    # Filter to data with at least 50% positivity
    variant_waves <- variants_clean %>%
      filter(Variant %in% c("Index", "Alpha", "Delta", "Omicron")) %>%
      arrange(Variant, year_week) %>%
      filter(Variant_pc >= 50) %>% 
      select(year_week, Variant)
    
    variant_months <- full_join(year_months_template, variant_waves) %>% 
      mutate(Variant = ifelse(year_week == "2021-50", "Omicron", Variant)) %>% 
      arrange(year_week) %>% 
      fill(Variant, .direction = "down") %>% 
      mutate(Variant = ifelse(is.na(Variant), "Index", Variant)) %>% 
      group_by(Variant) %>% 
      mutate(n_months = length(unique(yearmonth))) %>% 
      ungroup()
    
    return(variant_months)
  }
}


calculate_overall_vaccination_rates <- function(vax_clean, resolution){
  
  if(resolution=="Region"){
  Reg.vax.coverage = vax_clean %>%
    group_by(report_country) %>%
    filter(row_number() == n()) %>%
    ungroup() %>%
    summarise(first.dose = round((sum(nFirstDose)/sum(denominator))*100, 1),
              full.coverage = round((sum(nSecondDose)/sum(denominator))*100, 1),
              additional.dose = round((sum(nDoseAdditional1)/sum(denominator))*100, 1)) 
    
  } 
  
  if(resolution=="Country"){
    
    Reg.vax.coverage = vax_clean %>%
      group_by(report_country) %>%
      filter(row_number() == n()) %>%
      ungroup() %>%
      group_by(report_country) %>%
      summarise(first.dose = round((sum(nFirstDose)/sum(denominator))*100, 1),
                full.coverage = round((sum(nSecondDose)/sum(denominator))*100, 1),
                additional.dose = round((sum(nDoseAdditional1)/sum(denominator))*100, 1)) %>%
      ungroup()
    
  }
  
  

  return(Reg.vax.coverage)
}


create_VE_table_by_variant_time <- function(vax_clean, variant_waves){
  # Create table of VE values according the time period
  

  template <- expand.grid(year_week=unique(vax_clean$year_week))

  VE_by_variant <- full_join(template, variant_waves, by=c("year_week"="start")) %>%
    select(-c(report_country, end)) %>%
    arrange(year_week) %>%
    mutate(Dominant_virus = Variant) %>%
    fill(Dominant_virus, .direction = "updown") %>%
    select(-Variant) %>%
    mutate(VE1 = ifelse(Dominant_virus == "Alpha", 0.60,
                        ifelse(Dominant_virus == "Delta", 0.50,
                               ifelse(Dominant_virus == "Omicron", 0.40, 0))),
           VE2 = ifelse(Dominant_virus == "Alpha", 0.95,
                        ifelse(Dominant_virus == "Delta", 0.80,
                               ifelse(Dominant_virus == "Omicron", 0.60, 0))),
           VE3 = ifelse(Dominant_virus == "Delta", 0.80,
                        ifelse(Dominant_virus == "Omicron", 0.60, 0)))
  
  temp <- as.data.table(VE_by_variant)
  
  calculate_waning <- function(temp){
    print(temp$VE[1])
    t2 <- c(temp$VE[1])
    for(i in 2:length(temp$VE)){
      t2[i] <- t2[i-1]*0.99
    }
    return(t2)
  }
  
  
  temp[,VE1_waning := calculate_waning(.SD), .SDcols = "VE1", by = c("Dominant_virus") ]
  temp[,VE2_waning := calculate_waning(.SD), .SDcols = "VE2", by = c("Dominant_virus") ]
  temp[,VE3_waning := calculate_waning(.SD), .SDcols = "VE3", by = c("Dominant_virus") ]
  
  VE_by_variant <- temp
  
  return(VE_by_variant)      
}


find.vaccinations.by.period <- function(vax_clean){
  
  vaccinations_by_period <- vax_clean %>% 
    select(year_week, FirstDose, SecondDose, DoseAdditional1, DoseAdditional2, DoseAdditional3) %>% 
    distinct() %>% 
    group_by(year_week) %>% 
    summarise(FirstDose = sum(FirstDose),
              SecondDose = sum(SecondDose),
              DoseAdditional1 = sum(DoseAdditional1),
              DoseAdditional2 = sum(DoseAdditional2),
              DoseAdditional3 = sum(DoseAdditional3)) %>% 
    ungroup() %>%
    mutate(timing = ifelse(year_week == "2020-51", "Dec_Feb_21", NA),
           timing = ifelse(year_week == "2021-10", "Mar_May_21", timing),
           timing = ifelse(year_week == "2021-22", "Jun_Aug_21", timing),
           timing = ifelse(year_week == "2021-34", "Sep_Nov_21", timing),
           timing = ifelse(year_week == "2021-46", "Dec_Feb_22", timing),
           timing = ifelse(year_week == "2022-06", "Mar_May_22", timing),
           timing = ifelse(year_week == "2022-18", "Jun_Aug_22", timing)) %>% 
    fill(timing, .direction = "down") %>% 
    group_by(timing) %>% 
    mutate(nFirstDose = sum(FirstDose),
           nSecondDose = sum(SecondDose),
           nDoseAdditional1 = sum(DoseAdditional1),
           nDoseAdditional2 = sum(DoseAdditional2),
           nDoseAdditional3 = sum(DoseAdditional3)) %>% 
    ungroup()
  
  return(vaccinations_by_period)
}


clean_vaccine_denominator_data <- function(Vax_denom){
  
  Vax_denom <- Vax_denom %>% 
    select(-denom_source) %>%
    filter(!country %in% c("Andorra", "San Marino", "Liechtenstein")) %>% 
    mutate(denominator = ifelse(country == "Israel" & target_group == "Age60_69", 741639, denominator),
           denominator = ifelse(country == "Israel" & target_group == "Age70_79", 490784, denominator),
           denominator = ifelse(country == "Israel" & target_group == "Age80+", 277034, denominator)) %>%
    mutate(country = ifelse(str_detect(country,"^Kosovo*"), "Kosovo", country),
           target_group = ifelse(!target_group %in% c("1_Age<60", "1_Age60+"), 
                                 str_replace(target_group, "_", "-"), 
                                 target_group)) %>%
    rename(report_country = country,
           TargetGroup = target_group)
  
  all_target_group <- Vax_denom %>% 
    filter(TargetGroup %in% c("1_Age<60", "1_Age60+")) %>% 
    group_by(report_country) %>% 
    summarise(denominator = sum(denominator)) %>% 
    ungroup() %>% 
    mutate(TargetGroup = "ALL")
  
  Vax_denom <- full_join(Vax_denom, all_target_group)
 
  return(Vax_denom) 
}


# Find country denominators from cleaned vaccination data set
create.population.denominators <- function(population){
  
  country_population <- population %>% 
    filter(CountryName %in% countries_included$CountryName) %>% 
    distinct() %>% 
    mutate(TargetGroup = str_replace(TargetGroup, "Age", "Age "),
           TargetGroup = str_replace(TargetGroup, "_", "-")) %>% 
    arrange(CountryName, TargetGroup)
  
  return(country_population)
}


calculate.cases.by.age.update <- function(cases, age.group) {
  
  # Select variables needed according to age group
  if(age.group == "25+"){
    age.cols=c("Age25.49")
  } else if(age.group == "50+"){
    age.cols=c("Age50.59M", "Age50.59F")
  } else if(age.group == "60+"){
    age.cols=c("Age60.64M", "Age60.64F", "Age65.69M", "Age65.69F", 
               "Age70.74M", "Age70.74F", "Age75.79M", "Age75.79F",
               "Age80.M", "Age80.F")
  } else if(age.group == "All"){
    age.cols=c("Age25.49", "Age50.59M", "Age50.59F", "Age60.64M", "Age60.64F", "Age65.69M", "Age65.69F", 
               "Age70.74M", "Age70.74F", "Age75.79M", "Age75.79F",
               "Age80.M", "Age80.F")
  }
  
  # Data cleaning before aggregation by age group step
  cases_age <- cases %>%
    mutate(CountryName = ifelse(grepl("^UK-ENG_*", DataSource), "United Kingdom (England)", CountryName)) %>%
    mutate(CountryName = ifelse(grepl("^UK-NI_*", DataSource), "United Kingdom (Northern Ireland)", CountryName)) %>%
    mutate(CountryName = ifelse(grepl("^UK-SCOT_*", DataSource), "United Kingdom (Scotland)", CountryName)) %>%
    mutate(CountryName = ifelse(grepl("^UK-WALES_*", DataSource), "United Kingdom (Wales)", CountryName)) %>%
    # Filter data down to needed columns
    dplyr::select(c(CountryName, ReportingCountry, DateUsedForStatisticsWeek) | (starts_with("Age"))) %>%
    # melt data frame    
    pivot_longer(cols = age.cols,
                 names_to = "age_group",
                 values_to = "CasesObserved") %>% 
    rename(year_week = DateUsedForStatisticsWeek) %>%
    dplyr::select(CountryName, year_week, age_group, CasesObserved) %>%
    # Tidy up element names
    mutate(year_week = str_replace(year_week, "'", ""),
           age_group = str_replace(age_group, "\\D$", ""),
           age_group = str_replace(age_group, "\\.", "-"),
           age_group = str_replace(age_group, "80", "80+"),
           age_group = str_replace(age_group, "80+-", "80+")) 
  
  # Aggregate number of deaths observed by country and week 
  if(age.group == "60+"){
    # Different data variables are used for this age group, therefore requires one additional cleaning step.
    cases_age <- cases_age %>%
      mutate(age_group = str_replace(age_group, "\\+-", "+"),
             age_group = str_replace(age_group, "-", "_")) %>%
      group_by(CountryName, year_week) %>%
      summarise(age_group = "Age60+",
                CasesObserved = sum(CasesObserved, na.rm = TRUE)) %>%
      ungroup()
  }
  
  if(age.group == "All"){
    cases_age <- cases_age %>%
      mutate(age_group = ifelse(age_group %in% c("Age60-64", "Age65-69","Age70-74", "Age75-79", "Age80+-"), "Age60+", age_group),
             age_group = str_replace(age_group, "-", "_")) %>% 
      group_by(CountryName, year_week, age_group) %>%
      summarise(#age_group=age.group,
                CasesObserved = sum(CasesObserved, na.rm = TRUE)) %>%
      ungroup()
  } else {
    cases_age <- cases_age %>%
      mutate(age_group = str_replace(age_group, "-", "_")) %>% 
      group_by(CountryName, year_week) %>%
      summarise(age_group = paste0("Age",age.group),
                CasesObserved = sum(CasesObserved, na.rm = TRUE)) %>%
      ungroup()
  }

  
  return(cases_age)
}

fill.missing.seroprevalence <- function(seroprevalence_data) {
  
  yearmonth_levels <- paste(paste(rep(c("2020", "2021", "2022", "2023"), length = 48)) %>% sort(), sprintf("%02d", 1:12), sep="-") %>% 
    {factor(.[1:48])}
  
  seroprevalence <- seroprevalence_data %>% 
    mutate(yearmonth = factor(yearmonth, levels=yearmonth_levels)) %>% 
    complete(yearmonth) %>%
    mutate(imputed = is.na(seroprevalence)) %>% 
    arrange(yearmonth) %>%
    fill(seroprevalence, .direction="updown") %>% 
    filter(!is.na(yearmonth))
  
  return(seroprevalence)
}
