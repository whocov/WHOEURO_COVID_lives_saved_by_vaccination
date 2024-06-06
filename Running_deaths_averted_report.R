# Script to calculate number of lives saved by COVID-19 vaccination programs in the WHO European Region
# Margaux Mesle - meslem@who.int
# First created: June 2021
# Latest update: October 2023

# To calculate the number of deaths averted by vaccination, we need three sets of information:
#  - Number of deaths that occurred in country, by age group, in given weeks
#  - Vaccination coverage in country, for same age groups, in given weeks
#  - Vaccine effectiveness per dose and variant

# Please note: 
# - code information for all sensitivity analyses have been included at the very end of this script.

  rm(list=ls())
  library(ggplot2)
  library(stringr)
  library(stringi)
  library(dplyr)
  library(glue)
  library(tidyr)
  library(janitor)
  library(forcats)
  library(plotly)
  library(patchwork)
  library(scales)
  library(assertthat)
  library(readxl)
  library(ISOweek)
  library(tidyverse)
  library(formattable)
  library(rmarkdown)
  library(cowplot)
  library(zoo)
  library(markdown)
  library(flextable)
  library(lubridate)
  library(fst)
  library(ggpubr)
  library(ggnewscale)
  library(rio)
  
  
  
  setwd(here::here())

  ##############
  # USER INPUT #
  ##############
  
  data.reporting.week <- "2023-W19"
  # Set reporting week as the last week of analysis
  reporting.week <- "2023-W12"
  # Name last month of reporting - this is used in figure titles
  current.month <- "April"
  
  # Determine if running analysis or sensitivity analyses
  # sensitivity_manual <- "no"
  sensitivity_manual <- "no"
   if (!exists("sensitivity")) {
     sensitivity <- sensitivity_manual
   }

  # Determine which age groups are of interest: 25+, 50+ or 60+ years of age
    # COMMENT AND UNCOMMENT ACCORDINGLY 
  #age.group <- "25+"
  #age.group <- "50+"
  #age.group <- "60+"
  age.group <- "Include finer older"
  #age.group <- "Include finer older only"
  #age.group <- "All"
  
  
  # Select from below what study period should be
  # Vaccination: from week 51/2020 to data.reporting.week
  # Pre-Omicron: from week 51/2020 to 50/2021
  Period = "Vaccination"
  #Period = "Pre-Omicron"
  
  if(Period == "Pre-Omicron"){
    start.week.period <- "2020-50"
    end.week.period <- "2021-50"
  } else {
    start.week.period <- "2020-50"
    end.week.period <- str_replace(reporting.week, "W", "")
  }
  
  # Define which countries should only be part of part of the analysis
  countries_partial_reporting <- c("Albania", "Montenegro", "Norway", "Poland", "United Kingdom (Wales)")
  
  # Define micro states to be removed
  AD_LI_SM <- c("Andorra", "Liechtenstein", "San Marino")
  
  # Define countries to be removed (no reporting)
  countries_no_reporting <- c("Belarus", "Bulgaria", "Bosnia and Herzegovina", "Georgia", "Kazakhstan", "Kyrgyzstan",
                              "Russian Federation", "Serbia", "Uzbekistan")
  
  # Define countries reporting finer older age groups
  countries_reporting_finer <- c("Austria", "Belgium", "Croatia", "Cyprus", "Czechia", "Denmark", "Estonia", "Finland",
                                 "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Italy",
                                 "Kosovo", "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands", "North Macedonia", 
                                 "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland",
                                 "United Kingdom (England)", "United Kingdom (Scotland)")
  
  ####################
  # DEFINE FUNCTIONS #
  ####################

  # Source the functions
  source("Deaths_averted_calculations.R")
  source("R/0_functions.R")
  source("R/0_VE_meta_analysis.R")
  source("R/0_calc_ve_by_variant.R")
  source("R/0_calc_ve_by_variant_sensitivity.R")
  source("R/0_categorise_time_since_vaccination.R")
  source("R/01_Clean_Iceland_mortality_data.R")          # Clean Iceland mortality data sent to us
  source("R/01_Clean_Ukraine_vaccination_data.R")        # Clean Ukraine vaccination data sent to us
  source("R/01A_Clean_vaccination_data.R")               # Clean vaccination data from TESSy
  source("R/01B_Clean_vaccination_data_fine_ages.R")     # Clean vaccination data from TESSy for finer older age groups
  source("R/02A_Clean_mortality_data.R")                 # Clean mortality data from TESSy
  source("R/02B_Clean_mortality_data_fine_ages.R")       # Clean mortality data from TESSy for finer older age groups
  source("R/03_Find_vaccinations_by_age.R")              # Find vaccination doses by age
  source("R/A01_Create_data_summary_heatmap.R")          # Create summary heatmap of data available
  source("R/A01_Create_data_summary_table.R")            # Supplementary table 1
  source("R/A03_Create_sensitivity_summary_table.R")     # Supplementary table - sensitivity analyses regional and country results
  source("R/A03_Create_sensitivity_summary_plot.R")      # Supplementary plot - sensitivity analyses regional results by scenario
  source("R/A04_Plot_country_mortality_curves.R")        # Figures 2 and 3 : mortality rates
  source("R/A05A_Plot_country_vaccination_curves.R")     # Figures 4 and 5 : vaccination rates
  source("R/A05B_Plot_regional_vaccination_curves.R")    # Figures : Proportion lives saved regionally
  source("R/A06_Plot_country_expected_cases.R")          # Figure 6: Expected and observed mortality curves
  source("R/A09_Create_table_3.R")                       # Summary table 4: Overall summary table by country (age 25+)
  source("R/A10_Create_table_4.R")                       # Summary table 5: Finer summary table by country and age group
  source("R/A11A_Create_summary_panel.R")                # Figure 1: summary panel
  source("R/A11B_Create_summary_panel_finer_older.R")    # Supplementary Figure 1: summary panel for older age groups only
  source("R/A13_Create_summary_variant_table.R")
  source("R/A14_Create_summary_expected_variant_table.R")

  # Create output directories -----------------------------------------------
  dir.create("Outputs")
  output_directory <- ("Outputs")
  
  
  ################################
  # SET THE GRAPHICS ENVIRONMENT #
  ################################
  
  # Define x axis and colours for plotting
  week_breaks <- c("2020-51", "2021-01", "2021-05", "2021-10", "2021-15", "2021-20", "2021-25", "2021-30", "2021-35", "2021-40", "2021-45",
                   "2021-50", "2022-01", "2022-05", "2022-10", "2022-15", "2022-20", "2022-25", "2022-30", "2022-35", "2022-40", "2022-45",
                   "2022-50", "2023-01", "2023-05", "2023-10")
  week_labels <- c("51", "1\n2021", "5", "10", "15", "20", "25", "30", "35", "40", "45", 
                   "50", "1\n2022", "5", "10", "15", "20", "25", "30", "35", "40", "45", 
                   "50", "1\n2023", "5", "10")
  
  week_breaks_long <- c("2020-50", 
                        "2021-01", "2021-10", "2021-20", "2021-30", "2021-40", "2021-50", 
                        "2022-01", "2022-10", "2022-20", "2022-30", "2022-40", "2022-50",
                        "2023-01", "2023-10")
  week_labels_long <- c("50", 
                        "1\n2021", "10", "20", "30", "40", "50", 
                        "1\n2022", "10", "20", "30", "40", "50",
                        "1\n2023", "10")
  
  week_breaks_vaccination <- c("2020-50",
                               "2021-01", "2021-10", "2021-20", "2021-30", "2021-40", "2021-50", 
                               "2022-01", "2022-10", "2022-20", "2022-30", "2022-40", "2022-50",
                               "2023-01", "2023-10")
  week_labels_vaccination <- c("50",
                               "1\n2021", "10", "20", "30", "40", "50", 
                               "1\n2022", "10", "20", "30", "40", "50",
                               "1\n2023", "10")
  
  variant.order <- c("Index", "Alpha", "Delta", "Omicron")
  
  variant.colours <- c("Index" = "#4999AB", 
                       "Alpha" = "#9CC65A", 
                       "Delta" = "#C9D971", 
                       "Omicron" = "#E7E7B9")
  age.colours <- c("Age 25-49" = "#a82d17", 
                   "Age 50-59" = "#e1a744", 
                   "Age 60+" = "#1f497d")
  
  age_group_factor_levels <- c("25 to 49 years", "50 to 59 years", "≥60 years", "60 to 69 years", "70 to 79 years", "≥80 years")

  text_size <- 10

  ####################################
  # SET THE CALCULATIONS ENVIRONMENT #
  ####################################
  
  # Is the value for the sensitivity analysis?
 # ve_by_variant <- calc.ve.by.variant.sensitivity(sensitivity)
  
  # create range of all reporting weeks
  start.week <- start.week.period
  end.week <- end.week.period
  # Make sure first date is in November 2020 or earlier
  all.dates <- seq(Sys.Date() - 1500, Sys.Date() + 1050, 1)
  all.weeks <- seq(Sys.Date() - 2050, Sys.Date() + 1050, 1)
  all.weeks <- unique(str_replace(ISOweek(all.weeks), "W", ""))
  all.weeks <- all.weeks[which(all.weeks == start.week):which(all.weeks == end.week)] 
  reporting.weeks <- all.weeks[all.weeks <= end.week]

  year_months_template <- data.frame(all.dates = all.dates,
                                     year_week = str_replace(string = ISOweek(all.dates), pattern = "W", replacement = ""),
                                     yearmonth = format(as.Date(all.dates), "%Y-%m")) %>% 
    filter(year_week %in% reporting.weeks) %>% 
    select(-all.dates) %>% 
    add_row(year_week = "2020-50", yearmonth = "2020-12") %>% 
    add_row(year_week = "2020-51", yearmonth = "2020-12") %>%
    add_row(year_week = "2021-53", yearmonth = "2021-12") %>% 
    add_row(year_week = "2022-53", yearmonth = "2022-12") %>% 
    arrange(year_week) %>% 
    distinct() 
  
  # Find weeks representing Omicron period
  start.week.omicron <- "2021-51"
  end.week <- str_replace(data.reporting.week, "W", "")
  all.weeks <- seq(Sys.Date() - 2050, Sys.Date() + 1050, 1)
  all.weeks <- unique(str_replace(ISOweek(all.weeks), "W", ""))
  all.weeks <- all.weeks[which(all.weeks == start.week.omicron):which(all.weeks == end.week)] 
  omicron.weeks <- all.weeks[all.weeks <= end.week]
  
  
  ################
  # READ IN DATA #
  ################  
    # Helper data sets
  country_codes <- read_excel("Data/CountryCode.xlsx") %>% 
    rename(report_country = region) %>% 
    filter(!c(report_country %in% AD_LI_SM)) %>% 
    mutate(report_country = ifelse(str_detect(report_country, "^Kosovo"), "Kosovo", report_country))
  # Broad age group population data
  EU_population <- read_excel("Data/EU_population_by_year.xlsx")
  UN_population <- read_excel("Data/UN_population_by_year.xlsx")
  population <- clean.population.data(EU_population, UN_population)
  
  # Finer age group population data
  EU_population_fine <- read_excel("Data/EU_population_by_year_finer_ages.xlsx")
  UN_population_fine <- read_excel("Data/UN_population_by_year_finer_ages.xlsx")
  population_fine <- clean.population.data(EU_population_fine, UN_population_fine)
  
  # Seroprevalance and Vaccine Effectiveness (VE)
  seroprevalence <- read.csv("Data/seroprevalence.csv", header = TRUE, stringsAsFactors = FALSE) %>% 
    fill.missing.seroprevalence()
  VE_values <- run.meta.analysis()
  
    # Data sets
  variants_GISAID <- read.csv("Data/Variants_GISAID.csv", header = TRUE, stringsAsFactors = FALSE)
  deaths <- read.csv(paste0("Data/Weekly/tessy_aggregated_all_weeks_incl_new_age_sex_", data.reporting.week,".csv"), header = TRUE, stringsAsFactors = FALSE) %>% 
    filter(!c(CountryName %in% AD_LI_SM)) %>% 
    filter(!CountryName == "Iceland")
  cases <- read.csv(paste0("Data/Weekly/tessy_aggregated_all_weeks_incl_new_age_sex_", data.reporting.week,".csv"), header = TRUE, stringsAsFactors = FALSE) %>% 
    filter(!c(CountryName %in% AD_LI_SM))
  
  vaccine <- read.csv("Data/Weekly/2023_vaccine_rollout_dataset.csv", header = TRUE, stringsAsFactors = FALSE) %>% 
    filter(!c(ReportingCountry %in% c("AD", "LI", "SM")))
  variants <- read.csv(paste0("Data/Weekly/variant_combined_long_tessy_sublineages_", data.reporting.week,".csv"), header = TRUE, stringsAsFactors = FALSE) %>% 
    filter(!c(CountryName %in% AD_LI_SM))
  variant_lookup <- read.csv(paste0("Data/Weekly/variant_lookup_", data.reporting.week,".csv"), header=TRUE, stringsAsFactors = FALSE)
  
  
  # Some countries have sent us the data directly (due to time restraints)
  Iceland_mortality <- read_excel("Data/Iceland_mortality_data.xlsx") %>% type_convert() 
  Ukraine_vaccinations <- import_list("Data/Ukraine_vaccination.xlsx", rbind = TRUE) %>% 
    UA_vaccination_data_cleaning()
  England_vaccinations <- read_excel("Data/England_vaccination_data.xlsx", sheet = "Vaccine_data") %>% type_convert() 
  Israel_mortality <- read_excel("Data/Israel_mortality_vaccination.xlsx", sheet = "mortality") %>% type_convert() 
  Israel_vaccination <- read_excel("Data/Israel_mortality_vaccination.xlsx", sheet = "vaccine data") %>% type_convert() 
  Kosovo_mortality <- read.csv("Data/Kosovo_mortality.csv", header = TRUE, stringsAsFactors = FALSE) 
  Lithuania_mortality <- read.csv("Data/Lithuania_mortality.csv", header = TRUE, stringsAsFactors = FALSE) 
  Scotland_mortality <- read.csv("Data/Scotland_mortality.csv", header = TRUE, stringsAsFactors = FALSE)
  Scotland_vaccination <- read.csv("Data/Scotland_vaccination_by_dose.csv", header = TRUE, stringsAsFactors = FALSE)
  Romania_mortality <- read_excel("Data/Romania_mortality.xlsx") %>% type_convert() 
  Netherlands_vaccination <- read.csv("Data/Netherlands_vaccination.csv", header = TRUE, stringsAsFactors = FALSE)
  Croatia_mortality <- read_excel("Data/Croatia_mortality.xlsx") %>% type_convert() 
  
  # Find countries with enough data to be included in analysis
  countries_included <- create.data.summary.table(deaths, vaccine, variants, country_codes, 
                                                  countries_partial_reporting, countries_no_reporting) 
  
  # Note: not all countries are reporting age groups under 60 years; 
    # if considering looking at all age groups, filter to countries with 25-49ys breakdown
  if(age.group == "25+"){
    countries_included <- countries_included %>% filter(Age_group == "25-49" & Country_included == "Yes")
  } else if(age.group == "50+"){
    countries_included <- countries_included %>% filter(Age_group == "50-59" & Country_included == "Yes")
  } else if(age.group == "60+"){
    countries_included <- countries_included %>% filter(Age_group == "60+" & Country_included == "Yes")
  } else if(age.group == "Include finer older only"){
    countries_included <- countries_included %>% filter(Age_group == "Finer older" & Country_included == "Yes")
  } else if(age.group == "Include finer older"){
    countries_included <- countries_included %>% filter((Age_group == "Finer older" & Country_included == "Yes") |
                                                        (Age_group == "60+" & Country_included == "Yes"))
  } else if(age.group == "All"){
    countries_included <- countries_included 
  }
  
  
  # Create template of all weeks included in data
  weeks_template <- expand.grid(year_week = unique(deaths$DateUsedForStatisticsWeek)) %>% 
    mutate(year_week = str_replace(year_week, "'", ""),
           Period = ifelse(year_week %in% omicron.weeks, "Omicron", "Pre-Omicron"))
  
  # data cleaning
  variant_waves <- calculate.variant.waves(variants, variants_GISAID, variant_lookup, weeks_template, resolution = "Country") 
  variant_waves_region <- calculate.variant.waves(variants, variants_GISAID, variant_lookup, weeks_template, resolution = "Region") 
  variant_months_region <- calculate.variant.waves(variants, variants_GISAID, variant_lookup, weeks_template, resolution = "By month") 
  
  # Broad age groups
  deaths_age <- calculate.mortality.by.age.update(deaths, Croatia_mortality, Iceland_mortality, Israel_mortality, 
                                                  Kosovo_mortality, Lithuania_mortality, Scotland_mortality, Romania_mortality,
                                                  year_months_template) 
  vax_clean <- clean.vaccination.data(vaccine, country_codes, population, 
                                      England_vaccinations, Israel_vaccination, Scotland_vaccination, Ukraine_vaccinations,
                                      Netherlands_vaccination) 
  
  # Finer age groups
  deaths_age_finer <- calculate.mortality.by.finer.age.groups.update(deaths, Croatia_mortality, Iceland_mortality, 
                                                                     Israel_mortality, Kosovo_mortality, Lithuania_mortality, 
                                                                     Scotland_mortality, 
                                                                     Romania_mortality, year_months_template) 
  vax_clean_finer <- clean.vaccination.data.finer.ages(vaccine, country_codes, population_fine, 
                                                       England_vaccinations, Israel_vaccination, 
                                                       Scotland_vaccination, 
                                                       Ukraine_vaccinations, Netherlands_vaccination) 
  

  rm(Iceland_mortality, Israel_mortality, Kosovo_mortality, Lithuania_mortality, Scotland_mortality,
     England_vaccinations, Israel_vaccination, Scotland_vaccination, Ukraine_vaccinations, Romania_mortality)
  
  ####################
  # Run calculations #
  ####################


  # Filter data according to time period considered and whether to include older age group breakdown
  if(Period == "Pre-Omicron" & !age.group == "Include finer older"){
    
    countries_included <- countries_included %>% 
      filter(Period == "Pre-Omicron" & Country_included == "Yes")
    
    # Filter vaccination and mortality data to selected countries
    deaths_age <- deaths_age %>% filter(ReportCountry %in% countries_included$CountryName) 
    vax_clean <- vax_clean %>% filter(ReportCountry %in% unique(countries_included$CountryName))
    
    print("Period selected: pre-Omicron; age group selected does NOT include finer older ages ")
    
  } else if(Period == "Pre-Omicron" & age.group == "Include finer older only") {
    
    countries_included <- countries_included %>% 
      filter(Period == "Pre-Omicron" & Country_included == "Yes")
    
    # Filter vaccination and mortality data to selected countries
    deaths_age <- deaths_age_finer %>% filter(ReportCountry %in% countries_included$CountryName) 
    vax_clean <- vax_clean_finer %>% filter(ReportCountry %in% unique(countries_included$CountryName))
    
    print("Period selected: pre-Omicron; age group selected DOES include finer older ages")
    
  } else if(Period == "Pre-Omicron" & age.group == "Include finer older"){
    
    countries_included <- countries_included %>% 
      filter(Period == "Pre-Omicron" & Country_included == "Yes") 
    
    # Find countries reporting finer older age groups
    deaths_age_temp1 <- deaths_age_finer %>% filter(ReportCountry %in% countries_included$CountryName) %>% 
      filter(!ReportCountry %in% c("Germany", "Israel", "Republic of Moldova", "United Kingdom (Scotland)", "Ukraine")) 
    vax_clean_temp1 <- vax_clean_finer %>% filter(ReportCountry %in% unique(countries_included$CountryName)) %>% 
      filter(!ReportCountry %in% c("Germany", "Israel", "Republic of Moldova", "United Kingdom (Scotland)", "Ukraine")) 
    
    # Filter countries reporting 60+ only
    deaths_age_temp <- deaths_age %>% 
      filter(ReportCountry %in% c("Germany", "Israel", "Republic of Moldova", "United Kingdom (Scotland)", "Ukraine")) 
    vax_clean_temp <- vax_clean %>% 
      filter(ReportCountry %in% c("Germany", "Israel", "Republic of Moldova", "United Kingdom (Scotland)", "Ukraine"))
    
    # Make final dataset to be used
    deaths_age <- full_join(deaths_age_temp, deaths_age_temp1)
    vax_clean <- full_join(vax_clean_temp, vax_clean_temp1)
    
    print("Period selected: pre-Omicron; age group selected DOES include finer older ages and 60+ for 4 countries")
    
  } else if(Period == "Vaccination" & age.group == "Include finer older only"){
    
    countries_included <- countries_included %>% 
      filter(Period == "Full analysis" & Country_included == "Yes")
    
    # Filter vaccination and mortality data to selected countries
    deaths_age <- deaths_age_finer %>% 
      filter(ReportCountry %in% countries_included$CountryName) %>% 
      filter(!(TargetGroup == "Age 60+" | ReportCountry %in% c("Germany", "Israel", "Republic of Moldova", "United Kingdom (Scotland)", "Ukraine")))
    vax_clean <- vax_clean_finer %>% 
      filter(ReportCountry %in% unique(countries_included$CountryName)) %>% 
      filter(!(TargetGroup == "Age 60+" | ReportCountry %in% c("Germany", "Israel", "Republic of Moldova", "United Kingdom (Scotland)", "Ukraine")))
    
    print("Period selected: full analysis; age group selected DOES include finer older ages ONLY")
    
  }  else if(Period == "Vaccination" & !age.group == "Include finer older"){
    
    countries_included <- countries_included %>% 
      filter(Period == "Full analysis" & Country_included == "Yes")
    
    # Filter vaccination and mortality data to selected countries
    deaths_age <- deaths_age %>% filter(ReportCountry %in% countries_included$CountryName) 
    vax_clean <- vax_clean %>% filter(ReportCountry %in% unique(countries_included$CountryName))
    
    print("Period selected: full analysis; age group selected does NOT include finer older ages")
    
  } else {
    
    countries_included <- countries_included %>% 
      filter(Period == "Full analysis" & Country_included == "Yes") 
    
    # Find countries reporting finer older age groups
    deaths_age_temp1 <- deaths_age_finer %>% filter(ReportCountry %in%  unique(countries_included$CountryName)) %>% 
      filter(!ReportCountry %in% c("Germany", "Israel", "Republic of Moldova", "United Kingdom (Scotland)", "Ukraine")) 
    vax_clean_temp1 <- vax_clean_finer %>% filter(ReportCountry %in% unique(countries_included$CountryName)) %>% 
      filter(!ReportCountry %in% c("Germany", "Israel", "Republic of Moldova", "United Kingdom (Scotland)", "Ukraine")) 
    
    # Filter countries reporting 60+ only
    deaths_age_temp <- deaths_age %>% 
      filter(ReportCountry %in% c("Germany", "Israel", "Republic of Moldova", "United Kingdom (Scotland)", "Ukraine")) %>% 
      filter(TargetGroup %in% c("Age 25-49", "Age 50-59", "Age 60+"))
    vax_clean_temp <- vax_clean %>% 
      filter(ReportCountry %in% c("Germany", "Israel", "Republic of Moldova", "United Kingdom (Scotland)", "Ukraine")) %>% 
      filter(TargetGroup %in% c("Age 25-49", "Age 50-59", "Age 60+"))
    
    
    # Make final dataset to be used
    deaths_age <- full_join(deaths_age_temp, deaths_age_temp1)
    vax_clean <- full_join(vax_clean_temp, vax_clean_temp1)
    
    print("Period selected: full analysis; age group selected DOES include finer older ages and 60+ for 4 countries")
    
  }
  

  # Find other useful information 
  vaccinations_period <- find.vaccinations.by.period(vax_clean)
  vaccination_start_dates <- find.start.vaccination.dates(vax_clean)
  Country_population <- create.population.denominators(population)
  Country_population_finer <- create.population.denominators(population_fine)
  vaccinations_by_age <- vaccination.by.age(vax_clean, Country_population, geo = "Region")
  vaccinations_by_age_country <- vaccination.by.age(vax_clean, Country_population, geo = "Country")
  vaccinations_by_age_country_weekly <- vaccination.by.age(vax_clean, Country_population, geo = "Country by week")
  
  

  ##############################################################################
  
  # Run calculations
  if(sensitivity == "no"){
    
    sensitivity <- sensitivity_manual
    
    # Find correct values (VE, lag times and waning rates) for each scenario
    ve_by_variant <- calc.ve.by.variant.sensitivity(sensitivity, VE_values)
    
    Expected_cases_all_ages <- calculate.expected.deaths(deaths_age, vax_clean, variant_waves, ve_by_variant, 
                                                        seroprevalence, countries_included,
                                                        prior_immunity_discount = 1,
                                                        age.group = age.group, save.data = TRUE, time_since_vacc_cats = TRUE)
    
    
    render("Deaths_averted_visual_outputs_update.Rmd",
           output_file = paste0("Outputs/Deaths_averted_outputs_", reporting.week, ".doc"),
           output_format = "word_document",
           output_options = list(toc = TRUE, toc_float = TRUE))

  } else {
    # Running and saving each sensitivity analyses -----------------------------
    
    # Sensitivity analyses (SA) are:
      # SA 1: high VE values (yes_VE_high)
      # SA 2: low VE values (yes_VE_low)
      # SA 3: long lag values (yes_lag_long)
      # SA 4: short lag values (yes_lag_short)
      # SA 5: long VE waning values (yes_waning_slow)
      # SA 6: short VE waning values (yes_waning_fast)
      # SA 7: high discount for prior immunity (yes_prior_immunity_high)
      # SA 8: low discount for prior immunity (yes_prior_immunity_low)
    
    sensitivity_manual <- c("yes_VE_high", "yes_VE_low", 
                            "yes_lag_long", "yes_lag_short", 
                            "yes_waning_slow", "yes_waning_fast",
                            "yes_prior_immunity_high", "yes_prior_immunity_low")
    
    for(i in seq_along(sensitivity_manual)){
    
      sensitivity <- sensitivity_manual[i]
      # For scenarios 7 and 8, use the original model values, but change the prior immunity discount
      if (sensitivity == "yes_prior_immunity_high") {
          prior_immunity_discount <- 0.80
      } else if(sensitivity == "yes_prior_immunity_low"){
          prior_immunity_discount <- 0.95
      } else {
        prior_immunity_discount <- 1
      }
        
      # Find correct values (VE, lag times and waning rates) for each scenario
      ve_by_variant <- calc.ve.by.variant.sensitivity(sensitivity, VE_values)
      
      # Running each scenario in turn and saving the outputs
        Expected_cases_all_ages <- calculate.expected.deaths(deaths_age, vax_clean, variant_waves, ve_by_variant, 
                                                            seroprevalence, countries_included,
                                                            prior_immunity_discount = prior_immunity_discount,
                                                            age.group = age.group, save.data = TRUE, time_since_vacc_cats = TRUE)

    }

  }
