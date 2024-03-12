



 ### Summary panel plots
# Row 1: Mortality
rates_denom <- Country_population %>%
  filter(targetgroup %in% c("Age25_49", "Age50_59", "Age60+"))
age_group_factor_levels <- c("25 to 49 years", "50 to 59 years", "≥ 60 years")

mortality_data_plot <- deaths_age %>% 
  ungroup() %>% 
  group_by(year_week, age_group, CountryName) %>% 
  summarise(DeathsObserved = sum(DeathsObserved)) %>% 
  ungroup() %>% 
  arrange(CountryName, age_group, year_week) %>%
  full_join(rates_denom, by=c("CountryName" = "report_country", "age_group" = "targetgroup")) %>% 
  filter(!is.na(year_week)) %>% 
  mutate(Observed_MR = round((DeathsObserved / denominator) *100000)) %>% 
  filter(!is.na(Observed_MR)) %>% 
  group_by(age_group, CountryName) %>% 
  summarise(Observed_MR_min = min(Observed_MR),
            Observed_MR_max = max(Observed_MR)) %>% 
  ungroup() %>% 
  mutate(age_group = ifelse(age_group == "Age25_49", "25 to 49 years",
                            ifelse(age_group == "Age50_59", "50 to 59 years", "≥ 60 years")),
         age_group = factor(age_group, levels = age_group_factor_levels))

mortality_data_plot_total <- deaths_age %>% 
  ungroup() %>% 
  full_join(rates_denom, by=c("CountryName" = "report_country", "age_group" = "targetgroup")) %>% 
  filter(!is.na(denominator)) %>% 
  filter(!is.na(year_week)) %>% 
  group_by(CountryName, age_group) %>% 
  summarise(DeathsObserved = sum(DeathsObserved),
            denominator = sum(unique(denominator))) %>% 
  ungroup() %>% 
  arrange(age_group, CountryName) %>%
  mutate(Observed_MR = round((DeathsObserved / denominator) *100000, 3)) %>% 
  mutate(age_group = ifelse(age_group == "Age25_49", "25 to 49 years",
                            ifelse(age_group == "Age50_59", "50 to 59 years", "≥ 60 years")),
         age_group = factor(age_group, levels = age_group_factor_levels))

# Row 2: Vaccinations
legend.order <- c("RateFirstDose", "RateSecondDose", "RateDoseAdditional1", "RateDoseAdditional2")

vaccination_data_plot_min <- vax_clean %>%
  filter(targetgroup %in% c("Age25_49", "Age50_59", "Age60+")) %>% 
  filter(year_week %in% vaccine.weeks$DateUsedForStatisticsISO) %>% 
  group_by(report_country, targetgroup, year_week) %>%
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
  group_by(targetgroup, report_country) %>%
  summarise(RateFirstDoseMin = min(RateFirstDose),
            RateFirstDoseMax = max(RateFirstDose),
            RateSecondDoseMin = min(RateSecondDose),
            RateSecondDoseMax = max(RateSecondDose),
            RateDoseAdditional1Min = min(RateDoseAdditional1),
            RateDoseAdditional1Max = max(RateDoseAdditional1),
            RateDoseAdditional2Min = min(RateDoseAdditional2),
            RateDoseAdditional2Max = max(RateDoseAdditional2)) %>% 
  ungroup() %>% 
  pivot_longer(cols = c("RateFirstDoseMin", "RateSecondDoseMin", "RateDoseAdditional1Min", "RateDoseAdditional2Min"),
               names_to = "Dosage",
               values_to = "RatesMin") %>% 
  mutate(Dosage = str_remove(Dosage, "Min$")) 

vaccination_data_plot_max <- vaccination_data_plot_min %>% 
  select(targetgroup, report_country, ends_with("Max")) %>% 
  pivot_longer(cols = c("RateFirstDoseMax", "RateSecondDoseMax", "RateDoseAdditional1Max", "RateDoseAdditional2Max"),
               names_to = "Dosage",
               values_to = "RatesMax") %>% 
  mutate(Dosage = str_remove(Dosage, "Max$")) 

vaccination_data_plot <- full_join(vaccination_data_plot_min, vaccination_data_plot_max) %>% 
  select(targetgroup, report_country, Dosage, RatesMin, RatesMax) %>% 
  mutate(targetgroup = ifelse(targetgroup == "Age25_49", "25 to 49 years",
                            ifelse(targetgroup == "Age50_59", "50 to 59 years", "≥ 60 years")),
         targetgroup = factor(targetgroup, levels = age_group_factor_levels),
         Dosage = factor(Dosage, levels = legend.order),
         RatesMin = (RatesMin / 1000),
         RatesMax = (RatesMax / 1000))


vaccination_data_plot_total <- vax_clean %>%
  filter(year_week %in% vaccine.weeks$DateUsedForStatisticsISO) %>% 
  group_by(targetgroup, report_country) %>%
  summarise(FirstDose=sum(FirstDose),
            SecondDose=sum(SecondDose),
            DoseAdditional1=sum(DoseAdditional1),
            DoseAdditional2=sum(DoseAdditional2),
            denominator=sum(unique(denominator)),
            RateFirstDose = round((FirstDose/denominator*100000)),
            RateSecondDose = round((SecondDose/denominator*100000)),
            RateDoseAdditional1 = round((DoseAdditional1/denominator*100000)),
            RateDoseAdditional2 = round((DoseAdditional2/denominator*100000))) %>%
  ungroup() %>% 
  filter(targetgroup %in% c("Age25_49", "Age50_59", "Age60+")) %>% 
  pivot_longer(cols="RateFirstDose":"RateDoseAdditional2",
               names_to = "Dosage",
               values_to="Rate") %>% 
  select(report_country, targetgroup, Dosage, Rate) %>% 
  mutate(Dosage = factor(Dosage, levels = legend.order),
         Rate = (Rate / 1000), 
         targetgroup = ifelse(targetgroup == "Age25_49", "25 to 49 years",
                            ifelse(targetgroup == "Age50_59", "50 to 59 years", "≥ 60 years")),
         targetgroup = factor(targetgroup, levels = age_group_factor_levels))



# Scatter plot

temp <- full_join(mortality_data_plot_total, vaccination_data_plot_total, by = c("CountryName" = "report_country", "age_group" = "targetgroup")) %>% 
  filter(!is.na(Rate)) %>% 
  filter(Dosage == "RateSecondDose") %>% 
  select(CountryName, age_group, age_group, Observed_MR, Rate) %>% 
  distinct()


ggplot(data = temp, aes(x = Observed_MR, y = Rate, group = age_group, colour = age_group)) +
  geom_point() +
  facet_grid(.~ age_group, scales = "free")

