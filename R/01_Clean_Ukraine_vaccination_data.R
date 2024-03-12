# Script to read and clean Ukraine vaccination data
# Margaux Mesle
# May 2023

UA_vaccination_data_cleaning <- function(Ukraine_vaccinations){
  
  UA <- Ukraine_vaccinations %>% 
    row_to_names(row_number = 4) %>% 
    select(1:12) %>% 
    mutate(TargetGroup = case_when(ID == "1" ~ "ALL",
                                   ID == "2" ~ "Age<18",
                                   ID == "3" ~ "Age18_59",
                                   ID == "4" ~ "1_Age60+",
                                   ID == "5" ~ "AgeUNK",
                                   ID == "6" ~ "HCW")) %>% 
    rename(file_ID = "1") %>% 
    mutate(DateUsedForStatisticsISO = case_when(file_ID == 1 ~ "2022-15",
                                                file_ID == 2 ~ "2022-16",
                                                file_ID == 3 ~ "2022-17",
                                                file_ID == 4 ~ "2022-18",
                                                file_ID == 5 ~ "2022-19",
                                                file_ID == 6 ~ "2022-20",
                                                file_ID == 7 ~ "2022-21",
                                                file_ID == 9 ~ "2022-23",
                                                file_ID == 10 ~ "2022-24",
                                                file_ID == 11 ~ "2022-25",
                                                file_ID == 12 ~ "2022-26",
                                                file_ID == 13 ~ "2022-27",
                                                file_ID == 14 ~ "2022-28",
                                                file_ID == 15 ~ "2022-29",
                                                file_ID == 16 ~ "2022-30",
                                                file_ID == 17 ~ "2022-31",
                                                file_ID == 18 ~ "2022-32",
                                                file_ID == 19 ~ "2022-33",
                                                file_ID == 20 ~ "2022-34",
                                                file_ID == 21 ~ "2022-35",
                                                file_ID == 22 ~ "2022-36",
                                                file_ID == 23 ~ "2022-37",
                                                file_ID == 24 ~ "2022-38",
                                                file_ID == 25 ~ "2022-39", 
                                                file_ID == 26 ~ "2022-40",
                                                file_ID == 27 ~ "2022-41",
                                                file_ID == 28 ~ "2022-42",
                                                file_ID == 29 ~ "2022-43",
                                                file_ID == 30 ~ "2022-44",
                                                file_ID == 31 ~ "2022-45",
                                                file_ID == 32 ~ "2022-46",
                                                file_ID == 33 ~ "2022-47",
                                                file_ID == 34 ~ "2022-48",
                                                file_ID == 35 ~ "2022-49", 
                                                file_ID == 36 ~ "2022-50",
                                                file_ID == 37 ~ "2022-51",
                                                file_ID == 38 ~ "2022-52",
                                                file_ID == 39 ~ "2022-53",
                                                file_ID == 40 ~ "2023-01",
                                                file_ID == 41 ~ "2023-02", 
                                                file_ID == 42 ~ "2023-03",
                                                file_ID == 43 ~ "2023-04",
                                                file_ID == 44 ~ "2023-05",
                                                file_ID == 45 ~ "2023-06",
                                                file_ID == 46 ~ "2023-07",
                                                file_ID == 47 ~ "2023-08",
                                                file_ID == 48 ~ "2023-09",
                                                file_ID == 49 ~ "2023-10",
                                                file_ID == 50 ~ "2023-11",
                                                file_ID == 51 ~ "2023-12", 
                                                file_ID == 52 ~ "2023-13",
                                                file_ID == 53 ~ "2023-14",
                                                file_ID == 54 ~ "2023-15",
                                                file_ID == 55 ~ "2023-16", 
                                                file_ID == 56 ~ "2023-17",
                                                file_ID == 57 ~ "2023-18",
                                                file_ID == 58 ~ "2023-19",
                                                file_ID == 59 ~ "2023-20")) %>% 
    rename(VaccineName = `Вакцина`,
           DoseFirst = `Число сделанных первых доз`,
           DoseSecond = `Число сделанных вторых доз`,
           DoseAdditional1 = `Число дополн. доз (например,  \r\n3 доза)`) %>% 
    mutate(Vaccine = case_when(VaccineName == "AstraZeneca AB"~ "AZ",
                               VaccineName == "BioNTech  Manufacturing GmbH" ~ "COM",
                               VaccineName == "Janssen Biotech, Inc., USA" ~ "JANSS",
                               VaccineName == "Moderna US, Inc, Catalent Indiana, LLC" ~ "MOD",
                               VaccineName == "Синовак Лайф Саєнсіз Ко., Лтд., Китай"~ "SIN"),
           DoseFirst = as.numeric(DoseFirst),
           DoseSecond = as.numeric(DoseSecond),
           DoseAdditional1 = as.numeric(DoseAdditional1)) %>% 
    add_column(ReportingCountry = "UA", .before = "ID")
  
 
  UA_clean <- UA %>% 
    select(DateUsedForStatisticsISO, ReportingCountry, Vaccine, DoseFirst, DoseSecond, DoseAdditional1, TargetGroup) %>% 
    filter(!is.na(TargetGroup)) %>% 
    filter(!TargetGroup == "") %>% 
    filter(!is.na(Vaccine)) %>% 
    filter(TargetGroup == "1_Age60+")

  
  return(UA_clean)
}