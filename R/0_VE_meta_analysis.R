# Script to run meta-analysis to determine VE estimates
# Margaux Mesle - meslem@who.int
# January 2024


run.meta.analysis <- function(){
  
  VE_values <- read.csv("Data/VE_values.csv", header = TRUE, stringsAsFactors = FALSE) %>% 
    type_convert() %>%
    filter(!Comments == "Time stamp: 7+ days") %>% 
    select(Authors, VE.Variant, VE.score, `VE.score..lower.`, `VE.score..higher.`, 
           Risk.ratio, `Risk.ratio..lower.`, `Risk.ratio..higher.`) %>% 
    mutate(Dose = substr(VE.Variant, 3, 4),
           Variant = gsub(".*?\\((.*?)\\).*", "\\1", VE.Variant),
           Study = group_indices(., Authors)) %>% 
    mutate(Risk.ratio = ifelse(is.na(Risk.ratio), 1 - ( VE.score/ 100), Risk.ratio),
           `Risk.ratio..lower.` = ifelse(is.na(`Risk.ratio..lower.`), 1 - (`VE.score..higher.`/ 100), `Risk.ratio..lower.`),
           `Risk.ratio..higher.` = ifelse(is.na(`Risk.ratio..higher.`), 1 - (`VE.score..lower.`/ 100), `Risk.ratio..higher.`)) %>% 
    filter(Risk.ratio < 1 | is.na(Risk.ratio)) #%>% 
    # group_by(Authors, Dose, Variant) %>% 
    # slice_min(Risk.ratio, with_ties = FALSE) %>% 
    # ungroup()
  
  
  # Calculate each variance and weight
  weight <- VE_values  %>%  
    mutate(RR_log = log(Risk.ratio),
           RR_lower_log = log(`Risk.ratio..lower.`),
           RR_upper_log = log(`Risk.ratio..higher.`),
           se_estimate = (RR_upper_log - RR_lower_log)/3.92,
           var_estimate = se_estimate^2,
           Weight = 1 / var_estimate) 
  
  # Calculate: weighted mean (wm), 
  #            variance summary effect (variance_summary), 
  #            standard error (SE_summary)
  #            Lower and Upper confidence intervals
  
  
  weighted_mean <- weight %>% 
    group_by(Variant, Dose) %>% 
    summarise(pooled_estimate = sum(Weight*RR_log), 
              sum_weights = sum(Weight)) %>%
    mutate(pooled_estimate = pooled_estimate/sum_weights, 
           se_pooled = sqrt(1/sum_weights)) %>%
    mutate(Lower_CI = pooled_estimate + 1.96*se_pooled,
           Upper_CI = pooled_estimate - 1.96*se_pooled) 
  
  final_VE <- weighted_mean %>%
    mutate(across(c(pooled_estimate, Lower_CI, Upper_CI), ~ 100*(1 - exp(.))),
           across(c(pooled_estimate, Lower_CI, Upper_CI), ~ round(.))) %>% 
    select(Variant, Dose, pooled_estimate, Lower_CI, Upper_CI) %>% 
    rename(VE = pooled_estimate)
  
  return(final_VE)
}
