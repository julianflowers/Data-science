## Dowloading and cleaning suicide profile

library(fingertipsR)
library(tidyverse)
library(ggjoy)
library(WVPlots)
library(stringr)


## Identify suicide profiles
profiles <- profiles()
sui_profiles <- profiles %>%
  filter(str_detect(ProfileName, "[Ss]uicide"))

## Download profile data

dataset <- fingertips_data(ProfileID = 91)

summary(dataset)

## Latest data

dataset_latest <- dataset %>%
  group_by(Age, Sex, IndicatorID, Category) %>%
  filter(TimeperiodSortable == max(TimeperiodSortable))


ds1 <- dataset_latest %>%
  ungroup() %>% 
  filter(AreaType == "County & UA") %>%
  mutate(index = paste(IndicatorName, Age, Sex, sep = "_")) %>%
  select(AreaName, index, Value) 

## create wide table

analysis <- ds1 %>%
  tidyr::spread(key = index, value = Value)

## Check NAs

isna <- analysis %>%
  keep(is.numeric) %>%
  map_df(~sum(is.na(.))) %>%
  t() %>%
   as.data.frame() %>%
  rownames_to_column() %>%
  arrange(-V1)


isna %>%
  filter(V1 > 5) %>%
  arrange(-V1)

## Exclude variables

isna_filter <- isna %>%
  filter(V1 > 5) %>%
  pull(rowname)

## remove variables with a lot of missing data
analysis1 <- select_if(analysis, !names(analysis) %in% isna_filter)  

## exclude areas with no suicide rate in latest data
analysis1 <- analysis1 %>% 
  filter(!is.na(`Suicide: age-standardised rate per 100,000 population (3 year average)_10+ yrs_Persons`
))

## Impute missing data to mean value
analysis1 <- analysis1 %>%
  map_df(function(x) ifelse(is.na(x),  mean(x, na.rm = TRUE), x))

## rename outcome variable and exclude other suicide variables
analysis1 <- analysis1 %>%
  rename(outcome = `Suicide: age-standardised rate per 100,000 population (3 year average)_10+ yrs_Persons`) %>% 
  select( -contains("suicide"))

mean(is.na(analysis1))

colnames(analysis1) <- c("area", 
                                    "children_youth_justice", 
                                     "adult_carers_isolated_18+", 
                                     "adult_carers_isolated_all_ages", 
                                     "adult_carers_not_isolated", 
                                     "alcohol_rx_18+", 
                                     "alcohol_rx_ all_ages", 
                                     "alcohol_admissions_f", 
                                     "alchol_admissions_m", 
                                     "alcohol_admissions_p", 
                                     "children_leaving_care", 
                                     "depression", 
                                     "domestic abuse",
                                     "self-harm_female", 
                                     "self_harm_male", 
                                     "self_harm_persons", 
                                     "opiates", 
                                     "lt_health_problems", 
                                     "lt_unepmloyment", 
                                     "looked_after_children", 
                                     "marital_breakup",
                                     "old_pople_alone", 
                                     "alone", 
                                     "self_reported_well_being", 
                                     "smi", 
                                     "social_care_mh", 
                                     "homeless", 
                                     "alcohol_rx", 
                                     "drug_rx_non_opiate", 
                                     "drug_rx_opiate", 
                                     "suicide_rate", 
                                     "unemployment")


analysis_final <- analysis1 %>%
  select(-area)


