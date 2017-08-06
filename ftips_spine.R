library(fingertipsR)
library(tidyverse)
library(xtable)
library(pander)


data <- fingertips_data(ProfileID = 26)

latest_data <- data %>%
  mutate_if(is.factor, as.character) %>%
  group_by(Age, Sex, IndicatorID) %>%
  mutate(Timeperiod = as.numeric(Timeperiod)) %>%
  filter(Timeperiod == max(Timeperiod), AreaType == "County & UA", Category == "")%>%
  select(IndicatorName, AreaType, AreaName, Value, LowerCIlimit, UpperCIlimit) %>%
  ungroup()

england_data <- latest_data %>%
  filter(AreaName == "England")

latest_data %>%
  group_by(Age, Sex, IndicatorID) %>%
  mutate(min = min(Value, na.rm = TRUE), 
         mean = mean(Value, na.rm = TRUE), 
         q1 = quantile(Value, probs = 0.1,  na.rm = TRUE), 
         q2 = quantile(Value, probs = 0.25, na.rm = TRUE), 
         median = median(Value, na.rm = TRUE),
         q3 = quantile(Value, probs = 0.75, na.rm = TRUE), 
         q9 = quantile(Value, probs = 0.9, na.rm = TRUE), 
         max = max(Value, na.rm = TRUE), 
         )  %>%
  slice(1) -> test
  
knitr::kable(test, format = "pandoc")
pander::pander(test)
