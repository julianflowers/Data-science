library(fingertipsR)
library(tidyverse)
library(phutils)
library(stringr)

## Function to extract data frame of most recent data for each indicator for a given ProfileID(s) and AreaTypeID(s)

fingertips_latest_data <- function(ProfileID = 26, AreaTypeID = 102, inequalities = FALSE){
  require(dplyr)
  ind_test <- fingertips_data(ProfileID = ProfileID, AreaTypeID = AreaTypeID, inequalities = inequalities)
  ind_test <- ind_test %>% mutate(index = paste(IndicatorID, Timeperiod, Age, Sex))
  
  latest <- ind_test %>%
    group_by(IndicatorID, AreaType, Age, Sex) %>%
    do(tail(., 1)) 
 
  
  ind_test %>% filter(index %in% latest$index)
  
}

test <- fingertips_latest_data()

test %>% group_by(index) %>% count() %>% arrange(n)

test %>%
  filter(str_detect(index, "^90275")) %>% View()

## Function to identfy "red-reds' in latest data ie where trend is worsening and areas are worse than comparator

fingertips_red_red <- function(ProfileID = 26, AreaTypeID = 102){
  
  data <- fingertips_latest_data(ProfileID = ProfileID, AreaTypeID = AreaTypeID)
  
  data %>%
    filter(stringr::str_detect(RecentTrend, "worse"), ComparedtoEnglandvalueorpercentiles == "Worse")

  
}

## Function to identfy "top-10s' in latest data ie where trend is worsening and areas are worse than comparator


fingertips_top_ten <- function(ProfileID = 26, AreaTypeID = 102){
  
  data <- fingertips_latest_data(ProfileID = ProfileID, AreaTypeID = AreaTypeID)
  
  data %>%
    group_by(IndicatorID, Sex) %>%
    arrange(IndicatorID, Sex, -Value) %>%
    top_n(n = 10, .$Value) %>%
    ungroup()
  
}
 


