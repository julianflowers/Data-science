#####################################
#
# Script for extracting and tidying
# data from the SPOT tool
#
#####################################


library(tidyverse)
library(readxl)
library(magrittr)

url <- "https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/611718/spend-and-outcome-tool-ccgs.xlsm"

spot <- download.file(url, "spot.xlsx")

path <- paste0(getwd(),"/",  "spot.xlsx")

spot_data <- path %>%
  excel_sheets() %>%
  set_names()
  map(read_excel, path = path)

sheets <- path %>%
  excel_sheets() %>%
  set_names()

## Exploration

listviewer::jsonedit_gadget(spot_data)

str(spot_data[72], max.level = 2, list.len = 72)

test1 <- spot_data %>%
  map_df(dim) %>%
  map_dbl(2)

test <- spot_data[67] %>%
  map_df(extract, 1:test1[67])

oc <- sheets[grepl("^OC_DATA", sheets)]

names(spot_data[[1]])

head(test)
