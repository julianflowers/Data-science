## Script for processing atlas of variation spreadsheets into tidy datasets for conversion to PHOLIO format
##==============================

library(readxl)
library(tidyverse)
devtools::install_github(c("hadley/xml2",
                           "rsheets/linen",
                           "rsheets/cellranger",
                           "rsheets/rexcel",
                           "rsheets/jailbreakr"))
install.packages("rexcel")

## test with 2015 diagnostic atlas

url <- "https://fingertips.phe.org.uk/documents/DiagAtlasData_291116.xlsx"
destfile <- "aov_diag.xlsx"
aov_diag <- download.file(url, destfile)

## List sheets
sheets <- data.frame(excel_sheets("aov_diag.xlsx"))

sheet <- sheets %>% as.list()

## Download sheet names - function to extract map title
map_title <- function(sheet = sheet){
  title <- read_excel("aov_diag.xlsx", sheet = sheet, skip = 1)
  names(title %>% slice(2) %>% select(2))
}

### loop over list of sheets to extract map numbers and titles
titles <- list()

for(i in sheet$excel_sheets..aov_diag.xlsx..){
  map_titles <- map_title(sheet = i)
  titles <- rbind(titles, map_titles)
}


## convert to data frame and split map number from title
map_lookup <- titles %>% unlist() %>% data.frame() 
colnames(map_lookup) <- "maps"
map_lookup <- map_lookup %>% 
  separate(maps, c("map", "title"), sep = ":")

#### Given lack of consistency between sheets - start by downloading each on separately 
## Create a .csv file for each sheet.

### Read them all back in and identify which ones we have successfully parsed with column names

files <- list()
for(i in sheet$excel_sheets..aov_diag.xlsx..){
  
  test <- read_excel("aov_diag.xlsx", sheet = i, skip = 2)
  files <- write_csv(test, paste0(map_lookup[i, 2], ".xlsx"))
  
}


f <- list.files(pattern = ".csv") ## find the .csv files

## select ccg

f_ccg <- f[grepl("CCG", f)]

df <- data.frame()

for(f in f_ccg){
  
  f1 <- read_csv(f_ccg[f]) %>% mutate(file = f_ccg[f])
  
  
}

f1 <- read_csv(f_ccg[1]) %>% mutate(file = f_ccg[1])
names(f1)
f1_l <- f1 %>%
  slice(2:nrow(.)) %>%
  mutate(Admissions = as.numeric(Admissions), 
         `Population` = as.numeric(`Population\r\n(3-year)`),
         DSR = as.numeric(DSR)) %>% 
  select(file, everything()) %>%
  janitor::clean_names() %>%
  select(-population_3_year) %>%
  tidyr::gather(metric, value, admissions:population) 

unique(f1_l$ccg_name)

which(is.na(f1_l$value))

## quick analysis for England

f1_l %>%
  filter(metric %in% c("dsr", "x99_8percent_lower","x99_8percent_upper" )) %>%
  ggplot(aes(year, value)) +
  geom_boxplot()

## trendss

map <- list()
for (file in f) {
  
  file <- read_csv(file) %>% mutate(file = file)
  map <- bind_rows(file, map)
  
}

map %>% select(file)



testmap <- purrr::map(f, readr::read_csv) ## read them

testmap

names <- purrr::map(testmap, glimpse) ## get column names



## look like we have succeeded for 1,2,4,5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19
## 20, 21, 22, 23, 24, 25, 27, 36, 37, 38
## we'll try and sort these first.
## filter these out
success <- c(1,2,3,4,5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 25, 28, 29, 30, 31)
testmap1 <- testmap[success]

names1 <- names[success]
ccg <- testmap1[grepl("CCG", names1)] ## CCG data
utla <- testmap1[grepl("UTLA", names1)] ## UTLA data
trust <- testmap1[grepl("Trust", names1)] ## Trust data


## Just focus on CCG data

## ccg lookups

ccg_lookups <- map_lookup[grepl("CCG", map_lookup$title),]
dim(ccg_lookups)
## get the dimensions of each sheet
ncol_ccg <-purrr::map(ccg, ncol)
ncol_ccg1 <- purrr::map(ncol_ccg, ncol)


## try tidyr

ccg[1] %>%
  data.frame() %>%
  tidyr::gather(metrics, values, Numerator:Sigband)
