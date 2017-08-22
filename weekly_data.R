### Full production version of script to download gp in hours weekly bulletins
library(rvest)
library(downloader)
library(readxl)
library(tidyverse)


## Create directory to receive downloads 


if(!dir.exists("gp_data")) dir.create("gp_data")

setwd("gp_data") ## check

getwd()    ## check


## Set up root URLs
siteAddress_14 <- "https://www.gov.uk/government/publications/gp-in-hours-weekly-bulletins-for-2014"
siteAddress_15 <- "https://www.gov.uk/government/publications/gp-in-hours-weekly-bulletins-for-2015"
siteAddress_16 <- "https://www.gov.uk/government/publications/gp-in-hours-weekly-bulletins-for-2016"
siteAddress_17 <- "https://www.gov.uk/government/publications/gp-in-hours-bulletin"



## Read pages

pages14 <- read_html(siteAddress_14)
pages15 <- read_html(siteAddress_15)
pages16 <- read_html(siteAddress_16)
pages17 <- read_html(siteAddress_17)    

xls_14 <- pages14 %>%
  html_nodes("a") %>%       # find all links
  html_attr("href")


xls_15 <- pages15 %>%
  html_nodes("a") %>%       # find all links
  html_attr("href")

xls_16 <- pages16 %>%
  html_nodes("a") %>%       # find all links
  html_attr("href")

xls_17 <- pages17 %>%
  html_nodes("a") %>%       # find all links
  html_attr("href") 

## Identify xls files on each page

xls14 <- unique(xls_14[stringr::str_detect(xls_14, "xls$") ])
xls15 <- unique(xls_15[stringr::str_detect(xls_15, "xls$") ])  ## identify all xls files
xls16 <- unique(xls_16[stringr::str_detect(xls_16, "xls$") ])
xls17 <- unique(xls_17[stringr::str_detect(xls_17, "xls$") ])     

## Create a list

xls_all <- c(xls14, xls15, xls16, xls17) ## concatenate


## split up URLs

xls_split <- str_split(xls_all, pattern = "/") 


## create filenames

filenames <- lapply(xls_split, "[[", 9) ## create filenames

## create urls

urls_all <- lapply(xls_all, function(x) paste0("https://www.gov.uk", x)) 

## Download spreadsheets  (n ~ 200)                 

for(i in seq_along(urls_all)){
  download(urls_all[[i]], filenames[[i]], mode = "wb")
}

## Create and empty list of files

files <- list.files()


## Check
files[1:4]



## Extract rows 4-8 from sheet 1 in each file - this contains key metadata
df_all <- data.frame() ## empty data frame

for(i in seq_along(files[1:144])){
  
  sheet <- read_excel(files[[i]], sheet = 1) %>%slice(4:8) %>% mutate(X__1 = as.numeric(X__1),file = paste(files[[i]]))
  
  
  df_all <- bind_rows(df_all, sheet)
  
}


## Clean up data and save as a .csv 
df_all <- df_all %>% janitor::clean_names() 


df_all <- df_all %>% mutate(x_1 = ifelse(is.na(x_1), x_2, x_1)) %>% select(1:3)

df_all <- df_all %>%  select(metadata = general_practitioner_in_hours_syndromic_surveillance_system, value =  x_1, file)


## Extract core data - this is sheet 3

library(magrittr)
df1_all <- data.frame()

for(i in seq_along(files)){
  test1 <- read_excel(files[[i]], sheet = 3, skip = 4, na = "*") %>% 
    slice(1:149) 
  
  test2 <-  test1%>% 
    janitor::clean_names() %>%
    select(7:27) %>%
    select(-contains("ci")) %<>%
    lapply(function(x) as.numeric(x)) %>%
    data.frame() %>% 
    bind_cols(test1[, 1:6])  %>%
    mutate(file = paste(f[[i]]))    
  
  
  df1_all <- bind_rows(df1_all, test2)
  
}
## Check size
df1_all %>% dim()

## Data summary

unique(df1_all$file)

## Save data
readr::write_csv(df1_all, "surv_data.csv")

## Write to file

df_all %>% readr::write_csv("surv_metadata.csv")


