
library(rvest) ## web scraper
library(stringr) ## string manipulation
library(dplyr) ## data wrangling
library(data.table)
library(readxl)
if(!require("downloader"))install.packages("downloader")
   library(downloader)

## create directory

setwd("~")

if(!dir.exists("test_downloads")) dir.create("test_downloads")

setwd("test_downloads")
    
getwd()    

url <- 'https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/599925/PHE_GP_In_Hours_Weekly_Data_2017_Week_10.xls'

download(url, "test8.xls", mode = "wb")

list.files()


testxl <- read_excel("test8.xls", sheet = 3, skip = 4)

head(testxl)

siteAddress <- "https://www.gov.uk/government/publications/gp-in-hours-bulletin"

page <- read_html(siteAddress)




xls <- page %>%
  html_nodes("a") %>%       # find all links
  html_attr("href")


xls1 <- unique(xls[stringr::str_detect(xls, "xls$") ]) ## identify files with .xls extensions


#### Step 4 Extract filenames

xls_split <- str_split(xls1, pattern = "/")

filenames <- lapply(xls_split, "[[", 9)



urls <- lapply(xls1, function(x) paste0("https://www.gov.uk", x))
    

for(i in 1:10){
    download(urls[[i]], filenames[[i]], mode = "wb")
}

             
list.files()

## get core info from the first sheet
files <- list.files()


df <- data.frame()

for(f in files){
testx <- read_excel(f[[1]], sheet = 1) %>%slice(4:8) %>% mutate(file = paste(f[[1]]))

df <- bind_rows(df, testx)
    
    }

df %>% head

## need to convert excel serial numbers to dates

files[[1]]


library(magrittr)
df1 <- data.frame()

for(f in files){
   test1 <- read_excel(f[[1]], sheet = 3, skip = 4, na = "*") %>% 
    slice(1:149) 

   test2 <-  test1%>% 
    janitor::clean_names() %>%
    select(7:27) %>%
   select(-contains("ci")) %<>%
    lapply(function(x) as.numeric(x)) %>%
    data.frame() %>% 
    bind_cols(test1[, 1:6])  %>%
    mutate(file = paste(f[[1]]))    
        
    
df1 <- bind_rows(df1, test2)
    
}

df2 <- df1 %>% tidyr::gather(indicator, value, 1:18) %>% mutate(value = as.numeric(value))
           
df2 <- df2 %>% left_join(df)

setwd("~")

if(!dir.exists("gp_data")) dir.create("gp_data")

setwd("gp_data")
    
getwd()    


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


xls14 <- unique(xls_14[stringr::str_detect(xls_14, "xls$") ])
xls15 <- unique(xls_15[stringr::str_detect(xls_15, "xls$") ])  ## identify all xls files
xls16 <- unique(xls_16[stringr::str_detect(xls_16, "xls$") ])
xls17 <- unique(xls_17[stringr::str_detect(xls_17, "xls$") ])    

xls17

             
 
    
    

xls_all <- c(xls14, xls15, xls16, xls17) ## concatenate

xls_split <- str_split(xls_all, pattern = "/") ## split up URLs

filenames <- lapply(xls_split, "[[", 9) ## create filenames

urls_all <- lapply(xls_all, function(x) paste0("https://www.gov.uk", x)) ## create urls
    
for(i in seq_along(urls_all)){
    download(urls_all[[i]], filenames[[i]], mode = "wb")
}

files <- list.files()

files


f <- list.files()

testx <- read_excel(f[[80]], sheet = 1) %>%slice(4:8) %>% mutate(X__1 = as.numeric(X__1), file = paste(f[[1]]))
testx

  sheet <- read_excel(files[[12]], sheet = 1) %>%slice(4:8) %>% mutate(X__1 = as.numeric(X__1),file = paste(files[[12]]))
sheet


df_all <- data.frame()

for(i in seq_along(files)){

sheet <- read_excel(files[[i]], sheet = 1) %>%slice(4:8) %>% mutate(X__1 = as.numeric(X__1),file = paste(files[[i]]))

    
df_all <- bind_rows(df_all, sheet)
    
    }

df_all %>% janitor::clean_names() %>% mutate(x_1 = ifelse(!is.na(x), x, x_1)) %>% select(-x)

df_all %>% readr::write_csv("surv_metadata.csv")

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
    mutate(file = paste(f[[1]]))    
        
    
df1_all <- bind_rows(df1_all, test2)
    
}

df1_all %>% dim()

readr::write_csv(df1_all, "surv_data.csv")

