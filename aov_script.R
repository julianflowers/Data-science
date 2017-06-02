## Script for processing atlas of variation spreadsheets into tidy datasets for conversion to PHOLIO format
##==============================

## load packages
library(readxl)
library(tidyverse)
devtools::install_github(c("hadley/xml2",
                           "rsheets/linen",
                           "rsheets/cellranger",
                           "rsheets/rexcel",
                           "rsheets/jailbreakr"))
library(rexcel)
library(jailbreakr)

## test with 2015 diagnostic atlas
## download file
url <- "https://fingertips.phe.org.uk/documents/DiagAtlasData_291116.xlsx"
destfile <- "aov_diag.xlsx"
aov_diag <- download.file(url, destfile)

setwd("~/Documents/R_projects/timeSeries")

## List sheets
sheets <- data.frame(excel_sheets("aov_diag.xlsx"))

## Read in workbook
test_sheet <- rexcel_read_workbook("aov_diag.xlsx")


## Import all sheets
test_sheet$sheets


### Store sheet 1 as a table
tables <- split_sheet(test_sheet$sheets$`1`)

str(tables)


### Extract values from sheet
library(purrr)
data_list <- map(tables, function(x) x$values())

sheet <- sheets %>% as.list()

sheet[1]


# Engine
## Download sheet names - function to extract map title - this is at the top of the each datasheet in the
## second row and column

## this function extracts the indicator names
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

### identify CCG indicators

ccg_map <- map_lookup %>%
  filter(stringr::str_detect(title, "CCG"))

ccg_indicators <- ccg_map %>% separate(map, c("map", "number"))
ccg_list <- ccg_indicators %>% select(number) %>% as.list()

#### Given lack of consistency between sheets - in some cases data starts on 3rd row (1, 10, 11, 17, 18); in others on the 4th


ccg_list1 <- c("1", "10", "17", "18")
ccg_list2 <- c("2",  "3",  "4a", "5a", "6a", "9",  "15", "16", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "34", "36")
## Create a .csv file for each sheet.

### Read them all back in and identify which ones we have successfully parsed with column names
### some files need to skip 3 rows, some to skip 2 rows; also need to remove "/" from sheet 23
files <- list()

for(sheet in ccg_list1){
  
  test <- read_excel("aov_diag.xlsx", sheet = sheet, skip = 3)
  files <- write_csv(test, paste0(map_lookup[sheet, 2], ".csv"))
  
}

for(sheet in ccg_list2){
  
  test <- read_excel("aov_diag.xlsx", sheet = sheet, skip = 2) 
  files <- write_csv(test, paste0(map_lookup[sheet, 2], ".csv"))
  
}

f <- list.files(pattern = ".csv") ## find the .csv files

## select ccg files

f_ccg <- f[grepl("CCG", f)]

df <- data.frame()

for(f in f_ccg){
  
  f1 <- map(f_ccg, read_csv)
  
  f2 <- f_ccg[1] %>%
    janitor::clean_names() %>%
    select(contains("CCG|year"))
    
    %>% mutate(file = f_ccg[f])
  
  
}

m1 <- map(f_ccg, read_csv )
map(m1, head) 

f1 <- read_csv(f_ccg[4]) %>% mutate(file = f_ccg[4])
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


#### Mapping the data

We can map these data -  a quick way is to read the GeoJSON files from the [ONS Geography Portal](http://geoportal.statistics.gov.uk/datasets/1bc1e6a77cdd4b3a9a0458b64af1ade4_2) and plot with `tmap` package.

```{r create a map, cache=TRUE}
if(!require("ggmap"))install.packages("ggmap")
library(ggmap)
if(!require("tmap"))install.packages("tmap")
library(tmap)
library(rgeos)
if(!require("rgdal"))install.packages("rgdal")
library(rgdal)
if(!require("geojsonio"))install.packages("geojsonio")
library(geojsonio)
library(gganimate)
if(!require("viridis"))install.packages("viridis")
library(viridis)

## get super generalised CCG shape file from the ONS geography portal (use 2015 boundaries which have 209 CCGs)

ccg_map <- geojson_read("http://geoportal.statistics.gov.uk/datasets/67993b98f52743899751f188c960f7df_3.geojson", what = "sp")



ccg <- fortify(ccg_map, region = "ccg15cd")

ccg_map1<- ccg %>%
  left_join(map1, by = c("id" = "CCG code"))

palette <- rev(plasma(15))
credits <- "Contains ordnance survey data (c) \nCrown copyright and database right 2016"


g <- ccg_map1 %>%
  filter(stringr::str_detect(Period , "Q" )) %>%
  ggplot() +
  geom_polygon(
    aes(long, lat, 
        group = group,
        fill = Rate)) +
  coord_map() + theme_minimal() + 
  scale_fill_viridis(name = "Rate", option = "plasma") +
  theme(axis.text = element_blank(), axis.title = element_blank()) +
  facet_wrap(~Period)


g

```


