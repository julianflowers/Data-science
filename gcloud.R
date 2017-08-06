library(rvest)
library(tidyverse)
library(stringr)

url <- "https://www.digitalmarketplace.service.gov.uk/g-cloud/suppliers"

## function for extracting titles or text from urls
exText <- function(url, node = "a"){
  page <- read_html(url)
  links <- page %>%
    html_nodes(node) %>%
    html_text
  links <- as.tibble(links)
}

test1 <- exText(suppliers_list[1:26]) 

suppliers <- links[grepl("supplier|suppliers", x = links)]

suppliers_list <- paste0("https://www.digitalmarketplace.service.gov.uk/g-cloud/suppliers?prefix=",LETTERS)


listsup <- suppliers_list %>%
  map(exText) %>% unlist %>% as.tibble()


listsup_desc <- suppliers_list %>%
  map(exText, node = "p") %>%
  unlist() %>%
  as.tibble()


listsup_desc %>%
  DT::datatable()



## Identify suppliers by topic

gcloudTopic <- function(search = "health"){


listsup_desc %>%
    filter(stringr::str_detect(value, "search")) %>%
    DT::datatable()
  
  
}

gcloudTopic("[Dd]ata")

##

supplier1 <- read_html("https://www.digitalmarketplace.service.gov.uk/g-cloud/supplier/702547")




ex1 <- exText("https://www.digitalmarketplace.service.gov.uk/g-cloud/supplier/702547")

supplier_email <- function(id = 702547){
  
  url <- paste0("https://www.digitalmarketplace.service.gov.uk/g-cloud/supplier/", id)
  
  ex <- exText(url)
  
  emails <- ex %>%
    filter(stringr::str_detect(value, "@"))
  
  emails[2, ]
  
}


gcloudTopic(search = "[Bb]ig [Dd]ata")

supplier_email(700018)
