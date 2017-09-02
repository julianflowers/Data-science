library(rvest)
library(tidyverse)
library(stringr)

## Scsript to extract a table of G-Cloud suppliers to make it easier to find relevant ones

url <- "https://www.digitalmarketplace.service.gov.uk/g-cloud/suppliers"
page <- read_html(url) %>% html_nodes("a") %>% html_attr("href")
gs <- page[grepl(pattern =  "/g-cloud/supplier/", page)]
gsurls <- map(gs, function(x) paste0("https://www.digitalmarketplace.service.gov.uk", x))

## function for extracting titles or text from urls
exText <- function(url, node = "a"){
  page <- read_html(url) %>% html_nodes("a") %>% html_attr("href")
  gs <- page[grepl(pattern =  "/g-cloud/supplier/", page)]
  gsurls <- map(gs, function(x) paste0("https://www.digitalmarketplace.service.gov.uk", x))  

}
test <- exText(url)



suppliers_ABC <- paste0("https://www.digitalmarketplace.service.gov.uk/g-cloud/suppliers?prefix=",LETTERS)

#suppliers <- links[grepl("supplier|suppliers", x = links)]

## extract supplier ids
listsup <- suppliers_ABC %>%
  map(exText) %>% unlist()


## create datatable of suppliers names, emails and descriptions
df <- data.frame()
for(i in seq_along(listsup)){

l <- listsup[i] %>% read_html() %>% html_nodes("h1") %>% html_text()
l <- gsub("\\n", "", l)
supplier_name <- tm::stripWhitespace(l)

l1 <- listsup[i] %>% read_html() %>% html_nodes(".supplier-description") %>% html_text()
l1 <- gsub("\\n", "", l1)
supplier_details <- tm::stripWhitespace(l1)

l2 <- listsup[i] %>% read_html() %>% html_nodes("a") %>% html_text()
supplier_email <- l2[grepl("@", l2)][2]


df1 <- data.frame(cbind(supplier_name, supplier_details, supplier_email ))

df <- bind_rows(df, df1)
}

df %>%
  DT::datatable(filter = "top", caption = "G-Cloud suppliers" )


