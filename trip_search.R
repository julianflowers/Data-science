trip_searchR <- function(search_term, n = 1000){

  require(rvest)
  require(tidyverse)


  search_url <- url <- paste0("https://www.tripdatabase.com/search/xml?key=PHE31413&criteria=", search_term, "&max=", n)

  search <- read_xml(url)


  ## Extract IDs
  ids <- search %>%
    xml_contents() %>%
    xml_nodes("id") %>%
    as_list() %>%
    unlist()

  titles <- search %>%
    xml_contents() %>%
    xml_nodes("title") %>%
    as_list() %>%
    unlist() %>%
    cbind(ids)

  links <- search %>%
    xml_contents() %>%
    xml_nodes("link") %>%
    as_list() %>%
    unlist() %>%
    cbind(titles)

  date <- search %>%
    xml_contents() %>%
    xml_nodes("pubDate") %>%
    as_list() %>%
    unlist() %>%
    cbind(links)

result <- data.frame(date)
colnames(result) <- c("date", "link", "title", "id")
result <- result %>%
  select(id, date, title, link) %>%
  mutate_if(is.factor, as.character)

}



