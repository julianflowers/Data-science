Sys.setenv(https_proxy = "https://158.119.150.18:8080")
Sys.setenv(http_proxy = "http://158.119.150.18:8080")


library(R2PPT) 
library(RDCOMClient)
library(fingertipsR)
library(dplyr)
library(ggplot2)
library(stringr)
library(forcats)


## Initialise
my.slides<- PPT.Init(method="RDCOMClient")




##==========================================================================================================

## Title slide

title <- "title: replace text"
subtitle <- "subtitle: replace text"

my.slides <- PPT.AddTitleSlide(my.slides,title="Automated ppt from R",
                               subtitle="...using the Fingertips API")

title <- "The `fingertipsR` package"
text <- "5 functions \rprofiles() - extracts profile names and ids \rindicators() - extracts indicator names and ids\rindicator_metadata() - extracts relevant metadata\rfingertips_data() - extracts the data\rdeprivation_decile()"

my.slides <- PPT.AddTextSlide(my.slides, title = title, text = text)

title <- "The `fingertipsR` package"
text <- "5 functions \rprofiles() - extracts profile names and ids \rindicators() - extracts indicator names and ids\rindicator_metadata() - extracts relevant metadata\rfingertips_data() - extracts the data\rdeprivation_decile()"

my.slides <- PPT.AddTextSlide(my.slides, title = "Extract prevalence data", text = "profiles <- profiles() %>% filter(stringr::str_detect(ProfileName, 'prevalence')) ## identifies prevalence profile\rprevalence <- fingertips_data(ProfileID = 37, DomainID = 1938133099) # download data\reven more text")

my.slides <-PPT.ApplyTemplate(my.slides, file = "C:/Users/julian.flowers/Documents/Presentation-standard.potx")

PPT.SaveAs(my.slides,file="test_slides1.pptx")

