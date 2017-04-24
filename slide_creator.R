Sys.setenv(https_proxy = "https://158.119.150.18:8080")
Sys.setenv(http_proxy = "http://158.119.150.18:8080")


library(R2PPT) 
library(RDCOMClient)
library(fingertipsR)
library(dplyr)
library(ggplot2)
library(stringr)
library(forcats)


##ggplot2 themes
##=============================================================================================

theme_gov <- function(
    base_size = 12,
    base_colour = "gray40",
    axes = "x"
) {
    
    if (!axes %in% c("n","x","y","xy")) {
        
        stop("axes must be one of 'n', 'x', 'y', or 'xy'")
        
    }
    
    ## Set x and y axis colour
    
    x_col = "white"
    y_col = "white"
    
    if (axes == "x") {
        
        x_col = base_colour
        y_col = "white"
        
    }
    
    if (axes == "y") {
        
        x_col = "white"
        y_col = base_colour
        
    }
    
    if (axes == "xy") {
        
        x_col = base_colour
        y_col = base_colour
        
    }
    
    theme(
        legend.position = "none",
        
        ## Adjust tick marks
        
        axis.ticks = ggplot2::element_blank(),
        #axis.ticks = element_line(colour = "gray40"),
        #axis.ticks.y = element_blank(),
        #axis.ticks.length = grid::unit( -2, "mm"),
        
        ## Adjust the axis lines
        
        axis.line = ggplot2::element_line(colour = base_colour),
        axis.line.x = ggplot2::element_line(colour = x_col),
        axis.line.y = ggplot2::element_line(colour = y_col),
        
        ## Set the overall text attributes
        
        text = ggplot2::element_text(
            face = "plain", colour = base_colour, size = base_size,
            hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.8
        ),
        axis.text = ggplot2::element_text(colour = base_colour),
        plot.title = ggplot2::element_text(face = "bold", hjust = 1, colour = "black", vjust = -2.5),
        
        ## Axis title attributes. Adjustments of
        
        axis.title.y = ggplot2::element_text(hjust = 1, vjust = 1),
        axis.title.x = ggplot2::element_text(hjust = 1, vjust = 0),
        
        ## Background attributes (currently all blank)
        
        panel.background = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        
        ##Adjust the margin around the plot. This is highly sensitive to plot, so
        ##probably needs to be set on a plot by plot basis.
        
        #plot.margin = grid::unit(c(0,5,5,-30), "mm"),
        
        ## Strip attributes for facet grid and facet wrap
        
        strip.background =   ggplot2::element_blank(),
        strip.text =         ggplot2::element_text(color = "black", face = "bold", size = base_size + 1),
        strip.text.x =       ggplot2::element_text(),
        strip.text.y =       ggplot2::element_text(angle = -90),
        
        complete = FALSE
    )
}

phe_red <- "#98002E"
phe_green <- "#00AE9E"
theme_phe <- function(){
    theme_minimal() +
        theme(    
            plot.title = element_text(colour=phe_green, lineheight=.8, face="bold", size=18), 
            axis.title.x = element_text(face="bold", colour=phe_red, size=18) , 
            axis.title.y = element_text(face="bold", colour=phe_red, size=18), 
            axis.text = element_text(face="bold",colour="black", size=12) , 
        )
}
        
       
##==========================================================================================================

## Initialise
my.slides<- PPT.Init(method="RDCOMClient")

##==========================================================================================================

## Title slide

title <- "title: replace text"
subtitle <- "subtitle: replace text"

my.slides <- PPT.AddTitleSlide(my.slides,title="Automated ppt from R",
                            subtitle="...using the Fingertips API")


## Get some data (disease prevalence)
##==================================================================================================
area_types()

profiles <- profiles() %>% filter(stringr::str_detect(ProfileName, "prevalence")) ## identifies prevalence profile

prevalence <- fingertips_data(ProfileID = 37, AreaTypeID = c(101, 19))

levels(prevalence$IndicatorName)

unique(prevalence$IndicatorName)

prev_head <- head(prevalence, 20)

##===================================================================================================

## Add text slide

title <- "title: some text"
text <- "text \r texr \r text"

my.slides <- PPT.AddTextSlide(my.slides, title = "Extract prevalence data", text = "profiles <- profiles() %>% filter(stringr::str_detect(ProfileName, 'prevalence')) ## identifies prevalence profile\rprevalence <- fingertips_data(ProfileID = 37, DomainID = 1938133099) # download data\reven more text")

##====================================================================================================

## Add dataframe
##====================================================================================================
my.slides <- PPT.AddBlankSlide(my.slides)

my.slides <- PPT.AddDataFrame(my.slides, prev_head)
##====================================================================================================


## Apply template
##====================================================================================================
my.slides <-PPT.ApplyTemplate(my.slides, file = "C:/Users/julian.flowers/Documents/Presentation-standard.potx")


##====================================================================================================

## Create multiple charts
##====================================================================================================

for(i in levels(prevalence$CategoryType)) {
    for(j in unique(prevalence$IndicatorName)) {

prevalence %>%
    filter(IndicatorName == j & CategoryType == i) %>%
    ggplot(aes(Category, Value)) +
    geom_bar(stat = "Identity") +
    labs(title = str_wrap(paste(j ,i , sep = " ")) ,
         y = "Prevalence (%)", 
         x = "") +
    coord_flip() + 
    theme_phe() +
    theme(axis.text.y = element_text(size = 6, hjust = 1))    

ggsave(my_temp_file<- paste(tempfile(), ".png", sep = ""), width = 20, height = 12, units = "cm")

my.slides <- PPT.AddBlankSlide(my.slides)

my.slides <- PPT.AddGraphicstoSlide(my.slides, file = my_temp_file)

unlink(my_temp_file)

}
}


PPT.SaveAs(my.slides,file="test_slides.pptx")

