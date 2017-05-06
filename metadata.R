library(fingertipsR)
library(dplyr)
library(stringr)
library(tidytext)
library(corrr)
library(gganimate)
library(ggplot2)

install.packages('installr')
        install.packages('devtools')
        install.ImageMagick()
        devtools::install_github("yihui/animation")

devtools::install_github("dgrtwo/widyr")
library(widyr)

ind_risk <- indicators()
head(ind_risk)

profile_risk <- profiles()
profile_risk %>%
  select(ProfileID, ProfileName) %>%
  distinct

ind_meta <- indicator_metadata(ProfileID = c(8, 32, 94, 98, 121, 129, 37))
head(ind_meta)
str(ind_meta)

surveys <- ind_meta %>%
  filter(str_detect(Data.source,"[Ss]urvey" )) %>%
  select(Data.source)


profiles <- read.csv("~/Documents/R_projects/profiles/profile.csv")
glimpse(profiles)

corr <- correlate(profiles[, -c(1:2)])
network_plot(corr, colors = c("red", "green"), min_cor = 0.7, legend = TRUE)

library(purrr)
corr %>% shave(upper = FALSE) %>% rplot() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

### Animate.....

phof <- fingertips_data(ProfileID = 19)

unique(phof$IndicatorName)

p <- phof %>%
  filter(IndicatorName == "0.1ii - Life expectancy at 65"  |IndicatorName == "0.1ii - Life expectancy at birth" , AreaType == "County & UA", CategoryType == "") %>%
  select(IndicatorName, ParentName, AreaName, Timeperiod, Sex, Value) %>% 
  spread(IndicatorName, Value)


p1 <- p %>%
  ggplot(aes( Sex,`0.1ii - Life expectancy at 65`, fill = ParentName,shape = Sex, frame = Timeperiod)) +
  geom_boxplot() +
  facet_wrap(~ParentName) +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))



gganimate(p1, filename = "gapminder-gganimate.html")
