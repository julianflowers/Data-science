## Analysis of weekly in hours gp surveillance data

library(ggplot2)
library(dplyr)


data <- readr::read_csv("~/gp_data/surv_data.csv") %>% janitor::clean_names()

data %>% str(max.level = 1)

unique(data$file)

metadata <- readr::read_csv("~/gp_data/surv_metadata.csv") %>% janitor::clean_names()

metadata %>% str(max.level = 1)


## Extract week start data from `metadata`

week <- filter(metadata, metadata == "Date starting" )

week <- week %>% mutate(x4 = as.Date(value, origin = "1899-12-30"))

week <- week %>% select(file, x4)



## tidy data

data_l <- data %>%
  tidyr::gather(indicator, value, 1:17)

data_l <- data_l %>% left_join(week)


unique(data_l$indicator)


data_l1 <- data_l %>% 
  filter(indicator == "rate_per_100_000" | indicator == "sir") %>%
  group_by(indicator, x4) %>%
  summarise(mean_rate = mean(value, na.rm = TRUE))

data_l1

data_l1 %>%
  ggplot(aes(x4, mean_rate, colour = indicator)) +
  geom_smooth(span = 0.3, , lwd = .5, lty = "dotted" ) +
  geom_line(lwd = 0.3)+
  labs(title = "Mean weekly GP attendance rate for flu-like illness",
       y = "Rate per 100,000", 
       x = "Date", 
       caption = "Source: GP syndromic surveillance system") +
  theme_bw() +
  facet_wrap(~indicator, scales = "free")
