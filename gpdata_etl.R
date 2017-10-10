library(tidyverse)

fingertipsR::areatypes_by_indicators %>%
  filter(AreaTypeID == 7) %>%
  pull(IndicatorID) -> gpinds

gpdata <- fingertipsR::fingertips_data(IndicatorID = gpinds, AreaTypeID = 7)

data.table::fwrite(gpdata, "gpdata_analysis")
