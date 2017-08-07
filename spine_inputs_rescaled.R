# IndicatorData is a numerical vector of data for all child areas
# Polarity is string; one of:
## Not applicable
## RAG - Low is good
## RAG - High is good
## BOB - Blue orange blue

spine_inputs_rescaled <- function(UniqueIndicatorID, IndicatorData, IndicatorName, 
                                  Polarity, AreaCode, AreaCodes,
                                  Significance, IncludeRegion = FALSE) {
        Polarity <- stringr::str_trim(Polarity)
        area_val <- IndicatorData[AreaCodes == AreaCode]
        area_sig <- Significance[AreaCodes == AreaCode]
        df <- data.frame(UniqueGroup = UniqueIndicatorID,
                         IndicatorName = IndicatorName,
                         Polarity = Polarity) %>%
                mutate(wrst = ifelse(grepl("Low is good",Polarity),
                                     max(IndicatorData, na.rm = TRUE), 
                                     min(IndicatorData, na.rm = TRUE)),
                       bst = ifelse(grepl("Low is good",Polarity),
                                    min(IndicatorData, na.rm = TRUE), 
                                    max(IndicatorData, na.rm = TRUE)),
                       scale_min = ifelse(mean(IndicatorData, na.rm = TRUE) - min(IndicatorData, na.rm = TRUE) > 
                                                  max(IndicatorData, na.rm = TRUE) - mean(IndicatorData, na.rm = TRUE),
                                          min(IndicatorData, na.rm = TRUE),
                                          mean(IndicatorData, na.rm = TRUE) - (max(IndicatorData, na.rm = TRUE) - mean(IndicatorData, na.rm = TRUE))),
                       scale_max = ifelse(scale_min == min(IndicatorData, na.rm = TRUE),
                                          mean(IndicatorData, na.rm = TRUE) + (mean(IndicatorData, na.rm = TRUE) - min(IndicatorData, na.rm = TRUE)),
                                          max(IndicatorData, na.rm = TRUE)),
                       scale_wrst = ifelse(grepl("Low is good",Polarity),
                                           scale_min,
                                           scale_max),
                       scale_bst = ifelse(grepl("Low is good",Polarity),
                                          scale_max,
                                          scale_min),
                       wrst_grph = ifelse(grepl("Low is good",Polarity),
                                          (min(IndicatorData, na.rm = TRUE) - scale_wrst) / (scale_bst - scale_wrst),
                                          (max(IndicatorData, na.rm = TRUE) - scale_wrst) / (scale_bst - scale_wrst)),
                       q25_grph = ifelse(grepl("Low is good",Polarity),
                                        ((quantile(IndicatorData, probs = 0.25, na.rm = TRUE) - scale_wrst) / (scale_bst - scale_wrst)) - wrst_grph,
                                        ((quantile(IndicatorData, probs = 0.75, na.rm = TRUE) - scale_wrst) / (scale_bst - scale_wrst)) - wrst_grph),
                       q50_grph = 0.5,
                       q75_grph = ifelse(grepl("Low is good",Polarity),
                                         ((quantile(IndicatorData, probs = 0.75, na.rm = TRUE) - scale_wrst) / (scale_bst - scale_wrst)) - (wrst_grph + q25_grph),
                                         ((quantile(IndicatorData, probs = 0.25, na.rm = TRUE) - scale_wrst) / (scale_bst - scale_wrst)) - (wrst_grph + q25_grph)),
                       bst_grph = ifelse(grepl("Low is good",Polarity),
                                         ((max(IndicatorData, na.rm = TRUE) - scale_wrst) / (scale_bst - scale_wrst)) - (wrst_grph + q25_grph + q75_grph),
                                         ((min(IndicatorData, na.rm = TRUE) - scale_wrst) / (scale_bst - scale_wrst)) - (wrst_grph + q25_grph + q75_grph)),
                       area_grph = (area_val - scale_wrst) / (scale_bst - scale_wrst),
                       sig_grph = area_sig)
        if (IncludeRegion == TRUE) {
                df <- mutate(df,
                             rgn_grph = (mean(IndicatorData, na.rm = TRUE) - scale_wrst) / (scale_bst - scale_wrst))
        }
        return(df)
}


multi_indicator_spine <- function(UniqueIndicatorIDs, IndicatorName, 
                                  IndicatorData, Polarity, AreaCode, AreaCodes, 
                                  Significance, IncludeRegion = FALSE) {
        df <- data.frame()
        for (UniqueIndicatorID in unique(UniqueIndicatorIDs)) {
                IndicatorNameLoop <- unique(IndicatorName[UniqueIndicatorIDs == UniqueIndicatorID])
                PolarityLoop <- unique(Polarity[UniqueIndicatorIDs == UniqueIndicatorID])
                IndicatorDataLoop <- IndicatorData[UniqueIndicatorIDs == UniqueIndicatorID]
                AreaCodesLoop <- AreaCodes[UniqueIndicatorIDs == UniqueIndicatorID]
                SignificanceLoop <- as.character(Significance[UniqueIndicatorIDs == UniqueIndicatorID])
                df <- rbind(spine_inputs_rescaled(UniqueIndicatorID, IndicatorDataLoop, IndicatorNameLoop, 
                                                  PolarityLoop, AreaCode, AreaCodesLoop,
                                                  SignificanceLoop, IncludeRegion),
                            df)
        }
        df <- filter(df, complete.cases(df)) %>%
                select(matches("^UniqueGroup|^IndicatorName|grph$")) %>%
                gather(GraphPoint, Value, -(UniqueGroup:IndicatorName))
        df$GraphPoint <- factor(df$GraphPoint,
                                levels = c("bst_grph","q75_grph",
                                           "q25_grph","wrst_grph","area_grph",
                                           "rgn_grph","q50_grph","sig_grph"))
        return(df)
}

data <- fingertips_data(ProfileID = 26)
InputData <- filter(data, 
                    AreaType == "County & UA") %>%
        group_by(IndicatorID) %>%
        filter(TimeperiodSortable == max(TimeperiodSortable)) %>%
        ungroup()
gps <- InputData %>% 
        group_by(IndicatorID, Sex, Age, Category, CategoryType) %>%
        group_indices()
InputData <- InputData %>%
        mutate(group = gps) %>%
        ungroup()
        
polarity <- indicator_metadata(unique(InputData$IndicatorID)) %>%
        select(IndicatorID, Polarity)

InputData <- left_join(InputData, polarity, by = c("IndicatorID" = "IndicatorID"))

dfgraph <- multi_indicator_spine(InputData$group,InputData$IndicatorName, InputData$Value, 
                                 InputData$Polarity, "E06000022", InputData$AreaCode, 
                                 InputData$ComparedtoEnglandvalueorpercentiles,
                                 IncludeRegion = TRUE) %>%
        mutate(IndicatorName = paste0(stringr::str_trunc(as.character(IndicatorName), width = 20, "right"),
                                      " (",UniqueGroup,")"))

dfgrapharea <- dfgraph[dfgraph$GraphPoint %in% c("area_grph","sig_grph"),] %>%
        spread(GraphPoint,Value) %>%
        mutate(area_grph = as.numeric(area_grph),
               sig_grph = factor(sig_grph, levels = c("Better","Same","Worse","Not compared")))
other_inds <- c("area_grph","rgn_grph","q50_grph","sig_grph")
dfgraphbars <- dfgraph[!(dfgraph$GraphPoint %in% other_inds),] %>%
        mutate(Value = as.numeric(Value)) %>%
        droplevels()

cols <- c("wrst_grph" = "white", "q25_grph" = "lightgrey", "q75_grph" = "darkgrey",
          "bst_grph" = "lightgrey")

pointcols <- data.frame(category = c("Better","Same","Worse","Not compared"),
                   r = c(146, 255, 192, 166),
                   g = c(208, 192, 0, 166),
                   b = c(80, 0, 0, 166)) %>%
        mutate(hex = rgb(r,g,b, maxColorValue = 255))
pointcols <- structure(pointcols$hex,
                  names = as.character(pointcols$category))

other_inds <- c("area_grph","rgn_grph","q50_grph","sig_grph")
ggplot(dfgraphbars, 
       aes(x = IndicatorName, y = Value)) +
        geom_bar(stat = "identity", aes(fill = GraphPoint)) +
        geom_point(data = dfgrapharea,
                   aes(x = IndicatorName, y = area_grph, col = sig_grph)) +
        geom_hline(yintercept = 0.5, col = "darkred") +
        coord_flip() +
        scale_fill_manual(values = cols) +
        scale_colour_manual(name = "",
                            values=pointcols,
                            breaks = names(pointcols)) +
        theme_minimal() +
        labs(x = "", y = "") +
        theme(axis.text.x = element_blank(),
              #axis.text.y = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              legend.position = "none")
