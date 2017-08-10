library(tidyverse)
library(fingertipsR)

# This is how you use it:
# AreaCode is the area you want the chart plotted for and "..." is are the parameters you
# would pass to fingertips_data, eg:
# inds <- c(10101, 92949)
# p <- spine_chart("E06000016", IndicatorID = inds)
spine_chart <- function(AreaCode, ...) {
        data <- fingertips_data(..., rank = TRUE) %>%
                group_by(IndicatorID, CategoryType, Sex, Age) %>%
                filter(TimeperiodSortable == max(TimeperiodSortable)) %>%
                { mutate(ungroup(.), group = group_indices(.)) } %>% #copied from GitHub
                ungroup() 
        
        parentcode <- unique(as.character(data[data$AreaCode == AreaCode, "ParentCode"]$ParentCode))
        
        create_point_data <- function(df, areacode){
                if (substr(areacode,1,3) %in% c("E92","E12")){
                        df <- df %>%
                                filter(AreaCode == areacode) %>%
                                select(group, Value) %>%
                                rename(regionalvalue = Value)
                } else {
                        df <- df %>%
                                filter(AreaCode == areacode) %>%
                                select(group, ComparedtoEnglandvalueorpercentiles, 
                                       Polarity, Value) %>%
                                rename(areavalue = Value,
                                       Significance = ComparedtoEnglandvalueorpercentiles)
                }
                df <- data.frame(df) %>%
                        column_to_rownames(var="group")
        }
        parentdata <- create_point_data(data, parentcode)
        areadata <- create_point_data(data, AreaCode)
        mean <- create_point_data(data, "E92000001")
        data <- filter(data, AreaType == "County & UA")
        
        quantiles <- data %>%
                split(.$group) %>%
                map("Value") %>%
                map_df(quantile, na.rm = TRUE) %>%
                t() %>%
                data.frame()
        names(quantiles) <- c(paste0("Q",100*seq(0,1,by=0.25)))
        quantiles[,3] <- NULL
        quantiles <- merge(quantiles, mean, by = 0, all.x = TRUE) %>%
                column_to_rownames(var="Row.names") %>%
                rename(mean = regionalvalue)
        
        scaled_spine_inputs <- function(group, Q0, Q25, mean, Q75, Q100, Significance, Polarity, areavalue, regionalvalue) {
                Polarity <- stringr::str_trim(Polarity)
                quantiles <- structure(as.numeric(c(Q0, Q25, mean, Q75, Q100)),
                                       names = c("0%", "25%", "mean", "75%", "100%"))
                areavalue <- as.numeric(areavalue)
                regionalvalue <- as.numeric(regionalvalue)
                if (grepl("Low is good",Polarity)) {
                        quantiles <- rev(quantiles)
                }
                scale_min <- ifelse(quantiles["mean"] - quantiles["0%"] > 
                                            quantiles["100%"] - quantiles["mean"],
                                    quantiles["0%"],
                                    quantiles["mean"] - (quantiles["100%"] - quantiles["mean"]))
                scale_max <- ifelse(scale_min == quantiles["0%"],
                                    quantiles["mean"] + (quantiles["mean"] - quantiles["0%"]),
                                    quantiles["100%"])
                
                rescale <- function(val){
                        rescale <- (val - scale_min) / (scale_max - scale_min)
                        return(rescale)
                }
                quantiles <- rescale(quantiles[names(quantiles) != "mean"])
                if (grepl("Low is good",Polarity)) {
                        quantiles <- 1 - quantiles
                        quantiles <- diff(c(0,quantiles))
                } else {
                        quantiles <- diff(c(0,quantiles))
                }
                pointdata <- rescale(c(areavalue,regionalvalue))
                names(pointdata) <- c("area","region")
                graphpoints <- c("Worst","Q25","Q75","Best")
                scaled_spine_inputs <- list(bars = data.frame(group = group, 
                                                              quantiles = quantiles,
                                                              GraphPoint = factor(graphpoints, levels = rev(graphpoints))),
                                            points = data.frame(group = group, significance = Significance, area = pointdata[1], region = pointdata[2]))
        }
        
        dfgraph <- merge(quantiles, areadata, by = 0, all.x = TRUE) %>%
                column_to_rownames(var="Row.names") %>%
                merge(parentdata, by = 0, all.x =TRUE) %>%
                rename(group = Row.names) %>%
                lapply(map, .f = as.character) %>%
                pmap(scaled_spine_inputs)
        dfgraphfinal <- list(bars = suppressWarnings(map_df(dfgraph, "bars")),
                             points = suppressWarnings(map_df(dfgraph, "points")))
        
        cols <- data.frame(category = c("Better","Same","Worse",
                                             "Not compared","Higher", 
                                             "Similar", "Lower",
                                             "Worst","Q25","Q75","Best"),
                                r = c(146, 255, 192, 166, 190, 255, 85, 255, 201, 139, 201),
                                g = c(208, 192, 0, 166, 210, 192, 85, 255, 201, 139, 201),
                                b = c(80, 0, 0, 166, 255, 0, 230, 255, 201, 139, 201)) %>%
                mutate(hex = rgb(r,g,b, maxColorValue = 255))
        cols <- structure(cols$hex,
                               names = as.character(cols$category))
        ggplot(dfgraphfinal$bars, 
               aes(x = group, y = quantiles)) +
                geom_bar(stat = "identity", aes(fill = GraphPoint)) +
                geom_point(data = dfgraphfinal$points,
                           aes(x = group, y = region), shape = 23, fill = "white") +
                geom_point(data = dfgraphfinal$points,
                           aes(x = group, y = area, fill = significance), 
                           shape = 21, colour = "black") +
                geom_hline(yintercept = 0.5, col = "darkred") +
                coord_flip() +
                scale_fill_manual(values = cols) +
                theme_minimal() +
                labs(x = "", y = "") +
                theme(axis.text.x = element_blank(),
                      #axis.text.y = element_blank(),
                      panel.grid.major = element_blank(), 
                      panel.grid.minor = element_blank(),
                      legend.position = "none")
}
