library(tidyverse)
library(fingertipsR)
library(stringr)

# This is how you use it:
# AreaCode is the area you want the chart plotted for and "..." is are the parameters you
# would pass to fingertips_data, eg:
# inds <- c(10101, 92949)
# p <- spine_chart("E06000016", IndicatorID = inds, show.datatable = TRUE)
# p$plot is the ggplot
# p$table is the data table
spine_chart <- function(data, AreaCode, show.datatable = FALSE, ...) {

        preprocessed <- pre_process(data, AreaCode, ...)
        data <- preprocessed$data
        ind_names <- preprocessed$ind_names
        parentcode <- preprocessed$parentcode
        table_output <- data_table(data, AreaCode)   
        
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
        
        scaled_spine_inputs <- function(group, Q0, Q25, mean, Q75, Q100, Significance, Polarity, areavalue, regionalvalue, IndicatorName) {
                Polarity <- stringr::str_trim(Polarity)
                quantiles <- structure(as.numeric(c(Q0, Q25, mean, Q75, Q100)),
                                       names = c("0%", "25%", "mean", "75%", "100%"))
                areavalue <- as.numeric(areavalue)
                regionalvalue <- as.numeric(regionalvalue)
                if (grepl("Low is good",Polarity)|grepl("^BOB",Polarity)) {
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
                pointdata <- rescale(c(areavalue,regionalvalue))
                names(pointdata) <- c("area","region")
                if (grepl("Low is good",Polarity)|grepl("^BOB",Polarity)) {
                        quantiles <- 1 - quantiles
                        quantiles <- diff(c(0,quantiles))
                        pointdata <- 1 - pointdata
                } else {
                        quantiles <- diff(c(0,quantiles))
                }
                
                graphpoints <- c("Worst","Q25","Q75","Best")
                scaled_spine_inputs <- list(bars = data.frame(group = group,
                                                              #IndicatorName = paste0(IndicatorName, " (", group,")"), 
                                                              IndicatorName = IndicatorName, 
                                                              quantiles = quantiles,
                                                              GraphPoint = factor(graphpoints, levels = rev(graphpoints))),
                                            points = data.frame(group = group,
                                                                #IndicatorName = paste0(IndicatorName, " (", group,")"), 
                                                                IndicatorName = IndicatorName, 
                                                                significance = Significance, 
                                                                area = pointdata[1], 
                                                                region = pointdata[2]))
                
        }
        
        dfgraph <- merge(quantiles, areadata, by = 0, all.x = TRUE) %>%
                column_to_rownames(var="Row.names") %>%
                merge(parentdata, by = 0, all.x =TRUE) %>%
                mutate(Row.names = as.numeric(Row.names)) %>%
                left_join(ind_names, by = c("Row.names" = "group")) %>%
                rename(group = Row.names) %>%
                lapply(map, .f = as.character) %>%
                pmap(scaled_spine_inputs)
        dfgraphfinal <- list(bars = suppressWarnings(map_df(dfgraph, "bars")),
                             points = suppressWarnings(map_df(dfgraph, "points")))
        polarity <- select(areadata, Polarity) %>%
                rownames_to_column(var = "group")
        dfannotate <- rownames_to_column(quantiles, var = "group") %>%
                left_join(polarity, by = c("group" = "group")) %>%
                mutate(reverse = ifelse(grepl("Low is good",Polarity)|grepl("^BOB",Polarity),TRUE,FALSE),
                       Worst = ifelse(reverse == TRUE,Q100,Q0),
                       Best = ifelse(reverse == TRUE, Q0, Q100)) %>%
                select(group, Best, Worst) %>%
                gather(GraphPoint, label, Best:Worst) %>%
                mutate(y = ifelse(GraphPoint == "Best", 1.05, -0.05),
                       label = round(label,1),
                       GraphPoint = factor(GraphPoint, levels = c("Best","Q75","Q25","Worst")))
        
        timeperiod <- unique(data[,c("group","Timeperiod")]) %>%
                mutate(group = as.character(group))
        areadata <- rownames_to_column(areadata, var = "group") %>%
                select(group, areavalue)
        parentdata <- rownames_to_column(parentdata, var = "group")
        mean <- rownames_to_column(mean, var = "group") %>%
                rename(England = regionalvalue)
        
        dfannotatepoints <- left_join(parentdata, mean, by = c("group" = "group")) %>%
                left_join(areadata, by = c("group" = "group")) %>%
                left_join(timeperiod, by = c("group" = "group")) %>%
                mutate(England = round(England,1),
                       regionalvalue = round(regionalvalue,1),
                       areavalue = round(areavalue,1))
                
        
        dfgraphfinal$bars$IndicatorName <- factor(dfgraphfinal$bars$IndicatorName, 
                                                  levels = sort(unique(dfgraphfinal$bars$IndicatorName), decreasing = TRUE))
        dfgraphfinal$points$IndicatorName <- factor(dfgraphfinal$points$IndicatorName, 
                                                  levels = sort(unique(dfgraphfinal$points$IndicatorName), decreasing = TRUE))        
        cols <- data.frame(category = c("Better","Same","Worse",
                                             "Not compared","None","Higher", 
                                             "Similar", "Lower",
                                             "Worst","Q25","Q75","Best"),
                                r = c(146, 255, 192, 166, 166, 190, 255, 85, 255, 201, 139, 201),
                                g = c(208, 192, 0, 166, 166, 210, 192, 85, 255, 201, 139, 201),
                                b = c(80, 0, 0, 166, 166, 255, 0, 230, 255, 201, 139, 201)) %>%
                mutate(hex = rgb(r,g,b, maxColorValue = 255))
        cols <- structure(cols$hex,
                               names = as.character(cols$category))
        dfgraphfinal$bars <- left_join(dfgraphfinal$bars, dfannotate, by = c("group" = "group", "GraphPoint" = "GraphPoint"))
        dfgraphfinal$points <- left_join(dfgraphfinal$points, dfannotatepoints, by = c("group" = "group"))
        p <- ggplot(dfgraphfinal$bars, 
                                   aes(x = IndicatorName, y = quantiles)) +
                geom_bar(stat = "identity", aes(fill = GraphPoint)) +
                geom_point(data = dfgraphfinal$points,
                           aes(x = IndicatorName, y = region), shape = 23, fill = "white") +
                geom_point(data = dfgraphfinal$points,
                           aes(x = IndicatorName, y = area, fill = significance), 
                           shape = 21, colour = "black") +
                geom_hline(yintercept = 0.5, col = "darkred") +
                coord_flip() +
                scale_fill_manual(values = cols) +
                theme_minimal() +
                theme() +
                labs(x = "", y = "")
        if (show.datatable == TRUE) {
                p <- p +
                        scale_y_continuous(position = "top",
                                           breaks = c(-.53, -.35, -.25, -.15, -0.05,1.05),
                                           labels = c("Time\nperiod","Area\nvalue","Regional\nvalue","England\nvalue","Worst/\nLowest","Best/\nHighest")) +
                        expand_limits(y = c(-0.63,1.1)) +
                        geom_text(aes(label = label, y = y), 
                                  col = "#333333",
                                  size = 2.5) + 
                        geom_text(data = dfgraphfinal$points,
                                  aes(label = England), 
                                  y = -0.15, col = "#333333",
                                  size = 2.5) + 
                        geom_text(data = dfgraphfinal$points,
                                  aes(label = regionalvalue), 
                                  y = -0.25, col = "#333333",
                                  size = 2.5) + 
                        geom_text(data = dfgraphfinal$points,
                                  aes(label = areavalue), 
                                  y = -0.35, col = "#333333",
                                  size = 2.5) + 
                        geom_text(data = dfgraphfinal$points,
                                  aes(label = Timeperiod), 
                                  y = -0.53, col = "#333333",
                                  size = 2.5) + 
                        theme(axis.text.x = element_text(size = rel(0.85)),
                              axis.text.y = element_text(size = rel(0.65),
                                                         hjust = 0),
                              panel.grid.major = element_blank(), 
                              panel.grid.minor = element_blank(),
                              legend.position = "none")
        } else {
                p <- p +
                        theme(#axis.text.x = element_blank(),
                              axis.text.y = element_blank(),
                              panel.grid.major = element_blank(), 
                              panel.grid.minor = element_blank(),
                              legend.position = "none")
        }
        spine_chart <- list(plot = p,
                            table = table_output)
}

data_table <- function(data, AreaCode) {
        preprocessed <- pre_process(data, AreaCode)
        data <- preprocessed$data
        ind_names <- preprocessed$ind_names
        parentcode <- preprocessed$parentcode
        areacode <- AreaCode
        data_table <- filter(data,
                             AreaCode == areacode) %>%
                select(group, IndicatorName, Timeperiod, Sex, Age, Count ,Value) %>%
                rename("Area value" = Value)
        best_worst <- group_by(data, group, IndicatorName, Polarity) %>%
                summarise(min = min(Value, na.rm = TRUE), 
                          max = max(Value, na.rm = TRUE)) %>%
                mutate(Best = ifelse(grepl("Low is good",Polarity), min, max),
                          Worst = ifelse(grepl("Low is good",Polarity), max, min)) %>%
                select(-(min:max))
        parent_table <- filter(data,
                               AreaCode == parentcode) %>%
                select(group, IndicatorName, Value) %>%
                rename("Parent value" = Value)
        england_table <- filter(data,
                                AreaCode == "E92000001") %>%
                select(group, IndicatorName, Value) %>%
                rename("England value" = Value)
        data_table <- left_join(data_table, best_worst, by = c("group" = "group", "IndicatorName" = "IndicatorName")) %>%
                left_join(parent_table, by = c("group" = "group", "IndicatorName" = "IndicatorName")) %>%
                left_join(england_table, by = c("group" = "group", "IndicatorName" = "IndicatorName")) %>%
                mutate(labels = paste0(abbreviate(IndicatorName, 25), " (",group,")"),
                       labels = factor(labels)) %>%
                arrange(labels) %>%
                select(IndicatorName, Age, Sex, Timeperiod, Count, `Area value`, `Parent value`, `England value`) %>%
                rename(Period = Timeperiod,
                       Indicator = IndicatorName)
}
        
pre_process <- function(data, AreaCode, ...) {
        if (missing(data)){
                data <- fingertips_data(..., rank = TRUE)
        }
        data$Polarity <- stringr::str_trim(data$Polarity)
        data <- data %>%
                group_by(IndicatorID, CategoryType, Sex, Age) %>%
                filter(TimeperiodSortable == max(TimeperiodSortable)) %>%
                { mutate(ungroup(.), group = group_indices(.)) } %>% #copied from GitHub
                ungroup()
        ind_names <- select(data, group, IndicatorName, Sex, Age) %>%
                unique() %>%
                #mutate(IndicatorName = abbreviate(IndicatorName, 25))
                mutate(IndicatorName = str_trunc(as.character(IndicatorName),100),
                       IndicatorName = case_when((Age != "Not applicable" & Sex != "Not applicable") ~ paste0(IndicatorName, " (",Sex," ",Age,")"),
                                                 (Age == "Not applicable" & Sex != "Not applicable") ~ paste0(IndicatorName, " (",Sex,")"),
                                                 (Age != "Not applicable" & Sex == "Not applicable") ~ paste0(IndicatorName, " (",Age,")"),
                                                 (Age == "Not applicable" & Sex == "Not applicable") ~ IndicatorName),
                       IndicatorName = str_wrap(IndicatorName,70)) %>%
                select(group, IndicatorName)
        parentcode <- unique(as.character(data[data$AreaCode == AreaCode, "ParentCode"]$ParentCode))
        pre_process <- list(data = data, 
                            ind_names = ind_names,
                            parentcode = parentcode)
}
