min <- 14.9
max <- 74.8
mean <- 37.9

polarity <- 1:2

## Sets polarity

set_best <- function(polarity = 1, max = 74.8, min = 14.9){
  
  if(polarity == 1){
    
    best = max
    worst = min
    
    
  } else {
    
    best = min
    worst = max
  }
  return(c(best, worst))
}

set_best(polarity = 2)

#################

### Sets scale values

spine_scale <- function(min = 14.9, max = 74.8, mean = 37.9, polarity = 1, q1 = 27.9, q3 = 47.2, colour = 1, value = 53.5){
  
  ## scale minimum
  if(mean - min > max - mean){
    
    scale_min = min
    
  } else {
    
    scale_min = mean - (max - mean)
    
  }
  

  ## scale maximum
  
  if(min) {
    
    scale_max = max
    
  } else {
    
    scale_max = mean + (mean - min)
    
  }
  
  #return(c(scale_min, scale_max ))
  
  if(polarity == 1){
    
    best = scale_max
    worst = scale_min
    
    
  } else {
    
    best = scale_min
    worst = scale_max
  }
  
## scale values
  
  scale_val <- (value - worst)/(best - worst)
  scale_average = 0.5
  
  
  ## scale worst
  
  if(polarity == 1){
    
    scale_worst  = (min - worst)/(best - worst)
    
    
  } else {
    
    
    scale_worst = (max - worst)/ (best - worst)
    
  }
  
  
  
  ## scale best
  
  if(polarity == 1){
    
    scale_best = (max - worst)/(best - worst)
  } else {
    
    
    scale_best = (min - worst)/(best - worst)
    
  }
    
  
  
  ## scale q1
  if(polarity == 1){
    
    scale_q1 = (q1 - worst)/(best - worst)
    
    
  } else {
    
    scale_q1 = (q3 - worst)/(best - worst)
    
  }
  
  
  ## q3
  
  if(polarity == 1){
    
   scale_q3 = (q3 - worst) / (best - worst) 
    
  } else {
    
    scale_q3 = (q1 - worst) / (best - worst)
  }
  
  
  ## scalepad
  
  if(polarity == 1){
    
    scale_pad = 0 
    
  } else {
    
    scale_pad = 1
    
  }
  
  if(polarity == 1){
    
    chart_bottom = scale_worst - scale_pad
      
  } else {
      
    chart_bottom = scale_pad - scale_best
    
  }
  
    if(polarity == 1){
      
    chart_q1 = scale_q1 - scale_worst 
      
    } else {
      
    chart_q1 = scale_best - scale_q3  
      
      
    }
    
  if(polarity == 1){
  
    chart_iqr = scale_q3 - scale_q1 
      
     } else {
    
    chart_iqr = scale_q3 - scale_q1   
           
     }
  
  
  if(polarity == 1){
    
    chart_top = scale_best - scale_q3
     } else {
       
    chart_top = scale_q1 - scale_worst   
     }

    
    
  

 return(data.frame(cbind(scale_val, scale_average,chart_bottom, chart_iqr, chart_q1, chart_top))) 
         
}



dat1 <- spine_scale(value = c(50),   polarity = 1)
dat2 <- spine_scale(value = c(50),   polarity = 2)
dat3 <- rep(dat1, 4)
dat <- bind_rows(dat1, dat2)
dat <- bind_rows(dat, dat)

dat %>%
  mutate(rows = row_number()) %>%
  gather(metric, value, chart_bottom:chart_top) %>%
  ggplot(aes(rows, value, fill = metric)) +
  geom_col(aes(position = "fill"))


exp2 <- dat %>%
  mutate(rows = row_number()) 

%>%
  gather(metric, value, scale_worst:scale_pad)




require(tidyverse)

cols <- c("scale_pad" = "white", "scale_q1" = "darkgrey", "scale_q3" = "darkgrey",
          "scale_worst" = "lightgrey", "scale_best" = "lightgrey")

exp2 %>%
  filter(rows == 2) %>%
  ggplot() +
  geom_col(aes(row, scale_best))
  geom_point(aes(rows, scale_val)) +
  geom_hline(yintercept = exp1$scale_average) +
  coord_flip() +
  scale_fill_manual(values = cols)
