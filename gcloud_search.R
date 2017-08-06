spine_scale <- function(min = 14.9, max = 74.8, polarity = 1, mean = 37.9){
  
  min <- min(min)
  max <- max(max)
  polarity <- c(1, 2)
  mean <- mean(mean)
  
  ## scale minimum
  if(mean - min > max - mean){
    
    scale_min = min
    
  } else {
    
    scale_min = mean - (max - mean)
    
  }
  
  return(scale_min)
  
 ## scale maximum
  
  if(min) {
    
    scale_max = max
    
  } else {
    
    scale_max = mean + (mean - min)
    
  }
  
  return(cbind(scale_max, scale_min )
  
  
}




spine_chart_data(mean = 22)
