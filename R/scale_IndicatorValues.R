scale_IndicatorValues <- function(value, reference, scalingModel, truncateVals = FALSE){
  
  # Check for valid scaling model
  if(!(scalingModel %in% c("Low", "Max"))){
    stop("Invalid scaling model. Has to be either Low or Max.")
  }
  
  # Divide value by reference
  value_scaled <- value / reference
  
  # Adjust scale if MAX model is used
  if(scalingModel == "Max"){
    value_scaled <- 2 - value_scaled
    value_scaled <- ifelse(value_scaled < 0, 0, value_scaled)
  }
  
  # Truncate if desired
  if(truncateVals & any(value_scaled > 1)){
    value_scaled[which(value_scaled > 1)] <- 1
  }
  
  # Return scaled value
  return(value_scaled)
}