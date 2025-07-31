simulate_IndicatorData <- function(indData,
                                   years = NULL,
                                   nsim = 1000){
  
  ## Determine years to consider for sampling
  if(is.null(years)){
    years <- names(indData$indicatorValues)
    years <- years[which(years != 1950)]
  }
  
  
  ## Sample reference values
  
  # Set observation type (required by NIcalc::sampleObsMat)
  obstype <- indData$referenceValues$distributionFamilyName
  obstype[!is.na(obstype)] <- "tradObs"
  obstype[is.na(obstype)]  <- "customObs"
  
  # Sample from distributions
  message("Sampling from reference value distribution.")
  sampleMat_ref <- NIcalc::sampleObsMat(
    ICunitId = indData$referenceValues$ICunitId, 
    value = indData$referenceValues$expectedValue,
    distrib = indData$referenceValues$distributionFamilyName,
    mu = indData$referenceValues$distParameter1,
    sig = indData$referenceValues$distParameter2,
    customDistribution = indData$referenceValues$customDistribution,
    obsType = obstype,
    nsim = nsim
  )  
  
  # Add spatial unit and year
  sampleData_ref <- as.data.frame(sampleMat_ref) %>%
    tibble::add_column(.before=1,
                       ICunitID = row.names(sampleMat_ref)) %>%
    tibble::add_column(.after = 1,
                       year = NA) 
  
  
  ## Sample yearly indicator values
  
  # Set up dataframe for storing samples
  sampleData_comb <- data.frame()
  
  message("Sampling from yearly indicator value distributions.")
  
  for(t in 1:length(years)){

    # Select data subset
    subset.idx <- which(names(indData$indicatorValues) == years[t])
    indValues_t <- indData$indicatorValues[[subset.idx]]
    
    # Set observation type (required by NIcalc::sampleObsMat)
    obstype <- NULL
    obstype <- indValues_t$distributionFamilyName
    obstype[!is.na(obstype)] <- "tradObs"
    obstype[is.na(obstype)]  <- "customObs"
    
    # Check whether any values are available for the selected year
    allNA <- ifelse(all(is.na(indValues_t$expectedValue)), TRUE, FALSE)
    
    # Sample from distributions (if values are available)
    if(!allNA){
      message(paste0("Year ", years[t]))
      sampleMat_t <- NIcalc::sampleObsMat(
        ICunitId = indValues_t$ICunitId, 
        value = indValues_t$expectedValue,
        distrib = indValues_t$distributionFamilyName,
        mu = indValues_t$distParameter1,
        sig = indValues_t$distParameter2,
        customDistribution = indValues_t$customDistribution,
        obsType = obstype,
        nsim = nsim
      )
    }else{
      message(paste0("Year ", years[t], " (skipped due to all NA)"))
      next
    }
    
    sampleData_t <- as.data.frame(sampleMat_t) %>%
      tibble::add_column(.before=1,
                         ICunitID = row.names(sampleMat_t)) %>%
      tibble::add_column(.after = 1,
                         year = years[t]) 
    
    sampleData_comb <- rbind(sampleData_comb, sampleData_t)
  }
  
  
  ## Combine reference and sample data
  comb <- rbind(sampleData_ref, sampleData_comb)
  
  
  ## Add spatial unit names
  areas <- data.frame(ICunitName = unname(rownames(indData$ICunits)),
                      ICunitID = as.character(unname(indData$ICunits)))
  comb <- comb %>%
    dplyr::left_join(areas, by = "ICunitID") %>%
    dplyr::relocate(ICunitName, .after = 1)
  
 
  ## Reformat sampled data
  
  # Separate reference from indicator value data
  comb_ref <- comb %>%
    dplyr::filter(is.na(year))
  comb_values <- comb %>%
    dplyr::filter(!is.na(year))
  
  # Convert to longitudinal format
  comb_values_long <- comb_values %>%
    tidyr::pivot_longer(cols = starts_with("V")) %>%
    dplyr::mutate(sampleID = as.numeric(stringr::str_remove(name, "V"))) %>%
    dplyr::select(-name)
  
  comb_ref_long <- comb_ref %>%
    tidyr::pivot_longer(cols = starts_with("V")) %>%
    dplyr::mutate(sampleID = as.numeric(stringr::str_remove(name, "V"))) %>%
    dplyr::select(-name, -year) %>%
    dplyr::rename(refValue_sampled = value)
  
  
  ## Combine yearly indicator values and reference values
  
  # Set up data frame with fixed reference values (means as entered into the database)
  fixed_ref_long <- indData$referenceValues %>%
    dplyr::select(ICunitId, expectedValue) %>%
    dplyr::mutate(ICunitId = as.character(ICunitId)) %>%
    dplyr::rename(refValue_fixed = expectedValue,
                  ICunitID = ICunitId)
  
  # Merge reference values into indicator table and drop NA values
  simData_raw <- comb_values_long %>%
    dplyr::left_join(comb_ref_long, by = c("ICunitID", "ICunitName", "sampleID"))%>%
    dplyr::left_join(fixed_ref_long, by = "ICunitID") %>%
    dplyr::filter(!is.na(value))
  
  
  ## Make datasets of scaled indicator values
  
  # Identify scaling model
  scalingModel <- indData$indicators$scalingModel
  
  # Scale indicator values using fixed reference
  simData_scaled_fixedRef <- simData_raw %>%
    dplyr::mutate(value_sc = scale_IndicatorValues(value = value,
                                                   reference = refValue_fixed,
                                                   scalingModel = scalingModel),
                  value_sc_trunc = ifelse(value_sc > 1, 1, value_sc)) %>%
    dplyr::select(ICunitID, ICunitName, year, value_sc, value_sc_trunc)
    
  # Scale indicator values using sampled reference (including reference uncertainty)
  simData_scaled_sampledRef <- simData_raw %>%
    dplyr::mutate(value_sc = scale_IndicatorValues(value = value,
                                                   reference = refValue_sampled,
                                                   scalingModel = scalingModel),
                  value_sc_trunc = ifelse(value_sc > 1, 1, value_sc)) %>%
    dplyr::select(ICunitID, ICunitName, year, value_sc, value_sc_trunc)
  
  
  ## Collate output data in a list and return
  simData <- list(raw = simData_raw,
                  scaled_fixedRef = simData_scaled_fixedRef,
                  scaled_sampledRef = simData_scaled_sampledRef)
  
  return(simData)
}
