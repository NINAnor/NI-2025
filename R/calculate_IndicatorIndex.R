#' Calculate area-aggregated index for a single indicator
#'
#' @param dataSet a list containing aggregated indicator data for a single indicator. Output of `NIcalc::assembleNiObject()`.
#' @param testRun logical. If TRUE, does a test run with 10 simulations. If FALSE( (default), does a full run with 1000 iterations.
#' @param NAimputation logical. Whether (TRUE) or not (FALSE) missing values should be imputed.
#' @param weigh_byEcosystem logical. If TRUE, calculated area-aggregated index only for the specified ecosystem. 
#' If FALSE (default), uses the sum of ecosystems for which the indicator has fidelity > 0 as the basis for area weights.
#' @param ecosystem character string specifying which ecosystem to aggregate for.
#' Has to be provided if `weigh_byEcosystem = TRUE` and must be one of the following: "Skog", "Fjell", "Våtmark", "Åpent lavland", "Ferskvann", "Kystvann", "Hav".
#' @param include_subAreas logical. If TRUE (default) calculates indicator index for all of Norway and for represented sub-regions. If FALSE, only calculates for all of Norway.
#' @param truncAtRef logical. Whether indicator values should be truncated at the reference level before aggregation.
#'
#' @returns a list containing indicator name, years with data used in calculations, ecosystem aggregated for, and the calculated indicator index both as a summary table and complete output from `NIcalc::calculateIndex()`.
#' @export
#'
#' @examples
#' 
calculate_IndicatorIndex <- function(dataSet,
                                     testRun = FALSE,
                                     NAimputation,
                                     weigh_byEcosystem = FALSE,
                                     ecosystem = NULL,
                                     include_subAreas = TRUE,
                                     truncAtRef){
  
  all_ES <- c("Skog", "Fjell", "Våtmark", "Åpent lavland", 
              "Ferskvann", "Kystvann-bunn", "Kystvann-pelagisk", "Havbunn", "Hav-pelagisk")
  
  if(weigh_byEcosystem){
    
    if(is.null(ecosystem)){
      stop("Argument ecosystem is missing. This has to be provided when weigh_byEcosystem = TRUE.")
    }
    
    if(!(ecosystem %in% all_ES)){
      stop("Invalid ecosystem provided. Has to be one of: Skog, Fjell, Våtmark, Åpent lavland, Ferskvann, Kystvann, Hav.")
    }
  }
  
  
  #-------------#
  # DATA SET-UP #
  #-------------#
  
  # Get indicator info
  indicator <- dataSet$indicators
  indicatorName <- dataSet$indicators$name
  
  # Extract IC units (and drop NAs if present)
  ICunits <- dataSet$ICunits
  
  if(any(dim(ICunits) > 1)){
    ICunits <- ICunits[!is.na(ICunits),]
    ICunits <- as.matrix(ICunits, ncol = 1)
    colnames(ICunits) <- indicatorName
  }
  
  # Extract reference values
  refValues <- dataSet$referenceValues %>%
    dplyr::filter(ICunitId %in% ICunits)
  
  
  #--------------------#
  # SELECTION OF YEARS #
  #--------------------#
  
  # Find first non-NA year
  not_NA <- rep(FALSE, length(dataSet$indicatorValues))
  
  for(t in 1:length(dataSet$indicatorValues)){
    
    if(all(is.na(dataSet$indicatorValues[[t]]$expectedValue) | dataSet$indicatorValues[[t]]$expectedValue < 0)){
      next
    }else{
      not_NA[t] <- TRUE
    }
  }
  
  firstYear_Id <- min(which(not_NA))
  firstYear <- names(dataSet$indicatorValues)[firstYear_Id]
  
  # Drop years prior to first year with data
  dataSet_focal <- dataSet
  dataSet_focal$indicatorValues <- dataSet$indicatorValues[firstYear_Id:length(dataSet$indicatorValues)]
  
  # Find and drop later years that contain no data
  # (These contribute "nothing" as contain no data to help with imputation)
  no_contrib <- !not_NA
  no_contrib <- no_contrib[firstYear_Id:length(dataSet$indicatorValues)]
  
  dataSet_focal$indicatorValues <- dataSet_focal$indicatorValues[which(!no_contrib)]
  
  dataYears <- names(dataSet_focal$indicatorValues)
  
  message("Calculations based on data for the following years:")
  message(paste(dataYears, collapse = ", "))
  
  # Assemble observations and count NAs
  indObs <- NULL
  nas <- NULL
  for (yrs in names(dataSet_focal$indicatorValues)) {
    indObs[[yrs]] <- dataSet_focal$indicatorValues[[yrs]][dataSet_focal$indicatorValues[[yrs]]$ICunitId %in% ICunits,]
    nas[yrs] <- sum(is.na(indObs[[yrs]]$expectedValue))
  }
  
  xx <- names(indObs[which(nas != length(unique(ICunits)))])
  indObs <- indObs[xx]
  nas <- nas[xx]
  
  
  #---------------------#
  # SPATIAL UNIT SET-UP #
  #---------------------#
  
  # Extract BS units
  BSunits <- dataSet_focal$BSunits %>%
    dplyr::filter(name %in% rownames(ICunits))
  
  # Extract NI units
  NIunits <- data.frame(dataSet_focal$NIunits)[dataSet_focal$BSunits$name %in% rownames(ICunits), ]
  
  NIunits[is.na(NIunits)] <- 0
  ta.med <- which(dimnames(NIunits)[[1]] %in% BSunits$name)
  NIunits <- NIunits[ta.med, ]
  ta.med <- which(colSums(NIunits) > 0)
  NIunits <- NIunits[, ta.med]
  
  NIunits <- NIunits[BSunits$name, ]
  
  # Optional: drop sub-area NI units to calculate only for whole of Norway/all areas
  if(!include_subAreas){
    NIunits <- NIunits[, 1, drop = FALSE]
  }
  
  # Adjust ICunits
  ICunits <- ICunits[BSunits$name, ]
  ICunits <- as.matrix(ICunits,ncol = 1)
  colnames(ICunits) <- indicatorName
  
  if(length(rownames(ICunits)) == 0){
    rownames(ICunits) <- BSunits$name
  }  
  
  # Prepare input data
  indicatorInput <- NIcalc::niInput(indicators = indicator,
                                    ICunits = ICunits,
                                    BSunits = BSunits,
                                    referenceValues = refValues,
                                    indicatorValues = indObs,
                                    NIunits = NIunits)
  
  #---------------#
  # NA IMPUTATION #
  #---------------#
  
  # If selected: impute missing values
  if (sum(nas) > 0 & NAimputation) {
    indicatorImputes <- NIcalc::imputeData(x = indicatorInput,
                                   nSim = ifelse(testRun, 10, 1000),
                                   transConst = 0.01,
                                   maxit = 15,
                                   printFlag = TRUE)
  } else {
    indicatorImputes <- NULL
  }
  
  
  #-------------------#
  # INDEX CALCULATION #
  #-------------------#
  
  # Set ecosystem/column to use for area weighing
  if(weigh_byEcosystem){
    awBSunit <- ecosystem
  }else{
    ES_cols <- which(colnames(indicatorInput$BSunits) %in% all_ES)
    
    if(length(ES_cols) > 1){
      indicatorInput$BSunits$sumEco <- rowSums(indicatorInput$BSunits[, ES_cols])
    }else{
      indicatorInput$BSunits$sumEco <- indicatorInput$BSunits[, ES_cols]
    }
    
    awBSunit <- "sumEco"
  }
  
  # Run index calculation
  indicatorIndex <- NIcalc::calculateIndex(x = indicatorInput,
                                           imputations = indicatorImputes,
                                           nsim = ifelse(testRun, 10, 1000),
                                           fids = FALSE,
                                           tgroups = FALSE,
                                           keys = "ignore",
                                           w = 0,
                                           awbs = TRUE,
                                           awBSunit = awBSunit,
                                           truncAtRef = truncAtRef)
  
  NIareas <- names(indicatorIndex)
  years <- names(indicatorIndex[[1]])
  
  # Summarise information on imputations
  nImputations <- matrix(0, nrow = length(years), ncol = length(NIareas))
  row.names(nImputations) <- years
  colnames(nImputations) <- NIareas
  
  if (sum(nas) > 0 & NAimputation) {
    for (j in NIareas) {
      for (k in years) {
        denom <- length(indicatorIndex[[j]][[k]]$ICunits)
        num <- sum((indicatorImputes$identifiers$ICunitId %in% indicatorIndex[[j]][[k]]$ICunits) &
                     (as.character(indicatorImputes$identifiers$year) == k))
        nImputations[k,j] <- num/denom
      }
    }
  }
  
  # Summarise results
  result <- summary(indicatorIndex)
  
  for (j in NIareas) {
    xx <- dimnames(result[[j]])[[2]]
    result[[j]] <- cbind(result[[j]], nImputations[, j])
    dimnames(result[[j]]) <- list(years, c(xx, "Imputations"))
  }
  
  # Reformat results
  result_sum <- data.frame()
  for(j in 1:length(result)){
    sum_j <- result[[j]] %>%
      as.data.frame(row.names = FALSE) %>%
      dplyr::mutate(indexArea = names(result)[j],
                    year_t = rownames(result[[j]]),
                    .before = 1) %>%
      dplyr::rename(q025 = `2.5%`,
                    q975 = `97.5%`)
    
    result_sum <- rbind(result_sum, sum_j)
  }

  # Drop NAs (coded as 0)
  result_sum <- result_sum %>%
    dplyr::mutate(flagNA = ifelse(median == 0 & q025 == 0 & q975 == 0 & displacement == 0, TRUE, FALSE)) %>%
    dplyr::filter(!flagNA) %>%
    dplyr::select(-flagNA)
  
  # Collate and return results
  out.list <- list(
    indicator = indicatorName,
    dataYears = dataYears,
    ecosystem = ifelse(weigh_byEcosystem, ecosystem, "sum"),
    summary = result_sum,
    indexOutput = indicatorIndex
  )
  
  return(out.list)
}



