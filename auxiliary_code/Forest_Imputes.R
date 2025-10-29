library(ggplot2)
library(dplyr)

## Read in NI object
skog <- readRDS("data/NI2025_Data_Forest.rds")


## Extract and summarize imputed values
imputeSum <- t(apply(skog$NAImputes$imputations, 1, function(x) quantile(x, probs = c(0.025, 0.5, 0.975))))
imputeSum <-  cbind(skog$NAImputes$identifiers, imputeSum)   

readr::write_excel_csv(imputeSum, "Forest_Imputes_Summary.csv")

## Collate imputed values
imputeIdentifiers <- skog$NAImputes$identifiers %>%
  dplyr::mutate(id = 1:nrow(skog$NAImputes$identifiers))

imputeData <- reshape2::melt(skog$NAImputes$imputations) %>%
  as.data.frame() %>%
  dplyr::rename(id = Var1,
                sample = Var2) %>%
  dplyr::left_join(imputeIdentifiers, by = "id")

readr::write_excel_csv(imputeData, "Forest_Imputes_Data.csv")

## Plot histograms of imputations (per indicator)
indNames <- unique(imputeData$indName)

pdf("Forest_Imputes.pdf", height = 7, width = 9)   
for(i in 1:length(indNames)){

  data_sub <- imputeData %>%
    dplyr::filter(indName == indNames[i]) %>%
    dplyr::mutate(impId = paste0("ICunitID ", ICunitId, ", year ", year))
  
  max_cutoff <- quantile(data_sub$value, probs = 0.975)
  
  print(
    ggplot(subset(data_sub, value < max_cutoff), aes(x = value)) + 
      geom_histogram(bins = 100, fill = "cornflowerblue", color = NA) + 
      facet_wrap(~ impId, scales = "free") + 
      ggtitle(indNames[i]) + 
      theme_classic()
  )
  
}
dev.off()
