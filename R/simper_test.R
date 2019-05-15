simper_test <- function(data, metadata){
  simper_test_data <- vegan::simper(data, metadata$site)
  simper_test_data <-summary(simper_test_data)
  comparisons <- names(simper_test_data)
}


# editing area ------------------------------------------------------------

#return average between-group dissimilarity. This is the sum of the item average
sim_test$D1_D2$overall
#need to figure out how to iterate over comparisons and return overall
