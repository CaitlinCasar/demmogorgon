simper_test <- function(data, metadata){
  simper_test_data <- vegan::simper(data, metadata$site)
  simper_test_data <-summary(simper_test_data)
  comparisons <- names(simper_test_data)
}


# editing area ------------------------------------------------------------

#need to add this to the import metadata function 
#remove sample ids from metadata not in data 
test <- metadata$Ted.s.names[(!metadata$Ted.s.names %in% rownames(data))]
test <- which(unlist(metadata) %in% test)
test <- metadata[-test,]

#return average between-group dissimilarity. This is the sum of the item average
sim_test$D1_D2$overall
#need to figure out how to iterate over comparisons and return overall 