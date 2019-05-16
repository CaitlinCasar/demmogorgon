simper_test <- function(data, metadata, comparison_index=2, num_perm=1, p_threshold=1){
  simper_test_data <- vegan::simper(data, metadata[,comparison_index], permutations = num_perm)
  test <- as.data.frame(do.call(cbind, purrr::flatten(simper_test_data)))
  test <- sapply(unique(names(test)), function(x) unname(unlist(test[,names(test)==x])))
  test <- cbind(comparisons=names(rep(simper_test_data, each=ncol(data))), test)
  as.data.frame(test, stringsAsFactors = FALSE) %>% dplyr::filter(p<=p_threshold)
}
