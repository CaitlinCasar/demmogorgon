simper_test <- function(data, metadata, comparison_index=2, num_perm=1, p_threshold=1, sim_plot=TRUE, color_dict=NULL, phylum=FALSE){
  `%>%` <- magrittr::`%>%`
  simper_test_data <- vegan::simper(data, metadata[,comparison_index], permutations = num_perm)
  test <- as.data.frame(do.call(cbind, purrr::flatten(simper_test_data)))
  test <- sapply(unique(names(test)), function(x) unname(unlist(test[,names(test)==x])))
  test <- cbind(comparisons=names(rep(simper_test_data, each=ncol(data))), test)
  test <- as.data.frame(test, stringsAsFactors = FALSE) %>% dplyr::filter(p<=p_threshold)
  test$phylum <- gsub( "[.].*$", "", test$species)
  phylum_data <- test %>%
    dplyr::group_by_(.dots = list(colnames(test)[1], colnames(test[ncol(test)]))) %>%
    dplyr::summarize(avg_dissim=sum(as.numeric(average)))

  n <- length(unique(phylum_data$phylum))
  phylum_palette <- randomcoloR::distinctColorPalette(n)
  names(phylum_palette) <- c(unique(phylum_data$phylum))

if(sim_plot==TRUE){
  simper_plot <- ggplot2::ggplot(phylum_data , ggplot2::aes(fill=phylum, y=avg_dissim, x=comparisons)) +
    ggplot2::theme_gray() +
    ggplot2::geom_bar(stat='identity') +
    ggplot2::coord_flip()
  if(length(color_dict) > 0){
    simper_plot + ggplot2::scale_fill_manual(values=color_dict)
  }else{
    simper_plot + ggplot2::scale_fill_manual(values=phylum_palette)
  }
}else{
  if(phylum==TRUE){
    phylum_data
  }else{
    test
  }
}

}
