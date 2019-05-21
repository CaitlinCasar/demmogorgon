plot_communities <- function(data, color_dict=NULL,metadata, NMDS=FALSE, barplot=FALSE, dendrogram=FALSE, dendro_bar=FALSE, interactive=FALSE, threshold_abundance=5, phylum=FALSE, legend=FALSE, perm_val=100){
  `%>%` <- magrittr::`%>%`
  dendrogram_data <- as.dendrogram(hclust(ecodist::bcdist(data)))
  dendro.plot <- ggplot2::ggplot(dendrogram_data, horiz = T) + ggplot2::theme_gray()

  ######need to fix merging########

  filtered_data <-  data %>%
    dplyr::select_if(function(col) max(col) > threshold_abundance)
  filtered_data <- cbind(filtered_data, Less.Abundant.Taxa = 100-rowSums(filtered_data))
  filtered_data <- merge(metadata, filtered_data, by.x = colnames(metadata[1]), by.y = "row.names")

  filtered_data <- tidyr::gather(filtered_data, Taxa, Abundance, colnames(filtered_data[ncol(metadata)+1]):colnames(filtered_data[ncol(filtered_data)]), factor_key=TRUE)
  filtered_data[,1] <- factor(filtered_data[,1], levels = labels(dendrogram_data))

  n <- length(unique(colnames(data)))
  palette <- randomcoloR::distinctColorPalette(n+1)
  names(palette) <- c(unique(colnames(data)), "Less.Abundant.Taxa")

  ######need to fix merging########
  merged_data <- merge(metadata, data, by.x = colnames(metadata[1]), by.y = "row.names")

  phylum_data <- tidyr::gather(merged_data, Taxa, Abundance, colnames(merged_data[ncol(metadata)+1]):colnames(merged_data[ncol(merged_data)]), factor_key=TRUE)
  phylum_data$Phylum <- gsub( "[.].*$", "", phylum_data$Taxa)
  phylum.data <- phylum_data %>% dplyr::group_by_(.dots = list(colnames(phylum_data)[1], colnames(phylum_data[ncol(phylum_data)]))) %>% dplyr::summarize(Abundance=sum(Abundance))

  n <- length(unique(phylum.data$Phylum))
  phylum_palette <- randomcoloR::distinctColorPalette(n)
  names(phylum_palette) <- c(unique(phylum.data$Phylum))

  ###next line not working for some reason
  #phylum.data[,1] <- factor(phylum.data[,1], levels = labels(dendrogram_data))

  if(phylum==TRUE){
    bar_plot <- ggplot2::ggplot(phylum.data, ggplot2::aes_string(fill="Phylum", y="Abundance", x=names(phylum.data)[1])) +
      ggplot2::theme_gray() +
      ggplot2::geom_bar(stat='identity', position='fill') +
      ggplot2::coord_flip()
  }else{
    bar_plot <- ggplot2::ggplot(filtered_data, ggplot2::aes_string(fill="Taxa", y="Abundance", x=names(filtered_data)[1])) +
      ggplot2::theme_gray() +
      ggplot2::geom_bar(stat='identity', position='fill') +
      ggplot2::coord_flip()
  }

  if(length(color_dict) > 0){
    bar_plot<- bar_plot + ggplot2::scale_fill_manual(values=color_dict)
  }else{
    if(phylum==TRUE){
      bar_plot<- bar_plot + ggplot2::scale_fill_manual(values=phylum_palette)
    }else{
      bar_plot<- bar_plot + ggplot2::scale_fill_manual(values=palette)
    }
  }
  if(legend==TRUE){
    bar_plot <- bar_plot
  }else{
    bar_plot <- bar_plot + ggplot2::theme(legend.position = "none")
  }
  if(interactive==TRUE){
    bar_plot <- plotly::ggplotly(bar_plot)
  }else{
    bar_plot <- bar_plot
  }
  if(dendrogram==TRUE){
    dendro.plot
  }else if(barplot==TRUE){
    bar_plot
  }else if(dendro_bar==TRUE){
    cowplot::plot_grid(dendro.plot,bar_plot, align = "h")
  }else if(NMDS==TRUE){
    NMDS <- vegan::metaMDS(merged_data[(ncol(metadata)+1):ncol(merged_data)],k=2)

    #save results in data.frame
    NMDS.frame = data.frame(MDS1 = NMDS$points[,1], MDS2 = NMDS$points[,2])

    #combine NMDS coordinates with metadata
    merged_NMDS <- cbind(merged_data[1:ncol(metadata)], NMDS.frame)

    #fit vectors to family
    taxa_vectors <-vegan::envfit(NMDS$points, merged_data[,(ncol(metadata)+1):ncol(merged_data)], perm=perm_val)
    taxa_vectors_df<-as.data.frame(taxa_vectors$vectors$arrows*sqrt(taxa_vectors$vectors$r))
    taxa_vectors_df$pval <- taxa_vectors$vectors$pvals

    #sort vectors by smallest p value
    sig_vectors <- subset(taxa_vectors_df, pval <= 0.05, select=c(MDS1, MDS2, pval))
    sig_vectors$taxa<-rownames(sig_vectors)

    #add phylum column
    sig_vectors$phylum <- gsub( "[.].*$", "", sig_vectors$taxa)

    #filter sig_vectors for only taxa with lowest pvals
    sig_vectors <- sig_vectors %>% filter(pval <= min(sig_vectors$pval))

    #Now, plot them like a badass
    NMDS_plot <- ggplot2::ggplot(merged_NMDS, ggplot2::aes(x=MDS2, y=MDS1)) +
      ggplot2::geom_point(size=2, alpha=0.8) +
      ggplot2::geom_text(ggplot2::aes(label=paste0(site, ".", date),hjust = 1, vjust = 1),  size=2, color="black") +
      ggplot2::geom_segment(data=sig_vectors,inherit.aes = FALSE, ggplot2::aes(x=0,xend=MDS2,y=0,yend=MDS1, color=phylum), alpha=0.3)+
      ggplot2::coord_fixed(ratio = 1, xlim = NULL, ylim = NULL, expand = TRUE) +
      ggplot2::stat_ellipse(data=merged_NMDS, ggplot2::aes(color=site)) +
      ggplot2::theme_grey()
    if(length(color_dict) > 0){
      NMDS_plot  + ggplot2::scale_color_manual(values=color_dict)
    }else{
      if(phylum==TRUE){
        NMDS_plot  + ggplot2::scale_color_manual(values=phylum_palette)
      }else{
        NMDS_plot
      }
    }
  }else
    print("Please choose a plot type.")
}



# editing section ---------------------------------------------------------


###this works###
test = as.matrix(vegdist(data, method = "bray"))

)

# Bootstrapping with pvclust
test = pvclust(test, method.hclust = "average", nboot = 1000)



dend <- as.dendrogram(test)
test %>% as.dendrogram %>%
  plot(main = "Cluster dendrogram with AU/BP values (%)\n reproduced plot with dendrogram")
test %>% text


dend %>% pvclust_show_signif(test, show_type = "lwd") %>%
  plot(main = "Cluster dendrogram with AU/BP values (%)\n bp values are highlighted by signif")
test %>% text



ggplot2::ggplot(dend %>% pvclust_show_signif(test, show_type = "lwd") ) +
  geom_text(inherit.aes = FALSE, test_coords, aes(x=V1, y=height, label=au))

ggplot2::ggplot(test_dend, horiz = T) +
  geom_text(inherit.aes = FALSE, test_coords, aes(x=V1, y=height, label=au))
