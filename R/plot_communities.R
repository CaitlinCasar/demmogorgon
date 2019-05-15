plot_communities <- function(data, color_dict=NULL,metadata, NMDS=FALSE, barplot=FALSE, dendrogram=FALSE, dendro_bar=FALSE, interactive=FALSE, x=5, phylum=FALSE, legend=FALSE){
  `%>%` <- magrittr::`%>%`
  dendrogram_data <- as.dendrogram(hclust(ecodist::bcdist(data)))
  dendro.plot <- ggplot2::ggplot(dendrogram_data, horiz = T) + ggplot2::theme_gray()
  
  filtered_data <-  data %>%
    dplyr::select_if(function(col) max(col) > x)
  filtered_data <- cbind(filtered_data, Less.Abundant.Taxa = 100-rowSums(filtered_data))
  filtered_data <- tidyr::gather(cbind(metadata,filtered_data), Taxa, Abundance, colnames(filtered_data[1]):colnames(filtered_data[ncol(filtered_data)]), factor_key=TRUE)
  filtered_data[,1] <- factor(filtered_data[,1], levels = labels(dendrogram_data))
  
  n <- length(unique(colnames(data)))
  palette <- randomcoloR::distinctColorPalette(n+1)
  names(palette) <- c(unique(colnames(data)), "Less.Abundant.Taxa")
  
  phylum_data <- tidyr::gather(cbind(metadata,data), Taxa, Abundance, colnames(data[1]):colnames(data[ncol(data)]), factor_key=TRUE)
  phylum_data$Phylum <- gsub( "[.].*$", "", phylum_data$Taxa)
  phylum.data <- phylum_data %>% group_by_(.dots = list(colnames(phylum_data)[1], colnames(phylum_data[ncol(phylum_data)]))) %>% summarize(Abundance=sum(Abundance))
  ###next line not working for some reason 
  #phylum.data[,1] <- factor(phylum.data[,1], levels = labels(dendrogram_data))
  
  if(phylum==TRUE){
    bar_plot <- ggplot2::ggplot(phylum.data, aes_string(fill="Phylum", y="Abundance", x=names(phylum.data)[1])) + 
      ggplot2::theme_gray() +
      ggplot2::geom_bar(stat='identity', position='fill') +
      ggplot2::coord_flip()
  }else{
    bar_plot <- ggplot2::ggplot(filtered_data, aes_string(fill="Taxa", y="Abundance", x=names(filtered_data)[1])) + 
      ggplot2::theme_gray() +
      ggplot2::geom_bar(stat='identity', position='fill') +
      ggplot2::coord_flip()
  }
  if(length(color_dict) > 0){
    bar_plot<- bar_plot + ggplot2::scale_fill_manual(values=color_dict)
  }else{
    if(phylum==TRUE){
      n <- length(unique(phylum.data$Phylum))
      palette <- randomcoloR::distinctColorPalette(n+1)
      names(palette) <- c(unique(phylum.data$Phylum), "Less.Abundant.Taxa")
      bar_plot<- bar_plot + ggplot2::scale_fill_manual(values=palette)
    }else{
      bar_plot<- bar_plot + ggplot2::scale_fill_manual(values=palette)
    }
    bar_plot<- bar_plot + ggplot2::scale_fill_manual(values=palette)
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
    dendro.bar.plot <- cowplot::plot_grid(dendro.plot,bar_plot, align = "h")
  }else if(NMDS==TRUE){
    NMDS <- metaMDS(data,k=2)
    
    #save results in data.frame
    NMDS.frame = data.frame(MDS1 = NMDS$points[,1], MDS2 = NMDS$points[,2])
    
    #combine NMDS coordinates with metadata
    merged_NMDS <- cbind(metadata, NMDS.frame)
    
    #fit vectors to family
    taxa_vectors <-envfit(NMDS$points, data, perm=1000)
    taxa_vectors_df<-as.data.frame(taxa_vectors$vectors$arrows*sqrt(taxa_vectors$vectors$r))
    taxa_vectors_df$pval <- taxa_vectors$vectors$pvals
    
    #sort vectors by smallest p value
    sig_vectors <- subset(taxa_vectors_df, pval <= 0.05, select=c(MDS1, MDS2, pval))
    sig_vectors <- sig_vectors[order(sig_vectors$pval),]
    sig_vectors$family<-rownames(sig_vectors)
    
    #add phylum column
    sig_vectors$phylum <- gsub( "[.].*$", "", sig_vectors$family)
    
    #filter sig_vectors for only taxa with lowest pvals
    sig_vectors <- sig_vectors %>% filter(pval <= min(sig_vectors$pval))
    
    #Now, plot them like a badass
    ggplot(merged_NMDS, aes(x=MDS2, y=MDS1)) +
      geom_point(size=2, alpha=0.8) +
      geom_text(aes(label=paste0(site, ".", date),hjust = 1, vjust = 1),  size=2, color="black") +
      geom_segment(data=sig_vectors,inherit.aes = FALSE, aes(x=0,xend=MDS2,y=0,yend=MDS1, color=phylum, label=family), alpha=0.3)+
      coord_fixed(ratio = 1, xlim = NULL, ylim = NULL, expand = TRUE) +
      stat_ellipse(data=merged_NMDS, aes(color=site)) +
      scale_color_manual(values=color_dict) +
      theme_grey()
  }else
    print("Please choose a plot type.")
}


#convert family.data to long format   

#summarize phylum abundance 



#plot dendrogram and bar plot side by side 


