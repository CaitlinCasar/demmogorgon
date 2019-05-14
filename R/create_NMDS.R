NMDS_fun <- function(z){
  NMDS <- metaMDS(z[,4:ncol(data)],k=2)
  
  #save results in data.frame
  NMDS.frame = data.frame(MDS1 = NMDS$points[,1], MDS2 = NMDS$points[,2])
  
  #combine NMDS coordinates with metadata
  merged_NMDS <- cbind(data[,1:3], NMDS.frame)
  
  #fit vectors to family
  family.vectors <-envfit(NMDS$points, data[,4:ncol(data)], perm=1000)
  family.vectors.df<-as.data.frame(family.vectors$vectors$arrows*sqrt(family.vectors$vectors$r))
  family.vectors.df$pval <- family.vectors$vectors$pvals
  
  #sort vectors by smallest p value
  sig_vectors <- subset(family.vectors.df, pval <= 0.05, select=c(MDS1, MDS2, pval))
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
    #theme(legend.position="bottom", legend.box = "horizontal") +
    stat_ellipse(data=merged_NMDS, aes(color=site)) +
    scale_color_manual(values=phylum.colors) +
    theme_grey()
}