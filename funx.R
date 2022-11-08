
##snmf plot
snmf_df <- function(K, dmv, meta, pal){
  rownames(K) <- rownames(dmv$gt)
  K_df <- as.data.frame(K)
  K_df <- tibble::rownames_to_column(K_df, "sample")
  meta_S <- dplyr::select(meta, S, sample, lat)
  K_df <- merge(K_df, meta_S, by= "sample")
  K_df <- arrange(K_df, lat)
  
  long <- melt(K_df, id.vars = c("sample", "S", "lat"))
  
  addmix <- ggplot(long, aes(x = reorder(sample, lat), y= value, fill = variable)) + 
    geom_bar(stat="identity") +
    scale_x_discrete(labels = K_df$S, guide = guide_axis(n.dodge = 2)) +
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5), panel.background = element_blank()) +
    scale_fill_brewer(palette = pal) + 
    coord_flip() +
    theme(legend.position='none'
    )
  return(addmix)
}

# organizing data for SNMF Pie charts
sum_K <- function (K, dmv, meta) {
  rownames(K) <- rownames(dmv$gt)
  meta_S <- dplyr::select(meta, S, sample, lat, long)
  k_sum <- as.data.frame(K) %>%
    tibble::rownames_to_column("sample") %>% 
    merge(meta_S, by = "sample")%>%
    transform(lat = as.numeric(lat), long = as.numeric(long)) %>%
    arrange(lat) %>%
    group_by(S) %>%
    summarise_all(mean)
}


get_basemap <- function(basemap){
  
  #get xy from ggmap object
  xs<-seq(from=as.numeric(attr(basemap,"bb")[2]),
          to=as.numeric(attr(basemap,"bb")[4]),length.out=ncol(basemap))
  ys<-seq(from=as.numeric(attr(basemap,"bb")[1]),
          to=as.numeric(attr(basemap,"bb")[3]),length.out=nrow(basemap))
  df<-data.frame(expand.grid(x=xs,y=ys))
  df<-arrange(df,desc(y),x)
  
  #get colors
  df$val=as.vector(basemap)
  #set up colors for ggplot system
  df$val<-factor(df$val,levels=unique(df$val))
  
  basemap = ggplot() +
    geom_raster(data=df,aes(x=x,y=y,fill=val), alpha = 0.7)+
    scale_fill_manual(values=levels(df$val)) +
    theme_bw() +
    coord_quickmap() +
    scale_x_continuous("", expand = c(0, 0)) +
    scale_y_continuous("", expand = c(0, 0)) +
    theme(legend.position="none"
    )
  return(basemap)
}


# Define a function to plot pie charts using ggplot for each site
pie_charts = function(admix_df, site, cols){
  # admix_df = dataframe in long format of admixture proportions per site 
  # site = string 
  # cols = vector of colours of length(clusters)
  ggplot(data = subset(admix_df, S == site),
         aes(x = "", y = value, fill = variable))+
    geom_bar(width = 1, stat = "identity", colour = "black", show.legend = FALSE)+
    coord_polar(theta = "y")+
    scale_fill_manual(values = cols)+
    theme_void()
}