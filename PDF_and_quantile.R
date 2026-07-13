library(ggplotify)
library(ggplot2)
library(tmap)
library(sf)

# day <- sample(365:1095,1)
# case_vec <- as.numeric(colSums(filled_modified[1:day,-1]))/(population_specified_modified$Population*1000)
# case_vec_norm <- case_vec/sum(case_vec)
# q_func <- sort(case_vec_norm)

sf_use_s2(FALSE) 

end_of_month_indices <- c(which(day(filled$Date)==1)[-1]-1,1095)

negent_new <- function (x, K = ceiling(log2(length(x))) + 1){
  p <- table(cut(x, breaks = K))/length(x)
  sigma2 <- var(x)
  #p <- p[(p > 0)]
  bins <- seq(min(x),max(x),length.out=K+1)
  bins2 <- rep(0,K)
  for(i in 1:K){
    bins2[i] <- (bins[i]+bins[i+1])/2
  }
  y <- dnorm(bins2,mean=mean(x),sd=sd(x))
  y <- y/sum(y)
  if(length(which(is.na(y))) == K){ #so long as x ≠ (0,0,0...), but this makes it so that KL divergence is 0 here. Don't like
    #print(p)
    #print(x)
    y <- rep(0,K) #Does this make sense?
  }
  if(length(as.numeric(p))!=length(y)){
    print("Something went wrong")
    print(paste("p",length(as.numeric(p))))
    print(as.numeric(p))
    print(paste("y",length(y)))
    print(y)
  } else if(anyNA(as.numeric(p))){
    print(paste("p had an NA",as.numeric(p)))
  } else if(anyNA(y)){
    print(paste("y had an NA",y))
  }
  return(KL_new(as.numeric(p),y)) #how on earth do we take the negent of the quantile?
  #return(log(2 * pi * exp(1)*sigma2)*1/2 + sum(p * log(p)))
}

PDF_approx_new <- function (x, K = ceiling(log2(length(x))) + 1,epsilon=1e-05){
  p <- table(cut(x+epsilon, breaks = K))/length(x)
  #p <- p[(p > 0)]
  return(p) #how on earth do we take the negent of the quantile?
  #return(log(2 * pi * exp(1)*sigma2)*1/2 + sum(p * log(p)))
}

PDF_approx_epanechikov <- function (cv){
  #kern <- density(cv,n=512,from=0,to=max(cv),kernel="epanechnikov",bw="bcv")$y
  kern <- density(cv,n=512,from=min(cv),to=max(cv),bw="nrd0")$y
  kern <- kern/sum(kern)
  return(kern)
}

normal_approx <- function (x, K = ceiling(log2(length(x))) + 1,epsilon=1e-05){
  bins <- seq(min(x),max(x),length.out=K+1)
  bins2 <- rep(0,K)
  for(i in 1:K){
    bins2[i] <- (bins[i]+bins[i+1])/2
  }
  y <- dnorm(bins2,mean=mean(x),sd=sd(x))
  y <- y/sum(y)
  return(y) #how on earth do we take the negent of the quantile?
  #return(log(2 * pi * exp(1)*sigma2)*1/2 + sum(p * log(p)))
}

#months_vec <- c(12,24,34)
days_vec <- c(7,end_of_month_indices[22],end_of_month_indices[36])

#par(mfrow=c(1,2)) #create matrix
new_negent <- rep(0,length(days_vec))
old_negent <- rep(0,length(days_vec))
shannon_normal <- rep(0,length(days_vec))

quantile_graph_list <- vector(mode="list",length=3)
PDF_graph_list <- vector(mode="list",length=3)
grobbed_tmap_list <- vector(mode='list',length=3)
title_list <- vector(mode='list',length=3)
#load(file="~/Downloads/plot_list2.rdata")
#load(file="~/Downloads/plot_list3.RData")
#load(file="~/Downloads/plot_list4.RData")
#load(file="~/Downloads/plot_list5.RData")
#load(file="~/Downloads/plot_list6.RData")
load(file="~/Downloads/plot_list7.RData")
# 
# for(i in 1:length(end_of_month_indices)){
#   day <- end_of_month_indices[i]
#   case_vec <- as.numeric(colSums(filled_modified[1:day,-1]))/(population_specified_modified$Population*1000)
#   var_mean <- sd(case_vec)/mean(case_vec)
#   plot(PDF_approx_epanechikov(case_vec),main=paste(filled$Date[day],round(stats_of_whole_cumulative_50_2$negentPDF[day],4)))
# }

coeff_var_mat <- matrix(NA,nrow=999,ncol=3)

for(i in 1:999){
  indices <- sample(1:ncol(filled_modified[,-1]),replace=TRUE)
  booted_data <- filled_modified[,-1][,indices]
  pop_spec <- population_specified_modified[indices,]
  for(j in days_vec){
    day <- j
    case_vec <- as.numeric(colSums(booted_data[1:day,]))/(pop_spec$Population*1000)
    var_mean <- sd(case_vec)/mean(case_vec)
    coeff_var_mat[i,match(j,days_vec)] <- var_mean
  }
}
sort(coeff_var_mat[,1],decreasing=FALSE)[25] #June 30, 1630 low
sort(coeff_var_mat[,1],decreasing=FALSE)[975] #June 30, 1630 high
sort(coeff_var_mat[,2],decreasing=FALSE)[25] #December 31, 1630 low
sort(coeff_var_mat[,2],decreasing=FALSE)[975] #December 31, 1630 high
sort(coeff_var_mat[,3],decreasing=FALSE)[25] #October 31, low
sort(coeff_var_mat[,3],decreasing=FALSE)[975] #October 31, 1630 high

for(i in days_vec){
  day <- i
  case_vec <- as.numeric(colSums(filled_modified[1:day,-1]))/(population_specified_modified$Population*1000)
  var_mean <- sd(case_vec)/mean(case_vec)
  print(var_mean)
  case_vec_norm <- case_vec/sum(case_vec)
  q_func <- sort(case_vec_norm)
  #q_func <- sort(case_vec)
  pdf_vec <- as.numeric(PDF_approx_new(case_vec))
  normal <- normal_approx(case_vec)
  shannon_normal[match(i,days_vec)] <- -1*sum(normal[normal!=0]*log(normal[normal!=0]))
  # plot(pdf_vec,main=paste(filled$Date[day],round(var_mean,3))) # PDF
  # lines(normal_approx(case_vec),col="red")
  # barplot(q_func) #quantile
  # plot(PDF_approx_epanechikov(case_vec))
  new_negent[match(i,days_vec)] <- negent_new(case_vec)
  old_negent[match(i,days_vec)] <- negent(case_vec)
  max_ent_uniform <- rep(1/length(case_vec_norm),length(case_vec_norm))
  q_func_df <- as.data.frame(cbind(q_func,max_ent_uniform))
  if(match(i,days_vec)==1){
    quantile_graph <- ggplot(data=q_func_df,aes(y=q_func,x=seq(1,ncol(filled_modified[,-1])))) +
      geom_bar(stat = "identity") +
      geom_bar(aes(y=max_ent_uniform),stat="identity",fill="red",alpha=0.25)+
      labs(y="Normalized Mortality",x="Parishes Ranked by Mortality") +
      coord_cartesian(ylim=c(0,0.15))+
      theme_bw()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),plot.margin = margin(5.5, 5.5, 5.5, 5.5),axis.title = element_text(size=10))
    quantile_graph <- quantile_graph+ggtitle("Quantile Function")
  } else {
    quantile_graph <- ggplot(data=q_func_df,aes(y=q_func,x=seq(1,ncol(filled_modified[,-1])))) +
      geom_bar(stat = "identity") +
      geom_bar(aes(y=max_ent_uniform),stat="identity",fill="red",alpha=0.25)+
      labs(y="Normalized Mortality",x="Parishes Ranked by Mortality") +
      coord_cartesian(ylim=c(0,0.04))+
      theme_bw()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),plot.margin = margin(5.5, 5.5, 5.5, 5.5),axis.title = element_text(size=10))
  }
  K <- ceiling(log2(length(case_vec)))+1
  bins <- seq(min(case_vec),max(case_vec),length.out=K+1)
  bins2 <- rep(0,K)
  for(j in 1:K){
    bins2[j] <- (bins[j]+bins[j+1])/2
  }
  bins3 <- seq(from=min(case_vec),to=max(case_vec),length.out=512)
  pdf_vec_2 <- PDF_approx_epanechikov(case_vec)
  max_ent_normal <- dnorm(bins3,mean=mean(case_vec),sd=sd(case_vec))
  max_ent_normal <- max_ent_normal/sum(max_ent_normal)
  pdf_df <- data.frame(x=bins3,y=pdf_vec_2,y2=max_ent_normal)
  if(match(i,days_vec)==1){
    PDF_graph <- ggplot(data=pdf_df,aes(x=x,y=y))+
      geom_density(stat="identity",fill="darkgrey",alpha=0.5,lwd=0.25)+
      geom_density(aes(y=y2),stat="identity",fill="red",alpha=0.25,lwd=0.25)+
      labs(y="Normalized Parish Count",x="Cumulative Mortality")+
      coord_cartesian(ylim=c(0,0.01))+
      theme_bw()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),plot.margin = margin(5.5, 5.5, 5.5, 5.5),axis.title = element_text(size=10))
    PDF_graph <- PDF_graph+ggtitle("PDF")
  } else {
  PDF_graph <- ggplot(data=pdf_df,aes(x=x,y=y))+
    geom_density(stat="identity",fill="darkgrey",alpha=0.5,lwd=0.25)+
    geom_density(aes(y=y2),stat="identity",fill="red",alpha=0.25,lwd=0.25)+
    labs(y="Normalized Parish Count",x="Cumulative Mortality")+
    coord_cartesian(ylim=c(0,0.006))+
    theme_bw()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),plot.margin = margin(5.5, 5.5, 5.5, 5.5),axis.title = element_text(size=10))
  }
  quantile_graph_list[[match(i,days_vec)]] <- quantile_graph
  
  if(match(i,days_vec)==1){
    tmap_plot <- plot_list7[[match(i, days_vec)]] +
      tm_layout(
        main.title="    Choropleth Map",
        main.title.size=1.1,
        #legend.text.size = .55,
        outer.margins = c(0, 0, 0, 0),
        inner.margins = c(0, 0, 0, 0),
        frame = FALSE
      )
  } else{
    tmap_plot <- plot_list7[[match(i, days_vec)]] +
      tm_layout(
        main.title=" ",
        main.title.size=1.1,
        #legend.text.size = .55,
        outer.margins = c(0, 0, 0, 0),
        inner.margins = c(0, 0, 0, 0),
        frame = FALSE
      )
  }
  grobbed_tmap_plot <- tmap_grob(tmap_plot)

  grobbed_tmap_list[[match(i, days_vec)]] <- grobbed_tmap_plot

  PDF_graph_list[[match(i,days_vec)]] <- PDF_graph
  
  PDF_and_quantile_graph <- plot_grid(plotlist=list(PDF_graph,quantile_graph))
  
  #untitled_grid <- plot_grid(plotlist=list(PDF_and_quantile_graph,heatmap_plot))
  
  title <- ggdraw() + 
    draw_label(
      filled$Date[day],
      fontface = 'bold',
      x = 0,
      hjust = 0
    ) +
    theme(
      # add margin on the left of the drawing canvas,
      # so title is aligned with left edge of first plot
      plot.margin = margin(0, 0, 0, 7)
    )
  title_list[[match(i,days_vec)]] <- title
  overall_graph <- plot_grid(
    title, PDF_and_quantile_graph,
    ncol=1, nrow=2,
    # rel_heights values control vertical title margins
    rel_heights = c(0.1, 1)
  )
  print(overall_graph)
}
par(mfrow=c(1,1)) #reset

master_plot_list <- vector(mode='list',length=9)
master_plot_list_1 <- vector(mode="list",length=3)
master_plot_list_2 <- vector(mode="list",length=3)
master_plot_list_3 <- vector(mode="list",length=3)

for(k in 1:length(days_vec)){
  master_plot_list[[k]] <- grobbed_tmap_list[[k]]
  master_plot_list_1[[k]] <- grobbed_tmap_list[[k]]
}

for(k in 4:6){
  master_plot_list[[k]] <- quantile_graph_list[[k-3]]
  master_plot_list_2[[k-3]] <- quantile_graph_list[[k-3]]
}
for(k in 7:9){
  master_plot_list[[k]] <- PDF_graph_list[[k-6]]
  master_plot_list_3[[k-6]] <- PDF_graph_list[[k-6]]
}

plot_grid_1 <- plot_grid(plotlist=master_plot_list_1,ncol=3,nrow=1,align="hv",axis="tblr",labels=c("a","",""),label_size=12)
plot_grid_2 <- plot_grid(plotlist=master_plot_list_2,ncol=3,nrow=1,align="hv",axis="tblr",labels=c("b","",""),label_size=12)
plot_grid_3 <- plot_grid(plotlist=master_plot_list_3,ncol=3,nrow=1,align="hv",axis="tblr",labels=c("c","",""),label_size=12)

plot_grid(plot_grid_1,plot_grid_2,plot_grid_3,nrow=3,align="hv",axis="tblr")

plot_grid(plotlist=master_plot_list,ncol=3, nrow=3, align = 'hv', axis = 'tblr',labels=c("a","","","b","","","c","",""),label_size=12)
#ggsave("Gridded_Incidence_Map_Final.png",width=2900,height=1800,units="px")
ggsave("Gridded_Incidence_Map_Final_10.png",width=3000,height=1800,units="px")
ggsave("~/VenetianPlague_Entropy/Figures_Final/Figure1v2.png",width=3000,height=1800,units="px")
ggsave("~/VenetianPlague_Entropy/Figures_Final/Figure1v2.jpg",width=3000,height=1800,units="px")

# plot.ts(new_negent,col="red",ylim=c(0,2))
# lines(old_negent)