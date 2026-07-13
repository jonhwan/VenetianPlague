library(Hmisc)

#Gaussian vs CMF, PMF ----

##CMF ----

CDF_cumulative_BC <- function(date){
  cumulative_df <- Cumulative(date,filled)
  v <- as.numeric(colSums(filled[1:match(date,filled$Date),-1]))
  v <- v/(population_specified$Population*1000)
  m <- mean(v)
  s <- sd(v)
  x <- seq(0,max(v),length.out=length(colnames(filled[,-1])))
  y <- pnorm(x,mean=m,sd=s)*length(colnames(filled[,-1]))
  #plot(x,y,type="l",lwd=1)
  p <- cumulative_df %>%
    mutate(population_corrected = cumulativeDeaths/(population_specified$Population*1000)) %>%
    arrange(population_corrected) %>%
    ggplot(aes(y=factor(parish_list,level=parish_list[order(population_corrected)]),x=population_corrected))+
    geom_bar(stat="identity")+
    geom_line(aes(x=x,y=y))+
    ggtitle(paste("Cumulative up to",format(as.Date(date,origin="1970-01-01")))) +
    labs(y="Parish",x="Cumulative Deaths/Population")
  print(p)
}

CDF_cumulative_BC_grid <- function(plot_year){
  cumulative_dfs <- list()
  filled_months <- filled[day(filled$Date)==1,]
  for(date in unique(filled_months$Date)){
    v <- as.numeric(colSums(filled[1:match(date,filled$Date),-1]))
    v <- v/(population_specified$Population*1000)
    filled_no_Date <- filled[,-1]
    cumulative_df <- data.frame(v,colnames(filled[,-1]))
    colnames(cumulative_df) <- c("v","parish_list")
    m <- mean(v[!v %in% boxplot.stats(v)$out])
    s <- sd(v[!v %in% boxplot.stats(v)$out]) #removing outliers, questionable legitimacy
    x <- seq(0,max(v),length.out=length(v))
    y <- pnorm(x,mean=m,sd=s)*length(v)
    cumulative_dfs <- append(cumulative_dfs,list(cumulative_df),after=match(date,filled_months$Date))
    #plot(x,y,type="l",lwd=1)
    p <- cumulative_df %>%
      arrange(v) %>%
      ggplot(aes(y=factor(parish_list,level=parish_list[order(v)]),x=v))+
      geom_bar(stat="identity")+
      geom_line(aes(x=x,y=y))+
      ggtitle(paste("Cumulative up to",format(as.Date(date,origin="1970-01-01")))) +
      labs(y="Parish",x="Cumulative Deaths/Population") +
      theme(axis.text.y=element_blank())
    print(p)
  }
  l <- lapply(cumulative_dfs[((plot_year-1629)*12+1):((plot_year-1628)*12)],function(i){
    ggplot(i,aes(y=factor(parish_list,level=parish_list[order(v)]),x=v))+
      geom_bar(stat="identity")+
      geom_line(aes(x=seq(0,max(v),length.out=length(v)),y=pnorm(seq(0,max(v),length.out=length(v)),mean=mean(i$v[!i$v %in% boxplot.stats(i$v)$out]),sd=sd(i$v[!i$v %in% boxplot.stats(i$v)$out]))*length(i$v),color="red"))+
      labs(y="Parish",x="Cumulative Deaths/Population")+
      theme(axis.text.y=element_blank())
  })
  grid <- ggarrange(plotlist=l[c(1,7,2,8,3,9,4,10,5,11,6,12)],ncol=2,nrow=6)
  grid <- annotate_figure(grid, top = text_grob(as.character(plot_year),color="red",face="bold",size=14))
  print(grid)
  ggsave(paste("GaussianCDFModel(Red)vsCDFfor",as.character(plot_year),"OutliersRemoved.png",sep=""),width=1920,height=2100,units="px")
}

##PMF ----

PDF_cumulative_comparison <- function(date){
  v <- as.numeric(colSums(filled[1:match(date,filled$Date),-1]))
  v <- v/(population_specified$Population*1000)
  v <- v[!v %in% boxplot.stats(v)$out] #remove outliers (is this justified? It gets rid of zeros which is annoying)
  m <- mean(v)
  s <- sd(v) #produces a larger sd/distribution than the built in geom_density() function. That's because of outliers.
  x <- seq(0,max(v),length.out=length(v))
  y <- dnorm(x,mean=m,sd=s) #maybe dnorm isn't the best function for this
  cumulative_df_2 <- data.frame(v)
  colnames(cumulative_df_2) <- "v"
  #plot(x,y,type="l",lwd=1)
  p <- cumulative_df_2 %>%
    ggplot(aes(x=v,y=after_stat(density))) +
    geom_histogram(bins=100)+
    #geom_histogram(bins=length(colnames(filled[,-1])))+ #annoying thing about this bin size and fixed axes 
    #(N bins and bin size basically identical in fixed axes)
    #is they all get clumped up at the beginning but it's necessary to show the end part
    geom_density()+
    geom_line(aes(x=x,y=y,color="red"))+
    scale_y_continuous()+
    scale_x_continuous()+
    expand_limits(x=c(0,max(v)),y=c(0,40)) +
    ggtitle(paste("Cumulative up to",format(as.Date(date,origin="1970-01-01"))))+
    labs(y = "Density", x = "Cumulative Deaths/Population") #Density of Parishes
  plot_data <- (ggplot_build(p))$data[[2]]$y
  hist(v,prob=TRUE,breaks=x)
  lines(density(v,adjust=2),lty="dotted")
  # print(plot_data[c(1:length(colnames(filled[,-1])))*floor(length(plot_data)/length(colnames(filled[,-1])))])
  # print(y)
  # print(plot_data[c(1:length(colnames(filled[,-1])))*floor(length(plot_data)/length(colnames(filled[,-1])))]-y)
  gg_mean <- weighted.mean(ggplot_build(p)$data[[2]]$x[c(1:length(v))*floor(length(plot_data)/length(v))], plot_data[c(1:length(v))*floor(length(plot_data)/length(colnames(filled[,-1])))])
  print(gg_mean)
  print(weighted.mean(x,y))
  print(m)
  print(sqrt(wtd.var(ggplot_build(p)$data[[2]]$x[c(1:length(v))*floor(length(plot_data)/length(v))], plot_data[c(1:length(v))*floor(length(plot_data)/length(v))])))
  print(sqrt(wtd.var(x,y)))
  print(s)
  print(p)
}

PDF_cumulative_comparison_grid <- function(plot_year){
  cumulative_dfs <- list()
  filled_months <- filled[day(filled$Date)==1,]
  y_label <- "Density"
  for(date in unique(filled_months$Date)){
    v <- as.numeric(colSums(filled[1:match(date,filled$Date),-1]))
    v <- v/(population_specified$Population*1000)
    v <- v[!v %in% boxplot.stats(v)$out] #again, getting rid of outliers has questionable validity
    m <- mean(v)
    s <- sd(v)
    x <- seq(0,max(v),length.out=length(v))
    y <- dnorm(x,mean=m,sd=s)
    cumulative_df <- data.frame(v)
    colnames(cumulative_df) <- c("v")
    cumulative_dfs <- append(cumulative_dfs,list(cumulative_df),after=match(date,filled_months$Date))
    p <- cumulative_df %>%
      ggplot(aes(x=v,y=after_stat(density))) +
      geom_histogram(bins=length(v))+ #annoying thing about this bin size and fixed axes 
      #(N bins and bin size basically identical in fixed axes)
      #is they all get clumped up at the beginning but it's necessary to show the end part
      geom_density()+
      geom_line(aes(x=x,y=y,color="red"))+
      scale_y_continuous()+ #just because the max entropy for the CDF looks normal, doesn't mean the max entropy for the PDF will also be normal. Actually usually they're different
      scale_x_continuous()+
      expand_limits(x=c(0,max(v)),y=c(0,40)) +
      ggtitle(paste("Cumulative up to",format(as.Date(date,origin="1970-01-01"))))+
      labs(y = "Density", x = "Cumulative Deaths/Population") #Density of Parishes
    print(p)
  }
  #print(cumulative_dfs)
  l <- lapply(cumulative_dfs[((plot_year-1629)*12+1):((plot_year-1628)*12)],function(i){
    ggplot(i,aes(x=v,y=after_stat(density)))+
      geom_histogram(bins=length(v))+
      geom_density()+
      geom_line(aes(x=seq(0,max(v),length.out=length(v)),y=dnorm(seq(0,max(v),length.out=length(v)),mean=mean(v),sd=sd(v)),color="red"))+
      scale_y_continuous()+
      scale_x_continuous()+
      expand_limits(x=c(0,max(v)),y=c(0,40))+
      labs(y = "Density", x = "Cumulative Deaths/Population")
  })
  grid <- ggarrange(plotlist=l[c(1,7,2,8,3,9,4,10,5,11,6,12)],ncol=2,nrow=6) #need to do lapply, otherwise ggplot evaluates loops at the end of the loop so all the normal approx. will show up as the last one twelve times.
  grid <- annotate_figure(grid, top = text_grob(as.character(plot_year),color="red",face="bold",size=14))
  print(grid)
  ggsave(paste("GaussianModel(Red)vsPMFfor",as.character(plot_year),"OutliersRemoved.png",sep=""),width=1920,height=2100,units="px")
}

#Normalized p/q ----

shannon_offset <- function(case_vector,pop){
  pop_specified <- pop[pop$Parish %in% colnames(filled[,-1]),]
  pop_specified <- pop_specified[match(colnames(filled[,-1]),pop_specified$Parish),]
  somewhat_offset <- case_vector/pop_specified$Population
  normalized_offset <- (case_vector/pop_specified$Population)/sum(case_vector/pop_specified$Population)
  CV_norm <- case_vector/sum(case_vector)
  #print(-1*sum(CV_norm*log(as.numeric(sub("^0$","0.00001",normalized_offset)))))
  summand <- 0
  for(i in 1:length(normalized_offset)){
    if(normalized_offset[i] == 0){
      #summand <- summand + case_vector[i]*log(0.0000000001)
      next
    }
    summand <- summand + CV_norm[i]*log(normalized_offset[i]) #looks way too similar to sum(ni) when you do case_vector[i] instead of CV_norm[i]. I think because we just have too small sample size (so many 0's) to have that much variability in entropy, so when we multiply entropy by N it just looks like N.
  }
  # if(summand == 0){
  #   return(1) #should be inf but want to see the rest of the graph
  # } else {
  # return(-1*1/(summand/length(CV_norm[CV_norm != 0])))
  # }
  return(-1*summand)
}

plot_df <- data.frame(c(rep(0,length(filled$Date))))
colnames(plot_df) <- c("Shannon_offset")

for(date in filled$Date){
  filledOneDay <- as.integer(filled[date==filled$Date,-1])
  filledOneDay <- filledOneDay[!is.na(filledOneDay)]
  plot_df[match(date,filled$Date),1] <- shannon_offset(filledOneDay,population)
}
plot.ts(plot_df$Shannon_offset)
plot.ts(cumsum(plot_df$Shannon_offset))

#Kullback-Leibler ----

KL <- function(prob1,prob2){
  non_zeroes <- which(prob1 != 0)
  prob1 <- prob1[non_zeroes]
  prob2 <- prob2[non_zeroes] #because you would be multiplying by 0 anyways
  return(-1*sum(prob1*log(prob2/prob1)))
}

gaussian_KL <- function(date){
  case_vec <- as.numeric(colSums(filled[1:match(date,filled$Date),-1])) #Note: this is for the CUMULATIVE PDF
  pop_acc <- case_vec/(population_specified$Population*1000)
  pop_norm <- pop_acc/sum(pop_acc)
  no_outliers <- pop_acc[!pop_acc %in% boxplot.stats(pop_acc)$out]
  m <- mean(pop_acc) #or should it be pop_norm? i.e. normalize before or normalize after. looks like it's the same if you normalize before vs. after
  s <- sd(pop_acc) #if you want to get rid of outliers replace the pop_acc in front of m, s, and max with no_outliers. looks like the outlier version is what we want though
  x <- seq(0,max(pop_acc),length.out=length(pop_acc))
  y <- dnorm(x,sd=s,mean=m)
  y <- y/sum(y)
  pop_hist <- hist(pop_norm,breaks=seq(0,max(pop_norm),length.out=length(pop_acc)+1),plot=FALSE)$counts/sum(hist(pop_norm,breaks=seq(0,max(pop_norm),length.out=length(pop_acc)+1),plot=FALSE)$counts)
  return(KL(pop_norm,y)) #wait I don't think pop_norm is the correct PDF, don't we want the histogram thing or whatever?
  #return(KL(pop_hist,y))
} #things to try: plot this probability so you understand what it means for it to look "normal", try this function but with the histogram as PDF, also try the smoothing histogram for PDF, and finally try on the daily (non-cumulative) PDF

uniform_KL <- function(date){
  case_vec <- as.numeric(colSums(filled[1:match(date,filled$Date),-1]))
  pop_acc <- case_vec/(population_specified$Population*1000)
  pop_norm <- pop_acc/sum(pop_acc)
  #pop_norm <- pop_norm[which(pop_norm != 0)] #optional code to get rid of 0's
  y <- rep(1/length(pop_norm),length(pop_norm)) #try the version where you remove 0s so the probs for the non-zero terms get bigger
  pop_hist <- hist(pop_norm,breaks=seq(0,max(pop_norm),length.out=length(pop_acc)+1),plot=FALSE)$counts/sum(hist(pop_norm,breaks=seq(0,max(pop_norm),length.out=length(pop_acc)+1),plot=FALSE)$counts)
  return(KL(pop_norm,y))
  #return(KL(pop_hist,y)) #wait hold up this just looks like tom's 1/temperature thing. I think the PDF approaching normal max entropy is equivalent to pop_norm approaching uniform, i.e. KL is so low with uniform because uniform is max entropy dist for pop_norm (my predictions), idk if right or not
}

dailyGaussianKL <- function(date,outliers=TRUE){
  case_vec <- as.numeric(filled[filled$Date == date,-1])
  pop_acc <- case_vec/(population_specified$Population*1000)
  pop_norm <- pop_acc/sum(pop_acc)
  no_outliers <- pop_acc[!pop_acc %in% boxplot.stats(pop_acc)$out]
  if(outliers == TRUE){
    m <- mean(pop_acc) #or should it be pop_norm? i.e. normalize before or normalize after. looks like it's the same if you normalize before vs. after
    s <- sd(pop_acc) #if you want to get rid out outliers replace the pop_acc in front of m, s, and max with no_outliers. looks like the outlier version is what we want though
  } else{ #does weird stuff if you type outliers=FALSE, but why?
    if(IQR(pop_acc != 0)){
      m <- mean(no_outliers)
      s <- sd(no_outliers)
    } else{
      m <- mean(pop_acc)
      s <- sd(pop_acc)
    }
  }
  x <- seq(0,max(pop_acc),length.out=length(pop_acc))
  y <- dnorm(x,sd=s,mean=m)
  y <- y/sum(y)
  pop_hist <- hist(pop_norm,breaks=seq(0,max(pop_norm),length.out=length(pop_acc)+1),plot=FALSE)$counts/sum(hist(pop_norm,breaks=seq(0,max(pop_norm),length.out=length(pop_acc)+1),plot=FALSE)$counts)
  return(KL(pop_norm,y))
  #return(KL(pop_hist,y)) #wait I don't think pop_norm is the correct PDF, don't we want the histogram thing or whatever?
}

dailyUniformKL <- function(date){
  case_vec <- as.numeric(filled[filled$Date == date,-1])
  pop_acc <- case_vec/(population_specified$Population*1000)
  pop_norm <- pop_acc/sum(pop_acc)
  #pop_norm <- pop_norm[which(pop_norm != 0)] #optional code to get rid of 0's
  y <- rep(1/length(pop_norm),length(pop_norm)) #try the version where you remove 0s so the probs for the non-zero terms get bigger
  pop_hist <- hist(pop_norm,breaks=seq(0,max(pop_norm),length.out=length(pop_acc)+1),plot=FALSE)$counts/sum(hist(pop_norm,breaks=seq(0,max(pop_norm),length.out=length(pop_acc)+1),plot=FALSE)$counts)
  return(KL(pop_norm,y))
  #return(KL(pop_hist,y))
}

#should I try exponential and geometric? it looks like a skewed normal sometimes, so what would be a good distribution for that? figure out what pdf is approaching (is max entropy for cumulative pdf vs daily pdf different?)

KL_df <- as.data.frame(matrix(nrow=nrow(filled),ncol=4))
colnames(KL_df) <- c("Gaussian_KL","Uniform_KL","DailyGaussianKL","DailyUniformKL") #plots say that Uniform is a better match to reality on both daily and cumulative than normal, but if you look at the actual "PDFs" that doesn't make sense. I think there's something inaccurate about our normal approx.
#I think this could be prevented by getting rid of outliers, but some weird behavior happens when you do.
#my guess is that our outlier really screws up our KL with the normal, but is sort of balanced out by the uniform.
#If you plot the KL with respect to the histogram counts the gaussian and uniform just look like slightly scaled or squished versions of each other but if you plot the KL wrt the normalized case vector accounting for population you get more discrepancy.
negent_test <- c()
for(date in filled$Date){
  KL_df[match(date,filled$Date),1] <- gaussian_KL(date)
  KL_df[match(date,filled$Date),2] <- uniform_KL(date)
  KL_df[match(date,filled$Date),3] <- dailyGaussianKL(date,outliers=TRUE)
  KL_df[match(date,filled$Date),4] <- dailyUniformKL(date)
  negent_test <- c(negent_test,negent(as.numeric(colSums(filled[1:match(date,filled$Date),-1]))/population_specified$Population))
}
plot.ts(KL_df$Gaussian_KL)
plot.ts(negent_test)
plot.ts(KL_df$Uniform_KL)
plot.ts(KL_df$DailyGaussianKL)
plot.ts(KL_df$DailyUniformKL)

#No Outliers ----

##Cross Entropy ----

gaussianNoOutliers <- function(date,cumulative=TRUE,exclude_outliers=TRUE){ #gaussian ce but we get rid of outliers
  if(cumulative == TRUE){
    v <- as.numeric(colSums(filled[1:match(date,filled$Date),-1]))
  } else {
    v <- as.numeric(filled[filled$Date == date,-1])
  }
  v <- v/(population_specified$Population*1000)
  # if(IQR(v) != 0){
  #   v <- v[!v %in% boxplot.stats(v)$out]
  # }
  if(exclude_outliers == TRUE){
    Q <- quantile(v, probs=c(.01, .99), na.rm = FALSE) #1% and 99% quantiles just to get rid of that really big parish
    iqr <- IQR(v)
    v <- subset(v, v < Q[2]+1.5*iqr) #this changes the cumulative a lot compared to no removing outliers. This code may be responsible for the weirdness @ the start and the artificial dip right before the jump up as one parish genuinely got above the 99% quantile which was marked as an outlier before it spread to other parishes. 
  }
  #v <- v[!v %in% boxplot.stats(v)$out] #find a better algorithm for getting rid of outliers
  if(sum(v) == 0){
    v_norm <- v
  } else{
    v_norm <- v/sum(v)
  }
  m <- mean(v) #this also seems slightly off somehow
  s <- sd(v)
  x <- seq(0,max(v),length.out=length(v))
  y <- dnorm(x,mean=m,sd=s)
  y_norm <- y/sum(y)
  y_nonzeroes <- which(y != 0)
  v_hist <- hist(v_norm,breaks=seq(0,max(v_norm),length.out=length(v_norm)+1),plot=FALSE)$counts/sum(hist(v_norm,breaks=seq(0,max(v_norm),length.out=length(v_norm)+1),plot=FALSE)$counts)
  return(-1*sum(v_hist*log(y_norm))) #When removing outliers often get all 0's. How to fix, since outputting 0 for the entropy for those feels weird? maybe get a less trigger-happy outlier removal algorithm.
}

uniformNoOutliers <- function(date,cumulative=TRUE){ #doesn't really seem to work...
  if(cumulative == TRUE){
    v <- as.numeric(colSums(filled[1:match(date,filled$Date),-1]))
  } else {
    v <- as.numeric(filled[filled$Date == date,-1])
  }
  v <- v/(population_specified$Population*1000)
  Q <- quantile(v, probs=c(.01, .99), na.rm = FALSE) #1% and 99% quantiles just to get rid of that really big parish
  iqr <- IQR(v)
  v <- subset(v, v < Q[2]+1.5*iqr) #this changes the cumulative a lot compared to no removing outliers. This code may be responsible for the weirdness @ the start and the artificial dip right before the jump up as one parish genuinely got above the 99% quantile which was marked as an outlier before it spread to other parishes. 
  if(sum(v) == 0){
    v_norm <- v
  } else{
    v_norm <- v/sum(v)
  }
  y <- rep(1/length(v_norm),length(v_norm))
  v_hist <- hist(v_norm,breaks=seq(0,max(v_norm),length.out=length(v_norm)+1),plot=FALSE)$counts/sum(hist(v_norm,breaks=seq(0,max(v_norm),length.out=length(v_norm)+1),plot=FALSE)$counts)
  return(-1*sum(v_norm*log(y))) #When removing outliers often get all 0's. How to fix, since outputting 0 for the entropy for those feels weird? maybe get a less trigger-happy outlier removal algorithm.
}

no_outlier_CE <- as.data.frame(matrix(nrow=length(filled$Date),ncol=4))
colnames(no_outlier_CE) <- c("Gaussian_Cumulative","Gaussian_Daily","Uniform_Cumulative","Uniform_Daily")
gaussian_CE_outliers <- as.data.frame(matrix(nrow=length(filled$Date),ncol=1))
for(date in filled$Date){
  no_outlier_CE[match(date,filled$Date),1] <- gaussianNoOutliers(date)
  no_outlier_CE[match(date,filled$Date),2] <- gaussianNoOutliers(date,cumulative=FALSE)
  no_outlier_CE[match(date,filled$Date),3] <- uniformNoOutliers(date)
  no_outlier_CE[match(date,filled$Date),4] <- uniformNoOutliers(date,cumulative=FALSE)
  gaussian_CE_outliers[match(date,filled$Date),1] <- gaussianNoOutliers(date,cumulative=FALSE,exclude_outliers = FALSE)
}
plot.ts(no_outlier_CE$Gaussian_Cumulative) #used to start high. May be because so close to 0, a half of the normal just gets cut off. Sporadic jumps show fault of data? I think this is wrong though, looks so different from CE_df version (excluding outliers makes huge diff).
plot.ts(no_outlier_CE$Gaussian_Daily)
plot.ts(gaussian_CE_outliers[,1])
plot.ts(no_outlier_CE$Uniform_Cumulative)
plot.ts(no_outlier_CE$Uniform_Daily)

##Kullback-Leibler ----

gaussianKL_noOutliers <- function(date, cumulative=TRUE){
  if(cumulative == TRUE){
    v <- as.numeric(colSums(filled[1:match(date,filled$Date),-1]))
  } else {
    v <- as.numeric(filled[filled$Date == date,-1])
  }
  v <- v/(population_specified$Population*1000)
  Q <- quantile(v, probs=c(.01, .99), na.rm = FALSE)
  iqr <- IQR(v)
  v <- subset(v, v < Q[2]+1.5*iqr) 
  if(sum(v) == 0){
    v_norm <- v
  } else{
    v_norm <- v/sum(v)
  }
  m <- mean(v)
  s <- sd(v)
  x <- seq(0,max(v),length.out=length(v))
  y <- dnorm(x,mean=m,sd=s)
  y_norm <- y/sum(y)
  y_nonzeroes <- which(y != 0)
  v_hist <- hist(v_norm,breaks=seq(0,max(v_norm),length.out=length(v_norm)+1),plot=FALSE)$counts/sum(hist(v_norm,breaks=seq(0,max(v_norm),length.out=length(v_norm)+1),plot=FALSE)$counts)
  return(KL(v_hist,y_norm))
}

uniformKL_noOutliers <- function(date, cumulative=TRUE){
  if(cumulative == TRUE){
    v <- as.numeric(colSums(filled[1:match(date,filled$Date),-1]))
  } else {
    v <- as.numeric(filled[filled$Date == date,-1])
  }
  v <- v/(population_specified$Population*1000)
  Q <- quantile(v, probs=c(.01, .99), na.rm = FALSE)
  iqr <- IQR(v)
  v <- subset(v, v < Q[2]+1.5*iqr) 
  if(sum(v) == 0){
    v_norm <- v
  } else{
    v_norm <- v/sum(v)
  }
  y <- rep(1/length(v_norm),length(v_norm))
  return(KL(v_norm,y))
}

no_outlier_KL <- as.data.frame(matrix(nrow=length(filled$Date),ncol=4))
colnames(no_outlier_KL) <- c("Gaussian_Cumulative","Gaussian_Daily","Uniform_Cumulative","Uniform_Daily")
for(date in filled$Date){
  no_outlier_KL[match(date,filled$Date),1] <- gaussianKL_noOutliers(date)
  no_outlier_KL[match(date,filled$Date),2] <- gaussianKL_noOutliers(date,cumulative=FALSE)
  no_outlier_KL[match(date,filled$Date),3] <- uniformKL_noOutliers(date)
  no_outlier_KL[match(date,filled$Date),4] <- uniformKL_noOutliers(date,cumulative=FALSE)
}
plot.ts(no_outlier_KL$Gaussian_Cumulative)
plot.ts(no_outlier_KL$Gaussian_Daily)
plot.ts(no_outlier_KL$Uniform_Cumulative)
plot.ts(no_outlier_KL$Uniform_Daily)

#Sestiere ----

#Geography Clumped Data, have sestiere data (sestiere_pre, sestiere_post, sestiere_df) AND lat/long data (geo_df)

#date <- as.Date(sample(as.numeric(as.Date("1629-01-01")):as.numeric(as.Date("1631-12-31")),1),origin="1970-01-01")
date <- as.Date("1631-7-21")

cumulative_cases <- Cumulative(date,filled)
cumulative_cases$cumulativeDeaths <- cumulative_cases$cumulativeDeaths/(population_specified$Population*1000) #population_specified needs this order to begin with (alphabetical) so do the population correction first
cumulative_cases <- cumulative_cases[match(sestiere_df$Parish, cumulative_cases$parish_list),] #funnily enough filled only contains 4 parishes from S. Croce
sestiere_df_cumulative <- cbind(sestiere_df,cumulative_cases$cumulativeDeaths)
colnames(sestiere_df_cumulative) <- c("Parish", "Sestiere", "cumulative_pop_corrected")
#does it make sense to order the sestieres according to the one with the highest max cumulative_pop_corrected or to just keep the ordering of the sestieres consistent

##CDF cumulative ----
sestiere_df_cumulative %>%
    arrange(Sestiere,cumulative_pop_corrected) %>%
    ggplot(aes(y=factor(Parish,level=Parish[order(Sestiere,cumulative_pop_corrected)]),x=cumulative_pop_corrected,fill=Sestiere))+
    geom_bar(stat="identity")+
    ggtitle(paste("Cumulative up to",format(as.Date(date,origin="1970-01-01")))) +
    labs(y="Parish",x="Cumulative Deaths/Population")

##PDF cumulative ----
sestiere_df_cumulative %>%
  ggplot(aes(x=cumulative_pop_corrected,y=after_stat(density),fill=Sestiere)) + #how to group by Sestiere and make each a unique color?
  #geom_histogram(bins=50,color='#e9ecef',alpha=0.2)+ #position = "identity" makes them not stack on top of each other, but without it you get the original shape of the histogram. Annoyingly, way taller than density plot.
  #Since there are so few parishes from S. Croce, it is more prone to slight variation. Is there a way to account for # of parishes we have in a sestiere?
  geom_density(aes(x=cumulative_pop_corrected,fill=Sestiere,color=Sestiere),alpha=0.25) + #If a sestiere has a peak, it doesn't mean that it has more parishes in that bin than others, since geom_density calculates separately for each sestiere. Instead, it means that that sestiere's parishes were more concentrated around that incidence rate.
  #geom_density(aes(x=cumulative_pop_corrected))+
  scale_y_continuous()+
  scale_x_continuous()+
  expand_limits(x=c(0,0.65),y=c(0,40)) +
  ggtitle(paste("Cumulative up to",format(as.Date(date,origin="1970-01-01"))))+
  labs(y = "Density of Parishes", x = "Cumulative Deaths/Population")

##Grids ----

sestierePDFCumulative <- function(plot_year){
  plot_list <- list()
  filled_months <- filled[day(filled$Date) == 1,]
  for(date in filled_months$Date){
    cumulative_cases <- Cumulative(date,filled)
    cumulative_cases$cumulativeDeaths <- cumulative_cases$cumulativeDeaths/(population_specified$Population*1000) #population_specified needs this order to begin with (alphabetical) so do the population correction first
    cumulative_cases <- cumulative_cases[match(sestiere_df$Parish, cumulative_cases$parish_list),] #funnily enough filled only contains 4 parishes from S. Croce
    sestiere_df_cumulative <- cbind(sestiere_df,cumulative_cases$cumulativeDeaths)
    colnames(sestiere_df_cumulative) <- c("Parish", "Sestiere", "cumulative_pop_corrected")
    p <- sestiere_df_cumulative %>%
      ggplot(aes(x=cumulative_pop_corrected,fill=Sestiere,color=Sestiere),alpha=0.25)+
      geom_density(aes(x=cumulative_pop_corrected,fill=Sestiere,color=Sestiere),alpha=0.25)+
      scale_y_continuous()+
      scale_x_continuous()+
      expand_limits(x=c(0,0.65),y=c(0,40))+
      ggtitle(format(as.Date(date,origin="1970-01-01")))+
      #guides(color=guide_legend(override.aes=list(size=0.5)))+
      theme(legend.key.size = unit(0.25,"cm"),legend.title=element_text(size=5),legend.text=element_text(size=5),axis.title.x=element_text(size=5),axis.title.y=element_text(size=5),plot.title=element_text(size=10))+
      labs(y="Density", x = "Cumulative Deaths/Population")
    if(year(date) == plot_year){
      plot_list <- c(plot_list,list(p))
    }
    print(p)
  }
  grid <- ggarrange(plotlist=plot_list[c(1,7,2,8,3,9,4,10,5,11,6,12)],ncol=2,nrow=6)
  grid <- annotate_figure(grid, top = text_grob(as.character(plot_year),color="red",face="bold",size=14))
  print(grid)
  ggsave(paste("CumulativePDFbySestierefor",as.character(plot_year),".png",sep=""),width=1600,height=2160,units="px")
}

sestierePDFDaily <- function(plot_year){
  plot_list <- list()
  filled_months <- filled[day(filled$Date) == 1,]
  for(date in filled_months$Date){
    daily_cases <- data.frame(as.numeric(filled[filled$Date == date,-1]),colnames(filled[,-1]))
    colnames(daily_cases) <- c("dailyDeaths","parish_list")
    daily_cases$dailyDeaths <- daily_cases$dailyDeaths/(population_specified$Population*1000)
    daily_cases <- daily_cases[match(sestiere_df$Parish, daily_cases$parish_list),]
    sestiere_df_daily <- cbind(sestiere_df,daily_cases$dailyDeaths)
    colnames(sestiere_df_daily) <- c("Parish", "Sestiere", "today_pop_corrected")
    p <- sestiere_df_daily %>%
      ggplot(aes(x=today_pop_corrected,y=after_stat(density),fill=Sestiere))+
      geom_density(aes(x=today_pop_corrected,group=Sestiere,color=Sestiere,fill=Sestiere),alpha=0.25)+
      scale_y_continuous()+
      scale_x_continuous()+
      expand_limits(x=c(0,0.01),y=c(0,5000))+
      labs(x="Daily Deaths/Population",y="Density of Parishes")+
      ggtitle(paste("Daily PDF for",format(as.Date(date,origin="1970-01-01"))))
    if(year(date) == plot_year){
      plot_list <- c(plot_list,list(p))
    }
    print(p)
  }
  grid <- ggarrange(plotlist=plot_list[c(1,7,2,8,3,9,4,10,5,11,6,12)],ncol=2,nrow=6)
  grid <- annotate_figure(grid, top = text_grob(as.character(plot_year),color="red",face="bold",size=14))
  print(grid)
}

daily_cases <- data.frame(as.numeric(filled[filled$Date == date,-1]),colnames(filled[,-1]))
colnames(daily_cases) <- c("dailyDeaths","parish_list")
daily_cases$dailyDeaths <- daily_cases$dailyDeaths/(population_specified$Population*1000)
daily_cases <- daily_cases[match(sestiere_df$Parish, daily_cases$parish_list),]
sestiere_df_daily <- cbind(sestiere_df,daily_cases$dailyDeaths)
colnames(sestiere_df_daily) <- c("Parish", "Sestiere", "today_pop_corrected")

##CDF daily ----
sestiere_df_daily %>%
  arrange(Sestiere,today_pop_corrected) %>%
  ggplot(aes(y=factor(Parish,level=Parish[order(Sestiere,today_pop_corrected)]),x=today_pop_corrected,fill=Sestiere))+
  geom_bar(stat="identity")+
  ggtitle(paste("Daily Prevalence for",format(as.Date(date,origin="1970-01-01")))) +
  labs(y="Parish",x="Deaths Today/Population")

#colored versions of the pdf split up by sestiere? cumulative and not cumulative? need to make proportional to number of people in that parish (according to filled, so S. Croce would have to be scaled up to accommodate). does same go for CDF too?
##PDF daily ----
sestiere_df_daily %>%
  ggplot(aes(x=today_pop_corrected,y=after_stat(density),fill=Sestiere))+
  #geom_histogram(bins=50,alpha=0.5)+
  geom_density(aes(x=today_pop_corrected,group=Sestiere,color=Sestiere,fill=Sestiere),alpha=0.25)+
  #what should the conventional axis limits be?
  labs(x="Daily Deaths/Population",y="Density of Parishes")+
  ggtitle(paste("Daily PDF for",format(date)))

#CEs and KLs by the sestiere

##Sestiere CE ----

gaussianCE_v2 <- function(case_vec,pop_spec){
  population_corrected <- case_vec/(pop_spec$Population*1000)
  m <- mean(population_corrected) #for some reason, the peak isn't matched up on Dec 11, 1631 (0.4 on ggplot, near 0.3 on hist). May have something to do with expanding the axes, because on both they're on the same part of the axes, the axes are just scaled differently.
  s <- sd(population_corrected)
  x <- seq(0,max(population_corrected),length.out=length(population_corrected)) #n. of bins being same as n. of parishes is arbitrary. but it does come with the nice quirk that every bin can be filled once.
  y <- dnorm(x,mean=m,sd=s)/sum(dnorm(x,mean=m,sd=s)) #do we need to give it room to breathe in the negative x since sometimes it is really close to 0 so some part of the distribution lives in the -x part?
  daily_counts <- hist(population_corrected,breaks=seq(0,max(population_corrected),length.out=length(population_corrected)+1),plot=FALSE)$counts/sum(hist(population_corrected,breaks=seq(0,max(population_corrected),length.out=length(population_corrected)+1),plot=FALSE)$counts)
  if(is.nan(-1*sum(daily_counts*log(y)))){
    return(0) #unfortunately, this happens a lot.
  }
  return(-1*sum(daily_counts*log(y)))
}

uniformCE_v2 <- function(case_vec,pop_spec){
  CV_norm <- case_vec/(pop_spec$Population*1000)
  CV_norm <- CV_norm/sum(CV_norm)
  CV_norm <- CV_norm[which(CV_norm!=0)]#when you don't count the zeroes it becomes interesting, otherwise it's literally a straight line
  uniform_dist <- rep(c(1/length(CV_norm)),length(CV_norm)) 
  summand <- 0
  for(i in 1:length(CV_norm)){
    summand <- summand+CV_norm[i]*log(uniform_dist[i]) #weird that for normal we're doing the histogram but for uniform we're not... seems wrong
  }
  if(length(CV_norm) == 0){
    return(0)
  }
  return(-1*summand)
}

CE_sestiere_df <- as.data.frame(matrix(nrow=length(filled$Date)*length(unique(sestiere_df$Sestiere)),ncol=5))
colnames(CE_sestiere_df) <- c("Sestiere","Gaussian_Cumulative","Gaussian_Daily","Uniform_Cumulative","Uniform_Daily")
dateless_filled <- filled[,-1]
for(date in unique(filled$Date)){
  for(i in 1:length(unique(sestiere_df$Sestiere))){
    CE_sestiere_df[6*(match(date,filled$Date)-1)+i,1] <- unique(sestiere_df$Sestiere)[i]
    CE_sestiere_df[6*(match(date,filled$Date)-1)+i,2] <- gaussianCE_v2(colSums(dateless_filled[1:match(date,filled$Date), which(colnames(dateless_filled) %in% sestiere_df$Parish[which(sestiere_df$Sestiere==unique(sestiere_df$Sestiere)[i])])]),population_specified[population_specified$Parish %in% sestiere_df$Parish[which(sestiere_df$Sestiere==unique(sestiere_df$Sestiere)[i])],])
    CE_sestiere_df[6*(match(date,filled$Date)-1)+i,3] <- gaussianCE_v2(as.numeric(dateless_filled[match(date,filled$Date), which(colnames(dateless_filled) %in% sestiere_df$Parish[which(sestiere_df$Sestiere==unique(sestiere_df$Sestiere)[i])])]),population_specified[population_specified$Parish %in% sestiere_df$Parish[which(sestiere_df$Sestiere==unique(sestiere_df$Sestiere)[i])],])
    CE_sestiere_df[6*(match(date,filled$Date)-1)+i,4] <- uniformCE_v2(colSums(dateless_filled[1:match(date,filled$Date), which(colnames(dateless_filled) %in% sestiere_df$Parish[which(sestiere_df$Sestiere==unique(sestiere_df$Sestiere)[i])])]),population_specified[population_specified$Parish %in% sestiere_df$Parish[which(sestiere_df$Sestiere==unique(sestiere_df$Sestiere)[i])],])
    CE_sestiere_df[6*(match(date,filled$Date)-1)+i,5] <- uniformCE_v2(as.numeric(dateless_filled[match(date,filled$Date), which(colnames(dateless_filled) %in% sestiere_df$Parish[which(sestiere_df$Sestiere==unique(sestiere_df$Sestiere)[i])])]),population_specified[population_specified$Parish %in% sestiere_df$Parish[which(sestiere_df$Sestiere==unique(sestiere_df$Sestiere)[i])],])
  }
}

##Sestiere KL ----

gaussianKL_v2 <- function(case_vec,pop_spec){
  population_corrected <- case_vec/(pop_spec$Population*1000)
  m <- mean(population_corrected)
  s <- sd(population_corrected)
  x <- seq(0,max(population_corrected),length.out=length(population_corrected))
  y <- dnorm(x,mean=m,sd=s)/sum(dnorm(x,mean=m,sd=s))
  daily_counts <- hist(population_corrected,breaks=seq(0,max(population_corrected),length.out=length(population_corrected)+1),plot=FALSE)$counts/sum(hist(population_corrected,breaks=seq(0,max(population_corrected),length.out=length(population_corrected)+1),plot=FALSE)$counts)
  return(KL(daily_counts,y))
  #return(KL(population_corrected/sum(population_corrected),y))
}

gaussianKL_otherway <- function(case_vec, pop_spec){
  population_corrected <- case_vec/(pop_spec$Population*1000)
  m <- mean(population_corrected)
  s <- sd(population_corrected)
  x <- seq(0,max(population_corrected),length.out = length(population_corrected))
  y <- dnorm(x,mean=m,sd=s)/sum(dnorm(x,mean=m,sd=s))
  daily_counts <- hist(population_corrected,breaks=seq(0,max(population_corrected),length.out=length(population_corrected)+1),plot=FALSE)$counts/sum(hist(population_corrected,breaks=seq(0,max(population_corrected),length.out=length(population_corrected)+1),plot=FALSE)$counts)
  return(KL(y,daily_counts))
}

uniformKL_v2 <- function(case_vec,pop_spec){
  CV_norm <- case_vec/(pop_spec$Population*1000)
  CV_norm <- CV_norm/sum(CV_norm)
  #CV_norm <- CV_norm[which(CV_norm!=0)]#when you don't count the zeroes it becomes interesting, otherwise it's literally a straight line
  uniform_dist <- rep(c(1/length(CV_norm)),length(CV_norm)) 
  if(length(CV_norm) == 0){
    return(0)
  }
  return(KL(CV_norm,uniform_dist))
}

uniformKL_otherway <- function(case_vec,pop_spec){
  population_corrected <- case_vec/(pop_spec$Population*1000)
  CV_norm <- case_vec/(pop_spec$Population*1000)
  CV_norm <- CV_norm/sum(CV_norm)
  CV_norm <- CV_norm[which(CV_norm!=0)]
  daily_counts <- hist(population_corrected,breaks=seq(0,max(population_corrected),length.out=length(population_corrected)+1),plot=FALSE)$counts/sum(hist(population_corrected,breaks=seq(0,max(population_corrected),length.out=length(population_corrected)+1),plot=FALSE)$counts)
  uniform_dist <- rep(c(1/length(CV_norm),length(CV_norm)))
  #uniform_dist <- rep(c(1/length(daily_counts),length(daily_counts)))
  if(length(CV_norm) == 0){
    return(0)
  }
  return(KL(uniform_dist,CV_norm))
}

KL_sestiere_df <- as.data.frame(matrix(nrow=length(filled$Date)*length(unique(sestiere_df$Sestiere)),ncol=5))
colnames(KL_sestiere_df) <- c("Sestiere","Gaussian_Cumulative","Gaussian_Daily","Uniform_Cumulative","Uniform_Daily")
for(date in unique(filled$Date)){
  for(i in 1:length(unique(sestiere_df$Sestiere))){
    KL_sestiere_df[6*(match(date,filled$Date)-1)+i,1] <- unique(sestiere_df$Sestiere)[i]
    KL_sestiere_df[6*(match(date,filled$Date)-1)+i,2] <- gaussianKL_v2(colSums(dateless_filled[1:match(date,filled$Date), which(colnames(dateless_filled) %in% sestiere_df$Parish[which(sestiere_df$Sestiere==unique(sestiere_df$Sestiere)[i])])]),population_specified[population_specified$Parish %in% sestiere_df$Parish[which(sestiere_df$Sestiere==unique(sestiere_df$Sestiere)[i])],])
    KL_sestiere_df[6*(match(date,filled$Date)-1)+i,3] <- gaussianKL_v2(as.numeric(dateless_filled[match(date,filled$Date), which(colnames(dateless_filled) %in% sestiere_df$Parish[which(sestiere_df$Sestiere==unique(sestiere_df$Sestiere)[i])])]),population_specified[population_specified$Parish %in% sestiere_df$Parish[which(sestiere_df$Sestiere==unique(sestiere_df$Sestiere)[i])],])
    KL_sestiere_df[6*(match(date,filled$Date)-1)+i,4] <- uniformKL_otherway(colSums(dateless_filled[1:match(date,filled$Date), which(colnames(dateless_filled) %in% sestiere_df$Parish[which(sestiere_df$Sestiere==unique(sestiere_df$Sestiere)[i])])]),population_specified[population_specified$Parish %in% sestiere_df$Parish[which(sestiere_df$Sestiere==unique(sestiere_df$Sestiere)[i])],])
    KL_sestiere_df[6*(match(date,filled$Date)-1)+i,5] <- uniformKL_otherway(as.numeric(dateless_filled[match(date,filled$Date), which(colnames(dateless_filled) %in% sestiere_df$Parish[which(sestiere_df$Sestiere==unique(sestiere_df$Sestiere)[i])])]),population_specified[population_specified$Parish %in% sestiere_df$Parish[which(sestiere_df$Sestiere==unique(sestiere_df$Sestiere)[i])],])
  }
}

CE_sestiere_df %>%
  ggplot(aes(x=1:6570,y=Gaussian_Cumulative,col=Sestiere))+ #I don't know why 1:nrow(filled) doesn't work.
  geom_line()
CE_sestiere_df %>%
  ggplot(aes(x=1:6570,y=Gaussian_Daily,col=Sestiere))+ #I don't know why 1:nrow(filled) doesn't work.
  geom_line()
CE_sestiere_df %>%
  ggplot(aes(x=1:6570,y=Uniform_Cumulative,col=Sestiere))+ #I don't know why 1:nrow(filled) doesn't work.
  geom_line()
CE_sestiere_df %>%
  ggplot(aes(x=1:6570,y=Uniform_Daily,col=Sestiere))+ #I don't know why 1:nrow(filled) doesn't work.
  geom_line()

KL_sestiere_df %>%
  ggplot(aes(x=1:6570,y=Gaussian_Cumulative,col=Sestiere))+
  geom_line()
KL_sestiere_df %>%
  ggplot(aes(x=1:6570,y=Gaussian_Daily,col=Sestiere))+
  geom_line()
KL_sestiere_df %>%
  ggplot(aes(x=1:6570,y=Uniform_Cumulative,col=Sestiere))+
  geom_line()
KL_sestiere_df %>%
  ggplot(aes(x=1:6570,y=Uniform_Daily,col=Sestiere))+
  geom_line()

gaussian_CE_df <- as.data.frame(matrix(nrow=length(filled$Date),ncol=3))
colnames(gaussian_CE_df) <- c("Date","Cumulative", "Daily")
for(date in unique(filled$Date)){
  gaussian_CE_df[match(date,filled$Date),2] <- gaussianCE_v2(colSums(dateless_filled[1:match(date,filled$Date),]),population_specified)
  gaussian_CE_df[match(date,filled$Date),3] <- gaussianCE_v2(as.numeric(dateless_filled[match(date,filled$Date),]),population_specified)
}
gaussian_CE_df[,1] <- filled$Date

gaussian_CE_df %>% 
  ggplot(aes(x=Date,y=Cumulative)) +
  geom_line(color="#8185eb")+
  scale_x_date(date_breaks = "3 months",date_labels="%b%Y")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle=60,hjust=1))
gaussian_CE_df %>% 
  ggplot(aes(x=Date,y=Daily)) +
  geom_line()

#Misc ----

##Quantile Function ----
cumulative_df %>%
  mutate(population_corrected = cumulativeDeaths/(population_specified$Population*1000)) %>%
  arrange(population_corrected) %>%
  ggplot(aes(x=factor(bcParishes,level=bcParishes[order(population_corrected)]),y=population_corrected))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle=60,hjust=1))+
  geom_bar(stat="identity")+
  ggtitle(paste("Cumulative up to",bcDateChar)) +
  labs(x="Parish",y="Cumulative Deaths/Population")

axis_labels <- c("0",rep("",12),"0.25",rep("",12),"0.5",rep("",12),"0.75",rep("",12),"1")

##Daily Deaths ----

bc_df %>%
  mutate(population_corrected = bcDeaths/(population_specified$Population*1000)) %>%
  arrange(population_corrected) %>%
  ggplot(aes(y=factor(bcParishes,level=bcParishes[order(population_corrected)]),x=population_corrected)) +
  geom_col() +
  scale_y_discrete(labels=axis_labels)+
  ggtitle(bcDateChar) +
  labs(y="Probability",x="Deaths/Population")

##Cumulative cases ----

cumulative_df %>%
  mutate(population_corrected = cumulativeDeaths/(population_specified$Population*1000)) %>%
  arrange(population_corrected) %>%
  ggplot(aes(y=factor(bcParishes,level=bcParishes[order(population_corrected)]),x=population_corrected))+
  geom_col()+
  scale_y_discrete(labels=axis_labels)+
  ggtitle(paste("Cumulative up to",bcDateChar)) +
  labs(y="Probability",x="Cumulative Deaths/Population")

##Entropy Comparisons ----

CE_df_long <- CE_df %>%
  mutate(Sum_Scaled = Sum/(max(Sum)/max(Exponential))) %>%
  pivot_longer(c(Sum_Scaled,Shannon,Uniform,Exponential,Normal),names_to="Type",values_to="Comparison")
CE_df_long %>%
  ggplot(aes(x=1:(1095*5),y=Comparison,col=Type))+
  geom_line()
CE_df_long <- CE_df %>%
  mutate(Sum_Scaled = Sum/(max(Sum)/max(abs(Shannon)))) %>%
  pivot_longer(c(Sum_Scaled,Shannon),names_to = "Type", values_to = "Comparison")
CE_df_long %>%
  ggplot(aes(x=1:(1095*2),y=Comparison,col=Type))+
  geom_line()

##Cumulative Cases Entropy ----

cumulative_cases_shannon_percase <- c()
cumulative_cases_shannon_notpercase <- c()
daily_cases_notpercase <- c()
daily_cases_percase <- c()
for(date in filled$Date){
  cumulative_cases_shannon_percase <- c(cumulative_cases_shannon_percase,per_case_shannon_2(as.numeric(colSums(filled[1:match(date,filled$Date),-1])),population,parish_names)) #lol when I use per case shannon 2 it looks like what ni log pi should look like (logistic growth), but the ni log pi looks weirder
  cumulative_cases_shannon_notpercase <- c(cumulative_cases_shannon_notpercase,not_per_case_shannon(as.numeric(colSums(filled[1:match(date,filled$Date),-1])),population,parish_names))
  daily_cases_notpercase <- c(daily_cases_notpercase, not_per_case_shannon(as.numeric(filled[match(date,filled$Date),-1]),population,parish_names))
  daily_cases_percase <- c(daily_cases_percase,per_case_shannon_2(as.numeric(filled[match(date,filled$Date),-1]),population,parish_names))
}
plot.ts(cumulative_cases_shannon_percase)
plot.ts(cumulative_cases_shannon_notpercase)
plot.ts(cumsum(daily_cases_notpercase)) #if you sum up the cases and take the ni log pi entropy of that sum up or if you take the entropy of ni log pi for new cases everyday and sum up all those entropies you end up with almost identical graphs. Cool!
plot.ts(daily_cases_notpercase)
plot.ts(daily_cases_percase)
plot.ts(cumsum(daily_cases_percase)) #if you sum up the cases and take the pi log pi entropy of that, it looks like 1/Temp. However, if you sum up all the individual pi log pi entropies of every day it looks different--like a straight line.