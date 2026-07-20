library(msos)
library(entropy)
library(simboot)
library(MCPAN)
library(gridExtra)
library(cowplot)
library(ggpubr)

KullbackLeibler <- function(prob1,prob2){
  no_zeroes <- which(prob1 != 0 & prob2 != 0)
  prob1 <- prob1[no_zeroes]
  prob2 <- prob2[no_zeroes]
  # print(length(prob1))
  # print(length(prob2))
  if(-1*sum(prob1*log(prob2/prob1)) < 0){
    print(prob1)
    print(prob2)
  }
  return(-1*sum(prob1*log(prob2/prob1)))
}

gaussianKL_v2_test <- function(case_vec,pop_spec){
  population_corrected <- case_vec/(pop_spec$Population*1000)
  m <- mean(population_corrected)
  s <- sd(population_corrected)
  x <- seq(0,max(population_corrected),length.out=length(population_corrected))
  y <- dnorm(x,mean=m,sd=s)/sum(dnorm(x,mean=m,sd=s))
  daily_counts <- hist(population_corrected,breaks=seq(0,max(population_corrected),length.out=length(population_corrected)+1),plot=FALSE)$counts/sum(hist(population_corrected,breaks=seq(0,max(population_corrected),length.out=length(population_corrected)+1),plot=FALSE)$counts)
  return(KullbackLeibler(daily_counts,y))
}

gaussianKL_otherway_test <- function(case_vec, pop_spec){
  population_corrected <- case_vec/(pop_spec$Population*1000)
  m <- mean(population_corrected)
  s <- sd(population_corrected)
  x <- seq(0,max(population_corrected),length.out = length(population_corrected))
  y <- dnorm(x,mean=m,sd=s)/sum(dnorm(x,mean=m,sd=s))
  daily_counts <- hist(population_corrected,breaks=seq(0,max(population_corrected),length.out=length(population_corrected)+1),plot=FALSE)$counts/sum(hist(population_corrected,breaks=seq(0,max(population_corrected),length.out=length(population_corrected)+1),plot=FALSE)$counts)
  return(KullbackLeibler(y,daily_counts))
}

uniformKL_v2_test <- function(case_vec,pop_spec){
  CV_norm <- case_vec/(pop_spec$Population*1000)
  CV_norm <- CV_norm/sum(CV_norm)
  CV_norm <- CV_norm[which(CV_norm!=0)]#when you don't count the zeroes it becomes interesting, otherwise it's literally a straight line
  uniform_dist <- rep(c(1/length(CV_norm)),length(CV_norm)) 
  if(length(CV_norm) == 0){
    return(0)
  }
  return(KullbackLeibler(CV_norm,uniform_dist))
}

uniformKL_otherway_test <- function(case_vec,pop_spec){
  population_corrected <- case_vec/(pop_spec$Population*1000)
  CV_norm <- case_vec/(pop_spec$Population*1000)
  CV_norm <- CV_norm/sum(CV_norm)
  CV_norm <- CV_norm[which(CV_norm!=0)]
  daily_counts <- hist(population_corrected,breaks=seq(0,max(population_corrected),length.out=length(population_corrected)+1),plot=FALSE)$counts/sum(hist(population_corrected,breaks=seq(0,max(population_corrected),length.out=length(population_corrected)+1),plot=FALSE)$counts)
  uniform_dist <- rep(c(1/length(CV_norm),length(CV_norm)))
  if(length(CV_norm) == 0){
    return(0)
  }
  return(KullbackLeibler(uniform_dist,CV_norm))
}

KL_sestiere_df_test <- as.data.frame(matrix(nrow=length(filled$Date)*length(unique(sestiere_df$Sestiere)),ncol=5))
colnames(KL_sestiere_df_test) <- c("Sestiere","Gaussian_Cumulative","Gaussian_Daily","Uniform_Cumulative","Uniform_Daily")
for(date in unique(filled$Date)){
  for(i in 1:length(unique(sestiere_df$Sestiere))){
    KL_sestiere_df_test[6*(match(date,filled$Date)-1)+i,1] <- unique(sestiere_df$Sestiere)[i]
    KL_sestiere_df_test[6*(match(date,filled$Date)-1)+i,2] <- gaussianKL_v2_test(colSums(dateless_filled[1:match(date,filled$Date), which(colnames(dateless_filled) %in% sestiere_df$Parish[which(sestiere_df$Sestiere==unique(sestiere_df$Sestiere)[i])])]),population_specified[population_specified$Parish %in% sestiere_df$Parish[which(sestiere_df$Sestiere==unique(sestiere_df$Sestiere)[i])],])
    KL_sestiere_df_test[6*(match(date,filled$Date)-1)+i,3] <- gaussianKL_v2_test(as.numeric(dateless_filled[match(date,filled$Date), which(colnames(dateless_filled) %in% sestiere_df$Parish[which(sestiere_df$Sestiere==unique(sestiere_df$Sestiere)[i])])]),population_specified[population_specified$Parish %in% sestiere_df$Parish[which(sestiere_df$Sestiere==unique(sestiere_df$Sestiere)[i])],])
    KL_sestiere_df_test[6*(match(date,filled$Date)-1)+i,4] <- uniformKL_v2_test(colSums(dateless_filled[1:match(date,filled$Date), which(colnames(dateless_filled) %in% sestiere_df$Parish[which(sestiere_df$Sestiere==unique(sestiere_df$Sestiere)[i])])]),population_specified[population_specified$Parish %in% sestiere_df$Parish[which(sestiere_df$Sestiere==unique(sestiere_df$Sestiere)[i])],])
    KL_sestiere_df_test[6*(match(date,filled$Date)-1)+i,5] <- uniformKL_v2_test(as.numeric(dateless_filled[match(date,filled$Date), which(colnames(dateless_filled) %in% sestiere_df$Parish[which(sestiere_df$Sestiere==unique(sestiere_df$Sestiere)[i])])]),population_specified[population_specified$Parish %in% sestiere_df$Parish[which(sestiere_df$Sestiere==unique(sestiere_df$Sestiere)[i])],])
  }
}

KL_sestiere_df_test %>%
  ggplot(aes(x=1:6570,y=Gaussian_Cumulative,col=Sestiere))+
  geom_line()
KL_sestiere_df_test %>%
  ggplot(aes(x=1:6570,y=Gaussian_Daily,col=Sestiere))+
  geom_line()

KL_sestiere_df_test %>%
  ggplot(aes(x=1:6570,y=Uniform_Cumulative,col=Sestiere))+
  geom_line()
KL_sestiere_df_test %>%
  ggplot(aes(x=1:6570,y=Uniform_Daily,col=Sestiere))+
  geom_line()

#Properly updating populations

weekly_filled <- as.data.frame(matrix(nrow=length(unique(week(filled$Date)))*length(unique(year(filled$Date))),ncol=ncol(filled))) #thankfully since 1629-1631 aren't leap years don't have to worry about that.
for(year in unique(year(filled$Date))){
  filledOneYear <- filled[year == year(filled$Date),]
  for(week in unique(week(filled$Date))){
    filledOneWeek <- filledOneYear[week == week(filledOneYear$Date),]
    weeklySum <- as.numeric(colSums(filledOneWeek[,-1]))
    thisMonday <- filledOneWeek$Date[1]
    weekly_filled[53*(match(year,unique(year(filled$Date)))-1)+match(week,unique(week(filled$Date))),1] <- format(thisMonday,origin="1970-01-01")
    weekly_filled[53*(match(year,unique(year(filled$Date)))-1)+match(week,unique(week(filled$Date))),2:ncol(weekly_filled)] <- weeklySum
  }
}
colnames(weekly_filled) <- colnames(filled)
colnames(weekly_filled)[1] <- "Week_of"
weekly_filled$Week_of <- as.Date(weekly_filled$Week_of)

#Entropy of the PDF?

weekly_negent <- c()
weekly_gaussian_CE <- c()
weekly_shannon <- c()
weekly_shannon_notpercase <- c()
weekly_uniform_KL <- c()
weekly_uniform_CE <- c()
for(date in weekly_filled$Week_of){
  weekly_case_vector <- as.numeric(weekly_filled[match(date,weekly_filled$Week_of),-1])
  weekly_negent <- c(weekly_negent,negent(weekly_case_vector/(population_specified$Population*1000))) #KL
  weekly_gaussian_CE <- c(weekly_gaussian_CE, gaussianCE_v2(weekly_case_vector,population_specified))
  weekly_shannon <- c(weekly_shannon,per_case_shannon_2(weekly_case_vector,population,colnames(filled[,-1]))) #entropy
  weekly_shannon_notpercase <- c(weekly_shannon_notpercase,not_per_case_shannon(weekly_case_vector,population,colnames(filled[,-1])))
  weekly_uniform_KL <- c(weekly_uniform_KL,uniformKL_v2(weekly_case_vector,population_specified)) #KL
  weekly_uniform_CE <- c(weekly_uniform_CE,uniformCE_v2(weekly_case_vector,population_specified))
}
plot.ts(weekly_shannon_notpercase)
plot.ts(weekly_shannon)
plot.ts(weekly_shannon,ylim=c(0,4)) #gaussian
lines(weekly_negent,col="red")
lines(weekly_uniform_KL,col="blue")

gaussianKL_BC <- function(case_vec,pop_spec){
  population_corrected <- case_vec/(pop_spec$Population*1000)
  m <- mean(population_corrected)
  s <- sd(population_corrected)
  x <- seq(0,max(population_corrected),length.out=length(population_corrected))
  y <- dnorm(x,mean=m,sd=s)/sum(dnorm(x,mean=m,sd=s))
  daily_counts <- hist(population_corrected,breaks=seq(0,max(population_corrected),length.out=length(population_corrected)+1),plot=FALSE)$counts/sum(hist(population_corrected,breaks=seq(0,max(population_corrected),length.out=length(population_corrected)+1),plot=FALSE)$counts)
  non_zeroes <- which(daily_counts != 0)
  daily_counts <- daily_counts[non_zeroes]
  y <- y[non_zeroes] #because you would be multiplying by 0 anyways
  return(-1*sum(daily_counts*log(y/daily_counts))) #should it be bias corrected for y and daily_counts? so since it's in the numerator and denominator, it cancels out and doesn't come into play here?
}

gaussianCE_BC <- function(case_vec,pop_spec){
  population_corrected <- case_vec/(pop_spec$Population*1000)
  m <- mean(population_corrected)
  s <- sd(population_corrected)
  x <- seq(0,max(population_corrected),length.out=length(population_corrected))
  y <- dnorm(x,mean=m,sd=s)/sum(dnorm(x,mean=m,sd=s))
  daily_counts <- hist(population_corrected,breaks=seq(0,max(population_corrected),length.out=length(population_corrected)+1),plot=FALSE)$counts/sum(hist(population_corrected,breaks=seq(0,max(population_corrected),length.out=length(population_corrected)+1),plot=FALSE)$counts)
  if(is.nan(-1*sum(daily_counts*log(y)))){
    return(0)
  }
  return(-1*sum(daily_counts*log(y*(max(population_corrected)/length(population_corrected)))))
}

cumulative_negent <- c()
cumulative_uniform_KL <- c()
cumulative_gaussiance <- c()
cumulative_uniformce <- c()

for(date in filled$Date){
  cumulative_pop_corrected <- as.numeric(colSums(filled[1:match(date,filled$Date),-1]))/(population_specified$Population*1000)
  cumulative_negent <- c(cumulative_negent,gaussianKL_BC(as.numeric(colSums(filled[1:match(date,filled$Date),-1])),population_specified)) #negent looks a little different than the gaussianKL_v2 version, but doesn't it use way fewer bins?
  cumulative_uniform_KL <- c(cumulative_uniform_KL,uniformKL_v2(as.numeric(colSums(filled[1:match(date,filled$Date),-1])),population_specified)) #when I did uniformKL_v2(cumulative_pop_corrected,population_specified) it looks like a vertical shift of the negent
  cumulative_gaussiance <- c(cumulative_gaussiance,gaussianCE_BC(as.numeric(colSums(filled[1:match(date,filled$Date),-1])),population_specified))
  cumulative_uniformce <- c(cumulative_uniformce,uniformCE_v2(as.numeric(colSums(filled[1:match(date,filled$Date),-1])),population_specified))
}
plot.ts(cumulative_uniform_KL,ylim=c(0,2.5),col="red")
legend(125,1.75,legend=c("Uniform KL","Normal KL"),col=c("red","blue"),lty=1)
lines(cumulative_negent,col="blue")

plot.ts(cumulative_gaussiance)
plot.ts(cumulative_uniformce) #why is KL looking like a flipped version of CE when it should look like shannon - CE?

plot.ts(cumulative_gaussiance - cumulative_negent) #when you remove outliers it looks a LOT closer to the cumulative shannon. I guess cant expect to look the exact same because of that one outlier before the outbreak and also because we're doing histogram method
plot.ts(cumulative_uniformce - cumulative_uniform_KL)
plot.ts(cumulative_cases_shannon_percase)
plot.ts(cumulative_negent)
plot.ts(cumulative_uniform_KL)


#Entropy of the CDF?
CDF_UniEnt <- function(cv){
  cdf <- seq(0,max(cv),length.out=length(cv)) #or is max 1? but this is wrong, because they're not evenly spaced
  cdf <- cdf/sum(cdf) #normalize for the KL, but not sure this makes sense philosophically
  ce <- 0
  for(i in 1:length(cdf)){
    if(cdf[i] == 0){
      next
    }
    ce <- ce + cv[i]*log(cdf[i])
  }
  return(c(-1*ce,KL(cv,cdf)))
}

CDF_NormEnt <- function(cv){
  x <- seq(0,max(cv),length.out=length(cv))
  cdf <- pnorm(x,mean=mean(cv),sd=sd(cv))
  cdf <- cdf/sum(cdf) #seems wrong to do this, makes norm and uni suddenly incredibly similar
  ce <- 0
  for(i in 1:length(cdf)){
    if(cdf[i] == 0){
      next
    }
    ce <- ce + cv[i]*log(cdf[i])
  }
  return(c(-1*ce,KL(cv,cdf)))
}

CDF_df <- as.data.frame(matrix(nrow=53*3,ncol=2))

for(date in weekly_filled$Week_of){
  WCV <- sort(as.numeric(weekly_filled[match(date,weekly_filled$Week_of),-1])/(population_specified$Population*1000))
  CDF_df[match(date,weekly_filled$Week_of),1] <- CDF_UniEnt(WCV/sum(WCV))[1]
  CDF_df[match(date,weekly_filled$Week_of),2] <- CDF_NormEnt(WCV/sum(WCV))[1]
}
# plot.ts(CDF_df[,1]) #supposedly, shannon + KL = cross, but these graphs don't show that to be the case. 
# plot.ts(CDF_df[,2])

#All the methods for Shannon estimation in entropy library with entropy function: ("ML", "MM", "Jeffreys", "Laplace", "SG", "minimax", "CS", "NSB", "shrink")

#Compare the two
plot.ts(CDF_df[,1])
lines(weekly_uniform_CE,col="red")
plot.ts(CDF_df[,2])
lines(weekly_gaussian_CE,col="red")

#Combination?
plot.ts(CDF_df[,2]+weekly_gaussian_CE,ylim=c(5.5,8),col="blue")
lines(CDF_df[,1]+weekly_uniform_CE,col="red")
legend(0,6,legend=c("Gaussian CE","Uniform CE"),col=c("blue","red"),lty=1)

for(date in weekly_filled$Week_of){
  WCV <- sort(as.numeric(weekly_filled[match(date,weekly_filled$Week_of),-1])/(population_specified$Population*1000))
  CDF_df[match(date,weekly_filled$Week_of),1] <- CDF_UniEnt(WCV/sum(WCV))[2]
  CDF_df[match(date,weekly_filled$Week_of),2] <- CDF_NormEnt(WCV/sum(WCV))[2]
}

plot.ts(CDF_df[,1])
lines(weekly_uniform_KL,col="red")
plot.ts(CDF_df[,2])
lines(weekly_negent,col="red")

plot.ts(CDF_df[,2]+weekly_negent,col="blue")
lines(CDF_df[,1]+weekly_uniform_KL,col="red")
legend(10,2.5,legend=c("Gaussian KL","Uniform KL"),col=c("blue","red"),lty=1)

population_specified_test <- population_specified

for(date in filled$Date){
  population_specified_test$Population <- population_specified_test$Population - as.integer(filled[date == filled$Date,-1])
}

plot.ts(cumsum(rowSums(filled[,-1])))

filled_popcorrected <- as.data.frame(matrix(nrow=nrow(filled),ncol=ncol(filled[,-1])))
for(date in filled$Date){
  filled_popcorrected[match(date,filled$Date),] <- filled[match(date,filled$Date),-1]/(population_specified$Population*1000)
}
colnames(filled_popcorrected) <- colnames(filled[,-1])
plot.ts(cumsum(rowSums(filled_popcorrected)/ncol(filled[,-1]))) #average cumulative incidence rate

plot(1,type="n", xlab="", ylab="",xlim=c(0,1095),ylim=c(0,max(colSums(filled_popcorrected))))
for(parish in colnames(filled_popcorrected)){
  lines(x=1:1095,y=cumsum(filled_popcorrected[,match(parish,colnames(filled_popcorrected))]),lwd=0.5,col=rgb(red=0,green=0,blue=0,alpha=0.5))
} #outlier is S.Marco, with a much smaller than average population and little growth in # of cases during the outbreak

plot(1,type="n", xlab="", ylab="",xlim=c(0,nrow(weekly_filled)),ylim=c(0,max(weekly_filled[,-1])))
for(parish in colnames(weekly_filled[,-1])){
  lines(x=1:nrow(weekly_filled),y=weekly_filled[,match(parish,colnames(weekly_filled))],lwd=0.5,col=rgb(red=0,green=0,blue=0,alpha=0.5))
}

#Bias Adjustment on entropy estimates of the PMF
shannon_histogram <- function(case_vector,pop_spec){
  PC <- case_vector/(pop_spec$Population*1000)
  PDF <- hist(PC,breaks=seq(0,max(PC),length.out=length(PC)+1),plot=F)$counts/sum(hist(PC,breaks=seq(0,max(PC),length.out=length(PC)),plot=F)$counts) #should the max be max(PC), or something fixed?
  s <- 0
  for(i in 1:length(PDF)){
    if(PDF[i] == 0){
      next
    }
    s <- s + PDF[i]*log(PDF[i]/(max(PC)/length(PC))) #or + log(max(PC)/length(PC)) or *max(PC)/length(PC)? Not sure which
  }
  return(-1*s)
}

shannon_hist_weekly <- c()
shannon_hist_cumulative <- c()
for(week in weekly_filled$Week_of){
  shannon_hist_weekly <- c(shannon_hist_weekly, shannon_histogram(as.numeric(weekly_filled[match(week,weekly_filled$Week_of),-1]),population_specified))
  shannon_hist_cumulative <- c(shannon_hist_cumulative,shannon_histogram(as.numeric(colSums(weekly_filled[1:match(week,weekly_filled$Week_of),-1])),population_specified))
}
plot.ts(shannon_hist_weekly)
plot.ts(shannon_hist_cumulative)

BC <- c()
for(date in filled$Date){
  BC <- c(BC,shannon_histogram(as.numeric(filled[match(date,filled$Date),-1]),population_specified))
}
plot.ts(BC)

#Smooth
t <- c()
v <- c()
v_c <- c()
w <- c()
w_c <- c()
s <- c()
inst_s <- c()
sum_cv <- c()
for(i in 1:length(filled$Date)){
  instant_cv <- as.numeric(filled_popcorrected[i,])
  cv <- as.numeric(colSums(filled_popcorrected[1:i,])) #a lot of noise (?) when you try to do non-cumulative, even for weekly data
  sum_cv <- c(sum_cv,sum(cv))
  x <- seq(0,max(cv),length.out=512)
  y <- dnorm(x,mean=mean(cv),sd=sd(cv))
  y <- y/sum(y) #never has zeroes so dont have to worry about that (i wonder why)
  u <- rep(1/512,512)
  u2 <- rep(0,512)
  u2[match(max(y),y)] <- 1
  # if((i %% 31) == 0){
  #   print(ggplot(as.data.frame(rep(0,53)),aes(x=seq(0,max(cv),length.out=length(cv)+1)[-1],y=hist(cv,breaks=seq(0,max(cv),length.out=length(cv)+1),plot=F)$counts/sum(hist(cv,breaks=seq(0,max(cv),length.out=length(cv)+1),plot=F)$counts)))+geom_col())
  #   plot(x,density(cv,n=512,from=0,to=max(cv),kernel="epanechnikov")$y/sum(density(cv,n=512,from=0,to=max(cv),kernel="epanechnikov")$y),type="l",main=filled$Date[i]) #,xlim=c(0,0.7),ylim=c(0,0.0065) #Triangular SJ works well to accurately depict what is shown in the histograms at least
  #   lines(x,y,col="red")
  #   # plot(x,y,type="l")
  #   # plot(x,u2,type="l")
  #   # plot(x,u,type="l")
  # }
  #kern <- density(cv,n=512,from=0+max(cv)/(512*2),to=max(cv)-max(cv)/(512*2))$y/sum(density(cv,n=512,from=0+max(cv)/(512*2),to=max(cv)-max(cv)/(512*2))$y)
  inst_kern <- density(instant_cv,n=512,from=0,to=max(instant_cv))$y/sum(density(instant_cv,n=512,from=0,to=max(instant_cv))$y)
  #kern <- density(cv,n=512,from=0,to=max(cv))$y/sum(density(cv,n=512,from=0,to=max(cv))$y)
  kern <- density(cv,n=512,from=0,to=max(cv),kernel="epanechnikov")$y/sum(density(cv,n=512,from=0,to=max(cv),kernel="epanechnikov")$y)
  kern_nz <- which(kern != 0)
  #v <- c(v,KL(kern,y))
  v <- c(v,-1*sum(kern[kern_nz]*log(y[kern_nz]/(kern[kern_nz]/(max(cv)/512)))))
  #v_c <- c(v_c,-1*sum(kern*log(y/max(cv)))+1)
  v_c <- c(v_c,-1*sum(kern*log(y)))
  w <- c(w,KL(kern,u))
  #w_c <- c(w_c,-1*sum(kern*log(u/max(cv)))+1)
  w_c <- c(w_c,-1*sum(kern*log(u)))
  nz <- which(u2 != 0)
  t <- c(t,KL(kern[nz],u2[nz]))
  inst_s <- c(inst_s,shannon_entropy_real(inst_kern))
  #s <- c(s,-1*sum(kern[kern_nz]*log(kern[kern_nz]/max(cv)))+1) #bias correction? #when you do ni log pi it looks like the logarithmic growth that stops at around Dec 1629 and doesn't peak again. When you do offset of max(cv) + 1 you get the same shape as shannon_entropy_real. And the expected offset should be max(cv)/512 (bin width).
  #s <- c(s,shannon_entropy_real(kern)) #the normal one that isn't bias corrected
  s <- c(s, -1*sum(kern[kern_nz]*log(kern[kern_nz]/(max(cv)/512)))) #standard bias correction from bins (although that's for histogram approximation, not sure if it works the same here)
  #s <- c(s, -1*sum(kern[kern_nz]*log(kern[kern_nz]*512))) #KL with uniform and histogram bias correction
  #s <- c(s,-1*sum(kern[kern_nz]*log(kern[kern_nz]*max(cv)))) #just KL with uniform
  #s <- c(s,-1*sum(kern[kern_nz]*log(kern[kern_nz]/((max(cv)-min(cv))/(512-min(cv)/(max(cv)/512)))))) #assuming binwidth is (max(cv)-min(cv))/N where N = # of bins, but then with the modification of the number of bins it's taken up to that point. This doesn't really make that much sense I just wanted to try it.
}

plot(x=sum_cv,y=v,type="l")
plot(x=sum_cv,y=s,type="l")

#Put bootstrap through same routine as the observed data. so bootstrap parishes at the start and do the routine again

plot.ts(inst_s)
# time_seq <- 1:1095
# kern_plot <- as.data.frame(cbind(time_seq,s,v,w,v_c,w_c,t))
# kern_plot %>% ggplot(aes(x=filled$Date,y=s))+
#   geom_line()+
#   scale_x_date(date_breaks="6 months",date_labels="%b %Y")+
#   labs(x="Date",y="H",title="Shannon Entropy of Density Estimate")+
#   theme_minimal()
plot.ts(s,ylim=c(0,6.3))
lines(filled_sum/(max(filled_sum)/6))

# kern_plot_grouped <- kern_plot %>%
#   pivot_longer(c(v,w),names_to="comp",values_to="Kullback_Leibler") %>%
#   mutate(Compared_Distribution = gsub("v","Normal",gsub("w","Uniform",comp)))
# kern_plot_grouped %>% ggplot(aes(x=sort(rep(filled$Date,2)),y=Kullback_Leibler,col=Compared_Distribution))+
#   geom_line()+
#   ylim(0,2.5)+
#   scale_x_date(date_breaks="6 months",date_labels="%b %Y")+
#   labs(x="Date",y="H",title="Kullback Leibler Divergence with Density Estimate",color="Compared Distribution")+
#   theme_minimal()

plot.ts(v)
plot.ts(v,ylim=c(0,2.5))
shannon_pdf_plot <- ggplot(as.data.frame(s),aes(x=filled$Date,y=s))+
  geom_line()+
  labs(x="Date",y="H",title="Shannon Entropy of Kernel Density Estimated PDF")+
  scale_x_date(date_breaks="6 months",date_labels="%b %Y")
ggsave("Kernel PDF Shannon.png",width=2560,height=1600,units="px")
KL_pdf_plot <- ggplot(as.data.frame(v),aes(x=filled$Date,y=v))+
  scale_y_continuous()+
  expand_limits(y=c(0,2.5))+
  geom_line(aes(col="normal"))+
  geom_line(aes(y=w,col="uniform"))+
  scale_color_manual(values=c(normal="black",uniform="red"),labels=c(normal="Normal",uniform="Uniform"),limits=c("uniform","normal"))+
  labs(x="Date",y="H",title="KL Divergence with Cumulative Kernel Density Estimated PDF",col="Distribution")+
  scale_x_date(date_breaks="6 months",date_labels="%b %Y")
ggsave("Kernel PDF KL.png",width=2560,height=1600,units="px")
kernel_pdf_plots <- ggarrange(shannon_pdf_plot,KL_pdf_plot,ncol=2,nrow=1) #to add labels like A and B do,labels=c("A","B")
kernel_pdf_plots <- annotate_figure(kernel_pdf_plots,top=text_grob("Entropy of Cumulative Kernel Density Estimated PDF",face="bold",size=14,col="red"))
ggsave("Kernel PDF Entropy.png",width=2100,height=1080,units="px")

ggplot(as.data.frame(v_c),aes(x=filled$Date,y=v_c))+ #mostly redundant because shannon + KL = cross. If include then make the y limits 0 to 6.5 for all graphs so you can see how the normal KL affects the shannon for the CE. Also thought about plotting all but would be so cluttered.
  scale_y_continuous()+
  expand_limits(y=c(3.5,6.5))+
  geom_line(aes(col="normal"))+
  geom_line(y=w_c,aes(col="uniform"))+
  scale_color_manual(values=c(normal="black",uniform="red"),labels=c(normal="Normal",uniform="Uniform"),limits=c("uniform","normal"))+
  labs(x="Date",y="H",title="Cross Entropy",color="Distribution")+
  scale_x_date(date_breaks="6 months",date_labels="%b %Y")
lines(w,col="red")
plot.ts(s+v)
plot.ts(s+w,col="red") #not the same because we got rid of zeroes for KL to work
plot.ts(v_c)
plot.ts(w_c,col="red")
plot.ts(t) #more than anything else just a measure of standard deviation and presence of outliers basically

#What does it look like when we plot instantaneous cases on a logarithmic y axis?
ggplot(filled,aes(x=1:1095,y=rowSums(filled[,-1])))+geom_line()+scale_y_log10()

monthly_filled <- as.data.frame(matrix(nrow=length(unique(month(filled$Date)))*length(unique(year(filled$Date))),ncol=ncol(filled))) #thankfully since 1629-1631 aren't leap years don't have to worry about that.
for(year in unique(year(filled$Date))){
  filledOneYear <- filled[year == year(filled$Date),]
  for(month in unique(month(filled$Date))){
    filledOneMonth <- filledOneYear[month == month(filledOneYear$Date),]
    monthlySum <- as.numeric(colSums(filledOneMonth[,-1]))
    firstMonday <- filledOneMonth$Date[1]
    monthly_filled[12*(match(year,unique(year(filled$Date)))-1)+match(month,unique(month(filled$Date))),1] <- format(firstMonday,origin="1970-01-01")
    monthly_filled[12*(match(year,unique(year(filled$Date)))-1)+match(month,unique(month(filled$Date))),2:ncol(monthly_filled)] <- monthlySum
  }
}
colnames(monthly_filled) <- colnames(filled)
colnames(monthly_filled)[1] <- "Month_of"
monthly_filled$Month_of <- as.Date(monthly_filled$Month_of)
monthly_filled_PC <- as.data.frame(matrix(nrow=nrow(monthly_filled),ncol=ncol(monthly_filled[,-1])))
for(date in monthly_filled$Month_of){
  monthly_filled_PC[match(date,monthly_filled$Month_of),] <- monthly_filled[match(date,monthly_filled$Month_of),-1]/(population_specified$Population*1000)
}
colnames(monthly_filled_PC) <- colnames(monthly_filled[,-1])

monthly_shannon <- c()
monthly_not_shannon <- c()
cumulative_shannon_monthly <- c()
monthly_uniform_KL <- c()
for(m in monthly_filled$Month_of){
  monthly_shannon <- c(monthly_shannon,shannon_entropy_real(as.numeric(monthly_filled_PC[match(m,monthly_filled$Month_of),])))
  monthly_not_shannon <- c(monthly_not_shannon,shannon_entropy(as.numeric(monthly_filled_PC[match(m,monthly_filled$Month_of),])))
  cumulative_shannon_monthly <- c(cumulative_shannon_monthly,shannon_entropy_real(as.numeric(colSums(monthly_filled_PC[1:match(m,monthly_filled$Month_of),]))))
  monthly_uniform_KL <- c(monthly_uniform_KL, KL(as.numeric(colSums(monthly_filled_PC[1:match(m,monthly_filled$Month_of),]))/sum(as.numeric(colSums(monthly_filled_PC[1:match(m,monthly_filled$Month_of),]))),rep(1/ncol(monthly_filled_PC),ncol(monthly_filled_PC))))
}
plot.ts(monthly_shannon)
plot.ts(monthly_not_shannon)
plot.ts(cumulative_shannon_monthly)
plot.ts(monthly_uniform_KL)

monthly_uniform_KL_rep <- c()
for(a in unique(year(filled$Date))){
  #monthly_uniKL_year <- monthly_uniform_KL[(a-1)*12+1:a*12]
  for(m in 1:12){
    for(i in 1:length(which(m==month(filled$Date[year(filled$Date)==a])))){
      monthly_uniform_KL_rep <- c(monthly_uniform_KL_rep,monthly_uniform_KL[(match(a,unique(year(filled$Date)))-1)*12+m])
    }
  }
}
plot.ts(monthly_uniform_KL_rep,ylim=c(0,2.5))
lines(v,col="red")

cumulative_shannon_daily <- c()
shannon_daily <- c()
not_shannon_daily <- c()

for(d in filled$Date){
  cumulative_shannon_daily <- c(cumulative_shannon_daily,shannon_entropy_real(as.numeric(colSums(filled_popcorrected[1:match(d,filled$Date),-1]))))
  shannon_daily <- c(shannon_daily,shannon_entropy_real(as.numeric(filled_popcorrected[match(d,filled$Date),-1])))
  not_shannon_daily <- c(not_shannon_daily,shannon_entropy(as.numeric(filled_popcorrected[match(d,filled$Date),-1])))
}
plot.ts(cumulative_shannon_daily)
shannon_cumulative_daily_plot <- ggplot(as.data.frame(cumulative_shannon_daily),aes(x=filled$Date,y=cumulative_shannon_daily))+
  scale_y_continuous()+
  expand_limits(y=c(2,3.7))+
  geom_line()+
  theme_minimal()+
  scale_x_date(date_breaks="6 months",date_labels="%b %d %Y")+
  labs(y="H",x="Day",title="Cumulative Shannon Entropy with Daily Data")
plot.ts(shannon_daily)
plot.ts(not_shannon_daily)

weekly_filled_PC <- as.data.frame(matrix(nrow=nrow(weekly_filled),ncol=ncol(weekly_filled[,-1])))
for(date in weekly_filled$Week_of){
  weekly_filled_PC[match(date,weekly_filled$Week_of),] <- weekly_filled[match(date,weekly_filled$Week_of),-1]/(population_specified$Population*1000)
}
colnames(weekly_filled_PC) <- colnames(weekly_filled[,-1])

cumulative_shannon_weekly <- c()
shannon_weekly <- c()
not_shannon_weekly <- c()
for(w in weekly_filled$Week_of){
  cumulative_shannon_weekly <- c(cumulative_shannon_weekly,shannon_entropy_real(as.numeric(colSums(weekly_filled_PC[1:match(w,weekly_filled$Week_of),-1]))))
  shannon_weekly <- c(shannon_weekly,shannon_entropy_real(as.numeric(weekly_filled_PC[match(w,weekly_filled$Week_of),-1])))
  not_shannon_weekly <- c(not_shannon_weekly,shannon_entropy(as.numeric(weekly_filled_PC[match(w,weekly_filled$Week_of),-1])))
}
plot.ts(cumulative_shannon_weekly)
shannon_cumulative_weekly_plot <- ggplot(as.data.frame(cumulative_shannon_weekly),aes(x=weekly_filled$Week_of,y=cumulative_shannon_weekly))+
  scale_y_continuous()+
  expand_limits(y=c(2,3.7))+
  geom_line()+
  theme_minimal()+
  scale_x_date(date_breaks="6 months",date_labels="%b %d %Y")+
  labs(y="H",x="Week of",title="Cumulative Shannon Entropy with Weekly Data")
plot.ts(shannon_weekly)
plot.ts(not_shannon_weekly)

ggarrange(shannon_cumulative_daily_plot,shannon_cumulative_weekly_plot,ncol=1,nrow=2)

cumulative_shannon_weekly_repeat <- c()
for(i in 1:length(cumulative_shannon_weekly)){
  if(i %% 53 == 0){
    for(a in 1:2){
      cumulative_shannon_weekly_repeat <- c(cumulative_shannon_weekly_repeat,cumulative_shannon_weekly[i])
    }
  } else if(i %% 53 == 1){
    for(a in 1:6){
      cumulative_shannon_weekly_repeat <- c(cumulative_shannon_weekly_repeat,cumulative_shannon_weekly[i])
    }
  } else {
    for(a in 1:7){
      cumulative_shannon_weekly_repeat <- c(cumulative_shannon_weekly_repeat,cumulative_shannon_weekly[i])
    }
  }
}

shannon_weekly_repeat <- c()
for(i in 1:length(shannon_weekly)){
  if(i %% 53 == 0){
    for(a in 1:2){
      shannon_weekly_repeat <- c(shannon_weekly_repeat,shannon_weekly[i])
    }
  } else if(i %% 53 == 1){
    for(a in 1:6){
      shannon_weekly_repeat <- c(shannon_weekly_repeat,shannon_weekly[i])
    }
  } else {
    for(a in 1:7){
      shannon_weekly_repeat <- c(shannon_weekly_repeat,shannon_weekly[i])
    }
  }
}

combined_weekly_shannon_cumulative_plot <- ggplot(as.data.frame(cumulative_shannon_daily),aes(x=filled$Date,y=cumulative_shannon_daily))+
  scale_y_continuous()+
  expand_limits(y=c(2,3.7))+
  geom_line(aes(col="daily"),alpha=0.5)+
  geom_line(aes(y=cumulative_shannon_weekly_repeat,col="weekly"),alpha=0.5)+
  scale_color_manual(values=c(daily="blue",weekly="red"),labels=c(daily="Daily",weekly="Weekly"),limits=c("daily","weekly"))+
  scale_x_date(date_breaks="1 year",date_labels="%m-%d-%Y")+
  theme(plot.background=element_rect(fill="white"))+
  labs(y="H",x="Day (or Week of)",title="Cumulative Shannon Entropy",color="Data Used")

combined_weekly_shannon_plot <- ggplot(as.data.frame(shannon_daily),aes(x=filled$Date,y=shannon_daily))+
  scale_y_continuous()+
  expand_limits(y=c(0,4))+
  geom_line(aes(col="daily"),alpha=0.5)+
  geom_line(aes(y=shannon_weekly_repeat,col="weekly"),alpha=0.5)+
  scale_color_manual(values=c(daily="blue",weekly="red"),labels=c(daily="Daily",weekly="Weekly"),limits=c("daily","weekly"))+
  scale_x_date(date_breaks="1 year",date_labels="%m-%d-%Y")+
  theme(plot.background=element_rect(fill="white"))+
  labs(y="H",x="Day (or Week of)",title="Instantaneous Shannon Entropy",color="Data Used")

#Quantile and PDF Entropy on the same graph
#Maximum Likelihood fit of the normal
#Number on the x-axis

combined_weekly_daily <- ggarrange(combined_weekly_shannon_cumulative_plot,combined_weekly_shannon_plot,ncol=2,nrow=1)
combined_weekly_daily <- annotate_figure(combined_weekly_daily,top=text_grob("Shannon Entropy of Daily and Weekly Data",face="bold",size=14,col="red"))
ggsave("Daily and Weekly Combined.png",width=2560,height=1080,units="px")

monthly_shannon_cumulative_plot <- ggplot(as.data.frame(cumulative_shannon_monthly),aes(x=monthly_filled$Month_of,y=cumulative_shannon_monthly))+
  geom_line()+
  scale_x_date(date_breaks="6 months",date_labels="%m-%Y")+
  scale_y_continuous()+
  expand_limits(y=c(3.4,4))+
  theme(plot.background=element_rect(fill="white"))+
  labs(y="H",x="Month",title="Cumulative Shannon Entropy")
monthly_shannon_plot <- ggplot(as.data.frame(monthly_shannon),aes(x=monthly_filled$Month_of,y=monthly_shannon))+
  geom_line()+
  scale_x_date(date_breaks="6 months",date_labels="%m-%Y")+
  scale_y_continuous()+
  expand_limits(y=c(3.4,4))+
  theme(plot.background=element_rect(fill="white"))+
  labs(y="H",x="Month",title="Instantaneous Shannon Entropy")
monthly_shannon_grid <- ggarrange(monthly_shannon_cumulative_plot,monthly_shannon_plot,ncol=2,nrow=1) #should I make the y-axis the same or do 2 rows 1 column because x axes line up?
monthly_shannon_grid <- annotate_figure(monthly_shannon_grid,top=text_grob("Shannon Entropy of Monthly Data",face="bold",size=14,col="red"))
ggsave("Monthly Shannon Entropy.png",width=2560,height=1080,units="px")

#KL of cumulative raw infections vs. population

cumulative_raw_KL <- c()
for(date in filled$Date){
  cumulative_raw <- as.numeric(colSums(filled[1:match(date,filled$Date),-1]))
  cumulative_raw_normalized <- cumulative_raw/sum(cumulative_raw)
  population_specified_normalized <- population_specified$Population/sum(population_specified$Population)
  summand <- 0
  for(i in 1:length(cumulative_raw)){
    if(cumulative_raw[i] == 0){
      next
    }
    summand <- summand + cumulative_raw_normalized[i]*log((population_specified_normalized[i])/cumulative_raw_normalized[i])
  }
  cumulative_raw_KL <- c(cumulative_raw_KL,-1*summand)
}
plot.ts(cumulative_raw_KL)

#Gaussian MSE Fit
Gaussian_MSE_fit <- function(x,y,mu,sig,scale){
  f = function(p){
    d = p[3]*dnorm(x,mean=p[1],sd=p[2])
    sum((d-y)^2)
  }
  optim(c(mu,sig,scale,f))
}

t <- c()
v <- c()
v_c <- c()
w <- c()
w_c <- c()
s <- c()
inst_s <- c()
sum_cv <- c()
for(i in 1:length(filled$Date)){
  for(s in unique(sestiere_df$Sestiere)){
    cv <- as.numeric(colSums(filled_popcorrected[1:i,which(colnames(filled_popcorrected) %in% sestiere_df$Parish[sestiere_df$Sestiere == s])])) #a lot of noise (?) when you try to do non-cumulative, even for weekly data
    sum_cv <- c(sum_cv,sum(cv))
    x <- seq(0,max(cv),length.out=512)
    y <- dnorm(x,mean=mean(cv),sd=sd(cv))
    y <- y/sum(y) #never has zeroes so dont have to worry about that (i wonder why)
    u <- rep(1/512,512)
    u2 <- rep(0,512)
    u2[match(max(y),y)] <- 1
    if((i %% 31) == 0){
      #print(ggplot(as.data.frame(rep(0,length(cv))),aes(x=seq(0,max(cv),length.out=length(cv)+1)[-1],y=hist(cv,breaks=seq(0,max(cv),length.out=length(cv)+1),plot=F)$counts/sum(hist(cv,breaks=seq(0,max(cv),length.out=length(cv)+1),plot=F)$counts)))+geom_col())
      plot(x,density(cv,n=512,from=0,to=max(cv),kernel="epanechnikov")$y/sum(density(cv,n=512,from=0,to=max(cv),kernel="epanechnikov")$y),type="l",main=filled$Date[i],xlim=c(0,max(as.numeric(colSums(filled_popcorrected[1:i,])))),ylim=c(0,max(c(y,density(cv,n=512,from=0,to=max(cv),kernel="epanechnikov")$y/sum(density(cv,n=512,from=0,to=max(cv),kernel="epanechnikov")$y))))) #,xlim=c(0,0.7),ylim=c(0,0.0065) #Triangular SJ works well to accurately depict what is shown in the histograms at least
      lines(x,y,col="red")
      # plot(x,y,type="l")
      # plot(x,u2,type="l")
      # plot(x,u,type="l")
    }
  }
}

sestierePDFCumulative2 <- function(plot_year){
  plot_list <- list()
  filled_months <- filled[day(filled$Date) == 1,]
  for(date in filled_months$Date){
    cumulative_cases <- Cumulative(date,filled)
    cumulative_cases$cumulativeDeaths <- cumulative_cases$cumulativeDeaths/(population_specified$Population*1000) #population_specified needs this order to begin with (alphabetical) so do the population correction first
    cumulative_cases <- cumulative_cases[match(sestiere_df$Parish, cumulative_cases$parish_list),] #funnily enough filled only contains 4 parishes from S. Croce
    sestiere_df_cumulative <- cbind(sestiere_df,cumulative_cases$cumulativeDeaths)
    colnames(sestiere_df_cumulative) <- c("Parish", "Sestiere", "cumulative_pop_corrected")
    cv <- as.numeric(sestiere_df$cumulative_pop_corrected)
    KDE <- density(cv,n=512,from=0,to=max(cv),kernel="epanechnikov")$y/sum(density(cv,n=512,from=0,to=max(cv),kernel="epanechnikov")$y)
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

#uniform KL with quantile vs Gaussian KL with PDF


#PDF entropy
#nice entropy that goes up and stays saturated like log growth
#constraint: for given n infections, max einfections are you getting closer to max entropy for that number of infections
#try just pi log pi
#2 types of ref: ent and epidemics (Tsallis)
#Venetian plague
#both pdf and cdf entropy maximize (which is unexpected because max entropy for quantile is uniform which is just a dirac delta function as a pdf (min entropy)) because you end up with a slightly noisy uniform quantile that turns into a normal pdf
#figure zoterro out
#thermodynamics of entropy, do it in an SIR model where r0 becomes the entropy.