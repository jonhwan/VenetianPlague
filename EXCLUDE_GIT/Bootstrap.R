#Bootstrapping to see the CI on the entropy values!
library(boot)
library(progress)

entropyprocedure <- function(original_data,indices,timestep,stat=c("Shannon","Cases","Negent","UniformKL","Shannon_and_Cases","Negent_and_Cases")){ #might need to transpose for bootstrap to work
  boot_data <- original_data[,indices]
  # colnames(boot_data) <- sub("\\.+[0-9]+$","",colnames(boot_data))
  # pop_spec <- as.data.frame(matrix(nrow=ncol(boot_data),ncol=2))
  # colnames(pop_spec) <- c("Parish","Population")
  # for(i in 1:length(colnames(boot_data))){
  #   pop_spec$Population[i] <- population$Population[population$Parish==colnames(boot_data)[i]]
  # }
  pop_spec <- population_specified[indices,]
  cv <- as.numeric(colSums(boot_data[1:timestep,]))/(pop_spec$Population*1000)
  if(stat == "Cases"){
    sum_cv <- sum(cv)
    return(sum_cv)
  } else{
    #kern <- density(cv,n=512,from=0,to=max(cv),kernel="epanechnikov",bw="bcv")$y/sum(density(cv,n=512,from=0,to=max(cv),kernel="epanechnikov",bw="bcv")$y)
    kern <- density(cv,n=512,from=0,to=max(cv),kernel="epanechnikov")$y/sum(density(cv,n=512,from=0,to=max(cv),kernel="epanechnikov")$y)
    #kern <- density(cv,n=512,from=0,to=max(cv))$y
    kern <- kern/sum(kern) #is this the same as doing cv_norm inside the density function?
    kern_nz <- which(kern != 0)
    if(stat == "Shannon_and_Cases"){
      s <- -1*sum(kern[kern_nz]*log(kern[kern_nz]/(max(cv)/512)))  #standard bias correction from bins (although that's for histogram approximation, not sure if it works the same here)
      return(c(s,sum(cv)))
    } else if(stat == "Shannon"){
      s <- -1*sum(kern[kern_nz]*log(kern[kern_nz]/(max(cv)/512)))  #standard bias correction from bins (although that's for histogram approximation, not sure if it works the same here)
      return(s)
    } else if(stat == "Negent" | stat == "Negent_and_Cases"){
      x <- seq(0,max(cv),length.out=512)
      y <- dnorm(x,mean=mean(cv),sd=sd(cv))
      y <- y/sum(y)
      v <- -1*sum(kern[kern_nz]*log(y[kern_nz]/kern[kern_nz]))
      if(stat == "Negent"){
        return(v)
      } else {
        return(c(v,sum(cv)))
      }
    } else if(stat == "UniformKL"){
      u <- rep(1/512,512)
      w <- KL(kern,u)
      return(w)
    }
  }
}

#Manual boot
manual_boot <- function(data,N,ts,st){ #timestep, stat
  vec <- rep(0,N)
  for(i in 1:N){
    vec[i] <- entropyprocedure(data,sample(ncol(data),replace=TRUE),ts,st)
  }
  return(vec)
}

manual_boot_two_outputs <- function(data,N,ts,st){
  v1 <- rep(0,N)
  v2 <- rep(0,N)
  for(i in 1:N){
    x <- entropyprocedure(data,sample(ncol(data),replace=TRUE),ts,st)
    v1[i] <- x[1]
    v2[i] <- x[2]
  }
  return(cbind(v1,v2))
}

#set.seed(123)
b1 <- rep(0,length(filled$Date)) #lower
b2 <- rep(0,length(filled$Date)) #upper
bias <- rep(0,length(filled$Date))
pb <- progress_bar$new(total=length(filled$Date))
st <- Sys.time()
for(i in 1:length(filled$Date)){
  out <- manual_boot(data=filled[,-1],N=999,ts=i,st="Shannon")
  bias[i] <- mean(out) - entropyprocedure(filled[,-1],seq(1,ncol(filled[,-1]),length.out=ncol(filled[,-1])),timestep=i,stat="Shannon") #check if entropy procedure does the same thing as what produces s exactly. Do b1 - bias and b2 - bias
  sorted <- sort(out)
  b1[i] <- sorted[25]
  b2[i] <- sorted[975]
  pb$tick()
}
ed <- Sys.time()
round(ed-st)
plot.ts(b1)
lines(b2)
mean(bias)

y_offset <- log(1095)
plot.ts(b2-bias+y_offset,ylim=c(min(b1)+y_offset,max(b2)+y_offset))
lines(b1-bias+y_offset)
lines(s+y_offset,col="red") #OLD, doesn't know what s is. Maybe have to run Cross Entropy Analysis.R first. But why the bias needed?
for(m in unique(month(filled$Date))){
  points(x=rep(match(m,month(filled$Date)),999),y=manual_boot(data=filled[,-1],N=999,ts=match(m,month(filled$Date)),st="Shannon")-bias[match(m,month(filled$Date))]+log(1095),pch=".")
  # points(x=match(m,month(filled$Date)),y=s[match(m,month(filled$Date))]+log(1095),col="red")
  # points(x=match(m,month(filled$Date)),y=b1[match(m,month(filled$Date))]-bias[match(m,month(filled$Date))]+log(1095),col="black")
  # points(x=match(m,month(filled$Date)),y=b2[match(m,month(filled$Date))]-bias[match(m,month(filled$Date))]+log(1095),col="black")
  points(x=rep(match(m,month(filled$Date))+365,999),y=manual_boot(data=filled[,-1],N=999,ts=match(m,month(filled$Date))+365,st="Shannon")-bias[match(m,month(filled$Date))+365]+log(1095),pch=".")
  # points(x=match(m,month(filled$Date))+365,y=s[match(m,month(filled$Date))+365]+log(1095),col="red")
  # points(x=match(m,month(filled$Date))+365,y=b1[match(m,month(filled$Date))+365]-bias[match(m,month(filled$Date))+365]+log(1095),col="black")
  # points(x=match(m,month(filled$Date))+365,y=b2[match(m,month(filled$Date))+365]-bias[match(m,month(filled$Date))+365]+log(1095),col="black")
  points(x=rep(match(m,month(filled$Date))+730,999),y=manual_boot(data=filled[,-1],N=999,ts=match(m,month(filled$Date))+730,st="Shannon")-bias[match(m,month(filled$Date))+730]+log(1095),pch=".")
  # points(x=match(m,month(filled$Date))+730,y=s[match(m,month(filled$Date))+730]+log(1095),col="red")
  # points(x=match(m,month(filled$Date))+730,y=b1[match(m,month(filled$Date))+730]-bias[match(m,month(filled$Date))+730]+log(1095),col="black")
  # points(x=match(m,month(filled$Date))+730,y=b2[match(m,month(filled$Date))+730]-bias[match(m,month(filled$Date))+730]+log(1095),col="black")
}

b1 <- rep(0,length(filled$Date)) #lower
b2 <- rep(0,length(filled$Date)) #upper
bias <- rep(0,length(filled$Date))
pb <- progress_bar$new(total=length(filled$Date))
st <- Sys.time()
for(i in 1:length(filled$Date)){
  out <- manual_boot(data=filled[,-1],N=999,ts=i,st="Negent")
  bias[i] <- mean(out) - entropyprocedure(filled[,-1],seq(1,ncol(filled[,-1]),length.out=ncol(filled[,-1])),timestep=i,stat="Negent") #check if entropy procedure does the same thing as what produces s exactly. Do b1 - bias and b2 - bias
  sorted <- sort(out)
  b1[i] <- sorted[25]
  b2[i] <- sorted[975]
  pb$tick()
}
ed <- Sys.time()
round(ed-st)
plot.ts(b1)
lines(b2)
mean(bias)

cumulative_gaussianKL <- c()
for(i in 1:length(filled$Date)){
  cumulative_gaussianKL <- c(cumulative_gaussianKL,entropyprocedure(filled[,-1],seq(1,ncol(filled[,-1]),length.out=ncol(filled[,-1])),timestep=i,stat="Negent"))
}

ggplot(as.data.frame(b2),aes(x=filled$Date,y=b2-bias))+
  geom_line(aes(y=b2-bias))+
  geom_line(aes(y=b1-bias))+
  geom_ribbon(aes(ymin=b1-bias,ymax=b2-bias),col="black",alpha=0.25)+
  geom_line(aes(y=cumulative_gaussianKL),col="red")+
  labs(x="Date",y="KL Divergence",title="KL Divergence b/w PDF and Gaussian")

plot.ts(b2-bias,ylim=c(min(b1-bias),max(b2-bias)))
lines(b1-bias)
polygon(c(1:1095,rev(1:1095)),c(b1-bias,rev(b2-bias)),col=rgb(0,0,0,alpha=0.25))
lines(cumulative_gaussianKL,col="red")
for(m in unique(month(filled$Date))){
  points(x=rep(match(m,month(filled$Date)),999),y=manual_boot(data=filled[,-1],N=999,ts=match(m,month(filled$Date)),st="Negent")-bias[match(m,month(filled$Date))],pch=".")
  points(x=rep(match(m,month(filled$Date))+365,999),y=manual_boot(data=filled[,-1],N=999,ts=match(m,month(filled$Date))+365,st="Negent")-bias[match(m,month(filled$Date))+365],pch=".")
  points(x=rep(match(m,month(filled$Date))+730,999),y=manual_boot(data=filled[,-1],N=999,ts=match(m,month(filled$Date))+730,st="Negent")-bias[match(m,month(filled$Date))+730],pch=".")
}

#KL vs. cumulative incidence rate

upper_shannon <- rep(0,length(filled$Date)) #lower
lower_sum_incidence_rate <- rep(0,length(filled$Date))
lower_shannon <- rep(0,length(filled$Date)) #upper
upper_sum_incidence_rate <- rep(0,length(filled$Date))
bias_2 <- rep(0,length(filled$Date))
pb <- progress_bar$new(total=length(filled$Date))

st <- Sys.time()
for(i in 1:length(filled$Date)){
  out <- manual_boot_two_outputs(data=filled[,-1],N=999,ts=i,st="Negent_and_Cases")
  shannon_out <- out[,1]
  bias_2[i] <- mean(shannon_out) - entropyprocedure(filled[,-1],seq(1,length(filled[,-1]),length.out=length(filled[,-1])),timestep=i,stat="Negent") #check if entropy procedure does the same thing as what produces s exactly. Do b1 - bias and b2 - bias
  shannon_order <- order(shannon_out)
  sorted_sum_incidence_rate <- out[,2][shannon_order]
  sorted <- shannon_out[shannon_order]
  lower_shannon[i] <- sorted[25]
  lower_sum_incidence_rate[i] <- sorted_sum_incidence_rate[25]
  upper_shannon[i] <- sorted[975]
  upper_sum_incidence_rate[i] <- sorted_sum_incidence_rate[975]
  pb$tick()
}
ed <- Sys.time()
ed - st

PDF_negent <- c()
for(i in 1:length(filled$Date)){
  PDF_negent <- c(PDF_negent,entropyprocedure(filled[,-1],seq(1,53,length.out=53),i,"Negent"))
}

y_offset_2 <- 0
plot(x=upper_sum_incidence_rate,y=upper_shannon-bias_2+y_offset_2,ylim=c(min(lower_shannon)+y_offset_2,max(upper_shannon)+y_offset_2),pch=".") #type="l"?
points(x=lower_sum_incidence_rate,y=lower_shannon-bias_2+y_offset_2,pch=".")
points(x=sum_cv,y=PDF_negent+y_offset_2,col="red",pch=".")

ggplot(as.data.frame(upper_sum_incidence_rate),aes(x=0:max(c(sum_cv,lower_sum_incidence_rate,upper_sum_incidence_rate)),y=0:max(upper_shannon-bias_2)))+
  geom_point(aes(x=lower_sum_incidence_rate,y=lower_shannon-bias_2),shape=".")+
  geom_point(aes(x=upper_sum_incidence_rate,y=upper_shannon-bias_2),shape=".")+
  geom_point(aes(x=sum_cv,y=PDF_negent),shape=".",color="red")+
  labs(x="Sum of Cumulative Incidence Rate",y="KL Divergence",title="KL Divergence b/w PDF and Gaussian vs. Sum of Cumulative Incidence Rate")

for(m in unique(month(filled$Date))){
  points(x=rep(match(m,month(filled$Date)),999),y=manual_boot(data=filled[,-1],N=999,ts=match(m,month(filled$Date)),st="Shannon")-bias[match(m,month(filled$Date))]+log(1095),pch=".",cex=1)
  points(x=rep(match(m,month(filled$Date))+365,999),y=manual_boot(data=filled[,-1],N=999,ts=match(m,month(filled$Date))+365,st="Shannon")-bias[match(m,month(filled$Date))+365]+log(1095),pch=".",cex=1)
  points(x=rep(match(m,month(filled$Date))+730,999),y=manual_boot(data=filled[,-1],N=999,ts=match(m,month(filled$Date))+730,st="Shannon")-bias[match(m,month(filled$Date))+730]+log(1095),pch=".",cex=1)
}

shannonBySestiere_Bootstrapped <- function(){
  b1 <- as.data.frame(matrix(nrow=length(filled$Date),ncol=length(unique(sestiere_df$Sestiere)))) #lower
  b2 <- as.data.frame(matrix(nrow=length(filled$Date),ncol=length(unique(sestiere_df$Sestiere)))) #upper
  bias <- as.data.frame(matrix(nrow=length(filled$Date),ncol=length(unique(sestiere_df$Sestiere))))
  st <- Sys.time()
  for(s in unique(sestiere_df$Sestiere)){
    pb <- progress_bar$new(total=length(filled$Date))
    for(i in 1:length(filled$Date)){
      out <- manual_boot(data=filled[,match(sestiere_df$Parish[sestiere_df$Sestiere == s],colnames(filled))],N=999,ts=i,st="Shannon")
      bias[i,match(s,unique(sestiere_df$Sestiere))] <- mean(out) - entropyprocedure(filled[,match(sestiere_df$Parish[sestiere_df$Sestiere == s],colnames(filled))],seq(1,ncol(filled[,match(sestiere_df$Parish[sestiere_df$Sestiere == s],colnames(filled))]),length.out=ncol(filled[,match(sestiere_df$Parish[sestiere_df$Sestiere == s],colnames(filled))])),timestep=i,stat="Shannon") #check if entropy procedure does the same thing as what produces s exactly. Do b1 - bias and b2 - bias
      sorted <- sort(out)
      b1[i,match(s,unique(sestiere_df$Sestiere))] <- sorted[25]
      b2[i,match(s,unique(sestiere_df$Sestiere))] <- sorted[975]
      pb$tick()
    }
  }
  ed <- Sys.time()
  print(ed - st)
  return(list(b1,b2,bias))
}

shannonBySestiere <- function(){
  out_vec <- as.data.frame(matrix(nrow=length(filled$Date),ncol=length(unique(sestiere_df$Sestiere))))
  st <- Sys.time()
  for(s in unique(sestiere_df$Sestiere)){
    pb <- progress_bar$new(total=length(filled$Date))
    for(i in 1:length(filled$Date)){
      out_vec[i,match(s,unique(sestiere_df$Sestiere))] <- entropyprocedure(filled[,match(sestiere_df$Parish[sestiere_df$Sestiere == s],colnames(filled))],seq(1,ncol(filled[,match(sestiere_df$Parish[sestiere_df$Sestiere == s],colnames(filled))]),length.out=ncol(filled[,match(sestiere_df$Parish[sestiere_df$Sestiere == s],colnames(filled))])),timestep=i,stat="Shannon")
      pb$tick()
    }
  }
  ed <- Sys.time()
  print(ed - st)
  return(out_vec)
}

x <- shannonBySestiere_Bootstrapped()
y <- shannonBySestiere()

time_series <- 1:1095
plot.ts(x[[1]]-x[[3]])
plot.ts(x[[1]][,1]-x[[3]][,1],col="green",lwd=0.5,ylim=c(-9,2),lty=2)
lines(x[[2]][,1]-x[[3]][,1],col="green",lwd=0.5,lty=2)
polygon(c(time_series,rev(time_series)),c(x[[1]][,1]-x[[3]][,1],rev(x[[2]][,1]-x[[3]][,1])),col=rgb(0,255,0,maxColorValue=255,alpha=100),lty=0)
lines(y[,1],col="green",lwd=2)
lines(x[[1]][,2]-x[[3]][,2],col="red",lwd=0.5,lty=2)
lines(x[[2]][,2]-x[[3]][,2],col="red",lwd=0.5,lty=2)
polygon(c(time_series,rev(time_series)),c(x[[1]][,2]-x[[3]][,2],rev(x[[2]][,2]-x[[3]][,2])),col=rgb(255,0,0,maxColorValue=255,alpha=100),lty=0)
lines(y[,2],col="red",lwd=2)
lines(x[[1]][,3]-x[[3]][,3],col="blue",lwd=0.5,lty=2)
lines(x[[2]][,3]-x[[3]][,3],col="blue",lwd=0.5,lty=2)
polygon(c(time_series,rev(time_series)),c(x[[1]][,3]-x[[3]][,3],rev(x[[2]][,3]-x[[3]][,3])),col=rgb(0,0,255,maxColorValue=255,alpha=100),lty=0)
lines(y[,3],col="blue",lwd=2)
lines(x[[1]][,4]-x[[3]][,4],col="yellow",lwd=0.5,lty=2)
lines(x[[2]][,4]-x[[3]][,4],col="yellow",lwd=0.5,lty=2)
polygon(c(time_series,rev(time_series)),c(x[[1]][,4]-x[[3]][,4],rev(x[[2]][,4]-x[[3]][,4])),col=rgb(255,255,0,maxColorValue=255,alpha=100),lty=0)
lines(y[,4],col="yellow",lwd=2)
lines(x[[1]][,5]-x[[3]][,5],col="orange",lwd=0.5,lty=2)
lines(x[[1]][,5]-x[[3]][,5],col="orange",lwd=0.5,lty=2)
polygon(c(time_series,rev(time_series)),c(x[[1]][,5]-x[[3]][,5],rev(x[[2]][,5]-x[[3]][,5])),col=rgb(255,165,0,maxColorValue=255,alpha=100),lty=0)
lines(y[,5],col="orange",lwd=2)
lines(x[[1]][,6]-x[[3]][,6],col="pink",lwd=0.5,lty=2)
lines(x[[2]][,6]-x[[3]][,6],col="pink",lwd=0.5,lty=2)
polygon(c(time_series,rev(time_series)),c(x[[1]][,6]-x[[3]][,6],rev(x[[2]][,6]-x[[3]][,6])),col=rgb(255,192,203,maxColorValue=255,alpha=100),lty=0)
lines(y[,6],col="pink",lwd=2)
legend(600,-4,legend=unique(sestiere_df$Sestiere),col=c("green","red","blue","yellow","orange","pink"),lty=1)

#Shannon's Vs. Sum of cumulative incidence rate
#set.seed(123)
upper_shannon <- rep(0,length(filled$Date)) #lower
lower_sum_incidence_rate <- rep(0,length(filled$Date))
lower_shannon <- rep(0,length(filled$Date)) #upper
upper_sum_incidence_rate <- rep(0,length(filled$Date))
bias_2 <- rep(0,length(filled$Date))
pb <- progress_bar$new(total=length(filled$Date))

st <- Sys.time()
for(i in 1:length(filled$Date)){
  out <- manual_boot_two_outputs(data=filled[,-1],N=999,ts=i,st="Shannon_and_Cases")
  shannon_out <- out[,1]
  bias_2[i] <- mean(shannon_out) - entropyprocedure(filled[,-1],seq(1,length(filled[,-1]),length.out=length(filled[,-1])),timestep=i,stat="Shannon") #check if entropy procedure does the same thing as what produces s exactly. Do b1 - bias and b2 - bias
  shannon_order <- order(shannon_out)
  sorted_sum_incidence_rate <- out[,2][shannon_order]
  sorted <- shannon_out[shannon_order]
  lower_shannon[i] <- sorted[25]
  lower_sum_incidence_rate[i] <- sorted_sum_incidence_rate[25]
  upper_shannon[i] <- sorted[975]
  upper_sum_incidence_rate[i] <- sorted_sum_incidence_rate[975]
  pb$tick()
}
ed <- Sys.time()
ed - st

y_offset_2 <- log(1095)
plot(x=upper_sum_incidence_rate,y=upper_shannon-bias_2+y_offset_2,ylim=c(min(lower_shannon)+y_offset_2,max(upper_shannon)+y_offset_2),pch=".") #type="l"?
points(x=lower_sum_incidence_rate,y=lower_shannon-bias_2+y_offset_2,pch=".")
points(x=sum_cv,y=s+y_offset_2,col="red",pch=".")

for(m in unique(month(filled$Date))){
  points(x=rep(match(m,month(filled$Date)),999),y=manual_boot(data=filled[,-1],N=999,ts=match(m,month(filled$Date)),st="Shannon")-bias[match(m,month(filled$Date))]+log(1095),pch=".",cex=1)
  points(x=rep(match(m,month(filled$Date))+365,999),y=manual_boot(data=filled[,-1],N=999,ts=match(m,month(filled$Date))+365,st="Shannon")-bias[match(m,month(filled$Date))+365]+log(1095),pch=".",cex=1)
  points(x=rep(match(m,month(filled$Date))+730,999),y=manual_boot(data=filled[,-1],N=999,ts=match(m,month(filled$Date))+730,st="Shannon")-bias[match(m,month(filled$Date))+730]+log(1095),pch=".",cex=1)
}

quantilefunctionprocedure <- function(original_data,indices,timestep,stat=c("Shannon","Cases","Negent","UniformKL","Shannon_and_Cases","UniformKL_and_Cases")){
  boot_data <- original_data[,indices]
  colnames(boot_data) <- sub("\\.+[0-9]+$","",colnames(boot_data))
  pop_spec <- as.data.frame(matrix(nrow=ncol(boot_data),ncol=2))
  colnames(pop_spec) <- c("Parish","Population")
  for(i in 1:length(colnames(boot_data))){
    pop_spec$Population[i] <- population$Population[population$Parish==colnames(boot_data)[i]]
  }
  #pop_spec <- population_specified[indices,]
  cv <- as.numeric(colSums(boot_data[1:timestep,]))/(pop_spec$Population*1000)
  if(stat == "Cases"){
    return(sum(cv))
  } else if(stat == "Shannon"){
    nz <- which(cv != 0 & !is.na(cv))
    cv_norm <- cv/sum(cv)
    return(-1*sum(cv_norm[nz]*log(cv_norm[nz])))
  } else if(stat == "Shannon_and_Cases"){
    nz <- which(cv != 0 & !is.na(cv))
    cv_norm <- cv/sum(cv)
    return(c(-1*sum(cv_norm[nz]*log(cv_norm[nz])),sum(cv)))
  } else if(stat == "UniformKL"){
    return(KL(cv/sum(cv),rep(1/length(cv),length(cv))))
  } else if(stat == "UniformKL_and_Cases"){
    return(c(KL(cv/sum(cv),rep(1/length(cv),length(cv))),sum(cv)))
  } else if(stat == "Negent"){ #I don't get how you do negent for this since ordering of parishes is non-numbered, but this definitely isn't right
    nz <- which(cv != 0 & !is.na(cv))
    cv_norm <- cv/sum(cv)
    x <- seq(0,max(cv_norm),length.out=512)
    y <- dnorm(x,mean=mean(cv_norm),sd=sd(cv_norm))
    y <- y/sum(y)
    v <- -1*sum(cv_norm[nz]*log(y[nz]/(cv_norm[nz]/(max(cv_norm)/512))))
    return(v)
  }
}

manual_boot_2 <- function(data,N,ts,st){
  vec <- rep(0,N)
  for(i in 1:N){
    vec[i] <- quantilefunctionprocedure(data,sample(ncol(data),replace=TRUE),ts,st)
  }
  return(vec)
}

manual_boot_two_outputs_2 <- function(data,N,ts,st){
  v1 <- rep(0,N)
  v2 <- rep(0,N)
  for(i in 1:N){
    x <- quantilefunctionprocedure(data,sample(ncol(data),replace=TRUE),ts,st)
    v1[i] <- x[1]
    v2[i] <- x[2]
  }
  return(cbind(v1,v2))
}

upper_bound_quantile <- rep(0,length(filled$Date))
lower_bound_quantile <- rep(0,length(filled$Date))
bias_quantile <- rep(0,length(filled$Date))

pb <- progress_bar$new(total=length(filled$Date))
for(d in 1:length(filled$Date)){
  out <- manual_boot_2(filled[,-1],999,d,"Shannon")
  sorted <- sort(out)
  lower_bound_quantile[d] <- sorted[25]
  upper_bound_quantile[d] <- sorted[975]
  bias_quantile[d] <- mean(out) - quantilefunctionprocedure(filled[,-1],seq(1,ncol(filled[,-1]),length.out=ncol(filled[,-1])),d,"Shannon")
  pb$tick()
}

plot.ts(upper_bound_quantile-bias_quantile,ylim=c(min(lower_bound_quantile)-0.25,max(upper_bound_quantile)+0.25))
print(mean(bias_quantile))
print(upper_bound_quantile - shannon_entropy_real(rep(1/ncol(filled[,-1]),ncol(filled[,-1]))))
lines(lower_bound_quantile-bias_quantile)
lines(cumulative_cases_shannon_percase,col="red")

for(m in unique(month(filled$Date))){
  points(x=rep(match(m,month(filled$Date)),999),y=manual_boot_2(data=filled[,-1],N=999,ts=match(m,month(filled$Date)),st="Shannon")-bias_quantile[match(m,month(filled$Date))],pch=".",cex=1)
  points(x=rep(match(m,month(filled$Date))+365,999),y=manual_boot_2(data=filled[,-1],N=999,ts=match(m,month(filled$Date))+365,st="Shannon")-bias_quantile[match(m,month(filled$Date))+365],pch=".",cex=1)
  points(x=rep(match(m,month(filled$Date))+730,999),y=manual_boot_2(data=filled[,-1],N=999,ts=match(m,month(filled$Date))+730,st="Shannon")-bias_quantile[match(m,month(filled$Date))+730],pch=".",cex=1)
}

b1 <- rep(0,length(filled$Date)) #lower
b2 <- rep(0,length(filled$Date)) #upper
bias <- rep(0,length(filled$Date))
pb <- progress_bar$new(total=length(filled$Date))
st <- Sys.time()
for(i in 1:length(filled$Date)){
  out <- manual_boot_2(data=filled[,-1],N=999,ts=i,st="UniformKL")
  bias[i] <- mean(out) - quantilefunctionprocedure(filled[,-1],seq(1,ncol(filled[,-1]),length.out=ncol(filled[,-1])),timestep=i,stat="UniformKL") #check if entropy procedure does the same thing as what produces s exactly. Do b1 - bias and b2 - bias
  sorted <- sort(out)
  b1[i] <- sorted[25]
  b2[i] <- sorted[975]
  pb$tick()
}
ed <- Sys.time()
round(ed-st)
plot.ts(b1)
lines(b2)
mean(bias)

quantile_uniformKL <- c()
for(i in 1:length(filled$Date)){
  quantile_uniformKL <- c(quantile_uniformKL,quantilefunctionprocedure(filled[,-1],seq(1,53,length.out=53),i,"UniformKL"))
}

ggplot(as.data.frame(b2),aes(x=filled$Date,y=b2-bias))+
  geom_line(aes(y=b2-bias))+
  geom_line(aes(y=b1-bias))+
  geom_ribbon(aes(ymin=b1-bias,ymax=b2-bias),col="black",alpha=0.25)+
  geom_line(aes(y=quantile_uniformKL),col="red")+
  labs(x="Date",y="KL Divergence",title="KL Divergence b/w Quantile and Uniform")

plot.ts(b2-bias,ylim=c(min(b1-bias),max(b2-bias)))
lines(b1-bias)
polygon(c(1:1095,rev(1:1095)),c(b1-bias,rev(b2-bias)),col=rgb(0,0,0,alpha=0.25))
lines(quantile_uniformKL,col="red")
for(m in unique(month(filled$Date))){
  points(x=rep(match(m,month(filled$Date)),999),y=manual_boot(data=filled[,-1],N=999,ts=match(m,month(filled$Date)),st="Negent")-bias[match(m,month(filled$Date))],pch=".")
  points(x=rep(match(m,month(filled$Date))+365,999),y=manual_boot(data=filled[,-1],N=999,ts=match(m,month(filled$Date))+365,st="Negent")-bias[match(m,month(filled$Date))+365],pch=".")
  points(x=rep(match(m,month(filled$Date))+730,999),y=manual_boot(data=filled[,-1],N=999,ts=match(m,month(filled$Date))+730,st="Negent")-bias[match(m,month(filled$Date))+730],pch=".")
}

upper_shannon <- rep(0,length(filled$Date)) #lower
lower_sum_incidence_rate <- rep(0,length(filled$Date))
lower_shannon <- rep(0,length(filled$Date)) #upper
upper_sum_incidence_rate <- rep(0,length(filled$Date))
bias_2 <- rep(0,length(filled$Date))
pb <- progress_bar$new(total=length(filled$Date))

st <- Sys.time()
for(i in 1:length(filled$Date)){
  out <- manual_boot_two_outputs_2(data=filled[,-1],N=999,ts=i,st="UniformKL_and_Cases")
  shannon_out <- out[,1]
  bias_2[i] <- mean(shannon_out) - quantilefunctionprocedure(filled[,-1],seq(1,length(filled[,-1]),length.out=length(filled[,-1])),timestep=i,stat="UniformKL") #check if entropy procedure does the same thing as what produces s exactly. Do b1 - bias and b2 - bias
  shannon_order <- order(shannon_out)
  sorted_sum_incidence_rate <- out[,2][shannon_order]
  sorted <- shannon_out[shannon_order]
  lower_shannon[i] <- sorted[25]
  lower_sum_incidence_rate[i] <- sorted_sum_incidence_rate[25]
  upper_shannon[i] <- sorted[975]
  upper_sum_incidence_rate[i] <- sorted_sum_incidence_rate[975]
  pb$tick()
}
ed <- Sys.time()
ed - st

y_offset_2 <- 0
plot(x=upper_sum_incidence_rate,y=upper_shannon-bias_2+y_offset_2,ylim=c(min(lower_shannon)+y_offset_2,max(upper_shannon)+y_offset_2),pch=".") #type="l"?
points(x=lower_sum_incidence_rate,y=lower_shannon-bias_2+y_offset_2,pch=".")
points(x=sum_cv,y=quantile_uniformKL+y_offset_2,col="red",pch=".")

ggplot(as.data.frame(upper_sum_incidence_rate),aes(x=0:max(c(sum_cv,lower_sum_incidence_rate,upper_sum_incidence_rate)),y=0:max(upper_shannon-bias_2)))+
  geom_point(aes(x=lower_sum_incidence_rate,y=lower_shannon-bias_2),shape=".")+
  geom_point(aes(x=upper_sum_incidence_rate,y=upper_shannon-bias_2),shape=".")+
  geom_point(aes(x=sum_cv,y=quantile_uniformKL),shape=".",color="red")+
  labs(x="Sum of Cumulative Incidence Rate",y="KL Divergence",title="KL Divergence b/w Quantile and Uniform vs. Sum of Cumulative Incidence Rate")

upper_bound_quantile <- rep(0,length(filled$Date))
lower_bound_quantile <- rep(0,length(filled$Date))
bias_quantile <- rep(0,length(filled$Date))

pb <- progress_bar$new(total=length(filled$Date))
cumulative_gaussianKL_quantile <- c()
for(d in 1:length(filled$Date)){
  out <- manual_boot_2(filled[,-1],999,d,"Negent")
  sorted <- sort(out)
  lower_bound_quantile[d] <- sorted[25]
  upper_bound_quantile[d] <- sorted[975]
  bias_quantile[d] <- mean(out) - quantilefunctionprocedure(filled[,-1],seq(1,ncol(filled[,-1]),length.out=ncol(filled[,-1])),d,"Negent")
  cumulative_gaussianKL_quantile <- c(cumulative_gaussianKL_quantile,quantilefunctionprocedure(filled[,-1],seq(1,ncol(filled[,-1]),length.out=ncol(filled[,-1])),d,"Negent"))
  pb$tick()
}

plot.ts(upper_bound_quantile-bias_quantile,ylim=c(min(lower_bound_quantile)-0.25,max(upper_bound_quantile)+0.25))
print(mean(bias_quantile))
print(upper_bound_quantile - shannon_entropy_real(rep(1/ncol(filled[,-1]),ncol(filled[,-1]))))
lines(lower_bound_quantile-bias_quantile)
lines(cumulative_gaussianKL_quantile,col="red")

#Shannon of quantile vs. Sum of Cumulative Incidence Rate

upper_bound_quantile_2 <- rep(0,length(filled$Date))
upper_sum_incidence_rate_2 <- rep(0,length(filled$Date))
lower_bound_quantile_2 <- rep(0,length(filled$Date))
lower_sum_incidence_rate_2 <- rep(0,length(filled$Date))
bias_quantile_2 <- rep(0,length(filled$Date))

pb <- progress_bar$new(total=length(filled$Date))
for(d in 1:length(filled$Date)){
  out_2 <- manual_boot_two_outputs_2(filled[,-1],999,d,"Shannon_and_Cases")
  shannon_order_2 <- order(out_2[,1])
  sorted_sum_incidence_rate_2 <- out_2[,2][shannon_order_2]
  sorted_2 <- out_2[shannon_order_2]
  lower_bound_quantile_2[d] <- sorted_2[25]
  lower_sum_incidence_rate_2[d] <- sorted_sum_incidence_rate_2[25]
  upper_bound_quantile_2[d] <- sorted_2[975]
  upper_sum_incidence_rate_2[d] <- sorted_sum_incidence_rate_2[975]
  bias_quantile[d] <- mean(out_2) - quantilefunctionprocedure(filled[,-1],seq(1,ncol(filled[,-1]),length.out=ncol(filled[,-1])),d,"Shannon")
  pb$tick()
}

plot(x=upper_sum_incidence_rate_2, y=upper_bound_quantile_2-bias_quantile_2,ylim=c(min(lower_bound_quantile_2)-0.25,max(upper_bound_quantile_2)+0.25),xlim=c(min(lower_sum_incidence_rate_2),max(upper_sum_incidence_rate_2)))
print(mean(bias_quantile_2))
print(upper_bound_quantile_2 - shannon_entropy_real(rep(1/ncol(filled[,-1]),ncol(filled[,-1]))))
points(x=lower_sum_incidence_rate_2,y=lower_bound_quantile_2-bias_quantile_2)
points(x=cumsum(rowSums(filled_popcorrected)),y=cumulative_cases_shannon_percase,col="red")

quantile_by_sestiere_boot <- function(){
  b1 <- as.data.frame(matrix(nrow=length(filled$Date),ncol=length(unique(sestiere_df$Sestiere)))) #lower
  b2 <- as.data.frame(matrix(nrow=length(filled$Date),ncol=length(unique(sestiere_df$Sestiere)))) #upper
  bias <- as.data.frame(matrix(nrow=length(filled$Date),ncol=length(unique(sestiere_df$Sestiere))))
  for(s in unique(sestiere_df$Sestiere)){
    pb <- progress_bar$new(total=length(filled$Date))
    for(i in 1:length(filled$Date)){
      out <- manual_boot_2(data=filled[,match(sestiere_df$Parish[sestiere_df$Sestiere == s],colnames(filled))],N=999,ts=i,st="Shannon")
      bias[i,match(s,unique(sestiere_df$Sestiere))] <- mean(out) - quantilefunctionprocedure(filled[,match(sestiere_df$Parish[sestiere_df$Sestiere == s],colnames(filled))],seq(1,ncol(filled[,match(sestiere_df$Parish[sestiere_df$Sestiere == s],colnames(filled))]),length.out=ncol(filled[,match(sestiere_df$Parish[sestiere_df$Sestiere == s],colnames(filled))])),timestep=i,stat="Shannon") #check if entropy procedure does the same thing as what produces s exactly. Do b1 - bias and b2 - bias
      sorted <- sort(out)
      b1[i,match(s,unique(sestiere_df$Sestiere))] <- sorted[25]
      b2[i,match(s,unique(sestiere_df$Sestiere))] <- sorted[975]
      pb$tick()
    }
  }
  return(list(b1,b2,bias))
}
quantile_by_sestiere_boot_out <- quantile_by_sestiere_boot()

quantile_by_sestiere <- function(){
  out_vec <- as.data.frame(matrix(nrow=length(filled$Date),ncol=length(unique(sestiere_df$Sestiere))))
  for(s in unique(sestiere_df$Sestiere)){
    pb <- progress_bar$new(total=length(filled$Date))
    for(i in 1:length(filled$Date)){
      out_vec[i,match(s,unique(sestiere_df$Sestiere))] <- quantilefunctionprocedure(filled[,match(sestiere_df$Parish[sestiere_df$Sestiere == s],colnames(filled))],seq(1,ncol(filled[,match(sestiere_df$Parish[sestiere_df$Sestiere == s],colnames(filled))]),length.out=ncol(filled[,match(sestiere_df$Parish[sestiere_df$Sestiere == s],colnames(filled))])),timestep=i,stat="Shannon")
      pb$tick()
    }
  }
  return(out_vec)
}

quantile_by_sestiere_out <- quantile_by_sestiere()

time_series <- 1:1095

plot.ts(quantile_by_sestiere_boot_out[[1]]-quantile_by_sestiere_boot_out[[3]])
plot.ts(quantile_by_sestiere_boot_out[[1]][,1]-quantile_by_sestiere_boot_out[[3]][,1],col="green",lwd=0.5,ylim=c(min(quantile_by_sestiere_boot_out[[1]])-0.25,max(quantile_by_sestiere_boot_out[[2]]-quantile_by_sestiere_boot_out[[3]])+0.25),lty=2)
lines(quantile_by_sestiere_boot_out[[2]][,1]-quantile_by_sestiere_boot_out[[3]][,1],col="green",lwd=0.5,lty=2)
polygon(c(time_series,rev(time_series)),c(quantile_by_sestiere_boot_out[[1]][,1]-quantile_by_sestiere_boot_out[[3]][,1],rev(quantile_by_sestiere_boot_out[[2]][,1]-quantile_by_sestiere_boot_out[[3]][,1])),col=rgb(0,255,0,maxColorValue=255,alpha=100),lty=0)
lines(quantile_by_sestiere_out[,1],col="green",lwd=2)
lines(quantile_by_sestiere_boot_out[[1]][,2]-quantile_by_sestiere_boot_out[[3]][,2],col="red",lwd=0.5,lty=2)
lines(quantile_by_sestiere_boot_out[[2]][,2]-quantile_by_sestiere_boot_out[[3]][,2],col="red",lwd=0.5,lty=2)
polygon(c(time_series,rev(time_series)),c(quantile_by_sestiere_boot_out[[1]][,2]-quantile_by_sestiere_boot_out[[3]][,2],rev(quantile_by_sestiere_boot_out[[2]][,2]-quantile_by_sestiere_boot_out[[3]][,2])),col=rgb(255,0,0,alpha=100,maxColorValue=255),lty=0)
lines(quantile_by_sestiere_out[,2],col="red",lwd=2)
lines(quantile_by_sestiere_boot_out[[1]][,3]-quantile_by_sestiere_boot_out[[3]][,3],col="blue",lwd=0.5,lty=2)
lines(quantile_by_sestiere_boot_out[[2]][,3]-quantile_by_sestiere_boot_out[[3]][,3],col="blue",lwd=0.5,lty=2)
polygon(c(time_series,rev(time_series)),c(quantile_by_sestiere_boot_out[[1]][,3]-quantile_by_sestiere_boot_out[[3]][,3],rev(quantile_by_sestiere_boot_out[[2]][,3]-quantile_by_sestiere_boot_out[[3]][,3])),col=rgb(0,0,255,alpha=100,maxColorValue=255),lty=0)
lines(quantile_by_sestiere_out[,3],col="blue",lwd=2)
lines(quantile_by_sestiere_boot_out[[1]][,4]-quantile_by_sestiere_boot_out[[3]][,4],col="yellow",lwd=0.5,lty=2)
lines(quantile_by_sestiere_boot_out[[2]][,4]-quantile_by_sestiere_boot_out[[3]][,4],col="yellow",lwd=0.5,lty=2)
polygon(c(time_series,rev(time_series)),c(quantile_by_sestiere_boot_out[[1]][,4]-quantile_by_sestiere_boot_out[[3]][,4],rev(quantile_by_sestiere_boot_out[[2]][,4]-quantile_by_sestiere_boot_out[[3]][,4])),col=rgb(255,255,0,alpha=100,maxColorValue=255),lty=0)
lines(quantile_by_sestiere_out[,4],col="yellow",lwd=2)
lines(quantile_by_sestiere_boot_out[[1]][,5]-quantile_by_sestiere_boot_out[[3]][,5],col="orange",lwd=0.5,lty=2)
lines(quantile_by_sestiere_boot_out[[2]][,5]-quantile_by_sestiere_boot_out[[3]][,5],col="orange",lwd=0.5,lty=2)
polygon(c(time_series,rev(time_series)),c(quantile_by_sestiere_boot_out[[1]][,5]-quantile_by_sestiere_boot_out[[3]][,5],rev(quantile_by_sestiere_boot_out[[2]][,5]-quantile_by_sestiere_boot_out[[3]][,5])),col=rgb(255,165,0,alpha=100,maxColorValue=255),lty=0)
lines(quantile_by_sestiere_out[,5],col="orange",lwd=2)
lines(quantile_by_sestiere_boot_out[[1]][,6]-quantile_by_sestiere_boot_out[[3]][,6],col="pink",lwd=0.5,lty=2)
lines(quantile_by_sestiere_boot_out[[2]][,6]-quantile_by_sestiere_boot_out[[3]][,6],col="pink",lwd=0.5,lty=2)
polygon(c(time_series,rev(time_series)),c(quantile_by_sestiere_boot_out[[1]][,6]-quantile_by_sestiere_boot_out[[3]][,6],rev(quantile_by_sestiere_boot_out[[2]][,6]-quantile_by_sestiere_boot_out[[3]][,6])),col=rgb(255,192,203,alpha=100,maxColorValue=255),lty=0)
lines(quantile_by_sestiere_out[,6],col="pink",lwd=2)
legend(800,0.5,legend=unique(sestiere_df$Sestiere),col=c("green","red","blue","yellow","orange","pink"),lty=1,cex=0.5)

#using boot package
# filled_transposed <- as.data.frame(t(filled[,-1]))
# rownames(filled_transposed) <- colnames(filled[,-1])
# colnames(filled_transposed) <- filled$Date
# 
# start_time <- Sys.time()
# entropyprocedure(filled_transposed,seq(1,52,length.out=52),timestep=1,stat="Shannon")
# end_time <- Sys.time()
# time_taken <- round(end_time-start_time,2)
# time_taken
# 
# set.seed(123)
# results <- boot(data=filled_transposed,statistic=entropyprocedure,R=999,timestep=1,stat="Shannon")
# results
# summary(results)
# plot(results)
# 
# boot.ci(results,type="bca")