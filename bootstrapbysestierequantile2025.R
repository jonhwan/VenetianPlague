library(progress)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(ggpubr)
library(tidyverse)

outliers <- c("S.Agostin", "S.Polo", "S.Stae","S.Marco")
filled_modified <- filled[,!(colnames(filled) %in% outliers)]
sestiere_df_modified <- sestiere_df[!(sestiere_df$Parish %in% outliers),]
population_specified_modified <- population_specified[!(population_specified$Parish %in% outliers),]

entropyprocedure_sestiere_2 <- function(original_data,subset,indices,timestep,stat=c("Shannon","Cases","Negent","UniformKL","Shannon_and_Cases","Negent_and_Cases")){ #should just return all of them as a list so I don't have to bootstrap a million times
  boot_data <- original_data[,indices]
  pop_spec <- population_specified_modified[subset-1,][indices,]
  cv <- as.numeric(colSums(boot_data[1:timestep,]))/(pop_spec$Population*1000)
  sum_cv <- sum(cv)
  if (sum_cv == 0) {
    #print(paste("Zero CV at timestep", timestep))
    return(0)
  }
  if(stat == "Cases"){
    return(sum_cv)
  } else{
    cv <- cv + 1e-10
    cv_norm <- cv/sum_cv
    #kern <- PDF_approx(cv_norm)
    #kern <- PDF_approx(cv)
    kern <- sort(cv)
    kern <- kern/sum(kern) #is this the same as doing cv_norm inside the density function?
    kern_nz <- which(kern != 0)
    if(stat == "Shannon_and_Cases"){
      #s <- -1*sum(kern[kern_nz]*log(kern[kern_nz]/(max(cv)/512)))  #standard bias correction from bins (although that's for histogram approximation, not sure if it works the same here)
      s <- -1*sum(kern[kern_nz]*log(kern[kern_nz]))
      return(c(s,sum(cv)))
    } else if(stat == "Shannon"){
      s <- -1*sum(kern[kern_nz]*log(kern[kern_nz]))
      return(s)
    } else if(stat == "Negent" | stat == "Negent_and_Cases"){
      x <- seq(0,max(cv),length.out=length(kern)) 
      y <- dnorm(x,mean=mean(kern),sd=sd(kern))
      y <- y/sum(y)
      #v <- KL(kern,y) #how on earth do we take the negent of the quantile?
      v <- negent(cv) #using negent function
      if(stat == "Negent"){
        return(v)
      } else {
        return(c(v,sum(cv)))
      }
    } else if(stat == "UniformKL"){
      u <- rep(1/length(kern),length(kern))
      w <- KL_new(kern,u)
      return(w)
    }
  }
}

manual_boot_modified_2 <- function(data, subset, N, max_ts, st) {
  boot_runs <- matrix(NA, nrow = N, ncol = max_ts)
  for (i in 1:N) {
    sample_index <- sample(ncol(data), replace = TRUE)
    for (t in 1:max_ts) {
      boot_runs[i, t] <- entropyprocedure_sestiere_2(data, subset, sample_index, t, st)
    }
  }
  return(boot_runs)
}

find_closest_run <- function(all_runs, target_percentile) { #i think this is wrong
  all_runs_sum <- rowSums(all_runs)
  sorted_all_runs_sum <- sort(all_runs_sum)
  index <- match(sorted_all_runs_sum[target_percentile],all_runs_sum)
  return(as.numeric(all_runs[index,]))
}

find_closest_run_2 <- function(all_runs, target_percentile, true_run) { #wait this is definitely wrong lol
  distances <- rep(0,nrow(all_runs))
  for(i in 1:nrow(all_runs)){
    temp_sum <- 0
    for(j in 1:length(filled$Date)){
      temp_sum <- temp_sum+all_runs[i,j]-true_run[j]
    }
    distances[i] <- temp_sum
  }
  sorted_distances <- sort(distances)
  index <- match(sorted_distances[target_percentile],distances)
  return(all_runs[index,])
}

shannonBySestiere_Bootstrapped_3 <- function() {
  num_dates <- length(filled_modified$Date)
  sestiere_list <- unique(sestiere_df_modified$Sestiere)
  num_sestieri <- length(sestiere_list)
  
  b1 <- matrix(NA, nrow = num_dates, ncol = num_sestieri) # 2.5%
  b2 <- matrix(NA, nrow = num_dates, ncol = num_sestieri) # 97.5%
  closest_runs_025 <- matrix(NA, nrow = num_dates, ncol = num_sestieri)
  closest_runs_975 <- matrix(NA, nrow = num_dates, ncol = num_sestieri)
  
  st <- Sys.time()
  for (s in sestiere_list) {
    cat("Processing:", s, "\n")
    s_index <- match(s, sestiere_list)
    sestiere_parishes <- match(sestiere_df_modified$Parish[sestiere_df_modified$Sestiere == s], colnames(filled_modified))
    
    data_subset <- filled_modified[, sestiere_parishes]
    
    boot_mat <- manual_boot_modified_2(data = data_subset, subset = sestiere_parishes, N = 999, max_ts = num_dates, st = "Shannon")
    
    #b1[, s_index] <- apply(boot_mat, 2, quantile, probs = 0.025, na.rm = TRUE)
    #b2[, s_index] <- apply(boot_mat, 2, quantile, probs = 0.975, na.rm = TRUE)
    for(i in 1:ncol(boot_mat)){
      sorted <- sort(as.numeric(boot_mat[,i]))
      b1[i,s_index] <- sorted[25]
      b2[i,s_index] <- sorted[975]
    }
    
    closest_025 <- find_closest_run(boot_mat, 25)
    closest_975 <- find_closest_run(boot_mat, 975)
    
    closest_runs_025[, s_index] <- closest_025
    closest_runs_975[, s_index] <- closest_975
  }
  ed <- Sys.time()
  print(ed - st)
  
  return(list(
    percentile_025 = as.data.frame(b1),
    percentile_975 = as.data.frame(b2),
    closest_025 = as.data.frame(closest_runs_025),
    closest_975 = as.data.frame(closest_runs_975),
    all_runs = as.data.frame(boot_mat)
  ))
}

shannonBySestiere_3 <- function(){
  out_vec <- as.data.frame(matrix(nrow=length(filled_modified$Date),ncol=length(unique(sestiere_df_modified$Sestiere))))
  st <- Sys.time()
  for(s in unique(sestiere_df_modified$Sestiere)){
    pb <- progress_bar$new(total=length(filled_modified$Date))
    sestiere_parishes <- match(sestiere_df_modified$Parish[sestiere_df_modified$Sestiere == s],colnames(filled_modified))
    for(i in 1:length(filled_modified$Date)){
      out_vec[i,match(s,unique(sestiere_df_modified$Sestiere))] <- entropyprocedure_sestiere_2(filled_modified[,sestiere_parishes],subset=sestiere_parishes,seq(1,length(sestiere_parishes),length.out=length(sestiere_parishes)),timestep=i,stat="Shannon")
      pb$tick()
    }
  }
  ed <- Sys.time()
  print(ed - st)
  return(out_vec)
}

shannon_of_quantile <- function(stat_of_choice){
  out_vec <- rep(0,length(filled_modified$Date))
  st <- Sys.time()
  pb <- progress_bar$new(total=length(filled_modified$Date))
  for(i in 1:length(filled_modified$Date)){
    out_vec[i] <- entropyprocedure_sestiere_2(filled_modified[,-1],subset=2:50,indices=1:49, timestep=i,stat=stat_of_choice)
    pb$tick()
  }
  ed <- Sys.time()
  print(ed - st)
  return(out_vec)
}

x_3 <- shannonBySestiere_Bootstrapped_3()
y_3 <- shannonBySestiere_3()
individual = FALSE
time_series <- 1:1095
series_names <- unique(sestiere_df_modified$Sestiere)

# Ensure matrices are data.frames
lower_df <- as.data.frame(x_3$closest_025)
upper_df <- as.data.frame(x_3$closest_975)
observed_df <- as.data.frame(y_3)

# Add time column
lower_df$time <- upper_df$time <- observed_df$time <- time_series

# Pivot longer
lower_long <- pivot_longer(lower_df, -time, names_to = "series", values_to = "lower")
upper_long <- pivot_longer(upper_df, -time, names_to = "series", values_to = "upper")
obs_long   <- pivot_longer(observed_df, -time, names_to = "series", values_to = "observed")

# Combine all into one long dataframe
plot_df <- lower_long %>%
  left_join(upper_long, by = c("time", "series")) %>%
  left_join(obs_long, by = c("time", "series")) %>%
  mutate(series = factor(series, levels = paste0("V", 1:6), labels = series_names))

ggplot(plot_df, aes(x = rep(filled$Date,each=6), group = series, color = series, fill = series)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.4, color = NA) +
  geom_line(aes(y = lower), linetype = "dashed", linewidth = 0.5) +
  geom_line(aes(y = upper), linetype = "dashed", linewidth = 0.5) +
  geom_line(aes(y = observed), linewidth = 1) +
  scale_color_manual(values = c("green", "red", "blue", "yellow", "orange", "pink")) +
  scale_fill_manual(values = c("green", "red", "blue", "yellow", "orange", "pink")) +
  theme(plot.background=element_rect(fill="white")) +
  labs(
    title = "Shannon Entropy of Quantile of Cumulative Mortality by Sestiere",
    x = "Date",
    y = "H",
    color = "Sestiere",
    fill = "Sestiere"
  ) 
#ggsave("ShannonCumulativeQuantileSestieri.png",width=2100,height=1500,units="px")

plot.ts(x_3$closest_025)
#plot.ts(x_3$closest_025[,1]-x_3[[3]][,1],col="green",lwd=0.5,ylim=c(-9,2),lty=2)
plot.ts(x_3$closest_025[,1], col = "green", lwd = 0.5, lty = 2,ylim=c(0,2.5))
lines(x_3$closest_975[,1], col = "green", lwd = 0.5, lty = 2)
polygon(c(time_series,rev(time_series)),c(x_3$closest_025[,1],rev(x_3$closest_975[,1])),col=rgb(0,255,0,maxColorValue=255,alpha=100),lty=0)
lines(y_3[,1],col="green",lwd=2)
if (individual){
  plot.ts(x_3$closest_025[,2],col="red",lwd=0.5,lty=2,ylim=c(0,2.5))
} else{
  lines(x_3$closest_025[,2],col="red",lwd=0.5,lty=2)
}
lines(x_3$closest_975[,2],col="red",lwd=0.5,lty=2)
polygon(c(time_series,rev(time_series)),c(x_3$closest_025[,2],rev(x_3$closest_975[,2])),col=rgb(255,0,0,maxColorValue=255,alpha=100),lty=0)
lines(y_3[,2],col="red",lwd=2)
if (individual){
  plot.ts(x_3$closest_025[,3],col="blue",lwd=0.5,lty=2,ylim=c(0,2.5))
} else{
  lines(x_3$closest_025[,3],col="blue",lwd=0.5,lty=2)
}
lines(x_3$closest_975[,3],col="blue",lwd=0.5,lty=2)
polygon(c(time_series,rev(time_series)),c(x_3$closest_025[,3],rev(x_3$closest_975[,3])),col=rgb(0,0,255,maxColorValue=255,alpha=100),lty=0)
lines(y_3[,3],col="blue",lwd=2)
if (individual){
  plot.ts(x_3$closest_025[,4],col="yellow",lwd=0.5,lty=2,ylim=c(0,2.5))
} else{
  lines(x_3$closest_025[,4],col="yellow",lwd=0.5,lty=2)
}
lines(x_3$closest_975[,4],col="yellow",lwd=0.5,lty=2)
polygon(c(time_series,rev(time_series)),c(x_3$closest_025[,4],rev(x_3$closest_975[,4])),col=rgb(255,255,0,maxColorValue=255,alpha=100),lty=0)
lines(y_3[,4],col="yellow",lwd=2)
if (individual){
  plot.ts(x_3$closest_025[,5],col="orange",lwd=0.5,lty=2,ylim=c(0,2.5))
} else{
  lines(x_3$closest_025[,5],col="orange",lwd=0.5,lty=2)
}
lines(x_3$closest_975[,5],col="orange",lwd=0.5,lty=2)
polygon(c(time_series,rev(time_series)),c(x_3$closest_025[,5],rev(x_3$closest_975[,5])),col=rgb(255,165,0,maxColorValue=255,alpha=100),lty=0)
lines(y_3[,5],col="orange",lwd=2)
if (individual){
  plot.ts(x_3$closest_025[,6],col="pink",lwd=0.5,lty=2,ylim=c(0,2.5))
} else{
  lines(x_3$closest_025[,6],col="pink",lwd=0.5,lty=2)
}
lines(x_3$closest_975[,6],col="pink",lwd=0.5,lty=2)
polygon(c(time_series,rev(time_series)),c(x_3$closest_025[,6],rev(x_3$closest_975[,6])),col=rgb(255,192,203,maxColorValue=255,alpha=100),lty=0)
lines(y_3[,6],col="pink",lwd=2)
legend(900,0.9,legend=unique(sestiere_df_modified$Sestiere),col=c("green","red","blue","yellow","orange","pink"),lty=1,cex=0.5)


shannon_overall_bootstrapped_2 <- function() {
  num_dates <- length(filled_modified$Date)
  
  b1 <- rep(0,num_dates) # 2.5%
  b2 <- rep(0,num_dates) # 97.5%
  closest_runs_025 <- rep(0,num_dates)
  closest_runs_975 <- rep(0,num_dates)
  
  st <- Sys.time()
  
  data_subset <- filled_modified[,-1]
  
  boot_mat <- manual_boot_modified_2(data = data_subset, subset = 2:50, N = 999, max_ts = num_dates, st = "Shannon")
  
  #b1[, s_index] <- apply(boot_mat, 2, quantile, probs = 0.025, na.rm = TRUE)
  #b2[, s_index] <- apply(boot_mat, 2, quantile, probs = 0.975, na.rm = TRUE)
  for(i in 1:ncol(boot_mat)){
    sorted <- sort(as.numeric(boot_mat[,i]))
    b1[i] <- sorted[25]
    b2[i] <- sorted[975]
  }
  
  closest_025 <- find_closest_run(boot_mat, 25)
  closest_975 <- find_closest_run(boot_mat, 975)
  
  closest_runs_025 <- closest_025
  closest_runs_975 <- closest_975
  ed <- Sys.time()
  print(ed - st)
  
  return(list(
    percentile_025 = b1,
    percentile_975 = b2,
    closest_025 = closest_runs_025,
    closest_975 = closest_runs_975,
    all_runs = as.data.frame(boot_mat)
  ))
}
z_4 <- shannon_overall_bootstrapped_2()
a_4 <- shannon_of_quantile("Shannon")
plot.ts(z_4$closest_975,lty=2,lwd=0.5,col="grey",ylim=c(2,4))
lines(z_4$closest_025,lty=2,lwd=0.5,col="grey")
polygon(c(time_series,rev(time_series)),c(z_4$closest_025,rev(z_4$closest_975)),col=rgb(0,0,0,maxColorValue=255,alpha=100),lty=0)
lines(a_4,lty=1,lwd=2,col="black")
plot.ts(z_4$percentile_975,lty=2,lwd=0.5,col="grey",ylim=c(2,4))
lines(z_4$percentile_025,lty=2,lwd=0.5,col="grey")
polygon(c(time_series,rev(time_series)),c(z_4$percentile_025,rev(z_4$percentile_975)),col=rgb(0,0,0,maxColorValue=255,alpha=100),lty=0)
lines(a_4,lty=1,lwd=2,col="black")
beeg_2 <- as.data.frame(z_4[-5])
beeg_2$Actual <- a_4
shannon_pdf_bootstrapped <- ggplot(data=beeg_2,aes(x=filled$Date,y=Actual))+
  geom_line()+
  geom_ribbon(aes(ymin=closest_025,ymax=closest_975),alpha=0.5)+
  scale_x_date(date_breaks="6 months",date_labels="%m-%Y")+
  scale_y_continuous()+
  expand_limits(y=c(2,4))+
  theme(plot.background=element_rect(fill="white"))+
  labs(y="H",x="Date",title="Shannon Entropy of Quantile of Cumulative Mortality")
print(shannon_pdf_bootstrapped)
#ggsave("ShannonCumulativeQuantile.png",width=2100,height=1500,units="px")