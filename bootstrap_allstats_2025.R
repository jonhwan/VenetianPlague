library(progress)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(ggpubr)
library(tidyverse)
library(lubridate)
library(msos)

PDF_approx <- function(x,K=ceiling(log2(length(x)))+1){
  p <- table(cut(x, breaks = K))/length(x)
  sigma2 <- sum((1:K)^2 * p) - sum((1:K) * p)^2
  p <- p[(p>0)] #not sure if this makes sense or not, more obvious increase in Shannon PDF entropy when we do this
  return(p) 
}

PDF_approx_2 <- function(x,K=ceiling(log2(length(x)))+1){
  p <- table(cut(x,breaks=K))/length(x)
  return(p)
}

KL_new <- function(x,y,epsilon=1e-05){
  distance = 0
  pq_ratio = 0
  if(length(x)!=length(y)){
    print("not same length")
    return(NA)
  } else {
    for(i in 1:length(x)){
      if(is.nan(x[i]) | is.nan(y[i])){
        distance = distance + 0
      } else {
        if(x[i]==0 && y[i]==0){
          distance = distance + 0
        } else{
          if(y[i]==0){
            pq_ratio=x[i]/epsilon
          } else{
            pq_ratio = x[i]/y[i]
          }
          if(pq_ratio == 0){
            distance = distance + 0
          } else{
            distance = distance + x[i]*log(pq_ratio)
          }
        }
      }
    }
    return(distance)
  }
}

#outliers <- c("S.Agostin", "S.Polo", "S.Stae","S.Marco")
outliers <- c("S.Polo", "S.Stae","S.Marco") #No cases between 9/1/30 and 12/31/30, outbreak time
filled_modified <- filled[,!(colnames(filled) %in% outliers)]
sestiere_df_modified <- sestiere_df[!(sestiere_df$Parish %in% outliers),]
population_specified_modified <- population_specified[!(population_specified$Parish %in% outliers),]

entropyprocedure_sestiere_3 <- function(original_data,subset,indices,timestep,cumulative=TRUE){ #should just return all of them as a list so I don't have to bootstrap a million times
  boot_data <- original_data[,indices]
  pop_spec <- population_specified_modified[subset-1,][indices,]
  if(cumulative){
    non_pop_corrected <- as.numeric(colSums(boot_data[1:timestep,]))
  } else {
    non_pop_corrected <- as.numeric(boot_data[timestep,])
  }
  cv <- non_pop_corrected/(pop_spec$Population*1000)
  #cv <- cv+1e-10
  cv <- sort(cv)
  cv_norm <- cv/sum(cv)
  kern <- as.numeric(PDF_approx(cv))
  #kern <- as.numeric(PDF_approx_2(cv))
  kern_norm <- kern/sum(kern)
  shannon_PDF <- -1*sum(kern[kern!=0]*log(kern[kern!=0]))
  #shannon_PDF <- -1*sum(kern*log(kern)) #do this if you do cv <- cv+1e-10
  shannon_quantile <- -1*sum(cv_norm[cv_norm!=0]*log(cv_norm[cv_norm!=0]))
  negent_PDF <- negent(cv)
  #negent_PDF <- negent_new(cv)
  uniform_KL_PDF <- as.numeric(KL_new(kern,rep(1/length(kern),length(kern)))) #where do I define KL_new?
  uniform_KL_quantile <- KL_new(cv_norm,rep(1/length(cv_norm),length(cv_norm)))
  return(list(sum = sum(non_pop_corrected)/(sum(pop_spec$Population)*1000),shannonPDF = shannon_PDF, shannonQuantile = shannon_quantile, negentPDF = negent_PDF, uniformKL_PDF = uniform_KL_PDF,uniformKL_Quantile = uniform_KL_quantile))
}

manual_boot_modified_3 <- function(data, subset, N, max_ts,cumulative) {
  sum_mat <- matrix(NA, nrow = N, ncol = max_ts)
  shannonPDF_mat <- matrix(NA, nrow = N, ncol = max_ts)
  shannonQuantile_mat <- matrix(NA, nrow = N, ncol = max_ts)
  negentPDF_mat <- matrix(NA, nrow = N, ncol = max_ts)
  uniformKL_PDF_mat <- matrix(NA, nrow = N, ncol = max_ts)
  uniformKL_Quantile_mat <- matrix(NA, nrow = N, ncol = max_ts)
  for (i in 1:N) {
    sample_index <- sample(ncol(data), replace = TRUE)
    for (t in 1:max_ts) {
      current_list <- entropyprocedure_sestiere_3(data, subset, sample_index, t,cumulative)
      sum_mat[i,t] <- current_list$sum
      shannonPDF_mat[i,t] <- current_list$shannonPDF
      shannonQuantile_mat[i,t] <- current_list$shannonQuantile
      negentPDF_mat[i,t] <- current_list$negentPDF
      uniformKL_PDF_mat[i,t] <- current_list$uniformKL_PDF
      uniformKL_Quantile_mat[i,t] <- current_list$uniformKL_Quantile
    }
  }
  return(list(sum=sum_mat,shannonPDF=shannonPDF_mat,shannonQuantile=shannonQuantile_mat,negentPDF = negentPDF_mat, uniformKL_PDF = uniformKL_PDF_mat, uniformKL_Quantile = uniformKL_Quantile_mat))
}

find_closest_run <- function(all_runs, target_percentile) { #i think this is wrong
  all_runs_sum <- rowSums(all_runs)
  sorted_all_runs_sum <- sort(all_runs_sum)
  index <- match(sorted_all_runs_sum[target_percentile],all_runs_sum)
  return(as.numeric(all_runs[index,]))
}

shannonBySestiere_Bootstrapped_4 <- function(cumulative=TRUE) {
  num_dates <- length(filled_modified$Date)
  sestiere_list <- unique(sestiere_df_modified$Sestiere)
  num_sestieri <- length(sestiere_list)
  st <- Sys.time()
  master_list <- vector(mode="list",length=length(sestiere_list))
  for (s in sestiere_list) {
    cat("Processing:", s, "\n")
    s_index <- match(s, sestiere_list)
    sestiere_parishes <- match(sestiere_df_modified$Parish[sestiere_df_modified$Sestiere == s], colnames(filled_modified))
    
    data_subset <- filled_modified[, sestiere_parishes]
    
    boot_mat <- manual_boot_modified_3(data = data_subset, subset = sestiere_parishes, N = 999, max_ts = num_dates, cumulative=cumulative)
    
    b1_mat <- matrix(NA,ncol=length(boot_mat),nrow=num_dates)
    b2_mat <- matrix(NA,ncol=length(boot_mat),nrow=num_dates)
    closest_runs_025_mat <- matrix(NA,ncol=length(boot_mat),nrow=num_dates)
    closest_runs_975_mat <- matrix(NA,ncol=length(boot_mat),nrow=num_dates)
    #b1[, s_index] <- apply(boot_mat, 2, quantile, probs = 0.025, na.rm = TRUE)
    #b2[, s_index] <- apply(boot_mat, 2, quantile, probs = 0.975, na.rm = TRUE)
    for(j in 1:length(boot_mat)){ #could do all runs but that would be way too many lol
      closest_runs_025_mat[,j] <- find_closest_run(boot_mat[[j]], 25)
      closest_runs_975_mat[,j] <- find_closest_run(boot_mat[[j]], 975)
      for(i in 1:ncol(boot_mat[[j]])){
        sorted <- sort(as.numeric(boot_mat[[j]][,i]))
        b1_mat[i,j] <- sorted[25]
        b2_mat[i,j] <- sorted[975]
      }
    }
    master_list[[match(s,sestiere_list)]] <- list(b1=b1_mat,b2=b2_mat,closest_runs_025 = closest_runs_025_mat,closest_runs_975=closest_runs_975_mat)
  }
  ed <- Sys.time()
  print(ed - st)
  return(master_list)
}

shannonBySestiere_4 <- function(cumulative_boolean=TRUE){
  sum_vec <- rep(0,length(filled_modified$Date))
  shannonPDF_vec <- rep(0,length(filled_modified$Date))
  shannonQuantile_vec <- rep(0,length(filled_modified$Date))
  negentPDF_vec <- rep(0,length(filled_modified$Date))
  uniformKL_PDF_vec <- rep(0,length(filled_modified$Date))
  uniformKL_Quantile_vec <- rep(0,length(filled_modified$Date))
  master_list <- vector(mode="list",length=length(unique(sestiere_df_modified$Sestiere)))
  for(s in unique(sestiere_df_modified$Sestiere)){
    sestiere_parishes <- match(sestiere_df_modified$Parish[sestiere_df_modified$Sestiere == s],colnames(filled_modified))
    for(i in 1:length(filled_modified$Date)){
      current_list <- entropyprocedure_sestiere_3(filled_modified[,sestiere_parishes],subset=sestiere_parishes,seq(1,length(sestiere_parishes),length.out=length(sestiere_parishes)),timestep=i,cumulative=cumulative_boolean)
      sum_vec[i] <- current_list$sum
      shannonPDF_vec[i] <- current_list$shannonPDF
      shannonQuantile_vec[i] <- current_list$shannonQuantile
      negentPDF_vec[i] <- current_list$negentPDF
      uniformKL_PDF_vec[i] <- current_list$uniformKL_PDF
      uniformKL_Quantile_vec[i] <- current_list$uniformKL_Quantile
    }
    master_list[[match(s,unique(sestiere_df_modified$Sestiere))]] <- list(sum = sum_vec,shannonPDF = shannonPDF_vec,shannonQuantile = shannonQuantile_vec,negentPDF = negentPDF_vec, uniformKL_PDF = uniformKL_PDF_vec,uniformKL_Quantile=uniformKL_Quantile_vec)
  }
  return(master_list)
}

whole_dataset_analysis <- function(cumulative=TRUE){
  sum_vec <- rep(0,length(filled_modified$Date))
  shannonPDF_vec <- rep(0,length(filled_modified$Date))
  shannonQuantile_vec <- rep(0,length(filled_modified$Date))
  negentPDF_vec <- rep(0,length(filled_modified$Date))
  uniformKL_PDF_vec <- rep(0,length(filled_modified$Date))
  uniformKL_Quantile_vec <- rep(0,length(filled_modified$Date))
  st <- Sys.time()
  pb <- progress_bar$new(total=length(filled_modified$Date))
  for(i in 1:length(filled_modified$Date)){
    current_list <- entropyprocedure_sestiere_3(filled_modified[,-1],subset=2:ncol(filled_modified),indices=1:ncol(filled_modified[,-1]), timestep=i,cumulative)
    sum_vec[i] <- current_list$sum
    shannonPDF_vec[i] <- current_list$shannonPDF
    shannonQuantile_vec[i] <- current_list$shannonQuantile
    negentPDF_vec[i] <- current_list$negentPDF
    uniformKL_PDF_vec[i] <- current_list$uniformKL_PDF
    uniformKL_Quantile_vec[i] <- current_list$uniformKL_Quantile
    pb$tick()
  }
  ed <- Sys.time()
  print(ed - st)
  return(list(sum=sum_vec,shannonPDF=shannonPDF_vec,shannonQuantile=shannonQuantile_vec,negentPDF = negentPDF_vec, uniformKL_PDF = uniformKL_PDF_vec, uniformKL_Quantile = uniformKL_Quantile_vec))
}

whole_dataset_bootstrap <- function(cumulative=TRUE){
  num_dates <- length(filled_modified$Date)
  b1 <- rep(0,num_dates) # 2.5%
  b2 <- rep(0,num_dates) # 97.5%
  boot_mat <- manual_boot_modified_3(data = filled_modified[,-1], subset = 2:ncol(filled_modified), N = 999, max_ts = num_dates, cumulative=cumulative)
  b1_list <- vector(mode="list",length=length(boot_mat))
  b2_list <- vector(mode="list",length=length(boot_mat))
  closest_runs_025_list <- vector(mode="list",length(boot_mat))
  closest_runs_975_list <- vector(mode="list",length(boot_mat))
    #b1[, s_index] <- apply(boot_mat, 2, quantile, probs = 0.025, na.rm = TRUE)
    #b2[, s_index] <- apply(boot_mat, 2, quantile, probs = 0.975, na.rm = TRUE)
  for(j in 1:length(boot_mat)){ #could do all runs but that would be way too many lol
    closest_025 <- find_closest_run(boot_mat[[j]], 25)
    closest_975 <- find_closest_run(boot_mat[[j]], 975)
    for(i in 1:ncol(boot_mat[[j]])){
      sorted <- sort(as.numeric(boot_mat[[j]][,i]))
      b1[i] <- sorted[25]
      b2[i] <- sorted[975]
    }
    b1_list[[j]] <- b1
    b2_list[[j]] <- b2
    closest_runs_025_list[[j]] <- closest_025
    closest_runs_975_list[[j]] <- closest_975
  }
  return(list(b1=b1_list,b2=b2_list,closest_runs_025=closest_runs_025_list,closest_runs_975=closest_runs_975_list))
}

stats_by_sestiere_boot_cumulative_50_2 <- shannonBySestiere_Bootstrapped_4(cumulative=TRUE) #w/ fixed negent
stats_by_sestiere_boot_instantaneous_50_2 <- shannonBySestiere_Bootstrapped_4(cumulative=FALSE)
stats_by_sestiere_cumulative_50_2 <- shannonBySestiere_4(cumulative=TRUE)
stats_by_sestiere_instantaneous_50_2 <- shannonBySestiere_4(cumulative=FALSE)
stats_of_whole_cumulative_50_2 <- whole_dataset_analysis(cumulative=TRUE) #default
stats_of_whole_instantaneous_50_2 <- whole_dataset_analysis(cumulative=FALSE)
stats_of_whole_boot_cumulative_50_2 <- whole_dataset_bootstrap(cumulative=TRUE)
stats_of_whole_boot_instantaneous_50_2 <- whole_dataset_bootstrap(cumulative=FALSE)

saveRDS(stats_by_sestiere_boot_cumulative_50_2,"stats_by_sestiere_boot_cumulative_50_2.rds")
saveRDS(stats_by_sestiere_boot_instantaneous_50_2,"stats_by_sestiere_boot_instantaneous_50_2.rds")
saveRDS(stats_of_whole_boot_cumulative_50_2,"stats_of_whole_boot_cumulative_50_2.rds")
saveRDS(stats_of_whole_boot_instantaneous_50_2,"stats_of_whole_boot_instantaneous_50_2.rds")

# stats_by_sestiere_boot_cumulative_50 <- shannonBySestiere_Bootstrapped_4(cumulative=TRUE) #w/ fixed negent
# stats_by_sestiere_boot_instantaneous_50 <- shannonBySestiere_Bootstrapped_4(cumulative=FALSE)
# stats_by_sestiere_cumulative_50 <- shannonBySestiere_4(cumulative=TRUE)
# stats_by_sestiere_instantaneous_50 <- shannonBySestiere_4(cumulative=FALSE)
# stats_of_whole_cumulative_50 <- whole_dataset_analysis(cumulative=TRUE) #default
# stats_of_whole_instantaneous_50 <- whole_dataset_analysis(cumulative=FALSE)
# stats_of_whole_boot_cumulative_50 <- whole_dataset_bootstrap(cumulative=TRUE)
# stats_of_whole_boot_instantaneous_50 <- whole_dataset_bootstrap(cumulative=FALSE)

# stats_by_sestiere_boot_cumulative_4 <- shannonBySestiere_Bootstrapped_4(cumulative=TRUE) #w/ fixed negent
# stats_by_sestiere_boot_instantaneous_2 <- shannonBySestiere_Bootstrapped_4(cumulative=FALSE)
# stats_by_sestiere_cumulative_2 <- shannonBySestiere_4(cumulative=TRUE)
# stats_by_sestiere_instantaneous_2 <- shannonBySestiere_4(cumulative=FALSE)
# stats_of_whole_cumulative_4 <- whole_dataset_analysis(cumulative=TRUE) #default
# stats_of_whole_instantaneous_2 <- whole_dataset_analysis(cumulative=FALSE)
# stats_of_whole_boot_cumulative_2 <- whole_dataset_bootstrap(cumulative=TRUE)
# stats_of_whole_boot_instantaneous_2 <- whole_dataset_bootstrap(cumulative=FALSE)

# stats_by_sestiere_boot_cumulative <- shannonBySestiere_Bootstrapped_4(cumulative=TRUE)
# stats_by_sestiere_boot_instantaneous <- shannonBySestiere_Bootstrapped_4(cumulative=FALSE)
# stats_by_sestiere_cumulative <- shannonBySestiere_4(cumulative=TRUE)
# stats_by_sestiere_instantaneous <- shannonBySestiere_4(cumulative=FALSE)
# stats_of_whole_cumulative <- whole_dataset_analysis(cumulative=TRUE) #default
# stats_of_whole_instantaneous <- whole_dataset_analysis(cumulative=FALSE)
# stats_of_whole_boot_cumulative <- whole_dataset_bootstrap(cumulative=TRUE)
# stats_of_whole_boot_instantaneous <- whole_dataset_bootstrap(cumulative=FALSE)

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