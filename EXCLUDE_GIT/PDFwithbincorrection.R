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

shannon_PDF_vec <- c()
negent_PDF_vec <- c()

for(timestep in 1:1095){
  non_pop_corrected <- as.numeric(colSums(filled_modified[1:timestep,-1]))
  case_vector <- non_pop_corrected/(population_specified_modified$Population*1000)
  case_vector <- sort(case_vector)
  case_vector_norm <- case_vector/sum(case_vector)
  kernel <- as.numeric(PDF_approx(case_vector_norm))
  kernel <- kernel/sum(kernel)
  binwidth <- (max(case_vector_norm) - min(case_vector_norm)) / (ceiling(log2(length(case_vector_norm))) + 1)
  shannon_PDF <- -1*sum(kernel[kernel!=0]*log(kernel[kernel!=0]))-log(binwidth)
  negent_PDF <- negent(case_vector)+log(binwidth)
  shannon_PDF_vec <- c(shannon_PDF_vec,shannon_PDF)
  negent_PDF_vec <- c(negent_PDF_vec,negent_PDF)
}
plot.ts(shannon_PDF_vec)
plot.ts(negent_PDF_vec)