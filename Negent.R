#Exploring Negent
library(msos)
library(philentropy)
daily_case_vector_2 <- c()
daily_negent_2 <- c()
daily_gaussian_KL <- c()
daily_uniform_KL <- c()
negent_test_vec <- c()
for(date in filled$Date){
  daily_case_vector_2 <- as.numeric(filled[match(date,filled$Date),-1])
  daily_negent_2 <- c(daily_negent_2,negent(daily_case_vector_2/(population_specified$Population*1000))) #KL
  daily_uniform_KL <- c(daily_uniform_KL,uniformKL_new(daily_case_vector_2,population_specified))
  daily_gaussian_KL <- c(daily_gaussian_KL,gaussianKL_v2(daily_case_vector_2,population_specified))
  #diagnostic_negent(daily_case_vector_2/(population_specified$Population*1000))
  negent_test_vec <- c(negent_test_vec, negent_test(daily_case_vector_2,population_specified))
}
plot.ts(daily_gaussian_KL)
plot.ts(negent_test_vec)
#plot.ts(daily_uniform_KL)
plot.ts(daily_negent_2)

diagnostic_negent <- function(x,K=ceiling(log2(length(x))+1)){
  p <- table(cut(x, breaks = K))/length(x)
  sigma2 <- sum((1:K)^2 * p) - sum((1:K) * p)^2
  p <- p[(p>0)]
  print(p)
  (1 + log(2 * pi * (sigma2 + 1/12)))/2 + sum(p * log(p))
}

KL_new <- function(x,y,epsilon=1e-05){
  distance = 0
  pq_ratio = 0
  if(length(x)!=length(y)){
    print("not same length")
  } else {
    for(i in 1:length(x)){
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
    return(distance)
  }
}

negent_test <- function(case_vec,pop_spec){
  population_corrected <- case_vec/(pop_spec$Population*1000)
  daily_counts <- hist(population_corrected,breaks=seq(0,max(population_corrected),length.out=ceiling(log2(length(population_corrected)+1))),plot=FALSE)$counts/sum(hist(population_corrected,breaks=seq(0,max(population_corrected),length.out=length(population_corrected)+1),plot=FALSE)$counts)
  m <- mean(daily_counts)
  s <- sd(daily_counts)
  x <- seq(0,max(population_corrected),length.out=ceiling(log2(length(population_corrected)+1))-1)
  y <- dnorm(x,mean=m,sd=s)/sum(dnorm(x,mean=m,sd=s))
  #print(daily_counts)
  #print(matrix(c(daily_counts,y),nrow=2,byrow=TRUE))
  #print(philentropy::KL(matrix(c(daily_counts,y),nrow=2,byrow=TRUE),unit="log"))
  return(KL_new(daily_counts,y))
  #return(-1*sum(daily_counts*log(y/daily_counts)))
  #return(KL(population_corrected/sum(population_corrected),y))
}

uniformKL_new <- function(case_vec,pop_spec){
  CV_norm <- case_vec/(pop_spec$Population*1000)
  CV_norm <- CV_norm/sum(CV_norm)
  #CV_norm <- CV_norm[which(CV_norm!=0)]#when you don't count the zeroes it becomes interesting, otherwise it's literally a straight line
  uniform_dist <- rep(c(1/length(CV_norm)),length(CV_norm)) 
  if(length(CV_norm) == 0){
    return(0)
  }
  return(KL_new(CV_norm,uniform_dist))
}

