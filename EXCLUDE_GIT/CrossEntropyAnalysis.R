population <- data.frame(population_post$Parish,interpolated_1629/1000) #for interpolated population data for 1629
colnames(population) <- c("Parish","Population")
population_specified <- population[population$Parish %in% colnames(filled[,-1]),]
population_specified <- population_specified[match(colnames(filled[,-1]),population_specified$Parish),]

uniform_CE <- function(case_vector){
  CV_norm <- case_vector/(population_specified$Population*1000)
  CV_norm <- CV_norm/sum(CV_norm)
  CV_norm <- CV_norm[which(CV_norm!=0)]#when you don't count the zeroes it becomes interesting, otherwise it's literally a straight line
  uniform_dist <- rep(c(1/length(CV_norm)),length(CV_norm)) 
  summand <- 0
  for(i in 1:length(CV_norm)){
    summand <- summand+CV_norm[i]*log(uniform_dist[i])
  }
  return(-1*summand)
} #make a uniform vs cumulative PDF bar chart to show the cumulative is not approaching normal. What to do with CDF? How to take entropy of CDF?

uniform_CE_cumulative <- function(case_vector,date){
  CV_norm <- colSums(filled[1:match(date,filled$Date),-1])/(population_specified$Population*1000)
  CV_norm <- CV_norm/sum(CV_norm)
  CV_norm <- CV_norm[which(CV_norm!=0)]
  uniform_dist <- rep(c(1/length(CV_norm),length(CV_norm)))
  return(-1*sum(CV_norm*log(uniform_dist)))
}

shannon_entropy <- function(case_vector){ #by the way this isn't shannon, this is ni log pi but shannon is pi log pi
  CV_norm <- case_vector/sum(case_vector)
  summand <- 0
  for(i in 1:length(case_vector)){
    if(CV_norm[i] == 0){
      next
    }
    summand <- summand+case_vector[i]*log(CV_norm[i])
  }
  return(-1*summand)
}

normalized_shannon <- function(case_vector,pop){
  CV_norm <- case_vector/sum(case_vector)
  population_specified <- pop[pop$Parish %in% colnames(filled[,-1]),]
  population_specified <- population_specified[match(colnames(filled[,-1]),population_specified$Parish),]
  population_specified_modified <- population_specified
  population_specified_modified$Population <- population_specified_modified$Population/sum(population_specified_modified$Population)
  summand <- 0
  for(i in 1:length(case_vector)){
    if(CV_norm[i] == 0){
      next #if p=0, we treat shannon entropy as 0 (same as adding 0)
    }
    summand <- summand+case_vector[i]*log(CV_norm[i]/(population_specified_modified[i,match("Population",colnames(population_specified_modified))]))
  }
  return(-1*summand)
}

per_case_shannon <- function(case_vector,pop,names){
  CV_norm <- case_vector/sum(case_vector)
  pop_specified <- pop[pop$Parish %in% colnames(filled[,-1]),]
  pop_specified <- pop_specified[match(colnames(filled[,-1]),pop_specified$Parish),]
  pop_specified$Population <- pop_specified$Population/sum(pop_specified$Population)
  summand <- 0
  for(i in 1:length(case_vector)){
    if(CV_norm[i] == 0){
      next #if p=0, we treat shannon entropy as 0 (same as adding 0)
    }
    summand <- summand + (CV_norm[i]*log(CV_norm[i]/pop_specified[i,match("Population",colnames(population_specified))])) #need to divide by population INSIDE the log, and need to normalize population. Sum of CV_norm/pop_specified isn't 1, so not probs.
  }
  return(-1*summand) #Why is there so much variability? Also scaling by population doesn't seem to do much either...should plot that too
}

per_case_shannon_2 <- function(case_vector,pop,names){ #going to try normalizing p/q rather than p and q separately
  CV_norm <- case_vector/sum(case_vector)
  pop_specified <- pop[pop$Parish %in% colnames(filled[,-1]),]
  pop_specified <- pop_specified[match(colnames(filled[,-1]),pop_specified$Parish),]
  p_by_q <- case_vector/(pop_specified$Population*1000)
  p_by_q_norm <- p_by_q/sum(p_by_q)
  summand <- 0
  for(i in 1:length(case_vector)){
    if(p_by_q_norm[i] == 0){
      next
    }
    #summand <- summand + CV_norm[i]*log(p_by_q_norm[i]) #CV_norm on outside or p_by_q_norm?
    summand <- summand + p_by_q_norm[i]*log(p_by_q_norm[i])
  }
  return(-1*summand)
}

not_per_case_shannon <- function(case_vector,pop,names){
  CV_norm <- case_vector/sum(case_vector)
  pop_specified <- pop[pop$Parish %in% colnames(filled[,-1]),]
  pop_specified <- pop_specified[match(colnames(filled[,-1]),pop_specified$Parish),]
  pop_spec_normalized <- pop_specified$Population/sum(pop_specified$Population)
  p_by_q <- case_vector/(pop_specified$Population*1000)
  p_by_q_norm <- p_by_q/sum(p_by_q)
  summand <- 0
  for(i in 1:length(case_vector)){
    if(p_by_q_norm[i] == 0){
      next
    }
    summand <- summand + p_by_q[i]*log(p_by_q_norm[i])
  }
  return(-1*summand)
}

exponential_CE <- function(case_vector){ #actually kind of works but follows the wrong idea of expected value as the dice roll
  a = 1
  b = length(case_vector)
  B = mean(case_vector)
  z <- function(lamb){ #refer to https://bjlkeng.io/posts/macase_vectorimum-entropy-distributions/
    s = 0
    for (k in a:b){ #trying to figure out if it should be in a:b or in case_vector_sorted
      s = s + exp(-k*lamb)
    }
    return(1/s)
  }
  f <- function(lamb,B.=B){
    y = 0
    for (k in a:b){
      y = y + k*exp(-k*lamb)
    }
    return(k*z(lamb)-B.)
  }
  p <- function(k,lamb){
    return(z(lamb)*exp(-k*lamb))
  }
  lamb = uniroot.all(f,c(-20,20))
  case_vector_new = case_vector/sum(case_vector)
  norm = rep(0,length(case_vector))
  for(k in a:b){
    norm[k] = p(k,lamb)
  }
  return(-1*sum(case_vector_new*log(norm)))
}

normal_CE <- function(case_vector,date){ #have been doing daily for all the other ones, but want to try cumulative and daily for this one.
  population_corrected <- case_vector/(population_specified$Population*1000)
  m <- mean(population_corrected) #for some reason, the peak isn't matched up on Dec 11, 1631 (0.4 on ggplot, near 0.3 on hist). May have something to do with expanding the axes, because on both they're on the same part of the axes, the axes are just scaled differently.
  s <- sd(population_corrected)
  x <- seq(0,max(population_corrected),length.out=length(population_corrected)) #n. of bins being same as n. of parishes is arbitrary. but it does come with the nice quirk that every bin can be filled once.
  y <- dnorm(x,mean=m,sd=s)/sum(dnorm(x,mean=m,sd=s)) #do we need to give it room to breathe in the negative x since sometimes it is really close to 0 so some part of the distribution lives in the -x part?
  daily_counts <- hist(population_corrected,breaks=seq(0,max(population_corrected),length.out=length(population_corrected)+1),plot=FALSE)$counts/sum(hist(population_corrected,breaks=seq(0,max(population_corrected),length.out=length(population_corrected)+1),plot=FALSE)$counts)
  # plot(x,y,type="l",lwd=1)
  cumulative_pop_corrected <- colSums(filled[1:match(date,filled$Date),-1])/(population_specified$Population*1000)
  cumulative_counts <- hist(cumulative_pop_corrected,breaks=seq(0,max(cumulative_pop_corrected),length.out=length(cumulative_pop_corrected)+1),plot=FALSE)$counts/sum(hist(cumulative_pop_corrected,breaks=seq(0,max(cumulative_pop_corrected),length.out=length(cumulative_pop_corrected)+1),plot=FALSE)$counts)
  m_2 <- mean(cumulative_pop_corrected)
  s_2 <- sd(cumulative_pop_corrected)
  x_2 <- seq(0,max(cumulative_pop_corrected),length.out=length(cumulative_pop_corrected))
  # plot(x_2,cumulative_counts,type="l")
  y_2 <- dnorm(x_2,mean=m_2,sd=s_2)/sum(dnorm(x_2,mean=m_2,sd=s_2))
  # plot(x_2,y_2)
  return(c(-1*sum(daily_counts*log(y)),-1*sum(cumulative_counts*log(y_2)))) ##How to do ni log pi? counts of the histogram is always 53 but sum of case vector varies (we want it to be proportional to sum of case vector I believe).
}

CE_df <- as.data.frame(matrix(nrow=nrow(filled),ncol=6))
colnames(CE_df) <- c("Uniform","Shannon","Sum","Exponential","Normal","NormalCumulative")

for(date in filled$Date){
  filledOneDay <- as.integer(filled[date==filled$Date,-1])
  parish_names <- colnames(filled[date==filled$Date,-1][,!is.na(filledOneDay)])
  filledOneDay <- filledOneDay[!is.na(filledOneDay)]
  #population_specified$Population <- population_specified$Population - filledOneDay/1000 #will mess up cumulative stuff
  CE_df[match(date,filled$Date),1] <- uniform_CE(filledOneDay) #Kullback Leibler Divergence with a Uniform is just to get rid of units, I think.
  #CE_df[match(date,filled$Date),1] <- uniform_CE_cumulative(filledOneDay,date)
  CE_df[match(date,filled$Date),2] <- shannon_entropy(filledOneDay)
  CE_df[match(date,filled$Date),2] <- normalized_shannon(filledOneDay,population)
  CE_df[match(date,filled$Date),2] <- per_case_shannon(filledOneDay,population,parish_names) #instead of population, population_specified?
  #CE_df[match(date,filled$Date),2] <- 4+per_case_shannon(filledOneDay,population,parish_names)
  CE_df[match(date,filled$Date),2] <- per_case_shannon_2(filledOneDay,population,parish_names)
  #CE_df[match(date,filled$Date),2] <- -not_per_case_shannon(filledOneDay,population,parish_names)
  CE_df[match(date,filled$Date),3] <- sum(filledOneDay)
  CE_df[match(date,filled$Date),4] <- exponential_CE(filledOneDay[which(filledOneDay!=0)]) #exponential gives much clearer/cleaner results when you only work with nonzero parishes, but it works nonetheless with zeroes (just more noisily), peaking during the surge
  CE_df[match(date,filled$Date),5] <- normal_CE(filledOneDay,date)[1]
  CE_df[match(date,filled$Date),6] <- normal_CE(filledOneDay,date)[2]
}

#think in the context of constant prevalence
#50% prevalence highest entropy for an individual village? As you have the least information to make a call of if a given person has the disease or not