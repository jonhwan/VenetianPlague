#using entropy model for the cumulative cases
library(entropy)

entropy(c(0,1,1,0,1))

shannon_entropy_real <- function(case_vector){
  CV_norm <- case_vector/sum(case_vector)
  summand <- 0
  for(i in 1:length(case_vector)){
    if(CV_norm[i] == 0){
      next
    }
    summand <- summand+CV_norm[i]*log(CV_norm[i])
  }
  return(-1*summand)
}

shannon_entropy_real(c(0,1,1,0,1))

#attempt at implementing Wang entropy model to predict cumulative cases

#Step 1. Data recording. Using the reported data determine the number F = f(L) at the inflexion date.

filled_sum <- rep(0,nrow(filled))

for(i in 1:nrow(filled)){
  filled_sum[i] <- sum(filled[i,-1])
}
plot.ts(filled_sum[600:800])
print(filled_sum[650:700])

#D and f(D), the maximum date where alpha(D) = 0
D <- match(max(filled_sum),filled_sum)
f_at_d <- max(filled_sum)


#at t=670 looks like an inflection point with value F = 334. or wait if its the first time the second derivative is negative it should be earlier then. or when it's 0 but tending towards negative from the "exponential" growth at the start
#then at t=662 looks like an inflection point with value F = 307
#or one at t=669 with value F = 400
#or one at t=678 with value F = 428

plot.ts(filled_sum[600:700])
plot.ts((filled_sum - c(0,filled_sum[-length(filled_sum)]))[600:700])
print((filled_sum - c(0,filled_sum[-length(filled_sum)]))[600:700])

#or 639

inflection_date <- 669
inflection_date <- 639
inflection_date <- 651
inflection_date <- 637
inflection_date <- 642

f_inflection <- filled_sum[inflection_date]

#"derivative" of f at time L
#(filled_sum[671] - filled_sum[670])/1
df_dt_at_l <- (filled_sum[inflection_date] - filled_sum[(inflection_date-1)])/1 #f(x) - f(x-1) / 1
f_bar <- (2*f_inflection - df_dt_at_l)/2

#find L, the days since the start date of the epidemic
L = 3*f_bar/df_dt_at_l

start_date <- inflection_date - L
start_date_as_date <- filled$Date[start_date]

eta = 3 #affects if the distribution has a tail (lower = more severe tail). But still, doesn't really model that weird second wave.
sigma = 1/sqrt(2*eta)
mu = log(D-start_date) + sigma^2

#find k, the proportion constant by using f(L) = f_inflection
k = f_inflection*sqrt(2*pi)*sigma*L/exp(-((log(L)-mu)^2)/(2*sigma^2))

#predicted D?
D_predicted <- 1.649*L
print(D_predicted)
print(L + 2*f_bar/df_dt_at_l)
print(D - start_date)
f_at_d_predicted <- 2.12*f_inflection #these numbers are off by a lot, but the shape produced looks right
print(f_at_d_predicted)
print(2.12*f_bar)
print(f_at_d)

f <- function(t){
  return(k*exp(-((log(t)-mu)^2)/(2*sigma^2))/(sqrt(2*pi)*sigma*t))
}

plot.ts(f(1:(nrow(filled)-inflection_date)))
plot.ts(filled_sum[(inflection_date+1):nrow(filled)])

scaling_factor <- max(f(1:(nrow(filled)-inflection_date)))/max(filled_sum)
lines(f(1:(nrow(filled)-inflection_date))/scaling_factor)
scaled <- f(1:(nrow(filled)-inflection_date))/scaling_factor
plot.ts(filled_sum[(inflection_date+1):nrow(filled)]-scaled)

#Spore time series analysis, which I don't understand at all

# log_data <- log(filled[,2]+1)
# detr <- lm(log_data~seq(1,1095,1))
# N <- exp(detr$residuals)
# 
# par(pty="m")
# ts <- plot(N,ty="b",lwd=2)
# 
# par(pty="s")
# phpl <- plot(log(N[1:1094]),log(N[2:1095]),ty='b',lwd=2,xlab="log time t",ylab="log time t+1")
# lines(log(N),log(N),col="red3",lty=2)
# 
# par(pty="s")
# lagmax<-round(0.25*length(N))#Max lag value for acf and MI work
# N_acf<-acf(N, type="correlation", plot=TRUE, lag.max=lagmax,
#                   ylab="auto-correltation function", xlab="time lag index",
#                   main="")
# AMI<- mutual(N, lag.max = lagmax, main="")
# plot(AMI)
# m.max<-8
# d<-5
# tw<-3
# rt<-10
# eps<-sd(N)
# fn<-false.nearest(N,m.max,d,tw,rt,eps=eps)
# fn
# par(pty="s")
# plot(fn, main="")