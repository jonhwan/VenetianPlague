library(boot)
SHANNON_ENT <- function(x,indices){
  boot_data <- x[indices,]
  pop_spec <- population_specified_modified[indices,]
  cv <- as.numeric(rowSums(boot_data[,1:800]))/(pop_spec$Population*1000)
  cv_norm <- cv/sum(cv)
  return(-1*sum(cv_norm[cv_norm!=0]*log(cv_norm[cv_norm!=0])))
}

set.seed(49)
R_boot_pkg <- boot(t(filled_modified[,-1]),statistic=SHANNON_ENT,R=999)
R_boot_pkg
plot(R_boot_pkg)
boot.ci(R_boot_pkg,type="bca")

#Compare
stats_of_whole_boot_cumulative_50$b1[[3]][800]
#3.850404
stats_of_whole_boot_cumulative_50$b2[[3]][800]
#3.8842

#P-value
mean(abs(R_boot_pkg$t) > abs(R_boot_pkg$t0))
#0.5445445