#Effective number and number of non-zero parishes

non_zeroes_cumulative <- rep(0,length(filled$Date))
eff_number_cumulative <- rep(0,length(filled$Date))
non_zeroes_instantaneous <- rep(0,length(filled$Date))
eff_number_instantaneous <- rep(0,length(filled$Date))
for(i in 1:length(filled$Date)){
  case_vec_cumulative <- as.numeric(colSums(filled_modified[1:i,-1]))/(population_specified_modified$Population*1000)
  case_vec_cumulative_norm <- case_vec_cumulative/sum(case_vec_cumulative)
  case_vec_instantaneous <- as.numeric(filled_modified[i,-1])/(population_specified_modified$Population*1000)
  case_vec_instantaneous_norm <- case_vec_instantaneous/sum(case_vec_instantaneous)
  case_vec_cumulative_norm_nonzeroes <- case_vec_cumulative_norm[case_vec_cumulative_norm != 0]
  non_zeroes_cumulative[i] <- length(case_vec_cumulative_norm_nonzeroes)
  eff_number_cumulative[i] <- 1/sum(case_vec_cumulative_norm_nonzeroes^2)
  case_vec_instantaneous_norm_nonzeroes <- case_vec_instantaneous_norm[case_vec_instantaneous_norm != 0]
  non_zeroes_instantaneous[i] <- length(case_vec_instantaneous_norm_nonzeroes)
  eff_number_instantaneous[i] <- 1/sum(case_vec_instantaneous_norm_nonzeroes^2)
}

plot.ts(log(non_zeroes_cumulative),ylim=c(2,4),col="red")
lines(stats_of_whole_cumulative_50_2$shannonQuantile,col="black")
plot.ts(log(non_zeroes_instantaneous),ylim=c(0,4),col="red")
lines(stats_of_whole_instantaneous_50_2$shannonQuantile,col="black")
plot.ts(log(eff_number_cumulative),ylim=c(2,4),col="red")
lines(stats_of_whole_cumulative_50_2$shannonQuantile,col="black")
plot.ts(log(eff_number_instantaneous),ylim=c(0,4),col="red")
lines(stats_of_whole_instantaneous_50_2$shannonQuantile,col="black")