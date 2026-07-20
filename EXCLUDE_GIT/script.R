filled_nodate <- filled[,-1]
outlier_indices <- which(outliers %in% colnames(filled_nodate))
total_deaths <- colSums(filled_nodate[1:1095,])
print(colnames(filled_nodate) == population_df$Parish)
export_df <- as.data.frame(cbind(1:ncol(filled_nodate), total_deaths, population_df$Population*1000))
write.csv(export_df, "~/Downloads/total_deaths.csv")
print(outlier_indices)

