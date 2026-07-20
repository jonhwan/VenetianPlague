library(dplyr)
library(lubridate)

#New method (here we're doing actual daily)
ababa <- whole_dataset_analysis(cumulative=TRUE) #default
ababa2 <- whole_dataset_analysis(cumulative=FALSE)
end_of_month_indices <- c(which(day(filled$Date) == 1)[-1]-1,1095)
instantaneous_shannon_endofmonth <- ababa2$shannonQuantile[end_of_month_indices]
write.csv(instantaneous_shannon_endofmonth,"~/LegacyVeniceParishMaps/monthlyshannonvec3.csv")

#Old method

# monthly_totals <- filled_modified %>%
#   mutate(year_month = floor_date(Date, "month")) %>%
#   group_by(year_month) %>%
#   summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) #%>%
#   #mutate(across(-year_month, cumsum))
# monthly_totals <- as.data.frame(monthly_totals)
# shannon_monthly_vec <- rep(0,nrow(monthly_totals))
# for(i in 1:nrow(monthly_totals)){
#   cv <- as.numeric(monthly_totals[i,-1])/(population_specified_modified$Population*1000)
#   monthly_totals[i,-1] <- cv
#   cv <- cv[cv!=0]
#   cv_normalized <- cv/sum(cv)
#   shannon_monthly_vec[i] <- -sum(cv_normalized*log(cv_normalized))
# }
# shannon_monthly_vec
#write.csv(shannon_monthly_vec,"~/LegacyVeniceParishMaps/monthlyshannonvec2.csv")