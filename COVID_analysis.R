COVID_data <- read.csv("~/Downloads/united_states_covid19_deaths_ed_visits_and_positivity_by_state_clean.csv")
COVID_data$Total.Death.rate.per.100000 <- sort(as.numeric(gsub("Data not available",0,COVID_data$Total.Death.rate.per.100000)))
Total.Death.rate.per.100000_nozero <- COVID_data$Total.Death.rate.per.100000[COVID_data$Total.Death.rate.per.100000!=0]
Total.Death.rate.per.100000_nozero_norm <- Total.Death.rate.per.100000_nozero/sum(Total.Death.rate.per.100000_nozero)
PDF_approximation <- as.numeric(PDF_approx(Total.Death.rate.per.100000_nozero))
negent_new(Total.Death.rate.per.100000_nozero)
plot(PDF_approximation)
KL_new(Total.Death.rate.per.100000_nozero_norm,rep(1/length(Total.Death.rate.per.100000_nozero),length(Total.Death.rate.per.100000_nozero)))
barplot(Total.Death.rate.per.100000_nozero)