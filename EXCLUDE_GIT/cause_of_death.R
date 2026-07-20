library(tidyverse)

data_1630 <- read.csv("~/Downloads/Peste1630.csv")
data_1631 <- read.csv("~/Downloads/Peste1631.csv")

cause_death_1630 <- data_1630$Causa.della.morte
cause_death_1631 <- data_1631$Causa.della.morte

combined <- c(cause_death_1630,cause_death_1631)
cleaned_combined <- trimws(combined)
unique_list <- sort(unique(combined))
cleaned_unique_list <- sort(unique(cleaned_combined))
count <- 1:length(cleaned_unique_list)

for(i in 1:length(cleaned_unique_list)){
  count[i] <- length(which(cleaned_combined==cleaned_unique_list[i]))
}

plague_related <- rep(FALSE,length(count))
is_plague_related <- c("bubbone pestilenziale all'inguine","carbone","carboni","carboni e parto","carbonie e petecchie nere","contagio","contagio e petecchie nere","doglia di testa e mazzucco","febbre e mazzucco","febbre e petecchie",
                       "febbre e petecchie nere", "febbre e petecchie rosse","febbre e suspetto","febbre maligna","febbre maligna e mal sospetto","febbre maligna e punti","ferite da peste","macchie nel petto giudicate pestilenziali","mal contagioso","mal di mare e mal sospetto","mal di mazzucco","mal mazzucco","mal sospetto","mal sospetto e petecchie",
                       "mazzucco","nosella di mal contagioso","peste","peste e petecchie nere","peste e strupiata","petecche paonazze","petecchi nere","petecchie",
                       "petecchie e febbre maligna","petecchie e mazzucco","petecchie e spasimo","petecchie e un brusco","petecchie et un brusco","petecchie nere","petecchie nere contagiose","petecchie nere e carbone","petecchie nere e rosse","petecchie nere pestilenziali","petecchie pestilenziali","petecchie rosse","petecchie rosse e alcune nere","petecchie rosse e parto","petecchie rosse verso il nero","spasimo e mazzucco","spasimo e petecchie nere","un carbon","vermi e petecchie")
plague_related[which(cleaned_unique_list %in% is_plague_related)] <- TRUE
proportion <- count/sum(count)

output_df <- data.frame(cleaned_unique_list,count,plague_related,proportion)
colnames(output_df) <- c("cause","count","plague_related","proportion")
print(output_df)
write.csv(output_df,"~/Downloads/Cause_Death_1630.csv")

trimmed_output_df <- output_df[output_df$count >= 50,]
trimmed_output_df <- trimmed_output_df %>%
  arrange(desc(count))
write.csv(trimmed_output_df,"~/Downloads/Cause_Death_1630_Trimmed.csv")