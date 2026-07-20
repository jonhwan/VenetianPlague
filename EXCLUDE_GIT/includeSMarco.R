library(progress)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(ggpubr)
library(tidyverse)
library(lubridate)
library(msos)

#include S. Marco
outliers <- c("S.Polo", "S.Stae") #No cases between 9/1/30 and 12/31/30, outbreak time
filled_modified <- filled[,!(colnames(filled) %in% outliers)]
sestiere_df_modified <- sestiere_df[!(sestiere_df$Parish %in% outliers),]
population_specified_modified <- population_specified[!(population_specified$Parish %in% outliers),]

stats_by_sestiere_boot_cumulative_51 <- shannonBySestiere_Bootstrapped_4(cumulative=TRUE) #w/ fixed negent
stats_by_sestiere_boot_instantaneous_51 <- shannonBySestiere_Bootstrapped_4(cumulative=FALSE)
stats_by_sestiere_cumulative_51 <- shannonBySestiere_4(cumulative=TRUE)
stats_by_sestiere_instantaneous_51 <- shannonBySestiere_4(cumulative=FALSE)
stats_of_whole_cumulative_51 <- whole_dataset_analysis(cumulative=TRUE) #default
stats_of_whole_instantaneous_51 <- whole_dataset_analysis(cumulative=FALSE)
stats_of_whole_boot_cumulative_51 <- whole_dataset_bootstrap(cumulative=TRUE)
stats_of_whole_boot_instantaneous_51 <- whole_dataset_bootstrap(cumulative=FALSE)

saveRDS(stats_by_sestiere_boot_cumulative_51,"stats_by_sestiere_boot_cumulative_51.rds")
saveRDS(stats_by_sestiere_boot_instantaneous_51,"stats_by_sestiere_boot_instantaneous_51.rds")
saveRDS(stats_of_whole_boot_cumulative_51,"stats_of_whole_boot_cumulative_51.rds")
saveRDS(stats_of_whole_boot_instantaneous_51,"stats_of_whole_boot_instantaneous_51.rds")

#Reset
outliers <- c("S.Polo", "S.Stae","S.Marco") #No cases between 9/1/30 and 12/31/30, outbreak time
filled_modified <- filled[,!(colnames(filled) %in% outliers)]
sestiere_df_modified <- sestiere_df[!(sestiere_df$Parish %in% outliers),]
population_specified_modified <- population_specified[!(population_specified$Parish %in% outliers),]