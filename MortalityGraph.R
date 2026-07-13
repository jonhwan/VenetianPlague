library(cowplot)

population <- data.frame(population_post$Parish,interpolated_1629/1000)
colnames(population) <- c("Parish","Population")
population_specified <- population[population$Parish %in% colnames(filled[,-1]),]
population_specified <- population_specified[match(colnames(filled[,-1]),population_specified$Parish),]

outliers <- c("S.Polo", "S.Stae","S.Marco") #No cases between 9/1/30 and 12/31/30, outbreak time
filled_nodate <- filled[,-1]
outlier_indices <- which(colnames(filled_nodate) %in% outliers) #this is wrong
total_deaths <- colSums(filled_nodate[1:1095,])
print(colnames(filled_nodate) == population_specified$Parish)
export_df <- as.data.frame(cbind(1:ncol(filled_nodate), total_deaths, population_specified$Population*1000))
write.csv(export_df, "total_deaths.csv")
print(outlier_indices)

mort_df <- as.data.frame(matrix(NA,ncol=3,nrow=1095))
colnames(mort_df) <- c("Date","Mort","CumMort")
mort_df$Date <- as.Date(filled$Date)
for(i in 1:1095){
  mort_df[i,2] <- sum(as.numeric(filled_nodate[i,])/(population_specified$Population*1000))/ncol(filled_nodate)
  mort_df[i,3] <- sum(as.numeric(colSums(filled_nodate[1:i,]))/(population_specified$Population*1000))/ncol(filled_nodate)
}
regions <- tibble(x1=as.Date("1630-09-01"),x2=as.Date("1630-12-31"),y1=-Inf,y2=+Inf)
daily_mort_plot <- ggplot(mort_df)+
  geom_rect(data=regions,inherit.aes = FALSE,mapping=aes(xmin=x1,xmax=x2,ymin=y1,ymax=y2),color="transparent",fill="blue",alpha=0.2)+
  geom_line(aes(x=as.Date(Date),y=Mort))+
  labs(x="Date",y="Mean Daily Parish Mortality",title="Daily Mortality")+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.title = element_text(size=10))
cumulative_mort_plot <- ggplot(mort_df)+
  geom_rect(data=regions,inherit.aes=FALSE,mapping=aes(xmin=x1,xmax=x2,ymin=y1,ymax=y2),color="transparent",fill="blue",alpha=0.2)+
  geom_line(aes(x=as.Date(Date),y=CumMort))+
  labs(x="Date",y="Mean Cumulative Parish Mortality",title="Cumulative Mortality")+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.title = element_text(size=10))
untitled_grid <- plot_grid(plotlist=list(daily_mort_plot,cumulative_mort_plot),labels=c("a","b"),label_size=12)
title <- ggdraw() + 
  draw_label(
    "Mortality Over Time",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )
plot_grid(
  title, untitled_grid,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
)
ggsave("SuppFigX.jpg",width=2200,height=1080,units="px")

sestiere_vec <- c()
for(i in unique(sestiere_df_modified$Sestiere)){
  sestiere_vec <- c(sestiere_vec,length(sestiere_df_modified$Parish[sestiere_df_modified$Sestiere==i]))
}
print(sum(sestiere_vec)/length(sestiere_vec))
print(sd(sestiere_vec))

final_mortality <- as.numeric(colSums(filled_modified[1:1095,-1])/(population_specified_modified$Population*1000))
mean_mortality <- c()
for(i in unique(sestiere_df_modified$Sestiere)){
  sestiere_mortality <- final_mortality[which(population_specified_modified$Parish %in% sestiere_df_modified$Parish[sestiere_df_modified$Sestiere==i])]
  mean_mortality <- c(mean_mortality,sum(sestiere_mortality)/length(sestiere_mortality))
  print(sd(sestiere_mortality))
}
print(sum(mean_mortality)/length(mean_mortality))
print(sd(mean_mortality))