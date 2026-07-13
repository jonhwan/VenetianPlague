stats_of_whole_cumulative_50_2$shannonQuantile[1] #1.988281
stats_of_whole_boot_cumulative_50_2$b2[[3]][1] #2.530452
stats_of_whole_boot_cumulative_50_2$b1[[3]][1] #1.236387
stats_of_whole_cumulative_50_2$shannonQuantile[1095] #3.855849
stats_of_whole_boot_cumulative_50_2$b1[[3]][1095] #3.841083
stats_of_whole_boot_cumulative_50_2$b2[[3]][1095] #3.868945

stats_of_whole_cumulative_50_2$uniformKL_Quantile[1] #1.90354
stats_of_whole_boot_cumulative_50_2$b1[[6]][1] #1.361368
stats_of_whole_boot_cumulative_50_2$b2[[6]][1] #2.655433
stats_of_whole_cumulative_50_2$uniformKL_Quantile[1095] #0.03597084
stats_of_whole_boot_cumulative_50_2$b2[[6]][1095] #0.050_273722
stats_of_whole_boot_cumulative_50_2$b1[[6]][1095] #0.02287577

stats_of_whole_cumulative_50_2$shannonPDF[1] #0.6430871
stats_of_whole_boot_cumulative_50_2$b1[[2]][1] #0.3393223
stats_of_whole_boot_cumulative_50_2$b2[[2]][1] #0.9709571
stats_of_whole_cumulative_50_2$shannonPDF[1095] #1.738919
stats_of_whole_boot_cumulative_50_2$b1[[2]][1095] #1.538075
stats_of_whole_boot_cumulative_50_2$b2[[2]][1095] #1.839712

stats_of_whole_cumulative_50_2$negentPDF[1] #0.8352903
stats_of_whole_boot_cumulative_50_2$b2[[4]][1] #1.441464
stats_of_whole_boot_cumulative_50_2$b1[[4]][1] #0.7284727
stats_of_whole_cumulative_50_2$negentPDF[1095] #0.1072035
stats_of_whole_boot_cumulative_50_2$b1[[4]][1095] #0.07247547
stats_of_whole_boot_cumulative_50_2$b2[[4]][1095] #0.3258044

stats_of_whole_cumulative_50_2$uniformKL_PDF[1] #0.966350_28
stats_of_whole_boot_cumulative_50_2$b1[[5]][1] #0.5657428
stats_of_whole_boot_cumulative_50_2$b2[[5]][1] #1.170918
stats_of_whole_cumulative_50_2$uniformKL_PDF[1095] #0.2069911
stats_of_whole_boot_cumulative_50_2$b2[[5]][1095] #0.3832459
stats_of_whole_boot_cumulative_50_2$b1[[5]][1095] #0.08352609

stats_of_whole_boot_cumulative_50_2$closest_runs_025[[5]][1095] #0.1674439
stats_of_whole_boot_cumulative_50_2$closest_runs_975[[5]][1095] #0.2010526

min(stats_of_whole_cumulative_50_2$uniformKL_PDF) #0.1382115
min_index <- match(min(stats_of_whole_cumulative_50_2$uniformKL_PDF),stats_of_whole_cumulative_50_2$uniformKL_PDF)
filled$Date[min_index] #"1631-06-17"
stats_of_whole_cumulative_50_2$uniformKL_PDF[min_index]
stats_of_whole_boot_cumulative_50_2$b1[[5]][min_index]
stats_of_whole_boot_cumulative_50_2$b2[[5]][min_index]

min(stats_of_whole_cumulative_50_2$negentPDF) #0.1382115
min_index <- match(min(stats_of_whole_cumulative_50_2$negentPDF),stats_of_whole_cumulative_50_2$negentPDF)
filled$Date[min_index] #"1631-06-17"
stats_of_whole_cumulative_50_2$negentPDF[min_index]
stats_of_whole_boot_cumulative_50_2$b1[[4]][min_index]
stats_of_whole_boot_cumulative_50_2$b2[[4]][min_index]

min(stats_of_whole_cumulative_50_2$uniformKL_Quantile) #0.1382115
min_index <- match(min(stats_of_whole_cumulative_50_2$uniformKL_Quantile),stats_of_whole_cumulative_50_2$uniformKL_Quantile)
filled$Date[min_index] #"1631-06-17"
stats_of_whole_cumulative_50_2$uniformKL_Quantile[min_index]
stats_of_whole_boot_cumulative_50_2$b1[[6]][min_index]
stats_of_whole_boot_cumulative_50_2$b2[[6]][min_index]

max(stats_of_whole_cumulative_50_2$shannonQuantile) #0.1382115
max_index <- match(max(stats_of_whole_cumulative_50_2$shannonQuantile),stats_of_whole_cumulative_50_2$shannonQuantile)
filled$Date[max_index] #"1631-06-17"
stats_of_whole_cumulative_50_2$shannonQuantile[max_index]
stats_of_whole_boot_cumulative_50_2$b1[[3]][max_index]
stats_of_whole_boot_cumulative_50_2$b2[[3]][max_index]

max(stats_of_whole_cumulative_50_2$shannonPDF) #0.1382115
max_index <- match(max(stats_of_whole_cumulative_50_2$shannonPDF),stats_of_whole_cumulative_50_2$shannonPDF)
filled$Date[max_index] #"1631-06-17"
stats_of_whole_cumulative_50_2$shannonPDF[max_index]
stats_of_whole_boot_cumulative_50_2$b1[[2]][max_index]
stats_of_whole_boot_cumulative_50_2$b2[[2]][max_index]





stats_of_whole_instantaneous_50_2$shannonQuantile[1] #1.988281
stats_of_whole_boot_instantaneous_50_2$b2[[3]][1] #2.530452
stats_of_whole_boot_instantaneous_50_2$b1[[3]][1] #1.236387
stats_of_whole_instantaneous_50_2$shannonQuantile[1095] #3.855849
stats_of_whole_boot_instantaneous_50_2$b1[[3]][1095] #3.841083
stats_of_whole_boot_instantaneous_50_2$b2[[3]][1095] #3.868945

stats_of_whole_instantaneous_50_2$uniformKL_Quantile[1] #1.90354
stats_of_whole_boot_instantaneous_50_2$b1[[6]][1] #1.361368
stats_of_whole_boot_instantaneous_50_2$b2[[6]][1] #2.655433
stats_of_whole_instantaneous_50_2$uniformKL_Quantile[1095] #0.03597084
stats_of_whole_boot_instantaneous_50_2$b2[[6]][1095] #0.050_273722
stats_of_whole_boot_instantaneous_50_2$b1[[6]][1095] #0.02287577

stats_of_whole_instantaneous_50_2$shannonPDF[1] #0.6430871
stats_of_whole_boot_instantaneous_50_2$b1[[2]][1] #0.3393223
stats_of_whole_boot_instantaneous_50_2$b2[[2]][1] #0.9709571
stats_of_whole_instantaneous_50_2$shannonPDF[1095] #1.738919
stats_of_whole_boot_instantaneous_50_2$b1[[2]][1095] #1.538075
stats_of_whole_boot_instantaneous_50_2$b2[[2]][1095] #1.839712

stats_of_whole_instantaneous_50_2$negentPDF[1] #0.8352903
stats_of_whole_boot_instantaneous_50_2$b2[[4]][1] #1.441464
stats_of_whole_boot_instantaneous_50_2$b1[[4]][1] #0.7284727
stats_of_whole_instantaneous_50_2$negentPDF[1095] #0.1072035
stats_of_whole_boot_instantaneous_50_2$b1[[4]][1095] #0.07247547
stats_of_whole_boot_instantaneous_50_2$b2[[4]][1095] #0.3258044

stats_of_whole_instantaneous_50_2$uniformKL_PDF[1] #0.966350_28
stats_of_whole_boot_instantaneous_50_2$b1[[5]][1] #0.5657428
stats_of_whole_boot_instantaneous_50_2$b2[[5]][1] #1.170918
stats_of_whole_instantaneous_50_2$uniformKL_PDF[1095] #0.2069911
stats_of_whole_boot_instantaneous_50_2$b2[[5]][1095] #0.3832459
stats_of_whole_boot_instantaneous_50_2$b1[[5]][1095] #0.08352609

min(stats_of_whole_instantaneous_50_2$uniformKL_PDF) #0.1382115
min_index <- match(min(stats_of_whole_instantaneous_50_2$uniformKL_PDF),stats_of_whole_instantaneous_50_2$uniformKL_PDF)
filled$Date[min_index] #"1631-06-17"
stats_of_whole_instantaneous_50_2$uniformKL_PDF[min_index]
stats_of_whole_boot_instantaneous_50_2$b1[[5]][min_index]
stats_of_whole_boot_instantaneous_50_2$b2[[5]][min_index]

min(stats_of_whole_instantaneous_50_2$negentPDF) #0.1382115
min_index <- match(min(stats_of_whole_instantaneous_50_2$negentPDF),stats_of_whole_instantaneous_50_2$negentPDF)
filled$Date[min_index] #"1631-06-17"
stats_of_whole_instantaneous_50_2$negentPDF[min_index]
stats_of_whole_boot_instantaneous_50_2$b1[[4]][min_index]
stats_of_whole_boot_instantaneous_50_2$b2[[4]][min_index]

min(stats_of_whole_instantaneous_50_2$uniformKL_Quantile) #0.1382115
min_index <- match(min(stats_of_whole_instantaneous_50_2$uniformKL_Quantile),stats_of_whole_instantaneous_50_2$uniformKL_Quantile)
filled$Date[min_index] #"1631-06-17"
stats_of_whole_instantaneous_50_2$uniformKL_Quantile[min_index]
stats_of_whole_boot_instantaneous_50_2$b1[[6]][min_index]
stats_of_whole_boot_instantaneous_50_2$b2[[6]][min_index]

max(stats_of_whole_instantaneous_50_2$shannonQuantile) #0.1382115
max_index <- match(max(stats_of_whole_instantaneous_50_2$shannonQuantile),stats_of_whole_instantaneous_50_2$shannonQuantile)
filled$Date[max_index] #"1631-06-17"
stats_of_whole_instantaneous_50_2$shannonQuantile[max_index]
stats_of_whole_boot_instantaneous_50_2$b1[[3]][max_index]
stats_of_whole_boot_instantaneous_50_2$b2[[3]][max_index]

max(stats_of_whole_instantaneous_50_2$shannonPDF) #0.1382115
max_index <- match(max(stats_of_whole_instantaneous_50_2$shannonPDF),stats_of_whole_instantaneous_50_2$shannonPDF)
filled$Date[max_index] #"1631-06-17"
stats_of_whole_instantaneous_50_2$shannonPDF[max_index]
stats_of_whole_boot_instantaneous_50_2$b1[[2]][max_index]
stats_of_whole_boot_instantaneous_50_2$b2[[2]][max_index]

ncol(filled_modified[,-1])
sum(population_specified_modified$Population*1000)
median(population_specified_modified$Population*1000)
mean(population_specified_modified$Population*1000)
sd(population_specified_modified$Population*1000)
sum(rowSums(filled_modified[,-1]))
mean(colSums(filled_modified[,-1])/(population_specified_modified$Population*1000))
sd(colSums(filled_modified[,-1])/(population_specified_modified$Population*1000))



#Effective number and log of non-zeroes
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
# plot.ts(log(non_zeroes_cumulative),ylim=c(2,4),col="red")
# lines(stats_of_whole_cumulative_50_2$shannonQuantile,col="black")
# plot.ts(log(non_zeroes_instantaneous),ylim=c(0,4),col="red")
# lines(stats_of_whole_instantaneous_50_2$shannonQuantile,col="black")
# plot.ts(log(eff_number_cumulative),ylim=c(2,4),col="red")
# lines(stats_of_whole_cumulative_50_2$shannonQuantile,col="black")
# plot.ts(log(eff_number_instantaneous),ylim=c(0,4),col="red")
# lines(stats_of_whole_instantaneous_50_2$shannonQuantile,col="black")


#1: sum, 2: shannonPDF, 3: shannonQuantile, 4: negentPDF, 5: uniformKL_PDF, 6: uniformKL_Quantile
#can plot against incidence ("sum") if you want

plot_df <- as.data.frame(cbind(stats_of_whole_boot_cumulative_50_2$b1[[6]],stats_of_whole_boot_cumulative_50_2$b2[[6]],stats_of_whole_cumulative_50_2$uniformKL_Quantile))
colnames(plot_df) <- c("b1","b2","observed")
plot1 <- ggplot(data=plot_df,aes(x=filled$Date,y=observed))+
  geom_line()+
  geom_ribbon(aes(ymin=b1,ymax=b2),alpha=0.5)+
  geom_line(aes(y=rep(0,length(filled$Date))),col="red")+
  coord_cartesian(ylim=c(0,0.35))+
  # coord_cartesian(ylim=c(0,0.5))+ #S. Marco
  scale_x_date(date_breaks="12 months",date_labels="%m-%Y")+
  scale_y_continuous()+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),plot.background=element_rect(fill="white"),axis.title=element_text(size=10),plot.title=element_text(size=12))+
  labs(y="Negentropy (nats)",x="Date",title="Negentropy")
plot_df_2 <- as.data.frame(cbind(stats_of_whole_boot_cumulative_50_2$b1[[3]],stats_of_whole_boot_cumulative_50_2$b2[[3]],stats_of_whole_cumulative_50_2$shannonQuantile,eff_number_cumulative))
colnames(plot_df_2) <- c("b1","b2","observed","eff")
plot2 <- ggplot(data=plot_df_2,aes(x=filled$Date,y=observed))+
  geom_line()+
  #geom_line(aes(y=rep(log(ncol(filled_modified[,-1])),length(filled$Date))),linetype="dashed")+
  geom_line(aes(y=rep(log(ncol(filled_modified[,-1])),length(filled$Date))),col="red")+
  geom_ribbon(aes(ymin=b1,ymax=b2),alpha=0.5)+
  #geom_line(aes(y=log(eff)),col="red")+
  # coord_cartesian(ylim=c(3.5,3.95))+ #S. Marco
  coord_cartesian(ylim=c(3.5,3.9))+
  scale_x_date(date_breaks="12 months",date_labels="%m-%Y")+
  scale_y_continuous(name="Entropy (nats)", sec.axis=sec_axis(trans=~., name="Effective Number",labels = function(x) scales::comma(round(exp(x)), accuracy = 1)))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),plot.background=element_rect(fill="white"),axis.title=element_text(size=10),plot.title=element_text(size=12))+
  labs(y="Entropy (nats)",x="Date",title="Entropy")
plot_ls <- list(plot2,plot1)
untitled_grid <- plot_grid(plotlist=plot_ls,labels=c("a","b"),label_size=12)
# now add the title
title <- ggdraw() + 
  draw_label(
    "Quantile Function of Cumulative Mortality",
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
#ggsave("CumulativeQuantileGrid_Final.png",width=2100,height=1200,units="px")
#ggsave("CumulativeQuantileGrid_Final.jpg",width=2100,height=1200,units="px")
#ggsave("~/VenetianPlague_Entropy/Figures/Figure2.png",width=2100,height=1200,units="px")
#ggsave("~/VenetianPlague_Entropy/Figures/Figure2.jpg",width=2100,height=1200,units="px")
ggsave("~/VenetianPlague_Entropy/Figures_Final/Figure2.png",width=2100,height=1200,units="px")
ggsave("~/VenetianPlague_Entropy/Figures_Final/Figure2.jpg",width=2100,height=1200,units="px")
# ggsave("~/VenetianPlague_Entropy/SMarcoCumulativeQuantile.png",width=2100,height=1200,units="px")
# ggsave("~/VenetianPlague_Entropy/SMarcoCumulativeQuantile.jpg",width=2100,height=1200,units="px")




#1: sum, 2: shannonPDF, 3: shannonQuantile, 4: negentPDF, 5: uniformKL_PDF, 6: uniformKL_Quantile
plot_df <- as.data.frame(cbind(stats_of_whole_boot_cumulative_50_2$b1[[2]],stats_of_whole_boot_cumulative_50_2$b2[[2]],stats_of_whole_cumulative_50_2$shannonPDF))
colnames(plot_df) <- c("b1","b2","observed")
plot1 <- ggplot(data=plot_df,aes(x=filled$Date,y=observed))+
  geom_line()+
  #geom_line(aes(y=rep(log(ceiling(log2(ncol(filled_modified[,-1])))+1),length(filled$Date))),linetype="dashed")+
  geom_line(aes(y=rep(log(ceiling(log2(ncol(filled_modified[,-1])))+1),length(filled$Date))),col="red")+
  geom_ribbon(aes(ymin=b1,ymax=b2),alpha=0.5)+
  coord_cartesian(ylim=c(1,2))+
  # coord_cartesian(ylim=c(0,2))+ #S. Marco
  scale_x_date(date_breaks="12 months",date_labels="%m-%Y")+
  scale_y_continuous()+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),plot.background=element_rect(fill="white"),axis.title=element_text(size=10),plot.title=element_text(size=12))+
  labs(y="Entropy (nats)",x="Date",title="Entropy")
plot_df_2 <- as.data.frame(cbind(stats_of_whole_boot_cumulative_50_2$b1[[4]],stats_of_whole_boot_cumulative_50_2$b2[[4]],stats_of_whole_cumulative_50_2$negentPDF))
colnames(plot_df_2) <- c("b1","b2","observed")
plot2 <- ggplot(data=plot_df_2,aes(x=filled$Date,y=observed))+
  geom_line()+
  #geom_line(aes(y=funny_vec))+
  geom_ribbon(aes(ymin=b1,ymax=b2),alpha=0.5)+
  geom_line(aes(y=rep(0,length(filled$Date))),col="red")+
  coord_cartesian(ylim=c(0,0.45))+
  # coord_cartesian(ylim=c(0,1.5))+ #S. Marco
  scale_x_date(date_breaks="12 months",date_labels="%m-%Y")+
  scale_y_continuous()+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),plot.background=element_rect(fill="white"),axis.title=element_text(size=10),plot.title=element_text(size=12))+
  #labs(y=expression(paste(D[KL]," (nats)")),x="Date",title="Negentropy")
  labs(y="Negentropy (nats)",x="Date",title="Negentropy")
# plot_df_3 <- as.data.frame(cbind(stats_of_whole_boot_cumulative_50_2$b1[[5]],stats_of_whole_boot_cumulative_50_2$b2[[5]],stats_of_whole_cumulative_50_2$uniformKL_PDF))
# colnames(plot_df_3) <- c("b1","b2","observed")
# plot3 <- ggplot(data=plot_df_3,aes(x=filled$Date,y=observed))+
#   geom_line()+
#   geom_ribbon(aes(ymin=b1,ymax=b2),alpha=0.5)+
#   coord_cartesian(ylim=c(0,1))+
#   scale_x_date(date_breaks="12 months",date_labels="%m-%Y")+
#   scale_y_continuous()+
#   theme(plot.background=element_rect(fill="white"),axis.title=element_text(size=10))+
#   labs(y="KL Divergence",x="Date",title="KL Divergence from Uniform")
# plot_ls <- list(plot1,plot2,plot3)
# untitled_grid <- plot_grid(plotlist=plot_ls,labels=c("A","B","C"),label_size=12)
plot_ls <- list(plot1,plot2)
untitled_grid <- plot_grid(plotlist=plot_ls,labels=c("a","b"),label_size=12)
# now add the title
title <- ggdraw() + 
  draw_label(
    "PDF of Cumulative Mortality",
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
  ncol=1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
)
#ggsave("cumulative_50_2_2PDFGrid5.png",width=2100,height=150_20,units="px")
#ggsave("CumulativePDFGrid_Final.png",width=2100,height=1200,units="px")
#ggsave("CumulativePDFGrid_Final.jpg",width=2100,height=1200,units="px")
# ggsave("~/VenetianPlague_Entropy/Figures/Figure3.png",width=2100,height=1200,units="px")
# ggsave("~/VenetianPlague_Entropy/Figures/Figure3.jpg",width=2100,height=1200,units="px")
ggsave("~/VenetianPlague_Entropy/Figures_Final/Figure3.png",width=2100,height=1200,units="px")
ggsave("~/VenetianPlague_Entropy/Figures_Final/Figure3.jpg",width=2100,height=1200,units="px")
# ggsave("~/VenetianPlague_Entropy/SMarcoCumulativePDF.png",width=2100,height=1200,units="px")
# ggsave("~/VenetianPlague_Entropy/SMarcoCumulativePDF.jpg",width=2100,height=1200,units="px")


#1: sum, 2: shannonPDF, 3: shannonQuantile, 4: negentPDF, 5: uniformKL_PDF, 6: uniformKL_Quantile
#can plot against incidence ("sum") if you want

plot_df <- as.data.frame(cbind(stats_of_whole_boot_instantaneous_50_2$b1[[6]],stats_of_whole_boot_instantaneous_50_2$b2[[6]],stats_of_whole_instantaneous_50_2$uniformKL_Quantile))
colnames(plot_df) <- c("b1","b2","observed")
plot1 <- ggplot(data=plot_df,aes(x=filled$Date,y=observed))+
  geom_line(lwd=0.25)+
  geom_ribbon(aes(ymin=b1,ymax=b2),alpha=0.5)+
  geom_line(aes(y=rep(0,length(filled$Date))),col="red")+
  scale_x_date(date_breaks="12 months",date_labels="%m-%Y")+
  scale_y_continuous()+
  expand_limits(y=c(0,2))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),plot.background=element_rect(fill="white"),plot.title=element_text(size=12),axis.title=element_text(size=10))+
  #labs(y=expression(D[K*L]),x="Date",title="Negentropy")
  labs(y="Negentropy (nats)",x="Date",title="Negentropy")
plot_df_2 <- as.data.frame(cbind(stats_of_whole_boot_instantaneous_50_2$b1[[3]],stats_of_whole_boot_instantaneous_50_2$b2[[3]],stats_of_whole_instantaneous_50_2$shannonQuantile,log(non_zeroes_instantaneous)))
colnames(plot_df_2) <- c("b1","b2","observed","nonzeroes")
plot2 <- ggplot(data=plot_df_2,aes(x=filled$Date,y=observed))+
  #geom_line(aes(y=nonzeroes),lwd=0.25,col="red",alpha=0.75)+ #comment out if don't like
  geom_line(lwd=0.25)+
  geom_line(aes(y=rep(log(ncol(filled_modified[,-1])),length(filled$Date))),col="red")+
  geom_ribbon(aes(ymin=b1,ymax=b2),alpha=0.5)+
  scale_x_date(date_breaks="12 months",date_labels="%m-%Y")+
  scale_y_continuous()+
  expand_limits(y=c(2,4))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),plot.background=element_rect(fill="white"),plot.title=element_text(size=12),axis.title=element_text(size=10))+
  labs(y="Entropy (nats)",x="Date",title="Entropy")
plot_ls <- list(plot2,plot1)
untitled_grid <- plot_grid(plotlist=plot_ls,labels=c("a","b"),label_size=12)
# now add the title
title <- ggdraw() + 
  draw_label(
    "Quantile Function of Instantaneous Mortality",
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
#ggsave("InstantaneousQuantileGrid_Final_2.png",width=2100,height=1200,units="px")
#ggsave("InstantaneousQuantileGrid_Final_2.jpg",width=2100,height=1200,units="px")
#ggsave("InstantaneousQuantileGrid_Final.jpg",width=2100,height=1200,units="px")
#ggsave("InstantaneousQuantileGrid_Final_3.jpg",width=2100,height=1200,units="px")
ggsave("InstantaneousQuantileGrid_Final_4.jpg",width=2100,height=1200,units="px")

#1: sum, 2: shannonPDF, 3: shannonQuantile, 4: negentPDF, 5: uniformKL_PDF, 6: uniformKL_Quantile
plot_df <- as.data.frame(cbind(stats_of_whole_boot_instantaneous_50_2$b1[[2]],stats_of_whole_boot_instantaneous_50_2$b2[[2]],stats_of_whole_instantaneous_50_2$shannonPDF))
colnames(plot_df) <- c("b1","b2","observed")
plot1 <- ggplot(data=plot_df,aes(x=filled$Date,y=observed))+
  geom_ribbon(aes(x=filled$Date,ymin=b1,ymax=b2),alpha=0.5)+
  geom_line(lwd=0.25)+
  geom_line(aes(y=rep(log(ceiling(log2(ncol(filled_modified[,-1])))+1),length(filled$Date))),col="red")+
  scale_x_date(date_breaks="12 months",date_labels="%m-%Y")+
  scale_y_continuous()+
  expand_limits(y=c(0,2))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),plot.background=element_rect(fill="white"),axis.title=element_text(size=10),plot.title=element_text(size=12))+
  labs(y="Entropy (nats)",x="Date",title="Entropy")
plot_df_2 <- as.data.frame(cbind(stats_of_whole_boot_instantaneous_50_2$b1[[4]],stats_of_whole_boot_instantaneous_50_2$b2[[4]],stats_of_whole_instantaneous_50_2$negentPDF))
colnames(plot_df_2) <- c("b1","b2","observed")
plot2 <- ggplot(data=plot_df_2,aes(x=filled$Date,y=observed))+
  geom_ribbon(aes(x=filled$Date,ymin=b1,ymax=b2),alpha=0.5)+
  geom_line(lwd=0.25)+
  geom_line(aes(y=rep(0,length(filled$Date))),col="red")+
  scale_x_date(date_breaks="12 months",date_labels="%m-%Y")+
  scale_y_continuous()+
  coord_cartesian(ylim=c(0,1.5))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),plot.background=element_rect(fill="white"),axis.title=element_text(size=10),plot.title=element_text(size=12))+
  labs(y="Negentropy (nats)",x="Date",title="Negentropy")
# plot_df_3 <- as.data.frame(cbind(stats_of_whole_boot_instantaneous_50_2$b1[[5]],stats_of_whole_boot_instantaneous_50_2$b2[[5]],stats_of_whole_instantaneous_50_2$uniformKL_PDF))
# colnames(plot_df_3) <- c("b1","b2","observed")
# plot3 <- ggplot(data=plot_df_3,aes(x=filled$Date,y=observed))+
#   geom_ribbon(aes(x=filled$Date,ymin=b1,ymax=b2),alpha=0.5)+
#   geom_line(lwd=0.25)+
#   scale_x_date(date_breaks="12 months",date_labels="%m-%Y")+
#   scale_y_continuous()+
#   expand_limits(y=c(0,1.8))+
#   theme(plot.background=element_rect(fill="white"),axis.title=element_text(size=10))+
#   labs(y="KL Divergence",x="Date",title="KL Divergence from Uniform")
plot_ls <- list(plot1,plot2)
untitled_grid <- plot_grid(plotlist=plot_ls,labels=c("a","b"),label_size=12)
# now add the title
title <- ggdraw() + 
  draw_label(
    "PDF of Instantaneous Mortality",
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
  ncol=1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
)
#ggsave("InstantaneousPDFGrid_Final_2.png",width=2100,height=1200,units="px")
#ggsave("InstantaneousPDFGrid_Final_2.jpg",width=2100,height=1200,units="px")
#ggsave("InstantaneousPDFGrid_Final.jpg",width=2100,height=1200,units="px")
#ggsave("InstantaneousPDFGrid_Final_3.jpg",width=2100,height=1200,units="px")
ggsave("InstantaneousPDFGrid_Final_4.jpg",width=2100,height=1200,units="px")

stat_of_interest <- 3 #1: sum, 2: shannonPDF, 3: shannonQuantile, 4: negentPDF, 5: uniformKL_PDF, 6: uniformKL_Quantile
b1_plot_mat <- matrix(NA,nrow=1095,ncol=6) #6 = number of stats, not sestieri
b2_plot_mat <- matrix(NA,nrow=1095,ncol=6)
observed_plot_mat <- matrix(NA,nrow=1095,ncol=6)

for(i in 1:6){ #6 = number of sestieri
  b1_plot_mat[,i] <- stats_by_sestiere_boot_cumulative_50_2[[i]]$b1[,stat_of_interest]
  b2_plot_mat[,i] <- stats_by_sestiere_boot_cumulative_50_2[[i]]$b2[,stat_of_interest]
  observed_plot_mat[,i] <- stats_by_sestiere_cumulative_50_2[[i]][[stat_of_interest]]
}

time_series <- 1:1095
series_names <- unique(sestiere_df_modified$Sestiere)

# Ensure matrices are data.frames
lower_df <- as.data.frame(b1_plot_mat)
upper_df <- as.data.frame(b2_plot_mat)
observed_df <- as.data.frame(observed_plot_mat)

# Add time column
lower_df$time <- upper_df$time <- observed_df$time <- time_series

# Pivot longer
lower_long <- pivot_longer(lower_df, -time, names_to = "series", values_to = "lower")
upper_long <- pivot_longer(upper_df, -time, names_to = "series", values_to = "upper")
obs_long   <- pivot_longer(observed_df, -time, names_to = "series", values_to = "observed")

# Combine all into one long dataframe
plot_df <- lower_long %>%
  left_join(upper_long, by = c("time", "series")) %>%
  left_join(obs_long, by = c("time", "series")) %>%
  mutate(series = factor(series, levels = paste0("V", 1:6), labels = series_names))

maxima_vec <- rep(0,length(unique(sestiere_df_modified$Sestiere)))
for(s in unique(sestiere_df_modified$Sestiere)){
  maxima_vec[match(s,unique(sestiere_df_modified$Sestiere))] <- log(length(sestiere_df_modified$Parish[sestiere_df_modified$Sestiere==s]))
}

maxima_column <- rep(maxima_vec,nrow(filled_modified))
plot_df$maxima <- maxima_column
  
ggplot(plot_df, aes(x = rep(filled$Date,each=6), group = series, color = series, fill = series)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.4, color = NA) +
  geom_line(aes(y = lower), linetype = "dashed", linewidth = 0.25) +
  geom_line(aes(y = upper), linetype = "dashed", linewidth = 0.25) +
  geom_line(aes(y = observed), linewidth = 0.5) +
  geom_line(aes(y = maxima),linetype="dashed",linewidth=0.75) + 
  scale_color_manual(values = c("green", "red", "blue", "yellow", "orange", "pink")) +
  scale_fill_manual(values = c("green", "red", "blue", "yellow", "orange", "pink")) +
  coord_cartesian(ylim=c(0.5,2.5))+
  scale_y_continuous(name="Entropy (nats)", sec.axis=sec_axis(trans=~., name="Effective Number",labels = function(x) scales::comma(exp(x), accuracy = 0.1)))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),plot.background=element_rect(fill="white")) +
  labs(
    title = "Quantile Function of Cumulative Mortality by Sestiere",
    #title = names(stats_of_whole_cumulative_50_2)[stat_of_interest],
    x = "Date",
    y = "Entropy (nats)",
    color = "Sestiere",
    fill = "Sestiere"
  ) 
#ggsave("CumulativeQuantile_KL_Sestiere_Final.png",width=2100,height=1500,units="px") #needs to be .jpg
#ggsave("CumulativeQuantile_KL_Sestiere_Final.jpg",width=2100,height=1500,units="px")
#ggsave("~/VenetianPlague_Entropy/Figures/Figure4.png",width=2100,height=1500,units="px")
#ggsave("~/VenetianPlague_Entropy/Figures/Figure4.jpg",width=2100,height=1500,units="px")
ggsave("~/VenetianPlague_Entropy/Figures_Final/Figure4.png",width=2100,height=1500,units="px")
ggsave("~/VenetianPlague_Entropy/Figures_Final/Figure4.jpg",width=2100,height=1500,units="px")