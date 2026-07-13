library(dplyr)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(ggpubr)

bcDateChar <- "1631-05-01"
bcDate <- as.Date(bcDateChar,format="%Y-%m-%d")
bcDeaths <- as.integer(filled[bcDate == filled$Date, -1])
bcParishes <- colnames(filled[,-1])
bc_df <- data.frame(bcParishes,bcDeaths)

Cumulative <- function(date, df){
  tracker_vec <- c()
  cols <- colnames(df[,-1])
  for(i in 1:ncol(df)){
    if(i == 1){
      next
    }
    tracker_vec <- c(tracker_vec,sum(df[1:match(date,df$Date),i]))
  }
  return_df <- data.frame(tracker_vec,cols)
  colnames(return_df) <- c("cumulativeDeaths","parish_list")
  return(return_df)
}

bc_df %>%
  mutate(population_corrected = bcDeaths/(population_specified$Population*1000)) %>%
  arrange(population_corrected) %>%
  ggplot(aes(y=factor(bcParishes,level=bcParishes[order(population_corrected)]),x=population_corrected)) +
  geom_bar(stat = "identity") +
  ggtitle(bcDateChar) +
  labs(y="Parish",x="Deaths/Population")
ggsave("DeathsBarChart.png",width=1920,height=1080,units="px")

monthlyCumulativeCDF <- function(plot_year,showgrid=FALSE){
  plot_list <- list()
  filled_months <- filled[day(filled$Date)==1,]
  parish_list <- colnames(filled_months[,-1])
  for(date in unique(filled_months$Date)){
    cumulative_df <- Cumulative(date,filled)
    if(showgrid == FALSE){
      p <- cumulative_df %>%
        mutate(population_corrected = cumulativeDeaths/(population_specified$Population*1000)) %>%
        arrange(population_corrected) %>%
        ggplot(aes(y=factor(parish_list,level=parish_list[order(population_corrected)]),x=population_corrected))+
        geom_bar(stat="identity")+
        ggtitle(paste("Cumulative up to",format(as.Date(date,origin="1970-01-01")))) +
        labs(y="Parish",x="Cumulative Deaths/Population")
    } else {
      p <- cumulative_df %>%
        mutate(population_corrected = cumulativeDeaths/(population_specified$Population*1000)) %>%
        arrange(population_corrected) %>%
        ggplot(aes(y=factor(parish_list,level=parish_list[order(population_corrected)]),x=population_corrected))+
        geom_bar(stat="identity")+
        ggtitle(paste("Cumulative up to",format(as.Date(date,origin="1970-01-01")))) +
        labs(y="Parish",x="Cumulative Deaths/Population") +
        #theme(axis.text.y = element_blank())
        scale_y_discrete(labels=axis_labels)
    }
    if(year(date) == plot_year){
      plot_list <- c(plot_list,list(p))
    }
    print(p)
  }
  if(showgrid == TRUE){
    grid <- ggarrange(plotlist=plot_list[c(1,7,2,8,3,9,4,10,5,11,6,12)],ncol=2,nrow=6)
    grid <- annotate_figure(grid, top = text_grob(as.character(plot_year),color="red",face="bold",size=14))
    print(grid)
    ggsave(paste("AnnualCumulativeCDFGridfor",as.character(plot_year),"v2.png",sep=""),width=1920,height=2100,units="px") #less wide and more tall?
  }
}

cumulative_df <- Cumulative(bcDate,filled)
colnames(cumulative_df) <- c("cumulativeDeaths","bcParishes")

cumulative_df %>%
  mutate(population_corrected = cumulativeDeaths/(population_specified$Population*1000)) %>%
  arrange(population_corrected) %>%
  ggplot(aes(y=factor(bcParishes,level=bcParishes[order(population_corrected)]),x=population_corrected))+
  geom_bar(stat="identity")+
  ggtitle(paste("Cumulative up to",bcDateChar)) +
  labs(y="Parish",x="Cumulative Deaths/Population")

hist_df <- data.frame(sort(unique(bcDeaths)),as.data.frame(table(bc_df$bcDeaths))$Freq)
colnames(hist_df) <- c("Deaths","NParishes")

bc_df %>% #could do geom_histogram and just x=bcDeaths but it looks ugly
  mutate(population_corrected = bcDeaths/(population_specified$Population*1000)) %>%
  ggplot(aes(x=population_corrected)) +
  geom_histogram(binwidth=0.0005) + #should be proportional to dataset
  ggtitle(bcDateChar) +
  labs(y="# of Parishes",x="Deaths/Population")
ggsave("IncidenceRateBarChart.png",width=1920,height=1080,units="px")

cumulative_df %>%
  mutate(population_corrected = cumulativeDeaths/(population_specified$Population*1000)) %>%
  ggplot(aes(x=population_corrected,y=after_stat(density)))+
  geom_histogram(bins=50)+
  geom_density()+
  scale_x_continuous()+
  scale_y_continuous()+
  expand_limits(x=c(0,0.5),y=c(0,6))+
  ggtitle(paste("Cumulative up to",bcDateChar))+
  labs(y = "Density of Parishes",x="Cumulative Deaths/Population")

monthlyCumulativePDF <- function(plot_year,showgrid=FALSE){
  plot_list <- list()
  filled_months <- filled[day(filled$Date)==1,]
  if(showgrid == TRUE){
    y_label <- "Density"
  } else {
    y_label <- "Density of Parishes"
  }
  for(date in unique(filled_months$Date)){
    cumulative_df <- Cumulative(date,filled)
    p <- cumulative_df %>%
      mutate(population_corrected = cumulativeDeaths/(population_specified$Population*1000)) %>%
      ggplot(aes(x=population_corrected,y=after_stat(density))) +
      geom_histogram(bins=50)+ #annoying thing about this bin size and fixed axes 
      #(N bins and bin size basically identical in fixed axes)
      #is they all get clumped up at the beginning but it's necessary to show the end part
      geom_density()+
      scale_y_continuous()+
      scale_x_continuous()+
      expand_limits(x=c(0,0.65),y=c(0,40)) +
      ggtitle(paste("Cumulative up to",format(as.Date(date,origin="1970-01-01"))))+
      labs(y = y_label, x = "Cumulative Deaths/Population") #Density of Parishes
    if(year(date) == plot_year){
      plot_list <- c(plot_list, list(p))
    }
    print(p)
  }
  if(showgrid == TRUE){
    grid <- ggarrange(plotlist=plot_list[c(1,7,2,8,3,9,4,10,5,11,6,12)],ncol=2,nrow=6)
    grid <- annotate_figure(grid, top = text_grob(as.character(plot_year),color="red",face="bold",size=14))
    print(grid)
    ggsave(paste("AnnualCumulativePDFGridfor",as.character(plot_year),".png",sep=""),width=1920,height=2100,units="px")
  }
}

monthly_histogram <- function(year_data){
  plots <- list()
  for(month in unique(month(year_data$Date))){
    month_data <- year_data[month(year_data$Date) == month,]
    month_data_modified <- month_data[,-1]
    cumulative <- rep(0,ncol(month_data_modified))
    zeroes <- c()
    for(parish in 1:ncol(month_data_modified)){
      if(sum(month_data_modified[,parish]) == 0){ #if the sum is zero there likely were NAs for this month, so we don't count
        zeroes <- c(zeroes,parish)
      }
      cumulative[parish] <- sum(month_data_modified[,parish])/(population_specified[parish,match("Population",colnames(population_specified))]*1000)
    }
    cumulative <- cumulative[-zeroes]
    cumulative <- data.frame(cumulative)
    colnames(cumulative) <- c("CumulativeCasesperParish")
    p <- ggplot(cumulative, aes(x=CumulativeCasesperParish,y=after_stat(density))) +
      geom_histogram(bins=50) +
      geom_density()+
      labs(y="Parish Density",x="Incidence Rate this Month",title=month) +
      scale_x_continuous() +
      expand_limits(x=c(0,0.175)) +
      scale_y_continuous()+
      expand_limits(y=c(0,300))
    #xlim(0,0.175)+ #if want to generalize for any year, run a separate for loop to find the max incidence rate and the max counts (the latter using hist$count)
    #ylim(0,200)
    print(p)
    plots <- c(plots, list(p))
  }
  plot_grid <- cowplot::plot_grid(plotlist=plots,ncol=2,nrow=6,byrow=FALSE)
  plot_grid <- annotate_figure(plot_grid, top = text_grob(as.character(year(year_data$Date[1])),color="red",face="bold",size=14))
  print(plot_grid)
  ggsave(paste("MonthlyHistogramGridfor",as.character(year(year_data$Date[1])), ".png", sep="",collapse=NULL),width=1500,height=2100,units="px")
}