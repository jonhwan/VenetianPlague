library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(entropy)
library(forcats)
library(rootSolve)
library(data.table)
library(msos)
library(entropy)
library(simboot)
library(MCPAN)
library(boot)
library(gridExtra)
library(cowplot)
library(ggpubr)
library(progress)

all <- read.csv("ALL_utf.csv") #all contains 71 columns, population contains 75, filled contains 55.
city_peaks <- read.csv("city_cases_baseline_removed.csv")
filled <- read.csv("filledna_full_ts.csv")
geo_df <- read.csv("geo_df.csv")
population <- read.csv("ParishPopulations.csv") #year is 1696
sestiere_pre <- read.csv("sestiereDataPre.csv")
sestiere_post <- read.csv("SestiereDataPost.csv")

#Fix the Dates on all
all$Date <- sub("2029","1629",all$Date)
all$Date <- sub("2030","1630",all$Date)
all$Date <- sub("2031","1631",all$Date)
all$Date <- as.Date(all$Date, format="%m/%d/%Y")
colnames(all) <- sub("S.","S",colnames(all))
for(column in 1:ncol(all)){
  if(column == 1){
    next
  }
  if(!(is.integer(all[,column]))){
    all[,column] = as.integer(all[,column])
  }
}

#Fix the Dates on filled
colnames(filled)[1] <- "Date"
filled$Date <- as.Date(filled$Date, format="%Y-%m-%d")
colnames(filled) <- sub("S.","S",colnames(filled))
for(column in 1:ncol(filled)){
  if(column == 1){
    next
  }
  if(!(is.integer(filled[,column]))){
    filled[,column] = as.integer(filled[,column])
  }
}

#Fix the Dates on city_peaks
colnames(city_peaks)[1] <- "Date"
city_peaks$Date <- sub(" 00:00:00","",city_peaks$Date)
city_peaks$Date <- as.Date(city_peaks$Date, format="%Y-%m-%d")

#Fix geo_df
colnames(geo_df)[1] <- "Parish"
geo_df$Parish <- gsub("[[:space:]]",".",geo_df$Parish)
geo_df$Parish <- sub("S.","S",geo_df$Parish)
#Get rid of Angelo.Rafael because it has no lat/long data
#geo_df <- geo_df[!grepl("Angelo.Rafael",geo_df$Parish),]

#Fix population
population$Parish <- gsub("[[:space:]]",".",population$Parish)
population$Parish <- sub("S.","S",population$Parish)

#What are the missing cities in geo_df?
setdiff(geo_df$Parish,colnames(filled))

#What are the missing cities in population?
filled <- filled[,-match(c("S.Marcilian"),colnames(filled))] #Marcilian exists in the population_pre dataset.

#filled <- all
all_nas <- c()
for(col in 1:ncol(filled)){
  if(sum(is.na(filled[,col])) == length(filled[,col])){
    all_nas <- c(all_nas,col)
  }
}
if(length(all_nas) > 0){
  filled <- filled[,-all_nas]
}

missing_parishes <- c()
for(p in 1:length(colnames(filled))){
  if(!(colnames(filled)[p] %in% population$Parish) & colnames(filled)[p] != "Date"){
    missing_parishes <- c(missing_parishes,p)
  }
}
if(length(missing_parishes) > 0){
  filled <- filled[,-missing_parishes]
}

#Get rid of S. Marco, the outlier
#filled <- filled[,-match("S.Marco",colnames(filled))]

population_pre <- read.csv("ParishPopulationsPre.csv")
population_post <- read.csv("ParishPopulationsPost.csv")

colnames(population_pre) <- c("Parish", "Population_1509", "Population_1551", "Population_1581", "Population_1586")

population_pre$Parish <- gsub("[[:space:]]",".",population_pre$Parish)
population_pre$Parish <- sub("S.","S",population_pre$Parish)

population_post$Parish <- gsub("[[:space:]]",".",population_post$Parish)
population_post$Parish <- sub("S.","S",population_post$Parish)

remove_vec <- c()
for(i in 1:length(population_pre$Parish)){
  if(!(population_pre$Parish[i] %in% population_post$Parish)){
    remove_vec <- c(remove_vec,i)
  }
}

if(length(remove_vec) > 0){
  population_pre <- population_pre[-remove_vec,]
}

remove_vec <- c()
for(i in 1:length(population_post$Parish)){
  if(!(population_post$Parish[i] %in% population_pre$Parish)){
    remove_vec <- c(remove_vec, i)
  }
}

if(length(remove_vec) > 0){
  population_post <- population_post[-remove_vec,]
}

population_post <- population_post %>% arrange(Parish)
population_pre <- population_pre %>% arrange(Parish)

population_pre <- population_pre[,-2]

for(j in 1:ncol(population_pre)){
  if(j == 1){
    next
  }
  for(i in 1:nrow(population_pre)){
    population_pre[i,j] <- format(as.numeric(population_pre[i,j]),nsmall=3)
    if(nchar(population_pre[i,j]) < 7){
      population_pre[i,j] <- as.numeric(population_pre[i,j])*1000
    } else {
      population_pre[i,j] <- as.numeric(population_pre[i,j])
    }
  }
  population_pre[,j] <- as.numeric(population_pre[,j])
}
population_post$Population <- as.numeric(population_post$Population)*1000

population_df <- as.data.frame(cbind(population_pre,population_post[,-1]))
colnames(population_df) <- c("Parish", "Population_1551", "Population_1581", "Population_1586", "Population_1696")
t_pop_df <- transpose(population_df)
rownames(t_pop_df) <- colnames(population_df)
t_pop_df <- t_pop_df[-1,]
colnames(t_pop_df) <- population_df$Parish
year_vec <- c(c(1551,1581,1586,1696))
t_pop_df <- cbind(year_vec,t_pop_df)
t_pop_df_plot <- t_pop_df %>% 
  gather(key="Parish_var",value="value",-year_vec)
t_pop_df_plot$value <- as.numeric(t_pop_df_plot$value)

# population_df %>%
#   ggplot(aes(x=Population_1586,y=Population_1696))+
#   geom_point(size=0.5)+
#   geom_smooth(method=lm,se=FALSE,linewidth=0.5)
population_df %>%
  ggplot(aes(x=Population_1586,y=Population_1696))+
  geom_point(size=0.5)+
  geom_smooth(method=lm,se=FALSE,linewidth=0.5)+
  theme_bw()+
  theme(text=element_text(family=""),panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  labs(x="Population in 1586",y="Population in 1696",title="Population in 1696 vs. Population in 1586")
ggsave("SuppFig1.jpg",width=1920,height=1080,units="px")
cor(population_df$Population_1586,population_df$Population_1696) #Super high correlation like 0.95, Tom says because no room for houses so different people but same house, no room for development
plot(x=population_df$Population_1586,y=population_df$Population_1696)
abline(mod <- lm(population_df$Population_1696 ~ population_df$Population_1586)) #y=1696, x=1586 so slope smaller than 1 means that population went down over time
intercept <- as.numeric(coef(mod)[1])
slope <- as.numeric(coef(mod)[2]) #slope, basically 1 so we feel more comfortable about interpolating because it basically doesn't change much (though the pandemic does cause deaths). But I ran again and now 0.899? Look into

interpolated_1629 <- (intercept - (1-slope)*population_pre$Population_1586)*43/110 + population_pre$Population_1586

t_pop_df_plot %>%
  ggplot(aes(x=year_vec,y=value))+
  geom_line(aes(color=Parish_var),show.legend=FALSE,linewidth=0.5)+
  labs(x="Year",y="Population",title="Population over Time for each Parish")+
  scale_y_log10()+
  theme_minimal()
ggsave("PopulationsOverTimeNoLegendYLog10.png",width=1920,height=1080,units="px")

#geo_df doesn't have S.Giacomo.dell'Orio and Angelo.Rafael is blank.

#Fix the dates on sestiere_dfs
sestiere_pre$Parish <- gsub("[[:space:]]",".",sestiere_pre$Parish)
sestiere_pre$Parish <- sub("S.","S",sestiere_pre$Parish)
sestiere_post$Parish <- gsub("[[:space:]]",".",sestiere_post$Parish)
sestiere_post$Parish <- sub("S.","S",sestiere_post$Parish)

#arrange grouping by sestiere
sestiere_pre <- sestiere_pre %>% arrange(Sestiere,Parish)
sestiere_post <- sestiere_post %>% arrange(Sestiere,Parish)

#Match sestiere_dfs with those on filled

sestiere_df <- sestiere_pre

remove_vec <- c()
for(i in 1:length(sestiere_df$Parish)){
  if(!(sestiere_df$Parish[i] %in% colnames(filled[,-1]))){
    remove_vec <- c(remove_vec,i)
  }
}

if(length(remove_vec) > 0){
  sestiere_df <- sestiere_df[-remove_vec,]
}

filled_1629 <- filled[year(filled$Date)==1629,]
filled_1630 <- filled[year(filled$Date)==1630,]
filled_1631 <- filled[year(filled$Date) == 1631,]