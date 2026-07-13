library(dplyr)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(ggpubr)

Day <- c(1:nrow(CE_df))
CE_df_Plot <- cbind(CE_df,Day)
CE_df_Plot %>%
  ggplot(aes(x=Day,y=Uniform))+
  geom_point(size=0.5)+
  labs(y="Uniform Cross Entropy",x="Day since Jan 1, 1629", title="Daily Uniform Cross Entropy")
ggsave("UniformCrossEntropyPerDay.png",width=1920,height=1080,units="px")

CE_df_Plot %>%
  ggplot(aes(x=Day,y=Shannon))+
  geom_point(size=0.5)+
  labs(y="Shannon Entropy",x="Day since Jan 1, 1629", title="Daily Shannon Entropy")
ggsave("ShannonCrossEntropyPerDay.png",width=1920,height=1080,units="px")

Cumulative_Uniform <- cumsum(CE_df$Uniform)
CE_df_Plot <- cbind(CE_df_Plot,Cumulative_Uniform)
CE_df_Plot %>%
  ggplot(aes(x=Day,y=Cumulative_Uniform)) +
  geom_point(size=0.5)+
  labs(y="Cumulative Uniform Cross Entropy",x="Day since Jan 1, 1629",title="Cumulative Uniform Cross Entropy")
ggsave("UniformCrossEntropyCumulative.png",width=1920,height=1080,units="px")

Cumulative_Shannon <- cumsum(CE_df$Shannon)
CE_df_Plot <- cbind(CE_df_Plot, Cumulative_Shannon)
CE_df_Plot %>%
  ggplot(aes(x=Day,y=Cumulative_Shannon)) +
  geom_point(size=0.5)+
  labs(y="Cumulative Shannon Entropy",x="Day since Jan 1, 1629",title="Cumulative Shannon Entropy")
ggsave("ShannonCrossEntropyCumulative.png",width=1920,height=1080,units="px")

filtered_Shannon <- stats::filter(data.frame(CE_df$Shannon),rep(1/7,7))
filtered_Shannon <- as.data.frame(filtered_Shannon[!is.na(filtered_Shannon)])
colnames(filtered_Shannon) <- c("Shannon")

fourier_Shannon <- fft(CE_df$Shannon)
fourier_Shannon[20:1075] = 0+0i
ifourier_Shannon <- fft(fourier_Shannon,inverse=TRUE)/length(fourier_Shannon)

filtered_Shannon %>%
  ggplot(aes(x=1:length(Shannon),y=Shannon))+
  geom_point(size=0.5)+
  labs(y="Shannon Entropy",x="Day since Jan 1, 1629", title="Daily Shannon Entropy")

f_Shannon <- data.frame(Re(ifourier_Shannon))
colnames(f_Shannon) <- c("Shannon")
f_Shannon %>%
  ggplot(aes(x=1:length(Shannon),y=Shannon))+
  geom_point(size=0.5)+
  labs(y="Shannon Entropy",x="Day since Jan 1, 1629", title="Daily Shannon Entropy")

Cumulative_Shannon <- cumsum(filtered_Shannon$Shannon)
shannon_plot <- cbind(filtered_Shannon, Cumulative_Shannon)
shannon_plot %>%
  ggplot(aes(x=1:length(Shannon),y=Cumulative_Shannon)) +
  geom_point(size=0.5)+
  labs(y="Cumulative Shannon Entropy",x="Day since Jan 1, 1629",title="Cumulative Shannon Entropy")

# Cumulative_fShannon <- cumsum(f_Shannon$Shannon)
# fshannon_plot <- cbind(f_Shannon, Cumulative_fShannon)
# fshannon_plot %>%
#   ggplot(aes(x=1:length(Shannon),y=Cumulative_fShannon)) +
#   geom_point(size=0.5)+
#   labs(y="Cumulative Shannon Entropy",x="Day since Jan 1, 1629",title="Cumulative Shannon Entropy")

plot.ts(CE_df$Uniform)
#plot.ts(filtered_Shannon)
plot.ts(CE_df$Shannon)
plot.ts(CE_df$Sum)
plot.ts(CE_df$Exponential)
plot.ts(CE_df$Normal)
plot.ts(CE_df$NormalCumulative)

plot.ts(cumsum(CE_df$Uniform))
#plot.ts(cumsum(filtered_Shannon))
plot.ts(cumsum(CE_df$Shannon))
plot.ts(cumsum(CE_df$Sum))
plot.ts(cumsum(CE_df$Exponential))
plot.ts(cumsum(CE_df$Normal))
plot.ts(cumsum(CE_df$NormalCumulative))