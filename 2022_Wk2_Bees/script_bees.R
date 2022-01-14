###tidy tuesday project visualizing bee colony data###
#this syntax can be used to plot the change in bee colonies over time

#set working directory and load libraries
setwd("~/Desktop/School/Stats/R Skills/TidyTuesday/Bees")
library(tidyr)
library(dplyr)
library(ggplot2)

#read in data
tuesdata <- tidytuesdayR::tt_load('2022-01-11')
data<-tuesdata$colony
data<-na.omit(data) #remove missing values


#calculate average colonies gained and lost per year
bees<-data%>% 
  group_by(year) %>% 
  summarize(avg_lost=mean(colony_lost), 
            avg_gained=mean(colony_added))

#calculate net change in bee colonies per year and label as greater or less than zero
bees$net_change<-bees$avg_gained-bees$avg_lost
bees$gained<-ifelse(bees$net_change>0, "Overall Gain", "Overall Loss")


#set plotting theme
my_theme<-theme(
  plot.title = element_text(size=20, face="bold", color="white"),
  axis.title.x = element_text(size=25, face="bold", color="white"),
  axis.title.y = element_text(size=25, face="bold", color="white"),
  axis.text.y = element_text(size=20, face="bold", color="white"),
  axis.text.x = element_text(size=20, face="bold", color="white"),
  legend.position = "none",
  panel.border = element_rect(colour = "black", fill=NA, size=2), 
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  plot.background = element_rect(fill = "#083B53")
) 

#plot gains and losses in colonies per year, where green is a net gain and red is a loss
ggplot(data=bees, aes(x=year, y=net_change, fill=gained))+
  geom_bar(stat="identity", color="black")+
  scale_fill_manual(values = c("#0C9925", "#DC1212"))+
  ylab("Net Change in Colony Number")+
  xlab("Year")+
  ggtitle("Sharp losses in bee colonies during 2019 were recovered in 2020 and 2021")+
  my_theme

ggsave("bees.png", width = 30, height = 20, units = "cm")




