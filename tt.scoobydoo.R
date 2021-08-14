### Tidy Tuesday Project Visualizing Scooby Doo Data ###
#this syntax can be used to plot and analyze arrests as a function of crime motive

#load libraries
library(tidyverse) #read in and manipulate data
library(ggplot2) #plotting 

#read in data
raw <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-13/scoobydoo.csv")

#count number of arrests as a function of motive
arrest_data<-raw%>%
  filter(arrested !="NULL")%>%
  count(arrested, motive)
View(arrest_data)

#calculate percent arrests within each motive
totals<-aggregate(n~motive, data=arrest_data,FUN=sum)
colnames(totals)[colnames(totals)=="n"]<-"Total"
arrest_data<-subset(arrest_data, arrested=="TRUE")
colnames(arrest_data)[colnames(arrest_data)=="n"]<-"Arrests"
arrest_data<-merge(arrest_data, totals, by="motive")
arrest_data$pct_arrest<-(arrest_data$Arrests/arrest_data$Total)*100



#bar plot of arrests as a function of motive

#set theme
my_theme<-theme(
  plot.title = element_text(size=28, face="bold"),
  axis.title.x = element_text(size=25, face="bold"),
  axis.title.y = element_text(size=25, face="bold"),
  axis.text.y = element_text(size=15, face="bold", color="black"),
  axis.text.x = element_text(size=15, angle=-45, hjust = 0, face="bold", color="black"),
  legend.title = element_blank(),
  legend.text = element_text(size = 20, face="bold"),
  legend.key=element_blank(),
  panel.background = element_rect(fill="white", colour="white"),  
  panel.border = element_rect(colour = "black", fill=NA, size=2), 
) 


#set colors
my_colors<-c("#128a84","#00cfd4","#79af30",  "#D0D61B",
             "#F9E014","#F47920","#A44138","#bb5c37", "#B06E0E", 
             "#6A3400", "#4b0055", "#A091C6", "#f1d7f9")

#create plot
ggplot(data=arrest_data)+
  geom_bar(stat="identity", aes(x=motive, y=pct_arrest, fill=motive),color="black") +
  ggtitle("Arrest Rates of Scooby Doo Villains by Motive")+
  ylab("% Arrests")+xlab("Motive")+
  my_theme+
  scale_fill_manual(values=my_colors)
    
#save plot
ggsave("scooby_arrests.tiff", width = 30, height = 20, units = "cm")








