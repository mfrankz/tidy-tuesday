###tidy tuesday project visualizing olympic data###
#this syntax can be used to plot the age of male and female olympians over time


#set up working directory and load libraries
setwd("~/Desktop/School/Stats/R Skills/TidyTuesday/Olympian")
library(tidytuesdayR)
library(tidyverse)
library(ggplot2)


#read in data
data <- tidytuesdayR::tt_load(2021, week = 31)
olympics <- data$olympics
olympics <- olympics[!is.na(olympics$age), ] #remove NAs


#calculate average age of male and female olympians each year
avg_age<-olympics%>% 
  group_by(year, sex) %>% 
  summarize(MEAN = mean(age))

#format labels for the sex variable 
avg_age<-avg_age %>% 
  mutate(sex = recode(sex, F="Female", M="Male"))



#set plotting theme
my_theme<-theme(
  plot.title = element_text(size=30, face="bold"),
  axis.title.x = element_text(size=25, face="bold"),
  axis.title.y = element_text(size=25, face="bold"),
  axis.text.y = element_text(size=15, face="bold", color="black"),
  axis.text.x = element_text(size=14, hjust = 0, face="bold", color="black"),
  legend.title = element_blank(),
  legend.text = element_text(size = 20, face="bold"),
  legend.key=element_blank(),
  panel.background = element_rect(fill="azure3", colour="azure3"),  
  panel.border = element_rect(colour = "black", fill=NA, size=2), 
  strip.text.x = element_text(size = 25, face="bold"),
  strip.background = element_rect(color="white", fill="white"),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
) 


#preliminary visualization of the data
ggplot(data=avg_age, aes(x=year, y=MEAN))+ 
  geom_line(aes(color=sex),size=1.5)+
  facet_wrap(~sex)+
  scale_color_manual(values = c("firebrick4", "steelblue4"))+
  my_theme

#inspect outlier that emerged from the preliminary visualization
avg_age[which.max(avg_age$MEAN),] #identify which year contains the outlier
temp<-subset(olympics, year==1904 & sex=="F") #inspect data for 1904 (year containing outlier)
view(temp) #4 members of the US female archery team were over the age of 40

#create annotation to describe outlier on female plot
ann_text <- data.frame(geom="text", year=1933, MEAN=48, color="black", size=2.8, fontface=2, hjust=0,
                       lab = "In 1904, 4 out of 5 members\nof the only female team (USA\nArchery) were over 40 years old",
                       sex = factor("Female",levels = c("Female","Male")))

#create plot of age of male versus female olympians
ggplot(data=avg_age, aes(x=year, y=MEAN))+ 
  geom_line(aes(color=sex),size=1.5)+
  facet_wrap(~sex)+
  ggtitle("AVERAGE AGE OF OLYMPIANS BY YEAR")+
  xlab("YEAR")+
  ylab("AVERAGE AGE")+
  coord_cartesian(ylim=c(0,50))+
  geom_text(data = ann_text,label = 
              "In 1904, 4 out of 5 members\nof the only female team (USA\nArchery) were over 40 years old")+
  scale_color_manual(values = c("firebrick4", "steelblue4"))+
  my_theme

ggsave("olympian_age.tiff", width = 35, height = 20, units = "cm")
