
#Call up necessary packages
library(tidyverse)

#Set working directory
setwd("~/ZM_Project/ZM_Vanessa_Poster_Stats")

#Read in data file
data <- read.csv("ZM_FMT_Data.csv", header=T)

#Make some preliminary plots
ggplot(data, aes(Mass, Total_Number_ZM_Consmed_After_24_Hours))+
  geom_point()+
  geom_smooth(method="lm")


ggplot(data, aes(Site, Total_Number_ZM_Consmed_After_24_Hours))+
  geom_boxplot()

ggplot(data, aes(Plastron_Length, Total_Number_ZM_Consmed_After_24_Hours))+
  geom_point()+
  geom_smooth(method="lm")

ggplot(data, aes(Plastron_Length, Number_ZM_Small_Consumed_After_24_Hours))+
  geom_point()

ggplot(data, aes(Plastron_Length, Number_ZM_Medium_Consumed_After_24_Hours))+
  geom_point()

ggplot(data, aes(Plastron_Length, Number_ZM_Large_Consumed_After_24_Hours))+
  geom_point()

ggplot(data, aes(Age, Total_Number_ZM_Consmed_After_24_Hours, group=Age))+
  geom_boxplot()

#Making a smaller data subset with just the total number of mussels of each size class eaten for bar plot
sub_data <- data %>% 
  summarise(
    total_small = sum(Number_ZM_Small_Consumed_After_24_Hours),
    total_medium = sum(Number_ZM_Medium_Consumed_After_24_Hours),
    total_large = sum(Number_ZM_Large_Consumed_After_24_Hours),
    total_snails = sum(Number_Snails_Consumed_After_24_Hours)
)


sub_data_2 <- data.frame(size=c("Small", "Medium", "Large", "Snails"),
                         total=c(123, 87, 64, 211))


ggplot(sub_data_2, aes(total,size))+
  geom_bar(stat="identity")

ggplot(sub_data_2, aes(size,total))+
  geom_bar(stat="identity")



sub_data_3 <- data %>% 
  group_by(Site) %>% 
  summarise(
    total_small = sum(Number_ZM_Small_Consumed_After_24_Hours),
    total_medium = sum(Number_ZM_Medium_Consumed_After_24_Hours),
    total_large = sum(Number_ZM_Large_Consumed_After_24_Hours),
    total_snails = sum(Number_Snails_Consumed_After_24_Hours)
  )

sub_data_4 <- data.frame(site=c("59_Mile_Stretch", "Lake_Francis_Case", "59_Mile_Stretch", "Lake_Francis_Case", "59_Mile_Stretch", "Lake_Francis_Case","59_Mile_Stretch", "Lake_Francis_Case"),
                         size=c("Small", "Small", "Medium", "Medium", "Large", "Large", "Snails", "Snails"),
                         total=c(66, 57, 52, 35, 31, 33, 110, 101))

ggplot(sub_data_4, aes(total,size, fill=site))+
  geom_bar(stat="identity")


zm.df.summary <- data %>% 
  group_by(Site) %>% 
  summarise(
    Tot_SD = sd(Total_Number_ZM_Consmed_After_24_Hours),
    Tot_Mean = mean(Total_Number_ZM_Consmed_After_24_Hours),
    Tot_n = length(Total_Number_ZM_Consmed_After_24_Hours),
    Tot_SE = (mean(Total_Number_ZM_Consmed_After_24_Hours))/(sqrt(Tot_n)),
    Small_SD = sd(Number_ZM_Small_Consumed_After_24_Hours),
    Small_Mean = mean(Number_ZM_Small_Consumed_After_24_Hours),
    Small_n = length(Number_ZM_Small_Consumed_After_24_Hours),
    Small_SE = (mean(Number_ZM_Small_Consumed_After_24_Hours))/(sqrt(Small_n)),
    Medium_SD = sd(Number_ZM_Medium_Consumed_After_24_Hours),
    Medium_Mean = mean(Number_ZM_Medium_Consumed_After_24_Hours),
    Medium_n = length(Number_ZM_Medium_Consumed_After_24_Hours),
    Medium_SE = (mean(Number_ZM_Medium_Consumed_After_24_Hours))/(sqrt(Medium_n)),
    Large_SD = sd(Number_ZM_Large_Consumed_After_24_Hours),
    Large_Mean = mean(Number_ZM_Large_Consumed_After_24_Hours),
    Large_n = length(Number_ZM_Large_Consumed_After_24_Hours),
    Large_SE = (mean(Number_ZM_Large_Consumed_After_24_Hours))/sqrt(Large_n)
  )
View(zm.df.summary)

#Plot Standard Error
ggplot(data, aes(Site, Total_Number_ZM_Consmed_After_24_Hours, col=Site))+
  geom_jitter()+
  labs(x="Site Turtles Were \n Collected From", y="Total Number of Zebra Mussels of All \n Size Classes Consumed After 24 Hours")+
  geom_errorbar(data=zm.df.summary, mapping=aes(x=Site, y=Tot_Mean, ymin=Tot_Mean-Tot_SE, ymax=Tot_Mean+Tot_SE), color="black")+
  geom_point(data=zm.df.summary, mapping=aes(x=Site, y=Tot_Mean), color="black")

#Plot Standard Deviation
ggplot(data, aes(Site, Total_Number_ZM_Consmed_After_24_Hours, col=Site))+
  geom_point(position="jitter", size=2)+
  labs(x="Site Turtles Were Collected From", y="Total Number of Zebra Mussels of All \n Size Classes Consumed After 24 Hours per Turtle")+
  geom_errorbar(data=zm.df.summary, mapping=aes(x=Site, y=Tot_Mean, ymin=Tot_Mean-Tot_SD, ymax=Tot_Mean+Tot_SD), color="black")+
  geom_point(data=zm.df.summary, mapping=aes(x=Site, y=Tot_Mean), color="black", size=2)+
  scale_x_discrete(labels=c("59_Mile_Stretch" = "59 Mile Stretch", "Lake_Francis_Case" = "Lake Francis-Case"))+
  theme(legend.position="none")+
  theme(text = element_text(size=14))


zm.df.summary.2 <- data2 %>% 
  group_by(Size) %>% 
  summarise(
    mean_consumed = mean(Number_Consumed_Per_Turtle),
    sd_consumed = sd(Number_Consumed_Per_Turtle)
  )
View(zm.df.summary.2)

ggplot(data, aes(Size, Number_Consumed_Per_Turtle, color=Size))+
  geom_jitter(aes(shape=Site), height=0.25, size=2)+
  labs(x="Size Class of Zebra Mussels and Snails", y="Total Number of Zebra Mussels and Snails of Each \n Size Class Consumed After 24 Hours per Turtle")+
  geom_errorbar(data=zm.df.summary.2, mapping=aes(x=Size, y=mean_consumed, ymin=mean_consumed-sd_consumed, ymax=mean_consumed+sd_consumed), color="black")+
  geom_point(data=zm.df.summary.2, mapping=aes(x=Size, y=mean_consumed), color="black")+
  annotate(geom="text", x=1.1, y=1.35, label="a")+
  annotate(geom="text", x=2.1, y=1.83, label="a")+
  annotate(geom="text", x=3.1, y=2.56, label="b")+
  annotate(geom="text", x=4.1, y=4.4, label="c")+
  theme(text = element_text(size=14))



sizeAnova <- lm(Number_Consumed_Per_Turtle~Size, data=data2)  
anova(sizeAnova)

sizeAnovasummary <- summary(sizeAnova)
sizeAnovasummary

kruskal.test(Number_Consumed_Per_Turtle~Size, data=data2)

ggplot(data2, aes(Size, Number_Consumed_Per_Turtle))+
  geom_bar(stat="identity")


data2 %>% 
  mutate(Size = fct_reorder(Size, desc(Number_Consumed_Per_Turtle))) %>% 
  ggplot(aes(Size, Number_Consumed_Per_Turtle))+
  geom_bar(stat="identity", position=position_dodge2())+
  labs(x="Size Class of Zebra Mussels and Snails", y="Total Number of Zebra Mussels and Snails of Each \n Size Class Consumed After 24 Hours")+
  theme(text = element_text(size=14))+
  theme(legend.position="bottom")+
  scale_fill_discrete(name="Turtle Locality", labels=c("59 Mile Stretch", "Lake Francis-Case"))

zm.df.summary.2 %>% 
  #mutate(Size=fct_reorder(Size,desc(mean_consumed))) %>% 
  ggplot(aes(Size, mean_consumed))+
  geom_bar(stat="identity", fill="seagreen4")+
  labs(x="Size Class of Zebra Mussels and Snails", y="Mean Number of Zebra Mussels and Snails of Each \n Size Class Consumed After 24 Hours")+
  theme(text = element_text(size=14))+
  geom_errorbar(aes(Size, mean_consumed, ymin=mean_consumed, ymax=mean_consumed+sd_consumed, width=0.5))

sub_5 <- data2 %>% 
  group_by(Size, Site) %>% 
  summarise(
    totals = sum(Number_Consumed_Per_Turtle)
  )
sub_5


ggplot(sub_5,aes(Size, totals, fill=Site))+
  geom_bar(stat="identity", position=position_dodge2())+
  scale_fill_manual(values=c("59_Mile_Stretch"="deeppink4", "Lake_Francis_Case"="cyan4"), 
                    name="Turtle Locality", 
                    labels=c("59 Mile Stretch", "Lake Francis-Case"))+
  labs(x="Size Class of Zebra Mussels and Snails", y="Total Number of Zebra Mussels and Snails of Each \n Size Class Consumed After 24 Hours")+
  theme(text = element_text(size=14))+
  theme(legend.position="bottom")
  
