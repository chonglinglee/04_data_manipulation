#Created by Anastasia Stoops 10-26-2020 inspired by van Langen, J. (2020). Open-visualizations in R and Python. 
#https://github.com/jorvlan/open-visualizations


library(tidyverse)
library(ggplot2)
library(gghalves)

#width and height variables for saved plots
w = 10
h = 7


#set working directory
setwd("/Users/astoops/Documents/Self_Paced_Reading/Raw Datasets/")

##Step1 - load in the data
RT_accurate <- read.csv("Cleaned_211_Correct.csv")
RT_cleaned<-read.csv("Cleaned_211_All_accuracy.csv")
str(RT_accurate)
RT_accurate$SentenceType<-as.factor(RT_accurate$SentenceType)
RT_accurate$ParticipantCode<-as.factor(RT_accurate$ParticipantCode)
RT_accurate$Participant_ID<-as.factor(RT_accurate$Participant_ID)

#Step2 - add x-column for plotting
RT_cleaned$x[RT_cleaned$SentenceType=="ORC"] <-"4"
RT_cleaned$x[RT_cleaned$SentenceType=="SRC"] <-"3"
RT_cleaned$x[RT_cleaned$SentenceType=="Passive"] <-"2"
RT_cleaned$x[RT_cleaned$SentenceType=="Active"] <-"1"
RT_cleaned$x<-as.numeric(RT_cleaned$x)

RT_cleaned$x[RT_cleaned$SentenceType=="ORC"] <-"4"
RT_cleaned$x[RT_cleaned$SentenceType=="SRC"] <-"3"
RT_cleaned$x[RT_cleaned$SentenceType=="Passive"] <-"2"
RT_cleaned$x[RT_cleaned$SentenceType=="Active"] <-"1"
RT_cleaned$x<-as.numeric(RT_cleaned$x)


#Step3 - Create two tables of Participants Reading and Response means
read_means_p <- RT_accurate%>%
  group_by(ParticipantCode, SentenceType)%>% 
  summarize(N=length(ReadingTime_ms),Mean=mean(ReadingTime_ms),SD = sd(ReadingTime_ms))
read_means_p$se = read_means_p$SD/sqrt(read_means_p$N)

read_means_p$x[read_means_p$SentenceType=="ORC"] <-"4"
read_means_p$x[read_means_p$SentenceType=="SRC"] <-"3"
read_means_p$x[read_means_p$SentenceType=="Passive"] <-"2"
read_means_p$x[read_means_p$SentenceType=="Active"] <-"1"
read_means_p$x<-as.numeric(read_means_p$x)

read_means <- RT_accurate%>%
  group_by(SentenceType)%>% 
  summarize(N=length(ReadingTime_ms),Mean=mean(ReadingTime_ms),SD = sd(ReadingTime_ms))
read_means$se = read_means$SD/sqrt(read_means$N)
read_means$x[read_means$SentenceType=="ORC"] <-"4"
read_means$x[read_means$SentenceType=="SRC"] <-"3"
read_means$x[read_means$SentenceType=="Passive"] <-"2"
read_means$x[read_means$SentenceType=="Active"] <-"1"
read_means$x<-as.numeric(read_means$x)

answer_means_p <- RT_cleaned%>%
  group_by(ParticipantCode, Participant_ID, SentenceType)%>% 
  summarise(N=length(Accuracy),Mean=mean(Accuracy),SD = sd(Accuracy))
answer_means_p$se = answer_means_p$SD/sqrt(answer_means_p$N)

answer_means_p$x[answer_means_p$SentenceType=="ORC"] <-"4"
answer_means_p$x[answer_means_p$SentenceType=="SRC"] <-"3"
answer_means_p$x[answer_means_p$SentenceType=="Passive"] <-"2"
answer_means_p$x[answer_means_p$SentenceType=="Active"] <-"1"
answer_means_p$x<-as.numeric(answer_means_p$x)

answer_means <- RT_cleaned%>%
  group_by(SentenceType)%>% 
  summarize(N=length(Accuracy),Mean=mean(Accuracy),SD = sd(Accuracy))
answer_means$se = answer_means$SD/sqrt(answer_means$N)
answer_means$x[answer_means$SentenceType=="ORC"] <-"4"
answer_means$x[answer_means$SentenceType=="SRC"] <-"3"
answer_means$x[answer_means$SentenceType=="Passive"] <-"2"
answer_means$x[answer_means$SentenceType=="Active"] <-"1"
answer_means$x<-as.numeric(answer_means$x)

# write.csv(answer_means_p, "Accuracy_proportions_211.csv")

#Step4 - Visualize the 3 measures: reading and response times and accuracy

#add some jitter to avoid the datapoint overlap
set.seed (321)
RT_accurate$xj<-jitter(RT_accurate$x, amount = .09)
read_means_p$xj<-jitter(read_means_p$x, amount = .09)
read_means$xj<-jitter(read_means$x, amount = .09)

answer_means_p$xj<-jitter(answer_means_p$x, amount = .09)
answer_means$xj<-jitter(answer_means$x, amount = .09)

##Plot rt by condition

F1_response<-ggplot(read_means_p, aes(y=Mean))+
  #Add geom_() objects
  geom_point(data = read_means_p %>% filter(x=="1"), aes(x=xj), color = 'dodgerblue', size = 1.5, alpha = .4)+
  geom_point(data = read_means_p %>% filter(x=="2"), aes(x=xj), color = 'darkgreen', size = 1.5, alpha = .4)+
  geom_point(data = read_means_p %>% filter(x=="3"), aes(x=xj), color = 'darkorange', size = 1.5, alpha = .4)+
  geom_point(data = read_means_p %>% filter(x=="4"), aes(x=xj), color = 'red', size = 1.5, alpha = .4)+
  
  geom_line(aes(x=xj, group=ParticipantCode), color = 'lightgrey', alpha=.4)+
  
  geom_half_boxplot(data = read_means_p %>% filter(x=="1"), aes(x=x), position = position_nudge(x=-.1),side = "r", outlier.shape = NA, center=TRUE, errorbar.draw = TRUE, width = .5, fill = 'dodgerblue', alpha = .1)+
  geom_half_boxplot(data = read_means_p %>% filter(x=="2"), aes(x=x), position = position_nudge(x=-.1),side = "r", outlier.shape = NA, center=TRUE, errorbar.draw = TRUE, width = .5, fill = 'darkgreen', alpha = .1)+
  geom_half_boxplot(data = read_means_p %>% filter(x=="3"), aes(x=x), position = position_nudge(x=-.1),side = "r", outlier.shape = NA, center=TRUE, errorbar.draw = TRUE, width = .5, fill = 'darkorange', alpha = .1)+
  geom_half_boxplot(data = read_means_p %>% filter(x=="4"), aes(x=x), position = position_nudge(x=-.1),side = "r", outlier.shape = NA, center=TRUE, errorbar.draw = TRUE, width = .5, fill = 'red', alpha = .1)+
  
  geom_half_violin(data = read_means_p %>% filter(x=="4"), aes(x=x), position = position_nudge(x=-0.2), side = "l", fill='red', color='red', alpha=.3, trim=TRUE )+
  geom_half_violin(data = read_means_p %>% filter(x=="3"), aes(x=x), position = position_nudge(x=-0.2), side = "l", fill='darkorange', color='darkorange', alpha=.3, trim=TRUE )+
  geom_half_violin(data = read_means_p %>% filter(x=="2"), aes(x=x), position = position_nudge(x=-0.2), side = "l", fill='darkgreen', color='darkgreen', alpha=.3, trim=TRUE )+
  geom_half_violin(data = read_means_p %>% filter(x=="1"), aes(x=x), position = position_nudge(x=-0.2), side = "l", fill='dodgerblue', color='dodgerblue', alpha=.3, trim=TRUE )+
  
  geom_point(data = read_means %>% filter(x=="1"), aes(x = x, y=Mean), position = position_nudge(0.03), color = 'black', size = 2)+
  geom_point(data = read_means %>% filter(x=="2"), aes(x = x, y=Mean), position = position_nudge(0.03), color = 'black', size = 2)+
  geom_point(data = read_means %>% filter(x=="3"), aes(x = x, y=Mean), position = position_nudge(0.03), color = 'black', size = 2)+
  geom_point(data = read_means %>% filter(x=="4"), aes(x = x, y=Mean), position = position_nudge(0.03), color = 'black', size = 2)+
  geom_errorbar(data =read_means %>% filter(x=="1"), aes(x=x, y=Mean, ymin = Mean-se, ymax = Mean+se), position = position_nudge(0.03), color = "black", width = .05, size = .5)+
  geom_errorbar(data =read_means %>% filter(x=="2"), aes(x=x, y=Mean, ymin = Mean-se, ymax = Mean+se), position = position_nudge(0.03), color = "black", width = .05, size = .5)+
  geom_errorbar(data =read_means %>% filter(x=="3"), aes(x=x, y=Mean, ymin = Mean-se, ymax = Mean+se), position = position_nudge(0.03), color = "black", width = .05, size = .5)+
  geom_errorbar(data =read_means %>% filter(x=="4"), aes(x=x, y=Mean, ymin = Mean-se, ymax = Mean+se), position = position_nudge(0.03), color = "black", width = .05, size = .5)+
  
  #Define additional settings
  scale_x_continuous(breaks=c(1,2,3,4), labels=c("Active", "Passive", "SRC", "ORC"), limits=c(0, 5))+
  xlab("Condition")+
  ylab ("RT(ms)")+
  ggtitle("Figure 1: Response time means by Participant and Condition in ms")+
  theme_classic()+
  theme(plot.title = element_blank(),
        axis.title.x = element_text(face = "bold", color = "black", size=24),
        axis.text.x = element_text(face = "bold", color = "black", size=22),
        axis.text.y = element_text(face="bold", color = "black", size=22),
        axis.title.y = element_text(face="bold",color = "black", size=24),
        legend.title = element_text(color = "black", face="bold", size=16),
        legend.text = element_text(color = "black", size=16))
F1_response  
ggsave(path="/Users/astoops/Documents/Self_Paced_Reading/Plots", filename = "F1 Response time means by Participant and Conditions.png", plot=F1_response, width=w, height = h)

F1_accuracy<-ggplot(answer_means_p, aes(y=Mean))+
  #Add geom_() objects
  geom_point(data = answer_means_p %>% filter(x=="1"), aes(x=xj), color = 'dodgerblue', size = 1.5, alpha = .4)+
  geom_point(data = answer_means_p %>% filter(x=="2"), aes(x=xj), color = 'darkgreen', size = 1.5, alpha = .4)+
  geom_point(data = answer_means_p %>% filter(x=="3"), aes(x=xj), color = 'darkorange', size = 1.5, alpha = .4)+
  geom_point(data = answer_means_p %>% filter(x=="4"), aes(x=xj), color = 'red', size = 1.5, alpha = .4)+
  
  geom_line(aes(x=xj, group=ParticipantCode), color = 'lightgrey', alpha=.4)+
  
  geom_half_boxplot(data = answer_means_p %>% filter(x=="1"), aes(x=x, y=Mean), position = position_nudge(x=-.1),side = "r", outlier.shape = NA, center=TRUE, errorbar.draw = TRUE, width = .5, fill = 'dodgerblue', alpha = .1)+
  geom_half_boxplot(data = answer_means_p %>% filter(x=="2"), aes(x=x, y=Mean), position = position_nudge(x=-.1),side = "r", outlier.shape = NA, center=TRUE, errorbar.draw = TRUE, width = .5, fill = 'darkgreen', alpha = .1)+
  geom_half_boxplot(data = answer_means_p %>% filter(x=="3"), aes(x=x, y=Mean), position = position_nudge(x=-.1),side = "r", outlier.shape = NA, center=TRUE, errorbar.draw = TRUE, width = .5, fill = 'darkorange', alpha = .1)+
  geom_half_boxplot(data = answer_means_p %>% filter(x=="4"), aes(x=x, y=Mean), position = position_nudge(x=-.1),side = "r", outlier.shape = NA, center=TRUE, errorbar.draw = TRUE, width = .5, fill = 'red', alpha = .1)+
  
  geom_half_violin(data = answer_means_p %>% filter(x=="4"), aes(x=x, y=Mean), position = position_nudge(x=-0.2), side = "l", fill='red', color='red', alpha=.3, trim=TRUE )+
  geom_half_violin(data = answer_means_p %>% filter(x=="3"), aes(x=x, y=Mean), position = position_nudge(x=-0.2), side = "l", fill='darkorange', color='darkorange', alpha=.3, trim=TRUE )+
  geom_half_violin(data = answer_means_p %>% filter(x=="2"), aes(x=x, y=Mean), position = position_nudge(x=-0.2), side = "l", fill='darkgreen', color='darkgreen', alpha=.3, trim=TRUE )+
  geom_half_violin(data = answer_means_p %>% filter(x=="1"), aes(x=x, y=Mean), position = position_nudge(x=-0.2), side = "l", fill='dodgerblue', color='dodgerblue', alpha=.3, trim=TRUE )+
  
  geom_point(data = answer_means %>% filter(x=="1"), aes(x = x, y=Mean), position = position_nudge(0.03), color = 'black', size = 2)+
  geom_point(data = answer_means %>% filter(x=="2"), aes(x = x, y=Mean), position = position_nudge(0.03), color = 'black', size = 2)+
  geom_point(data = answer_means %>% filter(x=="3"), aes(x = x, y=Mean), position = position_nudge(0.03), color = 'black', size = 2)+
  geom_point(data = answer_means %>% filter(x=="4"), aes(x = x, y=Mean), position = position_nudge(0.03), color = 'black', size = 2)+
  geom_errorbar(data =answer_means %>% filter(x=="1"), aes(x=x, y=Mean, ymin = Mean-se, ymax = Mean+se), position = position_nudge(0.03), color = "black", width = .05, size = .5)+
  geom_errorbar(data =answer_means %>% filter(x=="2"), aes(x=x, y=Mean, ymin = Mean-se, ymax = Mean+se), position = position_nudge(0.03), color = "black", width = .05, size = .5)+
  geom_errorbar(data =answer_means %>% filter(x=="3"), aes(x=x, y=Mean, ymin = Mean-se, ymax = Mean+se), position = position_nudge(0.03), color = "black", width = .05, size = .5)+
  geom_errorbar(data =answer_means %>% filter(x=="4"), aes(x=x, y=Mean, ymin = Mean-se, ymax = Mean+se), position = position_nudge(0.03), color = "black", width = .05, size = .5)+
  
  #Define additional settings
  scale_x_continuous(breaks=c(1,2,3,4), labels=c("Active", "Passive", "SRC", "ORC"), limits=c(0, 5))+
  xlab("Condition")+
  ylab ("Accuracy Rates")+
  ggtitle("Figure 2: Accuracy by Participant and Condition in ms")+
  theme_classic()+
  theme(plot.title = element_blank(),
        axis.title.x = element_text(face = "bold", color = "black", size=24),
        axis.text.x = element_text(face = "bold", color = "black", size=22),
        axis.text.y = element_text(face="bold", color = "black", size=22),
        axis.title.y = element_text(face="bold",color = "black", size=24),
        legend.title = element_text(color = "black", face="bold", size=16),
        legend.text = element_text(color = "black", size=16))
F1_accuracy  
ggsave(path="/Users/astoops/Documents/Self_Paced_Reading/Plots", filename = "F1 Accuracy by Participant and Condition with Density Plots.png", plot=F1_accuracy, width=w, height = h)





