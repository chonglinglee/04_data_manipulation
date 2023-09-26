#Created by Anastasia Stoops 10-26-2020
library(tidyverse)
library(ggplot2)

#width and height variables for saved plots
w = 10
h = 5
cols<-c("Active"="dodgerblue", "Passive"="darkgreen", "SRC"="darkorange", "ORC"="red")

setwd("/Users/astoops/Documents/Self_Paced_Reading/Raw Datasets/")
accuracy<-read.csv('Cleaned_211_All_accuracy.csv')
data<-read.csv('Cleaned_211_Correct.csv')
demo<-read.csv('Demographics_RE_Vocabulary_ART_Accuracy_211.csv')
mean_p<-read.csv('Mean RT Accuracy ART and RE by Sentence Type 211.csv')


demo$ART_mc<-scale(demo$ART_score_value, scale=FALSE)
demo$RE_mc<-scale(demo$RE_Score, scale=FALSE)

demo$ART_z<-scale(demo$ART_score_value)
demo$RE_z<-scale(demo$RE_Score)

mean_ART<- demo%>% 
  # group_by(SentenceType)%>% 
  summarize(N=length(ART_score_value),Mean=mean(ART_score_value),SD = sd(ART_score_value))
mean_ART$se = mean_ART$SD/sqrt(mean_ART$N)

mean_ART

mean_RE<- demo%>% 
  # group_by(SentenceType)%>% 
  summarize(N=length(RE_Score),Mean=mean(RE_Score),SD = sd(RE_Score))
mean_RE$se = mean_RE$SD/sqrt(mean_RE$N)

mean_RE


# write_csv(demo, 'Demographics_RE_Vocabulary_ART_Accuracy_221.csv')

data$Easy_Hard<-as.numeric(with(data, ifelse(SentenceType=="Active" |SentenceType=="Passive", "-1", "1")))
data$Easy<-as.numeric(with(data, ifelse(SentenceType=="Active", "-1", 
                                        ifelse(SentenceType=="Passive", "1", "0"))))
data$Hard<-as.numeric(with(data, ifelse(SentenceType=="SRC", "-1", 
                                        ifelse(SentenceType=="ORC", "1", "0"))))

data$LinearTrend<-as.numeric(with(data, ifelse(SentenceType=="Active", "-3", 
                                               ifelse(SentenceType=="Passive", "-1", 
                                                      ifelse(SentenceType=="SRC", "1", "3")))))


accuracy$Easy_Hard<-as.numeric(with(accuracy, ifelse(SentenceType=="Active" |SentenceType=="Passive", "-1", "1")))
accuracy$Easy<-as.numeric(with(accuracy, ifelse(SentenceType=="Active", "-1", 
                                                ifelse(SentenceType=="Passive", "1", "0"))))
accuracy$Hard<-as.numeric(with(accuracy, ifelse(SentenceType=="SRC", "-1", 
                                                ifelse(SentenceType=="ORC", "1", "0"))))

accuracy$LinearTrend<-as.numeric(with(accuracy, ifelse(SentenceType=="Active", "-3", 
                                                       ifelse(SentenceType=="Passive", "-1", 
                                                              ifelse(SentenceType=="SRC", "1", "3")))))

# data$SentenceType<-as.factor(data$SentenceType, levels = c("Active", "Passive", "SRC", "ORC"))
# accuracy$SentenceType<-as.factor(accuracy$SentenceType, levels = c("Active", "Passive", "SRC", "ORC"))
# mean_p$SentenceType<-as.factor(mean_p$SentenceType, levels = c("Active", "Passive", "SRC", "ORC"))




F3a<-ggplot(data = mean_p, aes(ART_score_value, Mean_Accuracy, colour=SentenceType) )+
  geom_point(alpha = 0.3, size = 4, position = "jitter")+
  scale_colour_manual(values=cols, name="Sentence Types")+
  # coord_cartesian(xlim=c(0,15), ylim=c(0, 1))+
  geom_smooth(method = "lm", se=FALSE)+
  # facet_grid(SentenceType ~.)+
  xlab("ART")+
  ylab ("Accuracy")+
  theme_classic()+
  theme(plot.title = element_blank(),
        axis.title.x = element_text(face="bold",color = "black", size=24),
        axis.text.x = element_text(face = "bold", color = "black", size=22),
        axis.text.y = element_text(face="bold", color = "black", size=22),
        axis.title.y = element_text(face="bold",color = "black", size=24),
        legend.position="none")
F3a
ggsave(path="/Users/astoops/Documents/Self_Paced_Reading/Plots", filename = "F3A_Accuracy by ART and Sentence Type.png", plot=F3a, width=w, height = h)

F3b<-ggplot(data = mean_p, aes(ART_score_value, Mean_RT, colour=SentenceType) )+
  geom_point(aes(colour=SentenceType), alpha = 0.3, size = 4, position = "jitter")+
  scale_colour_manual(values=cols, aesthetics = c("colour", "fill"), name="Sentence Types")+
  # coord_cartesian(xlim=c(0,15), ylim=c(0, 1))+
  geom_smooth(method = "lm", se=FALSE)+
  # facet_grid(SentenceType ~.)+
  xlab("ART")+
  ylab ("Reading times (ms)")+
  theme_classic()+
  theme(plot.title = element_blank(),
        axis.title.x = element_text(face="bold",color = "black", size=24),
        axis.text.x = element_text(face = "bold", color = "black", size=22),
        axis.text.y = element_text(face="bold", color = "black", size=22),
        axis.title.y = element_text(face="bold",color = "black", size=24),
        legend.position="none")
F3b
ggsave(path="/Users/astoops/Documents/Self_Paced_Reading/Plots", filename = "F3B_RT by ART and Sentence Type.png", plot=F3b, width=w, height = h)

F3c<-ggplot(data = mean_p, aes(RE_Score, Mean_Accuracy, colour=SentenceType) )+
  geom_point(aes(colour=SentenceType), alpha = 0.3, size = 4, position = "jitter")+
  scale_colour_manual(values=cols, aesthetics = c("colour", "fill"), name="Sentence Types")+
  # coord_cartesian(xlim=c(0,15), ylim=c(0, 1))+
  geom_smooth(method = "lm", se=FALSE)+
  # facet_grid(SentenceType ~.)+
  xlab("Reading Enjoyment")+
  ylab ("Accuracy")+
  theme_classic()+
  theme(plot.title = element_blank(),
        axis.title.x =element_text(face="bold",color = "black", size=24),
        axis.text.x = element_text(face = "bold", color = "black", size=22),
        axis.text.y = element_text(face="bold", color = "black", size=22),
        axis.title.y = element_text(face="bold",color = "black", size=24),
        legend.position="none")
F3c
ggsave(path="/Users/astoops/Documents/Self_Paced_Reading/Plots", filename = "F3C_Accuracy by RE and Sentence Type.png", plot=F3c, width=w, height = h)

F3d<-ggplot(data = mean_p, aes(RE_Score, Mean_RT, colour=SentenceType) )+
  geom_point(aes(colour=SentenceType), alpha = 0.3, size = 4, position = "jitter")+
  scale_colour_manual(values=cols, aesthetics = c("colour", "fill"), name="Sentence Types")+
  # coord_cartesian(xlim=c(0,15), ylim=c(0, 1))+
  geom_smooth(method = "lm", se=FALSE)+
  # facet_grid(SentenceType ~.)+
  xlab("Reading Enjoyment")+
  ylab ("Reading times (ms)")+
  theme_classic()+
  theme(plot.title = element_blank(),
        axis.title.x = element_text(face="bold",color = "black", size=24),
        axis.text.x = element_text(face = "bold", color = "black", size=22),
        axis.text.y = element_text(face="bold", color = "black", size=22),
        axis.title.y = element_text(face="bold",color = "black", size=24),
        legend.position="none")
F3d
ggsave(path="/Users/astoops/Documents/Self_Paced_Reading/Plots", filename = "F3D_RT by RE and Sentence Type.png", plot=F3d, width=w, height = h)
