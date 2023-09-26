library(tidyverse)
library(lme4)
library(coda)


accuracy<-read.csv('Cleaned_211_All_accuracy.csv')
data<-read.csv('Cleaned_211_Correct.csv')


data$ART_z<-as.numeric(scale(data$ART_score_value))
data$RE_z<-as.numeric(scale(data$RE_Score))

accuracy$ART_z<-as.numeric(scale(accuracy$ART_score_value))
accuracy$RE_z<-as.numeric(scale(accuracy$RE_Score))

#code the comparison contrasts by assigning dummy coding
data$Easy_Hard<-as.numeric(with(data, ifelse(SentenceType=="Active" |SentenceType=="Passive", "-1", "1")))
data$Easy<-as.numeric(with(data, ifelse(SentenceType=="Active", "-1", 
                                       ifelse(SentenceType=="Passive", "1", "0"))))
data$Hard<-as.numeric(with(data, ifelse(SentenceType=="SRC", "-1", 
                                        ifelse(SentenceType=="ORC", "1", "0"))))

data$LinearTrend<-as.numeric(with(data, ifelse(SentenceType=="Active", "-3", 
                                              ifelse(SentenceType=="Passive", "-1", 
                                                     ifelse(SentenceType=="SRC", "1", "3")))))


accuracy$Easy_Hard<-as.numeric(with(accuracy, ifelse(SentenceType=="Active" |SentenceType=="Passive", "-1", "1")))
accuracy$Easy<-as.numeric(with(accuracy, ifelse(SentenceType=="Acive", "-1", 
                                        ifelse(SentenceType=="Passive", "1", "0"))))
accuracy$Hard<-as.numeric(with(accuracy, ifelse(SentenceType=="SRC", "-1", 
                                        ifelse(SentenceType=="ORC", "1", "0"))))

accuracy$LinearTrend<-as.numeric(with(accuracy, ifelse(SentenceType=="Active", "-3", 
                                               ifelse(SentenceType=="Passive", "-1", 
                                                      ifelse(SentenceType=="SRC", "1", "3")))))




#code exploratory treatment contrast with Active sentences set as a baseline

data$Condition<-as.factor(with(data, ifelse(SentenceType=="Active", "1", 
                                            ifelse(SentenceType=="Passive", "2", 
                                                   ifelse(SentenceType=="SRC", "3", "4")))))

accuracy$Condition<-as.factor(with(accuracy, ifelse(SentenceType=="Active", "1", 
                                            ifelse(SentenceType=="Passive", "2", 
                                                   ifelse(SentenceType=="SRC", "3", "4")))))


data$SentenceType<-as.factor(data$SentenceType)
accuracy$SentenceType<-as.factor(accuracy$SentenceType)




#Response time raw and log transformed vs ART and RE in separate models

ART_Orthogonal = glmer(ReadingTime_ms ~Easy_Hard*ART_z+ Easy*ART_z+ Hard*ART_z+(1|ItemType) + (1|ParticipantCode), data = data)
summary(ART_Orthogonal)
ART_Orthogonal_Log = lmer(Reading_Time_Log ~SES_factor+ Easy_Hard*ART_z+ Easy*ART_z+ Hard*ART_z+(1|ItemType) + (1|ParticipantCode), data = data)
summary(ART_Orthogonal_Log)


RE_Orthogonal = lmer(ReadingTime_ms ~Easy_Hard*RE_z+ Easy*RE_z+ Hard*RE_z+(1|ItemType) + (1|ParticipantCode), data = data)
summary(RE_Orthogonal)
RE_Orthogonal_Log = lmer(Reading_Time_Log ~Easy_Hard*RE_z+ Easy*RE_z+ Hard*RE_z+(1|ItemType) + (1|ParticipantCode), data = data)
summary(RE_Orthogonal_Log)


#both ART and RE in one model
ART_RE_Orthogonal_Three_Way = lmer(ReadingTime_ms ~Easy_Hard*ART_z+ Easy*ART_z+ Hard*ART_z+Easy_Hard*RE_z+ Easy*RE_z+ Hard*RE_z+(1|ItemType) + (1|ParticipantCode), data = data)
summary(ART_RE_Orthogonal_Three_Way)


ART_RE_Orthogonal_Three_Way_Log = lmer(Reading_Time_Log ~Easy_Hard*ART_z+ Easy*ART_z+ Hard*ART_z+Easy_Hard*RE_z+ Easy*RE_z+ Hard*RE_z+(1|ItemType) + (1|ParticipantCode), data = data)
summary(ART_RE_Orthogonal_Three_Way_Log)



#exploratory  analyses - treatment contrast
ART_Treatment_raw = lmer(ReadingTime_ms ~Condition*ART_z*SES_factor+(1|ItemType) + (1|ParticipantCode), data = data)
summary(ART_Treatment_raw)
ART_Treatment_Log= lmer(Reading_Time_Log ~Condition*ART_z*SES_factor+(1|ItemType) + (1|ParticipantCode), data = data)
summary(ART_Treatment_Log)

RE_Treatment_raw = lmer(ReadingTime_ms ~Condition*RE_z*SES_factor+(1|ItemType) + (1|ParticipantCode), data = data)
summary(RE_Treatment_raw)
RE_Treatment_Log= lmer(Reading_Time_Log ~Condition*RE_z*SES_factor+(1|ItemType) + (1|ParticipantCode), data =data)
summary(RE_Treatment_Log)

ART_RE_Treatment_Three_Way = lmer(ReadingTime_ms ~Condition*ART_z+Condition*RE_z+(1|ItemType) + (1|ParticipantCode), data = data)
summary(ART_RE_Treatment_Three_Way)
confint(ART_RE_Treatment_Three_Way)
ART_RE_Treatment_Three_Way_Log = lmer(Reading_Time_Log ~Condition*ART_z+Condition*RE_z+(1|ItemType) + (1|ParticipantCode), data = data)
summary(ART_RE_Treatment_Three_Way_Log )
confint(ART_RE_Treatment_Three_Way_Log )

anova(ART_Treatment_raw, ART_RR_Treatment_Three_Way)

#Accuracy - 

ART_Orthogonal = glmer(Accuracy ~Easy_Hard*ART_z+ Easy*ART_z+ Hard*ART_z+(1|ItemType) + (1|ParticipantCode), data = accuracy, family =binomial)
summary(ART_Orthogonal)
confint(ART_Orthogonal)

RE_Orthogonal = glmer(Accuracy ~Easy_Hard*RE_z+ Easy*RE_z+ Hard*RE_z+(1|ItemType) + (1|ParticipantCode), data = accuracy, family =binomial)
summary(RE_Orthogonal)
confint(RE_Orthogonal)

ART_Treatment_Accuracy = glmer(Accuracy ~Condition*ART_z*SES_factor+(1|ItemType)+ (1|ParticipantCode), data = accuracy, family =binomial)
summary(ART_Treatment_Accuracy)

RE_Treatment_Accuracy = glmer(Accuracy ~Condition*RE_z*SES_factor +(1|ItemType)+ (1|ParticipantCode), data = accuracy, family =binomial)
summary(RE_Treatment_Accuracy)


ART_RE_Orthogonal_Three_Way = glmer(Accuracy ~Easy_Hard*ART_z + Easy*ART_z + Hard*ART_z+Easy_Hard*RE_z + Easy*RE_z + Hard*RE_z+(1|ItemType), data = accuracy, family =binomial)
summary(ART_RE_Orthogonal_Three_Way)
confint(ART_RE_Orthogonal_Three_Way)

ART_RE_Treatment_Three_Way = glmer(Accuracy ~Condition*ART_z + Condition*RE_z +(1|ItemType), data = accuracy, family =binomial)
summary(ART_RE_Treatment_Three_Way)
confint(ART_RE_Orthogonal_Three_Way)

anova(ART_Treatment_Accuracy, ART_RE_Treatment_Three_Way)

ART_RE_Orthogonal_Three_Way_lm = lm(Accuracy ~Easy_Hard*ART_z*RE_z + Easy*ART_z*RE_z + Hard*ART_z*RE_z, data = accuracy)
summary(ART_RE_Orthogonal_Three_Way_lm)
confint(ART_RE_Orthogonal_Three_Way_lm)

ART_RE_Treatment_Three_Way_lm = lm(Accuracy ~Condition*ART_z+Condition*RE_z, data = accuracy)
summary(ART_RE_Treatment_Three_Way_lm)

RE_Treatment_lm = lm(Accuracy ~Condition*RE_z, data = accuracy)
summary(RE_Treatment_lm)



