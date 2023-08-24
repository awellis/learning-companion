#Clear existing data and graphics
rm(list=ls())
graphics.off()
#Load Hmisc library
library(Hmisc)
library(lavaan)
library(psych)
library(moments)
library(semTools)
library(e1071)
library(corrplot)
library(readr)
library(semPlot)
library(equaltestMI)
library(dplyr)
#Read Data
setwd("/Users/fabian/Library/Mobile Documents/com~apple~CloudDocs/New Project/Questionnaire")
load("Prolific_study 2.RData")
data=read.csv('LC_Val2.csv')
#Setting Labels

label(data$record_id)="Record ID"
label(data$redcap_survey_identifier)="Survey Identifier"
label(data$val_lc_timestamp)="Survey Timestamp"
label(data$consent_form)="Please indicate if you understood and consent to the above information."
label(data$prolific_pid)="Please enter your Prolific ID? Please note that this response should auto-fill with the correct ID"
label(data$plan_3)="When we have an assignment or project, I explicitly break it into smaller tasks."
label(data$plan_1)="I set specific learning goals before I begin a task."
label(data$con_2)="During class time, I engage actively with what is being said."
label(data$eval_4)="Once I finish a task, I ask myself if I learned as much as I could have."
label(data$con_4_rec)="During the lecture, I get distracted easily by external events."
label(data$reg_5)="I reevaluate my assumptions when I get confused."
label(data$mon_1)="When studying, I try to determine which concepts I dont understand well."
label(data$plan_2)="Before studying, I try to think through a topic and decide what I am supposed to learn from it."
label(data$reg_1)="Rigorous reading techniques involve thorough and systematic engagement with a text (e.g., previewing the text [headings, key terms], setting reading goals, taking notes, summarizing). If a text is difficult to understand, I apply more rigorous reading techniques."
label(data$mon_2)="When studying, I periodically check if Im achieving my learning goals."
label(data$con_1_rec)="During class time I often miss important points because Im thinking of other things."
label(data$atcheck_1)="For this statement, please click on the response value three. This is an attention check."
label(data$plan_4)="I read instructions carefully before I begin a task."
label(data$mon_3)="I find myself analyzing the usefulness of strategies while I study."
label(data$eval_1)="I ask myself questions to make sure I understand the material I have been studying."
label(data$plan_6)="During study preparation, I think of possible difficulties that may arise while studying."
label(data$mon_4)="I find myself pausing regularly to check my comprehension."
label(data$eval_3)="After I finish a task, I ask myself if there was an easier way to do things."
label(data$reg_2)="I try to change the way I study in order to fit the course requirements set by the teacher."
label(data$con_3_rec)="During class time, I frequently find that my mind was wandering off."
label(data$reg_3)="When I get confused taking notes in class, I make sure to sort it out afterward."
label(data$plan_5)="Before I start studying, I rank the importance of learning goals."
label(data$eval_2)="I ask myself if I achieved my learning goals after studying."
label(data$reg_4)="When I get distracted, I try to refocus on the lecture quickly."
label(data$elab_2)="I try to see how the exercises are related to real-life applications."
label(data$pro_4_rec)="It is usually hard for me to understand the logic behind mathematical proofs."
label(data$mem_2)="I memorize keywords to remind me of important concepts."
label(data$org_3)="I make diagrams, schemes, or tables to help me organize the content of the lectures."
label(data$mem_4)="I try to remember all the important rules so well that I dont forget them anymore."
label(data$pro_1)="I try to follow the mathematical proofs step by step."
label(data$test_3_rec)="Right before I test myself, I prefer to reread my notes."
label(data$org_2)="When studying, I outline the material to help me organize my thoughts."
label(data$elab_3)="When possible, I try to link the ideas of different classes with each other."
label(data$elab_5)="I make up examples to understand mathematical statements."
label(data$space_3_rec)="I usually stay up and work very late when we have an assignment due the next day."
label(data$org_5)="I try to simplify difficult content."
label(data$mem_3)="I make lists of important terms and memorize the lists."
label(data$pro_3)="I make sure to understand the mathematical proofs."
label(data$atcheck_2)="For this statement, please choose the response with value six. This is an attention check."
label(data$space_5)="When we have an exam, I manage to study all the important concepts sufficiently."
label(data$mem_1)="When studying, I practice saying the material to myself over and over."
label(data$test_6)="After testing myself, I control my answers with the solutions."
label(data$org_6)="I try to break down complicated content into easier bits of information."
label(data$pro_2)="I try to understand mathematical proofs to be able to use them with other tasks."
label(data$org_1)="When studying, I write short summaries of the important concepts."
label(data$pro_5)="Before I go through a mathematical proof, I make sure to understand the underlying assumptions."
label(data$elab_1)="When I study, I gather information from different sources, such as my lecture notes, the recommended documents, and discussions."
label(data$test_4)="When testing myself, I try really hard to retrieve stuff before I eventually look it up."
label(data$space_2)="I periodically review previously learned concepts throughout the semester."
label(data$org_4)="When studying, I review my lecture notes and make a list of important concepts."
label(data$test_1)="When I do exercises, I usually try to remember as much of the course content as possible before I look at my notes."
label(data$test_5)="I test myself on all the important topics of a lecture."
label(data$elab_4)="I try to understand how new content is related to what I previously learned."
label(data$space_1_rec)="I usually cram all information during the last week before an exam."
label(data$test_2)="I test myself to see if I can remember key ideas from the lessons."
label(data$space_4)="I start studying early for an exam so I wont get stressed in the end."
label(data$time_2)="I make sure I keep up with the weekly readings and assignments."
label(data$pers_3)="I work hard to do well in this class even if I dont like what we are doing."
label(data$peer_3)="When studying, I often set aside time to discuss the course material with a group of students from the class."
label(data$time_4_rec)="I find it hard to stick to a study schedule."
label(data$time_5)="I usually plan enough time for studying."
label(data$hatcheck_3)="Please select the same answer as in the statement right above. This is an attention check."
label(data$pers_1_rec)="I often feel so lazy or bored when studying that I quit before I finish what I planned to do."
label(data$peer_2)="I try to work with other students to complete the course assignments."
label(data$time_1)="I make good use of my study time."
label(data$peer_5)="I meet with other students to find a solution together."
label(data$pers_2)="Even when course materials are dull and uninteresting, I manage to keep working until I finish."
label(data$pers_4_rec)="When coursework is difficult, I give up or only study the easy parts."
label(data$peer_1)="When studying, I often try to explain the material to a classmate or friend."
label(data$time_3_rec)="I often find that I dont spend very much time studying because of other activities."
label(data$peer_4)="I seek help from other students when I cant solve a problem."
label(data$fb_planning)="Metacognitive planning:This measures how you approach a task. Do you simply start it without much forethought or do you think about what you know about this topic, what you could learn from it, or where you might face problems? Metacognitive planning activates your knowledge and prepares you to integrate new information into your existing knowledge, and thus new information might stick with you for much longer."
label(data$fb_concentration)="Focus and concentration: How easily do you get distracted or let yourself distract by external sources? To grasp new, especially complex information needs your full attention. Therefore, try to actively engage with the content of the lecture, e.g., by asking yourself questions about if this information is consistent with your previous knowledge, if it makes sense to you, or what other information you need to make sense of it."
label(data$fb_monitoring)="Monitoring: This measures whether you check your learning process while youre learning. Doing this helps determine if you already understand a topic or if you need more exercises."
label(data$fb_regulation)="Self-regulation: This measure indicates whether you adapt your behavior once you realize that you didnt keep track or got confused about something. For example, when reading, do you go back and reread a passage when you realize you didnt understand it? Or while taking notes, when you get confused, do you make sure afterward to clarify your confusion?"
label(data$fb_evaluation)="Evaluation: This measures if you revise your completed tasks and think about what you learned, if you achieved your learning goals, or what you could do better a next time with similar content. Thinking about your completed tasks helps you consolidate what you learned and also helps with future learning."
label(data$fb_elaboration)="Elaboration: Elaboration measures whether you link new information with existing knowledge and whether you link information from different sources."
label(data$fb_organization)="Organization: Organization indicates if you organize your knowledge by writing summaries, making outlines of the lecture content, or trying to visualize the content using diagrams or graphs, etc."
label(data$fb_proofs)="Understanding proofs in mathematics: Proofs are essential to mathematics (but in some studies they are also taken as a given and not inquired). When you understand the proofs, you understand the mechanisms and the essence of a concept. Thus, how much effort do you put into understanding them?"
label(data$fb_testing)="Testing yourself: Testing is often used as an assessment. But actually, testing yourself is one of the most effective - and seldomly used - learning strategies. You can test yourself by answering quizzes, previous exams, and end-of-chapter questions, or come up with your own questions during reading and try to answer your questions after reading (immediately after, as well as one or two days later again). We measured how purposefully you use self-testing as a learning strategy."
label(data$fb_spaced)="Spaced repetition: This measures if you usually cram for an exam (a low score) or if you learn distributedly over several weeks (a high score). Spaced repetition is also one of the most effective learning strategies for long-term retention."
label(data$fb_timeman)="Time management: Do you have a study schedule and stick to it? Is it easy for you to do your weekly readings/assignments? Effective learning needs consistent and continuous effort (combine this with spaced repetition and self-testing!). In meta-analyses, time management was one of the strongest predictors for high learning outcomes."
label(data$fb_persistence)="Persistence: This measures if you give up easily when facing hardships, challenges, or boredom (indicated by a low score) or if you manage to learn your stuff anyway (indicated by a high score)."
label(data$fb_peers)="Learning with peers: Learning with peers is good for accountability as well as for feedback and alternative solutions. How well do you use this resource?"
label(data$val_lc_complete)="Complete?"

colnames(data)[67] <- c("atcheck_3")

## Import demographic data from Prolific
demog <- read.csv("prolific_export_demographic.csv")
demog$Status <- as.factor(demog$Status)
summary(demog$Status)
data2 <- merge(data, demog, by.x = "prolific_pid", by.y = "Participant.id", all.x = T)
data2 <- data2[-c(4,136,259,372,379),] # they don't have complete data
summary(data2$Status)
data2 <- data2[data2$Status == "APPROVED" | is.na(data2$Status),]
# exclude those who failed at least one attention test
which(data2$atcheck_1.factor!= 3)
which(data2$atcheck_2.factor!= 6)
which(data2$atcheck_3 != data2$time_5)
data2 <- data2[-c(30,97,115,149,207,252,319,342,376),]
# remove last entry (record_id = 417), this participant only answered with 1 and 7...
data2 <- data2[-c(data2$record_id==417),]

str(data2)
data2 <- data2 %>% mutate_at(c(6:89), as.numeric)
# recode inverse items
data2$con_4 <- 8-data2$con_4_rec
data2$con_1 <- 8-data2$con_1_rec
data2$con_3 <- 8-data2$con_3_rec
data2$pro_4 <- 8-data2$pro_4_rec
data2$test_3 <- 8-data2$test_3_rec
data2$space_3 <- 8-data2$space_3_rec
data2$space_1 <- 8-data2$space_1_rec
data2$time_4 <- 8-data2$time_4_rec
data2$pers_1 <- 8-data2$pers_1_rec
data2$pers_4 <- 8-data2$pers_4_rec
data2$time_3 <- 8-data2$time_3_rec


#Metacognition
planning <- data2[,c(2,7,13,6,18,27,21)]
concentration <- data2[,c(2,186,8,187,185)]
regulation <- data2[,c(2,14,24,26,29,11)]
monitoring <- data2[,c(2,12,15,19,22)]
evaluation <- data2[,c(2,20,28,23,9)]

#Cognitive strategies
elaboration <- data2[,c(2,52,30,38,58,39)]
organization <- data2[,c(2,50,37,33,55,41,48)]
proofs <- data2[,c(2,35,49,43,188,51)]
memorize <- data2[,c(2,46,32,42,34)]
spacing <- data2[,c(2,191,54,190,61,45)]
selftest <- data2[,c(2,56,60,189,53,57,47)]

#Cognitive resources
time <- data2[,c(2,70,62,195,192,66)]
peers <- data2[,c(2,74,69,64,76,71)]
persistence <- data2[,c(2,193,72,63,194)]

m_con <- 'C =~ con_1 + con_2 + con_3 + con_4
con_1 ~~ con_2'
fitCon <- cfa(m_con, data = concentration2, estimator = "MLM")
summary(fitCon, fit.measures = T, standardized = T)
modindices(fitCon, sort = T, maximum.number = 15)
AVE(fitCon)
compRelSEM(fitCon, tau.eq = T) 
# this is a great factor! 64.7% explained variance, alpha 0.86
# without outliers: AVE 69.2%, alpha 0.884

m_mon <- 'M =~ mon_1 + mon_2 + mon_3 + mon_4
mon_2 ~~ mon_3'
fitMon <- cfa(m_mon, data = monitoring2, estimator = "MLM")
summary(fitMon, fit.measures = T, standardized = T)
modindices(fitMon, sort = T, maximum.number = 5)
AVE(fitMon)
compRelSEM(fitMon, tau.eq = T) 
# 37% AVE, alpha 0.68 # this was with model 'M =~ mon_1 + mon_2 + mon_3 + mon_4'
# without outliers now: AVE  33%, alpha 0.683

m_eva <- 'E =~ eval_1 + eval_2 + eval_3 + eval_4'
fitEva <- cfa(m_eva, data = evaluation2, estimator = "MLM")
summary(fitEva, fit.measures = T, standardized = T)
AVE(fitEva)
compRelSEM(fitEva, tau.eq = T) 
# 39.3% AVE, alpha = .70
# without outliers: AVE 42.1% alpha 0.722

m_pla <- 'P =~ plan_1 + plan_2 + plan_5 + plan_6'
fitPla <- cfa(m_pla, data = planning2, estimator = "MLM")
summary(fitPla, fit.measures = T, standardized = T)
modindices(fitPla, sort = T, maximum.number = 15)
AVE(fitPla)
compRelSEM(fitPla, tau.eq = T) 
# 39.8% AVE, alpha .71
# without outliers: AVE 43.8% alpha 0.734

m_reg <- 'R =~ reg_1 + reg_3 + reg_4 + reg_5
reg_1 ~~ reg_4'
fitReg <- cfa(m_reg, data = regulation2, estimator = "MLM")
summary(fitReg, fit.measures = T, standardized = T)
modindices(fitReg, sort = T, maximum.number = 15)
AVE(fitReg)
compRelSEM(fitReg, tau.eq = T) 
alpha(regulation[,2:6])
# AVE 26.8%, alpha .59. This is a bad factor
# without outliers: AVE 40.2%, alpha 0.62. Could try to combine it with monitoring, as they are both regulation processes during a task

MonReg2 <- merge(monitoring2, regulation2, by = "record_id")
m_MonReg <- 'F1 =~ mon_1 + mon_2 + mon_4 + reg_1 + reg_3 + reg_4
reg_1 + reg_3 ~~ reg_4'
fitMonReg <- cfa(m_MonReg, data = MonReg2, estimator = "MLM")
summary(fitMonReg, fit.measures = T, standardized = T)
modindices(fitMonReg, sort = T, maximum.number = 5)
AVE(fitMonReg)
compRelSEM(fitMonReg, tau.eq = T) 

data_wo_outliers <- merge(MonReg2, planning2, by ="record_id")
data_wo_outliers <- merge(data_wo_outliers, evaluation2, by ="record_id")
data_wo_outliers <- data_wo_outliers[,-c(6,7,13,14,21,22,27,28)]


m_meta3sim <- 'MR =~ mon_1 + mon_2 + mon_4 + reg_1 + reg_3 + reg_4
P =~ plan_1 + plan_2 + plan_5 + plan_6
E =~ eval_1 + eval_2 + eval_4
reg_3 ~~  reg_4
plan_5 ~~ eval_2'
fitMeta3sim <- cfa(m_meta3sim, data = data_wo_outliers, estimator = "MLM", std.lv = T)
summary(fitMeta3sim, fit.measures = T, standardized = T)
lavInspect(fitMeta3sim, "cov.lv")
pred.Meta3sim <- as.data.frame(lavPredict(fitMeta3sim))
cor(pred.Meta3sim)
modindices(fitMeta3sim, sort = T, maximum.number = 5)


data_wo_outliers <- merge(data_wo_outliers, persistence2, by ="record_id")
data_wo_outliers <- merge(data_wo_outliers, concentration2, by = "record_id")
data_wo_outliers <- data_wo_outliers[,-c(25,26,31,32)]

m_meta5sim <- 'MR =~ mon_1 + mon_2 + mon_4 + reg_1 + reg_3 + reg_4
PL =~ plan_1 + plan_2 + plan_5 + plan_6
EV =~ eval_1 + eval_2 + eval_4
CO =~ con_1 + con_2 + con_3 + con_4
PE =~ pers_1 + pers_2 + pers_3 + pers_4
pers_2 ~~ pers_3
con_1 ~~ con_2
reg_3 ~~  reg_4
plan_5 ~~ eval_2'
fitMeta5sim <- cfa(m_meta5sim, data = data_wo_outliers, estimator = "MLM", std.lv = T)
summary(fitMeta5sim, fit.measures = T, standardized = T)
lavInspect(fitMeta5sim, "cov.lv")
# Eval is collinear with MR and EV, remove EV from dataset
pred.Meta5sim <- as.data.frame(lavPredict(fitMeta5sim))
modindices(fitMeta3sim, sort = T, maximum.number = 5)

data_wo_outliers2 <- merge(data_wo_outliers2, persistence2, by ="record_id")
data_wo_outliers2 <- merge(data_wo_outliers2, concentration2, by = "record_id")
data_wo_outliers2 <- data_wo_outliers2[,-c(21,22,27,28)]
m_meta4sim <- 'MR =~ mon_1 + mon_2 + mon_4 + reg_1 + reg_3 + reg_4
PL =~ plan_1 + plan_2 + plan_5 + plan_6
CO =~ con_1 + con_2 + con_3 + con_4
PE =~ pers_1 + pers_2 + pers_3 + pers_4
pers_2 ~~ pers_3
con_1 ~~ con_2
reg_3 ~~  reg_4 + reg_1
mon_1 ~~  mon_2 + plan_5'
fitMeta4sim <- cfa(m_meta4sim, data = data_wo_outliers2, estimator = "MLM", std.lv = T)
summary(fitMeta4sim, fit.measures = T, standardized = T)
modindices(fitMeta4sim, sort = T, maximum.number = 5)
# maybe leave out reg_3?
m_meta4sim <- 'MR =~ mon_1 + mon_2 + mon_4 + reg_1 + reg_4
P =~ plan_1 + plan_2 + plan_5 + plan_6
CO =~ con_1 + con_2 + con_3 + con_4
PE =~ pers_1 + pers_2 + pers_3 + pers_4
pers_2 ~~ pers_3
con_1 ~~ con_2
mon_1 ~~  mon_2 + plan_5'
fitMeta4sim <- cfa(m_meta4sim, data = data_wo_outliers2, estimator = "MLM", std.lv = T)
summary(fitMeta4sim, fit.measures = T, standardized = T)
modindices(fitMeta4sim, sort = T, maximum.number = 10)

### cognitive strategies
m_ela <- 'E =~ elab_1 + elab_2 + elab_3 + elab_4 + elab_5'
m_ela <- 'E =~ elab_2 + elab_3 + elab_4 + elab_5'
fitEla <- cfa(m_ela, data = elaboration, estimator = "MLM")
summary(fitEla, fit.measures = T, standardized = T)
AVE(fitEla)
compRelSEM(fitEla, tau.eq = T) 
alpha(elaboration[,2:6])
# AVE 36.1%, alpha .72

m_mem <- 'M =~ mem_1 + mem_2 + mem_3 + mem_4'
fitMem <- cfa(m_mem, data = memorize, estimator = "MLM")
summary(fitMem, fit.measures = T, standardized = T)
AVE(fitMem)
compRelSEM(fitMem, tau.eq = T) 
alpha(memorize[,2:5])
# AVE 35.5%, alpha .68

m_org <- 'O =~ org_1 + org_2 + org_3 + org_4 + org_5 + org_6
org_5 ~~ org_6'
m_org <- 'O =~ org_1 + org_2 + org_3 + org_4
org_1 ~~ org_4'
fitOrg <- cfa(m_org, data = organization, estimator = "MLM")
summary(fitOrg, fit.measures = T, standardized = T)
modindices(fitOrg, sort = T, maximum.number = 15)
AVE(fitOrg)
compRelSEM(fitOrg, tau.eq = T) 
alpha(organization[,2:7])
## with the four items remaining: AVE 43.7%, alpha .77
# AVE 40.8%, alpha .78

m_pro <- 'P =~ pro_1 + pro_2 + pro_3 + pro_4 + pro_5
pro_1 ~~ pro_4'
fitPro <- cfa(m_pro, data = proofs, estimator = "MLM")
lavInspect(fitPro, "theta")
summary(fitPro, fit.measures = T, standardized = T)
modindices(fitPro, sort = T, maximum.number = 15)
AVE(fitPro)
compRelSEM(fitPro, tau.eq = T) 
# AVE 45.5%, alpha .79


m_sel <- 'S =~ test_1 + test_2 + test_4 + test_5 + test_6'
m_sel <- 'S =~ test_1 + test_2 + test_4 + test_5
test_2 ~~ test_5'
fitSel <- cfa(m_sel, data = selftest, estimator = "MLM")
summary(fitSel, fit.measures = T, standardized = T)
modindices(fitSel, sort = T, maximum.number = 15)
AVE(fitSel)
compRelSEM(fitSel, tau.eq = T) 
# AVE 36.8% alpha .75 // deleting test_6: AVE 41.3%, alpha .77

m_spa <- 'S =~ space_1 + space_2 + space_4 + space_5
space_2 ~~ space_5'
fitSpa <- cfa(m_spa, data = spacing, estimator = "MLM")
summary(fitSpa, fit.measures = T, standardized = T)
modindices(fitSpa, sort = T, maximum.number = 15)
AVE(fitSpa)
compRelSEM(fitSpa, tau.eq = T) 
# AVE 43.8%, alpha .70

## cognitive resources
m_pee <- 'P =~ peer_1 + peer_2 + peer_3 + peer_4 + peer_5'
fitPee <- cfa(m_pee, data = peers, estimator = "MLM")
summary(fitPee, fit.measures = T, standardized = T)
AVE(fitPee)
compRelSEM(fitPee, tau.eq = T) 
# this is a great factor! AVE 60%, alpha .88

m_per <- 'P =~ pers_1 + pers_2 + pers_3 + pers_4
pers_2 ~~ pers_3'
fitPer <- cfa(m_per, data = persistence, estimator = "MLM")
summary(fitPer, fit.measures = T, standardized = T)
modindices(fitPer, sort = T, maximum.number = 15)
AVE(fitPer)
compRelSEM(fitPer, tau.eq = T) 
# AVE 43.7%, alpha .78

m_tim <- 'T =~ time_1 + time_2 + time_3 + time_4 + time_5'
fitTim <- cfa(m_tim, data = time, estimator = "MLM")
summary(fitTim, fit.measures = T, standardized = T)
AVE(fitTim)
compRelSEM(fitTim, tau.eq = T) 
# AVE 44.1%, alpha .80





### EFA for metacognition

# We should see if we can reduce the number of factors in our MSR scale. Theoretically, I would prefer a 3-factor solution
# 1 --> preparation phase (planning), 2 --> exercise phase (monitoring and regulation) 3 --> evaluation phase
library(EFAtools)
df_train_nr <- sample(seq_len(nrow(data2)), size = 195)
df_train <- data2[df_train_nr, c(6,7,9,11:15,18:24,26:29)]
df_test <- data2[-df_train_nr, c(6,7,9,11:15,18:24,26:29)]
N_FACTORS(df_train, method = "PAF") # either 5 or 3 factors
N_FACTORS(df_train, method = "ULS") # also either 5 or 3 factors

corMSR <- cor(df_train)
EFA(corMSR, N=195, method = "PAF", n_factors = 3, rotation = "oblimin")
EFA(corMSR, N=195, method = "PAF", n_factors = 5, rotation = "oblimin")
# 3-Factor solution: first two factors could be interpretable, third consists of only three and
# they look like left overs, don't really fit together.
# 5-factor solution: First is the same as in 3-F-Solution. Second is a subsample of the second Factor in 3-F-S.
# Factor 4 is the same as factor 3 in 3-F-S. and Factors 3 and 5 consists of only two items.

# We can test the first two factors of both solutions.
m_factor1 <- 'F1 =~ plan_1 + plan_2 + plan_5 + mon_2 + mon_3 + eval_2 + eval_4'
fitFactor1 <- cfa(m_factor1, data = df_train, estimator = "MLM")
summary(fitFactor1, fit.measures = T, standardized = T)
# with the trained data, fit is very very good. But now with the test data:
fitFactor1 <- cfa(m_factor1, data = df_test, estimator = "MLM")
summary(fitFactor1, fit.measures = T, standardized = T)
# also a very good fit. But we have so many items, we could get rid of the items with the weakest loadings.
# step by step. The three items with loadings <.6 are mon_3, plan_2, eval_4
m_factor1 <- 'F1 =~ plan_1 + plan_2 + plan_5 + mon_2 + eval_2 + eval_4'
fitFactor1 <- cfa(m_factor1, data = df_test, estimator = "MLM")
summary(fitFactor1, fit.measures = T, standardized = T) # fit gets better, now practically perfect
m_factor1 <- 'F1 =~ plan_1 + plan_5 + mon_2 + eval_2 + eval_4'
fitFactor1 <- cfa(m_factor1, data = df_train, estimator = "MLM")
summary(fitFactor1, fit.measures = T, standardized = T)
AVE(fitFactor1) # 53.3%
compRelSEM(fitFactor1, tau.eq = T) # 0.848
m_factor1 <- 'F1 =~ plan_1 + plan_5 + mon_2 + eval_2'
fitFactor1 <- cfa(m_factor1, data = df_train, estimator = "MLM")
summary(fitFactor1, fit.measures = T, standardized = T)
AVE(fitFactor1) # 56%
compRelSEM(fitFactor1, tau.eq = T) # 0.831

# for the second factor
## for the 3-factor solution
m_factor2 <- 'F2 =~ plan_3 + plan_6 + reg_1 + reg_2 + reg_5 + mon_1 + mon_3 + mon_4 + eval_1 + eval_3'
fitFactor2 <- cfa(m_factor2, data = df_train, estimator = "MLM")
summary(fitFactor2, fit.measures = T, standardized = T)
# with the trained data, fit could be better. Chi2 is sign., CFI and TLI too low, RMSEA too high.
# test with test data:
fitFactor2 <- cfa(m_factor2, data = df_test, estimator = "MLM")
summary(fitFactor2, fit.measures = T, standardized = T) # better than with train data. But still to low. But we have 10 items... start by removing low loading items

m_factor2 <- 'F2 =~ plan_6 + reg_1 + reg_2 + reg_5 + mon_1 + mon_3 + mon_4 + eval_1 + eval_3'
fitFactor2 <- cfa(m_factor2, data = df_test, estimator = "MLM")
summary(fitFactor2, fit.measures = T, standardized = T) # better, still too low.

m_factor2 <- 'F2 =~ plan_6 + reg_1 + reg_5 + mon_1 + mon_3 + mon_4 + eval_1 + eval_3'
fitFactor2 <- cfa(m_factor2, data = df_test, estimator = "MLM")
summary(fitFactor2, fit.measures = T, standardized = T)

m_factor2 <- 'F2 =~ plan_6 + reg_5 + mon_1 + mon_3 + mon_4 + eval_1 + eval_3'
fitFactor2 <- cfa(m_factor2, data = df_test, estimator = "MLM")
summary(fitFactor2, fit.measures = T, standardized = T)
modindices(fitFactor2, sort = T, maximum.number = 10)

m_factor2 <- 'F2 =~ plan_6 + reg_5 + mon_1 + mon_3 + mon_4 + eval_1 + eval_3
reg_5 ~~  mon_1'
fitFactor2 <- cfa(m_factor2, data = df_test, estimator = "MLM")
summary(fitFactor2, fit.measures = T, standardized = T)
AVE(fitFactor2) # 29%
compRelSEM(fitFactor2, tau.eq = T) # C. alpha 0.738


m_factor2 <- 'F2 =~ plan_6 + reg_5 + mon_1 + mon_4 + eval_1 + eval_3
mon_4 ~~ eval_1'
fitFactor2 <- cfa(m_factor2, data = df_test, estimator = "MLM")
summary(fitFactor2, fit.measures = T, standardized = T)

m_factor2 <- 'F2 =~ plan_6 + reg_5 + mon_4 + eval_1'
fitFactor2 <- cfa(m_factor2, data = df_test, estimator = "MLM")
summary(fitFactor2, fit.measures = T, standardized = T)
modindices(fitFactor2, sort = T, maximum.number = 10)

m_factor2 <- 'F2 =~ plan_6 + reg_5 + mon_1 + mon_4 + eval_1
mon_4 ~~ eval_1'
fitFactor2 <- cfa(m_factor2, data = df_test, estimator = "MLM")
summary(fitFactor2, fit.measures = T, standardized = T)
modindices(fitFactor2, sort = T, maximum.number = 10)
AVE(fitFactor2) # 28.5%
compRelSEM(fitFactor2, tau.eq = T) # C alpha 0.695

m_twoFactors <- 'F1 =~ plan_1 + plan_5 + mon_2 + eval_2
F2 =~ plan_6 + reg_5 + mon_1 + mon_3 + mon_4 + eval_1 + eval_3
reg_5 ~~  mon_1'
fitTwoFactors <- cfa(m_twoFactors, data = df_test, estimator = "MLM")
summary(fitTwoFactors, fit.measures = T, standardized = T)

# use all data for analysis
fitTwoFactors <- cfa(m_twoFactors, data = data2, estimator = "MLM")
summary(fitTwoFactors, fit.measures = T, standardized = T)

# Test the second factor of the 5-factor-solution
m_factor2 <- 'F2 =~ reg_1 + reg_5 + mon_1 + mon_3 + eval_1'
fitFactor2 <- cfa(m_factor2, data = df_test, estimator = "MLM")
summary(fitFactor2, fit.measures = T, standardized = T)

m_factor2 <- 'F2 =~ reg_1 + reg_5 + mon_1 + eval_1'
fitFactor2 <- cfa(m_factor2, data = df_test, estimator = "MLM")
summary(fitFactor2, fit.measures = T, standardized = T)
AVE(fitFactor2) # 28.5%
compRelSEM(fitFactor2, tau.eq = T) # C alpha 0.619
# This factor has lower reliability. And in both cases the AVE is not good.