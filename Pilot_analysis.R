#Analysis Script - CatConf Behavioural Data, Pilot Study. 
#behavioural analysis of pilot study
library(plyr) #to load in data
library(readr) #to read files
library(tidyverse)

#devtools::install_github("matherion/userfriendlyscience")
#install.packages("userfriendlyscience")
library(userfriendlyscience)

#load in files one by one and stitch together
mydir = setwd("C:/Users/saraha/Desktop/Pilot Data")
myfiles = list.files(path=mydir, pattern="zapBox_v0.1.0_*", full.names=TRUE) #pull all files
dat_csv = ldply(myfiles, read_csv) #load in data

#inspect data
glimpse(dat_csv)

#show column names
colnames(dat_csv)

#BASIC CHECKS
#check we have every participant
completeParticipants <- subset(dat_csv, trial_index == 439)
numParticipants <- nrow(completeParticipants)
#answer should match up the number of imported files = 10 in pilot

#check we recorded PIS and consent
#this record is needed for our ethics, number of obs should match number of participants
PIS <- subset(dat_csv, PIS == 'TRUE')
if (nrow(PIS) != numParticipants){
  errorCondition('PIS ERROR')
}

consent <- subset(dat_csv, consent == 'TRUE')
if (nrow(consent) != numParticipants){
  errorCondition('CONSENT ERROR')
}


#how long did the experiment take on average to complete? 
endTime <- completeParticipants$time_elapsed
minutesTaken <- ((endTime/60)/1000)
averageTime <- mean(minutesTaken)
#average time for pilots was 39 minutes aka 40 minutes on prolific. 

#check for any text feedback on feedback form


#DEMOGRAPHICS
#what is the age range?
ageData <- subset(dat_csv, participantAge != 'NA')
age <- ageData$participantAge
ageRange <- range(age) #19-34 in pilot
meanAge <- mean(age) #25.6 in pilot

#what is the gender split?
genderData <- subset(dat_csv, participantGender != 'NA')
gender <- genderData$participantGender #7 female 3 male
female <- gender == 'female'

#what handedness do we have? 
handData <- subset(dat_csv, participantHandedness != 'NA')
handedness <- handData$participantHandedness

#ACCURACY
#look at quickfire rounds for accuracy rates 
#(did the participants learn a pairing) 
quickfireData <- subset(dat_csv, trial_type == 'jspsych-quickfire') #200 trials across 10 pp
#quickfireAccuracy <- ((subset(quickfireData, correct == 1))/20)*100 
#had not saved correct/incorrect for pilot, now updated, so calculate manually

#feedback = images/coins.png, or images/bomb.png; button 0 = Retrieve, button 1 = Zap
correctCoins <- subset(quickfireData, quickfireData$feedback == 'images/coins.png' & quickfireData$button == 0)
correctBombs <- subset(quickfireData, quickfireData$feedback == 'images/bomb.png' & quickfireData$button == 1)
correctQuickfire <- rbind(correctBombs, correctCoins)
quickfireTrials <- 20
quickfireAccuracy <- ((nrow(correctQuickfire)/quickfireTrials*numParticipants)*100) # mean accuracy in quickfire is 84%

#per participant? over time? split half?

#Look at "unclear" trial percentage accuracy - above chance? 
unclearData <- subset(dat_csv, block > 0)
unclearTrials <- 46
blockNum = 4
unclearAccuracy <- ((nrow(subset(unclearData, correct == 1))/(unclearTrials*blockNum*numParticipants)*100), #81% accuracy

Block1Accuracy <- ((nrow(subset(unclearData, block ==1 & correct ==1))/(unclearTrials*numParticipants))*100);
Block2Accuracy <- ((nrow(subset(unclearData, block ==2 & correct ==1))/(unclearTrials*numParticipants))*100);
Block3Accuracy <- ((nrow(subset(unclearData, block ==3 & correct ==1))/(unclearTrials*numParticipants))*100);
Block4Accuracy <- ((nrow(subset(unclearData, block ==4 & correct ==1))/(unclearTrials*numParticipants))*100);

#plot block by block accuracy
BlockNumber <- c(1, 2, 3, 4)
BlockAccuracy <- c(Block1Accuracy, Block2Accuracy, Block3Accuracy, Block4Accuracy)
plot(BlockNumber,BlockAccuracy, xlim=c(0,4), ylim=c(0,100), xlab="Block", ylab="Mean Accuracy", main="Blockwise Accuracy"),
abline(h=50, col="red"),
abline(h=unclearAccuracy, col = "blue")

#CONFIDENCE and ACCURACY


#Confidence in correct trials
#Confidence in incorrect trials


#Look at confidence as a function of accuracy
#Across all trials - looking for sensible ratings

#Block by block - looking for sensible ratings


#CONFIDENCE AND DISTANCE FROM BOUND
#Look at confidence as a function of the midline distance
#Across all trials - looking for sensible ratings

#Block by block - looking for sensible ratings


#Confidence as a function of the distribution variance 
#Block by block by block comparison. 




