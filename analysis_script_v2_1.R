#Analysis Script - CatConf Behavioural Data, Study - V2.1. 
#behavioural analysis of study data including data cleaning and exclusion work

###################################################
#LOAD IN PACKAGES REQUIRED
library(plyr) 
library(readr) 
library(tidyverse)
library(ggplot2)
library(dplyr)
library(devtools)
library(OneR)
library(dvmisc)
library(iterators)
library(foreach)
library(ggpubr)
library(rstatix)
library(patchwork) 
library(hrbrthemes)
library(psych)
library(RDocumentation)
library(DescTools)
library(cocor)
library(ggpmisc)
library(viridis)
library(car)
library(stats)
library(emmeans) #pairwise ttests
library(afex) #running ANOVA
library(pracma) #fitting a sigmoid

###################################################
#DATA LOADING, CLEANING, AND TIDYING
mydir = setwd("C:/Users/A-J/Desktop/Data_V2.1")
myfiles = list.files(path=mydir, pattern="zapBox_v0.2.1_*", full.names=TRUE) #pull all files
dat_csv = ldply(myfiles, read_csv) #load in data

#create a PID code to identify participants more clearly. 
user_ids <- unique(dat_csv$user_id) # cache this to save recalculating 
dat_csv$PID <- NA_real_ # initalise PIDs as NA
dat_csv$PID <- sapply(
  dat_csv$user_id,  # vector to iterate over
  function(id) which(user_ids == id)  # function to apply to each element
)

###################################################
#BASIC CHECKS
#check we have every participant
completeParticipants <- unique(dat_csv$PID)
numParticipants <- length(completeParticipants)
#answer should match up the number of imported files
if (length(myfiles) != numParticipants){
  errorCondition('PARTICIPANT LOADING ERROR ') #error if all files did not load
} #numParticipants= 10 in pilot

#check we recorded PIS and consent, record needed for our ethics,
if (nrow(PIS<- subset(dat_csv, PIS == 'TRUE')) != numParticipants){
  errorCondition('PIS ERROR')
}
if (nrow(consent <- subset(dat_csv, consent == 'TRUE')) != numParticipants){
  errorCondition('CONSENT ERROR')
}

#######################################################
#DEMOGRAPHICS
#what is the age, mean, range?
ageData <- subset(dat_csv, participantAge != "NA")
summary(ageData$participantAge)#mean 33
range(ageData$participantAge) #18-64
sd(ageData$participantAge) #12.7

#what is the gender split?
genderData <- subset(dat_csv, participantGender != 'NA')
gender <- genderData$participantGender #36 female 24 male
female <- sum(gender == 'female')
male <- sum(gender == 'male')
nonbinary <- sum(gender == 'non-binary')

#what handedness do we have?
handData <- subset(dat_csv, participantHandedness != 'NA')
handedness <- handData$participantHandedness
right <- sum(handedness == "right") #54 right, 5 left, 1 ambi
ambi <- sum(handedness == 'ambidextrous')

#######################################################
#PARTICIPANT COMMENTS
feedback_strat <- subset(dat_csv, feedback_strategy != 'NA')
feedback_strat <- feedback_strat$feedback_strategy
feedback_strat
#seem to follow sensible strategies - little too much left V right maybe? 

feedback_tech <- subset(dat_csv, feedback_technical != 'NA')
feedback_tech <- feedback_tech$feedback_technical
feedback_tech
#no tech issues reported - would like to know how many blocks there are left - consider for future iterations

feedback_thought <- subset(dat_csv, feedback_comments != 'NA')
feedback_thought <- feedback_thought$feedback_comments
feedback_thought
# enjoyed/positive

########################################################
#apply exclusion criteria here
#2sd of the mean accuracy in both quickfire and confidence trials

#look at quickfire first
quickfireData <- subset(dat_csv, trial_type == 'jspsych-quickfire') #1200 trials across 60 pp
trials <- c(1:1200)
for (i in trials){
  if(quickfireData$button[i] == "null"){
    quickfireData$correct[i] = 0
    quickfireData$incorrect[i] =1 
  }} #reset NA values

PIDwiseQuickfire <- quickfireData %>% 
  group_by(PID) %>%
  summarise(meanQuick = mean(correct))

averageQuick <- mean(PIDwiseQuickfire$meanQuick) #93.1666 average
sdQuick <- sd(PIDwiseQuickfire$meanQuick) #8.971
cutoffLow <- averageQuick - 2*sdQuick
cutoffHigh <- averageQuick + 2*sdQuick
excludeQuick <- subset(PIDwiseQuickfire, meanQuick < cutoffLow | meanQuick > cutoffHigh) #potential exclusion list

#look at confidence trials 
unclearData <- subset(dat_csv, block > 0)
PIDwiseCloudy <- unclearData %>% 
  group_by(PID) %>%
  summarise(meanCloud = mean(correct))

averageCloud <- mean(PIDwiseCloudy$meanCloud) #81.1
sdCloud <- sd(PIDwiseCloudy$meanCloud) #12.5
cutoffLow <- averageCloud - 2*sdCloud
cutoffHigh <- averageCloud + 2*sdCloud
excludeCloud <- subset(PIDwiseCloudy, meanCloud < cutoffLow | meanCloud > cutoffHigh) #potential exclusion list

#cross over of 3 partipants, total of 8 exclusion candidates. 
#proceed with these PIDs to be removed. 
excludePIDs <- unique(c(excludeQuick$PID, excludeCloud$PID))
dat_csv <- dat_csv[ ! dat_csv$PID %in% excludePIDs, ]
new_data <- dat_csv
#exclusion criteria have now been applied, proceed as normal from here. 

completeParticipants <- unique(new_data$PID)
########################################################
#EXPERIMENTAL INFO
#how long did the experiment take on average to complete?
lastTrials <- subset(new_data, trial_index > 500)
minutesTaken <- ((lastTrials$time_elapsed/60)/1000)
averageTime <- summary(minutesTaken) 
averageTime
sd(minutesTaken)
#mean 34, range - 23-65
timeout <- subset(new_data, time_elapsed > 7000000) # just 1 pp, pp25 who is running long

#########################################################################
#QUICKFIRE ROUNDS
#look at quickfire rounds for accuracy rates(did the participants learn a pairing) 
quickfireData <- subset(new_data, trial_type == 'jspsych-quickfire') #1200 trials across 60 pp
trials <- c(1:length(quickfireData$button))
for (i in trials){
  if(quickfireData$button[i] == "null"){
    quickfireData$correct[i] = 0
    quickfireData$incorrect[i] =1 
  } #reset NA values
}
#feedback = images/coins.png, or images/bomb.png; button 0 = Retrieve, button 1 = Zap
correctCoins <- subset(quickfireData, quickfireData$feedback == 'images/coins.png' & quickfireData$button == 0)
correctBombs <- subset(quickfireData, quickfireData$feedback == 'images/bomb.png' & quickfireData$button == 1)
correctQuickfire <- rbind(correctBombs, correctCoins)
quickfireTrials <- 20
quickfireAccuracy <- ((nrow(correctQuickfire)/(quickfireTrials*length(completeParticipants))*100)) 
# mean accuracy in quickfire is 93.5%

#per participant?
PIDwiseCorrectQuickfire <- quickfireData %>% 
  group_by(PID) %>%
  summarise(mean = mean(correct))
PIDwiseQuickfireAccuracy <- ggplot(data = PIDwiseCorrectQuickfire) +
  geom_point(mapping = aes(x = PID, y = mean)) +
  labs(title="Participant Quickfire Accuracy", x="PID", y="Accuracy") 
PIDwiseQuickfireAccuracy + xlim(1, length(completeParticipants))+ ylim(0, 1)
#looks pretty good - exclusions tidied up

#split half
correctQuickfire1stHalf <- subset(quickfireData, trial_index <18)
correctQuickfire2ndHalf <- subset(quickfireData, trial_index >=19)

PIDwiseCorrectQuickfire1stHalf <- correctQuickfire1stHalf %>% 
  group_by(PID) %>%
  summarise(grp.mean = mean(correct))
PIDwiseCorrectQuickfire1stHalf$split_half <- 1
PIDwiseCorrectQuickfire2ndHalf<- correctQuickfire2ndHalf %>% 
  group_by(PID) %>%
  summarise(grp.mean = mean(correct))
PIDwiseCorrectQuickfire2ndHalf$split_half <- 2
splithalf <- rbind(PIDwiseCorrectQuickfire1stHalf, PIDwiseCorrectQuickfire2ndHalf)

PIDwiseSplitHalfQuickfireAccuracy <- ggplot(data = splithalf) +
  geom_point(mapping = aes(x = PID, y = grp.mean, color = split_half)) +
  labs(title="Split Half-Participant Quickfire Accuracy", x="PID", y="Accuracy") 

PIDwiseSplitHalfQuickfireAccuracy + xlim(1,length(completeParticipants)) + ylim(0, 1)

#########################################################################
#TRAINING FASTDROP "CLEAR DAY" TRIALS 
#check up on distribution properties. 
#calculate the mean and variance for each and see if it maps across to the distributions
mask <- new_data$distribution_variance == 6400 & new_data$button == "null" & new_data$distribution_mean == 222
summary(new_data$drop_location[mask]) #mean == 222.2 (close to 222)
sd(new_data$drop_location[mask], na.rm = TRUE)#78.7 (close to 80)

mask <- new_data$distribution_variance == 6400 & new_data$button == "null" & new_data$distribution_mean == 578
summary(new_data$drop_location[mask]) #mean == 577.7 (close to 578)
sd(new_data$drop_location[mask],na.rm = TRUE)#78.6 (close to 80)

mask <- new_data$distribution_variance == 19600 & new_data$button == "null" & new_data$distribution_mean == 278
summary(new_data$drop_location[mask]) #mean == 278.5 (close to 578)
sd(new_data$drop_location[mask],na.rm = TRUE)#137.5 (close to 140)

mask <- new_data$distribution_variance == 19600 & new_data$button == "null" & new_data$distribution_mean == 522
summary(new_data$drop_location[mask]) #mean == 521.5 (close to 522)
sd(new_data$drop_location[mask], na.rm = TRUE)#137.5 (close to 140)
#mean and sd working well using the force function to generate trials. 

#####################################################################################
#DATA TRANSFORMATION - flip trials around their midpoints and realign on 350 
clearData <- subset(new_data, block <= 0) #subset data to focus on training trials
clearData$flippedLocation <- clearData$drop_location #set default locations
clearData$boundary <- 350 #set default boundary
clearData$distance_to_bound <- abs(clearData$drop_location - clearData$boundary) #set default distance to bound

mask <- clearData$distribution_name == 'wide' & clearData$distribution_mean == 278 & !is.na(clearData$drop_location)
clearData$boundary[mask] <- 450
clearData$distance_to_bound[mask] <- abs(clearData$drop_location[mask] - clearData$boundary[mask])
clearData$flippedLocation[mask] <- clearData$boundary[mask] + clearData$distance_to_bound[mask]
mask <- clearData$distribution_name == 'wide' & clearData$distribution_mean == 278 & !is.na(clearData$drop_location) & clearData$drop_location > 450
clearData$flippedLocation[mask] <- clearData$boundary[mask] - clearData$distance_to_bound[mask]

mask <- clearData$distribution_name == 'narrow' & clearData$distribution_mean == 578 & !is.na(clearData$drop_location)
clearData$boundary[mask] <- 450
clearData$distance_to_bound[mask] <- abs(clearData$drop_location[mask] - clearData$boundary[mask])
clearData$flippedLocation[mask] <- clearData$boundary[mask] - clearData$distance_to_bound[mask]
mask <- clearData$distribution_name == 'narrow' & clearData$distribution_mean == 578 & !is.na(clearData$drop_location) & clearData$drop_location < 450
clearData$flippedLocation[mask] <- clearData$boundary[mask] + clearData$distance_to_bound[mask]

clearData$recentredFlipLocation <- clearData$flippedLocation #set default recentred location
mask <- clearData$boundary == 450
clearData$recentredFlipLocation[mask] <- clearData$flippedLocation[mask] - 100

#density plots
a <- ggplot(data=clearData, aes(recentredFlipLocation))
a + geom_density(aes(color = distribution_name)) +
  scale_color_manual(values = c("#868686FF", "#EFC000FF"))
mu <- clearData %>% 
  group_by(distribution_name) %>%
  summarise(grp.mean = mean(recentredFlipLocation))
a + geom_density(aes(fill = distribution_name), alpha = 0.4) +
  geom_vline(aes(xintercept = grp.mean, color = distribution_name),
             data = mu, linetype = "dashed") +
  scale_color_manual(values = c("#868686FF", "#EFC000FF"))+
  scale_fill_manual(values = c("#868686FF", "#EFC000FF")) +
  labs(title="Density Plot for ( recentered flipped) Training Locations", x="Drop location (flipped and centred)", y="Density")+
  xlim(0,800)
#means at 222 and 522 and midline at 350 as should be with transformation. good job me. 

clearData1 <- subset(clearData, boundary == 350)
b <- ggplot(data=clearData1, aes(flippedLocation))
b + geom_density(aes(color = distribution_name)) +
  scale_color_manual(values = c("#868686FF", "#EFC000FF"))
mu <- clearData1 %>% 
  group_by(distribution_name) %>%
  summarise(grp.mean = mean(drop_location))
b + geom_density(aes(fill = distribution_name), alpha = 0.4) +
  geom_vline(aes(xintercept = grp.mean, color = distribution_name),
             data = mu, linetype = "dashed") +
  scale_color_manual(values = c("#868686FF", "#EFC000FF"))+
  scale_fill_manual(values = c("#868686FF", "#EFC000FF")) +
  labs(title="Density Plot for ( flipped) Training Locations", x="Drop location (flipped)", y="Density")
# means and midline looks spot on

clearData2 <- subset(clearData, boundary == 450)
c <- ggplot(data=clearData2, aes(flippedLocation))
c + geom_density(aes(color = distribution_name)) +
  scale_color_manual(values = c("#868686FF", "#EFC000FF"))
mu <- clearData2 %>% 
  group_by(distribution_name) %>%
  summarise(grp.mean = mean(drop_location))
c + geom_density(aes(fill = distribution_name), alpha = 0.4) +
  geom_vline(aes(xintercept = grp.mean, color = distribution_name),
             data = mu, linetype = "dashed") +
  scale_color_manual(values = c("#868686FF", "#EFC000FF"))+
  scale_fill_manual(values = c("#868686FF", "#EFC000FF")) +
  labs(title="Density Plot for (recentered flipped) Training Locations", x="Drop location (recentred flipped)", y="Density")
# means and midline looks spot on

#########################################################################
#TEST "CLOUDY DAY" TRIALS
#accuracy - above chance? 
unclearData <- subset(new_data, block > 0)
unclearTrials <- 60
blockNum <- 4

unclearAccuracy <- (nrow(subset(unclearData, correct == 1))/(unclearTrials*blockNum*length(completeParticipants))*100) 
#84.34\% accuracy

#find accuracy per participant per block
block1Cloud <- subset(unclearData, block == 1)
block2Cloud <- subset(unclearData, block == 2)
block3Cloud <- subset(unclearData, block == 3)
block4Cloud <- subset(unclearData, block == 4)

block1CloudAccuracy <- block1Cloud %>% 
  group_by(PID) %>%
  summarise(grp.mean = mean(correct))
block1CloudAccuracy$block <- 1

block2CloudAccuracy <- block2Cloud %>% 
  group_by(PID) %>%
  summarise(grp.mean = mean(correct))
block2CloudAccuracy$block <- 2

block3CloudAccuracy <- block3Cloud %>% 
  group_by(PID) %>%
  summarise(grp.mean = mean(correct))
block3CloudAccuracy$block <- 3

block4CloudAccuracy <- block4Cloud %>% 
  group_by(PID) %>%
  summarise(grp.mean = mean(correct))
block4CloudAccuracy$block <- 4

blockPIDwideCloudAccuracy <- rbind(block1CloudAccuracy, block2CloudAccuracy, block3CloudAccuracy, block4CloudAccuracy)

blockPIDwideCloudAccuracyPlot <- ggplot(data = blockPIDwideCloudAccuracy) +
  geom_point(mapping = aes(x = PID, y = grp.mean, color = block)) +
  xlim(1,length(completeParticipants)) + ylim(0, 1) +
  labs(title="Cloudy Day: Blockwise Participant Accuracy", x="PID", y="Accuracy", color = "Block") +
  scale_color_gradient()
blockPIDwideCloudAccuracyPlot 
#overall accuracy high across participants and blocks
#only 4 participants had values for 1 block each that fell below the cutoff point - exclusions working well. 

#################################################################
#TRENDS AND PATTERNS IN DATA 
#reaction time
unclearData$RT <- as.numeric(as.character(unclearData$delta_response_time))
unclearData <- subset(unclearData, RT != 'NA')
unclearCorrect <- subset(unclearData, correct == 1)
unclearIncorrect <- subset(unclearData, incorrect == 1)
avgCorrectRT <- mean(unclearCorrect$RT) #1835.2
avgIncorrectRT <- mean(unclearIncorrect$RT) #2134.5

#reaction time per participant
unclearCorrectPID <- unclearCorrect %>% 
  group_by(PID) %>%
  summarise(mean = mean(RT))
unclearCorrectPID$group <- "Correct"

unclearIncorrectPID <- unclearIncorrect %>% 
  group_by(PID) %>%
  summarise(mean = mean(RT))
unclearIncorrectPID$group <- "Incorrect"

unclearCombined <- rbind(unclearCorrectPID, unclearIncorrectPID)
bxp <- ggboxplot(
  unclearCombined, x = "group", y = "mean", 
  ylab = "Reaction Time (ms)", xlab = "",add = "jitter",
  title="Cloudy Day: Decision Reaction Time"
)
bxp

#t-test the difference
unclearCorrectPID$meanIncorrect <- unclearIncorrectPID$mean
t.test(unclearCorrectPID$mean, unclearCorrectPID$meanIncorrect, paired = TRUE, alternative = "two.sided")
#significantly different

#########################################################################
#CONFIDENCE TRIALS 
confidence <- subset(dat_csv, confidence != 'null') #remove missing confidence reports
confidence$confidence <- as.numeric(as.character(confidence$confidence))
confidence$confidenceRT <- as.numeric(as.character(confidence$delta_confidence_response_time))

#bin confidence into 2 bins (no more possible with pilot sample - watch this space) 
confidence$binnedConfidence <- NA_real_ # initalise distance bins as NA
breaks <- quantile(confidence$confidence, seq(0, 1, by = 0.2))
confidence$binnedConfidence <- cut(confidence$confidence, breaks, include.lowest = TRUE, 
                                   labels = c("1", "2", "3", "4", "5"))

#DATA TRANSFORMATION - flip trials around their midpoints and realign on 350 
confidence$flippedLocation <- confidence$drop_location #set default locations
confidence$boundary <- 350 #set default boundary
confidence$distance_to_bound <- abs(confidence$drop_location - confidence$boundary) #set default distance to bound

mask <- confidence$distribution_name == 'wide' & confidence$distribution_mean == 278 & !is.na(confidence$drop_location)
confidence$boundary[mask] <- 450
confidence$distance_to_bound[mask] <- abs(confidence$drop_location[mask] - confidence$boundary[mask])
confidence$flippedLocation[mask] <- confidence$boundary[mask] + confidence$distance_to_bound[mask]
mask <- confidence$distribution_name == 'wide' & confidence$distribution_mean == 278 & !is.na(confidence$drop_location) & confidence$drop_location > 450
confidence$flippedLocation[mask] <- confidence$boundary[mask] - confidence$distance_to_bound[mask]

mask <- confidence$distribution_name == 'narrow' & confidence$distribution_mean == 578 & !is.na(confidence$drop_location)
confidence$boundary[mask] <- 450
confidence$distance_to_bound[mask] <- abs(confidence$drop_location[mask] - confidence$boundary[mask])
confidence$flippedLocation[mask] <- confidence$boundary[mask] - confidence$distance_to_bound[mask]
mask <- confidence$distribution_name == 'narrow' & confidence$distribution_mean == 578 & !is.na(confidence$drop_location) & confidence$drop_location < 450
confidence$flippedLocation[mask] <- confidence$boundary[mask] + confidence$distance_to_bound[mask]

confidence$recentredFlipLocation <- confidence$flippedLocation #set default recentred location
mask <- confidence$boundary == 450
confidence$recentredFlipLocation[mask] <- confidence$flippedLocation[mask] - 100

#bin the distance according to the way it was generated in the experiment
confidence$rawLocation <- confidence$flippedLocation
midline1 <- 350
breaks <- c(0, (midline1 - 180), (midline1 - 140), (midline1 - 100), (midline1 - 60), (midline1 - 20), 
            (midline1 + 20), (midline1 + 60), (midline1 +100), (midline1 + 140), (midline1 + 180), 800)
mask <- confidence$boundary == midline1
confidence$binnedLocation[mask] <- cut(confidence$rawLocation[mask], breaks, include.lowest = TRUE,labels = c( "85", "190", "230", "270", "310", "350", "390", "430", "470", "510", "675"))
confidence$distanceBin[mask] <- 1

mask <- confidence$binnedLocation == 5 | confidence$binnedLocation == 7
confidence$distanceBin[mask] <- 2
mask <- confidence$binnedLocation == 4 | confidence$binnedLocation == 8
confidence$distanceBin[mask] <- 3
mask <- confidence$binnedLocation == 3 | confidence$binnedLocation == 9
confidence$distanceBin[mask] <- 4
mask <- confidence$binnedLocation == 2 | confidence$binnedLocation == 10
confidence$distanceBin[mask] <- 5
mask <- confidence$binnedLocation == 1 | confidence$binnedLocation == 11
confidence$distanceBin[mask] <- 6

midline2 <- 450
breaks <- c(0, (midline2 - 180), (midline2 - 140), (midline2 - 100), (midline2 - 60), (midline2 - 20), 
            (midline2 + 20), (midline2 + 60), (midline2 +100), (midline2 + 140), (midline2 + 180), 800)
mask <- confidence$boundary == midline2
confidence$binnedLocation[mask] <- cut(confidence$rawLocation[mask], breaks, include.lowest = TRUE,labels = c( "185", "290", "330", "370", "410", "450", "490", "530", "570", "610", "775"))
confidence$distanceBin[mask] <- 1

mask <- confidence$boundary == midline2 & confidence$binnedLocation == 5 | confidence$boundary == midline2 & confidence$binnedLocation == 7
confidence$distanceBin[mask] <- 2
mask <- confidence$boundary == midline2 & confidence$binnedLocation == 4 | confidence$boundary == midline2 & confidence$binnedLocation == 8
confidence$distanceBin[mask] <- 3
mask <- confidence$boundary == midline2 & confidence$binnedLocation == 3 | confidence$boundary == midline2 & confidence$binnedLocation == 9
confidence$distanceBin[mask] <- 4
mask <- confidence$boundary == midline2 & confidence$binnedLocation == 2 | confidence$boundary == midline2 & confidence$binnedLocation == 10
confidence$distanceBin[mask] <- 5
mask <- confidence$boundary == midline2 & confidence$binnedLocation == 1 | confidence$boundary == midline2 & confidence$binnedLocation == 11
confidence$distanceBin[mask] <- 6

#check over the binning
Locations  <- ggplot(data=confidence, aes(binnedLocation, fill = distribution_name)) + 
  geom_histogram(col=I("navy")) +
  labs(title="Histogram", x="Location Bin", y="Frequency") 
Locations

Locations  <- ggplot(data=confidence, aes(distanceBin, fill = distribution_name)) + 
  geom_histogram(col=I("navy")) +
  labs(title="Histogram", x="Location Bin", y="Frequency") 
Locations

# plot out raw confidence reports as a density.... 
a <- ggplot(data=confidence, aes(confidence))
a + geom_density(aes(color = distribution_name)) +
  scale_color_manual(values = c("#868686FF", "#EFC000FF"))
a + geom_density(aes(fill = distribution_name), alpha = 0.4) +
  geom_vline(aes(xintercept = grp.mean, color = distribution_name),
             data = mu, linetype = "dashed") +
  xlim(0,100)+
  scale_color_manual(values = c("#868686FF", "#EFC000FF"))+
  scale_fill_manual(values = c("#868686FF", "#EFC000FF")) +
  labs(title="Density Plot for Confidence Reports", x="Raw Confidence", y="Density")

#Look at confidence as a function of accuracy
correctConfidence <- subset(confidence, correct == 1)
incorrectConfidence <- subset(confidence, incorrect == 1)

#look at the difference between correct and incorrect trials. 
correctConf <- correctConfidence %>% 
  group_by(PID) %>%
  summarise(meanCorrect = mean(confidence))
incorrectConf <- incorrectConfidence %>% 
  group_by(PID) %>%
  summarise(meanIncorrect = mean(confidence))
correctConf$meanIncorrect <- incorrectConf$meanIncorrect

t.test(correctConf$meanCorrect, correctConf$meanIncorrect, paired = TRUE, alternative = "two.sided")
#significantly different

#test the significance of the RT difference
correctConfRT <- correctConfidence %>% 
  group_by(PID) %>%
  summarise(grp.mean = mean(confidenceRT))
correctConfRT$group <-1
incorrectConfRT <- incorrectConfidence %>% 
  group_by(PID) %>%
  summarise(grp.mean = mean(confidenceRT))
incorrectConfRT$group <-0
confCompareRT <- rbind(correctConfRT,incorrectConfRT)

t.test(correctConfRT$grp.mean, incorrectConfRT$grp.mean, paired = TRUE, alternative = "two.sided") #not sig 0.1
#difference in the reaction times for confidence reports for correct and incorrect trials


#########################################################################
#DISTANCE FROM BOUND
#ACCURACY
accuracyDist <- confidence %>% 
  group_by(distanceBin) %>%
  summarise(grp.mean = mean(correct))
accuracyDist$accuracy <- accuracyDist$grp.mean

a <- ggplot(data=accuracyDist, aes(distanceBin, accuracy))+ 
  geom_point()+
  labs(title="Accuracy by distance to bound", x="Distance Bin", y="Mean Accuracy")
a

#CONFIDENCE
confDist <- confidence %>% 
  group_by(distanceBin) %>%
  summarise(grp.mean = mean(confidence))
confDist$confidence <- confDist$grp.mean

a <- ggplot(data=confDist, aes(distanceBin, confidence))+ 
  geom_point()+
  labs(title="Confidence by distance to bound", x="Distance Bin", y="Mean Confidence")
a

##################################################################################################
#analysis time - ANOVA prep
#average out the mean confidence per participant per distanceBin
#want the "correct responses in bins 2-5 as a function of wide or narrow. 

#how do we select the correct responses - for those bins accuracy is already defined in that way. 
correctConfidence <- subset(confidence, correct == 1)

correctConfN <- subset(correctConfidence, distribution_name == "narrow")
confidencePerParticipantPerBinN <- correctConfN  %>% 
  group_by(PID, distanceBin) %>%
  summarise(grp.mean = mean(confidence)) 
confidencePerParticipantPerBinN$distribution <- "narrow"

correctConfW <- subset(correctConfidence, distribution_name == "wide")
confidencePerParticipantPerBinW <- correctConfW  %>% 
  group_by(PID, distanceBin) %>%
  summarise(grp.mean = mean(confidence)) 
confidencePerParticipantPerBinW$distribution <- "wide"

confidencePerParticipantPerBin <- rbind(confidencePerParticipantPerBinN, confidencePerParticipantPerBinW)
  
confidencePPPB <- subset(confidencePerParticipantPerBin, distanceBin > 1 & distanceBin <6)
confidencePPPB$distanceBin <- as.factor(confidencePPPB$distanceBin)
confidencePPPB$distribution<- as.factor(confidencePPPB$distribution)


#plot out aggregate means
confN <- subset(confidencePPPB, distribution == "narrow")
nBins <- confN %>%
  group_by(distanceBin)%>%
  summarise(binMean = mean(grp.mean))
nBins$distribution <- "narrow"

confW <- subset(confidencePPPB, distribution == "wide")
wBins <- confW %>%
  group_by(distanceBin)%>%
  summarise(binMean = mean(grp.mean))
wBins$distribution <- "wide"

bins <- rbind(nBins, wBins)

ggplot(bins, aes(factor(distanceBin), y=binMean, fill=distribution))+ 
  stat_summary(fun = mean, geom = "bar", position = position_dodge(1)) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", position = position_dodge(1), width=0.5) +
  scale_fill_manual(values=c("#E69F00","#999999")) +
  labs(title="Confidence across distance bins", x="Distance Bin", y="Mean Confidence")+
  ylim(0,100)

aggregate(binMean~distribution+distanceBin, data=bins, FUN=mean) #and check the grand means

#run the anova
confidenceAOV <- as.data.frame(confidencePPPB)

ggplot(confidenceAOV, aes(factor(distanceBin), y=grp.mean, fill=distribution))+ 
  geom_violin(position=position_dodge(1)) + 
  geom_boxplot(position=position_dodge(1),width=0.1)+
  scale_fill_manual(values=c("#E69F00","#999999"))

confAOV <- aov_ez(id="PID", dv="grp.mean", data=confidenceAOV, within = c("distanceBin", "distribution"))
confAOV
#check the pairwise comparisons
int_comp <- emmeans(confAOV, ~distribution | distanceBin)
pairs(int_comp, adjust="none")

#redo as within subject t-tests
bin2N <- subset(confidenceAOV, distanceBin == 2 & distribution == "narrow")
bin2W <- subset(confidenceAOV, distanceBin == 2 & distribution == "wide")
t.test(bin2N$grp.mean, bin2W$grp.mean, paired = TRUE, alternative = "two.sided") #not sig
mean(bin2N$grp.mean) #higher
mean(bin2W$grp.mean) #lower

bin3N <- subset(confidenceAOV, distanceBin == 3 & distribution == "narrow")
bin3W <- subset(confidenceAOV, distanceBin == 3 & distribution == "wide")
t.test(bin3N$grp.mean, bin3W$grp.mean, paired = TRUE, alternative = "two.sided") #sig
mean(bin3N$grp.mean) #lower
mean(bin3W$grp.mean) #higher

bin4N <- subset(confidenceAOV, distanceBin == 4 & distribution == "narrow")
bin4W <- subset(confidenceAOV, distanceBin == 4 & distribution == "wide")
t.test(bin4N$grp.mean, bin4W$grp.mean, paired = TRUE, alternative = "two.sided") #sig
mean(bin4N$grp.mean) #lower
mean(bin4W$grp.mean) #higher

bin5N <- subset(confidenceAOV, distanceBin == 5 & distribution == "narrow")
bin5W <- subset(confidenceAOV, distanceBin == 5 & distribution == "wide")
t.test(bin5N$grp.mean, bin5W$grp.mean, paired = TRUE, alternative = "two.sided") #sig
mean(bin5N$grp.mean) #lower
mean(bin5W$grp.mean) #higher
############################################################################
#ERROR TRIALS 
##check errors in different bins, how many bins can we plausible analyse
errorBin2 <- subset(confidence, distanceBin ==2)
errorsPID <- errorBin2 %>%
  group_by(PID)%>%
  summarise(errorCount2 = sum(incorrect))

errorBin3 <- subset(confidence, distanceBin ==3)
errorBin3PID <- errorBin3 %>%
  group_by(PID)%>%
  summarise(errorCount3 = sum(incorrect))

errorBin4 <- subset(confidence, distanceBin ==4)
errorBin4PID <- errorBin4 %>%
  group_by(PID)%>%
  summarise(errorCount4 = sum(incorrect))

errorBin5 <- subset(confidence, distanceBin ==5)
errorBin5PID <- errorBin5 %>%
  group_by(PID)%>%
  summarise(errorCount5 = sum(incorrect))

errorsPID$errorCount3 <- errorBin3PID$errorCount3
errorsPID$errorCount4 <- errorBin4PID$errorCount4
errorsPID$errorCount5 <- errorBin5PID$errorCount5

errorsPID #this is a table of frequency of errors per distance bin (2-5) per participant
#bins 2 and 3 have okay error rates - bins 4 and 5 are negligible for many participants.

#in bins 2 and 3 where are the errors coming from = wide or narrow trials?
errors2N <- subset(errorBin2, distribution_name == "narrow")
errorBinNW <- errors2N %>%
  group_by(PID)%>%
  summarise(errorCount2N = sum(incorrect))

errors2W <- subset(errorBin2, distribution_name == "wide")
errorBin2Wide <- errors2W %>%
  group_by(PID)%>%
  summarise(errorCount2W = sum(incorrect))

errors3N <- subset(errorBin3, distribution_name == "narrow")
errorBin3Narrow <- errors3N %>%
  group_by(PID)%>%
  summarise(errorCount3N = sum(incorrect))

errors3W <- subset(errorBin3, distribution_name == "wide")
errorBin3Wide <- errors3W %>%
  group_by(PID)%>%
  summarise(errorCount3W = sum(incorrect))

errorBinNW$errorCount2W <- errorBin2Wide$errorCount2W
errorBinNW$errorCount3N <- errorBin3Narrow$errorCount3N
errorBinNW$errorCount3W <- errorBin3Wide$errorCount3W

errorBinNW #these are the error found in bin 2 and 3 split by distribution

t.test(errorBinNW$errorCount2N, errorBinNW$errorCount2W, paired = TRUE, alternative = "two.sided") #non sig
mean(errorBinNW$errorCount2N)#6.3 narrow
mean(errorBinNW$errorCount2W)#6.4 wide
t.test(errorBinNW$errorCount3N, errorBinNW$errorCount3W, paired = TRUE, alternative = "two.sided") # non sig
mean(errorBinNW$errorCount3N) #3.3 Narrow make more errors in bin 3 thinking it's wide when it's narrow
mean(errorBinNW$errorCount3W) #2.1 Wide - over apply wide distirbtuion spread to bin 3

#confidence on the trials in which they make errors -focus on bin2 (maybe bin3?)
errorBinConf <- subset(errorBin2, incorrect == 1)
mean(errorBinConf$confidence) #48.6

errorBinConf1 <- errorBinConf %>%
  group_by(PID)%>%
  summarise(meanConf = mean(confidence))

correctBinConf <- subset(errorBin2, correct ==1)
mean(correctBinConf$confidence) #65.16

errorBinConf2 <- correctBinConf %>%
  group_by(PID)%>%
  summarise(meanConf = mean(confidence))

t.test(errorBinConf1$meanConf, errorBinConf2$meanConf, paired = TRUE, alternative = "two.sided") 
#sig difference in the confidence in the correct and error trials in this bin

#where are the errors coming from - wide or narrow? 
errorWide <- subset(errorBinConf, distribution_name == "wide") #337
mean(errorWide$confidence) #48.5
errorNarrow <- subset(errorBinConf, distribution_name == "narrow") #328
mean(errorNarrow$confidence) #48.7

errorBinWide <- errorWide %>%
  group_by(PID)%>%
  summarise(meanConf = mean(confidence)) #only 49 of 52 have wide errors in bin 2
extra <- c(1, 15, 24)
extra <-as.data.frame(extra)
extra$meanConf <- c("NA")
extra <- extra %>% 
  rename(
    PID = extra)
errorBinWide <- rbind(errorBinWide, extra)
errorBinWide <- arrange(errorBinWide, PID)
errorBinWide$meanConf <- as.numeric(errorBinWide$meanConf)

errorBinNarrow <- errorNarrow %>%
  group_by(PID)%>%
  summarise(meanConf = mean(confidence)) #only 47 have made narrow errors in bin 2
extra <- c(2, 8, 17, 20, 37)
extra <-as.data.frame(extra)
extra$meanConf <- c("NA", "NA","NA","NA","NA")
extra <- extra %>% 
  rename(
    PID = extra)
errorBinNarrow <- rbind(errorBinNarrow, extra)
errorBinNarrow <- arrange(errorBinNarrow, PID)
errorBinNarrow$meanConf <- as.numeric(errorBinNarrow$meanConf)

t.test(errorBinNarrow$meanConf, errorBinWide$meanConf, paired = TRUE, alternative = "two.sided") #not sig diff
#many pbins empty so only running on 44 people - not realiable 

############################################################################
#BIN 1 and BIN 6 ANALYSIS
#BIN1: look at bin1 - when they thought it was narrow versus when they thought it was wide
bin1 <- subset(confidence, distanceBin == 1)

#first set up a block type variable (0/1) to tell us whether narrow is a bomb or wide is
#blocktype = 0, narrow = bomb (should zap 1), blocktype = 1, wide is bomb (should zap 1)
mask <- bin1$stimulus == "orange1" & bin1$distribution_name == "wide" | 
  bin1$stimulus == "blue1" & bin1$distribution_name == "narrow"
bin1$blocktype[mask] <- 0 #when zap1 think it's narrow, when retrieve0 think it's wide

narrowResponseZap <- subset(bin1, blocktype == 0 & button == 1)
wideResponseRetrieve <- subset(bin1, blocktype == 0 & button == 0)

mask <- bin1$stimulus == "orange1" & bin1$distribution_name == "narrow" | 
  bin1$stimulus == "blue1" & bin1$distribution_name == "wide"
bin1$blocktype[mask] <- 1 #when zap1 think it's wide, when retrieve0 think it's narrow

narrowResponseRetrieve <- subset(bin1, blocktype == 1 & button == 0)
wideResponseZap <- subset(bin1, blocktype == 1 & button == 1)

wideResponse <- rbind(wideResponseRetrieve, wideResponseZap)
narrowResponse <- rbind(narrowResponseRetrieve, narrowResponseZap)

wideConf <- wideResponse %>%
  group_by(PID)%>% 
  summarise(conf = mean(confidence))
wideConf$distribution <- "wide"

narrowConf <- narrowResponse %>%
  group_by(PID)%>%
  summarise(conf = mean(confidence))
#1 pp never thought it was a narrow trial at all in this bin
extra <- c( 24)
extra <-as.data.frame(extra)
extra$conf <- c("NA")
extra <- extra %>% 
  rename(
    PID = extra,
    conf = conf)
narrow <- rbind(narrowConf, extra)
narrowDF <- arrange(narrow, PID)
narrowDF$conf <- as.numeric(narrowDF$conf)
narrowDF$distribution <- "narrow"

#plot out grand means
bin1s <- rbind(narrowDF, wideConf)
ggplot(bin1s, aes(factor(distribution), y=conf))+ 
  stat_summary(fun = mean, geom = "bar", position = position_dodge(1)) + 
  scale_fill_manual(values=c("#E69F00","#999999")) +
  labs(title="Confidence in central bin", x="Distribution", y="Mean Confidence")+
  ylim(0,100)

means <- bin1s %>%
  group_by(PID)%>%
  summarise(conf = mean(confidence))

t.test(wideConf$conf, narrowDF$conf, paired = TRUE, alternative = "two.sided") #DIFF
#difference in confidence in bin1 for wide and narrow confidence reports
#more confident in narrow responses in bin1 than wide response 
#but 1pp didn't respond as though it was narrow ever in bin 1

#look medians instead of means to make sure outliers aren't pushing effects. 
wideConf <- wideResponse %>%
  group_by(PID)%>% 
  summarise(confWide = median(confidence))

narrowConf <- narrowResponse %>%
  group_by(PID)%>%
  summarise(confNarrow = median(confidence))
#some people haven't thought it was a narrow trial at all in this bin
extra <- c( 24)
extra <-as.data.frame(extra)
extra$conf <- c("NA")
extra <- extra %>% 
  rename(
    PID = extra,
    conf = conf)
narrow <- rbind(narrowConf, extra)
narrowDF <- arrange(narrow, PID)
narrowDF$conf <- as.numeric(narrowDF$conf)
narrowDF$distribution <- "narrow"

t.test(wideConf$confWide, narrowDF$conf, paired = TRUE, alternative = "two.sided") #also sig 
mean(wideConf$confWide) #50.7
mean(narrowConf$confNarrow) #56.5
#less confidence in bin1 when responding wide than narrow


#BIN 6: look at only correct trials in bin 6 - diffs for wide and narrow?
bin6 <- subset(confidence, distanceBin == 6  & correct ==1)

bin6W <- subset(bin6, distribution_name == "wide")
mean(bin6W$confidence) #84.7
bin6_conf <- bin6W %>%
  group_by(PID)%>%
  summarise(meanConfW = mean(confidence))

bin6N <- subset(bin6, distribution_name == "narrow")
mean(bin6N$confidence) #84.1
bin6_N_conf <- bin6N %>%
  group_by(PID)%>%
  summarise(meanConfN = mean(confidence))
bin6_conf$meanConfN <- bin6_N_conf$meanConfN

t.test(bin6_conf$meanConfN, bin6_conf$meanConfW, paired = TRUE, alternative = "two.sided") #no diff
mean(bin6_conf$meanConfN) #84.2
mean(bin6_conf$meanConfW) #84.2

#look medians instead of means to make sure outliers aren't pushing effects. 
bin6_conf <- bin6W %>%
  group_by(PID)%>%
  summarise(medConfW = median(confidence))

bin6_N_conf <- bin6N %>%
  group_by(PID)%>%
  summarise(medConfN = median(confidence))
bin6_conf$medConfN <- bin6_N_conf$medConfN
t.test(bin6_conf$medConfN, bin6_conf$medConfW, paired = TRUE, alternative = "two.sided") #no diff

############################################################################
#per PID - for 11 bins, what is the likelihood of judging wide or narrow 
#use $binnedLocation, bin 6 is middle bin (flipped data so narrow on left, wide on right)

#(take responses to infer judgements)
mask <- confidence$stimulus == "orange1" & confidence$distribution_name == "wide" | 
  confidence$stimulus == "blue1" & confidence$distribution_name == "narrow"
confidence$blocktype[mask] <- 0 #when zap1 think it's narrow, when retrieve0 think it's wide
narrowResponseZap <- subset(confidence, blocktype == 0 & button == 1)
narrowResponseZap$Response <- "narrow"
wideResponseRetrieve <- subset(confidence, blocktype == 0 & button == 0)
wideResponseRetrieve$Response <- "wide"

mask <- confidence$stimulus == "orange1" & confidence$distribution_name == "narrow" | 
  confidence$stimulus == "blue1" & confidence$distribution_name == "wide"
confidence$blocktype[mask] <- 1 #when zap1 think it's wide, when retrieve0 think it's narrow
narrowResponseRetrieve <- subset(confidence, blocktype == 1 & button == 0)
narrowResponseRetrieve$Response <- "narrow"
wideResponseZap <- subset(confidence, blocktype == 1 & button == 1)
wideResponseZap$Response <- "wide"

wideResponse <- rbind(wideResponseRetrieve, wideResponseZap)
narrowResponse <- rbind(narrowResponseRetrieve, narrowResponseZap)
Response <- rbind(wideResponse, narrowResponse) #this is the response based data to work with

#where are they responding more as though it is narrow or wide? (on all trials) 
responseLocations  <- ggplot(data=Response, aes(binnedLocation, fill= Response)) + 
  geom_histogram(col=I("navy")) +
  labs(title="Histogram for Responses", x="Location Bin", y="Frequency")
responseLocations
#look at individual splits
responseLocationsPID  <- ggplot(data=Response, aes(binnedLocation, fill= Response)) + 
  geom_histogram(col=I("navy")) +
  labs(title="Histogram for Responses", x="Location Bin", y="Frequency") +
  facet_wrap( ~ PID)
responseLocationsPID # starting to think about individual flip points, subjective boundaries. 

#when wide is good versus bad how are they interpreting things (midbin as being wide on avg?)
#blocktype 1 wide is bad, block type 0 wide is good
wideGood <- subset(Response, blocktype == 0)
wideGoodPlot  <- ggplot(data=wideGood, aes(binnedLocation, fill= Response)) + 
  geom_histogram(col=I("navy")) +
  labs(title="Histogram for Responses Wide = Retrieve", x="Location Bin", y="Frequency")
wideGoodPlot 
#more conservative to say trial is wide in midbin 
#(pull narrow "wider" and respond for wide in the midbin to be safer) 

wideBad <- subset(Response, blocktype == 1)
wideBadPlot  <- ggplot(data=wideBad, aes(binnedLocation, fill= Response)) + 
  geom_histogram(col=I("navy")) +
  labs(title="Histogram for Responses Wide = Zap", x="Location Bin", y="Frequency")
wideBadPlot #notice the shift, more wide repsonses (more zaps because wide bad)

#average accuracy split by distribution across bins - think about 11 bins? 
accuracyWide <- wideResponse %>%
  group_by(binnedLocation)%>%
  summarise(meanAccuracy = mean(correct))
accuracyWide$distribution <- "wide"

accuracyNarrow <- narrowResponse %>%
  group_by(binnedLocation)%>%
  summarise(meanAccuracy = mean(correct))
accuracyNarrow$distribution <- "narrow"

accuracyBins <- rbind(accuracyWide, accuracyNarrow)


#confidence across the 11 bins 
#looking at correct responses only (across all 11 bins)
ResponseCorrect <- subset(Response, correct == 1) 
ggplot(ResponseCorrect, aes(factor(binnedLocation), y=confidence, fill=Response))+ 
  stat_summary(fun = mean, geom = "bar", position = position_dodge(1)) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", position = position_dodge(1), width=0.5) +
  scale_fill_manual(values=c("#E69F00","#999999")) +
  labs(title="Confidence across distance bins", x="Distance Bin", y="Mean Confidence")+
  ylim(0,100)

#all trials
ggplot(Response, aes(factor(binnedLocation), y=confidence, fill=Response))+ 
  stat_summary(fun = mean, geom = "bar", position = position_dodge(1)) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", position = position_dodge(1), width=0.5) +
  scale_fill_manual(values=c("#E69F00","#999999")) +
  labs(title="Response based confidence per distance bin", x="Distance Bin", y="Mean Confidence")+
  ylim(0,100)


############################################################################
#finding subjective decision boundary per participant
#the point at which they flip from responding wide for wide to narrow and vice versa
#look at individual splits

responseLocationsPID  #look again at this plot

#for each bin, set responses as a percentage (not as frequency)
Response$response <- 0 #narrow
mask <- Response$Response == "wide"
Response$response[mask] <- 1 #wide

percentResponseW <- Response %>%
  group_by(binnedLocation) %>%
  summarise(percentResp = mean(response))
percentResponseW$condition <- "wide"

percentResponseN <- Response %>%
  group_by(binnedLocation) %>%
  summarise(percentResp = 1 - mean(response))
percentResponseN$condition <- "narrow"

percentResponse <- rbind(percentResponseW, percentResponseN)
 
ggplot(percentResponse, aes(y=percentResp, x=binnedLocation, fill = condition)) + 
  geom_col()
ggplot(percentResponse, aes(binnedLocation,y=percentResp, fill = condition)) +
  geom_col(position="dodge")

#for each bin, for each pp - find the average y (% response) point in each bin
percentRW <- Response %>%
  group_by(binnedLocation, PID) %>%
  summarise(percentResp = mean(response))
percentRW$condition <- "wide"
percentRN <- Response %>%
  group_by(binnedLocation, PID) %>%
  summarise(percentResp = 1 - mean(response))
percentRN$condition <- "narrow"

pppbResponse <- rbind(percentRW, percentRN)

ggplot(pppbResponse, aes(x=binnedLocation, y=percentResp, fill = condition)) + 
  geom_col()+
  facet_wrap( ~ PID)

#for each bin, for each pp - find the average X (binned location) 
avgLocation <- Response %>%
  group_by(binnedLocation, PID) %>%
  summarise(avgLocation = mean(flippedLocation))

avgLocation$narrowResponse <- percentRN$percentResp
avgLocation$wideResponse <- percentRW$percentResp

ggplot(avgLocation, aes(x=avgLocation, y=narrowResponse)) + 
  geom_point() +
  facet_wrap( ~ PID)

ggplot(avgLocation, aes(x=avgLocation, y=wideResponse)) + 
  geom_point() +
  facet_wrap( ~ PID)

#pp midpoint is inferred cross over point towards the right: fit a sigmoid
x <- avgLocation$avgLocation
y <- avgLocation$wideResponse

# fitting code
df <- data.frame(x = x,
                 y = y)
a_start <- 1 #lupper
b_start <- 0.1 #slope
c_start <- 0  #lower
d_start <- 350 #mid point


fitmodel <- nls(y ~ I( c + ((a-c)/(1 + exp((-b/1) * (x-d))))), data = df, start=list(a=a_start, b=b_start, c=c_start, d=d_start)) 
params=coef(fitmodel)

# function needed for visualization purposes
sigmoid = function(params, x) {
  params[3] + ((params[1]-params[3]) / (1 + exp((-params[2]/1) * (x - params[4]))))
}
xlist <- 0:800
y2 <- sigmoid(params,xlist)
plot(y2,type="l") 
 

##########################################################
#now fit a sigmoid per person 
#3pp not fitting the data right now - update the start values? 
avgLocation <- subset(avgLocation, PID != 53)

PID <- (unique(avgLocation$PID))
PIDcount <- c(1:length(PID))
params <- matrix(data = NA, nrow = 4, ncol = length(PID))
y2 <- matrix(data = NA, nrow = 801, ncol = length(PID))

for (i in PIDcount){
  index <- PID[i]
  dataOfInterest <- subset(avgLocation, PID == index)
  x <- dataOfInterest$avgLocation
  y <- dataOfInterest$wideResponse
  
  df <- data.frame(x = x,
                 y = y)
  
  a_start <- 0.9 #upper asymptote
  b_start <- 0.01 #slope
  c_start <- 0.1 #lower asymptote
  d_start <- 400 #mid point
  
  fitmodel <- nls(y ~ I( c + ((a-c)/(1 + exp((-b/1) * (x-d))))), data = df, start=list(a=a_start, b=b_start, c=c_start, d=d_start)) 
  params[,i] <- coef(fitmodel)
  y2[,i] <- sigmoid(params[,i],xlist)
  plot(y2[,i],type="l", xlab = index)
}

#reshuffle y2 to plot using ggpplot
sigmoidData <- data.frame(confidence = y2[,1])
sigmoidData$PID <- 1


sigmoidData$halfX <- halfX[,1]
sigmoidData$xlist <- c(0:800)
newData <- data.frame(confidence = y2[,2])

PID <- PID[2:40]
PIDcount <- c(1:length(PID))
for (i in PIDcount){
  newData$confidence <- y2[,i+1]
  newData$PID <- PID[i]
  newData$halfX <- halfX[,i+1]
  newData$xlist <- c(0:800)
  sigmoidData <- rbind(sigmoidData, newData)
}
ggplot(sigmoidData, aes(x=xlist, y=confidence)) + 
  geom_point() +
  facet_wrap( ~ PID)


#halfX is the inferred cross over point, if this is "towards the right" that means a bias to the wide
#would change confidence in bins 2,3,4 - closer to bound and yet still more confidence
#do people with a "truer" distribution represntation have a biger confidence difference?

crossOver <- data.frame(cross <-unique(sigmoidData$halfX))
crossOver$PID <- PID

############################################################################
#create an excel spreadsheet for Nick to doublecheck values
PID <- c(1:58)
conf <- data_frame(PID)

bin1N <- subset(correctConfN, distanceBin ==1)
bin1<- bin1N %>% group_by(PID) %>% summarise(confidence = mean(confidence))
bin2N <- subset(correctConfN, distanceBin ==2)
conf$Narrow_2 <- bin2N %>% group_by(PID) %>% summarise(confidence = mean(confidence))
bin3N <- subset(correctConfN, distanceBin ==3)
conf$Narrow_3 <- bin3N %>% group_by(PID) %>% summarise(confidence = mean(confidence))
bin4N <- subset(correctConfN, distanceBin ==4)
conf$Narrow_4 <- bin4N %>% group_by(PID) %>% summarise(confidence = mean(confidence))
bin5N <- subset(correctConfN, distanceBin ==5)
conf$Narrow_5 <- bin5N %>% group_by(PID) %>% summarise(confidence = mean(confidence))
bin6N <- subset(correctConfN, distanceBin ==6)
conf$Narrow_6 <- bin6N %>% group_by(PID) %>% summarise(confidence = mean(confidence))

bin1W <- subset(correctConfW, distanceBin ==1)
conf$Wide_1<- bin1W %>% group_by(PID) %>% summarise(confidence = mean(confidence))
bin2W <- subset(correctConfW, distanceBin ==2)
conf$Wide_2 <- bin2W %>% group_by(PID) %>% summarise(confidence = mean(confidence))
bin3W <- subset(correctConfW, distanceBin ==3)
conf$Wide_3 <- bin3W %>% group_by(PID) %>% summarise(confidence = mean(confidence))
bin4W <- subset(correctConfW, distanceBin ==4)
conf$Wide_4 <- bin4W %>% group_by(PID) %>% summarise(confidence = mean(confidence))
bin5W <- subset(correctConfW, distanceBin ==5)
conf$Wide_5 <- bin5W %>% group_by(PID) %>% summarise(confidence = mean(confidence))
bin6W <- subset(correctConfW, distanceBin ==6)
conf$Wide_6 <- bin6W %>% group_by(PID) %>% summarise(confidence = mean(confidence))


#adjust bin1 values 
bin1PID <- bin1N[,1] #missing PID 1, 15 and 24 
missing <- c(1, 15, 24)
bin1conf <- rbind(bin1, c(1, "NA"))
bin1conf <- rbind(bin1conf, c(15, "NA"))
bin1conf <- rbind(bin1conf, c(24, "NA"))
bin1conf$PID <- as.numeric(bin1conf$PID)
bin1conf <- arrange(bin1conf, PID)

bin1conf$N2 <- conf$Narrow_2[,2]
confidence_SPSS <- data_frame(PID)
confidence_SPSS$N1 <- bin1conf$confidence
confidence_SPSS$N2 <- pull(conf$Narrow_2[,2])
confidence_SPSS$N3 <- pull(conf$Narrow_3[,2])
confidence_SPSS$N4 <- pull(conf$Narrow_4[,2])
confidence_SPSS$N5 <- pull(conf$Narrow_5[,2])
confidence_SPSS$N6 <- pull(conf$Narrow_6[,2])
confidence_SPSS$W1 <- pull(conf$Wide_1[,2])
confidence_SPSS$W2 <- pull(conf$Wide_2[,2])
confidence_SPSS$W3 <- pull(conf$Wide_3[,2])
confidence_SPSS$W4 <- pull(conf$Wide_4[,2])
confidence_SPSS$W5 <- pull(conf$Wide_5[,2])
confidence_SPSS$W6 <- pull(conf$Wide_6[,2])

library(writexl)
write_xlsx(confidence_SPSS,"C:/Users/saraha/Desktop/Pilot_Data_V2.1/confidence_SPSS.xlsx")

############################################################################
mean(halfX)
