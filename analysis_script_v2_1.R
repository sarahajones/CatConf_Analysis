#Analysis Script - CatConf Behavioural Data, Study - V2.1. 
#behavioural analysis of pilot study
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
#PARTICIPANT COMMENTS
#check on the recorded comments

feedback_strat <- subset(dat_csv, feedback_strategy != 'NA')
feedback_strat <- feedback_strat$feedback_strategy
feedback_strat
#seem to follow sensible strategies - little too much left V right maybe? 

feedback_tech <- subset(dat_csv, feedback_technical != 'NA')
feedback_tech <- feedback_tech$feedback_technical
feedback_tech
#no tech issues reported 

feedback_thought <- subset(dat_csv, feedback_comments != 'NA')
feedback_thought <- feedback_thought$feedback_comments
feedback_thought
# enjoyed/positive

########################################################
#apply exclusion criteria here


########################################################
#EXPERIMENTAL INFO
#how long did the experiment take on average to complete?
lastTrials <- subset(dat_csv, trial_index > 500)
minutesTaken <- ((lastTrials$time_elapsed/60)/1000)
averageTime <- summary(minutesTaken) 
averageTime
sd(minutesTaken)
#mean 34, range - 23-65
timeout <- subset(dat_csv, time_elapsed > 7000000)
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

#what handedness do we have?
handData <- subset(dat_csv, participantHandedness != 'NA')
handedness <- handData$participantHandedness
right <- sum(handedness == "right") #54 right, 5 left, 1 ambi

#########################################################################
#QUICKFIRE ROUNDS
#look at quickfire rounds for accuracy rates(did the participants learn a pairing) 
quickfireData <- subset(dat_csv, trial_type == 'jspsych-quickfire') #1200 trials across 60 pp
trials <- c(1:1200)
for (i in trials){
  if(quickfireData$button[i] == "null"){
    quickfireData$correct[i] = 0
    quickfireData$incorrect[i] =1 
  }} #reset NA values

#feedback = images/coins.png, or images/bomb.png; button 0 = Retrieve, button 1 = Zap
correctCoins <- subset(quickfireData, quickfireData$feedback == 'images/coins.png' & quickfireData$button == 0)
correctBombs <- subset(quickfireData, quickfireData$feedback == 'images/bomb.png' & quickfireData$button == 1)
correctQuickfire <- rbind(correctBombs, correctCoins)
quickfireTrials <- 20
quickfireAccuracy <- ((nrow(correctQuickfire)/(quickfireTrials*numParticipants))*100) 
# mean accuracy in quickfire is 89.5%


#per participant?
PIDwiseCorrectQuickfire <- quickfireData %>% 
  group_by(PID) %>%
  summarise(mean = mean(correct))
PIDwiseQuickfireAccuracy <- ggplot(data = PIDwiseCorrectQuickfire) +
  geom_point(mapping = aes(x = PID, y = mean)) +
  labs(title="Participant Quickfire Accuracy", x="PID", y="Accuracy") 
PIDwiseQuickfireAccuracy + xlim(1, length(completeParticipants))+ ylim(0, 1)
#looks pretty good - only 2 below 75 and none at chance

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
#2 pp of concern? 
#3 below average for second half - pp10, p21 and pp 18?
#other at chance in second half? pp41




#########################################################################
#TRAINING FASTDROP "CLEAR DAY" TRIALS 
#check up on distribution properties. 
#calculate the mean and variance for each and see if it maps across to the distributions
mask <- dat_csv$distribution_variance == 6400 & dat_csv$button == "null" & dat_csv$distribution_mean == 222
summary(dat_csv$drop_location[mask]) #mean == 222.2 (close to 222)
sd(dat_csv$drop_location[mask], na.rm = TRUE)#78.7 (close to 80)

mask <- dat_csv$distribution_variance == 6400 & dat_csv$button == "null" & dat_csv$distribution_mean == 578
summary(dat_csv$drop_location[mask]) #mean == 577.7 (close to 578)
sd(dat_csv$drop_location[mask],na.rm = TRUE)#78.6 (close to 80)

mask <- dat_csv$distribution_variance == 19600 & dat_csv$button == "null" & dat_csv$distribution_mean == 278
summary(dat_csv$drop_location[mask]) #mean == 278.5 (close to 578)
sd(dat_csv$drop_location[mask],na.rm = TRUE)#137.4 (close to 140)

mask <- dat_csv$distribution_variance == 19600 & dat_csv$button == "null" & dat_csv$distribution_mean == 522
summary(dat_csv$drop_location[mask]) #mean == 521.5 (close to 522)
sd(dat_csv$drop_location[mask], na.rm = TRUE)#137.4 (close to 140)
#mean and sd working well using the force function to generate trials. 

#####################################################################################
#DATA TRANSFORMATION - flip trials around their midpoints and realign on 350 
clearData <- subset(dat_csv, block <= 0) #subset data to focus on training trials
clearData$flippedLocation <- clearData$drop_location #set default locations
clearData$boundary <- 350 #set default boundary
clearData$distance_to_bound <- abs(clearData$drop_location - clearData$boundary) #set default distance to bound
clearData$recentredFlipLocation <- clearData$flippedLocation #set default recentred location

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

mask <- clearData$boundary == 450
clearData$recentredFlipLocation[mask] <- clearData$flippedLocation[mask] - 100


#plot out 1 pp for each training block to get better idea of the information 1 pp had before each block. 
clearDataPP1 <- subset(clearData, PID == 1)
ppClearLocations  <- ggplot(data=clearDataPP1, aes(drop_location, fill= distribution_name)) + 
  geom_histogram(col=I("navy")) +
  labs(title="Histogram for Training Locations", x="Drop location", y="Frequency") +
  facet_wrap( ~ block)
ppClearLocations #original values

ppClearLocations  <- ggplot(data=clearDataPP1, aes(flippedLocation, fill= distribution_name)) + 
  geom_histogram(col=I("navy")) +
  labs(title="Histogram for Training Locations", x="Flipped location", y="Frequency") +
  facet_wrap( ~ block)
ppClearLocations #flipped around midlines

ppClearLocations  <- ggplot(data=clearDataPP1, aes(recentredFlipLocation, fill= distribution_name)) + 
  geom_histogram(col=I("navy")) +
  labs(title="Histogram for Training Locations", x="Flipped (recentred) location", y="Frequency") +
  facet_wrap( ~ block)
ppClearLocations #recentred on 350

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
  labs(title="Density Plot for (flipped) Training Locations", x="Drop location (flipped)", y="Density")
# means and midline looks spot on

#########################################################################
#TEST "CLOUDY DAY" TRIALS
#accuracy - above chance? 
unclearData <- subset(dat_csv, block > 0)
unclearTrials <- 60
blockNum <- 4

unclearAccuracy <- (nrow(subset(unclearData, correct == 1))/(unclearTrials*blockNum*numParticipants)*100) 
#80.5% accuracy

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
  labs(title="Blockwise Participant Accuracy", x="PID", y="Accuracy") 
blockPIDwideCloudAccuracyPlot + xlim(1,numParticipants) + ylim(0, 1)
#overall accuracy high across participants and blocks
#participants 10/21/58 look low across all blocks 

#get an average pp accuracy across all blocks. 

testAccuracy <- blockPIDwideCloudAccuracy %>%
  group_by(PID)%>%
  summarise(accuracyMean = mean(grp.mean))

testAccuracy$quickfireAccuracy <- 

#reaction time
unclearData$RT <- as.numeric(as.character(unclearData$delta_response_time))
unclearData <- subset(unclearData, RT != 'NA')

unclearCorrect <- subset(unclearData, correct == 1)
unclearIncorrect <- subset(unclearData, incorrect == 1)
avgCorrectRT <- mean(unclearCorrect$RT) #1795
avgIncorrectRT <- mean(unclearIncorrect$RT) #1928

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
  ylab = "Reaction Time (ms)", xlab = "Mean RT within participants - split by accuracy", add = "jitter"
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

#bin the distance according to the way it was generated in the experiment
confidence$rawLocation <- confidence$drop_location
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

t.test(confCompareRT$grp.mean, confCompareRT$group, paired = TRUE, alternative = "two.sided")
#difference in the reaction times for confidence reports for correct and incorrect trials


#########################################################################
#DISTANCE FROM BOUND
#ACCURACY
accuracyDist <- confidence %>% 
  group_by(distanceBin) %>%
  summarise(grp.mean = mean(correct))
accuracyDist$accuracy <- accuracyDist$grp.mean

a <- ggplot(data=accuracyDist, aes(distanceBin, accuracy))+ geom_point()
a

#CONFIDENCE
confDist <- confidence %>% 
  group_by(distanceBin) %>%
  summarise(grp.mean = mean(confidence))
confDist$confidence <- confDist$grp.mean

a <- ggplot(data=confDist, aes(distanceBin, confidence))+ geom_point()
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
confAOV <- aov_ez(id="PID", dv="grp.mean", data=confidenceAOV, within = c("distanceBin", "distribution"))
confAOV
#check the pairwise comparisons
int_comp <- emmeans(confAOV, ~distribution | distanceBin)
pairs(int_comp,adjust="none")


