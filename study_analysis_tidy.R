#Analysis Script - CatConf Behavioural Data, Full Study. 
#########################################################################
#load in packages
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


#########################################################################
#DATA LOADING, CLEANING, AND TIDYING
mydir = setwd("C:/Users/saraha/Desktop/CatConfStudy1 Data")
myfiles = list.files(path=mydir, pattern="zapBox_v0.1.0_*", full.names=TRUE) #pull all files
dat_csv = ldply(myfiles, read_csv) #load in data

#create a PID code to identify participants more clearly. 
user_ids <- unique(dat_csv$user_id) # cache this to save recalculating 
dat_csv$PID <- NA_real_ # initalise PIDs as NA
dat_csv$PID <- sapply(
  dat_csv$user_id,  # vector to iterate over
  function(id) which(user_ids == id)  # function to apply to each element
)

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

#EXPERIMENTAL INFO
#how long did the experiment take on average to complete?
lastTrials <- subset(dat_csv, trial_index > 438)
minutesTaken <- ((lastTrials$time_elapsed/60)/1000)
averageTime <- summary(minutesTaken)
#check if slow people took break or were just slow?

#DEMOGRAPHICS
#what is the age, mean, range?
ageData <- subset(dat_csv, participantAge != "NA")
summary(ageData$participantAge)
range(ageData$participantAge) #18-60
sd(ageData$participantAge) #11.38

#what is the gender split?
genderData <- subset(dat_csv, participantGender != 'NA')
gender <- genderData$participantGender #17 female 23 male
female <- gender == 'female'

#what handedness do we have?
handData <- subset(dat_csv, participantHandedness != 'NA')
handedness <- handData$participantHandedness
right <- handedness == "right" #6 left handed participants

#########################################################################
#DATA TRANSFORMATION
#create flipped drop locations for wide and narrow
dat_csv$flippedLocation <- dat_csv$drop_location
dat_csv$distribution <- "wide"

mask <- dat_csv$distribution_variance == 10000 & dat_csv$distribution_mean == 200 & !is.na(dat_csv$drop_location)
dat_csv$flippedLocation[mask] <- dat_csv$boundary[mask] + dat_csv$distance_to_bound[mask]
mask <- dat_csv$distribution_variance == 5000 & dat_csv$distribution_mean == 500 & !is.na(dat_csv$drop_location)
dat_csv$flippedLocation[mask] <- dat_csv$boundary[mask] - dat_csv$distance_to_bound[mask]
dat_csv$distribution[mask] <- "narrow"
mask <- dat_csv$distribution_variance == 5000 & dat_csv$distribution_mean == 200 & !is.na(dat_csv$drop_location)
dat_csv$distribution[mask] <- "narrow"
#dat_csv <- subset(dat_csv, flippedLocation > 0 & drop_location != "NA") #exclude those cases where the value is now neg

#check up on distribution properities. 
#calculate the mean and variance for each and see if it maps across to the distributions
mask <- dat_csv$distribution_variance == 5000
summary(dat_csv$flippedLocation[mask])
sd(dat_csv$flippedLocation[mask])#68.6 close to 71
#mean and sd unchanged by transformation and approximate values required. 

#########################################################################
#QUICKFIRE ROUNDS
#look at quickfire rounds for accuracy rates (did the participants learn a pairing) 
quickfireData <- subset(dat_csv, trial_type == 'jspsych-quickfire') #200 trials across 40 pp
quickfireTrials <- 20
correctQuickfire <- (subset(quickfireData, correct == 1)) 
quickfireAccuracy <- ((nrow(correctQuickfire)/(quickfireTrials*numParticipants))*100) #92.875

#per participant?
PIDwiseCorrectQuickfire <- quickfireData %>% 
  group_by(PID) %>%
  summarise(grp.mean = mean(correct))
PIDwiseQuickfireAccuracy <- ggplot(data = PIDwiseCorrectQuickfire) +
  geom_point(mapping = aes(x = PID, y = grp.mean)) +
  labs(title="Participant Quickfire Accuracy", x="PID", y="Accuracy") 
PIDwiseQuickfireAccuracy + xlim(0, 40)+ ylim(0, 1)

#split half
#trialsOfInterest <- range(quickfireData$trial_index) #9-28
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
PIDwiseSplitHalfQuickfireAccuracy + xlim(1,40) + ylim(0, 1)

#########################################################################
#TRAINING FASTDROP "CLEAR DAY" TRIALS 
clearData <- subset(dat_csv, block <= 0) #subset data to focus on training trials
clearDataPP1 <- subset(clearData, PID == 1)

#plot out 1 pp for each training block to get better idea of the information 1 pp had before each block. 
ppClearLocations  <- ggplot(data=clearDataPP1, aes(flippedLocation, fill= distribution)) + 
  geom_histogram(col=I("navy")) +
  labs(title="Histogram for Training Locations", x="Drop location", y="Frequency") +
  facet_wrap( ~ block)
ppClearLocations

#density plots
a <- ggplot(data=clearData, aes(clearData$flippedLocation))
a + geom_density(aes(color = distribution)) +
  scale_color_manual(values = c("#868686FF", "#EFC000FF"))
mu <- clearData %>% 
  group_by(distribution) %>%
  summarise(grp.mean = mean(flippedLocation))
a + geom_density(aes(fill = distribution), alpha = 0.4) +
  geom_vline(aes(xintercept = grp.mean, color = distribution),
             data = mu, linetype = "dashed") +
  xlim(0,800)+
  scale_color_manual(values = c("#868686FF", "#EFC000FF"))+
  scale_fill_manual(values = c("#868686FF", "#EFC000FF")) +
  labs(title="Density Plot for (flipped) Training Locations", x="Drop location (flipped)", y="Density")

#########################################################################
#TEST "CLOUDY DAY" TRIALS
#accuracy - above chance? 
unclearData <- subset(dat_csv, block > 0)
unclearTrials <- 46
blockNum <- 4
unclearAccuracy <- (nrow(subset(unclearData, correct == 1))/(unclearTrials*blockNum*numParticipants)*100) #89% accuracy
unclearData<- subset(unclearData, flippedLocation > 0) #exclude those cases where the value is now neg (7)

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
blockPIDwideCloudAccuracyPlot + xlim(1,40) + ylim(0, 1)

#reaction time
unclearData$RT <- as.numeric(as.character(unclearData$delta_response_time))
unclearData <- subset(unclearData, RT != 'NA')

unclearCorrect <- subset(unclearData, correct == 1)
unclearIncorrect <- subset(unclearData, incorrect == 1)
avgCorrectRT <- mean(unclearCorrect$RT) #1634.137
avgIncorrectRT <- mean(unclearIncorrect$RT) #2103.023

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

#########################################################################
#CONFIDENCE TRIALS 
confidence <- subset(dat_csv, confidence != 'null') #remove missing confidence reports
confidence$confidence <- as.numeric(as.character(confidence$confidence))
confidence$confidenceRT <- as.numeric(as.character(confidence$delta_confidence_response_time))

#bin distance to boundary into 5 bins
confidence$binnedDistance <- NA_real_ # initalise distance bins as NA
breaks <- quantile(confidence$distance_to_bound, seq(0, 1, by = 0.20))
confidence$binnedDistance <- cut(confidence$distance_to_bound, breaks, include.lowest = TRUE, labels = c("1", "2", "3", "4", "5"))

#bin confidence into 3 bins (more values not possible with this dataset)
confidence$binnedConfidence <- NA_real_ # initalise distance bins as NA
breaks <- quantile(confidence$confidence, seq(0, 1, by = 0.33))
confidence$binnedConfidence <- cut(confidence$confidence, breaks, include.lowest = TRUE, labels = c("1", "2", "3"))

# plot out raw confidence reports as a density.... 
a <- ggplot(data=confidence, aes(confidence))
a + geom_density(aes(color = distribution)) +
  scale_color_manual(values = c("#868686FF", "#EFC000FF"))
a + geom_density(aes(fill = distribution), alpha = 0.4) +
  geom_vline(aes(xintercept = grp.mean, color = distribution),
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

#########################################################################
#CONFIDENCE AND DISTANCE FROM BOUND
#Across all trials - looking for sensible ratings, overlaid accuracy + split by distribution
rawConfidenceAccuracy = ggplot(data = confidence) +
  geom_point(mapping = aes(x = distance_to_bound, y = confidence, color = correct))+
  labs(title="Confidence as a function of distance to bound", x="Distance from Category Bound", y="Raw Confidence")+
  facet_wrap(~distribution)
rawConfidenceAccuracy +xlim(0, 400)

#take average at each distance point... split by distribution
wideConf <- subset(confidence, distribution == "wide")
avgedWideConf <- wideConf%>% 
  group_by(confidence) %>%
  summarise(grp.mean = mean(distance_to_bound))
avgedWideConf$distribution <- "wide"
narrowConf <- subset(confidence, distribution == "narrow")
avgedNarrowConf <- narrowConf%>% 
  group_by(confidence) %>%
  summarise(grp.mean = mean(distance_to_bound))
avgedNarrowConf$distribution <- "narrow"
avgedWideNarrow <-rbind(avgedWideConf, avgedNarrowConf)

avgedConfidence = ggplot(data = avgedWideNarrow) +
  geom_point(mapping = aes(x = grp.mean, y = confidence, color = distribution))
avgedConfidence

#binned distance, average out confidence per bin
correctConfPerDistanceBin <- correctConfidence%>% 
  group_by(binnedDistance) %>%
  summarise(grp.mean = mean(confidence))
correctConfPerDistanceBin$correct <- 1
incorrectConfPerDistanceBin <- incorrectConfidence%>% 
  group_by(binnedDistance) %>%
  summarise(grp.mean = mean(confidence))
incorrectConfPerDistanceBin$correct <- 0
accuracyConfidence <- rbind(correctConfPerDistanceBin, incorrectConfPerDistanceBin)

binnedAvgConfidenceByDistanceByAccuracy = ggplot(data = accuracyConfidence) +
  geom_point(mapping = aes(x = binnedDistance, y = grp.mean, color = correct))+
  labs(title="Average confidence as a function of distance to bound (binned)", x="Distance from Category Bound (binned)", y="Average Confidence")
binnedAvgConfidenceByDistanceByAccuracy +ylim(0,100)

#stacked bar graph split by distribution
stackedBarChart<- ggplot(confidence, aes(x = binnedDistance, y = confidence))+
  geom_col(aes(fill = binnedConfidence), width = 0.7) +
  scale_color_viridis(discrete = TRUE, option = "D")+
  scale_fill_viridis(discrete = TRUE) +
  labs(title="Confidence (Binned) & Distance to Bound (Binned)", x="Binned Distance from Category Bound", y="Frequency")+
  facet_wrap(~distribution)
stackedBarChart 

#find average confidence for each binned point on flipped locations
confidence$binnedFlippedLocation <- NA_real_ # initalise distance bins as NA
breaks <- c(0, 50, 100, 150, 200, 250, 300, 350, 400, 450, 500, 550, 600, 650, 700, 750, 800)
confidence$binnedFlippedLocation <- cut(confidence$flippedLocation, breaks, include.lowest = TRUE, labels = c("25", "75", "125", "175", "225", "275", "325", "375", "425", "475", "525", "575", "625", "675", "725", "775"))
confidence$locationbins <- as.numeric(cut(confidence$flippedLocation, breaks, include.lowest = TRUE, labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16")))

confidencePerDistanceBin <- confidence %>% 
  group_by(binnedFlippedLocation) %>%
  summarise(grp.mean = mean(confidence), .groups = "drop") %>%
  mutate(x = as.numeric(as.character(binnedFlippedLocation)), y = grp.mean/1e4)

ggplot(data=confidence, aes(x=flippedLocation)) + 
  geom_density(aes(fill = distribution), alpha = 0.4) +
  geom_vline(aes(xintercept = grp.mean, color = distribution),
             data = mu, linetype = "dashed") +
  geom_point(mapping = aes(x = x, y = y), data = confidencePerDistanceBin) +
  scale_color_manual(values = c("#868686FF", "#EFC000FF"))+
  scale_fill_manual(values = c("#868686FF", "#EFC000FF")) +
  scale_y_continuous(sec.axis = sec_axis(trans=~ .*1e4, name="Confidence")) +
  labs(title="Density Plot for Location (flipped) overlaid with Average Confidence per Location", x="Raw Location", y="Density")

#########################################################################
#run anova: confidence per distance bin by distribution
#set up avg confidence per distance bin, PID and distribution
narrow <- subset(confidence, distribution == "narrow")
narrow1 <-subset(narrow, binnedDistance == 1)
narrow1 <- data.frame( PID = c(1:40), binnedDistance = 1, confidence = tapply(narrow1$confidence, narrow1$PID, mean), distribution = "narrow")
narrow2 <-subset(narrow, binnedDistance == 2)
narrow2 <- data.frame( PID = c(1:40), binnedDistance = 2, confidence = tapply(narrow2$confidence, narrow2$PID, mean),distribution = "narrow")
narrow3 <-subset(narrow, binnedDistance == 3)
narrow3 <- data.frame( PID = c(1:40), binnedDistance = 3, confidence = tapply(narrow3$confidence, narrow3$PID, mean),distribution = "narrow")
narrow4 <-subset(narrow, binnedDistance == 4)
narrow4 <- data.frame( PID = c(1:40), binnedDistance = 4, confidence = tapply(narrow4$confidence, narrow4$PID, mean),distribution = "narrow")
narrow5 <-subset(narrow, binnedDistance == 5)
narrow5 <- data.frame( PID = c(1:40), binnedDistance = 5, confidence = tapply(narrow5$confidence, narrow5$PID, mean),distribution = "narrow")
narrow <- rbind (narrow1, narrow2, narrow3, narrow4, narrow5)

wide <- subset(confidence, distribution == "wide")
wide1 <-subset(wide, binnedDistance == 1)
wide1 <- data.frame( PID = c(1:40), binnedDistance = 1, confidence = tapply(wide1$confidence, wide1$PID, mean), distribution = "wide")
wide2 <-subset(wide, binnedDistance == 2)
wide2 <- data.frame( PID = c(1:40), binnedDistance = 2, confidence = tapply(wide2$confidence, wide2$PID, mean),distribution = "wide")
wide3 <-subset(wide, binnedDistance == 3)
wide3 <- data.frame( PID = c(1:40), binnedDistance = 3, confidence = tapply(wide3$confidence, wide3$PID, mean),distribution = "wide")
wide4 <-subset(wide, binnedDistance == 4)
wide4 <- data.frame( PID = c(1:40), binnedDistance = 4, confidence = tapply(wide4$confidence, wide4$PID, mean),distribution = "wide")
wide5 <-subset(wide, binnedDistance == 5)
wide5 <- data.frame( PID = c(1:40), binnedDistance = 5, confidence = tapply(wide5$confidence, wide5$PID, mean),distribution = "wide")
wide <- rbind (wide1, wide2, wide3, wide4, wide5)

my_data <- rbind(narrow, wide)

#convert distribution and bins to factors and recode levels
my_data$distribution <- factor(my_data$distribution, 
                               levels = c("wide", "narrow"),
                               labels = c("wide", "narrow"))
my_data$binnedDistance <- factor(my_data$binnedDistance, 
                                 levels = c("1", "2", "3", "4", "5"),
                                 labels = c("1", "2", "3", "4", "5"))

table(my_data$binnedDistance, my_data$distribution) #balanced design

my_data$PID <- factor(my_data$PID)

ggboxplot(my_data, x = "binnedDistance", y = "confidence", color = "distribution")+
  labs(title="Confidence per distance bin - split by distribution", x="Binned Distance", y="Within-Participant Average Confidence")

#Run a two-way ANOVA with interaction effect
aov <- anova_test(data = my_data, dv = confidence, wid = PID, within = c(distribution, binnedDistance))
aov

#########################################################################
#CHOICE BASED CONFIDENCE
wideOrange <- subset(confidence, distribution == "wide" & spaceship_class == "orange1")
wideBlue <- subset(confidence, distribution == "wide" & spaceship_class == "blue1")
narrowOrange <- subset(confidence, distribution == "narrow" & spaceship_class == "orange1")
narrowBlue <- subset(confidence, distribution == "narrow" & spaceship_class == "blue1")

wideOrangeNarrowBlue <- rbind(wideOrange, narrowBlue) #will respond 0 for wide and 1 for narrow
wideBlueNarrowOrange <- rbind(wideBlue, narrowOrange) #will respond 1 for wide and 0 for narrow

wideWONB <-subset(wideOrangeNarrowBlue, button == 0) #when they thought it was wide
narrowWONB <-subset(wideOrangeNarrowBlue, button == 1)#when they thought it was narrow
wideWBNO <-subset(wideBlueNarrowOrange, button ==1)#when they thought it was wide
narrowWBNO <- subset(wideBlueNarrowOrange, button ==0)#when they thought it was narrow

wideResponse <- rbind(wideWONB, wideWBNO)
narrowResponse <- rbind(narrowWONB, narrowWBNO)

confidenceWidePerDistanceBin <- wideResponse  %>% 
  group_by(binnedFlippedLocation) %>%
  summarise(grp.mean = mean(confidence), .groups = "drop") %>%
  mutate(x = as.numeric(as.character(binnedFlippedLocation)), y = grp.mean/1e4)
confidenceWidePerDistanceBin$choice <- "wide"

confidenceNarrowPerDistanceBin <- narrowResponse  %>% 
  group_by(binnedFlippedLocation) %>%
  summarise(grp.mean = mean(confidence), .groups = "drop") %>%
  mutate(x = as.numeric(as.character(binnedFlippedLocation)), y = grp.mean/1e4)
confidenceNarrowPerDistanceBin$choice <- "narrow"

confidenceChoiceDistanceBin <- rbind(confidenceWidePerDistanceBin , confidenceNarrowPerDistanceBin)

ggplot(data=confidence, aes(x=flippedLocation)) + 
  geom_density(aes(fill = distribution), alpha = 0.4) +
  geom_vline(aes(xintercept = grp.mean, color = distribution),
             data = mu, linetype = "dashed") +
  geom_point(mapping = aes(x = x, y = y), data = confidenceWidePerDistanceBin, color = "deepskyblue4") +
  geom_point(mapping = aes(x = x, y = y), data = confidenceNarrowPerDistanceBin) +
  scale_color_manual(values = c("#868686FF", "#EFC000FF"))+
  scale_fill_manual(values = c("#868686FF", "#EFC000FF")) +
  scale_y_continuous(sec.axis = sec_axis(trans=~ .*1e4, name="Confidence")) +
  labs(title="Density Plot for Location (flipped) overlaid with Average Confidence per Location", x="Raw Location", y="Density")

#hone in on values from 175 -525, that is the space between the means +1 either side
choiceConfidence <- subset(confidenceChoiceDistanceBin, x > 150 & x < 550)
choiceConfidenceWide <- subset(choiceConfidence, choice == "wide")
choiceConfidenceNarrow <- subset(choiceConfidence, choice == "narrow")
choiceConfidenceNarrow$xRev <- rev(choiceConfidenceNarrow$x)

ggplot(data=choiceConfidenceNarrow, aes(x=xRev, y=grp.mean)) + 
  geom_point(color = "black") +
  geom_point(data=choiceConfidenceWide, aes(x=x, y=grp.mean), color = "deepskyblue4")+
  labs(title="Overlaid average confidence based on choice", x="Location", y="Average Confidence")

###################################################################
#working out how to plot a regression linein choice confidence data
choiceConfidenceStats <- subset(confidenceChoiceDistanceBin, x > 150 & x < 550)
choiceConfidenceStatsWide <- subset(choiceConfidenceStats, choice == "wide")
choiceConfidenceStatsNarrow <- subset(choiceConfidenceStats, choice == "narrow")
choiceConfidenceStatsNarrow$x <- rev(choiceConfidenceStatsNarrow$x)
choiceConfidenceStats <- rbind(choiceConfidenceStatsNarrow, choiceConfidenceStatsWide)

my.formula <- y ~ x
ggplot(choiceConfidenceStats, aes(x=x, y=grp.mean, color = choice, linetype=choice)) + geom_point() +
  geom_smooth(method="lm", se=FALSE,  formula = my.formula) 

my.formula <- y ~ poly(x, 2)
ggplot(choiceConfidenceStats, aes(x=x, y=grp.mean, color = choice, linetype=choice)) + geom_point() +
  geom_smooth(method="lm", se=FALSE,  formula = my.formula)+geom_vline(xintercept = 350, linetype="dotted", color = "black") +
  geom_vline(xintercept = 350, linetype="dotted", color = "black") +
  geom_vline(xintercept = 500, color = "black") +
  geom_vline(xintercept = 200, color = "black") +
  labs(title="Average confidence based on choice per location bin", x="Location", y="Average Confidence")

my.formula <- y ~ poly(x, 3)
ggplot(choiceConfidenceStats, aes(x=x, y=grp.mean, color = choice, linetype=choice)) + geom_point() +
  geom_smooth(method="lm", se=FALSE,  formula = my.formula)+
  geom_vline(xintercept = 350, linetype="dotted", color = "black") +
  geom_vline(xintercept = 500, color = "black") +
  geom_vline(xintercept = 200, color = "black") +
  labs(title="Average confidence based on choice per location bin", x="Location", y="Average Confidence")

#play around with line fits and finding/using the coefficients of the lines
# get underlying plot
x <- choiceConfidenceStats$x
y <- choiceConfidenceStats$grp.mean
plot(x, y, pch=20)

# basic straight line of fit
fit <- glm(y~x)
co <- coef(fit)
abline(fit, col="blue", lwd=2)

# logarithmic
f <- function(x,a,b) {a * log(x) + b}
fit <- nls(y ~ f(x,a,b), start = c(a=1, b=1)) 
co <- coef(fit)
curve(f(x, a=co[1], b=co[2]), add = TRUE, col="orange", lwd=2) 

# polynomial
f <- function(x,a,b,d) {(a*x^2) + (b*x) + d}
fit <- nls(y ~ f(x,a,b,d), start = c(a=1, b=1, d=1)) 
co <- coef(fit)
curve(f(x, a=co[1], b=co[2], d=co[3]), add = TRUE, col="pink", lwd=2) 

DF <- data.frame(choiceConfidenceStats$x, choiceConfidenceStats$grp.mean)
ggplot(DF, aes(x = x, y = y)) +
  geom_point() +
  stat_smooth(method = 'lm', aes(colour = 'linear'), se = FALSE) +
  stat_smooth(method = 'lm', formula = y ~ poly(x,2), aes(colour = 'polynomial'), se= FALSE) +
  stat_smooth(method = 'nls', formula = y ~ a * log(x) + b, aes(colour = 'logarithmic'), se = FALSE, method.args = list(start = list(a = 1, b = 1))) +
  theme_bw() +
  scale_colour_brewer(name = 'Trendline', palette = 'Set2')

##########################################################################
#FITTING A REGRESSION MODEL TO THE DATA
#take that data set section of interest - near the midline
shortenedData <- subset(confidence, flippedLocation > 175 & flippedLocation < 525)

#plot linear regression line
my.formula <- y ~ x
ggplot(shortenedData, aes(x=flippedLocation, y=confidence, linetype=distribution)) + geom_point() +
  geom_smooth(method="lm", se=FALSE,  formula = my.formula) +
  facet_wrap(~distribution)

ggplot(shortenedData, aes(x=flippedLocation, y=confidence, color=distribution)) + geom_point() +
  geom_smooth(method="lm", se=FALSE,  formula = my.formula) +
  facet_wrap(~PID)

# For each participant we are going to create 4 regression lines
# Find the slope and the intercept fopr each of the four lines, for each participant. 
# THESE values will be entered into the test statistics. 

#1. location 200 - 350 chose narrow
narrowBody <-subset(narrowResponse, flippedLocation > 200 & flippedLocation < 350) #mean
#2. location 350 - 500 chose narrow
narrowTail <- subset(narrowResponse,flippedLocation > 350 & flippedLocation < 500) #tail
#3. location 200-350 chose wide
wideTail <- subset(wideResponse, flippedLocation > 200 & flippedLocation < 350) #tail
#4. location 350-500 chose wide
wideBody <- subset(wideResponse,flippedLocation > 350 & flippedLocation < 500) #mean

#plot out narrowBody versus wideBody for all
my.formula <- y ~ x
ggplot(narrowBody, aes(x=flippedLocation, y=confidence)) + geom_point() +
  geom_smooth(method="lm", se=TRUE,  formula = my.formula) +
  geom_point(data=wideBody, aes(x=flippedLocation, y=confidence), color = "deepskyblue4")+
  geom_smooth(data = wideBody, method="lm", se=TRUE,  formula = my.formula) 

#plot out narrowTail versus wideTail for all
ggplot(narrowTail, aes(x=flippedLocation, y=confidence)) + geom_point() +
  geom_smooth(method="lm", se=TRUE,  formula = my.formula) +
  geom_point(data=wideTail, aes(x=flippedLocation, y=confidence), color = "deepskyblue4")+
  geom_smooth(data = wideTail, method="lm", se=TRUE,  formula = my.formula) 

#plot all 4 together
ggplot(narrowBody, aes(x=flippedLocation, y=confidence)) + geom_point() +
  geom_smooth(method="lm", se=TRUE,  formula = my.formula) +
  geom_point(data=narrowTail, aes(x=flippedLocation, y=confidence), color = "black")+
  geom_smooth(data = narrowTail, method="lm", se=TRUE,  formula = my.formula) +
  geom_point(data=wideTail, aes(x=flippedLocation, y=confidence), color = "deepskyblue4")+
  geom_smooth(data = wideTail, method="lm", se=TRUE,  formula = my.formula, color = "red") +
  geom_point(data=wideBody, aes(x=flippedLocation, y=confidence), color = "deepskyblue4")+
  geom_smooth(data = wideBody, method="lm", se=TRUE,  formula = my.formula, color = "red") 


#for each participant, for each line, fit a linear regression line
#continuous flippedLocation, continuous confidence 
#create a function to calculate the glm coefficients (intercept and slope)
getCoeff <- function(data)
{
  fit <-lm(data$confidence~data$distance_to_bound)
  co <- coef(fit)
}

x <- c(1:40)
coefNarrowBody <- matrix(data = NA, nrow = (length(x)), ncol = 2) 
for (i in unique(narrowBody$PID)){
  coefNarrowBody[i,] <- getCoeff(subset(narrowBody, PID == i))
}
coefNarrowBody <- cbind(coefNarrowBody, x, "narrow", "body")

coefNarrowTail <-matrix(data = NA, nrow = (length(x)), ncol = 2) 
for (i in unique(narrowTail$PID)){
  coefNarrowTail[i,] <- getCoeff(subset(narrowTail, PID == i))
}
coefNarrowTail <- cbind(coefNarrowTail, x, "narrow", "tail")

coefWideTail <-matrix(data = NA, nrow = (length(x)), ncol = 2) 
for (i in unique(wideTail$PID)){
  coefWideTail[i,] <- getCoeff(subset(wideTail, PID == i))
}
coefWideTail <- cbind(coefWideTail, x, "wide", "tail")

coefWideBody <-matrix(data = NA, nrow = (length(x)), ncol = 2) 
for (i in unique(wideBody$PID)){
  coefWideBody[i,] <- getCoeff(subset(wideBody, PID == i))
}
coefWideBody <- cbind(coefWideBody, x, "wide", "body")

#run some t-tests?
#test if narrowBody differs from wideBody (INTERCEPTS) (from midline to mean)
t.test(as.numeric(coefNarrowBody[,1]), as.numeric(coefWideBody[,1]), paired = TRUE, alternative = "two.sided") 
#test if narrowTail differs from wideTail (INTERCEPTS) (from midline to tail)
t.test(as.numeric(coefNarrowTail[,1]), as.numeric(coefWideTail[,1]), paired = TRUE, alternative = "two.sided") 
#test if narrowBody differs from wideBody (SLOPE) (from midline to mean)
t.test(as.numeric(coefNarrowBody[,2]), as.numeric(coefWideBody[,2]), paired = TRUE, alternative = "two.sided") 
#test if narrowTail differs from wideTail (SLOPE) (from midline to tail)
t.test(as.numeric(coefNarrowTail[,2]), as.numeric(coefWideTail[,2]), paired = TRUE, alternative = "two.sided") 

#run an anova? 
coefs <- rbind(coefNarrowBody, coefNarrowTail,coefWideTail, coefWideBody)
coefs <- as.data.frame(coefs)
coefs$V1 <- as.numeric(coefs$V1)
coefs$V2 <- as.numeric(coefs$V2)
coefs$x <- as.factor(coefs$x)
coefs$V4 <- as.factor(coefs$V4)
coefs$V5 <- as.factor (coefs$V5)

coefs <- coefs %>% 
  rename(
    intercept = V1,
    slope = V2,
    PID = x,
    distribution = V4,
    linetype = V5
  )

table(coefs$distribution, coefs$linetype) #balanced design

#Run a two-way within-subjects ANOVA with interaction effect for intercepts 
coefs %>%
  group_by(distribution, linetype) %>%
  shapiro_test(intercept) #issues with normality on tail data... rest fine 

#test homogeneity of variance
leveneTest(intercept ~ distribution, coefs, center=median) #fine
leveneTest(intercept ~ linetype, coefs, center=median)#line type... again the small amount of data in tail driving

#run anova
coefsInt <- subset(coefs, select = -slope)
aovInt <- anova_test(data = coefsInt, dv = intercept, wid = PID, within = c(distribution, linetype))
get_anova_table(aovInt)

#Run a two-way within-subjects ANOVA with interaction effect for slopes 
coefsSlope <- subset(coefs, select = -intercept)
aovSlope <- anova_test(data = coefsSlope, dv = slope, wid = PID, within = c(distribution, linetype))
get_anova_table(aovSlope)


##########################################################################
#DISCONTINUITY OF REGRESSION LINES
getSSE <- function (data,int){
  fit <-lm(I(data$distance_to_bound - int) ~ 0 + data$confidence)
  res <- summary(fit)
  SSE <-sum(res$residuals^2)
}
intercepts <- c(1:100)

#start with narrow lines - these must share an intercept (no gap)
residualsNarrowBody <- matrix(data = NA, nrow = (length(intercepts)), ncol = length(unique(x)))
residualsNarrowTail <- matrix(data = NA, nrow = (length(intercepts)), ncol = length(unique(x)))

#loop through all possible intercepts (1-100) and get the SSE on each intercept
for(int in intercepts){
  #for each participant
  for(i in unique(narrowBody$PID)){
    residualsNarrowBody[int,i] <- getSSE(subset(narrowBody, PID == i), int) #calc sum square error
    }
}
for (int in intercepts){
  for (i in unique(narrowTail$PID)){
    residualsNarrowTail[int,i] <- getSSE(subset(narrowTail, PID == i), int) #calc sum square error
    }
}

residualsNarrowTail[,2] <-0 #reset NAs, THIS SHOULD BE AUTOMATED
residualsNarrowTail[,19]<-0 #reset NAs, THIS SHOULD BE AUTOMATED

#what intercept has the lowest sum of "sum of squared error" terms (across tail and body line)
#that is the intercept to take for that PID
lowSSEIntNarrow <- matrix(data = NA, nrow = length(unique(x)), ncol = 1)
for (i in unique(x)){
  lowSSEIntNarrow[i,] <- which.min(residualsNarrowBody[,i] + residualsNarrowTail[,i])
} #this value is the value to set the intercept at for this PID

#now take wide lines - these must share an intercept (no gap)
residualsWideBody <- matrix(data = NA, nrow = (length(intercepts)), ncol = length(unique(x)))
residualsWideTail <- matrix(data = NA, nrow = (length(intercepts)), ncol = length(unique(x)))

#loop through all possible intercepts (1-100) 
for(int in intercepts){
  #for each participant
  for(i in unique(x)){
    residualsWideBody[int,i] <- getSSE(subset(wideBody, PID == i), int) #calc sum suqare error
  }
}
for(int in intercepts){
  #for each participant
  for(i in unique(wideTail$PID)){
residualsWideTail[int,i] <- getSSE(subset(wideTail, PID == i), int) #calc sum suqare error
  }
}
residualsWideTail[,22]<-0

#what has the lowest sum of squared error - that is the intercept to take for that PID
lowSSEIntWide <- matrix(data = NA, nrow = length(unique(x)), ncol = 1)
for (i in unique(x)){
  lowSSEIntWide[i,] <- which.min(residualsWideBody[,i]+residualsWideTail[,i])
} #this value is the value to set the intercept at for this PID

#now have best intercepts for each pp for the narrow and wide lines 
#now refit lm to get slope for individual lines, for each PID, with established intercept
getCoeffSetInt <- function(data, int)
{
  fit <-lm(I(data$distance_to_bound - int) ~ 0 + data$confidence)
  co <- coef(fit)
}

x <- c(1:40)
#Narrow lines use the intercepts from lowSEEIntNarrow
coefNarrowBody <- matrix(data = NA, nrow = (length(x)), ncol = 2) 
for (i in unique(narrowBody$PID)){
  coefNarrowBody[i,] <- getCoeffSetInt(subset(narrowBody, PID == i), lowSSEIntNarrow[i,])
}
coefNarrowBody <- cbind(coefNarrowBody,lowSSEIntNarrow, x, "narrow", "body")


coefNarrowTail <-matrix(data = NA, nrow = (length(x)), ncol = 2) 
for (i in unique(narrowTail$PID)){
  coefNarrowTail[i,] <- getCoeffSetInt(subset(narrowTail, PID == i), lowSSEIntNarrow[i,])
}
coefNarrowTail <- cbind(coefNarrowTail,lowSSEIntNarrow, x, "narrow", "tail")

coefWideTail <-matrix(data = NA, nrow = (length(x)), ncol = 2) 
for (i in unique(wideTail$PID)){
  coefWideTail[i,] <- getCoeffSetInt(subset(wideTail, PID == i), lowSSEIntWide[i,])
}
coefWideTail <- cbind(coefWideTail, lowSSEIntWide, x, "wide", "tail")

coefWideBody <-matrix(data = NA, nrow = (length(x)), ncol = 2) 
for (i in unique(wideBody$PID)){
  coefWideBody[i,] <- getCoeffSetInt(subset(wideBody, PID == i), lowSSEIntWide[i,])
}
coefWideBody <- cbind(coefWideBody, lowSSEIntWide, x, "wide", "body")
  
#run some t-tests?
#test if narrowBody differs from wideBody (INTERCEPTS) (from midline to mean)
t.test(coefNarrowBody[,2], coefWideBody[,2], paired = TRUE, alternative = "two.sided") 
#test if narrowTail differs from wideTail (INTERCEPTS) (from midline to tail)
t.test(coefNarrowTail[,2], coefWideTail[,2], paired = TRUE, alternative = "two.sided") 
#test if narrowBody differs from wideBody (SLOPE) (from midline to mean)
t.test(coefNarrowBody[,1], coefWideBody[,1], paired = TRUE, alternative = "two.sided") 
#test if narrowTail differs from wideTail (SLOPE) (from midline to tail)
t.test(coefNarrowTail[,1], coefWideTail[,1], paired = TRUE, alternative = "two.sided") 

#run an anova? 
coefsSetInt <- rbind(coefNarrowBody, coefNarrowTail,coefWideTail, coefWideBody)
coefsSetInt <- as.data.frame(coefsSetInt)
coefsSetInt <- subset(coefsSetInt, select = - V2)
coefsSetInt$V1 <- as.numeric(coefsSetInt$V1)
coefsSetInt$V3 <- as.numeric(coefsSetInt$V3)
coefsSetInt$x <- as.factor(coefsSetInt$x)
coefsSetInt$V5 <- as.factor(coefsSetInt$V5)
coefsSetInt$V6 <- as.factor (coefsSetInt$V6)

coefs <- coefsSetInt %>% 
  rename(
    intercept = V3,
    slope = V1,
    PID = x,
    distribution = V5,
    linetype = V6
  )

table(coefs$distribution, coefs$linetype) #balanced design

#Run a two-way within-subjects ANOVA with interaction effect for slopes 
coefs %>%
  group_by(distribution, linetype) %>%
  shapiro_test(slope) 

#test homogeneity of variance
leveneTest(slope ~ distribution, coefs, center=median) #fine
leveneTest(slope ~ linetype, coefs, center=median)#line type... again the small amount of data in tail driving

#run anova
coefsSlope <- subset(coefs, select = -intercept)
aovSlope <- anova_test(data = coefsSlope, dv = slope, wid = PID, within = c(distribution, linetype))
get_anova_table(aovSlope)

#no effects to speak of - because we are fixing the intercepts...
#are we diminishing the differences that are there to be seen? 

##########################################################################
#Wide side and Narrow side analysis. 
#instead of looking at differences between the narrow and the wide distributions

#Take wide side first, that is wideBody and narrowTail (should share a slope)
#flip narrowTail around the midline 
narrowTail$newFlipLocation <- (350 - narrowTail$distance_to_bound)-200
wideBody$newFlipLocation <- wideBody$flippedLocation-200
wideSide <- rbind(narrowTail, wideBody)

#look at NarrowSide
narrowBody$newFlipLocation <- (narrowBody$distance_to_bound + 350)-200
wideTail$newFlipLocation <- wideTail$flippedLocation -200
narrowSide <- rbind(wideTail, narrowBody)

#plot together
ggplot(wideSide, aes(x=newFlipLocation, y=confidence)) + geom_point() +
  geom_smooth(method="lm", se=TRUE,  formula = my.formula, color = 'red') +
  ggplot(narrowSide, aes(x=newFlipLocation, y=confidence)) + geom_point(color = 'deepskyblue4') +
  geom_smooth(method="lm", se=TRUE,  formula = my.formula, color = 'blue')

ggplot(wideSide, aes(x=newFlipLocation, y=confidence)) + geom_point() +
  geom_point(data = narrowSide, aes(x=newFlipLocation, y=confidence), color = 'deepskyblue4')+
  geom_smooth(data = wideSide, method="lm", se=TRUE,  formula = my.formula, color = 'red') +
  geom_smooth(data = narrowSide, method="lm", se=TRUE,  formula = my.formula, color = 'blue')

#for each participant, fit a wideside and a narrowside line
getCoeffSide <- function(data)
{
  fit <-lm(data$confidence~data$newFlipLocation)
  co <- coef(fit)
}

coefNarrowSide <-matrix(data = NA, nrow = (length(x)), ncol = 4)
coefWideSide <-matrix(data = NA, nrow = (length(x)), ncol = 4)
for (i in unique(x)){
  coefNarrowSide[i,] <- getCoeffSide(subset(narrowSide, PID == i))
  coefWideSide[i,] <- getCoeffSide(subset(wideSide, PID == i))
}

#check for differences in slope and intercept pp
#test if differs in INTERCEPTS
t.test(coefNarrowSide[,1], coefWideSide[,1], paired = TRUE, alternative = "two.sided")
#test if differs in SLOPE
t.test(coefNarrowSide[,2], coefWideSide[,2], paired = TRUE, alternative = "two.sided")

##########################################################################
#DISTRIBUTION/LOCATION LIKELIHOODS
#at each flippedlocation bin what is the probability of it being narrow/wide, which is more probable where...
bin1 <- subset(confidence, binnedFlippedLocation == 25)
bin1N <- subset(bin1, distribution =="narrow")
bin1Probs <- c((length(bin1N$binnedFlippedLocation)/length(bin1$binnedFlippedLocation)), (1 - (length(bin1N$binnedFlippedLocation)/length(bin1$binnedFlippedLocation))))

bin2 <- subset(confidence, binnedFlippedLocation == 75)
bin2N <- subset(bin2, distribution =="narrow")
bin2Probs <- c((length(bin2N$binnedFlippedLocation)/length(bin2$binnedFlippedLocation)), (1 - (length(bin2N$binnedFlippedLocation)/length(bin2$binnedFlippedLocation))))

bin3 <- subset(confidence, binnedFlippedLocation == 125)
bin3N <- subset(bin3, distribution =="narrow")
bin3Probs <- c((length(bin3N$binnedFlippedLocation)/length(bin3$binnedFlippedLocation)), (1 - (length(bin3N$binnedFlippedLocation)/length(bin3$binnedFlippedLocation))))

bin4 <- subset(confidence, binnedFlippedLocation == 175)
bin4N <- subset(bin4, distribution =="narrow")
bin4Probs <- c((length(bin4N$binnedFlippedLocation)/length(bin4$binnedFlippedLocation)), (1 - (length(bin4N$binnedFlippedLocation)/length(bin4$binnedFlippedLocation))))

bin5 <- subset(confidence, binnedFlippedLocation == 225)
bin5N <- subset(bin5, distribution =="narrow")
bin5Probs <- c((length(bin5N$binnedFlippedLocation)/length(bin5$binnedFlippedLocation)), (1 - (length(bin5N$binnedFlippedLocation)/length(bin5$binnedFlippedLocation))))

bin6 <- subset(confidence, binnedFlippedLocation == 275)
bin6N <- subset(bin6, distribution =="narrow")
bin6Probs <- c((length(bin6N$binnedFlippedLocation)/length(bin6$binnedFlippedLocation)), (1 - (length(bin6N$binnedFlippedLocation)/length(bin6$binnedFlippedLocation))))

bin7 <- subset(confidence, binnedFlippedLocation == 325)
bin7N <- subset(bin7, distribution =="narrow")
bin7Probs <- c((length(bin7N$binnedFlippedLocation)/length(bin7$binnedFlippedLocation)), (1 - (length(bin7N$binnedFlippedLocation)/length(bin7$binnedFlippedLocation))))

bin8 <- subset(confidence, binnedFlippedLocation == 375)
bin8N <- subset(bin8, distribution =="narrow")
bin8Probs <- c((length(bin8N$binnedFlippedLocation)/length(bin8$binnedFlippedLocation)), (1 - (length(bin8N$binnedFlippedLocation)/length(bin8$binnedFlippedLocation))))

bin9 <- subset(confidence, binnedFlippedLocation == 425)
bin9N <- subset(bin9, distribution =="narrow")
bin9Probs <- c((length(bin9N$binnedFlippedLocation)/length(bin9$binnedFlippedLocation)), (1 - (length(bin9N$binnedFlippedLocation)/length(bin9$binnedFlippedLocation))))

bin10 <- subset(confidence, binnedFlippedLocation == 475)
bin10N <- subset(bin10, distribution =="narrow")
bin10Probs <- c((length(bin10N$binnedFlippedLocation)/length(bin10$binnedFlippedLocation)), (1 - (length(bin10N$binnedFlippedLocation)/length(bin10$binnedFlippedLocation))))

bin11 <- subset(confidence, binnedFlippedLocation == 525)
bin11N <- subset(bin11, distribution =="narrow")
bin11Probs <- c((length(bin11N$binnedFlippedLocation)/length(bin11$binnedFlippedLocation)), (1 - (length(bin11N$binnedFlippedLocation)/length(bin11$binnedFlippedLocation))))

bin12 <- subset(confidence, binnedFlippedLocation == 575)
bin12N <- subset(bin12, distribution =="narrow")
bin12Probs <- c((length(bin12N$binnedFlippedLocation)/length(bin12$binnedFlippedLocation)), (1 - (length(bin12N$binnedFlippedLocation)/length(bin12$binnedFlippedLocation))))

bin13 <- subset(confidence, binnedFlippedLocation == 625)
bin13N <- subset(bin1, distribution =="narrow")
bin13Probs <- c((length(bin13N$binnedFlippedLocation)/length(bin13$binnedFlippedLocation)), (1 - (length(bin13N$binnedFlippedLocation)/length(bin13$binnedFlippedLocation))))

bin14 <- subset(confidence, binnedFlippedLocation == 675)
bin14N <- subset(bin14, distribution =="narrow")
bin14Probs <- c((length(bin14N$binnedFlippedLocation)/length(bin14$binnedFlippedLocation)), (1 - (length(bin14N$binnedFlippedLocation)/length(bin14$binnedFlippedLocation))))

bin15 <- subset(confidence, binnedFlippedLocation == 725)
bin15N <- subset(bin15, distribution =="narrow")
bin15Probs <- c((length(bin15N$binnedFlippedLocation)/length(bin15$binnedFlippedLocation)), (1 - (length(bin15N$binnedFlippedLocation)/length(bin15$binnedFlippedLocation))))

bin16 <- subset(confidence, binnedFlippedLocation == 775)
bin16N <- subset(bin16, distribution =="narrow")
bin16Probs <- c((length(bin16N$binnedFlippedLocation)/length(bin16$binnedFlippedLocation)), (1 - (length(bin16N$binnedFlippedLocation)/length(bin16$binnedFlippedLocation))))

probBins <-rbind(bin1Probs, bin2Probs,bin3Probs, bin4Probs, bin5Probs, bin6Probs, bin7Probs, bin8Probs, bin9Probs, bin10Probs, bin11Probs, bin12Probs, bin13Probs, bin14Probs, bin15Probs, bin16Probs)
probBins #shows that the first 7 bins go to narrow and rest (9) go to wide.. but is this precise enough? 

#accordingly mark each trial with its likeli distribution 
confidence$likeliDist <- NA_real_
foreach(i=1:length(confidence$binnedFlippedLocation)) %do%
  if (confidence$flippedLocation[i] < 375){
    confidence$likeliDist[i] = "narrow"
  } else {
    confidence$likeliDist[i] = "wide"
  }

#now see if the choices made align with the most probable distribtuion 
#KEEP ONLY "CORRECT" JUDGEMENTS, CHOICE IN LINE WITH MORE LIKELI DISTRIBTUION.... 
wideOrange <- subset(confidence, distribution == "wide" & spaceship_class == "orange1")
wideBlue <- subset(confidence, distribution == "wide" & spaceship_class == "blue1")
narrowOrange <- subset(confidence, distribution == "narrow" & spaceship_class == "orange1")
narrowBlue <- subset(confidence, distribution == "narrow" & spaceship_class == "blue1")

wideOrangeNarrowBlue <- rbind(wideOrange, narrowBlue) #will respond 0 for wide and 1 for narrow
wideBlueNarrowOrange <- rbind(wideBlue, narrowOrange) #will respond 1 for wide and 0 for narrow

wideWONB <-subset(wideOrangeNarrowBlue, button == 0 & likeliDist == "wide") #when wide was likeli and they thought it was wide
narrowWONB <-subset(wideOrangeNarrowBlue, button == 1 & likeliDist == "narrow")#when narrow was likeli and they thought it was narrow
wideWBNO <-subset(wideBlueNarrowOrange, button ==1 & likeliDist == "wide")#when wide was likeli and they thought it was wide
narrowWBNO <- subset(wideBlueNarrowOrange, button ==0 & likeliDist == "narrow")#when narrow was likeli and they thought it was narrow

likeWideResponse <- rbind(wideWONB, wideWBNO)
likeNarrowResponse <- rbind(narrowWONB, narrowWBNO)
likeliResponses <- rbind(likeWideResponse, likeNarrowResponse)

#now retake within individual mean confidences at the binned locations (5) 
narrow <- subset(likeliResponses, distribution == "narrow")
narrow1 <-subset(narrow, binnedDistance == 1)
narrow1 <- data.frame( PID = c(1:40), binnedDistance = 1, confidence = tapply(narrow1$confidence, narrow1$PID, mean), distribution = "narrow")
narrow2<-subset(narrow, binnedDistance == 2)
narrow2 <- data.frame( PID = c(1:40), binnedDistance = 2, confidence = tapply(narrow2$confidence, narrow2$PID, mean),distribution = "narrow")
narrow3 <-subset(narrow, binnedDistance == 3)
narrow3 <- data.frame( PID = c(1:40), binnedDistance = 3, confidence = tapply(narrow3$confidence, narrow3$PID, mean),distribution = "narrow")
narrow4<-subset(narrow, binnedDistance == 4)
narrow4 <- data.frame( PID = c(1:40), binnedDistance = 4, confidence = tapply(narrow4$confidence, narrow4$PID, mean),distribution = "narrow")
narrow5 <-subset(narrow, binnedDistance == 5)
narrow5 <- data.frame( PID = c(1:40), binnedDistance = 5, confidence = tapply(narrow5$confidence, narrow5$PID, mean),distribution = "narrow")
narrow <- rbind (narrow1, narrow2, narrow3, narrow4, narrow5)

wide <- subset(likeliResponses, distribution == "wide")
wide1 <-subset(wide, binnedDistance == 1)
wide1 <- data.frame( PID = c(1:40), binnedDistance = 1, confidence = tapply(wide1$confidence, wide1$PID, mean), distribution = "wide")
wide2<-subset(wide, binnedDistance == 2)
wide2 <- data.frame( PID = c(1:40), binnedDistance = 2, confidence = tapply(wide2$confidence, wide2$PID, mean),distribution = "wide")
wide3 <-subset(wide, binnedDistance == 3)
wide3 <- data.frame( PID = c(1:40), binnedDistance = 3, confidence = tapply(wide3$confidence, wide3$PID, mean),distribution = "wide")
wide4 <-subset(wide, binnedDistance == 4)
wide4 <- data.frame( PID = c(1:40), binnedDistance = 4, confidence = tapply(wide4$confidence, wide4$PID, mean),distribution = "wide")
wide5 <-subset(wide, binnedDistance == 5)
wide5 <- data.frame( PID = c(1:40), binnedDistance = 5, confidence = tapply(wide5$confidence, wide5$PID, mean),distribution = "wide")
wide <- rbind (wide1, wide2, wide3, wide4, wide5)

my_data <- rbind(narrow, wide)

my_data$distribution <- factor(my_data$distribution, 
                               levels = c("wide", "narrow"),
                               labels = c("wide", "narrow"))
my_data$binnedDistance <- factor(my_data$binnedDistance, 
                                 levels = c("1", "2", "3", "4", "5"),
                                 labels = c("1", "2", "3", "4", "5"))

ggboxplot(my_data, x = "binnedDistance", y = "confidence", color = "distribution")+
  labs(title="Likelihood-Based Confidence per distance bin - split by distribution", x="Binned Distance", y="Within-Participant Average Confidence")


##########################################################################
#Plot out the likelihood of choosing each probability at each location value
#for the height of the gaussian (ratio of each distribution at each location point)

#what data do we want: confidence trials, flippedLocations
my_data_frame <- subset(confidence, flippedLocation >0)

#looking at ratio of each distribution at each location point
ggplot(data=my_data_frame, aes(x=flippedLocation)) + 
  geom_density(aes(fill = distribution), alpha = 0.4) +
  geom_vline(aes(xintercept = grp.mean, color = distribution),
             data = mu, linetype = "dashed") +
  scale_color_manual(values = c("#868686FF", "#EFC000FF"))+
  scale_fill_manual(values = c("#868686FF", "#EFC000FF")) +
  scale_y_continuous(sec.axis = sec_axis(trans=~ .*1e4, name="Confidence")) +
  labs(title="Density Plot for Location (flipped)", x="Location", y="Density")

#from 0 - 800 what ratio of trials come from narrow at each point
#round location data to the nearest digit
my_data_frame$roundLocation <- round(my_data_frame$flippedLocation)
y <- c(0:800)

probRatios <- matrix(data = NA, nrow = (length(y)), ncol = 3) 
for (val in y){
  #go to the first location point
  #take the subset of data with that location point
  data_of_interest <- subset(my_data_frame, roundLocation == val)
  #get total number of trials at that location
  totalCount <- length(data_of_interest$roundLocation)
  #get total number of narrow trials at that point
  narrowCount <- sum(data_of_interest$distribution == "narrow")
  probRatios[(val),1] <- narrowCount/totalCount
  probRatios[(val),2] <- 1-(narrowCount/totalCount)
  probRatios[val,3] <- val
} 

#rough plot the likelihoods
likelihoods <-as.data.frame(probRatios) #V1 = narrow, V2 = wide, V3 = location
ggplot(data=likelihoods, aes(x=V3, y=V2)) + geom_point() +
  geom_point(data=likelihoods, aes(x=V3, y=V1), color = "deepskyblue4")+
  labs(title="Plot of likelihoods for each distribution", x="Location", y="Likelihood")

#plot likelihoods as barplot
ggplot(data=likelihoods, aes(x = V3, y=V2)) + 
  geom_line() +
  geom_line(data=likelihoods, aes(x = V3, y= V1), color ="deepskyblue4" )+
  labs(title="Plot of likelihoods for each distribution", x="Location", y="Likelihood")


