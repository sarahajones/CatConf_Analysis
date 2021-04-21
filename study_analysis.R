#Analysis Script - CatConf Behavioural Data, Full Study. 
#behavioural analysis of study
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
#install.packages("patchwork")
library(patchwork) # To display 2 charts together
#install.packages("hrbrthemes")
library(hrbrthemes)
#install.packages("psych")
library(psych)
#install.packages("RDocumentation")
library(RDocumentation)
#install.packages("DescTools")
library(DescTools)
#install.packages("cocor")
library(cocor)
#install.packages("ggpmisc")
library(ggpmisc)
#################################################################################
#DATA LOADING, CLEANING, AND TIDYING
#load in files one by one and stitch together
mydir = setwd("C:/Users/saraha/Desktop/CatConfStudy1 Data")
myfiles = list.files(path=mydir, pattern="zapBox_v0.1.0_*", full.names=TRUE) #pull all files
dat_csv = ldply(myfiles, read_csv) #load in data

#create a PID code to identify participants more clearly. 
user_ids <- unique(dat_csv$user_id) # cache this to save recalculating it lots
dat_csv$PID <- NA_real_ # initalise PIDs as NA
dat_csv$PID <- sapply(
  dat_csv$user_id,  # vector to iterate over
  function(id) which(user_ids == id)  # function to apply to each element
)

#flip drop locations for wide and narrow
dat_csv$flippedLocation <- NA_real_ # initalise NA
responseBoundary <- 350

wide200 <- subset(dat_csv, dat_csv$distribution_variance == 10000 & dat_csv$distribution_mean == 200 )
wide200$flippedLocation <- responseBoundary + wide200$distance_to_bound
wide500 <- subset(dat_csv, dat_csv$distribution_variance == 10000 & dat_csv$distribution_mean == 500 )
wide500$flippedLocation <- wide500$drop_location
wideData <- rbind( wide200, wide500)
wideData$distribution <- "wide"

narrow500 <- subset(dat_csv, dat_csv$distribution_variance == 5000 & dat_csv$distribution_mean == 500 )
narrow500$flippedLocation <- responseBoundary - narrow500$distance_to_bound
narrow200 <- subset(dat_csv, dat_csv$distribution_variance == 5000 & dat_csv$distribution_mean == 200 )
narrow200$flippedLocation <- narrow200$drop_location
narrowData <- rbind( narrow200, narrow500)
narrowData$distribution <- "narrow"
flippedData <- rbind(wideData, narrowData)

flippedNeg <- subset(flippedData, flippedLocation < 0) #12cases where the value is now neg
flippedData <- subset(flippedData, flippedLocation > 0) #exclude those 12 cases

#calculate the distance to bound in the flipped case too
flippedData$flipped_distance_to_bound <- NA_real_ # initalise distance to bound in flipped data as NA
foreach(i=1:length(flippedData$flippedLocation)) %do%
  if (flippedData$flippedLocation[i] < responseBoundary){
    flippedData$flipped_distance_to_bound[i] <- responseBoundary - flippedData$flippedLocation[i]
  }else{
    flippedData$flipped_distance_to_bound[i] <- flippedData$flippedLocation[i] - responseBoundary
  }

#check up on distribution properities. 
#calculate the mean and variance for each and see if it maps across to the distributions
wideFlippedClear <- subset(flippedData, distribution == "wide")
wideVarianceFlipped <- var(wideFlippedClear$flipped_distance_to_bound) 
wideVarianceRaw <- var(wideFlippedClear$distance_to_bound)#var unchanged by flip
wideVariance <- var(wideFlippedClear$flippedLocation)
wideSD<- sd(wideFlippedClear$flippedLocation) #91 (close to 100)

narrowFlippedClear <- subset(flippedData, distribution == "narrow")
narrowVarianceFlipped <- var(narrowFlippedClear$flipped_distance_to_bound) 
narrowVarianceRaw <- var(narrowFlippedClear$distance_to_bound)#var unchanged by flip
narrowVariance <- var(narrowFlippedClear$flippedLocation)
narrowSD <- sd(narrowFlippedClear$flippedLocation) #68.6 close to 71


################################################################################################
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

#how long did the experiment take on average to complete? 
lastTrials <- subset(dat_csv, trial_index > 438)
endTime <- lastTrials$time_elapsed
minutesTaken <- ((endTime/60)/1000)
averageTime <- mean(minutesTaken)
rangeTime <- range(minutesTaken) #check variation on time too here, 17.7-95.8
stdTime <- sd(minutesTaken) #19.4
#check if slow people took break or were just slow?
#average time for pilots was 36.7 minutes aka 40 minutes on prolific. 

#check for any text feedback on feedback form
feedBackDataComment <- subset(dat_csv, feedback_comments != "NA")
feedBackDataTechnical <- subset(dat_csv, feedback_technical != "NA")
feedBackDataStrategy <- subset(dat_csv, feedback_strategy != "NA")
#Few technical issues
#very positive feedback on the study :) 
#strategy comments - themes of screen splitting 
#(good that we veered off midline but was it enough?)

#DEMOGRAPHICS
#what is the age, mean, range?
ageData <- subset(dat_csv, participantAge != 'NA')
age <- ageData$participantAge
ageRange <- range(age) #18-60 
meanAge <- mean(age) #29.48 
stdAge <- sd(age) #11.38 

#what is the gender split?
genderData <- subset(dat_csv, participantGender != 'NA')
gender <- genderData$participantGender #17 female 23 male
female <- gender == 'female'

#what handedness do we have? 
handData <- subset(dat_csv, participantHandedness != 'NA')
handedness <- handData$participantHandedness
right <- handedness == "right" #6 left handed participants

############################################################################################
#QUICKFIRE ROUNDS
#look at quickfire rounds for accuracy rates (did the participants learn a pairing) 
quickfireData <- subset(dat_csv, trial_type == 'jspsych-quickfire') #200 trials across 40 pp
quickfireTrials <- 20
correctQuickfire <- (subset(quickfireData, correct == 1)) 
quickfireAccuracy <- ((nrow(correctQuickfire)/(quickfireTrials*numParticipants))*100) 
# mean accuracy in quickfire is 92.875

#per participant?
PIDwiseCorrectQuickfire <- quickfireData %>% 
  group_by(PID) %>%
  summarise(grp.mean = mean(correct))

PIDwiseQuickfireAccuracy <- ggplot(data = PIDwiseCorrectQuickfire) +
  geom_point(mapping = aes(x = PID, y = grp.mean)) +
  labs(title="Participant Quickfire Accuracy", x="PID", y="Accuracy") 
PIDwiseQuickfireAccuracy + xlim(0, 40)+ ylim(0, 1)

#over time? split half?
trialsOfInterest <- range(quickfireData$trial_index) #9-28
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
#looks like people start to explore space to check for probabilitic relationship and then improve
#all but 1 above 75% by second half. 

########################################################################################
#TRAINING FASTDROP TRIALS  "CLEAR DAY TRIALS" 
clearData <- subset(dat_csv, block <= 0) #subset data to focus on training trials
clearTrials <-50 #number of training trials in each block
blockNum <-4 #number of trianing (and testing) blocks


clearData$distribution <- NA_real_ # initalise distance to bound in flipped data as NA

foreach(i=1:length(clearData$distribution_variance)) %do%
  if (clearData$distribution_variance[i] == 5000){
    clearData$distribution[i] = "narrow"
  }else{
    clearData$distribution[i] = "wide"
  }

fastDropLocations <- ggplot(data=clearData, aes(clearData$drop_location)) + 
  geom_histogram( fill=I("blue"), 
                  col=I("navy")) +
  labs(title="Histogram for Training Locations", x="Drop location", y="Frequency") 
fastDropLocations #plot aggregate across all training blocks and all trials. 

blockwiseFastDropLocations <- ggplot(data=clearData, aes(clearData$drop_location)) + 
  geom_histogram( fill=I("blue"), 
                  col=I("navy")) +
  labs(title="Histogram for Training Locations", x="Drop location", y="Frequency") +
  facet_wrap( ~ block)
blockwiseFastDropLocations #plot aggregate blockwise (NB randomisation across pp blurs diffs)

#remember these blocks are randomised across pps so it is not clear which is the narrow and wide distributions as we have the means
#plot out 1 pp for each trianing block to get better idea of the information 1 pp had before each block. 
clearDataPP1 <- subset(clearData, PID == 1)

ppBlockwiseFastDropLocations  <- ggplot(data=clearDataPP1, aes(drop_location, fill=I("blue"))) + 
  geom_histogram(col=I("navy")) +
  labs(title="Histogram for Training Locations", x="Drop location", y="Frequency") +
  facet_wrap( ~ block)
ppBlockwiseFastDropLocations

#plot out with aes on distribution name 
ppBlockwiseFastDropLocations  <- ggplot(data=clearDataPP1, aes(drop_location, fill= distribution)) + 
  geom_histogram(col=I("navy")) +
  labs(title="Histogram for Training Locations", x="Drop location", y="Frequency") +
  facet_wrap( ~ block)
ppBlockwiseFastDropLocations

#use flipped dataset to replot
flippedClear <- subset(flippedData, block <= 0)

flippedFastDropLocations <- ggplot(data=flippedClear, aes(flippedClear$flippedLocation, fill = distribution)) + 
  geom_histogram(col=I("navy")) +
  labs(title="Histogram for (flipped) Training Locations", x="Drop location (flipped)", y="Frequency") 
flippedFastDropLocations 

#density plots
a <- ggplot(data=flippedClear, aes(flippedClear$flippedLocation))
# Change y axis to count instead of density
# Change line color by distribution
a + geom_density(aes(color = distribution)) +
  scale_color_manual(values = c("#868686FF", "#EFC000FF"))
# Change fill color by distribution and add mean line
mu <- flippedClear %>% 
  group_by(distribution) %>%
  summarise(grp.mean = mean(flippedLocation))
# Use semi-transparent fill: alpha = 0.4
a + geom_density(aes(fill = distribution), alpha = 0.4) +
  geom_vline(aes(xintercept = grp.mean, color = distribution),
             data = mu, linetype = "dashed") +
  xlim(0,800)+
  scale_color_manual(values = c("#868686FF", "#EFC000FF"))+
  scale_fill_manual(values = c("#868686FF", "#EFC000FF")) +
  labs(title="Density Plot for (flipped) Training Locations", x="Drop location (flipped)", y="Density")

###########################################################################
#TEST TRIALS "CLOUDY DAY TRIALS"
#ACCURACY
#Look at "unclear" trial percentage accuracy - above chance? 
flippedUnclearData <- subset(flippedData, block > 0)
unclearTrials <- 46
unclearAccuracy <- (nrow(subset(flippedUnclearData, correct == 1))/(unclearTrials*blockNum*numParticipants)*100) #89% accuracy

#plot block by block accuracy
blockUnclearAccuracy <- flippedUnclearData %>% 
  group_by(block) %>%
  summarise(grp.mean = mean(correct))

blockwiseAccuracy <- ggplot(data = blockUnclearAccuracy) +
  geom_point(mapping = aes(x = block, y = grp.mean)) +
  labs(title="Blockwise Test Accuracy", x="Block Number", y="Accuracy") 
blockwiseAccuracy + ylim(0.5,1) 

#look at per participant accuracy
PIDUnclearAccuracy <- flippedUnclearData %>% 
  group_by(PID) %>%
  summarise(grp.mean = mean(correct))

PIDwiseAccuracy <- ggplot(data = PIDUnclearAccuracy) +
  geom_point(mapping = aes(x = PID, y = grp.mean)) +
  labs(title="Participant Test Trial Accuracy", x="PID", y="Accuracy") 
PIDwiseAccuracy + ylim(0, 1)

#pp per block
block1Cloud <- subset(flippedUnclearData, block == 1)
block2Cloud <- subset(flippedUnclearData, block == 2)
block3Cloud <- subset(flippedUnclearData, block == 3)
block4Cloud <- subset(flippedUnclearData, block == 4)

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

#accuracy and reaction time
flippedUnclearData$RT <- as.numeric(as.character(flippedUnclearData$delta_response_time))
flippedUnclearData <- subset(flippedUnclearData, RT != 'NA')

unclearCorrect <- subset(flippedUnclearData, correct == 1)
unclearIncorrect <- subset(flippedUnclearData, incorrect ==1)
avgCorrectRT <- mean(unclearCorrect$RT) #1633.715
avgIncorrectRT <- mean(unclearIncorrect$RT) #2103.023


bxp <- ggboxplot(
  flippedUnclearData, x = "correct", y = "RT", 
  ylab = "Reaction Time (ms)", xlab = "Incorrect                Correct", add = "jitter"
)
bxp
#standard
t.test(flippedUnclearData$RT, flippedUnclearData$correct, paired = TRUE, alternative = "two.sided")

#welchs
stat.test <- flippedUnclearData %>%
  t_test(RT ~ correct) %>%
  add_significance()
stat.test

stat.test <- stat.test %>% add_xy_position(x = "correct")
bxp + 
  stat_pvalue_manual(stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(stat.test, detailed = TRUE))


##WITHIN PID 
unClearPID <- unclearCorrect %>% 
  group_by(PID) %>%
  summarise(meanCorrect = mean(RT))

unClearIncorrectPID <- unclearIncorrect %>% 
  group_by(PID) %>%
  summarise(meanIncorrect = mean(RT))

unClearPID$meanIncorrect <- unClearIncorrectPID$meanIncorrect

######USE THIS VALUE
t.test(unClearPID$meanCorrect, unClearPID$meanIncorrect, paired = TRUE, alternative = "two.sided")

unClearPID$CorrectRT <- unClearPID$grp.mean
unClearPID$IncorrectRT <- unClearIncorrectPID$grp.mean

bxp <- ggboxplot(
  RTComb, x = "group", y = "grp.mean", 
  ylab = "Reaction Time (ms)", xlab = "", add = "jitter"
)
bxp

my_data = tibble(my_iv = c(unClearPID$group, unClearIncorrectPID$group),
                   my_dv = c(unClearPID$grp.mean, unClearIncorrectPID$grp.mean))

stat.test <- t.test(my_dv ~ my_iv, data = my_data, var.equal = FALSE)
stat.test
stat.test <- stat.test %>% add_xy_position(x = my_iv$value)
bxp +
  stat_pvalue_manual(stat.test, tip.length = 0) +
  labs(title="Average Reaction Time, within Participant") 

######################################################################################
#CONFIDENCE
unClearConfidence <- subset(flippedUnclearData, confidence != 'null') #remove missing confidence reports
unClearConfidence$confidence <- as.numeric(as.character(unClearConfidence$confidence))
unClearConfidence$confidenceRT <- as.numeric(as.character(unClearConfidence$delta_confidence_response_time))

#BINNING VALUES FOR CONFIDENCE AND DISTANCE TO BOUND
#bin distance to boundary into 5 bins
#bin 3 should contain the mean i.e. 150 from midline
unClearConfidence$binnedDistance <- NA_real_ # initalise distance bins as NA
breaks <- quantile(unClearConfidence$distance_to_bound, seq(0, 1, by = 0.20))
unClearConfidence$binnedDistance <- cut(unClearConfidence$distance_to_bound, breaks, include.lowest = TRUE, labels = c("1", "2", "3", "4", "5"))

#bin Confidence 
#(PROBLEMATIC? CANNOT DO MORE AS BINS NOT UNIQUE AS SO MANY @ 100)
unClearConfidence$binnedConfidence <- NA_real_ # initalise distance bins as NA
breaks <- quantile(unClearConfidence$confidence, seq(0, 1, by = 0.33))
unClearConfidence$binnedConfidence <- cut(unClearConfidence$confidence, breaks, include.lowest = TRUE, labels = c("1", "2", "3"))
#WARNING THIS BINS CONFIDENCE ON AVERAGE NOT WITHIN EACH PERSON


# plot out raw confidence reports as a density.... 
a <- ggplot(data=unClearConfidence, aes(unClearConfidence$confidence))
a + geom_density(aes(color = distribution)) +
  scale_color_manual(values = c("#868686FF", "#EFC000FF"))
# Change fill color by distribution use semi-transparent fill: alpha = 0.4
a + geom_density(aes(fill = distribution), alpha = 0.4) +
  geom_vline(aes(xintercept = grp.mean, color = distribution),
             data = mu, linetype = "dashed") +
  xlim(0,100)+
  scale_color_manual(values = c("#868686FF", "#EFC000FF"))+
  scale_fill_manual(values = c("#868686FF", "#EFC000FF")) +
  labs(title="Density Plot for Confidence Reports", x="Raw Confidence", y="Density")

#Look at confidence as a function of accuracy
#Confidence in correct trials
correctConfidenceTrials <- subset(unClearConfidence, correct == 1)
meanCorrectConfidence <- mean(correctConfidenceTrials$confidence)
meanConfidenceRTCorrectTrials <- mean(correctConfidenceTrials$confidenceRT)

#Confidence in incorrect trials
incorrectConfidenceTrials <- subset(unClearConfidence, incorrect == 1)
meanIncorrectConfidence <- mean(incorrectConfidenceTrials$confidence)
meanConfidenceRTinCorrectTrials <- mean(incorrectConfidenceTrials$confidenceRT)

#look at the difference between correct and incorrect trials. 
correctConf <- correctConfidenceTrials %>% 
  group_by(PID) %>%
  summarise(meanCorrect = mean(confidence))

incorrectConf <- incorrectConfidenceTrials %>% 
  group_by(PID) %>%
  summarise(meanIncorrect = mean(confidence))

correctConf$meanIncorrect <- incorrectConf$meanIncorrect

######USE THIS VALUE
t.test(correctConf$meanCorrect, correctConf$meanIncorrect, paired = TRUE, alternative = "two.sided")

bxp <- ggboxplot(
  confCompare, x = "group", y = "grp.mean", 
  ylab = "Raw Confidence Report", xlab = "", add = "jitter"
)
bxp

my_data = tibble(my_iv = c(correctConf$group, incorrectConf$group),
                 my_dv = c(correctConf$grp.mean, incorrectConf$grp.mean))

stat.test <- t.test(my_dv ~ my_iv, data = my_data, var.equal = FALSE)
stat.test

stat.test <- confCompare %>%
  t_test(grp.mean ~ group) %>%
  add_significance()
stat.test

stat.test <- stat.test %>% add_xy_position(x = "group")
bxp + 
  stat_pvalue_manual(stat.test, tip.length = 0) +
  labs(title="Average Raw Confidence, within Participant") +
  ylim(0,100)


#test the signifnace of the RT difference
correctConfRT <- correctConfidenceTrials %>% 
  group_by(PID) %>%
  summarise(grp.mean = mean(confidenceRT))
correctConfRT$group <-1

incorrectConfRT <- incorrectConfidenceTrials %>% 
  group_by(PID) %>%
  summarise(grp.mean = mean(confidenceRT))
incorrectConfRT$group <-0

confCompareRT <- rbind(correctConfRT,incorrectConfRT)
######USE THIS VALUE
t.test(confCompareRT$grp.mean, confCompareRT$group, paired = TRUE, alternative = "two.sided")

bxp <- ggboxplot(
  confCompareRT, x = "group", y = "grp.mean", 
  ylab = "Reaction Time (ms)", xlab = "", add = "jitter"
)
bxp

stat.test <- confCompareRT %>%
  t_test(grp.mean ~ group) %>%
  add_significance()
stat.test

stat.test <- stat.test %>% add_xy_position(x = "group")
bxp + 
  stat_pvalue_manual(stat.test, tip.length = 0) +
  labs(title="Average Raw Confidence, within Participant", subtitle = get_test_label(stat.test, detailed = TRUE))
  
###################################################################################
#CONFIDENCE AND DISTANCE FROM BOUND
#confidence as a function of the midline distance

#Across all trials - looking for sensible ratings
totalRawConfidence = ggplot(data = unClearConfidence) +
  geom_point(mapping = aes(x = distance_to_bound, y = confidence))
totalRawConfidence + xlim(0, 400) 

#overlaid accuracy
correctVincorrectConfidence = ggplot(data = unClearConfidence) +
  geom_point(mapping = aes(x = distance_to_bound, y = confidence, color = correct))+
  labs(title="Confidence as a function of distance to bound", x="Distance from Category Bound", y="Raw Confidence")
correctVincorrectConfidence +xlim(0, 400)

#split by distribution
correctVincorrectConfidence = ggplot(data = unClearConfidence) +
  geom_point(mapping = aes(x = distance_to_bound, y = confidence, color = correct))+
  labs(title="Confidence as a function of distance to bound", x="Distance from Category Bound", y="Raw Confidence")+
  facet_wrap(~distribution)
correctVincorrectConfidence +xlim(0, 400)

#take average at each distance point.... 
avgedConf <- unClearConfidence%>% 
  group_by(confidence) %>%
  summarise(grp.mean = mean(distance_to_bound))

avgedConfidence = ggplot(data = avgedConf) +
  geom_point(mapping = aes(x = grp.mean, y = confidence))
avgedConfidence

wideConf <- subset(unClearConfidence, distribution == "wide")
avgedWideConf <- wideConf%>% 
  group_by(confidence) %>%
  summarise(grp.mean = mean(distance_to_bound))
avgedWideConf$distribution <- "wide"

narrowConf <- subset(unClearConfidence, distribution == "narrow")
avgedNarrowConf <- narrowConf%>% 
  group_by(confidence) %>%
  summarise(grp.mean = mean(distance_to_bound))
avgedNarrowConf$distribution <- "narrow"

avgedWideNarrow <-rbind(avgedWideConf, avgedNarrowConf)
avgedWideNarrowConfidence = ggplot(data = avgedWideNarrow) +
  geom_point(mapping = aes(x = grp.mean, y = confidence, color = distribution))
avgedWideNarrowConfidence


#binned distance, quintiles of distance to the bound 
#average out confidence per bin
confidencePerDistanceBin <- unClearConfidence %>% 
  group_by(binnedDistance) %>%
  summarise(grp.mean = mean(confidence))

correctConfPerDistanceBin <- correctConfidenceTrials%>% 
  group_by(binnedDistance) %>%
  summarise(grp.mean = mean(confidence))
correctConfPerDistanceBin$correct <- 1

incorrectConfPerDistanceBin <- incorrectConfidenceTrials%>% 
  group_by(binnedDistance) %>%
  summarise(grp.mean = mean(confidence))
incorrectConfPerDistanceBin$correct <- 0

accuracyConfidence <- rbind(correctConfPerDistanceBin, incorrectConfPerDistanceBin)

binnedAvgConfidenceByDistance = ggplot(data = confidencePerDistanceBin) +
  geom_point(mapping = aes(x = binnedDistance, y = grp.mean))+
  labs(title="Average confidence as a function of distance to bound (binned)", x="Distance from Category Bound (binned)", y="Average Confidence")
binnedAvgConfidenceByDistance +ylim(0,100)

binnedAvgConfidenceByDistanceByAccuracy = ggplot(data = accuracyConfidence) +
  geom_point(mapping = aes(x = binnedDistance, y = grp.mean, color = correct))+
  labs(title="Average confidence as a function of distance to bound (binned)", x="Distance from Category Bound (binned)", y="Average Confidence")
binnedAvgConfidenceByDistanceByAccuracy +ylim(0,100)


# Create stacked bar graphs with labels
library(viridis)
stackedBarChart <- ggplot(unClearConfidence, aes(x = binnedDistance, y = confidence))+
  geom_col(aes(fill = binnedConfidence), width = 0.7) +
  scale_color_viridis(discrete = TRUE, option = "D")+
  scale_fill_viridis(discrete = TRUE) +
  labs(title="Confidence (Binned) & Distance to Bound (Binned)", x="Binned Distance from Category Bound", y="Frequency")
stackedBarChart 

#split by distribution
stackedBarChartSplit <- ggplot(unClearConfidence, aes(x = binnedDistance, y = confidence))+
  geom_col(aes(fill = binnedConfidence), width = 0.7) +
  scale_color_viridis(discrete = TRUE, option = "D")+
  scale_fill_viridis(discrete = TRUE) +
  labs(title="Confidence (Binned) & Distance to Bound (Binned)", x="Binned Distance from Category Bound", y="Frequency")+
facet_wrap(~distribution)
stackedBarChartSplit 

#'within PID'
PIDwiseConf <- unClearConfidence %>%
  group_by(PID) %>%
  summarise(
  #breaksConf = quantile(confidence, seq(0, 1, by = 0.33)),
  #binnedConf = cut(confidence, breaksConf, include.lowest = TRUE, labels = c("1", "2", "3")),
  breaksDistance = quantile(distance_to_bound, seq(0, 1, b = 0.2)),
  binnedDist = cut(distance_to_bound, breaksDistance,include.lowest = TRUE, labels = c("1", "2", "3", "4", "5" ))
  )
  
  
#######################################################################
#Confidence as a function of distnace to bound/ratio under curve

a <- ggplot(data=unClearConfidence, aes(unClearConfidence$flippedLocation))
a + geom_density(aes(color = distribution)) +
  scale_color_manual(values = c("#868686FF", "#EFC000FF"))
# Change fill color by distribution and add mean line
mu <- unClearConfidence %>% 
  group_by(distribution) %>%
  summarise(grp.mean = mean(flippedLocation))
# Use semi-transparent fill: alpha = 0.4
a + geom_density(aes(fill = distribution), alpha = 0.4) +
  geom_vline(aes(xintercept = grp.mean, color = distribution),
             data = mu, linetype = "dashed") +
  scale_color_manual(values = c("#868686FF", "#EFC000FF"))+
  scale_fill_manual(values = c("#868686FF", "#EFC000FF")) +
  labs(title="Density Plot for Location", x="Raw Location", y="Density")


#find average confidence for each binned point on flipped locations
unClearConfidence$binnedFlippedLocation <- NA_real_ # initalise distance bins as NA
#breaks <- quantile(unClearConfidence$flippedLocation, seq(0, 1, by = 0.10))
breaks <- c(0, 50, 100, 150, 200, 250, 300, 350, 400, 450, 500, 550, 600, 650, 700, 750, 800)
unClearConfidence$binnedFlippedLocation <- cut(unClearConfidence$flippedLocation, breaks, include.lowest = TRUE, labels = c("25", "75", "125", "175", "225", "275", "325", "375", "425", "475", "525", "575", "625", "675", "725", "775"))
unClearConfidence$locationbins <- cut(unClearConfidence$flippedLocation, breaks, include.lowest = TRUE, labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16"))
unClearConfidence$locationbins <- as.numeric(unClearConfidence$locationbins)

confidencePerDistanceBin <- unClearConfidence %>% 
  group_by(binnedFlippedLocation) %>%
  summarise(grp.mean = mean(confidence), .groups = "drop") %>%
  mutate(x = as.numeric(as.character(binnedFlippedLocation)), y = grp.mean/1e4)


ggplot(data=unClearConfidence, aes(x=flippedLocation)) + 
  geom_density(aes(fill = distribution), alpha = 0.4) +
  geom_vline(aes(xintercept = grp.mean, color = distribution),
             data = mu, linetype = "dashed") +
  geom_point(mapping = aes(x = x, y = y), data = confidencePerDistanceBin) +
  scale_color_manual(values = c("#868686FF", "#EFC000FF"))+
  scale_fill_manual(values = c("#868686FF", "#EFC000FF")) +
  scale_y_continuous(sec.axis = sec_axis(trans=~ .*1e4, name="Confidence")) +
  labs(title="Density Plot for Location (flipped) overlaid with Average Confidence per Location", x="Raw Location", y="Density")

############################################################
#run anova on binned location, avg confidence in that bin and distribution PER PID

narrow <- subset(unClearConfidence, distribution == "narrow")
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

wide <- subset(unClearConfidence, distribution == "wide")
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
str(my_data)

# Show a random sample
set.seed(1234)
dplyr::sample_n(my_data, 10)

#convert distribtuion to factor and recode levels
my_data$distribution <- factor(my_data$distribution, 
                               levels = c("wide", "narrow"),
                               labels = c("wide", "narrow"))

my_data$binnedDistance <- factor(my_data$binnedDistance, 
                               levels = c("1", "2", "3", "4", "5"),
                               labels = c("1", "2", "3", "4", "5"))
head(my_data)

table(my_data$binnedDistance, my_data$distribution) #balanced design

# Box plot with multiple groups
# +++++++++++++++++++++
# Plot conf by groups narrow/wide
# Color box plot by a second group: distance
library("ggpubr")
ggboxplot(my_data, x = "binnedDistance", y = "confidence", color = "distribution")+
  labs(title="Confidence per distance bin - split by distribution", x="Binned Distance", y="Within-Participant Average Confidence")


aov2 <- aov(confidence ~ as.factor(distribution) + as.factor(binnedDistance), data = my_data)
summary(aov2)
#main effects of both are significant

# Two-way ANOVA with interaction effect
aov3 <- aov(confidence ~ as.factor(distribution)*as.factor(binnedDistance), data = my_data)
summary(aov3)

res<-aov3$residuals #check residuals,
hist(res,main="Histogram of
residuals",xlab="Residuals") #skewed
library(car)
leveneTest(confidence ~ as.factor(distribution) * as.factor(binnedDistance), data = my_data)
#interaction effect non-sig, so the relationship between confidence and distribution 
#does not depend on the binned dist (so distribution and binned distance not dependent)

group_by(my_data, distribution, binnedDistance) %>%
  summarise(
    count = n(),
    mean = mean(confidence, na.rm = TRUE),
    sd = sd(confidence, na.rm = TRUE)
  )

#paairwise comparisons
TukeyHSD(aov3)

#####################################
#CHOICE BASED CONFIDENCE
#####################################
wideOrange <- subset(unClearConfidence, distribution == "wide" & spaceship_class == "orange1")
wideBlue <- subset(unClearConfidence, distribution == "wide" & spaceship_class == "blue1")
narrowOrange <- subset(unClearConfidence, distribution == "narrow" & spaceship_class == "orange1")
narrowBlue <- subset(unClearConfidence, distribution == "narrow" & spaceship_class == "blue1")

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

ggplot(data=unClearConfidence, aes(x=flippedLocation)) + 
  geom_density(aes(fill = distribution), alpha = 0.4) +
  geom_vline(aes(xintercept = grp.mean, color = distribution),
             data = mu, linetype = "dashed") +
  geom_point(mapping = aes(x = x, y = y), data = confidenceWidePerDistanceBin, color = "deepskyblue4") +
  geom_point(mapping = aes(x = x, y = y), data = confidenceNarrowPerDistanceBin) +
  scale_color_manual(values = c("#868686FF", "#EFC000FF"))+
  scale_fill_manual(values = c("#868686FF", "#EFC000FF")) +
  scale_y_continuous(sec.axis = sec_axis(trans=~ .*1e4, name="Confidence")) +
  labs(title="Density Plot for Location (flipped) overlaid with Average Confidence per Location", x="Raw Location", y="Density")

###############################################################################################
#DISTRIBUTION/LOCATION LIKELIHOODS
#take the location bin for flipped location, unClearConfidence$binnedFlippedLocation.
#at each location what is the probability of it being narrow, 
#what is the probability of it being wide, which one is more probable where...

bin1 <- subset(unClearConfidence, unClearConfidence$binnedFlippedLocation == 25)
bin1N <- subset(bin1, distribution =="narrow")
bin1Probs <- c((length(bin1N$binnedFlippedLocation)/length(bin1$binnedFlippedLocation)), (1 - (length(bin1N$binnedFlippedLocation)/length(bin1$binnedFlippedLocation))))

bin2 <- subset(unClearConfidence, unClearConfidence$binnedFlippedLocation == 75)
bin2N <- subset(bin2, distribution =="narrow")
bin2Probs <- c((length(bin2N$binnedFlippedLocation)/length(bin2$binnedFlippedLocation)), (1 - (length(bin2N$binnedFlippedLocation)/length(bin2$binnedFlippedLocation))))

bin3 <- subset(unClearConfidence, unClearConfidence$binnedFlippedLocation == 125)
bin3N <- subset(bin3, distribution =="narrow")
bin3Probs <- c((length(bin3N$binnedFlippedLocation)/length(bin3$binnedFlippedLocation)), (1 - (length(bin3N$binnedFlippedLocation)/length(bin3$binnedFlippedLocation))))

bin4 <- subset(unClearConfidence, unClearConfidence$binnedFlippedLocation == 175)
bin4N <- subset(bin4, distribution =="narrow")
bin4Probs <- c((length(bin4N$binnedFlippedLocation)/length(bin4$binnedFlippedLocation)), (1 - (length(bin4N$binnedFlippedLocation)/length(bin4$binnedFlippedLocation))))

bin5 <- subset(unClearConfidence, unClearConfidence$binnedFlippedLocation == 225)
bin5N <- subset(bin5, distribution =="narrow")
bin5Probs <- c((length(bin5N$binnedFlippedLocation)/length(bin5$binnedFlippedLocation)), (1 - (length(bin5N$binnedFlippedLocation)/length(bin5$binnedFlippedLocation))))

bin6 <- subset(unClearConfidence, unClearConfidence$binnedFlippedLocation == 275)
bin6N <- subset(bin6, distribution =="narrow")
bin6Probs <- c((length(bin6N$binnedFlippedLocation)/length(bin6$binnedFlippedLocation)), (1 - (length(bin6N$binnedFlippedLocation)/length(bin6$binnedFlippedLocation))))

bin7 <- subset(unClearConfidence, unClearConfidence$binnedFlippedLocation == 325)
bin7N <- subset(bin7, distribution =="narrow")
bin7Probs <- c((length(bin7N$binnedFlippedLocation)/length(bin7$binnedFlippedLocation)), (1 - (length(bin7N$binnedFlippedLocation)/length(bin7$binnedFlippedLocation))))

bin8 <- subset(unClearConfidence, unClearConfidence$binnedFlippedLocation == 375)
bin8N <- subset(bin8, distribution =="narrow")
bin8Probs <- c((length(bin8N$binnedFlippedLocation)/length(bin8$binnedFlippedLocation)), (1 - (length(bin8N$binnedFlippedLocation)/length(bin8$binnedFlippedLocation))))

bin9 <- subset(unClearConfidence, unClearConfidence$binnedFlippedLocation == 425)
bin9N <- subset(bin9, distribution =="narrow")
bin9Probs <- c((length(bin9N$binnedFlippedLocation)/length(bin9$binnedFlippedLocation)), (1 - (length(bin9N$binnedFlippedLocation)/length(bin9$binnedFlippedLocation))))

bin10 <- subset(unClearConfidence, unClearConfidence$binnedFlippedLocation == 475)
bin10N <- subset(bin10, distribution =="narrow")
bin10Probs <- c((length(bin10N$binnedFlippedLocation)/length(bin10$binnedFlippedLocation)), (1 - (length(bin10N$binnedFlippedLocation)/length(bin10$binnedFlippedLocation))))

bin11 <- subset(unClearConfidence, unClearConfidence$binnedFlippedLocation == 525)
bin11N <- subset(bin11, distribution =="narrow")
bin11Probs <- c((length(bin11N$binnedFlippedLocation)/length(bin11$binnedFlippedLocation)), (1 - (length(bin11N$binnedFlippedLocation)/length(bin11$binnedFlippedLocation))))

bin12 <- subset(unClearConfidence, unClearConfidence$binnedFlippedLocation == 575)
bin12N <- subset(bin12, distribution =="narrow")
bin12Probs <- c((length(bin12N$binnedFlippedLocation)/length(bin12$binnedFlippedLocation)), (1 - (length(bin12N$binnedFlippedLocation)/length(bin12$binnedFlippedLocation))))

bin13 <- subset(unClearConfidence, unClearConfidence$binnedFlippedLocation == 625)
bin13N <- subset(bin1, distribution =="narrow")
bin13Probs <- c((length(bin13N$binnedFlippedLocation)/length(bin13$binnedFlippedLocation)), (1 - (length(bin13N$binnedFlippedLocation)/length(bin13$binnedFlippedLocation))))

bin14 <- subset(unClearConfidence, unClearConfidence$binnedFlippedLocation == 675)
bin14N <- subset(bin14, distribution =="narrow")
bin14Probs <- c((length(bin14N$binnedFlippedLocation)/length(bin14$binnedFlippedLocation)), (1 - (length(bin14N$binnedFlippedLocation)/length(bin14$binnedFlippedLocation))))

bin15 <- subset(unClearConfidence, unClearConfidence$binnedFlippedLocation == 725)
bin15N <- subset(bin15, distribution =="narrow")
bin15Probs <- c((length(bin15N$binnedFlippedLocation)/length(bin15$binnedFlippedLocation)), (1 - (length(bin15N$binnedFlippedLocation)/length(bin15$binnedFlippedLocation))))

bin16 <- subset(unClearConfidence, unClearConfidence$binnedFlippedLocation == 775)
bin16N <- subset(bin16, distribution =="narrow")
bin16Probs <- c((length(bin16N$binnedFlippedLocation)/length(bin16$binnedFlippedLocation)), (1 - (length(bin16N$binnedFlippedLocation)/length(bin16$binnedFlippedLocation))))


probBins <-rbind(bin1Probs, bin2Probs,bin3Probs, bin4Probs, bin5Probs, bin6Probs, bin7Probs, bin8Probs, bin9Probs, bin10Probs, bin11Probs, bin12Probs, bin13Probs, bin14Probs, bin15Probs, bin16Probs)
#can I plot this as a kind of stacked bar chart without simultaing??

#shows that the first 7 bins go to narrow and rest (9) go to wide.. but is this precise enough? 
unClearConfidence$likeliDist <- NA_real_
foreach(i=1:length(unClearConfidence$binnedFlippedLocation)) %do%
if (unClearConfidence$flippedLocation[i] < 375){
  unClearConfidence$likeliDist[i] = "narrow"
} else {
  unClearConfidence$likeliDist[i] = "wide"
}

#now see if the choices made align with the most probable distribtuion 
#KEEP ONLY "CORRECT" JUDGEMENTS, CHOICE IN LINE WITH MORE LIKELI DISTRIBTUION.... 

wideOrange <- subset(unClearConfidence, distribution == "wide" & spaceship_class == "orange1")
wideBlue <- subset(unClearConfidence, distribution == "wide" & spaceship_class == "blue1")
narrowOrange <- subset(unClearConfidence, distribution == "narrow" & spaceship_class == "orange1")
narrowBlue <- subset(unClearConfidence, distribution == "narrow" & spaceship_class == "blue1")
wideOrangeNarrowBlue <- rbind(wideOrange, narrowBlue) #will respond 0 for wide and 1 for narrow
wideBlueNarrowOrange <- rbind(wideBlue, narrowOrange) #will respond 1 for wide and 0 for narrow

wideWONB <-subset(wideOrangeNarrowBlue, button == 0 & likeliDist == "wide") #when wide was likeli and they thought it was wide
narrowWONB <-subset(wideOrangeNarrowBlue, button == 1 & likeliDist == "narrow")#when narrow was likeli and they thought it was narrow
wideWBNO <-subset(wideBlueNarrowOrange, button ==1 & likeliDist == "wide")#when wide was likeli and they thought it was wide
narrowWBNO <- subset(wideBlueNarrowOrange, button ==0 & likeliDist == "narrow")#when narrow was likeli and they thought it was narrow

likeWideResponse <- rbind(wideWONB, wideWBNO)
likeNarrowResponse <- rbind(narrowWONB, narrowWBNO)

likeliResponses <- rbind(likeWideResponse, likeNarrowResponse)

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
# Box plot with multiple groups
# +++++++++++++++++++++
# Plot conf by groups narrow/wide
# Color box plot by a second group: distance
library("ggpubr")
ggboxplot(my_data, x = "binnedDistance", y = "confidence", color = "distribution")+
  labs(title="Confidence per distance bin - split by distribution", x="Binned Distance", y="Within-Participant Average Confidence")



#split into finer bins
#find average confidence for each binned point on flipped locations
unClearConfidence$fineBins<- NA_real_ # initalise distance bins as NA
breaks <- c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 
            110, 120, 130, 140, 150, 160, 170, 180, 190, 200, 
            210, 220, 230, 240, 250, 260, 270, 280, 290, 300, 
            310, 320, 330, 340, 350, 360, 370, 380, 390, 400, 
            410, 420, 430, 440, 450, 460, 470, 480, 490, 500, 
            510, 520, 530, 540, 550, 560, 570, 580, 590, 600, 
            610, 620, 630, 640, 650, 660, 670, 680, 690, 700, 
            710, 720, 730, 740, 750, 760, 770, 780, 790, 800)
unClearConfidence$fineBins <- cut(unClearConfidence$flippedLocation, breaks)




#####################################################################################
#take values from 175 -525 , that is the space between the means +1 either side.
#This is 8 confidence by distance bin points...16 observations  
#This is where we will fit the curves/lines... 
choiceConfidenceStats <- subset(confidenceChoiceDistanceBin, x > 150 & x < 550)
choiceConfidenceStatsWide <- subset(choiceConfidenceStats, choice == "wide")
choiceConfidenceStatsNarrow <- subset(choiceConfidenceStats, choice == "narrow")

ggplot(data=choiceConfidenceStatsWide, aes(x=x, y=grp.mean)) + 
  geom_point(color = "deepskyblue4") +
  geom_point(data=choiceConfidenceStatsNarrow, aes(x=x, y=grp.mean), color = "black") +
labs(title="Overlaid average confidence based on choice", x="Location", y="Average Confidence")

  
choiceConfidenceStatsNarrow$xRev <- rev(choiceConfidenceStatsNarrow$x)
ggplot(data=choiceConfidenceStatsNarrow, aes(x=xRev, y=grp.mean)) + 
  geom_point(color = "black") +
  geom_point(data=choiceConfidenceStatsWide, aes(x=x, y=grp.mean), color = "deepskyblue4")+
  labs(title="Overlaid average confidence based on choice", x="Location", y="Average Confidence")

##fit some lines; just NarowResponse to explore
library(Hmisc)
library(dplyr)
library(reshape2)

ggplot(data=choiceConfidenceStatsNarrow, aes(x=xRev, y=grp.mean)) + 
  geom_point(color = "deepskyblue4")

# attempt curve fitting using a First Order Equation (y = mx + b) a linear model
p <- plot(choiceConfidenceStatsNarrow$xRev,choiceConfidenceStatsNarrow$grp.mean,pch=19)
fit <- lm(choiceConfidenceStatsNarrow$grp.mean~choiceConfidenceStatsNarrow$xRev) 
lines(choiceConfidenceStatsNarrow$xRev,predict(fit,data.frame(x=choiceConfidenceStatsNarrow$xRev)),col="black")
#2nd order polynomial fit
fit2 <- lm(choiceConfidenceStatsNarrow$grp.mean~poly(choiceConfidenceStatsNarrow$xRev,2,raw=TRUE)) 
lines(choiceConfidenceStatsNarrow$xRev,predict(fit2,data.frame(x=choiceConfidenceStatsNarrow$xRev)),col="blue")
#3rd order polynomial
fit3 <- lm(choiceConfidenceStatsNarrow$grp.mean~poly(choiceConfidenceStatsNarrow$xRev,3,raw=TRUE)) 
lines(choiceConfidenceStatsNarrow$xRev,predict(fit3,data.frame(x=choiceConfidenceStatsNarrow$xRev)),col="red")
#4th order
fit4 <- lm(choiceConfidenceStatsNarrow$grp.mean~poly(choiceConfidenceStatsNarrow$xRev,4,raw=TRUE)) 
lines(choiceConfidenceStatsNarrow$xRev,predict(fit4,data.frame(x=choiceConfidenceStatsNarrow$xRev)),col="green")

#4th order describedd well but more sigmoidal for sure.
###################################################################
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
  labs(title="Average confidence based on choice per location bin", x="Location", y="Average Confidence")

my.formula <- y ~ poly(x, 3)
ggplot(choiceConfidenceStats, aes(x=x, y=grp.mean, color = choice, linetype=choice)) + geom_point() +
  geom_smooth(method="lm", se=FALSE,  formula = my.formula)+
  geom_vline(xintercept = 350, linetype="dotted", color = "black") +
  geom_vline(xintercept = 500, color = "black") +
labs(title="Average confidence based on choice per location bin", x="Location", y="Average Confidence")

my.formula <- y ~ poly(x, 4)
ggplot(choiceConfidenceStats, aes(x=x, y=grp.mean, color = choice)) + geom_point() +
  geom_smooth(method="lm", se=FALSE,  formula = my.formula)+
  geom_vline(xintercept = 350, linetype="dotted", color = "black") +
  geom_vline(xintercept = 500, color = "black") +
  labs(title="Average confidence based on choice per location bin", x="Location", y="Average Confidence")

##########################################################################
# get underlying plot
x <- choiceConfidenceStatsZap$x
y <- choiceConfidenceStatsZap$grp.mean
plot(x, y, pch=20)

# basic straight line of fit
fit <- glm(y~x)
co <- coef(fit)
abline(fit, col="blue", lwd=2)


# logarithmic
f <- function(x,a,b) {a * log(x) + b}
fit <- nls(y ~ f(x,a,b), start = c(a=1, b=1)) 
co <- coef(fit)
co
curve(f(x, a=co[1], b=co[2]), add = TRUE, col="orange", lwd=2) 

# polynomial
f <- function(x,a,b,d) {(a*x^2) + (b*x) + d}
fit <- nls(y ~ f(x,a,b,d), start = c(a=1, b=1, d=1)) 
co <- coef(fit)
curve(f(x, a=co[1], b=co[2], d=co[3]), add = TRUE, col="pink", lwd=2) 



DF <- data.frame(choiceConfidenceStatsZap$x, choiceConfidenceStatsZap$grp.mean)
ggplot(DF, aes(x = x, y = y)) +
  geom_point() +
  stat_smooth(method = 'lm', aes(colour = 'linear'), se = FALSE) +
  stat_smooth(method = 'lm', formula = y ~ poly(x,2), aes(colour = 'polynomial'), se= FALSE) +
  stat_smooth(method = 'nls', formula = y ~ a * log(x) + b, aes(colour = 'logarithmic'), se = FALSE, method.args = list(start = list(a = 1, b = 1))) +
  theme_bw() +
  scale_colour_brewer(name = 'Trendline', palette = 'Set2')

##########################################################################
#take that data set section of interest - 
#where locations are captured between teh means plus 50px beyond either side
shortenedData <- subset(unClearConfidence, flippedLocation > 175 & flippedLocation < 525)

#plot linear regression line
my.formula <- y ~ x
ggplot(shortenedData, aes(x=flippedLocation, y=confidence, linetype=distribution)) + geom_point() +
  geom_smooth(method="lm", se=FALSE,  formula = my.formula) +
  facet_wrap(~distribution)

ggplot(shortenedData, aes(x=flippedLocation, y=confidence, color=distribution)) + geom_point() +
  geom_smooth(method="lm", se=FALSE,  formula = my.formula) +
  facet_wrap(~PID)


#plot 2nd order polynomial
my.formula <- y ~ poly(x, 2)
ggplot(shortenedData, aes(x=flippedLocation, y=confidence, linetype=distribution)) + geom_point() +
  geom_smooth(method = "lm", formula = my.formula, se=FALSE,)+
  facet_wrap(~distribution)
ggplot(shortenedData, aes(x=flippedLocation, y=confidence, color=distribution)) + geom_point() +
  geom_smooth(method="lm", se=FALSE,  formula = my.formula) +
  facet_wrap(~PID)

#plot 3rd order polynomial....
my.formula <- y ~ poly(x, 3)
ggplot(shortenedData, aes(x=flippedLocation, y=confidence, color=distribution)) + geom_point() +
  geom_smooth(method="lm", se=FALSE,  formula = my.formula) +
  facet_wrap(~PID)

#how to fit sigmoid instead? 
p1 <- nls(confidence~SSlogis(flippedLocation,Asym,xmid,scal),data=shortenedData,
          subset=(distribution=="narrow")
          ## , weights=1/std_dev^2  ## error in qr.default: NA/NaN/Inf ...
) #unweighted


#install.packages("nls2")
library(nls2)
#calculate the STD for weighting
p1B <- nls2(confidence~SSlogis(flippedLocation,Asym,xmid,scal),data=shortenedData,
            subset=(distribution =="narrow"))

p2 <- update(p1,subset=(distribution=="wide"))
p2B <- update(p1B,subset=(distribution=="wide"))

pframe0 <- data.frame(confidence=10^seq(log10(min(shortenedData$confidence)),log10(max(shortenedData$confidence)), length.out=100))
pp <- rbind(
  data.frame(pframe0,mean_response=predict(p1,pframe0),
             drug="drug_1",wts=FALSE),
  data.frame(pframe0,mean_response=predict(p2,pframe0),
             drug="drug_2",wts=FALSE),
  data.frame(pframe0,mean_response=predict(p1B,pframe0),
             drug="drug_1",wts=TRUE),
  data.frame(pframe0,mean_response=predict(p2B,pframe0),
             drug="drug_2",wts=TRUE)
)

library(ggplot2); theme_set(theme_bw())
(ggplot(df,aes(conc,mean_response,colour=drug)) +
    geom_pointrange(aes(ymin=mean_response-std_dev,
                        ymax=mean_response+std_dev)) +
    scale_x_log10() +
    geom_line(data=pp,aes(linetype=wts),size=2)
)
#split data by distribution
#fit a regression line
#participants as a raandom variable (latent variable/)_
#intereted int eh aprmaeter of steepness between 


wide <- subset(unClearConfidence, distribution == "wide")
#for each participant, take location (flipped from 175 - 525) and confidence
#fit a regression line
#participants as a raandom variable (latent variable/)_
#intereted int eh aprmaeter of steepness between 

####################################################

#dummy test - thi corelation is not valid due to location spread confounds
#correlate confidence with flipped distance from the bound (total distance)
correlation <- cor.test(unClearConfidence$flipped_distance_to_bound, unClearConfidence$confidence,  method = "pearson")
correlation #significant, highly as expected. 
#now test the difference between the correlatiosn for each distribution
correlationWide <- cor.test(wideConf$flipped_distance_to_bound, wideConf$confidence,  method = "pearson")
correlationWide
rhoWide <- FisherZ(correlationWide$estimate) #transform to a z score to compare

correlationNarrow <-cor.test(narrowConf$flipped_distance_to_bound, narrowConf$confidence,  method = "pearson")
correlationNarrow
rhoNarrow <- FisherZ(correlationNarrow$estimate) #transform to a z score to compare
#the confidence intervals overlap - not different. 

#look at difference in correlations 
#moving away for the midline (at 350) to the category means (comparable for distance)
cutoffwideConf <- subset(wideConf, flippedLocation< 500 & flippedLocation > 350)
cutoffnarrowConf <- subset(narrowConf, flippedLocation > 200 & flippedLocation < 350)

WIDE <- cutoffwideConf %>%
  group_by(PID,binnedDistance)%>% 
  summarise(grp.mean = mean(confidence))
WIDE$group <- "wide"
WIDE$binnedDistance <- as.numeric(as.character(WIDE$binnedDistance))
NARROW <- cutoffnarrowConf %>%
  group_by(PID,binnedDistance)%>% 
  summarise(grp.mean = mean(confidence))
NARROW$group <- "narrow"
NARROW$binnedDistance <- as.numeric(as.character(NARROW$binnedDistance))

wideLM <- lm(binnedDistance~ grp.mean, data = WIDE)
wideLM
narrowLM <- lm(binnedDistance~grp.mean,data = NARROW)
narrowLM


