setwd("~/Documents/MPI/KangSukColours/ColourExperiment/processing/")

millisecondsToReadbleTime = function(t){
  seconds = floor((t/1000)) %% 60
  minutes = floor((t/1000)/60)
  return(paste(minutes,"min",seconds,"sec"))
}

d = read.csv("../data/processedData/variants_processed.csv", stringsAsFactors = F)

colourNumbers = as.numeric(c("1","5","7","14",'18','24','6'))

d = d[d$trial_value %in% colourNumbers,]

d$trialString = paste(d$filename, d$trialID, d$trial_start)


##

totalTime = sum(tapply(d$trial_length, d$trialString, head,n=1))

(totalTime/1000)/60 #minutes

totalTime_w1 = sum(tapply(d[d$week==1,]$trial_length, d[d$week==1,]$trialString, head,n=1))

sampleTime = totalTime*0.2 #seconds

selTime = 0
selectedTrials = data.frame(filename="",start=0,end=0, stringsAsFactors = F)
set.seed(23389)
while(selTime < sampleTime){
  selectedTrial = d[sample(1:nrow(d),1),]
  selectedTrials = rbind(
    selectedTrials, 
    c(selectedTrial$filename, 
    millisecondsToReadbleTime(selectedTrial$trial_start),
    millisecondsToReadbleTime(selectedTrial$trial_end))
  )
  selTime = selTime + selectedTrial$trial_length
}
selectedTrials = selectedTrials[2:nrow(selectedTrials),]

write.csv(selectedTrials,file="../processing/RandomTrialsForCoding_byTime.csv")

dr = d[d$week==1 & d$trial_value %in% colourNumbers,]
set.seed(2387)
rw1 = sample(unique(dr$trialString),10)
             #ceiling(length(unique(dr$trialString))*0.1))
rw1[1] = sample(unique(dr[dr$trial_value==14,]$trialString),1)

sel = d[d$trialString %in% rw1,]

ransel = data.frame(
    filename = sel$filename, 
    colour = sel$trial_value,
    start=millisecondsToReadbleTime(sel$trial_start),
    end=millisecondsToReadbleTime(sel$trial_end))

ransel = ransel[!duplicated(ransel),]



write.csv(ransel,"../processing/RandomTrials.csv")

###

# Random variants

d2 = d[d$trial_value %in% colourNumbers,]

d2 = d2[d2$trial_value !=6,]
d2 = d2[!d2$sign_value %in%
                      c("SAME",
                        "DIFFERENT",
                        "DO NOT UNDERSTAND",
                        "UNDERSTAND",
                        "ME",
                        "YOU",
                        "YES",
                        "NOT",
                        "SIGNING"),]


dim(d2)
# There are 489 variant tokens coded
sel.n = 42

d2Orig = d2

set.seed(2398)
d2 = d2[sample(1:nrow(d2),sel.n),]

d2 = d2[,c("X","filename",'trialID','trial_value','sign_start','sign_end','speakerName')]

d2$sign_startms = d2$sign_start
d2$sign_start = millisecondsToReadbleTime(d2$sign_start)
d2$sign_endms = d2$sign_end
d2$sign_end = millisecondsToReadbleTime(d2$sign_end)

d2 = d2[order(d2$filename,d2$sign_start),]

write.csv(d2, "../processing/RandomVariants.csv")

rowsToSample = d2Orig[d2Orig$week==4,]$X
rowsToSample = rowsToSample[!rowsToSample %in% d2$X]
set.seed(3278)
rowsToSample = sample(rowsToSample, sel.n)
d2b = d2Orig[match(rowsToSample,d2Orig$X),]

d2b = d2b[,c("X","filename",'trialID','trial_value','sign_start','sign_end','speakerName')]

d2b$sign_startms = d2b$sign_start
d2b$sign_start = millisecondsToReadbleTime(d2b$sign_start)
d2b$sign_endms = d2b$sign_end
d2b$sign_end = millisecondsToReadbleTime(d2b$sign_end)

d2b = d2b[order(d2b$filename,d2b$sign_start),]

write.csv(d2b, "../processing/RandomVariants_SecondRound.csv")
