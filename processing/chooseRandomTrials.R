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

rowsToSample = d2Orig[d2Orig$week==1,]$X
rowsToSample = rowsToSample[!rowsToSample %in% d2$X]

d2b = d2Orig[match(rowsToSample,d2Orig$X),]


d2b$weight = 0.0000001



d2b[d2b$TryMarked,]$weight = 0.290^4
d2b[d2b$CandidateUnderstanding=="Yes",]$weight = 3^4
d2b[d2b$T0,]$weight = 0.533^4
d2b[d2b$T_minus_1,]$weight = 0.5619^4
d2b[d2b$Teach,]$weight = 1.5^4

d2b$weight = 0.0000001

d2b$weight = (as.numeric(d2b$TryMarked) * 0.01) +
             (as.numeric(d2b$CandidateUnderstanding=="Yes") * 0.9523) +
             (as.numeric(d2b$T0) * 0.733) +
             (as.numeric(d2b$T_minus_1) * 0.7619) +
             (as.numeric(d2b$Teach) * 2.9761)

d2b$weight = d2b$weight^3

set.seed(33278)
d2b = d2b[sample(1:nrow(d2b), 20, prob = d2b$weight),]

# try marking fine
# more teaching and CU
table(d2b$Teach)
table(d2b$T0)
table(d2b$CandidateUnderstanding)
table(d2b$TryMarked)
table(d2b$T_minus_1)

d2b = d2b[,c("X","filename",'trialID','trial_value','sign_start','sign_end','speakerName')]

d2b$sign_startms = d2b$sign_start
d2b$sign_start = millisecondsToReadbleTime(d2b$sign_start)
d2b$sign_endms = d2b$sign_end
d2b$sign_end = millisecondsToReadbleTime(d2b$sign_end)

d2b = d2b[order(d2b$filename,d2b$sign_start),]

write.csv(d2b, "../processing/RandomVariants_SecondRound.csv")


##
# ROUND 3

d3 = d2Orig[(!d2Orig$X %in% d2b$X) & (!d2Orig$X %in% sel$X),]
d3 = d3[grepl("one_week",d3$filename),]

# Select 50
set.seed(3289)
d3 = d3[sample(1:nrow(d3),50),]
# Random order
d3 = d3[sample(1:nrow(d3)),]

write.csv(d3[,c("X","filename",'trialID','trial_value','sign_start','sign_end','speakerName')],
          "../processing/RandomVariants_ThirdRound.csv",
          fileEncoding = 'utf-8')


#####
d3 = d2Orig[(!d2Orig$X %in% d2b$X) & (!d2Orig$X %in% sel$X),]
d3 = d3[grepl("one_week",d3$filename),]


nx = 6

set.seed(23897)

TryMarkedT = sample(d3[d3$TryMarked,]$X,nx)
TryMarkedF = sample(d3[!d3$TryMarked,]$X,nx)
T0T = sample(d3[d3$T0,]$X,nx)
T0F = sample(d3[!d3$T0,]$X,nx)
T_minus_1T = sample(d3[d3$T_minus_1,]$X,nx)
T_minus_1F = sample(d3[!d3$T_minus_1,]$X,nx)
TeachT = sample(d3[d3$Teach,]$X,4)
TeachF = sample(d3[!d3$Teach,]$X,nx)

chosenTurns = c(TryMarkedT,TryMarkedF,
  T0T,T0F,
  T_minus_1T ,T_minus_1F,
  TeachT, TeachF)

turns_for_sean = d3[d3$X %in% chosenTurns,
                      c("X","filename",'trialID','trial_value','sign_start','sign_end','speakerName',
                        "TryMarked","T0","T_minus_1","Teach")]

turns_for_sean = turns_for_sean[order(turns_for_sean$filename,turns_for_sean$sign_start),]

turns_for_connie = turns_for_sean
turns_for_connie$TryMarked = "?"
turns_for_connie$T0 = "?"
turns_for_connie$T_minus_1 = "?"
turns_for_connie$Teach = "?"

turns_for_connie$sign_start2 = millisecondsToReadbleTime(turns_for_connie$sign_start)
turns_for_connie$sign_end2 = millisecondsToReadbleTime(turns_for_connie$sign_end)

write.csv(turns_for_connie,"RandomVariants_Balanced_ForConnie.csv")
write.csv(turns_for_sean,"RandomVariants_Balanced_ForSean.csv")
