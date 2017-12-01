library(psych)
setwd("~/Documents/MPI/KangSukColours/ColourExperiment/processing/")

millisecondsToReadbleTime = function(t){
  seconds = floor((t/1000)) %% 60
  minutes = floor((t/1000)/60)
  return(paste(minutes,"min",seconds,"sec"))
}

d = read.csv("../data/processedData/variants_processed.csv", stringsAsFactors = F)

colourNumbers = as.numeric(c("1","5","7","14",'18','24','6'))

d = d[d$trial_value %in% colourNumbers,]

rc = read.delim("../data/reliability/RandomVariants21oktober2017.txt",sep="\t", stringsAsFactors = F)

for(v in c("Try.marking","T0",'T.1', 'Teaching', 'Candidate.Understanding')){
  print(v)
  rc[rc[,v]=="YES",v] = TRUE
  rc[rc[,v]=="NO",v] = FALSE
  rc[!rc[,v]%in% c(TRUE,FALSE),v] = NA
  rc[,v] = rc[,v]=="TRUE"
}


rc$KS.Try.marking = d[match(rc$X, d$X),]$TryMarked
rc$KS.T0 = d[match(rc$X, d$X),]$T0
rc$KS.T.1 = d[match(rc$X, d$X),]$T_minus_1
rc$KS.Teaching = d[match(rc$X, d$X),]$Teach
rc$KS.Candidate.Understanding = d[match(rc$X, d$X),]$CandidateUnderstanding=="Yes"

table(rc$KS.Try.marking, rc$Try.marking)
tm = cbind(rc$KS.Try.marking, rc$Try.marking)
tm = tm[complete.cases(tm),]
cohen.kappa(tm)

table(rc$KS.T0, rc$T0)
t0 = cbind(rc$KS.T0, rc$T0)
t0 = t0[complete.cases(t0),]
cohen.kappa(t0)

tm1 = cbind(rc$KS.T.1, rc$T.1)
tm1 = tm1[complete.cases(tm1),]
cohen.kappa(tm1)

tteaching = cbind(rc$KS.Teaching, rc$Teaching)
tteaching = tteaching[complete.cases(tteaching),]
cohen.kappa(tteaching)

tcu = cbind(rc$KS.Candidate.Understanding, rc$Candidate.Understanding)
tcu = tcu[complete.cases(tcu),]
cohen.kappa(tcu)

#write.csv(rc, "")
