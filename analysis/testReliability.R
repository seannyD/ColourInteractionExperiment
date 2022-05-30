library(psych)
try(setwd("~/Documents/MPI/KangSukColours/ColourExperiment/processing/"))

millisecondsToReadbleTime = function(t){
  seconds = floor((t/1000)) %% 60
  minutes = floor((t/1000)/60)
  
  #spoint = paste0(seconds,substr(t/1000 - floor(floor((t/1000))),2,4))
  return(paste(minutes,"min",seconds,"sec"))
}

d = read.csv("../data/processedData/variants_processed.csv", stringsAsFactors = F)

colourNumbers = as.numeric(c("1","5","7","14",'18','24','6'))

d = d[d$trial_value %in% colourNumbers,]

rc = read.delim("../data/reliability/RandomVariants21oktober2017.txt",sep="\t", stringsAsFactors = F)
rc = rc[rc$Try.marking!="No coding",]
rc2 = read.delim("../data/reliability/RandomVariants3December2017.txt",sep="\t", stringsAsFactors = F)
names(rc2)[names(rc2)=="X.2"] = "Comments"
if(!"Candidate.Understanding" %in% names(rc2)){
  rc2$Candidate.Understanding = NA
}

rc2 = rc2[,names(rc)]

rc = rbind(rc,rc2)

for(v in c("Try.marking","T0",'T.1', 'Teaching', 'Candidate.Understanding')){
  print(v)
  rc[!is.na(rc[,v]) & rc[,v]=="YES",v] = TRUE
  rc[!is.na(rc[,v]) & rc[,v]=="NO",v] = FALSE
  rc[(!is.na(rc[,v])) & (!rc[,v]%in% c(TRUE,FALSE)),v] = NA
  rc[,v] = rc[,v]=="TRUE"
}

# Check matching of cases is correct
rc$sign_start == millisecondsToReadbleTime(d[match(rc$X, d$X),]$sign_start)
rc$sign_start_milli = d[match(rc$X, d$X),]$sign_start
rc$sign_end_milli = d[match(rc$X, d$X),]$sign_end

rc$KS.Try.marking = d[match(rc$X, d$X),]$TryMarked
rc$KS.T0 = d[match(rc$X, d$X),]$T0
rc$KS.T.1 = d[match(rc$X, d$X),]$T_minus_1
rc$KS.Teaching = d[match(rc$X, d$X),]$Teach
rc$KS.Candidate.Understanding = d[match(rc$X, d$X),]$CandidateUnderstanding=="Yes"

###
# Disagreements
rc$Disagree_TryMark = rc$Try.marking != rc$KS.Try.marking
rc$Disagree_TryMark[is.na(rc$Disagree_TryMark)] = F
rc$Disagree_T0 = rc$T0 != rc$KS.T0
rc$Disagree_T0[is.na(rc$Disagree_T0)] = F
rc$Disagree_T.1 = rc$T.1 != rc$KS.T.1
rc$Disagree_T.1[is.na(rc$Disagree_T.1)] = F
rc$Disagree_Teach = rc$Teaching != rc$KS.Teaching
rc$Disagree_Teach[is.na(rc$Disagree_Teach)] = F

rc$Disagree = rc$Disagree_TryMark | rc$Disagree_T0 | rc$Disagree_T.1 |rc$Disagree_Teach

rc$Disagree = c("No","Yes")[rc$Disagree+1]

rc$Disagree_What = paste(
paste(c("","TryMarking")[as.numeric(1+rc$Disagree_TryMark)]),
paste(c("","T-1")[as.numeric(1+rc$Disagree_T.1)]),
paste(c("","T0")[as.numeric(1+rc$Disagree_T0)]),
paste(c("","Teaching")[as.numeric(1+rc$Disagree_Teach)])
)

rc$Disagree_What = gsub("NA","",rc$Disagree_What)
rc$Disagree_What = gsub("([a-z0-9]) +([A-Z])","\\1, \\2",rc$Disagree_What)
rc$Disagree_What = gsub("^ +","",rc$Disagree_What)

rcd = rc[,c("X","filename","trialID","trial_value","sign_start","sign_end",
            "sign_start_milli","sign_end_milli",
            "speakerName",
      "KS.Try.marking","Try.marking",
      "KS.T.1","T.1",
      "KS.T0","T0",
      "KS.Teaching","Teaching",
      "Disagree","Disagree_What")]
rcd = rcd[!is.na(rcd$Try.marking),]

write.csv(rcd,"../data/reliability/KS_v_CD.csv",row.names = F)

#########



table(rc$KS.Try.marking, rc$Try.marking)
tm = cbind(rc$KS.Try.marking, rc$Try.marking)
tm = tm[complete.cases(tm),]
cohen.kappa(tm)

table(rc$KS.T0, rc$T0)
t0 = cbind(rc$KS.T0, rc$T0)
t0 = t0[complete.cases(t0),]
cohen.kappa(t0)

table(rc$KS.T.1, rc$T.1)
tm1 = cbind(rc$KS.T.1, rc$T.1)
tm1 = tm1[complete.cases(tm1),]
cohen.kappa(tm1)

table(rc$KS.Teaching, rc$Teaching)
tteaching = cbind(rc$KS.Teaching, rc$Teaching)
tteaching = tteaching[complete.cases(tteaching),]
cohen.kappa(tteaching)

table(rc$KS.Candidate.Understanding, rc$Candidate.Understanding)
tcu = cbind(rc$KS.Candidate.Understanding, rc$Candidate.Understanding)
tcu = tcu[complete.cases(tcu),]
cohen.kappa(tcu)

#write.csv(rc, "")

scu = prop.table(table(rc$KS.Candidate.Understanding))[1]
st0 = prop.table(table(rc$KS.T0))[1]
st1 = prop.table(table(rc$KS.T.1))[1]
stt = prop.table(table(rc$KS.Teaching))[1]
str = prop.table(table(rc$KS.Try.marking))[1]

scu
st0
st1
stt
str


rc$CU.disagree = !is.na(rc$Candidate.Understanding) & rc$KS.Candidate.Understanding!=rc$Candidate.Understanding 
rc$T0.disagree = !is.na(rc$T0) & rc$KS.T0!=rc$T0
rc$T.1.disagree = !is.na(rc$T.1) & rc$KS.T.1!=rc$T.1
rc$Teaching.disagree = !is.na(rc$Teaching) & rc$KS.Teaching!=rc$Teaching
rc$Try.marking.disagree = !is.na(rc$Try.marking) & rc$KS.Try.marking!=rc$Try.marking

rc.disagree = rc[rc$CU.disagree | rc$T0.disagree | rc$T.1.disagree | rc$Teaching.disagree | rc$Try.marking.disagree,]

rc.disagree = rc.disagree[order(rc.disagree$T.1.disagree, rc.disagree$CU.disagree, rc.disagree$Try.marking, rc.disagree$Teaching, rc.disagree$T0, decreasing = T),]

write.csv(rc.disagree,"../data/reliability/Disagreements.csv")


#####

orig = read.csv("../data/reliability/RandomVariants_Balanced_ForSean.csv",stringsAsFactors = F)
chk = read.csv("../data/reliability/RandomVariants_Balanced_CHECKED.csv",stringsAsFactors = F,skip = 1)
chk = chk[,1:14]
chk = chk[match(orig$X,chk$X),]
all(chk$X == orig$X)

chk[chk=="?"] = NA
chk[chk=="N"] = FALSE
chk[chk=="Y"] = TRUE

#orig = orig[!orig$filename %in% c("Colour_game_01_3_Indonesia-India_After_one_week.eaf","Colour_game_01_3_Jordan-Nepal_After_one_week.eaf"),]
#chk = chk[!chk$filename %in% c("Colour_game_01_3_Indonesia-India_After_one_week.eaf","Colour_game_01_3_Jordan-Nepal_After_one_week.eaf"),]

names(chk)[names(chk)=="Tminus1"] = "T_minus_1"

properties = c("TryMarked","T0","T_minus_1","Teach")
trueVals = c()
for(p in properties){
  print(p)
  print(table(orig[,p], chk[,p]))
  tm = cbind(orig[,p], chk[,p])
  tm = tm[complete.cases(tm),]
  ck = cohen.kappa(tm)
  print(ck)
  print("------")
  trueVals = c(trueVals,ck$kappa)
}

for(p in properties){
x = c()
for(i in 1:1000){
  tm = cbind(orig[,p], sample(chk[,p]))
  tm = tm[complete.cases(tm),]
  ck = cohen.kappa(tm)
  x = c(x,ck$kappa)
}
hist(x,main = p)
}

for(j in 1:length(properties)){
  p = properties[j]
  x = c()
  for(i in 1:1000){
    tm = cbind(orig[,p], sample(c(T,F),replace = T,size = length(orig[,p])))
    tm = tm[complete.cases(tm),]
    ck = cohen.kappa(tm)
    x = c(x,ck$kappa)
  }
  hist(x,main = p)
  abline(v=trueVals[j],col=2)
}