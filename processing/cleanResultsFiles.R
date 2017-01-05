setwd("~/Documents/MPI/KangSukColours/ColourExperiment/processing/")


colourNumbers = c("1","5","7","14",'18','24','6')
colourNames = c("red",'brown','black','green','yellow','pink','white')
names(colourNames) = colourNumbers
colourNamesDark = c("dark red", 'orange', 'dark gray', 'dark green','gold', 'purple','gray')


checkIconicity = function(x){
  tx = table(x[x!=''])
  
  if(length(tx)==0){
    return(NA)
  }
  
  if(length(tx)==1){
    return(names(tx)[1])
  } else{
    print(paste("Warning: different iconicity values for sign"))
    print(tx)
    return(names(sort(tx,decreasing = T))[1])
  }
}

getDetails = function(x){
  x = strsplit(x,"_")[[1]]
  y = strsplit(x[5],"-")[[1]]
  return(c(week=as.numeric(x[3]), session=as.numeric(x[4]),y[1],y[2]))
}

findOverlaps = function(rangesA){
  
  overlaps = sapply(1:nrow(rangesA), function(i){
    # if start or end is within range, then findInterval returns 1,
    # so multiplying should only return 1 if BOTH are
    startO= findInterval(rangesA[,1], c(rangesA[i,1],rangesA[i,2])) 
    endO = findInterval(rangesA[,2], c(rangesA[i,1],rangesA[i,2]))
    (startO==1) | (endO==1)
  })
  ret = data.frame(which(overlaps==1, arr.ind = T))
  ret = ret[ret[,1]!=ret[,2],]
  if(nrow(ret)==0){
    return(NA)
  }
  ret = data.frame(t(apply(ret, 1, function(X){
    c(rangesA[X[1],], rangesA[X[2],])
  })))
  ret = ret[(ret[,2] - ret[,3]) != 0,]
  names(ret) = c("TrialA_start","TrialA_end","TrialB_start","TrialB_end")
  return(ret)
}

  
d = read.csv("../data/processedData/variants.csv", stringsAsFactors = F, quote='')

d[,c("week",'session','part1','part2')] = t(sapply(d$filename,getDetails))

d$week = as.numeric(d$week)

d = d[order(d$week,d$session,d$sign_start),]


#d = d[d$sign_value!='SAME',]
d = d[d$sign_value!='',]
d = d[d$sign_value!='?',]
d$sign_value = gsub("^ ","", d$sign_value)
d$sign_value = gsub(" $","", d$sign_value)
d$sign_value = gsub("[!\\?\t]","", d$sign_value)
d$sign_value = toupper(d$sign_value)

# Edit signs
source("signChanges.R")

for(i in 1:length(signChanges)){
 if(signChanges[[i]][1] %in% d$sign_value){
    d[d$sign_value==signChanges[[i]][1]
      & !is.na(d$sign_value),]$sign_value = signChanges[[i]][2]
  }
}

d = d[!is.na(d$sign_value),]

d$sign_value = gsub("^ ","", d$sign_value)
d$sign_value = gsub(" $","", d$sign_value)
d$sign_value = gsub("[!\\?\t]","", d$sign_value)
d$sign_value = toupper(d$sign_value)


# Edit colour trial names

d$trial_value = gsub("[^0-9]+"," ",d$trial_value)
d$trial_value = sapply(d$trial_value, function(X){
  strsplit(X," ")[[1]][1]
})

#####
# Find any trials that are overlapping
overlaps = sapply(unique(d$filename),function(X){
  dx = d[d$filename==X,]
  dx = dx[!duplicated(dx$trial_start),]
  findOverlaps(cbind(dx$trial_start,dx$trial_end))
})

overlaps[!is.na(overlaps)]

# make sure that all iconicity ratings are the same
d$iconic[d$iconic==""] = NA
ndiff = tapply(d$iconic, d$sign_value, function(x){(length(unique(x[!is.na(x)])))})
names(ndiff) = tapply(d$sign_value, d$sign_value, function(x){head(x,1)})
ndiff[ndiff>1]

iconicityMeasures = tapply(d$iconic, d$sign_value, checkIconicity)
d$iconic = iconicityMeasures[d$sign_value]

# Check indexicality
# One case:

d[d$sign_value=="WHITE",]$Indexicality = "Yes-body"

d$Indexicality[d$Indexicality==""] = "Yes"
d$Indexicality[d$Indexicality=="yes"] = "Yes"
d$Indexicality[d$Indexicality=="no"] = "No"
d$Indexicality[d$Indexicality=="yes-body"] = "Yes-body"

#  Try marking

d$TryMarked = d$TryMarked=="Yes"
d$TryMarked[is.na(d$TryMarked)] = F

# Teaching
d$Teach = d$Teach == "Yes"
d$Teach[is.na(d$Teach)] = F

# trial lengths
trialLengths = tapply(d$trial_length,d$trial_start,head,n=1)/1000
trialStart = tapply(d$trial_start,d$trial_start,head,n=1)/1000

plot(trialLengths~trialStart)

plot(d$sign_length~d$sign_start)

#boxplot(d$sign_length~d$iconic, ylab='Sign duration', xlab='Iconic')

d2 = d[order(d$week,d$session, d$sign_start),]
sel = d2$trial_value=='19' & !d2$T0Check & nchar(d2$sign_value)>0
signs  = unique(d2[sel,]$sign_value)
set.seed(1211)
cols = sample(rainbow(length(signs)))
names(cols) = signs
plot(rank(d2[sel,]$sign_start),rep(1,sum(sel)), col=cols[d2[sel,]$sign_value], pch=16)

tapply(d$sign_value, d$week, function(X){length(unique(X))})

d$speaker = as.numeric(d$speaker)

d$speakerName = sapply(1:nrow(d),function(X){
  d[X,c("part1","part2")][,d$speaker[X]]
  })

inventedBy = tapply( d$speakerName, d$sign_value, head,n=1)  
d$inventedBy = inventedBy[d$sign_value]  




d$IX = grepl("IX",d$sign_notes)
d$OS = grepl("OS",d$sign_notes)
d$LW = grepl("LW",d$sign_notes)
d$SE = grepl("SE",d$sign_notes)

# absolute frequency
# Note that this includes the use of variants in all target colour contexts
d$freq_week_1_total = table(d[d$week==1,]$sign_value)[d$sign_value]
d$freq_week_1_total[is.na(d$freq_week_1_total)] = 0

d$freq_week_4_total = table(d[d$week==4,]$sign_value)[d$sign_value]
d$freq_week_4_total[is.na(d$freq_week_4_total)] = 0

# absolute frequency used in each colour context
d$freq_week4_withinColour = NA

for(colx in colourNumbers){
  d$freq_week4_withinColour[d$trial_value==colx] = 0
  dx = d[d$trial_value==colx & d$week==4, ]
  tx = table(dx$sign_value)
  d[d$trial_value==colx & d$week==4,]$freq_week4_withinColour = tx[d[d$trial_value==colx & d$week==4,]$sign_value]
}

# relative frequency, compared to other competitiors for the same colour
d$prop_week4_withinColour = NA

for(colx in colourNumbers){
  d$prop_week4_withinColour[d$trial_value==colx] = 0
  dx = d[d$trial_value==colx & d$week==4, ]
  tx = table(dx$sign_value)
  tx = tx / sum(tx)
  d[d$trial_value==colx & d$week==4,]$prop_week4_withinColour = tx[d[d$trial_value==colx & d$week==4,]$sign_value]
}

variants = data.frame()
# For each target colour
for(colourID in colourNumbers){
  # select trials for that target colour
  d2 =d[d$trial_value==colourID,]
  
  # make a data frame for variants used for this colour
  v = data.frame(colour=colourID, colourName = colourNames[which(colourNumbers==colourID)],sign=unique(d2[d2$week==1,]$sign_value), stringsAsFactors = F)
  
  # Total frequency of use (all colour contexts)
  v$freq_week_1_total = table(d[d$week==1,]$sign_value)[v$sign]
  v$freq_week_4_total = table(d[d$week==4,]$sign_value)[v$sign]
  
  # count occurances in both weeks, only when referring to this colour
  v$freq_week_1 = table(d2[d2$week==1,]$sign_value)[v$sign]
  v$freq_week_4 = table(d2[d2$week==4,]$sign_value)[v$sign]
  
  v$freq_week_1[is.na(v$freq_week_1)] = 0
  v$freq_week_4[is.na(v$freq_week_4)] = 0
  
  v$prop_week_1 = 0
  v$prop_week_4 = 0
  
  signCounts = table(d2[d2$week==1,]$sign_value)
  signCounts = signCounts / sum(signCounts)
    
  v[v$sign %in% names(signCounts),]$prop_week_1 = signCounts[v[v$sign %in% names(signCounts),]$sign]
  
  signCounts = table(d2[d2$week==4,]$sign_value)
  signCounts = signCounts / sum(signCounts)
  
  v[v$sign %in% names(signCounts),]$prop_week_4 = signCounts[v[v$sign %in% names(signCounts),]$sign]
  
  v$prop_week_1[is.na(v$prop_week_1)] = 0
  v$prop_week_4[is.na(v$prop_week_4)] = 0
  
  v$origin = tapply(d2$signOrigin, d2$sign_value, function(X){
    X= X[!is.na(X)]
    X = X[X!=""]
    if(length(X)==0){
      return("None")
    }
    if(length(unique(X))>1){
      return("Mixed")
    }
    return(X[1])
  })[v$sign]
  
  # Iconicity - no changes over the different weeks
  v$iconic = tapply(d2$iconic, d2$sign_value, head,n=1)[v$sign]
  
  # Checking
  #v$check = tapply(d2[d2$week==1,]$T0Check,d2[d2$week==1,]$sign_value,function(X){sum(X,na.rm=T)>0})[v$sign]
  v$check = tapply(d2[d2$week==1,]$T0Check,d2[d2$week==1,]$sign_value,sum, na.rm=T)[v$sign]
  
  # Indexical
  indexical = tapply(d2$Indexicality,d2$sign_value,function(X){
      x = table(X)
      names(x)[which(x==max(x))[1]]
    })
  #indexical[is.null(indexical)] = "No"
  v$indexical = unlist(indexical)[v$sign]
  v$indexical[is.na(v$indexical)] = "No"
  
  # Invented by
  v$inventedBy= tapply(d2$inventedBy, d2$sign_value, head, n=1)[v$sign]
  
  # Try marked
  v$TryMarked = tapply(d2[d2$week==1,]$TryMarked, d2[d2$week==1,]$sign_value, sum, na.rm=T)[v$sign]

  # Teaching
  v$Teach = tapply(d2[d2$week==1,]$Teach, d2[d2$week==1,]$sign_value, sum, na.rm=T)[v$sign]
  
  # Average length
  v$averageLength_week_1 = tapply(d2[d2$week==1,]$sign_length, d2[d2$week==1,]$sign_value, mean)
  # Average trial Length
  v$averageTrialLength_week_1 = tapply(d2[d2$week==1,]$trial_length, d2[d2$week==1,]$sign_value, mean)
  
  variants = rbind(variants,v)
  
}

variants$iconic[is.na(variants$iconic)] = "No"
variants$freq_week_4[is.na(variants$freq_week_4)] = 0

b = read.delim("../data/otherData/SignProperties.tab", sep='\t', encoding = 'utf-8', stringsAsFactors = F)

b$Sign = gsub("^ ","", b$Sign)
b$Sign = gsub(" $","", b$Sign)
b$Sign = toupper(b$Sign)

for(i in 1:length(signChanges)){
  if(signChanges[[i]][1] %in% b$Sign){
    b[b$Sign==signChanges[[i]][1]
      & !is.na(b$Sign),]$Sign = signChanges[[i]][2]
  }
}

variants$BodyAnchor = b[match(variants$sign, b$Sign),]$Body.Anchor

write.csv(variants,file='../data/processedData/variants_summary.csv')

allTargetVars = sort(unique(d[d$trial_value %in% colourNumbers,]$sign_value))

cat(
  paste(paste('"',allTargetVars,'"',sep=''),collapse='\n'),
  file="../processing/ListOfVariants.txt")

write.csv(d, file="../data/processedData/variants_processed.csv")

