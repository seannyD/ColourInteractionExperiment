setwd("~/Documents/MPI/KangSukColours/ColourExperiment/processing/")


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

  
d = read.csv("../data/processedData/variants.csv", stringsAsFactors = F, quote='')

d[,c("week",'session','part1','part2')] = t(sapply(d$filename,getDetails))

d$week = as.numeric(d$week)

d = d[order(d$week,d$session,d$sign_start),]

# make sure that all iconicity ratings are the same
d$iconic[d$iconic==""] = NA
ndiff = tapply(d$iconic, d$sign_value, function(x){(length(unique(x[!is.na(x)])))})
names(ndiff) = tapply(d$sign_value, d$sign_value, function(x){head(x,1)})
ndiff[ndiff>1]

iconicityMeasures = tapply(d$iconic, d$sign_value, checkIconicity)
d$iconic = iconicityMeasures[d$sign_value]


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



d$speakerName = sapply(1:nrow(d),function(X){
  d[X,c("part1","part2")][,d$speaker[X]]
  })

inventedBy = tapply( d$speakerName, d$sign_value, head,n=1)  
d$inventedBy = inventedBy[d$sign_value]  




d$IX = grepl("IX",d$sign_notes)
d$OS = grepl("OS",d$sign_notes)
d$LW = grepl("LW",d$sign_notes)
d$SE = grepl("SE",d$sign_notes)


d$freq_week_4 = tapply(d[d$week==4,]$sign_value,d[d$week==4,]$sign_value, length)[d$sign_value]

variants = data.frame()
for(colourID in c("18","19")){
  d2 =d[d$trial_value==colourID,]
  
  v = data.frame(colour=colourID,sign=unique(d2[d2$week==1,]$sign_value[nchar(d2[d2$week==1,]$sign_value)>0]), stringsAsFactors = F)
  
  v$freq_week_1 = table(d2[d2$week==1,]$sign_value)[v$sign]
  v$freq_week_4 = table(d2[d2$week==4,]$sign_value)[v$sign]
  v$iconic = tapply(d2$iconic, d2$sign_value, head,n=1)[v$sign]
  v$check = tapply(d2[d2$week==1,]$T0Check,d2[d2$week==1,]$sign_value,function(X){sum(X,na.rm=T)>0})[v$sign]
  v$inventedBy= tapply(d2$inventedBy, d2$sign_value, head, n=1)[v$sign]
  
  variants = rbind(variants,v)

}

variants$iconic[is.na(variants$iconic)] = "No"
variants$freq_week_4[is.na(variants$freq_week_4)] = 0

colourNumbers = c("1","5","7","14",'18','24')

allTargetVars = sort(unique(d[d$trial_value %in% colourNumbers,]$sign_value))

cat(
  paste(paste('"',allTargetVars,'"',sep=''),collapse='\n'),
  file="../processing/ListOfVariants.txt")

write.csv(d, file="../data/processedData/variants_processed.csv")
