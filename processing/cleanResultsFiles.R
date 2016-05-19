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
iconicityMeasures = tapply(d$iconic, d$sign_value, checkIconicity)
d$iconic = iconicityMeasures[d$sign_value]

trialLengths = tapply(d$trial_length,d$trial_start,head,n=1)/1000
trialStart = tapply(d$trial_start,d$trial_start,head,n=1)/1000

plot(trialLengths~trialStart)

plot(d$sign_length~d$sign_start)

#boxplot(d$sign_length~d$iconic, ylab='Sign duration', xlab='Iconic')

sel = d$trial_value=='19' & !d$T0Check & nchar(d$sign_value)>0
signs  = unique(d[sel,]$sign_value)
set.seed(1211)
cols = sample(rainbow(length(signs)))
names(cols) = signs

plot(d[sel,]$sign_start, col=cols[d[sel,]$sign_value], pch=16)

tapply(d$sign_value, d$week, function(X){length(unique(X))})

cols = rainbow(2)
barplot(matrix(tapply(d$sign_value, paste(d$speakerName,d$week), function(X){length(unique(X))}), nrow=2), beside = T, names.arg = sort(unique(d$speakerName)), col=cols, main='Number of variants used')
legend(9,80,c("Week 1","Week 4"), col=cols, pch=15)

d$speakerName = sapply(1:nrow(d),function(X){
  d[X,c("part1","part2")][,d$speaker[X]]
  })

inventedBy = tapply( d$speakerName, d$sign_value, head,n=1)  
d$inventedBy = inventedBy[d$sign_value]  

plot(as.vector(table(inventedBy)), as.vector(table(d$inventedBy)), xlab='Number of variants innovated', ylab="Frequency of variants", col='white')
text(as.vector(table(inventedBy)), as.vector(table(d$inventedBy)), names(table(inventedBy)))


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

summary(lm(log(1 + freq_week_4) ~ log(freq_week_1+1) + iconic + check + inventedBy, data=variants))


library(party)

variants$iconic = as.factor(variants$iconic)
variants$inventedBy = as.factor(variants$inventedBy)
f = ctree(freq_week_4 ~ freq_week_1 + iconic + check + inventedBy, data=variants, control=ctree_control(mincriterion = 0.5))
plot(f)


d3 = d
d3$iconic[is.na(d3$iconic)] = "No"
d3$freq_week_4[is.na(d3$freq_week_4)] = 0

for(i in c('iconic','speakerName','inventedBy')){
  d3[,i] = as.factor(d3[,i])
}

summary(lm(log(1 + freq_week_4) ~ iconic + T0Check + inventedBy, data=d3))



f2 = ctree(freq_week_4 ~ iconic + T0Check + speakerName + sign_length+ inventedBy, data=d3, control=ctree_control(mincriterion = 0.95))
plot(f2, inner_panel=node_inner(f2,id=F),terminal_panel=node_barplot)

write.csv(d, file="../data/processedData/variants_processed.csv")
