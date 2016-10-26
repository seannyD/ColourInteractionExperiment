library(ggplot2)
library(lme4)
setwd("~/Documents/MPI/KangSukColours/ColourExperiment/analysis/")

d = read.csv("../data/processedData/variants_processed.csv", stringsAsFactors = F)


d$trial_value = as.character(d$trial_value)  # legacy compatibility
colourNumbers = c("1","5",'6',"7","14",'18','24')
colourNames = c("red",'brown','white','black','green','yellow','pink')
names(colourNames) = colourNumbers
colourNamesDark = c("dark red", 'orange','gray', 'black', 'dark green','gold', 'pink')


d = d[d$trial_value %in% colourNumbers,]

d$trialColourName = colourNames[as.character(d$trial_value)]
d$trialColourName = factor(d$trialColourName, levels = colourNames)




###########
sun = table(d[d$sign_value=="SUN",]$week)
yellow = table(d[d$sign_value=="GREEN1-1",]$week)

barplot(rbind(sun,yellow), beside=T, xlab='week', ylab='frequency', ylim=c(0,20),col=1:2)
legend(1,20,legend=c("SUN","GREEN1-1"), col=1:2, pch=15)

# pdf("../results/descriptive/graphs/IconicityByWeek.pdf")
# barplot(table(d$iconic,d$week), col=1:2, xlab='Week')
# legend(1.5,150,legend=c("Iconic","Not iconic"), col=2:1, pch=15)
# dev.off()

# w1 = table(d[d$week==1,]$iconic, d[d$week==1,]$trialColourName)
# w4 = table(d[d$week==4,]$iconic, d[d$week==4,]$trialColourName)
# 
# 
# pdf("../results/descriptive/graphs/IconicityByWeekByColour.pdf")
# barplot(cbind(w1,c(NA,NA),w4), col=1:2, las=2)
# abline(v=7.9)
# axis(1,line=2.5,at=c(4,12), labels=c("Week 1","Week 3"), tick=F, lwd=0)
# legend(1.5,150,legend=c("Iconic","Not iconic"), col=2:1, pch=15)
# dev.off()


library(gplots)
pdf("../results/descriptive/graphs/LengthByWeek_YELLOW.pdf")
plotmeans(d[d$sign_value=="YELLOW",]$sign_length~d[d$sign_value=="YELLOW",]$week, xlab='Week', ylab="Length (ms)", main='Length of the sign YELLOW')
dev.off()

pdf("../results/descriptive/graphs/LengthByWeek.pdf")
plotmeans(d$sign_length~d$week, xlab='Week', ylab="Length (ms)", main='Length of signs')
dev.off()

# pdf("../results/descriptive/graphs/IconicByWeekByPart.pdf", width=8, height=5)
# partIconic = table(d$iconic,d$speakerName,d$week)
# cbind(partIconic[,,1],partIconic[,,2])
# barplot(cbind(partIconic[,,1],partIconic[,,2]), col=1:2)
# abline(v=4.9)
# text(c(2.5,7.5),c(65,65), c("Week 1", "week 3"))
# legend(5,50, c("Iconic","Non-iconic"), col=2:1, pch=15)
# dev.off()


inventedBy = tapply(d$sign_value,d$inventedBy, function(X){length(unique(X))})
pdf("../results/descriptive/graphs/NumVarByFreq.pdf")
plot(
    as.vector(inventedBy), 
    as.vector(table(d$inventedBy)), 
    xlab='Number of variants innovated', ylab="Frequency of variants", col=1, ylim=c(75,130))
text(as.vector(inventedBy), as.vector(table(d$inventedBy)), names((inventedBy)),pos=3)
dev.off()

freqWithinVarByInventor = tapply(variants$freq_week_4_withinColour, variants$inventedBy, mean)

pdf("../results/descriptive/graphs/NumVarByFreq_withinColour.pdf")
plot(
  as.vector(inventedBy), 
  as.vector(freqWithinVarByInventor), 
  xlab='Number of variants innovated', ylab="Mean fitness of variants", col=1, ylim=c(0,0.15))
text(as.vector(inventedBy), as.vector(freqWithinVarByInventor), names((inventedBy)),pos=3)
dev.off()

# number of variants adopted
adopted = sapply(sort(unique(d$speakerName)), function(X){
  x = d[d$speakerName==X,]$inventedBy
  x2 = tapply(d[d$speakerName==X,]$sign_value, d[d$speakerName==X,]$inventedBy, function(z){length(unique(z))})
  sum(x2[names(x2)!=X])
})
pdf("../results/descriptive/graphs/AdoptedVsInvented.pdf")
plot(inventedBy,adopted, ylim=c(0,35), xlim=c(0,35), xlab="Number of variants invented", ylab="Number of variants adopted")
abline(0,1, col='gray')
text(inventedBy,adopted,names(inventedBy), pos=3)
dev.off()

pdf("../results/descriptive/graphs/NumVariants.pdf", width=8, height=5)
cols = rainbow(2)
barplot(matrix(tapply(d$sign_value, paste(d$speakerName,d$week), function(X){length(unique(X))}), nrow=2), beside = T, names.arg = sort(unique(d$speakerName)), col=cols, main='Number of variants used')
legend(9,80,c("Week 1","week 3"), col=cols, pch=15)
dev.off()

week1L = tapply(d[d$director==d$speaker & d$week==1,]$sign_length, d[d$director==d$speaker & d$week==1,]$trialColourName, mean,na.rm=T)
week4L = tapply(d[d$director==d$speaker & d$week==4,]$sign_length, d[d$director==d$speaker & d$week==4,]$trialColourName, mean,na.rm=T)


pdf("../results/descriptive/graphs/LengthOfSignsByColour.pdf")
plot(c(0.5,2.5),range(c(week1L,week4L),na.rm=T), type='n', ylab='Sign length (ms)', xlab='Week', xaxt="n")
axis(1,at=1:2, c("Week 1", "week 3"))
points(rep(1,length(week1L)), week1L, col=colourNamesDark, pch=16)
points(rep(2,length(week4L)), week4L, col=colourNamesDark, pch=16)
for(i in 1:length(week1L)){
  lines(c(1,2),c(week1L[i],week4L[i]), col=colourNamesDark[i])
}
dev.off()

for(i in 1:length(colourNames)){
 plotmeans(sign_length~week, data = d[d$director==d$speaker & d$trialColourName==colourNames[i], ], add=i!=1, col=colourNamesDark[i])
}

week1L.trial = tapply(d[d$director==d$speaker & d$week==1,]$trial_length, d[d$director==d$speaker & d$week==1,]$trialColourName, mean)
week4L.trial = tapply(d[d$director==d$speaker & d$week==4,]$trial_length, d[d$director==d$speaker & d$week==4,]$trialColourName, mean)

barplot(rbind(week1L.trial, week4L.trial),beside = T, col=rep(colourNames,each=2), ylab='Trial length')



myData <- aggregate(d[d$director==d$speaker,]$trial_length,
  by = list(colour = d[d$director==d$speaker,]$trialColourName, d[d$director==d$speaker,]$week),
  FUN = function(x) c(mean = mean(x), sd = sd(x),
  n = length(x)))

myData <- do.call(data.frame, myData)
myData$se <- myData$x.sd / sqrt(myData$x.n)
colnames(myData) = c('Colour','Week','mean','sd','n','se')

dodge <- position_dodge(width = 0.9)
limits <- aes(ymax = myData$mean + myData$se, ymin = myData$mean - myData$se)


p <- ggplot(data = myData, aes(x = Colour, y = mean, group = Week))

pdf("../results/descriptive/graphs/LengthOfTrialsByColourAndWeek_gg.pdf")
p + geom_bar(stat = "identity", position = 'dodge', fill=rep(colourNames,each=2)) +
  geom_errorbar(limits, position = dodge, width = 0.25) +
  theme(axis.text.x=element_text(), axis.ticks.x=element_blank(),
        axis.title.x=element_blank(), plot.title=element_text("Trial Length")) + ylab("Trial length (ms)") +
    scale_colour_manual(values=colourNames)
dev.off()

######
myData <- aggregate(d[d$director==d$speaker,]$sign_length,
                    by = list(colour = d[d$director==d$speaker,]$trialColourName, d[d$director==d$speaker,]$week),
                    FUN = function(x) c(mean = mean(x), sd = sd(x),
                                        n = length(x)))

myData <- do.call(data.frame, myData)
myData$se <- myData$x.sd / sqrt(myData$x.n)
colnames(myData) = c('Colour','Week','mean','sd','n','se')

dodge <- position_dodge(width = 0.9)
limits <- aes(ymax = myData$mean + myData$se, ymin = myData$mean - myData$se)


p <- ggplot(data = myData, aes(x = Colour, y = mean, group = Week))

pdf("../results/descriptive/graphs/LengthOfSignsByColourAndWeek_gg.pdf")
p + geom_bar(stat = "identity", position = 'dodge', fill=rep(colourNames,each=2)) +
  geom_errorbar(limits, position = dodge, width = 0.25) +
  theme(axis.text.x=element_text(), axis.ticks.x=element_blank(),
        axis.title.x=element_blank(), plot.title=element_text("Sign Length")) + ylab("Sign length (ms)") +
  scale_colour_manual(values=colourNames)
dev.off()

####

pdf("../results/descriptive/graphs/LengthOfTrialsByColour.pdf")
plot(c(0.5,2.5),range(c(week1L.trial,week4L.trial)), type='n', ylab='Trial length (ms)', xlab='Week', xaxt="n")
axis(1,at=1:2, c("Week 1", "week 3"))
points(rep(1,length(week1L.trial)), week1L.trial, col=colourNames, pch=16)
points(rep(2,length(week4L.trial)), week4L.trial, col=colourNames, pch=16)
for(i in 1:length(week1L)){
  lines(c(1,2),c(week1L.trial[i],week4L.trial[i]), col=colourNames[i])
}
dev.off()

plotmeans(sign_length~trialColourName,  d[d$director==d$speaker & d$week==1,], connect = F, col = colourNames, pch=16, barcol = 1,xlab='Colour', ylab='Sign length (ms)')
plotmeans(sign_length~trialColourName,  d[d$director==d$speaker & d$week==4,], connect = F, col = colourNames, pch=16, barcol = 1,xlab='Colour', ylab='Sign length (ms)', add=T)


###########

variants = read.csv('../data/processedData/variants_summary.csv', stringsAsFactors = F)

d$BodyAnchor = variants[match(d$sign_value, variants$sign),]$BodyAnchor

w1 = table(d[d$week==1,]$Indexicality,d[d$week==1,]$trialColourName)
w1 = w1[,c("red",'black','white','brown','green','yellow','pink')]
w3 = table(d[d$week==4,]$Indexicality,d[d$week==4,]$trialColourName)
w3 = w3[,c("red",'black','white','brown','green','yellow','pink')]

xtab = cbind(w1,c(NA,NA,NA), w3)

pdf("../results/descriptive/graphs/BodyAnchoredByColourByWeek.pdf", width=6, height = 5)
barplot(xtab, col=3:5, las=2)
legend(12,30,c('No','Yes',"Yes, Body Anchored"), col=3:5, pch=15)
axis(1,line=2.5,at=c(4,12), labels=c("Week 1","Week 3"), tick=F, lwd=0)
dev.off()





pdf("../results/descriptive/graphs/FreqWeek1_vs_FreqWeek2.pdf", width=5, height=5)
plot(variants$freq_week_1, jitter(variants$freq_week_4), pch=16, col=rgb(0,0,0,0.2), ylab='Frequency in week 3', xlab='Frequency in week 1')
dev.off()

pdf("../results/descriptive/graphs/FreqWeek1_vs_FreqWeek2_withinColour.pdf", width=5, height=5)
plot(variants$freq_week_1_withinColour, jitter(variants$freq_week_4_withinColour), pch=16, col=variants$colourName, ylab='Frequency in week 3', xlab='Frequency in week 1')
dev.off()

week1 = tapply(d[d$trial_value %in% colourNumbers & d$week==1,]$sign_value, d[d$trial_value %in% colourNumbers & d$week==1,]$trial_value, function(X){length(unique(X,na.rm=T))})
week4 = tapply(d[d$trial_value %in% colourNumbers & d$week==4,]$sign_value, d[d$trial_value %in% colourNumbers & d$week==4,]$trial_value, function(X){length(unique(X,na.rm=T))})

xtab = cbind(week1,week4)

pdf("../results/descriptive/graphs/NumberOfVariantsByColourByWeek.pdf", width=8, height=5)
barplot(xtab,beside=T, col=colourNames[rownames(xtab)], main='Number of Variants')
dev.off()


####
# Try marking

variants$check.any = variants$check >0

plotmeans(freq_week_4_withinColour ~
            paste(TryMarked>0,Teach>0),
          data = variants[!(!(variants$TryMarked>0) & (variants$Teach>0)),],
          legends = c("No Try Mark","Try Marking","Try Marking & Teaching"),
          xlab="", ylab='Fitness of variants')

plotmeans(variants$freq_week_4_withinColour~
            paste(variants$colourName,variants$TryMarked>0),
          col = rep(colourNamesDark,each=2), pch=c(16,15))

plotmeans(variants$freq_week_4_withinColour ~ 
            paste(variants$colourName,variants$check.any),
          col = rep(colourNamesDark,each=2), pch=c(16,15))

### sign origin

plotmeans(freq_week_4_withinColour~ origin, data=variants[variants$origin %in% c("ISL","JSL","NSL"),])

#######

library(party)

variants$iconic = as.factor(variants$iconic)
variants$inventedBy = as.factor(variants$inventedBy)
variants$indexical = as.factor(variants$indexical)
variants$TeachT = as.factor(variants$Teach>0)
variants$TryMarkedT = as.factor(variants$TryMarked>0)
f = ctree(freq_week_4_withinColour ~ 
       #     freq_week_1 + 
        #    indexical + 
         #   TeachT +
            TryMarked,
          #  check + 
           # inventedBy + 
       #     factor(colourName), 
       data=variants, control=ctree_control(mincriterion = 0.1))
plot(f, terminal_panel=node_barplot)


d3 = d
d3$freq_week_4[is.na(d3$freq_week_4)] = 0
d3$freq_week4_withinColour[is.na(d3$freq_week4_withinColour)] = 0

d3 = d3[d3$freq_week4_withinColour>0,]

for(i in c('Indexicality','speakerName')){
  d3[,i] = as.factor(d3[,i])
}

f2 = ctree(freq_week4_withinColour ~ 
             Indexicality + 
             T0Check + 
             speakerName + 
             Teach + 
             TryMarked, 
           data=d3, 
           control=ctree_control(mincriterion = 0.75))
plot(f2, inner_panel=node_inner(f2,id=F),terminal_panel=node_barplot)



