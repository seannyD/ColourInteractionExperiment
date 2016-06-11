library(ggplot2)
setwd("~/Documents/MPI/KangSukColours/ColourExperiment/analysis/")

d = read.csv("../data/processedData/variants_processed.csv", stringsAsFactors = F)

colourNumbers = c("1","5","7","14",'18','24')
colourNames = c("red",'brown','black','green','yellow','pink')
names(colourNames) = colourNumbers
colourNamesDark = c("dark red", 'orange', 'gray', 'dark green','gold', 'purple')


d = d[d$trial_value %in% colourNumbers,]

d$trialColourName = colourNames[d$trial_value]
d$trialColourName = factor(d$trialColourName, levels = colourNames)

sun = table(d[d$sign_value=="SUN",]$week)
yellow = table(d[d$sign_value=="GREEN1-1",]$week)

barplot(rbind(sun,yellow), beside=T, xlab='week', ylab='frequency', ylim=c(0,20),col=1:2)
legend(1,20,legend=c("SUN","GREEN1-1"), col=1:2, pch=15)

pdf("../results/descriptive/graphs/IconicityByWeek.pdf")
barplot(table(d$iconic,d$week), col=1:2, xlab='Week')
legend(1.5,150,legend=c("Iconic","Not iconic"), col=2:1, pch=15)
dev.off()


library(gplots)
pdf("../results/descriptive/graphs/LengthByWeek_YELLOW.pdf")
plotmeans(d[d$sign_value=="YELLOW",]$sign_length~d[d$sign_value=="YELLOW",]$week, xlab='Week', ylab="Length (ms)", main='Length of the sign YELLOW')
dev.off()

pdf("../results/descriptive/graphs/LengthByWeek.pdf")
plotmeans(d$sign_length~d$week, xlab='Week', ylab="Length (ms)", main='Length of signs')
dev.off()

pdf("../results/descriptive/graphs/IconicByWeekByPart.pdf", width=8, height=5)
partIconic = table(d$iconic,d$speakerName,d$week)
cbind(partIconic[,,1],partIconic[,,2])
barplot(cbind(partIconic[,,1],partIconic[,,2]), col=1:2)
abline(v=4.9)
text(c(2.5,7.5),c(65,65), c("Week 1", "Week 4"))
legend(5,50, c("Iconic","Non-iconic"), col=2:1, pch=15)
dev.off()

pdf("../results/descriptive/graphs/NumVarByFreq.pdf")
plot(as.vector(table(inventedBy)), as.vector(table(d$inventedBy)), xlab='Number of variants innovated', ylab="Frequency of variants", col='white')
text(as.vector(table(inventedBy)), as.vector(table(d$inventedBy)), names(table(inventedBy)))
dev.off()

pdf("../results/descriptive/graphs/NumVariants.pdf", width=8, height=5)
cols = rainbow(2)
barplot(matrix(tapply(d$sign_value, paste(d$speakerName,d$week), function(X){length(unique(X))}), nrow=2), beside = T, names.arg = sort(unique(d$speakerName)), col=cols, main='Number of variants used')
legend(9,80,c("Week 1","Week 4"), col=cols, pch=15)
dev.off()

week1L = tapply(d[d$director==d$speaker & d$week==1,]$sign_length, d[d$director==d$speaker & d$week==1,]$trialColourName, mean)
week4L = tapply(d[d$director==d$speaker & d$week==4,]$sign_length, d[d$director==d$speaker & d$week==4,]$trialColourName, mean)


pdf("../results/descriptive/graphs/LengthOfSignsByColour.pdf")
plot(c(0.5,2.5),range(week1L), type='n', ylab='Sign length (ms)', xlab='Week', xaxt="n")
axis(1,at=1:2, c("Week 1", "Week 4"))
points(rep(1,length(week1L)), week1L, col=colourNames, pch=16)
points(rep(2,length(week4L)), week4L, col=colourNames, pch=16)
for(i in 1:length(week1L)){
  lines(c(1,2),c(week1L[i],week4L[i]), col=colourNames[i])
}
dev.off()

for(i in 1:length(colourNames)){
 plotmeans(sign_length~week, data = d[d$director==d$speaker & d$trialColourName==colourNames[i], ], add=i!=1, col=colourNames[i])
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
axis(1,at=1:2, c("Week 1", "Week 4"))
points(rep(1,length(week1L.trial)), week1L.trial, col=colourNames, pch=16)
points(rep(2,length(week4L.trial)), week4L.trial, col=colourNames, pch=16)
for(i in 1:length(week1L)){
  lines(c(1,2),c(week1L.trial[i],week4L.trial[i]), col=colourNames[i])
}
dev.off()

plotmeans(sign_length~trialColourName,  d[d$director==d$speaker & d$week==1,], connect = F, col = colourNames, pch=16, barcol = 1,xlab='Colour', ylab='Sign length (ms)')
plotmeans(sign_length~trialColourName,  d[d$director==d$speaker & d$week==4,], connect = F, col = colourNames, pch=16, barcol = 1,xlab='Colour', ylab='Sign length (ms)', add=T)


###########



variants = data.frame()
for(colourID in colourNumbers){
  d2 =d[d$trial_value==colourID,]
  
  v = data.frame(colour=colourID, colourName = colourNames[which(colourNumbers==colourID)],sign=unique(d2[d2$week==1,]$sign_value[nchar(d2[d2$week==1,]$sign_value)>0]), stringsAsFactors = F)
  
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


week1 = tapply(d[d$trial_value %in% colourNumbers & d$week==1,]$sign_value, d[d$trial_value %in% colourNumbers & d$week==1,]$trial_value, function(X){length(unique(X,na.rm=T))})
week4 = tapply(d[d$trial_value %in% colourNumbers & d$week==4,]$sign_value, d[d$trial_value %in% colourNumbers & d$week==4,]$trial_value, function(X){length(unique(X,na.rm=T))})

xtab = cbind(week1,week4)

pdf("../results/descriptive/graphs/NumberOfVariantsByColourByWeek.pdf", width=8, height=5)
barplot(xtab,beside=T, col=colourNames[rownames(xtab)], main='Number of Variants')
dev.off()

#######

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



