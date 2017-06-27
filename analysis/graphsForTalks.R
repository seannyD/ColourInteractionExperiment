
setwd("~/Documents/MPI/KangSukColours/ColourExperiment/analysis/")

variants = read.csv('../data/processedData/variants_summary.csv', stringsAsFactors = F)

variants$Teach = variants$Teach >0

variants$check.any = variants$check>0

variants$freq_week_1.logcenter = log(variants$freq_week_1 + 1)
variants$freq_week_1.logcenter = 
  variants$freq_week_1.logcenter - mean(variants$freq_week_1.logcenter)

# cut TryMarking into two categories
variants$TryMarked.cat = cut(variants$TryMarked, 
                             c(-Inf,3,Inf), 
                             labels = c("Low",'High')) 

variants$averageLength_week_1.logcenter = log(variants$averageLength_week_1)
variants$averageLength_week_1.logcenter = 
  variants$averageLength_week_1.logcenter -
  mean(variants$averageLength_week_1.logcenter)



variants = variants[order(variants$freq_week_4_withinColour),]
x = variants[variants$colourName=='yellow',]$freq_week_4_withinColour

variants[variants$colourName=='pink' & variants$freq_week_4_withinColour>0,c("sign",'freq_week_4_withinColour')]


barplot(t(t(x)),beside=F)
text(1,x,variants[variants$colourName=='green' & variants$freq_week_4_withinColour>0,]$sign)


x = variants[variants$colourName=="pink",]

x$freq_week_1_withinColour = x$freq_week_1 / sum(x$freq_week_1)
x$freq_week_4_withinColour = x$freq_week_4 / sum(x$freq_week_4)

pdf("../results/descriptive/graphs/PinkArrows.pdf", width=4,height = 4)
plot(c(0.9,2.5),0:1,type='n', xlab='',ylab='Proportion of variants used', xaxt='n')
arrows(1,x$freq_week_1_withinColour,2,x$freq_week_4_withinColour, length=0.2)
points(rep(1,nrow(x)),x$freq_week_1_withinColour, pch=16)
axis(1,at=c(1,2),labels = c("Week 1", "Week 2"))
dev.off()


barplot(cbind(x$freq_week_1_withinColour,x$freq_week_4_withinColour))


plotmeans(variants$freq_week_4_withinColour~variants$indexical,legends = c("Non-Indexical","Indexical","Indexical (Body)") , xlab='',ylab='Frequency of variant in week 3')

plotmeans(variants$freq_week_4_withinColour~cut(variants$freq_week_1, c(-Inf,2,5,Inf)),legends=c("Low","Medium","High"), xlab='Frequency',ylab='Frequency of variant in week 3')

plot(variants$freq_week_4_withinColour~variants$averageLength_week_1.logcenter)
plotmeans(variants$freq_week_4_withinColour~cut(variants$averageLength_week_1,c(0,1000,Inf)), 
          legends = c("< 1s", ">1s"),
          xlab='',ylab='Frequency of variant in week 3')
    
plotmeans(variants$freq_week_4_withinColour~variants$TryMarked.cat,
          xlab='',ylab='Frequency of variant in week 3')


