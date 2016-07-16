variants = variants[order(variants$freq_week_4_withinColour),]
x = variants[variants$colourName=='yellow',]$freq_week_4_withinColour

variants[variants$colourName=='pink' & variants$freq_week_4_withinColour>0,c("sign",'freq_week_4_withinColour')]


barplot(t(t(x)),beside=F)
text(1,x,variants[variants$colourName=='green' & variants$freq_week_4_withinColour>0,]$sign)


x = variants[variants$colourName=="pink",]

plot(c(0.9,2.5),0:1,type='n', xlab='',ylab='Proportion of variants used', xaxt='n')
arrows(1,x$freq_week_1_withinColour,2,x$freq_week_4_withinColour)
points(rep(1,nrow(x)),x$freq_week_1_withinColour, pch=16)

barplot(cbind(x$freq_week_1_withinColour,x$freq_week_4_withinColour))


plotmeans(variants$freq_week_4_withinColour~variants$indexical,legends = c("Non-Indexical","Indexical","Indexical (Body)") , xlab='',ylab='Frequency of variant in week 3')

plotmeans(variants$freq_week_4_withinColour~cut(variants$freq_week_1, c(-Inf,2,5,Inf)),legends=c("Low","Medium","High"), xlab='Frequency',ylab='Frequency of variant in week 3')

plot(variants$freq_week_4_withinColour~variants$averageLength_week_1.logcenter)
plotmeans(variants$freq_week_4_withinColour~cut(variants$averageLength_week_1,c(0,1000,Inf)), 
          legends = c("< 1s", ">1s"),
          xlab='',ylab='Frequency of variant in week 3')
    
plotmeans(variants$freq_week_4_withinColour~variants$TryMarked.cat,
          xlab='',ylab='Frequency of variant in week 3')
