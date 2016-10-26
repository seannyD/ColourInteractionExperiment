library(ggplot2)
setwd("~/Documents/MPI/KangSukColours/ColourExperiment/analysis/")


useOnlyDirector = F


d = read.csv("../data/processedData/variants_processed.csv", stringsAsFactors = F)



d = d[d$sign_value!='SAME',]
d = d[d$sign_value!='',]
d = d[d$sign_value!='?',]

d[d$sign_value=="FOLWER",]$sign_value = "FLOWER"
d[d$sign_value=="BIGHT",]$sign_value = "BRIGHT"
d[d$sign_value=="SIGINING",]$sign_value = "SIGNING"

colourNumbers = c("1","5",'6',"7","14",'18','24')
colourNames = c("red",'brown','white','black','green','yellow','pink')
names(colourNames) = colourNumbers
colourNamesDark = c("dark red", 'orange','gray', 'dark gray', 'dark green','gold', 'purple')


d = d[d$trial_value %in% colourNumbers,]

d$trialColourName = colourNames[d$trial_value]
d$trialColourName = factor(d$trialColourName, levels = colourNames)

individuals = unique(c(d$part1,d$part2))
individuals = c("India",'Jordan','Indonesia',"Nepal")

getLetters = function(pairs){
  letterCount = 1
  let = matrix(nrow=6,ncol=4)
  colnames(let) = individuals
  currentLetter = "A"
  for(i in 1:nrow(pairs)){
      let[letterCount,pairs[i,]$part1] = currentLetter
      let[letterCount,pairs[i,]$part2] = currentLetter
      if(currentLetter=="A"){
        currentLetter = "B"
      } else{
        currentLetter = "A"
      }
      if(i %% 2==0){
        letterCount = letterCount +1
      }
  }
  return(let)
}

makeTamatizPlot = function(res,pairs){
  variants = unique((res[!is.na(res)]))
  colours = rainbow(length(variants))
  names(colours) = variants
  col = colours[res]
  col[is.na(col)] = 'white'
  col = matrix(col, ncol=4)
  
  # note that we're plotting upside down!
  plot(c(5,1),c(7,1),type='n',xaxt='n',yaxt='n',xlab='',ylab='', bty='n',ylim=c(7,0.9),xlim=c(0.5,5.2))
  for(j in 1:6){
    for(i in 1:4){
    
      rect(i,j,i+0.9,j+0.9,col = col[j,i])
      
      text(i+0.5,j+0.8,res[j,i],cex=0.5)
    }
  }
  text((1:4)+0.5,rep(0.8,4),individuals)
  text(rep(0.8,6),(1:6)+0.5,paste("S",rep(1:3,2),sep=''))
  #text(rep(0.8,6),(1:6)+0.5,paste("S",rep(1:3,2),sep=''))
  text(rep(0.6,2),c(2.5,5.5),c("Week1","Week4") ,srt=90)
  abline(h=3.95)
  
  letters = getLetters(pairs)
  for(i in 1:6){
    for(j in 1:4){
      text(j+0.5,i+0.5,letters[7-i,j])
    }
  }
}





# d is already in correct time order
for(colourID in colourNumbers){
  
  res = matrix(nrow=6,ncol=4)
  colnames(res) = individuals
  rowTracker = 1
  
  for(week in unique(d$week)){
    pairs = unique(paste(d[d$week==week,]$part1,d[d$week==week,]$part2, d[d$week==week,]$session))
    dx = d[d$week==week & d$trial_value==colourID,]
    dx$pair = paste(dx$part1,dx$part2,dx$session)
    for(session in 1:3){
      dxx = dx[dx$session==session,]
      if(useOnlyDirector){
        dxx = dxx[dxx$director==dxx$speaker,] 
        } 
      firstSigns = tapply(dxx$sign_value,dxx$speakerName,head,n=1)
      res[rowTracker,] = firstSigns[individuals]
      rowTracker = rowTracker + 1
    }
    
  }
  pairs = d[,c("part1","part2","week","session")]
  pairs = pairs[!duplicated(apply(pairs,1,paste,collapse='')),]
  
  filename = paste("../results/descriptive/selectionPlots/SelectionPlot_", colourNames[colourID],".pdf",sep='')
  
  pdf(filename, width=6,height=6)
  makeTamatizPlot(res,pairs)
  dev.off()
  
  
}


##############

makePropSquare = function(prop,x,y,vcolours){
  vcolours = vcolours[prop!=0]
  prop = prop[prop!=0]
  if(length(prop)>0){
    prop = prop[order(vcolours)]
    vcolours = sort(vcolours)
    #vcolours = vcolours[order(prop)]
    
    
    prop2 = prop/sum(prop)
    prop2 = prop2*0.9
    x2 = cumsum(prop2)
    x1 = c(0,x2[1:(length(x2)-1)])
    x2 = x2 + x
    x1 = x1 + x
    y1 = y + 0
    y2 = y + 0.5
   
    rect(x1,y1,x2,y2,col=vcolours)
    
  }
  rect(x-0.05,y-0.5,x+0.95,y+0.5)
}

nv = unique(d[d$director==d$speaker,]$sign_value)
vcols = rainbow(length(nv))
set.seed(127)
vcols = sample(vcols)
names(vcols)= nv



for(colourID in colourNumbers){
  
  filename = paste("../results/descriptive/selectionPlots/SelectionPlot_Proportions_", colourNames[colourID],".pdf",sep='')
  
  pdf(filename, width=6,height=6)
  
  plot(c(0,7),c(0,5), ylim=c(6.5,0),xlim=c(0,5), type='n',xlab='',ylab='', xaxt='n',yaxt='n',bty ='n')
  title(colourNames[colourID])
  
  pairs = d[,c("part1","part2","week","session")]
  pairs = pairs[!duplicated(apply(pairs,1,paste,collapse='')),]
  letters = getLetters(pairs)
  for(i in 1:6){
    for(j in 1:4){
      text(j+0.5,i-0.25,letters[7-i,j])
    }
  }
  
  text((1:4)+0.5,rep(0,4),individuals)
  text(rep(0.6,6),(1:6),paste("S",rep(1:3,2),sep=''))
  
  text(rep(0.2,2),c(2,5),c("Week1","Week4") ,srt=90)
  abline(h=3.5)
  
  rowTracker = 1
  for(week in unique(d$week)){
    pairs = unique(paste(d[d$week==week,]$part1,d[d$week==week,]$part2, d[d$week==week,]$session))
    dx = d[d$week==week & d$trial_value==colourID,]
    dx$pair = paste(dx$part1,dx$part2,dx$session)
    dx$speakerName = factor(dx$speakerName,levels=individuals)
    for(session in 1:3){
      dxx = dx[dx$session==session,] 
      if(useOnlyDirector){
        dxx = dxx[dxx$director==dxx$speaker,]
      }
      tx = as.matrix(table(dxx$sign_value,dxx$speakerName))
      tx = tx[,individuals]
      for(i in 1:4){
        if(is.null(dim(tx))){
          partVars = tx[i]
        } else{
          partVars = tx[,i]
        }
        
        vcolsx = vcols[rownames(tx)]
        if(is.null(rownames(tx))){
          vcolsx = vcols[dxx$sign_value[1]]
        }
        
        makePropSquare(partVars,i,rowTracker, vcolsx)
      }
      rowTracker = rowTracker + 1
    }
    
  }
  dev.off()
}
