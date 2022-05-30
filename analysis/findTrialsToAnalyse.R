setwd("~/Documents/MPI/KangSukColours/ColourExperiment/analysis/")

# check: being used in week 3?
#TODO: teaching/checking by speaker
#TODO: List of most sucessful variants


#The most successful variants


bestVariants = by(variants[,c("sign",'freq_week_4','prop_week_4')],
                  variants$colourName,
                  function(X){
                    X[X[,2]==max(X[,2]),]
                  })
bestVariants =t(sapply(bestVariants,rbind))
bestVariants = as.data.frame(cbind(rownames(bestVariants),matrix(unlist(bestVariants),ncol=3)))
write.csv(bestVariants,file="../results/descriptive/WinningVariants.csv")


# Variants that failed hardest

variants$diff = variants$freq_week_4 - variants$freq_week_1

variants[which(variants$diff==min(variants$diff)),]

variants[which(variants$diff==-11),]

d = read.csv("../data/processedData/variants_processed.csv", stringsAsFactors = F)

d$week1 = grepl("game_01",d$filename)
d = d[d$week1,]

d$trialID2 = paste(d$filename,d$trialID)

yellow_chest = tapply(d$sign_value,d$trialID2, function(X){
  sum(X=="YELLOW-CHEST")
})

yellow_chest[which(yellow_chest==max(yellow_chest))]

d[d$sign_value=="YELLOW-CHEST",]

variants[variants$sign=="YELLOW-CHEST",]

# Example A: demonstrate that frequency is not a good predictor, because bad variants are often repeated.  We need an example of a sign that was very frequent in week 1, but not frequent in week 3, and used several times within the same sequence.
# YELLOW-CHEST is the variant that has the biggest drop in usage over the two weeks.  It's used 12 times in week 1, but only once in week 3.
# In the first week is used by the Nepalese signer 4 times in a row in the file Colour_game_01_1_Nepal-Indonesia_After_one_week.eaf in trial 3.  (trial starts about 1 minute 10 seconds).
# The only other signer to use the sign is the Jordanian speaker, in file Colour_game_01_3_Jordan-Nepal_After_one_week.eaf, trials 4 adn 27.


repetitions = tapply(d$sign_value, d$trialID2, function(X){
  max(table(X))
})

repetitions[which(repetitions==max(repetitions))]

# The trial with the largest number of repetitions of a single sign is Colour_game_01_1_Jordan-India_After_one_week.eaf trial 26, starts about 2 minutes 45 seconds.

diversity = tapply(d$sign_value, d$trialID2, function(X){
  length(unique(X))
})

diversity[which(diversity==max(diversity))]

# First time produce black

for(i in unique(d$speakerName)){
  dx = d[d$speakerName==i & d$sign_value=="BLACK",]
  tx = format( as.POSIXct(Sys.Date())+dx[1,]$sign_start/1000, "%M:%S")
  print(c(i,dx[1,]$trialID2,tx,dx[1,]$sign_value))
}

# The first time that participants produce a variant for 'black':

# "Jordan"                                            
# "Colour_game_01_1_Jordan-India_After_one_week.eaf 1"
# "00:10"       

# "Indonesia"                                             
# "Colour_game_01_1_Nepal-Indonesia_After_one_week.eaf 21"
# "00:11" 

# "India"                                             
# "Colour_game_01_1_Jordan-India_After_one_week.eaf 1"
# "00:12"     

# "Nepal"                                                 
# "Colour_game_01_1_Nepal-Indonesia_After_one_week.eaf 12"
# "00:17" 

table(variants$TryMarked.cat, variants$Teach)

# 9 cases of TryMarked=="Low" & Teach
tm_teach = variants[variants$TryMarked.cat=="Low" & variants$Teach,]
tm_teach_signs = c("GREEN-NSL","EAT+PEEL","YELLOW(ISL)","LIPSTICK","LIP")

d$sign_start_mmss = format( as.POSIXct(Sys.Date())+d$sign_start/1000, "%M:%S")

dx = d[d$sign_value %in% tm_teach_signs,c("filename",'trialID','sign_start_mmss','sign_value')]

write.csv(dx,"../results/descriptive/Signs_TryMarkedLow_TeachTrue.csv")

tmHigh_teachtrue = variants[variants$TryMarked.cat=="High" & variants$Teach,]
tmHigh_teachfalse_signs = c("BROWN" ,"YELLOW-CHEST", "LEMON", "BRIGHT" ,"SUN","RED", "TONGUE" )
dx = d[d$sign_value %in% tmHigh_teachfalse_signs,c("filename",'trialID','sign_start_mmss','sign_value')]
write.csv(dx,"../results/descriptive/Signs_TryMarkedHigh_TeachTrue.csv")





