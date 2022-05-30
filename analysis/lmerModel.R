library(ggplot2)
library(lme4)
setwd("~/Documents/MPI/KangSukColours/ColourExperiment/analysis/")

getMEText = function(r,ef, wald=NULL){
  
  AIC = r[2,]$AIC
  loglikDiff = signif(diff(r$logLik),2)
  chi = round(r$Chisq[2],2)
  df = r$`Chi Df`[2]
  p = signif(r$`Pr(>Chisq)`[2],2)
  
  wald.text = ""
  
  if(!is.null(wald)){
    est = signif(wald[1],2)
    stder = signif(wald[2],2)
    t = signif(wald[3],2)
    wptext = ""
    if(!is.na(wald[4])){
      wptext = paste(", Wald p =",signif(wald[4],2))
    }
    wald.text = paste("beta = ",est,", std.err = ",stder, ", Wald t = ",t,wptext,';')
  }
  
  begin = 'There was no significant'
  if(p <0.1){
    begin = "There was a marginal"
  }
  if(p < 0.05){
    begin = 'There was a significant'  
  }
  
  
  return(paste(begin,ef,"(",wald.text,"log likelihood difference =",
               loglikDiff,", df = ",df,", Chi Squared =", chi,", p = ",p,")."))
}



# LOAD DATA

variants = read.csv('../data/processedData/variants_summary.csv', stringsAsFactors = F)

d$BodyAnchor = variants[match(d$sign_value, variants$sign),]$BodyAnchor

m0 = lmer(log(1 + freq_week_4_withinColour) ~ 
            1
          + (1 | colourName), 
          data=variants)
m1 = lmer(log(1 + freq_week_4_withinColour) ~ 
            (indexical) + 
            + (1 | colourName), 
          data=variants)
m2 = lmer(log(1 + freq_week_4_withinColour) ~ 
            (indexical) + 
            (Teach) 
          + (1 | colourName), 
          data=variants)

m3 = lmer(log(1 + freq_week_4_withinColour) ~ 
            (indexical) + 
            (Teach) + (TryMarked) 
          + (1 | colourName), 
          data=variants)

m4 = lmer(log(1 + freq_week_4_withinColour) ~ 
            (indexical) + 
            (Teach) * (TryMarked) 
          + (1 | colourName), 
          data=variants)

m5 = lmer(log(1 + freq_week_4_withinColour) ~ 
            (indexical) + 
            (Teach) * (TryMarked) +
            log(freq_week_1+1)
          + (1 | colourName), 
          data=variants)

anova(m0,m1,m2,m3,m4,m5)



getMEText(anova(m2,m3), "main effect of try marking",summary(m5)$coef['TryMarked',])
