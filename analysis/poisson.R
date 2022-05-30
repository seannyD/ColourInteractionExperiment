# Predict raw frequencies using a poisson distribution


m0 = glmer(freq_week_4 ~ 
            1
          + (1 | colourName) + (0 + indexical | colourName) , 
          data=variants, family=poisson)
m1 = glmer(freq_week_4 ~ 
            (indexical) + 
            + (1 | colourName) + (0 + indexical | colourName) , 
          data=variants, family=poisson)
m2 = glmer(freq_week_4 ~ 
            (indexical) + 
            (Teach) 
          + (1 | colourName) + (0 + indexical | colourName) , 
          data=variants, family=poisson)

m3 = glmer(freq_week_4 ~ 
            (indexical) + 
            (Teach) + (TryMarked) 
          + (1 | colourName) + (0 + indexical | colourName) , 
          data=variants, family=poisson)

m4 = glmer(freq_week_4 ~ 
            (indexical) + 
            Teach * TryMarked
          + (1 | colourName) + (0 + indexical | colourName) , 
          data=variants, family=poisson)

m5 = glmer(freq_week_4 ~ 
            (indexical) + 
            (Teach * TryMarked) +
            freq_week_1_total.logcenter
          + (1 | colourName) + (0 + indexical | colourName) , 
          data=variants, family=poisson)

m6 = glmer(freq_week_4 ~ 
            (indexical) + 
            (Teach * TryMarked) +
            freq_week_1_total.logcenter +
            averageLength_week_1.logcenter
          + (1 | colourName) + (0 + indexical | colourName) , 
          data=variants, family=poisson)

m7 = glmer(freq_week_4 ~ 
            (indexical) + 
            (Teach * TryMarked) +
            freq_week_1_total.logcenter +
            averageLength_week_1.logcenter+
            check.any 
          + (1 | colourName) + (0 + indexical | colourName) , 
          data=variants, family=poisson)

m8 = glmer(freq_week_4 ~ 
            (indexical) + 
            (Teach * TryMarked) +
            freq_week_1_total.logcenter +
            averageLength_week_1.logcenter+
            check.any + inventedBy
          + (1 | colourName) + (0 + indexical | colourName) , 
          data=variants, family=poisson)

anova(m0,m1,m2,m3,m4,m5, m6,m7,m8)

plot(variants$freq_week_4, exp(predict(m8)))

