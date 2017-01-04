m0 = glmer(freq_week_4 ~ 
             1
           + (1 | colourName) , 
           data=variants, family=poisson)

m1 = glmer(freq_week_4 ~ 
              1 + freq_week_1_total + 
              + (1 | colourName) , 
            data=variants, family=poisson)



m2 = glmer(freq_week_4 ~ 
             1 + freq_week_1_total + 
             (indexical) + 
             + (1 | colourName) , 
           data=variants, family=poisson)
m3 = glmer(freq_week_4 ~ 
             1 + freq_week_1_total + 
             (indexical) + 
             (Teach) 
           + (1 | colourName) , 
           data=variants, family=poisson)

m4 = glmer(freq_week_4 ~ 
             1 + freq_week_1_total + 
             (indexical) + 
             (Teach) + (TryMarked) 
           + (1 | colourName) , 
           data=variants, family=poisson)

m5 = glmer(freq_week_4 ~ 
             1 + freq_week_1_total + 
             (indexical) + 
             Teach * TryMarked
           + (1 | colourName) , 
           data=variants, family=poisson)

m6 = glmer(freq_week_4 ~ 
             1 + freq_week_1_total + 
             (indexical) + 
             (Teach * TryMarked) +
             averageLength_week_1.logcenter +
           + (1 | colourName) , 
           data=variants, family=poisson)


m7 = glmer(freq_week_4 ~ 
             1 + freq_week_1_total + 
             (indexical) + 
             (Teach * TryMarked) +
             averageLength_week_1.logcenter+
             check.any 
           + (1 | colourName) , 
           data=variants, family=poisson)

m8 = glmer(freq_week_4 ~ 
             1 + freq_week_1_total + 
             (indexical) + 
             (Teach * TryMarked) +
             averageLength_week_1.logcenter+
             check.any + inventedBy
           + (1 | colourName) , 
           data=variants, family=poisson)


anova(m0,m1,m2,m3,m4,m5, m6,m7,m8)