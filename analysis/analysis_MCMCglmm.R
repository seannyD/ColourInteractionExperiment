

library(MCMCglmm)

colourNameRandomEffectsN = 3

prior.m3 <- list(
  R=list(V=1, n=1, fix=1),
  G=list(G1=list(V        = diag(colourNameRandomEffectsN),  # family intercept+slope
                 n        = colourNameRandomEffectsN,
                 alpha.mu = rep(0, colourNameRandomEffectsN),
                 alpha.V  = diag(colourNameRandomEffectsN)*25^2)))

prior.m3 <- list(
  R=list(V=1, n=1, fix=1),
  G=list(G1=list(V= diag(5), n = 5)))

prior.m5d.1 = list(R = list(V = diag(2), nu = 0.002, fix = 2),
                   G = list(G1=list(V=diag(5),n=5)))

m3.mcmcglmm <- MCMCglmm(
  freq_week_4 ~ 
    trait - 1 +
    at.level(trait,1):Teach,
  random = ~idh(trait):colourName,
  data   = variants,
  family = "zipoisson",
  prior  = prior.m5d.1,
  thin   =     100,
  burnin =    100,
  nitt   =  1100,
  verbose = TRUE)