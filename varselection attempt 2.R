# Let's try this again.
# Plan of action
# Input data

file <- '/home/beeb/Documents/Data_Science/Data/Stat/'
library(ggplot2)
setwd(file)
synth.reg <- read.table('synthetic_regression.txt', header = TRUE, nrows= 400)
synth.reg.test <- read.table('synthetic_regression.txt', header = TRUE, nrows = 3000)
synth.reg.test <- synth.reg.test[501:3000,]
t <- as.matrix(synth.reg[,1])
d <- as.matrix(synth.reg[,2:ncol(synth.reg)])
x <- d
d <- as.matrix(synth.reg.test[,2:ncol(synth.reg.test)])
x <- d
t <- as.matrix(synth.reg.test[,1])
# This bit of code figures out which of the xs need to be put in groups together
# The first thing we're going to do is see if any of the x's are correlated
cors <- cor(synth.reg)
# Take the variances out
m <- ncol(cors)
for(i in 1:m) {
  cors[i,i] <- 0
}
corchecker <- list()
for(i in 1:m) {
 corchecker[[i]] <- which(cors[,i] > 0.5)
}

together <- list()
for(i in 1:m) {
  if(length(corchecker[[i]]) > 1) {
    together[[i]] <- append(corchecker[[i]], i)
  }
  else {
    together[[i]] <- i
  }
}

# Now we start the stepwise analysis
  currentmod <- rep(1, nrow(x))
  incvars <- list()
  varsleft <- ncol(x)
  # Stepwise!
  #for(j in 1:ncol(x)) {
  j <- 1
  for(k in 1:30) {
    cat(':')
    cat(j)
    likelihoods <- rep(0, varsleft)
    for(i in 1:varsleft) {
      cat(i)
      mod <- cbind(currentmod, x[,together[[i]]])
      model <- lm(t ~ mod)
      likelihoods[i] <- logLik(model)
    }
    bestvar <- which(likelihoods == max(likelihoods))
    incvars[[j]] <- together[[bestvar[1]]]

    currentmod <- cbind(currentmod, x[,bestvar])
    together <- together[setdiff(1:length(together), bestvar)]
    varsleft <- varsleft - length(bestvar)
    if(length(together) < 700)  {
      break
    }
    j <- j + 1
  }
  
  # This bit exists due to the complication of needing to treat certain variables in groups
  # That is, highly correlated variables should go together
  # 'Steps' will tell us; first take the first three vars; then the next two vars; then
  # one by itself ; etc etc.
  steps <- cumsum(sapply(incvars,length))  
  incvars2 <- unlist(incvars)
  #Reasons
synth.reg.test <- synth.reg
  # Let's see how well this has worked
  j <- 1
  collect.r2 <- rep(0, length(cumsum))
for(finalvar in steps[1:10]) {
    varsthisstep <- incvars2[1:finalvar]
      
    for.model <- as.matrix(d)
    for.model <- for.model[,varsthisstep]
    for.model <- as.data.frame(for.model)
    names(for.model) <- paste0("X.", varsthisstep)
    for.model$t <- t
    m <- lm(t ~ ., data=for.model)
    
    # Now we test it
    synth.reg.test2 <- as.matrix(synth.reg.test)
    synth.reg.test2 <- synth.reg.test2[,2:ncol(synth.reg.test2)]
    synth.reg.test2 <- synth.reg.test2[,varsthisstep]
    synth.reg.test2 <- as.data.frame(synth.reg.test2)
    synth.reg.test2 <- as.data.frame(synth.reg.test2)
    synth.reg.test2$t<- synth.reg.test$t

    synth.reg.test2$predvals <- predict(m, newdata = synth.reg.test2)
    # Calculating the R^2 - automatically? Hmm
    tbar <- mean(synth.reg.test2$t)
    ressumsquare <- sum((synth.reg.test2$t - synth.reg.test2$predvals)**2)
    totsumsquare <- sum((synth.reg.test2$t - tbar)**2)
    r.squared <- 1 - (ressumsquare/totsumsquare)
    collect.r2[j] <- r.squared
    for.model$predvals <- predict(m)
  cat(j)
    plottest <- ggplot(data = synth.reg.test2, aes(x = predvals, y = t, colour = (t-predvals)/t)) +
      geom_point() +
      geom_abline(intercept = 0, slope = 1) + 
      ggtitle(paste('first', finalvar, 'vars', round(r.squared, 3)))
    plottest
    assign(paste0('plottest', j), plottest)
    plottrain <- ggplot(data = for.model, aes(x = predvals, y = t, colour = (t-predvals)/t)) + 
      geom_point() +
      geom_abline(intercept = 0, slope = 1) + 
      ggtitle(paste('first', finalvar, 'vars'))
    assign(paste0('plottrain', j), plottrain)
    plottrain
    j <- j + 1
}
  
# From here we have laboriously discovered that we still know basically nothing.
  
  model <- lm(t ~ X.5 + X.400 + X.100, data = synth.reg)
  synth.reg.test2$predvals <- predict(model, newdata = synth.reg.test2)
  # Calculating the R^2 - automatically? Hmm
  tbar <- mean(synth.reg.test2$t)
  ressumsquare <- sum((synth.reg.test2$t - synth.reg.test2$predvals)**2)
  totsumsquare <- sum((synth.reg.test2$t - tbar)**2)
  r.squared <- 1 - (ressumsquare/totsumsquare)
  collect.r2[j] <- r.squared
  for.model$predvals <- predict(m)
  cat(j)
  plottest <- ggplot(data = synth.reg.test2, aes(x = predvals, y = t, colour = (t-predvals)/t)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1) + 
    ggtitle(paste('first', finalvar, 'vars', round(r.squared, 3)))
  plottest

  
  cook <- cooks.distance(best.model)

  best.model <- lm(t ~ X.13 + X.14 + X.5 + X.18 + X.400 + X.17 + X.11 + X.12 +
                     X.1 + X.16 + X.100 + X.15, data = synth.reg)
  inf <- influence(best.model)
  hat <- inf$hat
  ordered.hat <- hat[order(hat)]
  highest.leverage <- as.integer(names(ordered.hat)[351:400])
  mix <- synth.reg$residuals * synth.reg$leverage
  tryagain <- which(mix > 0.1626324)
  # Basically, we're going to re-run the code, except without these high leverage points.

  synth.reg.nolev <- synth.reg[setdiff(1:nrow(synth.reg), tryagain), ]
  new.best.model <-  lm(t ~ X.13 + X.14 + X.5 + X.18 + X.400 + X.17 + X.11 + X.12 +
                          X.1 + X.16 + X.100 + X.15, data = synth.reg.nolev)
  synth.reg$predvals <- predict(new.best.model, newdata = synth.reg)
  synth.reg$leverage <- hat
  synth.reg$residuals <- abs(best.model$residuals)
  plottrain <- ggplot(data = synth.reg, aes(x = predvals, y = t, 
                                            colour = residuals)) + 
    geom_point() +
    geom_abline(intercept = 0, slope = 1) +
    geom_abline(intercept = 1.96 * sqrt(var(synth.reg$residuals)), slope = 1) +
    geom_abline(intercept = -1.96 * sqrt(var(synth.reg$residuals)), slope = 1)    
  plottrain
    # Urgh, looks like we're going to have to run another sodding EM algorithm.
  # this function calculates the log likelihood for a multivariate norm
  like.norm <- function(input, mean, var) {
    normaliser <- 1/sqrt(pi * var * 2)
    inner <- -0.5 * ((input - mean) ** 2 ) / var
    return(normaliser * exp(inner))
  }

    em <- function(t, x, inipi) {
      obs <- length(t)
      mu1 <- rep(max(t), length(t))
      mu2 <- rep(min(t), length(t))
      var1 <- var(t) / 3
      var2 <- var(t) / 2
      # these will be used for testing convergence
      mu1old <- mu1
      mu2old <- mu2
      pi <- inipi
      gamma1 <- rep(0, length(t))   
      for( i in 1:100) {
        cat(i)
        # E step
        for(observation in 1:length(t)) {
          cat(observation)
          cat('.')
          l1.1 <- like.norm(t[observation], mu1[observation], var1)
          l1 <- pi * l1.1
          l2.1<-like.norm(t[observation], mu2[observation], var2)
          l2 <-  (1-pi) * l2.1
          gamma1[observation] <- l1 / (l1 + l2)
        }
        gamma2 <- 1 - gamma1
        # M step
        n1 <- sum(gamma1)
        n2 <- sum(gamma2)
        # Today is not a good day.
        w1 <- diag(gamma1)
        beta1 <- solve(t(x) %*% w1 %*% x) %*% t(x) %*% w1 %*% t
        mu1 <- x %*% beta1
        w2 <- diag(gamma2)
        beta2 <- solve(t(x) %*% w2 %*% x) %*% t(x) %*% w2 %*% t
        mu2 <- x %*% beta2
        var1 <- 1/n1 * sum(gamma1 * (t - mu1)**2)
        var2 <- 1/n2 * sum(gamma2 * (t - mu2)**2)
        #pi <- n1/(n1 + n2)
        # Check for convergence
        v <- sum(abs(mu1old - mu1), na.rm = TRUE) < 0.00001 & sum(abs(mu2old - mu2), na.rm = TRUE) < 0.0001
        if(v) { break }
        else { mu1old <- mu1
        mu2old <- mu2
        }
      }
      return(list("probabilities" = gamma1, "varexcept" = var1, "betaexcept" = beta1,
                  "varnorm" = var2, "betanorm" = beta2))
    }   
  
  matrix.for.robust <- matrix(0, 400, 12)
  matrix.for.robust[,1:12] <- c(synth.reg$X.13, synth.reg$X.14, synth.reg$X.5,
                                synth.reg$X.18, synth.reg$X.400, synth.reg$X.17,
                                synth.reg$X.11, synth.reg$X.12, synth.reg$X.1,
                               synth.reg$X.16, synth.reg$X.100, synth.reg$X.15)
  matrix.for.robust <- cbind(1, matrix.for.robust)
  t <- synth.reg$t
  letshopethisworks <- em(t, matrix.for.robust, 0.125)
  
  rownames(letshopethisworks$betanorm) <- names(best.model$coefficients)
  rownames(letshopethisworks$betaexcept) <- names(best.model$coefficients)
  synth.reg$predvals2 <- matrix.for.robust %*% letshopethisworks$betanorm
  synth.reg$predvals3 <- matrix.for.robust %*% letshopethisworks$betaexcept
  synth.reg$probabilities <- letshopethisworks$probabilities
  plottrain <- ggplot(data = synth.reg, aes(x = predvals3, y = t, 
                                            colour = probabilities)) + 
    geom_point() +
    geom_abline(intercept = 0, slope = 1) +
    geom_abline(intercept = 1.96 * sqrt(var(synth.reg$residuals)), slope = 1) +
    geom_abline(intercept = -1.96 * sqrt(var(synth.reg$residuals)), slope = 1)    
  plottrain
  
  
  # You want crude? I'll give you crude
  like.norm <- function(input, mean, var) {
    normaliser <- 1/sqrt(pi * var * 2)
    inner <- -0.5 * ((input - mean) ** 2 ) / var
    return(normaliser * exp(inner))
  }
  
  like <- rep(0, 400)

classifier <- function(dataset, repetitions, model) {
  like <- rep(0,nrow(dataset))
  for(rep in 1:repetitions) {
    cat(rep)
    for(obs in 1:nrow(dataset)) {
      cat(obs)
      like[obs] <- log(like.norm(model$model[obs,1], model$fitted.values[obs], 
                                 var(model$residuals)))
    }
      worst <- order(like)
      current.data <- dataset[setdiff(1:nrow(dataset), which(worst>350)),]
      model <-  lm(t ~ X.13 + X.14 + X.5 + X.18 + X.400 + X.17 + X.11 + X.12 +
                  X.1 + X.16 + X.100 + X.15, data = current.data)
      return(list(like = like, model = model, worst = worst))
  }
}

synth.reg$predvals <- predict(p$model, newdata = synth.reg)
synth.reg$leverage <- hat
synth.reg$residuals <- abs(best.model$residuals)
synth.reg$exclude <- p$like
synth.reg$exclude2 <- 0

plottrain <- ggplot(data = synth.reg, aes(x = predvals, y = t, 
                                          colour = exclude)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  geom_abline(intercept = 1.96 * sqrt(var(synth.reg$residuals)), slope = 1) +
  geom_abline(intercept = -1.96 * sqrt(var(synth.reg$residuals)), slope = 1)    
plottrain