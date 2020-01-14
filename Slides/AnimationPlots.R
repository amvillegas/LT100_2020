plotParamPath <- function(modelfit, j, ktlabels){
  maxModel <- extractStMoMo(modelfit, length(modelfit$lambda))
  #----------------------------------
  # Compute limits for each plot 
  #----------------------------------
  
  #lambda
  
  xlim_lambda <- c(modelfit$lambda[1], modelfit$lambda[length(modelfit$lambda)])
  
  #ax
  ylim_ax <- range(sapply(modelfit$beta, function(x) x$ax))
  
  #bx
  ktIndex <- as.numeric(rownames(maxModel$kt))
  ylim_kt <- matrix(NA, nrow = maxModel$model$N, ncol = 2)
  for (i in 1:maxModel$model$N){
    ylim_kt[i, ] <- range(sapply(modelfit$beta, function(x) x$kt[ktIndex[i], ]))
  }
  
  #gc
  ylim_gc <- range(sapply(modelfit$beta, function(x) x$gc))
  
  
  #----------------------------------
  # Do the plots 
  #----------------------------------
  
  
  ages <- modelfit$ages
  years <- modelfit$years
  cohorts <- modelfit$cohorts
  
  
  ncol <-  ceiling((maxModel$model$N + 2)/2)
  
  laymat <- c(rep(1,ncol), 
              rep(seq(2,(ncol+1)), 2),
              rep(seq(ncol+2,ncol*2+1), 2))  
  layout(matrix(laymat, 5, ncol, byrow = TRUE))    
  #lambda
  par(mar=c(2,1,2,1))
  dotchart(x = modelfit$lambda[j], xlim = xlim_lambda, pch = 16, col = "red", 
           main = expression(lambda))
  
  #ax
  par(mar=c(2,1,2,1))
  if (max(abs(modelfit$beta[[j]]$ax)) == 0) {color = "white"} else {color = "red"}
  plot(x = ages, y = modelfit$beta[[j]]$ax, ylim = ylim_ax, type = "l", 
       col = color, yaxt = 'n',ylab = '', main = expression(alpha[x]))
  
  
  #bx
  for (i in 1:maxModel$model$N){
    if (max(abs(modelfit$beta[[j]]$kt[ktIndex[i],])) == 0) {color = "white"} else {color = "red"}
    plot(x = years, y = modelfit$beta[[j]]$kt[ktIndex[i],],
         ylim = ylim_kt[i, ] , type = "l", col = color, yaxt = 'n', 
         main = ktlabels[ktIndex[i]])
  }
  
  #gc
  par(mar=c(2,1,2,1))
  if (max(abs(modelfit$beta[[j]]$gc)) == 0) {color = "white"} else {color = "red"}
  plot(x = cohorts, y = modelfit$beta[[j]]$gc, ylim = ylim_gc, type = "l", 
       col = color, yaxt = 'n',ylab = '', main = expression(gamma[t-x]))

}

plotFitPath <- function(modelfit, j, agesplot){
  
  
  currentfit <- extractStMoMo(modelfit, j)
  
  obsq <- currentfit$Dxt/currentfit$Ext
  fitq <- fitted(currentfit, type = "rates")
  
  years <- currentfit$years
  
  
  ncol <-  ceiling((length(agesplot))/2)
  laymat <- c(rep(1,ncol), 
              rep(seq(2,(ncol+1)), 2),
              rep(seq(ncol+2,ncol*2+1), 2))  
  layout(matrix(laymat, 5, ncol, byrow = TRUE))
  
  #lambda
  xlim_lambda <- c(modelfit$lambda[1], modelfit$lambda[length(modelfit$lambda)])
  par(mar=c(2,1,2,1))
  dotchart(x = modelfit$lambda[j], xlim = xlim_lambda, pch = 16, col = "red", 
           main = expression(lambda))
  
  #rates
  for (i in 1:length(agesplot)){
    par(mar=c(2,1,2,1))
    plot(x = years, y = obsq[agesplot[i],], yaxt = 'n', ylab = '', 
         ylim = c(min(obsq[agesplot[i], ])*0.9, max(obsq[agesplot[i], ])*1.1))
    points(x = years, y = fitq[agesplot[i],], col="red", type="l", yaxt = 'n', ylab = '')
    title(main = paste("Age ", agesplot[i], sep = ""), cex.lab = 0.25)
  }
}
