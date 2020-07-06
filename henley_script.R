###Henley script
###Author: William Hammond
###Date: 01-Apr-2019

henley <- read.csv(file.choose())
head(henley)

#install.packages("drc")
library(drc)
####Started here, but this again is dose-response.
mL <- drm(A750 ~ Days, data = henley, fct = L.3(), type = "continuous")
summary(mL)
plot(mL, type = "all", xlim=c(0,15), log="")

head(mL)

#### Here, use package "nls" for its SSlogis() function, a self-starting logistic function.

log.fit <- function(dep, ind, yourdata){
  #Self-starting...
  
  y <- yourdata[, dep]
  x <- yourdata[, ind]
  
  log.ss <- nls(y ~ SSlogis(x, phi1, phi2, phi3))
  
  #C
  L <- summary(log.ss)$coef[1] #upper asymptote
  #a
  x0 <- summary(log.ss)$coef[2]
  #k
  k <- (1 / summary(log.ss)$coef[3]) #
  
  plot(y ~ x, main = "Logistic Function", xlab=ind, ylab=dep)
  lines(0:max(x), predict(log.ss, data.frame(x=0:max(x))), col="red")
  
  r1 <- sum((x - mean(x))^2)
  r2 <- sum(residuals(log.ss)^2)
  
  r_sq <- (r1 - r2) / r1 #likelihood ratio r2
  
  out <- data.frame(cbind(c(L=L, x0=x0, k=k, R.value=sqrt(r_sq))))
  names(out)[1] <- "Logistic Curve"
  
  return(out)
}

###Now, the function exists so that you can type log.fit("dependent variable","independent variable", dataframe_name)
log.fit("A750", "Days", henley)

