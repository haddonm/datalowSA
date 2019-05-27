<<<<<<< HEAD
## ----setup, include=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(
	echo = TRUE,
	message = FALSE,
	warning = FALSE)
options(knitr.kable.NA = "",
        knitr.table.format = "pandoc")

options("show.signif.stars"=FALSE,"stringsAsFactors"=FALSE,
        "max.print"=50000,"width"=240)

library(datalowSA)
suppressPackageStartupMessages(library(knitr))


## ----"aspmData", echo=TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# library(datalowSA)
data(dataspm)
fish <- dataspm$fish
props <- dataspm$props # length-, weight-, maturity- and selectivity-at-age


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(glb <- dataspm$glb)


## ----"guessparam", fig.width=7,fig.height=4.5-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
pars <- c(12.9,0.25)
aspmLL(pars,infish=fish,inglb=glb,inprops=props)
fishery <- dynamics(pars,infish=fish,inglb=glb,inprops = props)
plotASPM(fishery)


## ----"firstASPM", echo=TRUE, warning=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

pars <- c(13.7,0.19)
ans <- fitASPM(pars,infish=fish,inglb=glb,inprops=props)
outoptim(ans) # a tidier way of printing the list output from optim
fishery <- dynamics(ans$par,infish=fish,inglb=glb,inprops = props)


## ----printfishery, echo=TRUE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
kable(fishery,digits=c(0,3,3,3,3,3,4,4,3))

## ----plot2pars, echo=TRUE, fig.width=7, fig.height=4.5, fig.align="center"--------------------------------------------------------------------------------------------------------------------------------------------------------------------
plotASPM(fishery,CI=NA)

## ----"ASPM_deep", echo=TRUE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
data(fishdat)
fish <- fishdat$fish
glb <- fishdat$glb
props <- fishdat$props
pars <- c(14,0.3)
ans <- fitASPM(pars,infish=fish,inglb=glb,inprops = props)
str(ans)

## ----thedynamics, echo=TRUE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
fishery <- dynamics(ans$par,infish=fish,inglb=glb,inprops = props)
print(fishery)


## ----addCI, echo=TRUE, fig.width=7,fig.height=5.5, fig.align="center"-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ceCI <- getLNCI(fishery[,"PredCE"],ans$par[2])
plotASPM(fishery,CI=ceCI)


## ----threepars, echo=TRUE,fig.width=7,fig.height=5,fig.align="center"-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
data(dataspm)
fish <- dataspm$fish
glb <- dataspm$glb
props <- dataspm$props
pars <- c(14,0.19,0.6) # Fit 3 par__aspm__with penalty
# pars <- c(13.2794439,0.1731744,0.4933178) # for a second time through
scalepar <- magnitude(pars)
bestL <- optim(pars,aspmPENLL,method="Nelder-Mead",
              infish=fish,inglb=glb,inprops=props,
              control=list(maxit = 1000,parscale=scalepar))
outoptim(bestL)
fisheryPen <- dynamics(bestL$par,infish=fish,inglb=glb,inprops=props)
ceCI <- getLNCI(fisheryPen[,"PredCE"],bestL$par[2])
plotASPM(fisheryPen,CI=ceCI)

## ----robustness, echo=TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  set.seed(12335)  # to get repeatable results, normally you would not do this
  data(fishdat)
  fish <- fishdat$fish
  glb <- fishdat$glb
  props <- fishdat$props
  pars <- c(14,0.3)
  out <- robustASPM(pars,fish,glb,props,scaler=20,N=15,console=FALSE)
  str(out)
  print(round(out$results,4))


## ----robustdataspm, echo=TRUE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  set.seed(12235)  # to get repeatable results, normally you would not do this
  data(dataspm)
  fish <- dataspm$fish
  glb <- dataspm$glb
  props <- dataspm$props
  pars <- c(14,0.2,0.6)
  out <- robustASPM(pars,fish,glb,props,scaler=15,N=10,console=FALSE)
  print(round(out$results,3))
  print(round(out$range,3))

## ----correlations, echo=TRUE, fig.width=7,fig.height=6, fig.align="center"--------------------------------------------------------------------------------------------------------------------------------------------------------------------
cor(out$results[,c("LnR0","Depl","-veLL","MSY")])  # correlations between outputs
 #plotprep(width=8,height=6)
intensity <- 2   #  how many points overlapping = maximum colour
pairs(out$results[,c("LnR0","Depl","-veLL","MSY")],pch=16,
      col=rgb(1,0,0,1/intensity),font=7,font.labels = 7)


## ----production_curve, echo=TRUE,fig.width=7,fig.height=5,fig.align="center"------------------------------------------------------------------------------------------------------------------------------------------------------------------
data(dataspm)
fish <- dataspm$fish
glb <- dataspm$glb
props <- dataspm$props
pars <- c(13.75,0.189667,0.6) # Fit 3 par__aspm__with penalty
bestL <- optim(pars,aspmPENLL,method="Nelder-Mead",
              infish=fish,inglb=glb,inprops=props,
              control=list(maxit = 1000, parscale = c(10,1,0.1)))
# two times through
bestL <- optim(bestL$par,aspmPENLL,method="Nelder-Mead",
              infish=fish,inglb=glb,inprops=props,
              control=list(maxit = 1000, parscale = c(10,1,0.1)))
par <- bestL$par
print(par)
prod <- getProductionC(exp(par[1]),fish,glb,props,
                      Hrg=c(0.01,0.45,0.005),nyr=50)
head(round(prod,3),6)
tail(round(prod,3),6)
anspen <- prodASPM(prod,target=0.48,console=FALSE,plot=TRUE)
round(anspen,3)


## ----phaseplot, echo=TRUE, fig.width=7,fig.height=6,fig.align="center"------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#   plotprep(width=7,height=5.5)
fisheryPen <- dynamics(bestL$par,infish=fish,inglb=glb,inprops=props)
outs <- aspmphaseplot(fisheryPen,prod,anspen,Blim=0.2,fnt=7)


## ----bootstrapsetup, echo=TRUE, fig.width=7,fig.height=5--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  library(datalowSA)
library(datalowSA)
data(dataspm)
fish <- dataspm$fish
glb <- dataspm$glb
props <- dataspm$props
pars <- c(13.5,0.18,0.5)
bestL <- fitASPM(pars,fish,glb,props,callfun=aspmPENLL)
fishery <- dynamics(bestL$par,fish,glb,props)
kable(fishery,digits=c(0,1,1,3,3,3,3,3,3))


## ----takebootstrap, echo=TRUE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
reps <- 20
starttime <- Sys.time()
answer <- bootASPM(fish,glb,props,bestL$par,iter=reps)
Sys.time() - starttime
str(answer,max.level=1)


## ----finalyear, echo=TRUE, fig.width=7,fig.height=6-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
yrs <- fishery[,"Year"]
nyrs <- length(yrs)
par(mfrow=c(2,2),mai=c(0.45,0.45,0.05,0.05)) 
par(cex=0.85, mgp=c(1.35,0.35,0), font.axis=7,font=7,font.lab=7)  
label <- names(answer$result[1,1,])
label <- label[-3]  # remove CPUE
numvar <- length(label)
bootvar <- answer$result[,nyrs,label[1]]
for (i in 1:numvar) { # i=3
   bootvar <- answer$result[,nyrs,label[i]]
   quantCI <- quantile(bootvar,probs=c(0.05,0.5,0.95),na.rm=TRUE)
   hist(bootvar,breaks=30,main="",xlab=label[i],col="red")
   abline(v=quantCI,col=c(4,4,4),lwd=c(1,2,1))
}

## ----perceCI, echo=TRUE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
pickvar <- "Deplete"
bootvar <- answer$result[,,pickvar]
yrs <- as.numeric(colnames(bootvar))
nyrs <- length(yrs)
quantCI <- t(apply(bootvar,2,quants))
kable(quantCI,digits=c(3,3,3,3,3,3))

## ----plottimeseries, echo=TRUE, fig.width=7,fig.height=4.5------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ymax <- getmaxy(bootvar)
par(mfrow=c(1,1),mai=c(0.45,0.45,0.05,0.05)) 
par(cex=0.85, mgp=c(1.35,0.35,0), font.axis=7,font=7,font.lab=7)
plot(yrs,bootvar[1,],type="n",lwd=1,col=0,ylim=c(0,ymax),
     panel.first = grid(),xlab="",ylab=pickvar)
for (i in 1:reps) lines(yrs,bootvar[i,],lwd=1,col="grey")
lines(yrs,quantCI[,"50%"],lwd=2,col="red")
arrows(x0=yrs,y0=quantCI[,"5%"],y1=quantCI[,"95%"],
       col=2,lwd=1,length=0.035,angle=90,code=3)

=======
## ----setup, include=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(
	echo = TRUE,
	message = FALSE,
	warning = FALSE)
options(knitr.kable.NA = "",
        knitr.table.format = "pandoc")

options("show.signif.stars"=FALSE,"stringsAsFactors"=FALSE,
        "max.print"=50000,"width"=240)

library(datalowSA)
suppressPackageStartupMessages(library(knitr))


## ----"aspmData", echo=TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# library(datalowSA)
data(dataspm)
fish <- dataspm$fish
props <- dataspm$props # length-, weight-, maturity- and selectivity-at-age


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(glb <- dataspm$glb)


## ----"guessparam", fig.width=7,fig.height=4.5-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
pars <- c(12.9,0.25)
aspmLL(pars,infish=fish,inglb=glb,inprops=props)
fishery <- dynamics(pars,infish=fish,inglb=glb,inprops = props)
plotASPM(fishery)


## ----"firstASPM", echo=TRUE, warning=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

pars <- c(13.7,0.19)
ans <- fitASPM(pars,infish=fish,inglb=glb,inprops=props)
outoptim(ans) # a tidier way of printing the list output from optim
fishery <- dynamics(ans$par,infish=fish,inglb=glb,inprops = props)


## ----printfishery, echo=TRUE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
kable(fishery,digits=c(0,3,3,3,3,3,4,4,3))

## ----plot2pars, echo=TRUE, fig.width=7, fig.height=4.5, fig.align="center"--------------------------------------------------------------------------------------------------------------------------------------------------------------------
plotASPM(fishery,CI=NA)

## ----"ASPM_deep", echo=TRUE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
data(fishdat)
fish <- fishdat$fish
glb <- fishdat$glb
props <- fishdat$props
pars <- c(14,0.3)
ans <- fitASPM(pars,infish=fish,inglb=glb,inprops = props)
str(ans)

## ----thedynamics, echo=TRUE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
fishery <- dynamics(ans$par,infish=fish,inglb=glb,inprops = props)
print(fishery)


## ----addCI, echo=TRUE, fig.width=7,fig.height=5.5, fig.align="center"-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ceCI <- getLNCI(fishery[,"PredCE"],ans$par[2])
plotASPM(fishery,CI=ceCI)


## ----threepars, echo=TRUE,fig.width=7,fig.height=5,fig.align="center"-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
data(dataspm)
fish <- dataspm$fish
glb <- dataspm$glb
props <- dataspm$props
pars <- c(14,0.19,0.6) # Fit 3 par__aspm__with penalty
# pars <- c(13.2794439,0.1731744,0.4933178) # for a second time through
scalepar <- magnitude(pars)
bestL <- optim(pars,aspmPENLL,method="Nelder-Mead",
              infish=fish,inglb=glb,inprops=props,
              control=list(maxit = 1000,parscale=scalepar))
outoptim(bestL)
fisheryPen <- dynamics(bestL$par,infish=fish,inglb=glb,inprops=props)
ceCI <- getLNCI(fisheryPen[,"PredCE"],bestL$par[2])
plotASPM(fisheryPen,CI=ceCI)

## ----robustness, echo=TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  set.seed(12335)  # to get repeatable results, normally you would not do this
  data(fishdat)
  fish <- fishdat$fish
  glb <- fishdat$glb
  props <- fishdat$props
  pars <- c(14,0.3)
  out <- robustASPM(pars,fish,glb,props,scaler=20,N=15,console=FALSE)
  str(out)
  print(round(out$results,4))


## ----robustdataspm, echo=TRUE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  set.seed(12235)  # to get repeatable results, normally you would not do this
  data(dataspm)
  fish <- dataspm$fish
  glb <- dataspm$glb
  props <- dataspm$props
  pars <- c(14,0.2,0.6)
  out <- robustASPM(pars,fish,glb,props,scaler=15,N=10,console=FALSE)
  print(round(out$results,3))
  print(round(out$range,3))

## ----correlations, echo=TRUE, fig.width=7,fig.height=6, fig.align="center"--------------------------------------------------------------------------------------------------------------------------------------------------------------------
cor(out$results[,c("LnR0","Depl","-veLL","MSY")])  # correlations between outputs
 #plotprep(width=8,height=6)
intensity <- 2   #  how many points overlapping = maximum colour
pairs(out$results[,c("LnR0","Depl","-veLL","MSY")],pch=16,
      col=rgb(1,0,0,1/intensity),font=7,font.labels = 7)


## ----production_curve, echo=TRUE,fig.width=7,fig.height=5,fig.align="center"------------------------------------------------------------------------------------------------------------------------------------------------------------------
data(dataspm)
fish <- dataspm$fish
glb <- dataspm$glb
props <- dataspm$props
pars <- c(13.75,0.189667,0.6) # Fit 3 par__aspm__with penalty
bestL <- optim(pars,aspmPENLL,method="Nelder-Mead",
              infish=fish,inglb=glb,inprops=props,
              control=list(maxit = 1000, parscale = c(10,1,0.1)))
# two times through
bestL <- optim(bestL$par,aspmPENLL,method="Nelder-Mead",
              infish=fish,inglb=glb,inprops=props,
              control=list(maxit = 1000, parscale = c(10,1,0.1)))
par <- bestL$par
print(par)
prod <- getProductionC(exp(par[1]),fish,glb,props,
                      Hrg=c(0.01,0.45,0.005),nyr=50)
head(round(prod,3),6)
tail(round(prod,3),6)
anspen <- prodASPM(prod,target=0.48,console=FALSE,plot=TRUE)
round(anspen,3)


## ----phaseplot, echo=TRUE, fig.width=7,fig.height=6,fig.align="center"------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#   plotprep(width=7,height=5.5)
fisheryPen <- dynamics(bestL$par,infish=fish,inglb=glb,inprops=props)
outs <- aspmphaseplot(fisheryPen,prod,anspen,Blim=0.2,fnt=7)


## ----bootstrapsetup, echo=TRUE, fig.width=7,fig.height=5--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  library(datalowSA)
library(datalowSA)
data(dataspm)
fish <- dataspm$fish
glb <- dataspm$glb
props <- dataspm$props
pars <- c(13.5,0.18,0.5)
bestL <- fitASPM(pars,fish,glb,props,callfun=aspmPENLL)
fishery <- dynamics(bestL$par,fish,glb,props)
kable(fishery,digits=c(0,1,1,3,3,3,3,3,3))


## ----takebootstrap, echo=TRUE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
reps <- 20
starttime <- Sys.time()
answer <- bootASPM(fish,glb,props,bestL$par,iter=reps)
Sys.time() - starttime
str(answer,max.level=1)


## ----finalyear, echo=TRUE, fig.width=7,fig.height=6-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
yrs <- fishery[,"Year"]
nyrs <- length(yrs)
par(mfrow=c(2,2),mai=c(0.45,0.45,0.05,0.05)) 
par(cex=0.85, mgp=c(1.35,0.35,0), font.axis=7,font=7,font.lab=7)  
label <- names(answer$result[1,1,])
label <- label[-3]  # remove CPUE
numvar <- length(label)
bootvar <- answer$result[,nyrs,label[1]]
for (i in 1:numvar) { # i=3
   bootvar <- answer$result[,nyrs,label[i]]
   quantCI <- quantile(bootvar,probs=c(0.05,0.5,0.95),na.rm=TRUE)
   hist(bootvar,breaks=30,main="",xlab=label[i],col="red")
   abline(v=quantCI,col=c(4,4,4),lwd=c(1,2,1))
}

## ----perceCI, echo=TRUE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
pickvar <- "Deplete"
bootvar <- answer$result[,,pickvar]
yrs <- as.numeric(colnames(bootvar))
nyrs <- length(yrs)
quantCI <- t(apply(bootvar,2,quants))
kable(quantCI,digits=c(3,3,3,3,3,3))

## ----plottimeseries, echo=TRUE, fig.width=7,fig.height=4.5------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ymax <- getmaxy(bootvar)
par(mfrow=c(1,1),mai=c(0.45,0.45,0.05,0.05)) 
par(cex=0.85, mgp=c(1.35,0.35,0), font.axis=7,font=7,font.lab=7)
plot(yrs,bootvar[1,],type="n",lwd=1,col=0,ylim=c(0,ymax),
     panel.first = grid(),xlab="",ylab=pickvar)
for (i in 1:reps) lines(yrs,bootvar[i,],lwd=1,col="grey")
lines(yrs,quantCI[,"50%"],lwd=2,col="red")
arrows(x0=yrs,y0=quantCI[,"5%"],y1=quantCI[,"95%"],
       col=2,lwd=1,length=0.035,angle=90,code=3)

>>>>>>> 02950d79bbe34812ac9c4d09e13b418822ea40de
