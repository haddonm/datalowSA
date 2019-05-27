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


## ----echo=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
data(invert)
fish <- invert$fish   # contains available fishery data
head(fish,15)


## ----"catch_MSY", echo=TRUE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#library(datalowSA)

glb <- invert$glb     # contains available biological data
checkdata(invert)
              # normally one would run at least 20000 iterations, preferably more
reps <- 5000  # read the help for run_cMSY to understand each input parameter
              # try changing the value of sigpr and makeplots to TRUE                 
answer <- run_cMSY(fish,glb,n=reps,sigpR=0.025,maximumH=1.0)
str(answer,max.level=1)


## ----phaseplot, fig.width=5.9,fig.height=4----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
out <- cMSYphaseplot(answer,fish)


## ----strout, echo = TRUE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
str(out)

## ----summaryofanswer--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summcMSY <- summarycMSY(answer,fish,final=TRUE)
str(summcMSY,max.level=1)  # try max.level = 2


## ----plottraj, echo = TRUE, fig.width=7, fig.height=4.5---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
out <- plottrajectory(answer$R1,fish$year,fish$catch,answer$parbound,
                      oneplot=FALSE,Bmax=25000,
                      scalar=1.0,plotout=TRUE,plotall=15)


## ----plotcMSY, echo = TRUE, fig.width=7, fig.height=4-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plotcMSY6(summcMSY,fish[,"catch"],label=glb$spsname)


## ----echo=TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
results <- pulloutStats(answer$R1)
print(str(results))


## ----echo = FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
round(results$output,2)


## ----plottrend, echo-TRUE, fig.width=5.9, fig.height=4.72-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

out <- plottrajectory(answer$R1,fish$year,fish$catch,answer$parbound,
                      oneplot=TRUE,scalar=1.0,plotout=TRUE,plotall=7)


## ----echo=TRUE, fig.width=6.5,fig.height=4----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
results <- pulloutStats(answer$R1)
# Note the use of constC=0, we are not doing any projections yet so no constant catches
effectC <- plotconstC(results$deplet,endyear=2017,constC=0,limit=0.2,target=0.4,
                      console=TRUE,intensity=NA,contours=TRUE) 
abline(h=c(0.2,0.3,0.4),col=3,lwd=2)

## ----echo=TRUE, fig.width=6.5,fig.height=4----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

effectC <- plotconstC(results$deplet,endyear=2017,constC=0,limit=0.2,target=0.4,
                      console=FALSE,intensity=30) 
abline(h=c(0.2,0.3,0.4),col=3,lwd=2)

## ----makeprojections, echo = TRUE, fig.width=6.5, fig.height=4--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
output <- doconstC(answer$R1,projn=10,constCatch=250,lastyear=2017,limit=0.2,
                   target=0.48)


## ----trendmsy, fig.width=6.5,fig.height=4-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
r <- summcMSY$r
K <- summcMSY$K
meanmsy <- trendMSY(summcMSY$r,summcMSY$K,inc=300)
centr <- central(r)
centK <- central(K)
means <- central(summcMSY$msy)
avMSY <- means["Geometric","Mean"]
# plotprep(width=7,height=4.0)
par(mfrow=c(1,1),mai=c(0.45,0.45,0.05,0.05),oma=c(0.0,0,0.0,0.0)) 
par(cex=0.85, mgp=c(1.35,0.35,0), font.axis=7,font=7,font.lab=7)
plot(r,K,type="p",pch=16,col=rgb(1,0,0,1/3),panel.first=grid())
rval <- seq(0.1,0.6,length=100)
kval <- (4 * avMSY)/rval  # calculate the K value that would generate avMSY
lines(rval,kval,col=4,lwd=2)
pickvalid <- which(meanmsy[,"N"] > 0)
lines(meanmsy[pickvalid,"rcenter"],meanmsy[pickvalid,"Kcenter"],lwd=2,col=3)
points(centr[2,1],centK[2,1],pch=16,cex=2,col=1)


## ----msycontour, fig.width=6.5,fig.height=4---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
r <- summcMSY$r
K <- summcMSY$K
avMSY <- seq(100,600,100)  
nMSY <- length(avMSY)
# plotprep(width=7,height=4.0)
par(mfrow=c(1,1),mai=c(0.45,0.45,0.05,0.05),oma=c(0.0,0,0.0,0.0)) 
par(cex=0.85, mgp=c(1.35,0.35,0), font.axis=7,font=7,font.lab=7)
plot(r,K,type="p",pch=16,col=rgb(1,0,0,1/3),panel.first=grid(),ylim=c(2500,9000))
rval <- seq(0.1,0.6,length=100)
for (i in 1:nMSY) {
   kval <- (4 * avMSY[i])/rval  # calculate the K value that would generate avMSY
   lines(rval,kval,col=4,lwd=2)
}
x <- seq(0.15,0.375,length=nMSY)
yval <- 4 * (avMSY/x)
for (i in 1:nMSY) text(x[i],yval[i],avMSY[i],cex=1,font=7,pos=4)


## ----fullcatches, echo=TRUE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(datalowSA)
data(invert)
fish <- invert$fish
glb <- invert$glb

reps <- 5000  # one would run at least 20000, preferably more
answer <- run_cMSY(fish,glb,n=reps,sigpR=0.025)
summcMSY <- summarycMSY(answer,fish,final=TRUE)
ans <- pulloutStats(answer$R1)
round(ans$output[,1:3],3)

## ----reducecatches, echo=TRUE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
commprop <- 0.8
propcom <- rnorm(31,mean=commprop,sd=0.025) # 31 equals the number of years
fishC <- fish
fishC[,"catch"] <- fishC[,"catch"]*propcom
answerC <- run_cMSY(fishC,glb,n=5000,sigpR=0.025)
ansC <- pulloutStats(answerC$R1)
#str(ans10)
round(ansC$output[,1:3],3)
round(ans$output[,1:3],3)
msy <- ansC$output["MSY","Mean"]/ans$output["MSY","Mean"]
depl <- ansC$output["CurrDepl","Mean"]/ans$output["CurrDepl","Mean"]
cat("reduced catch MSY/full catch MSY = ", msy, mean(propcom),"\n")
cat("Proportion of Current Depetion = ",depl,"\n")

## ----trendcatches, echo=TRUE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
propcom <- seq(0.8,0.5,length=31) # 31 equals the number of years
fishC <- fish
fishC[,"catch"] <- fishC[,"catch"]*propcom
answerC <- run_cMSY(fishC,glb,n=5000,sigpR=0.025)
ansC <- pulloutStats(answerC$R1)
round(ansC$output[,1:3],3)
round(ans$output[,1:3],3)
msy <- ansC$output["MSY","Mean"]/ans$output["MSY","Mean"]
depl <- ansC$output["CurrDepl","Mean"]/ans$output["CurrDepl","Mean"]
cat("reduced catch MSY/full catch MSY = ", msy, mean(propcom),"\n")
cat("Proportion of Current Depletion = ",depl,"\n")

## ----partialphaseplot, echo = TRUE, fig.width=7, fig.height=5---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
cMSYphaseplot(answerC,fishC)


## ----partialphaseplot2, echo = TRUE, fig.width=7, fig.height=5--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
cMSYphaseplot(answer,fish)


## ----"highharvest", echo=TRUE, fig.width=7,fig.height=5---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# library(datalowSA)
data(invert)
fish <- invert$fish
glb <- invert$glb 
# normally one would run 20000+ replicates, but for speed here we use 5000
answer <- run_cMSY(fish,glb,n=5000,sigpR=0.025,finaldepl=c(0.05,0.5),maximumH=1.0)
ans <- pulloutStats(answer$R1)
round(ans$output[,1:3],3)
out <- plottrajectory(answer$R1,fish$year,fish$catch,answer$parbound,
                      oneplot=TRUE,scalar=1.0,plotout=TRUE,plotall=7)

## ----"controlharvest", echo=TRUE, fig.width=7,fig.height=5------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# library(datalowSA)
# normally one would run 20000+ replicates, but for speed we use 5000
answer <- run_cMSY(fish,glb,n=5000,sigpR=0.025,finaldepl=c(0.05,0.5),maximumH=0.5)
ans <- pulloutStats(answer$R1)
round(ans$output[,1:3],3)
out <- plottrajectory(answer$R1,fish$year,fish$catch,answer$parbound,
                      oneplot=TRUE,scalar=1.0,plotout=TRUE,plotall=7)

## ----"controlharvest2", echo=TRUE, fig.width=7,fig.height=5-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# library(datalowSA)
# normally one would run 20000+ replicates, but for speed we use 5000
answer <- run_cMSY(fish,glb,n=5000,sigpR=0,finaldepl=c(0.05,0.5),maximumH=1.0)
ans <- pulloutStats(answer$R1)
round(ans$output[,1:3],3)
out <- plotconstC(ans$deplet,endyear=2017,constC=0)


## ----"plotthefishery",  echo=TRUE, fig.width=7,fig.height=5-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plotfish(fish,glb)


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


## ----echo=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
data(invert)
fish <- invert$fish   # contains available fishery data
head(fish,15)


## ----"catch_MSY", echo=TRUE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#library(datalowSA)

glb <- invert$glb     # contains available biological data
checkdata(invert)
              # normally one would run at least 20000 iterations, preferably more
reps <- 5000  # read the help for run_cMSY to understand each input parameter
              # try changing the value of sigpr and makeplots to TRUE                 
answer <- run_cMSY(fish,glb,n=reps,sigpR=0.025,maximumH=1.0)
str(answer,max.level=1)


## ----phaseplot, fig.width=5.9,fig.height=4----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
out <- cMSYphaseplot(answer,fish)


## ----strout, echo = TRUE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
str(out)

## ----summaryofanswer--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summcMSY <- summarycMSY(answer,fish,final=TRUE)
str(summcMSY,max.level=1)  # try max.level = 2


## ----plottraj, echo = TRUE, fig.width=7, fig.height=4.5---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
out <- plottrajectory(answer$R1,fish$year,fish$catch,answer$parbound,
                      oneplot=FALSE,Bmax=25000,
                      scalar=1.0,plotout=TRUE,plotall=15)


## ----plotcMSY, echo = TRUE, fig.width=7, fig.height=4-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plotcMSY6(summcMSY,fish[,"catch"],label=glb$spsname)


## ----echo=TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
results <- pulloutStats(answer$R1)
print(str(results))


## ----echo = FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
round(results$output,2)


## ----plottrend, echo-TRUE, fig.width=5.9, fig.height=4.72-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

out <- plottrajectory(answer$R1,fish$year,fish$catch,answer$parbound,
                      oneplot=TRUE,scalar=1.0,plotout=TRUE,plotall=7)


## ----echo=TRUE, fig.width=6.5,fig.height=4----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
results <- pulloutStats(answer$R1)
# Note the use of constC=0, we are not doing any projections yet so no constant catches
effectC <- plotconstC(results$deplet,endyear=2017,constC=0,limit=0.2,target=0.4,
                      console=TRUE,intensity=NA,contours=TRUE) 
abline(h=c(0.2,0.3,0.4),col=3,lwd=2)

## ----echo=TRUE, fig.width=6.5,fig.height=4----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

effectC <- plotconstC(results$deplet,endyear=2017,constC=0,limit=0.2,target=0.4,
                      console=FALSE,intensity=30) 
abline(h=c(0.2,0.3,0.4),col=3,lwd=2)

## ----makeprojections, echo = TRUE, fig.width=6.5, fig.height=4--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
output <- doconstC(answer$R1,projn=10,constCatch=250,lastyear=2017,limit=0.2,
                   target=0.48)


## ----trendmsy, fig.width=6.5,fig.height=4-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
r <- summcMSY$r
K <- summcMSY$K
meanmsy <- trendMSY(summcMSY$r,summcMSY$K,inc=300)
centr <- central(r)
centK <- central(K)
means <- central(summcMSY$msy)
avMSY <- means["Geometric","Mean"]
# plotprep(width=7,height=4.0)
par(mfrow=c(1,1),mai=c(0.45,0.45,0.05,0.05),oma=c(0.0,0,0.0,0.0)) 
par(cex=0.85, mgp=c(1.35,0.35,0), font.axis=7,font=7,font.lab=7)
plot(r,K,type="p",pch=16,col=rgb(1,0,0,1/3),panel.first=grid())
rval <- seq(0.1,0.6,length=100)
kval <- (4 * avMSY)/rval  # calculate the K value that would generate avMSY
lines(rval,kval,col=4,lwd=2)
pickvalid <- which(meanmsy[,"N"] > 0)
lines(meanmsy[pickvalid,"rcenter"],meanmsy[pickvalid,"Kcenter"],lwd=2,col=3)
points(centr[2,1],centK[2,1],pch=16,cex=2,col=1)


## ----msycontour, fig.width=6.5,fig.height=4---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
r <- summcMSY$r
K <- summcMSY$K
avMSY <- seq(100,600,100)  
nMSY <- length(avMSY)
# plotprep(width=7,height=4.0)
par(mfrow=c(1,1),mai=c(0.45,0.45,0.05,0.05),oma=c(0.0,0,0.0,0.0)) 
par(cex=0.85, mgp=c(1.35,0.35,0), font.axis=7,font=7,font.lab=7)
plot(r,K,type="p",pch=16,col=rgb(1,0,0,1/3),panel.first=grid(),ylim=c(2500,9000))
rval <- seq(0.1,0.6,length=100)
for (i in 1:nMSY) {
   kval <- (4 * avMSY[i])/rval  # calculate the K value that would generate avMSY
   lines(rval,kval,col=4,lwd=2)
}
x <- seq(0.15,0.375,length=nMSY)
yval <- 4 * (avMSY/x)
for (i in 1:nMSY) text(x[i],yval[i],avMSY[i],cex=1,font=7,pos=4)


## ----fullcatches, echo=TRUE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(datalowSA)
data(invert)
fish <- invert$fish
glb <- invert$glb

reps <- 5000  # one would run at least 20000, preferably more
answer <- run_cMSY(fish,glb,n=reps,sigpR=0.025)
summcMSY <- summarycMSY(answer,fish,final=TRUE)
ans <- pulloutStats(answer$R1)
round(ans$output[,1:3],3)

## ----reducecatches, echo=TRUE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
commprop <- 0.8
propcom <- rnorm(31,mean=commprop,sd=0.025) # 31 equals the number of years
fishC <- fish
fishC[,"catch"] <- fishC[,"catch"]*propcom
answerC <- run_cMSY(fishC,glb,n=5000,sigpR=0.025)
ansC <- pulloutStats(answerC$R1)
#str(ans10)
round(ansC$output[,1:3],3)
round(ans$output[,1:3],3)
msy <- ansC$output["MSY","Mean"]/ans$output["MSY","Mean"]
depl <- ansC$output["CurrDepl","Mean"]/ans$output["CurrDepl","Mean"]
cat("reduced catch MSY/full catch MSY = ", msy, mean(propcom),"\n")
cat("Proportion of Current Depetion = ",depl,"\n")

## ----trendcatches, echo=TRUE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
propcom <- seq(0.8,0.5,length=31) # 31 equals the number of years
fishC <- fish
fishC[,"catch"] <- fishC[,"catch"]*propcom
answerC <- run_cMSY(fishC,glb,n=5000,sigpR=0.025)
ansC <- pulloutStats(answerC$R1)
round(ansC$output[,1:3],3)
round(ans$output[,1:3],3)
msy <- ansC$output["MSY","Mean"]/ans$output["MSY","Mean"]
depl <- ansC$output["CurrDepl","Mean"]/ans$output["CurrDepl","Mean"]
cat("reduced catch MSY/full catch MSY = ", msy, mean(propcom),"\n")
cat("Proportion of Current Depletion = ",depl,"\n")

## ----partialphaseplot, echo = TRUE, fig.width=7, fig.height=5---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
cMSYphaseplot(answerC,fishC)


## ----partialphaseplot2, echo = TRUE, fig.width=7, fig.height=5--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
cMSYphaseplot(answer,fish)


## ----"highharvest", echo=TRUE, fig.width=7,fig.height=5---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# library(datalowSA)
data(invert)
fish <- invert$fish
glb <- invert$glb 
# normally one would run 20000+ replicates, but for speed here we use 5000
answer <- run_cMSY(fish,glb,n=5000,sigpR=0.025,finaldepl=c(0.05,0.5),maximumH=1.0)
ans <- pulloutStats(answer$R1)
round(ans$output[,1:3],3)
out <- plottrajectory(answer$R1,fish$year,fish$catch,answer$parbound,
                      oneplot=TRUE,scalar=1.0,plotout=TRUE,plotall=7)

## ----"controlharvest", echo=TRUE, fig.width=7,fig.height=5------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# library(datalowSA)
# normally one would run 20000+ replicates, but for speed we use 5000
answer <- run_cMSY(fish,glb,n=5000,sigpR=0.025,finaldepl=c(0.05,0.5),maximumH=0.5)
ans <- pulloutStats(answer$R1)
round(ans$output[,1:3],3)
out <- plottrajectory(answer$R1,fish$year,fish$catch,answer$parbound,
                      oneplot=TRUE,scalar=1.0,plotout=TRUE,plotall=7)

## ----"controlharvest2", echo=TRUE, fig.width=7,fig.height=5-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# library(datalowSA)
# normally one would run 20000+ replicates, but for speed we use 5000
answer <- run_cMSY(fish,glb,n=5000,sigpR=0,finaldepl=c(0.05,0.5),maximumH=1.0)
ans <- pulloutStats(answer$R1)
round(ans$output[,1:3],3)
out <- plotconstC(ans$deplet,endyear=2017,constC=0)


## ----"plotthefishery",  echo=TRUE, fig.width=7,fig.height=5-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plotfish(fish,glb)


>>>>>>> 02950d79bbe34812ac9c4d09e13b418822ea40de
