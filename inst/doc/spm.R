## ----"setup", include=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
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


## ----"getdata", echo=TRUE, fig.width=7,fig.height=6-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# library(datalowSA)  # include the library before starting analysis
data(dataspm)
fish <- dataspm$fish
# check data.frame has all required columns; not usually required; use head(fish,10)
checkdata(dataspm) 


## ----"trylag", fig.width=7, fig.height=4------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ans <- getlag(fish,plotout=FALSE)
par(mfrow=c(1,1),mai=c(0.5,0.45,0.05,0.05),oma=c(0.0,0,0.0,0.0)) 
par(cex=1.0, mgp=c(1.35,0.35,0), font.axis=7,font=7,font.lab=7)  
plot(ans,lwd=3,col=2,main="",ylab="Cross-Correlation")
minim <- which.min(ans$acf)
text(0,-0.6,paste0("Optimum Lag = ",ans$lag[minim]),cex=1.1,font=7,pos=4)


## ----getdata2, echo=TRUE, fig.width=7,fig.height=6--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# display the dynamics for a set of guessed initial parameters
pars <- c(0.16,6700,3500)  # r, K, and Binit
negLL(pars,fish,simpspm)
ans <- displayModel(pars,fish,schaefer=TRUE,target=0.48,addrmse=TRUE)


## ----fitspm, echo = TRUE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
pars <- c(0.16,6700,3500) # r, K, and Binit, implies a depletion of ~0.52
cat("initial -ve log-likelihood ",negLL(pars,fish,simpspm),"\n") 
bestSP <- optim(par=pars,fn=negLL,callfun=simpspm,indat=fish,
                control=list(maxit = 1000, parscale = c(1,1000,1000)))
outoptim(bestSP) # outoptim just prints the results more compactly.
# read the help files to understand what each component means.
# Schaefer model dynamics is the default so no need for schaefer=TRUE

## ----plotspm, fig.width=7,fig.height=6--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ans <- displayModel(bestSP$par,fish,schaefer=TRUE,addrmse=TRUE)


## ----anscontents, echo=TRUE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
str(ans)


## ----fitFox, echo = TRUE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
pars <- c(0.16,6700,3500) # r, K, and Binit, implies a depletion of ~0.52
cat("initial -ve log-likelihood ",negLL(pars,fish,simpspm,schaefer=FALSE),"\n") 
parscl <- magnitude(pars)
bestSP <- optim(par=pars,fn=negLL,callfun=simpspm,indat=fish,schaefer=FALSE,
                control=list(maxit = 1000, parscale = parscl))
outoptim(bestSP   )
# Fox model dynamics is set by schaefer=FALSE

## ----plotfox, fig.width=7,fig.height=6--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ans <- displayModel(bestSP$par,fish,schaefer=FALSE,addrmse=TRUE)


## ----anscontentsFox, echo=TRUE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
str(ans)


## ----checkstartingpoint, echo=TRUE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  pars <- c(0.16,6700,3500) # the original starting point
  origpar <- pars
  N <-  20       # the number of random starting points to try
  scaler <- 10   # how variable; smaller = more variable
  # define a matrix for the results
  columns <- c("ir","iK","iB0","iLike","r","K","Binit","-veLL","MSY","Iters")
  results <- matrix(0,nrow=N,ncol=length(columns),dimnames=list(1:N,columns))
  pars <- cbind(rnorm(N,mean=origpar[1],sd=origpar[1]/scaler),
                rnorm(N,mean=origpar[2],sd=origpar[2]/scaler),
                rnorm(N,mean=origpar[3],sd=origpar[3]/scaler))
  # this randomness ignores the strong correlation between r and K
  for (i in 1:N) {
     bestSP <- fitSPM(pars[i,],fish,schaefer=TRUE)
     opar <- bestSP$par
     msy <- opar[1]*opar[2]/4
     origLL <- negLL(pars[i,],fish,simpspm)
     results[i,] <- c(pars[i,],origLL,bestSP$par,bestSP$value,msy,bestSP$counts[1])
  }
round(results[order(results[,"-veLL"]),],3)
round(apply(results,2,range),3)           # were the initial parameters diverse?
round(apply(results,2,median),3) # central tendency without outliers.


## ----calcoptimum, echo=TRUE, fig.width=7,fig.height=5-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
pars <- c(0.2424, 5173.5972, 2846.0953) # r, K, and Binit, median values
bestSP <- fitSPM(pars,fish,schaefer=TRUE)
ans <- displayModel(bestSP$par,fish,schaefer=TRUE,addrmse=TRUE)



## ----phaseplot, echo=TRUE, fig.width=7,fig.height=5.5,fig.align="center"----------------------------------------------------------------------------------------------------------------------------------------------------------------------
# plotprep(width=7,height=5.5) # to avoid using the RStudio plot window
spmphaseplot(ans,fnt=7)

## ----preparefit, echo = TRUE, fig.width=7,fig.height=5----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
data(dataspm)
fish <- dataspm$fish
colnames(fish) <- tolower(colnames(fish))
pars <- c(r=0.25,K=5500,Binit=2900)
ans <- fitSPM(pars,fish,schaefer=TRUE,maxiter=1000) #Schaefer version
answer <- displayModel(ans$par,fish,schaefer=TRUE,addrmse=TRUE)

## ----bootstrapfit, echo=TRUE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
reps <- 100       # this might take ~60 seconds, be patient
startime <- Sys.time()  # how long will this take
boots <- bootspm(ans$par,fishery=fish,iter=reps,schaefer=TRUE)
print(Sys.time() - startime)
str(boots)

## ----uncertainty, echo=TRUE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
dynam <- boots$dynam
bootpar <- boots$bootpar
rows <- colnames(bootpar)
columns <- c(c(0.025,0.05,0.5,0.95,0.975),"Mean")
bootCI <- matrix(NA,nrow=length(rows),ncol=length(columns),
                 dimnames=list(rows,columns))
for (i in 1:length(rows)) { # i=1
   tmp <- sort(bootpar[,i])
   qtil <-  quantile(tmp,probs=c(0.025,0.05,0.5,0.95,0.975),na.rm=TRUE)
   bootCI[i,] <- c(qtil,mean(tmp,na.rm=TRUE))
}
round(bootCI,4)


## ----plothists, echo=TRUE, fig.width=7,fig.height=6-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
par(mfrow=c(3,2),mai=c(0.45,0.45,0.15,0.05),oma=c(0.0,0,0.0,0.0)) 
par(cex=0.85, mgp=c(1.35,0.35,0), font.axis=7,font=7,font.lab=7)  
hist(bootpar[,"r"],breaks=25,col=2,main="",xlab="r")
abline(v=c(bootCI["r",c(2,3,4,6)]),col=c(3,3,3,4),lwd=c(1,2,1,2))
hist(bootpar[,"K"],breaks=25,col=2,main="",xlab="K")
abline(v=c(bootCI["K",c(2,3,4,6)]),col=c(3,3,3,4),lwd=c(1,2,1,2))
hist(bootpar[,"Binit"],breaks=25,col=2,main="",xlab="Binit")
abline(v=c(bootCI["Binit",c(2,3,4,6)]),col=c(3,3,3,4),lwd=c(1,2,1,2))
hist(bootpar[,"MSY"],breaks=25,col=2,main="",xlab="MSY")
abline(v=c(bootCI["MSY",c(2,3,4,6)]),col=c(3,3,3,4),lwd=c(1,2,1,2))
hist(bootpar[,"Depl"],breaks=25,col=2,main="",xlab="Final Depletion")
abline(v=c(bootCI["Depl",c(2,3,4,6)]),col=c(3,3,3,4),lwd=c(1,2,1,2))
hist(bootpar[,"Harv"],breaks=25,col=2,main="",xlab="Final Harvest Rate")
abline(v=c(bootCI["Harv",c(2,3,4,6)]),col=c(3,3,3,4),lwd=c(1,2,1,2))


## ----plotdynamics, echo=TRUE, fig.width=7,fig.height=4.5--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
years <- fish[,"year"]
par(mfrow=c(1,1),mai=c(0.45,0.45,0.05,0.05),oma=c(0.0,0,0.0,0.0)) 
par(cex=0.85, mgp=c(1.35,0.35,0), font.axis=7,font=7,font.lab=7)  
ymax <- getmaxy(c(dynam[,,"PredCE"],fish[,"cpue"]))
plot(fish[,"year"],fish[,"cpue"],type="n",ylim=c(0,ymax),xlab="Year",
     ylab="CPUE",panel.first = grid())
for (i in 1:reps) lines(years,dynam[i,,"PredCE"],lwd=1,col="grey")
lines(years,answer$Dynamics$outmat[,"PredCE"],lwd=2,col=2)
points(years,fish[,"cpue"],cex=1.1,pch=16,col=4)
percs <- apply(dynam[,,"PredCE"],2,quants)
arrows(x0=years,y0=percs["5%",],y1=percs["95%",],length=0.03,angle=90,code=3,col=2)


