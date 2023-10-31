
#' @title aspmLL negative log-likelihood for the ASPM
#'
#' @description aspmLL is the negative log-likelihood for normally distributed
#'     data, set-up for use with the ASPM model fitting. It would be necessary
#'     to log-transform the original data to convert log-normal data to normal.
#'     This includes a penalty function that is the SSQ of the catch vs 
#'     predicted catch, which should invariably be ~0 when the fully selected
#'     harvest rates are plausible; meaning are below 75% of exploitable biomass.
#'
#' @param par the dynamics relies on many parameters sitting in the global
#'     environment in particular ages, nages, maxage, M, maa, waa, sela, fish,
#'     and nyrs. 'pars' can contain either two or three parameters. 1) is
#'     the log-transformed average unfished recruitment, inR0. 2) is the 
#'     variability around the index of relative abundance (cpue) during the 
#'     fitting process, and if is present 3) is the initial depletion level 
#'     initdepl, which if present will be fitted as well.
#' @param infish the fish data.frame from readdata or built in dataset
#' @param inglb the glb data.frame from readdata or built in dataset
#' @param inprops the props data.frame from readdata or built in dataset
#'
#' @return a scalar that is the negative log-likelihood using normal
#'     random errors
#' @export
#'
#' @examples
#' \dontrun{
#' data(fishdat)
#' fish <- fishdat$fish
#' glb <- fishdat$glb
#' props <- fishdat$props
#' pars <- c(14,0.3)
#' dynamics(pars,fish,glb,props)
#' aspmLL(pars,fish,glb,props)      # should be 5.171
#' pars <- c(14.0,0.3,0.95) # logR0, sigCE, depletion
#' dynamics(pars,fish,glb,props)    # note the harvest rates of 85% exploitable biomass
#' aspmLL(pars,fish,glb,props)      # should be 114.9547
#' pars <- c(14,0.3)
#' bestL <- optim(pars,aspmLL,method="Nelder-Mead",infish=fish,inglb=glb,inprops=props,
#'                control=list(maxit = 1000, parscale = c(10,0.1)))
#' outoptim(bestL)
#' fishery <- dynamics(bestL$par,fish,glb,props)
#' print(round(fishery,4)) 
#' }
aspmLL <- function(par,infish,inglb,inprops) {  # par=pars;infish=fish; inprops=props; inglb=glb;
   fishery <- dynamics(par,infish,inglb,inprops)
   penalty <- sum((fishery[,"Catch"] - fishery[,"PredC"])^2,na.rm=TRUE)/1000.0
   pick <- which(fishery$CPUE > 0)
   LL <- -sum(dnorm(log(fishery[pick,"CPUE"]),log(fishery[pick,"PredCE"]),par[2],log=TRUE))
   return((LL + penalty))
} # end of aspmLL

#' @title aspmPENLL penalized negative log-likelihood for the ASPM
#'
#' @description aspmPENLL is a penalized negative log-likelihood for normally
#'     distributed data, set-up for use with the ASPM model fitting. It would
#'     be necessary to log-transform the original data to convert log-normal
#'     data to normal. The assumption is made that there will be three
#'     parameters, the unfished R0, the initial depletion in the first year. 
#'     For use with initially depleted stocks
#'
#' @param par a vector of two numbers the hypothesized inR0 and the initial 
#'     depletion in the first year.  (in that order).
#' @param infish the fish data.frame from readdata or built in dataset
#' @param inglb the glb data.frame from readdata or built in dataset
#' @param inprops the props data.frame from readdata or built in dataset
#'
#' @return a scalar that is the negative log-likelihood using normal
#'     random errors
#' @export
#'
#' @examples
#' \dontrun{
#' data(dataspm)
#' fish <- dataspm$fish
#' glb <- dataspm$glb
#' props <- dataspm$props
#' pars <- c(13.53,0.190667,0.95) # assume the stock is known to have been 
#' aspmLL(pars,infish=fish,inglb=glb,inprops=props)   # initially depleted
#' aspmPENLL(pars,infish=fish,inglb=glb,inprops=props) # penalty approaching 1 
#' pars <- c(13.53,0.190667,0.85) # depletion away from 1.0 has almost no effect 
#' aspmLL(pars,infish=fish,inglb=glb,inprops=props) 
#' aspmPENLL(pars,infish=fish,inglb=glb,inprops=props)
#' }
aspmPENLL <- function(par,infish,inglb,inprops) {
   fishery <- dynamics(par,infish,inglb,inprops)
   penalty <- sum((fishery[,"Catch"] - fishery[,"PredC"])^2,na.rm=TRUE)/1000.0
   pick <- which(fishery$CPUE > 0)
   LL <- -sum(dnorm(log(fishery[pick,"CPUE"]),log(fishery[pick,"PredCE"]),par[2],log=TRUE))
   LL <- LL + penalty + penalty1(par[3])
   return(LL)
} # end of aspmpenLL

#' @title aspmphaseplot - plots the phase plot of harvest rate vs biomass
#' 
#' @description aspmphaseplot uses the output from displayModel to plot up 
#'     the phase plot of harvest rate vs Biomass, marked with the limit and
#'     default targets. It identifies the start and end years (green and red
#'     dots) and permits the stock status to be determined visually. It also 
#'     plots out the catch time-series and harvest rate time-series to aid in
#'     interpretation of the phase plot.
#'
#' @param fishery the object output by the function dynamics, containing the 
#'     fishery dynamics (Year, Catch, PredC, SpawnB, ExploitB, FullH, CPUE,
#'     PredCE, and Deplete).
#' @param prod the matrix containing the production data from the function
#'     getProductionC
#' @param ans the vector of results from the function prodASPM
#' @param Blim the limit reference point, defaults to 0.2 so that 0.2B0 is used.
#' @param filename default is empty. If a filename is put here a .png file
#'     with that name will be put into the working directory. 
#' @param resol the resolution of the png file, defaults to 200 dpi
#' @param fnt the font used in the plot and axes. Default=7, bold Times. Using
#'     6 gives Times, 1 will give SansSerif, 2 = bold Sans
#'
#' @return an invisible list of B0, Bmsy, Hmsy, and Hlim.
#' @export
#'
#' @examples
#' \dontrun{
#'  # library(datalowSA)
#'   data(dataspm)
#'   fish <- dataspm$fish
#'   glb <- dataspm$glb
#'   props <- dataspm$props
#'   # Fit 3 par ASPM with penalty
#'   pars <- c(13.75,0.189667,0.6)
#'   bestL <- optim(pars,aspmPENLL,method="Nelder-Mead",
#'                  infish=fish,inglb=glb,inprops=props,
#'                  control=list(maxit = 1000, parscale = c(10,1,0.1)))
#'   outoptim(bestL)
#'   fisheryPen <- dynamics(bestL$par,infish=fish,inglb=glb,inprops=props)
#'   par <- bestL$par
#'   prod <- getProductionC(exp(par[1]),fish,glb,props,
#'                          Hrg=c(0.01,0.45,0.005),nyr=50)
#'   anspen <- prodASPM(prod,console=TRUE,plot=TRUE)
#'   plotprep(width=7,height=5.5)
#'   outs <- aspmphaseplot(fisheryPen,prod,anspen,Blim=0.2,fnt=7)
#'   str(outs)
#' }       
aspmphaseplot <- function(fishery,prod,ans,Blim=0.2,filename="",resol=200,
                          fnt=7) {
#   fishery=fishery; prod=prod;ans=ans;Blim=0.2;filename="";resol=200;fnt=7
   lenfile <- nchar(filename)
   if (lenfile > 3) {
      end <- substr(filename,(lenfile-3),lenfile)
      if (end != ".png") filename <- paste0(filename,".png")
      png(filename=filename,width=5.5,height=5.0,units="in",res=resol)
   } 
   B0 <- ans["B0"] 
   Bmsy <- ans["Bmsy"]
   Hmsy <- ans["Hmsy"]
   Btarg <- ans["Btarg"]
   Htarg=ans["Htarg"]
   pickL <- which.closest(Blim,prod[,"Depletion"])
   Hlim <- prod[pickL,"Harvest"]
   Hmax <- getmaxy(c(fishery[,"FullH"],Hlim))
   pickyr <- which(fishery[,"FullH"] > 0)
   numval <- dim(fishery)[1]
   par(mai=c(0.45,0.45,0.05,0.05),oma=c(0.0,0,0.0,0.0)) 
   par(cex=0.85, mgp=c(1.35,0.35,0), font.axis=fnt,font=fnt,font.lab=fnt)  
   layout(matrix(c(1,2)),heights=c(3,1))
   plot(fishery[,"SpawnB"],fishery[,"FullH"],type="l",lwd=2,col=1,xlab="Biomass",
        ylab="Annual Harvest Rate",ylim=c(0,Hmax),yaxs="i",xlim=c(0,B0))
   points(fishery[,"SpawnB"],fishery[,"FullH"],pch=16,cex=1.0,col=4)
   points(fishery[2,"SpawnB"],fishery[2,"FullH"],pch=16,cex=1.5,col=3)
   points(fishery[numval,"SpawnB"],fishery[numval,"FullH"],pch=16,cex=1.5,col=2)
   abline(v=c(Blim*B0,Btarg,B0),col=c(2,3,3),lty=2)
   abline(h=c(Htarg,Hlim),col=c(3,2),lty=2)
   text(Btarg,0.05*Hmax,"Btarg",cex=1.0,font=fnt,pos=4)
   text(Blim*B0,0.05*Hmax,"Blim",cex=1.0,font=fnt,pos=4)
   text(0,0.95*Htarg,"Htarg",cex=1.0,font=fnt,pos=4)
   text(0,0.95*Hlim,"Hlim",cex=1.0,font=fnt,pos=4)
   yrs <- fishery[pickyr,"Year"]
   catch <- fishery[pickyr,"Catch"]
   harvest <- fishery[pickyr,"FullH"]
   par(mai=c(0.3,0.45,0.05,0.45)) 
   cmax <- getmaxy(catch)
   plot(yrs,catch,type="l",lwd=2,col=2,ylab="",xlab="",
        ylim=c(0,cmax),yaxs="i",panel.first=grid(ny=0))
   par(new=TRUE)
   plot(yrs,harvest,type="l",lwd=2,col=4,ylim=c(0,Hmax),yaxt="n",ylab="",
        yaxs="i",xlab="")
   points(yrs[1],harvest[1],pch=16,cex=1.5,col=3)
   points(yrs[numval],harvest[numval],pch=16,cex=1.5,col=2)
   abline(h=c(Htarg),col=c(4),lty=2)
   ym2 <- round(Hmax,2)
   axis(side=4,at=seq(0,ym2,length=3),labels = seq(0,ym2,length=3))
   mtext("Catch (t)",side=2,outer=F,line=1.2,font=fnt,cex=1.0,col=2) 
   mtext("Harvest Rate",side=4,outer=F,line=1.1,font=fnt,cex=1.0,col=4) 
   if (lenfile > 0) {
      outfile <- paste0(getwd(),"/",filename)
      print(outfile)
      dev.off()
   }
   result <- list(B0=B0,Btarg=Btarg,Bmsy=Bmsy,Hmsy=Hmsy,Htarg=Htarg,Hlim=Hlim)
   return(invisible(result))
} # end of aspmphaseplot

#' @title aspmSSQ sum-of-squared residuals for the ASPM.
#'
#' @description aspmLL calculates the sum-of-squared residuals, set-up for use
#'     with the ASPM model fitting. It requires the use of functions from
#'     datalowSA for fitting of ASPM (see example below). It first identifies
#'     which of the CPUE column in 'fishery' are numeric (not NA) and then
#'     calculates the sum-of-squared residuals after log-transforming both
#'     the CPUE and PredCE columns.
#'
#' @param par a single number that is the hypothesized inR0, which is the only
#'     parameter.
#' @param infish the fish data.frame from readdata or built in dataset
#' @param inglb the glb data.frame from readdata or built in dataset
#' @param inprops the props data.frame from readdata or built in dataset
#'
#' @return a single value that is the sum-of-squared residuals.
#' @export
#'
#' @examples
#' \dontrun{
#' data(fishdat)
#' fish <- fishdat$fish
#' glb <- fishdat$glb
#' props <- fishdat$props
#' pars <- c(14)
#' aspmSSQ(pars,infish=fish,inglb=glb,inprops=props)
#' pars <- c(13.75)  # note the presence of the maximum harvest rate of 0.85
#' dynamics(pars,infish=fish,inglb=glb,inprops=props) # leads to a large increase 
#' aspmSSQ(pars,infish=fish,inglb=glb,inprops=props) #because of penalty incurred 
#' }
aspmSSQ <- function(par,infish,inglb,inprops) {
   fishery <- dynamics(par,infish,inglb,inprops)
   penalty <- sum((fishery[,"Catch"] - fishery[,"PredC"])^2,na.rm=TRUE)/1000.0
   pick <- which(fishery$CPUE > 0)
   ssq <- sum((log(fishery[pick,"CPUE"]) - log(fishery[pick,"PredCE"]))^2)
   return((ssq + penalty))
} # end of aspmSSQ



#' @title bh calculates the expected Beverton-Holt recruitment
#'
#' @description bh calculate the expected Beverton-Holt stock recruitment level
#'     from the available spawning biomass, the steepness, R0 and B0. This would
#'     be used when fitting a model to data. 
#'
#' @param spb the current spawning or mature biomass
#' @param h the steepness of the Beverton-Holt stock recruitment curve
#' @param R0 the unfished average recruitment level
#' @param B0 the unfished spawning biomass.
#'
#' @return the expected Beverton-Holt recruitment level, a real number in the 
#'     linear scale
#' @export
#'
#' @examples
#' rec <- bh(10000,0.75,1500000,30000)
#' print(rec)   # should be 1285714
#' bh(spb=30000,h=0.75,R0=1500000,B0=30000)  # should be 1500000
bh <- function(spb,h,R0,B0) {
   a <- (4 * h * R0)/(5 * h - 1)
   b <- (B0 * (1 - h))/(5 * h -1)
   return((a * spb)/(b + spb))
} # end of bh

#' @title bootASPM boostraps on the results of fitting an ASPM
#' 
#' @description bootASPM uses the optimum parameters from fitting an ASPM to 
#'     a set of data in a bootstrap process so as to provide 'iter' bootstrap
#'     replicate predicted CPUE trajectories. These can be used to generate 
#'     estimates of the uncertainty around the predicted outcomes from the ASPM.
#'     The very first row of the results is the original predicted CPUE. The 
#'     bootstraps are of the residuds between the observed and the predicted.
#'     The new bootstrap indices in each case are the combination of the 
#'     original cpue series times the boostrapped residuals. 1000 bootstraps
#'     can take about 2 minutes.
#'
#' @param infish the fish object from the fisheries data set
#' @param inglb the globals 'glb' object from the fisheries data set
#' @param inprops the properties 'props' object from the fisheries data set. 
#' @param optpar the optimum parameters obtained from fitting the ASPM 
#' @param iter how many bootstrap replicates are required? Defaults to 10 so
#'     that the bootstraps can be tested before running for, say, 1000 runs.
#' @param callfun the function that is called to fit the model. Could be aspmLL,
#'     which is the default, or aspmPENLL, for three parameter models, or even
#'     aspmSSQ.
#'
#' @return a matrix of bootstrap replicate predicted CPUE vectors 
#' @export
#'
#' @examples
#' \dontrun{
#' data(fishdat)
#' fish <- fishdat$fish
#' glb <- fishdat$glb
#' props <- fishdat$props
#' pars <- c(14,0.3)
#' bestL <- optim(pars,aspmLL,method="Nelder-Mead",infish=fish,inglb=glb,
#'                inprops=props,control=list(maxit = 1000, parscale = c(10,0.1)))
#' answer <- bootASPM(fish,glb,props,bestL$par,iter=10)
#' str(answer,max.level=1)
#' round(answer$result[,,"PredCE"],4) 
#' }  # infish=fish; inglb=glb; inprops=props; optpar=bestL$par;iter=10; callfun=aspmLL
bootASPM <- function(infish,inglb,inprops,optpar,iter=10,callfun=aspmLL) { 
   fish <- infish
   fisheryO <- dynamics(optpar,fish,inglb,inprops)
   pick <- which(fisheryO[,"CPUE"] > 0)
   predCE <- fisheryO[pick,"PredCE"]
   resids <- fisheryO[pick,"CPUE"]/predCE
   yrs <- fisheryO[,"Year"]
   varib <- c("SpawnB","FullH","CPUE","PredCE","Deplete")
   result <- array(0,dim=c(iter,length(fisheryO[,"Year"]),length(varib)),
                   dimnames=list(1:iter,yrs,varib))
   Bzero <- numeric(iter)
   pickcol <- c(4,6,7,8,9)
   for (vout in 1:5) result[1,,vout] <- fisheryO[,pickcol[vout]]
   if (length(optpar) < 3) {
      Bzero[1] <- fisheryO[1,"SpawnB"]
      parname <- c("R0","sigCE")
   } else {
      Bzero[1] <- getB0(exp(optpar[1]),inglb,inprops)
      parname <- c("R0","sigCE","Depl")
   }
   param <- matrix(0,nrow=iter,ncol=length(optpar),dimnames=list(1:iter,parname))
   param[1,] <- optpar
   pickF <- which(fish[,"cpue"] > 0)
   for (i in 2:iter) { # i = 2
      boot <- sample(resids,replace=TRUE)
      fish[pickF,"cpue"] <- predCE * boot  # replace the original observed cpue
      bestB <- fitASPM(optpar,fish,inglb,inprops,callfun=callfun)
      fisheryB <- dynamics(bestB$par,fish,inglb,inprops)
      for (vout in 1:5) result[i,,vout] <- fisheryB[,pickcol[vout]]
      if (length(optpar) < 3) {
         Bzero[i] <- fisheryB[1,"SpawnB"]
      } else {
         Bzero[i] <- getB0(exp(bestB$par[1]),inglb,inprops)
      }
      param[i,] <- bestB$par
      
      if ((i/20) - (i %/% 20) == 0) cat(i," ")
   }
   answer <- list(result=result,B0=Bzero,param=param)
   return(answer)
}

#' @title doDepletion - depletes the stock to the declared initdep level
#'
#' @description doDepletion - depletes the stock to the declared initdep level
#'     There is a printout to the screen of the final outcome. The procedure
#'     assumes you have the unfished NaA in column 1 of the stock$NaA object.
#'     The depletion is arrived at by literally searching for the first
#'     constant harvest rate that leads to the required depletion.
#'
#' @param inR0 the estimated log transformed unfished recruitment
#' @param indepl the initial depletion level to be searched for
#' @param inprops the props object from the data object
#' @param inglb the globals object from the data object
#' @param inc the starting value and step in harvest rate used when finding the 
#'     selected depletion level; defaults to 0.02
#' @param Numyrs the number of years of fishing while searching for the
#'     desired depletion. Some number between 40 - 50 seems to work well.
#'     
#' @return A list containing the vector of numbers-at-age at the required 
#'     depletion level, the Fvalue required to achieve that depletion, and
#'     the actual depletion level achieved
#' @export
#' @examples
#' \dontrun{
#' data(dataspm)
#' fish <- dataspm$fish
#' glb <- dataspm$glb
#' props <- dataspm$props
#' dep <- doDepletion(glb$R0,indepl=0.4,props,glb,inc=0.02)
#' print(dep)
#' }  
doDepletion <- function(inR0,indepl,inprops,inglb,inc=0.02,Numyrs=50) {
   maxage <- inglb$maxage
   Nages <- inglb$nages
   M <- inglb$M
   steep <- inglb$steep
   maa <- inprops$maa
   waa <- inprops$waa
   sela <- inprops$sela
   Nt <- matrix(0,nrow=(maxage+1),ncol=Numyrs,
                dimnames=list(seq(0,maxage,1),seq(0,(Numyrs-1),1)))
   Frange <- seq(0.02,2*M,inc)
   NF <- length(Frange)
   unfish <- unfished(inglb,inprops,inR0)
   B0 <- unfish$B0
   naa <- unfish$N0
   for (hr in 1:NF) {  # hr=1
      dHarv <- Frange[hr]  # apply continually increasing H
      # Fy <- -log(1-dHarv)
      SpawnB <- B0
      Nt[,1] <- naa
      R0 <- exp(inR0)
      year <- 2     # instead of zero to allow for indexing matrices
      repeat {
         Nt[1,year] <- bh(SpawnB,steep,R0,B0)  #  Age 0
         Nt[2:(Nages-1),year] <- Nt[1:(Nages-2),(year-1)] * exp(-M/2)
         Nt[Nages,year] <- (Nt[(Nages-1),(year-1)]*exp(-M/2)) + (Nt[(Nages),(year-1)]*exp(-M/2))
         # Now do the fishing
         Nt[,year] <- (Nt[,year] * (1-(sela * dHarv))) * exp(-M/2)
         SpawnB <- SpB(Nt[,year],maa,waa)
         depletion <- SpawnB/B0
         year <- year + 1
         if ((depletion <= indepl) | (year > Numyrs)) break
      }
      if (depletion <= indepl) break
   }
   ans <- list(Ndepl=Nt[,(year-1)],Fval=Frange[hr],depl=depletion,SpawnB=SpawnB)
   return(ans)
} # End of DoDepletion



#' @title dynamics describe the ASPM dynamics
#'
#' @description dynamics summarizes the dynamics of the Age-Structured
#'     Production Model (ASPM). Fitting the ASPM entails estimating the unfished
#'     recruitment level (R0), which is input as a parameter. 
#'
#' @param pars the dynamics relies on many parameters sitting in the global
#'     environment in particular ages, nages, maxage, M, maa, waa, sela, fish,
#'     and nyrs. 'pars' can contain either two or three parameters. 1) is
#'     the log-transformed average unfished recruitment, inR0. 2) is the 
#'     variability around the index of relative abundance (cpue) during the 
#'     fitting process, and if is present 3) is the initial depletion level 
#'     initdepl, which if present will be fitted as well.
#' @param infish the fish data.frame from readdata or built in dataset
#' @param inglb the glb data.frame from readdata or built in dataset
#' @param inprops the props data.frame from readdata or built in dataset
#' 
#' @return a data.frame containing the fishery dynamics according to the input
#'     parameter inR0. In particular it includes teh Catch and PredC, and the
#'     CPUE and PredCE, which can be used in a maximum likelihood context.
#' @export
#'
#' @examples
#' \dontrun{
#' data(dataspm)
#' fish <- dataspm$fish
#' glb <- dataspm$glb
#' props <- dataspm$props
#' par <- c(glb$R0,0.20)  # not fitted to the data, this is just an initial guess
#' fishery <- dynamics(par,infish=fish,inglb=glb,inprops=props)
#' print(fishery)
#' }
dynamics <- function(pars,infish,inglb,inprops) {  
  # pars=pars;infish=fish;inglb=glb;inprops=props
   waa <- inprops$waa
   maa <- inprops$maa
   sela <- inprops$sela
   R0 <- exp(pars[1])
   B0 <- getB0(R0,inglb,inprops)   
   if (length(pars) == 3) {
      dep <- doDepletion(pars[1],indepl=pars[3],inprops,inglb,inc=0.02)
      spb <- SpB(dep$Ndepl,maa,waa)
      Rinit <- bh(spb,inglb$steep,R0,B0)
   } else {
      Rinit <- R0
   }
   nyrs <- length(infish[,"year"])
   nages <- inglb$nages
   maxage <- inglb$maxage
   Nt <- matrix(0,nrow=nages,ncol=(nyrs+1),dimnames=list(inglb$ages,0:nyrs))
   columns <- c("Year","Catch","PredC","SpawnB","ExploitB","FullH","CPUE",
                "PredCE","Deplete")
   fishery <- matrix(NA,nrow=(nyrs+1),ncol=length(columns),
                     dimnames=list(0:nyrs,columns))
   fishery[,"Year"] <- c((infish$year[1]-1),infish$year)
   fishery[,"Catch"] <- c(NA,infish$catch)
   fishery[,"CPUE"] <- c(NA,infish$cpue)
   hS <- exp(-inglb$M/2)
   surv <- exp(-inglb$M)
   # now calculate unfished numbers-at-age given inR0
   if (length(pars) == 3) {
      Nt[,1] <- dep$Ndepl
   } else {
      Nt[,1] <- Rinit
      for (age in 1:(maxage-1)) Nt[age+1,1] <- Nt[age,1] * surv
      Nt[maxage+1,1] <- (Nt[maxage,1] * surv)/(1-surv)
   }
   for (yr in 2:(nyrs+1)) {  # yr=2
      spb <- SpB(Nt[,(yr-1)],maa,waa)
      exb <- ExB(Nt[,(yr-1)]*hS,sela,waa)
      Nt[1,yr] <- bh(spb,inglb$steep,R0,B0)
      harvest <- min((fishery[yr,"Catch"]/exb),0.85)
      hrate <- sela * harvest
      Ct <- (Nt[,(yr-1)] * hS) * hrate
      Nt[2:nages,yr] <- ((Nt[1:(nages-1),(yr-1)] * hS) - Ct[1:(nages-1)]) * hS
      Nt[nages,yr] <- Nt[nages,yr] + ((Nt[nages,yr-1] * hS) - Ct[nages]) * hS
      fishery[(yr-1),4:5] <- c(spb,exb)
      fishery[yr,c(3,6)] <- c(sum(Ct * waa)/1000,hrate[nages])
   }
   spb <- SpB(Nt[,yr],maa,waa)   # to complete final year
   exb <- ExB(Nt[,yr]*hS,sela,waa)
   fishery[yr,4:5] <- c(spb,exb)
   fishery[,"Deplete"] <- fishery[,"SpawnB"]/B0
   ExpB <- fishery[1:nyrs,"ExploitB"]
   pick <- which(infish$cpue > 0)
   avq <- exp(mean(log(infish$cpue[pick]/fishery[pick,"ExploitB"]),na.rm=TRUE))
   fishery[2:(nyrs+1),"PredCE"] <- ExpB * avq
   return(as.data.frame(fishery))
} # end of dynamics

#' @title ExB calculate exploitable biomass from numbers-at-age
#'
#' @description ExB calculates the spawning biomass from a vector of
#'     numbers-at-age, selectivity-at-age, and Weight-at-age.
#'
#' @param invect the numbers-at-age as a vector
#' @param SelA selectivity-at-age vector
#' @param WeightA weight-at-age vector as kilograms
#' 
#' @return ExB a scalar as tonnes.
#' @export
#' @examples
#' \dontrun{
#' data(fishdat)
#' str(fishdat)
#' glb <- fishdat$glb
#' fish <- fishdat$fish
#' props <- fishdat$props
#' unfish <- unfished(glb,props,glb$R0)
#' N1 <- unfish$N0 * exp(-glb$M/2)  # no growth
#' ExB(N1,props$sela,props$waa)  # should be 17999.6
#' }
ExB <- function(invect, SelA, WeightA) {
  ans <- sum(SelA * WeightA * invect)/1000.0
  return(ans)
}


#' @title fitASPM fits an age-structured production model
#'
#' @description fitASPM fits an age-structured produciton model that only has
#'     a single parameter - the unfished recruitment level R0.
#'
#' @param initpar a vector of 2 or 3 numbers that are the initial parameter
#'     values given to the estimate of logR0, and the estimate of the variation
#'     around the CPUE data that the model is to be fitted to, and finally, the
#'     initial depletion.
#' @param infish the fish data.frame from readdata or built in dataset
#' @param inglb the glb data.frame from readdata or built in dataset
#' @param inprops the props data.frame from readdata or built in dataset
#' @param callfun the negative log-likelihoood used; either aspmLL or aspmPENLL
#' 
#' @return a list containing the optim output
#' @export
#'
#' @examples
#' \dontrun{
#' data(dataspm)
#' fish <- dataspm$fish
#' glb <- dataspm$glb
#' props <- dataspm$props
#' pars <- c(14,0.3)
#' aspmLL(pars,fish,glb,props)      # should be -2.277029
#' bestspm <- fitASPM(pars,infish=fish,inglb=glb,inprops=props)
#' bestspm
#' fishery <- dynamics(bestspm$par,fish,glb,props)
#' round(fishery,4)
#' }
fitASPM <- function(initpar,infish,inglb,inprops,callfun=aspmLL) { 
   paramscale = magnitude(initpar)
   bestL <- optim(initpar,callfun,method="Nelder-Mead",infish=infish,inglb=inglb,
                  inprops=inprops,control=list(maxit = 1000, parscale = paramscale))
   paramscale = magnitude(bestL$par) 
   bestL <- optim(bestL$par,callfun,method="Nelder-Mead",infish=infish,inglb=inglb,
                  inprops=inprops,control=list(maxit = 1000, parscale = paramscale))   
   return(bestL)
}

#' @title getB0 calculates the B0 from biological properties and R0
#'
#' @description getB0 calculates the B0 from biological properties of M,
#'     maxage, maa, and waa, plus the input of R0 on the nominal scale (the 
#'     hypothetical unfished recruitment level). This is used in the 'dynamics'
#'     function.
#'
#' @param inR0 the estimate of unfished recruitment
#' @param inglb the glb data.frame from readdata or built in dataset
#' @param inprops the props data.frame from readdata or built in dataset
#' 
#' @return a single number that is the estimate of B0
#' @export
#'
#' @examples
#' \dontrun{
#' data(fishdat)
#' glb <- fishdat$glb
#' props <- fishdat$props
#' getB0(1275000,glb,props) # shoud give 19429.76
#' getB0(1000000,glb,props) # should give 15239.03
#' }
getB0 <- function(inR0,inglb,inprops) { # inR0 = exp(par["R0"]); inglb=glb; inprops=props
   maxage <- inglb$maxage
   surv <- exp(-inglb$M)
   Nt <- numeric(inglb$nages)
   Nt[1] <- 1  # calculate numbers-at-age per recruit
   for (age in 1:(maxage-1)) Nt[age+1] <- Nt[age] * surv
   Nt[maxage+1] <- (Nt[maxage] * surv)/(1-surv)
   A0 <-  sum(inprops$maa * inprops$waa * Nt)/1000.0
   B0 <- inR0 * A0
   return(B0)
}  # end of getB0



#' @title getProduction estimates MSY from output of ASPM
#'
#' @description getProduction takes the optimum estimate of R0 from ASPM and
#'     estimates the production curve, from which it gains the MSY, Bmsy, Hmsy,
#'     and Dmsy (depletion at MSY). It generate the production curve by stepping
#'     through the dynamics of the fishery over a series of constant harvest
#'     rates for however many iterations of nyr required.
#'
#' @param inR0 the optimum estimate of R0 from ASPM on nominal scale
#' @param infish the fish data.frame from readdata or built in dataset
#' @param inglb the glb data.frame from readdata or built in dataset
#' @param inprops the props data.frame from readdata or built in dataset#'
#' @param Hrg the desired sequence of harvest rates to be used to define the
#'     production curve. The default is c(0.025,0.4,0.025), but it is a good
#'     idea to plot teh harvest rate against yield and manually adjust the
#'     sequence values to focus attention and detail on where the MSY falls.
#' @param nyr The number of years making up a single iteration while searching
#'     for the equilibrium yield. default = 50.
#' @param maxiter defaults to 3. Determines how many runs through the nyr
#'     steps are conducted to reach equilibrium. One can trial different values
#'     but 3 is a reasonable compromise. It is best to test different maxiter
#'     values to ensure equilibria are reached in each yield estimate.
#'
#' @return a data.frame containing the sequence of harvest rates, the related
#'     spawning biomass, the exploitable biomass, the yield, and depletion.
#' @export
#'
#' @examples
#' \dontrun{
#' data(fishdat)
#' fish <- fishdat$fish
#' glb <- fishdat$glb
#' props <- fishdat$props
#' pars <- c(14,0.3)
#' bestL <- optim(pars,aspmLL,method="Nelder-Mead",infish=fish,inglb=glb,
#'                inprops=props,control=list(maxit=1000,parscale=c(10,0.1)))
#' prod <- getProduction(exp(bestL$par[1]),infish=fish,inglb=glb,inprops=props,
#'                       Hrg=c(0.0005,0.07,0.0005),nyr=50)
#' head(prod,20)
#' tail(prod,20)
#' }
getProduction <- function(inR0,infish,inglb,inprops,
                          Hrg=c(0.025,0.4,0.025),nyr=50,maxiter=3) {
   maxage <- inglb$maxage; M <- inglb$M;  steep <- inglb$steep
   nages <- inglb$nages; maa <- inprops$maa; waa <- inprops$waa
   sela <- inprops$sela
   surv <- exp(-M)
   hS <- exp(-M/2)
   B0 <- getB0(inR0,inglb,inprops)   
   getyield <- function(NAA,hrate) {
      for (iter in 1:maxiter) {  # iterate until equilibrium yield reached
         for (yr in 2:nyr) {  # yr=nyrs+1
            spb <- SpB(NAA[,(yr-1)],maa,waa)
            NAA[1,yr] <- bh(spb,steep,inR0,B0)
            Ct <- (NAA[,(yr-1)] * hS) * (hrate * sela)
            NAA[2:nages,yr] <- ((NAA[1:(nages-1),(yr-1)] * hS) - Ct[1:(nages-1)]) * hS
            NAA[nages,yr] <- NAA[nages,yr] + ((NAA[nages,yr-1] * hS) - Ct[nages]) * hS
         }
         newcatch <- sum(Ct * waa)/1000
         NAA[,1] <- NAA[,nyr]
      }
      spb <- SpB(NAA[,nyr],maa,waa)
      exb <- ExB(NAA[,nyr]*hS,sela,waa)
      return(c(spb=spb,exb=exb,yield=newcatch))
   } # end of getyield
   Nt <- matrix(0,nrow=(maxage+1),ncol=nyr,dimnames=list(0:maxage,1:nyr))
   hrange <- seq(Hrg[1],Hrg[2],Hrg[3])
   nH <- length(hrange)
   columns <- c("Harvest","SpawnB","ExploitB","Yield","Depletion")
   production <- matrix(NA,nrow=(nH+1),ncol=length(columns),
                        dimnames=list(c(0,hrange),columns))
   production[,"Harvest"] <- c(0,hrange)
   Nt[1,1] <- inR0
   for (age in 1:(maxage-1)) Nt[age+1,1] <- Nt[age,1] * surv
   Nt[maxage+1,1] <- (Nt[maxage,1] * surv)/(1-surv)
   production[1,"SpawnB"] <- SpB(Nt,maa,waa)
   production[1,"ExploitB"] <- ExB(Nt*hS,sela,waa)
   for (hnum in 1:nH)
      production[(hnum+1),2:4] <- getyield(Nt,hrange[hnum])
   production[,"Depletion"] <- production[,"SpawnB"]/production[1,"SpawnB"]
   return(as.data.frame(production))
} # end of getProduction



#' @title logist Logistic selectivity function
#'
#' @description logist calcualtes a Logistic curve that can be used as a
#'     selectivity function, or maturity curve, of wherever a logistic is
#'     required. This version uses the logistic function
#'     1/(1+exp(-log(19.0)*(lens-inL50)/(inL95-inL50))),
#'     which explicitly defines the SM50 and uses SM95 as the second parameter.
#' @param inL50 is the length at 50 percent selection/maturity/whatever
#' @param delta is the difference in selection/maturity/whatever between
#'     inL50 and inL95
#' @param depend a vector of lengths/ages for which the logistic value will be
#'     calculated.
#' @param knifeedge defaults to 0. If knifeedge is set to a particular length or
#'     age then the logistic value <= the value of knifeedge is set to
#'     zero, which is essentially knife-edge. Allows for knife-edge selectivity
#' @return A vector of length(depend) containing the predicted logistic values
#' @export
#' @examples
#' \dontrun{
#' in50 <- 100.0
#' deltaS <- 8.0
#' lens <- seq(2,210,2)
#' select <- logist(inL50=in50,delta=deltaS,depend=lens)
#' selectk <- logist(in50,deltaS,lens,knifeedge=105)
#' round(cbind(lens[35:70],select[35:70],selectk[35:70]),5)
#' } 
logist <- function(inL50,delta,depend,knifeedge=0) {
   ans <- 1/(1+exp(-log(19.0)*(depend-inL50)/(delta)))
   if (knifeedge > 0) {
      pick <- which(depend <= knifeedge)
      if (length(pick) > 0) ans[pick] <- 0.0
   }
   return(ans)
}

#' @title MaA an alternative logistic function commonly used for maturity
#'
#' @description MaA - the logistic function exp(a+bxdepend)/(1+exp(a+bxdepend)),
#'     which can also be expressed as 1/(1+(1/exp(a + b x depend))). This has
#'     the property that the SM50 = -a/b and the interquartile distance is
#'     2.Ln(3)/b.
#' @param ina is the intercept of the exponential function
#' @param inb is the gradient of the exponential function
#' @param depend is a vector of lengths/ages for which the logistic maturity
#'     value will be calculated
#' @return A vector of length(depend) containing the predicted maturity values
#'
#' @export
#' @examples
#' a <- -14.383
#' b <- 0.146017
#' lens <- seq(2,210,2)
#' round(MaA(a,b,depend=lens),5) # length based
#' round(MaA(-2.5,0.95,0:25),5)   # age based
MaA <- function(ina,inb,depend) {
   ans <- exp(ina+inb*depend)/(1+exp(ina+inb*depend))
   return(ans)
}

#' @title plotASPM plots catch, CPUE, Spawning Biomass and Harvest Rate
#' 
#' @description plotASPM after running fitASPM the optimum parameters can be 
#'     put through the dynamics function to generate a dataframe containing
#'     the optimum dynamics. These can be plotted using plotASPM, which plots
#'     out the catches, the Spawning Biomass, the relative CPUE and its fit to
#'     the observed CPUE, and the harvest rate. This routine is still under 
#'     development to include more options.
#'
#' @param infish an object generated by the dynamics function
#' @param CI defaults to NA, if confidence intervals around the cpue have been
#'     obtained using getLNCI, then the resulting matrix will generate 95pc CIs 
#' @param defineplot define the plot size and character outside the plot or
#'     automatically inside. Defaults to TRUE
#' @param target target depletion level. Defaults to 0.48
#' @param usef defines the font to use usef(ont),default = 7 bold times
#' @param png save a png file with the name in 'png', default = "", which
#'     means no file produced
#'
#' @return Nothing, but it does plot six graphs in a single plot.
#' @export
#'
#' @examples
#' \dontrun{
#' data(dataspm)
#' fish <- dataspm$fish
#' glb <- dataspm$glb
#' props <- dataspm$props
#' pars <- c(14,0.3)
#' aspmLL(pars,fish,glb,props)      # should be -2.277029
#' bestspm <- fitASPM(pars,infish=fish,inglb=glb,inprops=props)
#' fishery <- dynamics(bestspm$par,fish,glb,props)
#' plotASPM(fishery,defineplot=TRUE)
#' ceCI <- getLNCI(fishery[,"PredCE"],bestspm$par[2])
#' plotASPM(fishery,CI=ceCI)
#' }  # infish=fishery; CI=NA; defineplot=TRUE; target=0.48; usef=7;png="test.png"
plotASPM <- function(infish,CI=NA,defineplot=TRUE, target=0.48,usef=7,png="") { 
   if (nchar(png) > 0) defineplot=FALSE
   if (defineplot) { 
      if (names(dev.cur()) %in% c("null device", "RStudioGD"))
         dev.new(width = 7, height = 5.5, noRStudioGD = TRUE)
   }
   par(mfrow=c(3,2),mai=c(0.25,0.4,0.1,0.05),oma=c(0.0,0,0.0,0.0),tck=-0.02) 
   par(cex=0.85,mgp=c(1.35,0.35,0),font.axis=usef,font=usef,font.lab=usef)  
   if (nchar(png) > 0) {
      dev.off()
      graphics.off()
      graphfile <- png
      if (file.exists(graphfile)) file.remove(graphfile)
      png(filename=graphfile,width=210,height=160,units="mm",res=200) 
      par(mfrow=c(3,2),mai=c(0.25,0.4,0.1,0.05),oma=c(0.0,0,0.0,0.0),tck=-0.02) 
      par(cex=0.85,mgp=c(1.35,0.35,0),font.axis=usef,font=usef,font.lab=usef)
   }
   yrs <- infish$Year
   # plot catches
   ymax <- getmaxy(infish$Catch)
   plot(yrs,infish$Catch,type="l",lwd=2,ylim=c(0,ymax),yaxs="i",xlab="",
        panel.first=grid(),ylab="Catch (t)")
   # plot Spawning Biomass
   ymax <- getmaxy(infish$SpawnB)
   plot(yrs,infish$SpawnB,type="l",lwd=2,ylim=c(0,ymax),yaxs="i",xlab="",
        panel.first=grid(),ylab="Spawning Biomass (t)")
   # plot CPUE
   ymax <- getmaxy(c(infish$CPUE,infish$PredCE))
   plot(yrs,infish$CPUE,type="p",pch=16,col=2,cex=1.0,ylim=c(0,ymax),yaxs="i",
        xlab="",panel.first=grid(),ylab="Relative CPUE")
   lines(yrs,infish$PredCE,lwd=2,col=1)
   if ("matrix" %in% class(CI)) {
      segments(x0=yrs,y0=CI[,1],x1=yrs,y1=CI[,3],lwd=1,col=4)
   }
   # plot harvest rate
   ymax <- getmaxy(infish$FullH)
   plot(yrs,infish$FullH,type="l",lwd=2,ylim=c(0,ymax),yaxs="i",xlab="",
        panel.first=grid(),ylab="Annual Harvest Rate")
   # plot the residuals
   pickCE <- which(infish[,"CPUE"] > 0)
   resid <- infish[pickCE,"CPUE"]/infish[pickCE,"PredCE"]
   nresid <- length(resid)
   ymax <- getmaxy(resid);    ymin <- getminy(resid,mult=1.1)
   plot(yrs[pickCE],resid,"n",ylim=c(ymin,ymax),ylab="LogN Residuals",xlab="")
   grid()
   abline(h=1.0,col=1)
   segments(x0=yrs[pickCE],y0=rep(1.0,nresid),x1=yrs[pickCE],y1=resid,lwd=2,col=2)
   points(yrs[pickCE],resid,pch=16,col=1,cex=1.0)
   rmseresid <- sqrt(sum(resid^2)/nresid)
   text(min(yrs[pickCE]),ymin*1.05,paste("rmse = ",round(rmseresid,3),sep=""),
        font=7,cex=1.0,pos=4)
   # plot the depletion level
   ymax <- getmaxy(infish$Deplete)
   plot(yrs,infish$Deplete,type="l",lwd=2,ylim=c(0,ymax),yaxs="i",xlab="",
        panel.first=grid(),ylab="Depletion")
   abline(h=c(0.2,target),col=c(2,3),lwd=1)
   if (nchar(png) > 0) dev.off()
} # end of plotASPM

#' @title plotceASPM plots just the fit of the ASPM model to the CPUE data
#' 
#' @description plotceASPM plots just the fit of the ASPM model to the CPUE data
#'     and provides a more detailed visual than plotASPM. It has the option of 
#'     including lognormal confidence intervals around the predcted CPUE.
#'
#' @param infish output from the dynamics function using the optimum parameters
#' @param CI the output matrix from getLNCI function. Needs to have the lower CI
#'    in column 1 and the upper CI in the third column.
#' @param defineplot defaults to TRUE, determines whether to set up an new 
#'     graphics window.
#'
#' @return returns nothing but does plot a graph
#' @export
#'
#' @examples
#' \dontrun{
#' data(dataspm)
#' fish <- dataspm$fish
#' glb <- dataspm$glb
#' props <- dataspm$props
#' pars <- c(14,0.3)
#' aspmLL(pars,fish,glb,props)      # should be -2.277029
#' bestspm <- fitASPM(pars,infish=fish,inglb=glb,inprops=props)
#' fishery <- dynamics(bestspm$par,fish,glb,props)
#' ceCI <- getLNCI(fishery[,"PredCE"],bestspm$par[2])
#' plotceASPM(fishery,CI=ceCI)
#' }
plotceASPM <- function(infish,CI=NA,defineplot=TRUE) { # infish=fisheryPen; CI=ceCI; defineplot=TRUE
   if (defineplot) { 
      if (names(dev.cur()) %in% c("null device", "RStudioGD"))
         dev.new(width = 7, height = 4.5, noRStudioGD = TRUE)
   } 
   par(mfrow=c(1,1),mai=c(0.4,0.5,0.1,0.05),oma=c(0.0,0,0.0,0.0)) 
   par(cex=1.0, mgp=c(1.35,0.35,0), font.axis=7,font=7,font.lab=7,tck=-0.02)  
   yrs <- infish$Year
   if (class(CI) == "matrix") { 
      confint <- CI[,3]
    } else {   
      confint <- NA  
   }
   ymax <- getmaxy(c(infish$CPUE,infish$PredCE,confint))
   plot(yrs,infish$CPUE,type="p",ylim=c(0,ymax),yaxs="i",
        xlab="",panel.first=grid(),ylab="Relative CPUE")
   if (class(CI) == "matrix") {
      arrows(x0=yrs,y0=CI[,1],x1=yrs,y1=CI[,3],code=3,length=0.03,angle=90,
             lwd=1,col=4)
   }   
   points(yrs,infish$CPUE,pch=16,col=2,cex=1.0)     
   lines(yrs,infish$PredCE,lwd=2,col=1)
} # end of plotceASPM

#' @title prodASPM summarizes ASPM statistics and plots the productivity
#'
#' @description prodASPM summarizes ASPM statistics and plots the productivity.
#'     The statistics it returns are the MSY, Bmsy, Hmsy (harvest rate that
#'     leads at equilibrium to MSY), Dmsy (the depletion level at which MSY
#'     occurs, and the predicted B0). These are printed to the console if the
#'     parameter 'console' is set to TRUE. In addition, a plot of the production
#'     curve, (yield vs Spawning Biomass), yield vs Harvest Rate, and yield vs
#'     Depletion level is produced if the parameter plot is set to TRUE
#'
#' @param inprod The production matrix generated by getProductionC
#' @param target in addition to the MSY what biomass target, in terms of
#'     target x B0 is wanted. Default = 0.48
#' @param console print the results directly to the console; default = TRUE
#' @param plot plot the yield vs Spawning biomass, harvest rate, and depletion.
#'     defaults to TRUE.
#'
#' @return a vector containing seven output statistics. It also prints to the
#'     console and plots the results if those parameters are set TRUE
#' @export
#'
#' @examples
#' \dontrun{
#' data(fishdat)
#' fish <- fishdat$fish
#' glb <- fishdat$glb
#' props <- fishdat$props
#' pars <- c(14,0.3)
#' bestL <- optim(pars,aspmLL,method="Nelder-Mead",infish=fish,inglb=glb,
#'                inprops=props,control=list(maxit=1000,parscale=c(10,0.1)))
#' prod <- getProduction(exp(bestL$par[1]),infish=fish,inglb=glb,inprops=props,
#'                       Hrg=c(0.0005,0.07,0.0005),nyr=50)
#'  plotprep(width=7,height=6)
#' spsprod <- prodASPM(prod, console=TRUE, plot=TRUE)
#' }
prodASPM <- function(inprod, target=0.48, console=TRUE, plot=TRUE) {
   pickMSY <- which.max(inprod[,"Yield"])
   MSY <- inprod[pickMSY,"Yield"]
   Hmsy <- inprod[pickMSY,"Harvest"]
   Bmsy <-  inprod[pickMSY,"SpawnB"]
   B0 <- inprod[1,"SpawnB"]
   Dmsy <- inprod[pickMSY,"SpawnB"]/B0
   pickTarg <- which.closest(target,inprod[,"Depletion"])
   targC <- inprod[pickTarg,"Yield"]
   Htarg <- inprod[pickTarg,"Harvest"]
   Btarg <- inprod[pickTarg,"SpawnB"]
   if (console) {
      cat("MSY   = ", MSY,"\n")
      cat("Bmsy  = ", Bmsy,"\n")
      cat("Hmsy  = ", Hmsy,"\n")
      cat("Dmsy  = ", Dmsy,"\n")
      cat("B0    = ", B0, "\n")
      cat("targC = ", targC,"\n")
      cat("Htarg = ", Htarg,"\n")
      cat("Btarg = ", Btarg,"\n")
   }
   if (plot) {
      par(mfrow=c(3,1),mai=c(0.45,0.45,0.05,0.05),oma=c(0.1,0,0.0,0.0))
      par(cex=1.0, mgp=c(1.35,0.35,0), font.axis=7,font=7,font.lab=7)
      ymax <- getmaxy(inprod[,"Yield"],mult=1.1)
      plot(inprod[,"SpawnB"],inprod[,"Yield"],type="l",lwd=2,col=1,ylim=c(0,ymax),yaxs="i",
           xlab="Spawning Biomass (t)",ylab="Surplus Production (t)")
      grid()
      text(0.8*B0,0.9*MSY,paste0("MSY = ",round(MSY,3)),cex=1,pos=4)
      text(0.8*B0,0.75*MSY,paste0("targC = ",round(targC,3)),cex=1,pos=4)
      text(0.75*Bmsy,0.075*ymax,paste0("Bmsy = ",round(Bmsy,3)),cex=1,pos=4)
      abline(h=c(0,MSY),col=c(1,2))
      abline(v=c(Bmsy,Btarg,inprod[1,"SpawnB"]),col=c(2,3,"grey"))
      plot(rev(inprod[,"Harvest"]),rev(inprod[,"Yield"]),type="l",lwd=2,col=1,ylim=c(0,ymax),yaxs="i",
           xlim=c(max(inprod[,"Harvest"]),0),xlab="Annual Harvest Rate",ylab="Surplus Production (t)")
      grid()
      text(0.95*Hmsy,0.075*ymax,paste0("Hmsy = ",round(Hmsy,4)),cex=1,pos=4)
      abline(h=c(0,MSY),col=c(1,2))
      abline(v=c(Hmsy,Htarg),col=c(2,3))
      plot(inprod[,"Depletion"],inprod[,"Yield"],type="l",lwd=2,col=1,ylim=c(0,ymax),yaxs="i",
           xlab="Stock Depletion Level",ylab="Surplus Production (t)")
      grid()
      text(0.75*Dmsy,0.075*ymax,paste0("Dmsy = ",round(Dmsy,4)),cex=1,pos=4)
      abline(h=c(0,MSY),col=c(1,2))
      abline(v=c(Dmsy,target),col=c(2,3))
   }
   ans <- c(MSY=MSY,Bmsy=Bmsy,Hmsy=Hmsy,Dmsy=Dmsy,B0=B0,
            targC=targC,Htarg=Htarg,Btarg=Btarg)
   return(ans)
} # end of prodASPM

#' @title robustASPM conducts a robustness test on the quality of fit of an ASPM
#' 
#' @description robustASPM conducts a robustness test on the quality of fit of 
#'     an ASPM. This is done by using the original optimal model parameters or 
#'     the original guessed parameter values, add random variation to each of 
#'     them, and re-fit the model. This process needs to be repeated multiple 
#'     times. This should enable an analysis of the stability of the modelling 
#'     outcomes. If the optimum parameters are used then add more variation, if
#'     initial guesses are used you may need to select different starting points
#'     so that the random variation covers the parameter space reasonably well.
#'
#' @param inpar the parameter set to begin the trials with
#' @param fish the fisheries data: at least year, catch, and cpue
#' @param glb the global variables containing the biological information
#' @param props the properties calculated from the globals
#' @param N the number of random trials to run; defaults to 10, which is too few
#' @param scaler the divisor that sets the degree of normal random variation to 
#'     add to the parameter values; default = 15 the smaller the value the more
#'     variable the outcome
#' @param Hrange a vector of three numbers denoting the range of harvest rates
#'     to use when characterizing the productivity implied by each fitted 
#'     parameter set. defaults to c(0.01, 0.45, 0.005)
#' @param numyrs the number of years used to drive each harvest rate to an
#'     equilibrium population structure
#' @param console print summary statistics to the screen? default = TRUE
#'
#' @return a list of results from each run, the range of values across runs, and
#'     the median values.
#' @export
#'
#' @examples
#' \dontrun{
#'   data(dataspm)
#'   fish <- dataspm$fish
#'   glb <- dataspm$glb
#'   props <- dataspm$props
#'   pars <- c(14,0.19,0.6)
#'   out <- robustASPM(pars,fish,glb,props)
#'   str(out)
#'   print(out$results)
#' }
robustASPM <- function(inpar,fish,glb,props,N=10,scaler=15,
                       Hrange=c(0.01,0.45,0.005),numyrs=50,console=TRUE) {
   origpar <- inpar
   origpar[1] <- exp(origpar[1]) # return Ln(R0) to nominal sale
   if (length(origpar) == 2) {
      pars <- cbind(rnorm(N,mean=origpar[1],sd=origpar[1]/scaler),
                    rnorm(N,mean=origpar[2],sd=origpar[2]/scaler))  
      columns <- c("iLnR0","isigmaCE","iLike","LnR0","sigmaCE",
                   "-veLL","MSY","B0","Iters")  # prefix i implies input
      usefun=aspmLL
   } else {
      pars <- cbind(rnorm(N,mean=origpar[1],sd=origpar[1]/scaler),
                    rnorm(N,mean=origpar[2],sd=origpar[2]/scaler),
                    rnorm(N,mean=origpar[3],sd=origpar[3]/scaler))
      usefun=aspmPENLL
      columns <- c("iLnR0","isigmaCE","iDepl","iLike","LnR0","sigmaCE","Depl",
                   "-veLL","MSY","B0","Iters")  # prefix i implies input
   }
   pars[,1] <- log(pars[,1])  # return R0 to log(R0)
   results <- matrix(0,nrow=N,ncol=length(columns),dimnames=list(1:N,columns))
   for (i in 1:N) {    # use aspmPENLL just in case   i=1
      origLL <-  usefun(pars[i,],fish,glb,props)        
      bestSP <- fitASPM(pars[i,],fish,glb,props,callfun=usefun)
      opar <- bestSP$par
      prod <- getProductionC(exp(opar[1]),fish,glb,props,
                             Hrg=Hrange,nyr=numyrs)
      anspen <- prodASPM(prod,console=FALSE,plot=FALSE)
      results[i,] <- c(pars[i,],origLL,bestSP$par,bestSP$value,anspen["MSY"],
                       anspen["B0"],bestSP$counts[1])
      if (console) cat(i,"   ")
   }
   if (console) cat("\n")
   ordres <- results[order(results[,"-veLL"]),] # see best and worst fit
   bounds <- apply(results,2,range)
   medvalues <- apply(results,2,median)
   if (console) {
      print(bounds)   # see the minimum and maximum)
      print(medvalues) 
   }
   return(list(results=ordres,range=bounds,medians=medvalues))
} # end of robustASMP

#' @title SpB - calculate spawning biomass from a vector of numbers-at-age
#'
#' @description SpB - calculates the spawning biomass from a vector of
#'     numbers-at-age, Maturity-at-age, and Weight-at-age.
#'
#' @param invect - the numbers-at-age as a vector
#' @param MatureA maturity at age vector
#' @param WeightA weight at age vector as kilograms
#' @return SpB - a scalar as tonnes.
#' @export
#' @examples
#' \dontrun{
#' data(fishdat)
#' str(fishdat)
#' glb <- fishdat$glb
#' fish <- fishdat$fish
#' props <- fishdat$props
#' unfish <- unfished(glb,props,glb$R0)
#' N0 <- unfish$N0
#' SpB(N0,props$maa,props$waa)  # should be 18326.52
#' unfish$B0
#' }
SpB <- function(invect, MatureA, WeightA) {
   ans <- sum(MatureA * WeightA * invect)/1000.0
   return(ans)
}

#' @title unfished generates the numbers at age for an unfished population
#'
#' @description unfished generates the numbers at age for an unfished
#'     population, and determines the recruitment dynamics. It requires the
#'     input of R0. The output includes the unfished numbers-at-age N0, and the 
#'     unfished exploitable biomass, which is from the numbers-at-age after 
#'     half of natural mortality, ExN0.
#' @param glob the global constants object containing biology and structure
#' @param propert the data.frame containing laa, maa, waa, maa, and sela
#' @param inR0 the unfished recruitment from B0
#' 
#' @return a list containing N0, ExNO, R0, A0, B0, and ExB0
#' @export
#' 
#' @examples
#' \dontrun{
#' data(fishdat)
#' glb <- fishdat$glb
#' fish <- fishdat$fish
#' props <- fishdat$props
#' unfish <- unfished(glb,props,glb$R0)
#' print(unfish)
#' }
unfished <- function(glob,propert,inR0) {
   # setup for calculations
   R0 <- exp(inR0)
   maxage <- glob$maxage
   hsurv <- exp(-glob$M/2)
   surv <- exp(-glob$M)
   Nt <- numeric(maxage+1)
   Nt2 <- numeric(maxage+1)  # Needed for calculation of Exploitable biomass
   # now calculate numbers-at-age per recruit
   Nt[1] <- 1    # sets R0 = 1 for initiation NaA[0,0]
   for (age in 1:(maxage-1)) Nt[age+1] <- Nt[age] * surv
   Nt[maxage+1] <- (Nt[maxage] * surv)/(1-surv)
   # Estimate the biomass A0 generated by a recruitment of 1.0
   A0 <- SpB(Nt,propert$maa,propert$waa) # to get tonnes
   B0 <- R0 * A0
   # Now Generate the initial age distribution using R0
   Nages <- length(glob$ages)
   N0 <- R0*Nt
   # Now calculate the unfished exploitable biomass half way through year
   Nt2[1] <- R0
   Nt2[2:(Nages-1)] <- N0[1:(Nages-2)] * hsurv
   Nt2[Nages] <- (N0[(Nages-1)]*hsurv) + (N0[(Nages)]*hsurv)
   Nt2[1] <- Nt2[1] * hsurv
   expB0 <- ExB(Nt2,propert$sela,propert$waa)
   res <- list(N0=N0, B0=B0, ExN0=Nt2, ExB0=expB0, R0=R0, A0=A0)
   return(res)
}  # end of unfished




