

#' @title asmreduction conducts an age-structured stock reduction
#'
#' @description asmreduction conducts an age-structured stock
#'     reduction based on R functions out of the datalowSA package.
#'
#' @param inR0 the trial value of unfished recruitment R0
#' @param fish a data.frame containing the year and catch in each year
#' @param glb  the global variables defined in the data structures for
#'     datalowSA
#' @param props the biological properties of the species, including
#'     length-, weight-, maturity-, and selectivity-at-age
#' @param limitH a vecotr of two numbers denoting the lowest and
#'     highest values of the maximum harvest rate the stock is
#'     assumed to have experienced.
#' @param projyr number of years fo projecting at a constant catch. If
#'     set to 0 the contents of constC are ignored
#' @param constC the constant catch to apply in the projections
#'
#' @return a list containing a summary matrix, and the full results
#'     for fully selected harvets rate, the spawning biomass, the
#'     depletion, and the explotiable biomass in each trajectory.
#' @export
#'
#' @examples
#' \dontrun{
#'   data(orhdat1)
#'   fish <- orhdat1$fish # 19 years of data
#'   glb <- orhdat1$glb
#'   props <- orhdat1$props
#'   inR0 <- seq(12.0,14.2,0.02)
#'   limitH <- c(0.25,0.5)
#'   glb$M <- 0.032
#'   glb$steep <- 0.6
#'   reduct <- asmreduction(inR0,fish,glb,props,limitH=limitH)
#'   str(reduct,max.level = 1)
#' }
asmreduction <- function(inR0,fish,glb,props,limitH=c(0,1),
                         projyr=0,constC=0.0) {
   #  projyr=0; constC=0
   steps <- length(inR0)
   year <- fish[,"year"]
   yrs <- c((year[1]-1),year)
   norigyr <- length(yrs)
   if (projyr > 0) {
      endyr <- tail(year,1)
      addyrs <- (endyr+1):(endyr+projyr)
      yrs <- c(yrs,addyrs)
      fish <- as.data.frame(cbind(year=yrs[2:length(yrs)],
                                  catch=c(fish[,"catch"],rep(constC,projyr))))
   }
   nyrs <- length(yrs)
   columns <- c("logR0","B0","Bcurr","depl","MaxH")
   answer <- matrix(0,nrow=steps,ncol=length(columns),dimnames=list(inR0,columns))
   fullh <- matrix(0,nrow=nyrs,ncol=steps,dimnames=list(yrs,inR0))
   spawnb <- matrix(0,nrow=nyrs,ncol=steps,dimnames=list(yrs,inR0))
   exploitb <- matrix(0,nrow=nyrs,ncol=steps,dimnames=list(yrs,inR0))
   depl <- matrix(0,nrow=nyrs,ncol=steps,dimnames=list(yrs,inR0))
   for (i in 1:steps) { # step through inR0   i=1
      fishery <- dynamics(inR0[i],infish=fish,inglb=glb,inprops=props)
      answer[i,] <- c(inR0[i],getB0(exp(inR0[i]),glb,props),fishery[norigyr,"SpawnB"],
                      fishery[norigyr,"Deplete"],max(fishery[,"FullH"],na.rm=TRUE))
      fullh[,i] <- fishery[,"FullH"]
      spawnb[,i] <- fishery[,"SpawnB"]
      depl[,i] <- fishery[,"Deplete"]
      exploitb[,i] <- fishery[,"ExploitB"]
   }
   maxH <- apply(fullh[1:norigyr,],2,max,na.rm=TRUE) # max H in each trajectory
   pickL <- which.closest(limitH[1],maxH)  # pick low H
   pickH <- which.closest(limitH[2],maxH)  # pick high H
   pickR <- pickH:pickL  # pick rows
   if (length(pickR) <= 5)
     stop("Lowest logR0 value not low enough to achieve lowest limH \n")
   out <- list(answer=answer,fullh=fullh,spawnb=spawnb,depl=depl,
               pickR=pickR,yrs=yrs,inR0=inR0,limitH=limitH,
               projyr=projyr,constC=constC,M=glb$M,h=glb$steep,
               catches=fish[,"catch"])
   return(out)
} # end of asmreduction

#' @title calcout runs asmreduction across a vector of M values
#' 
#' @description calcout runs asmreduction across an input vector of M 
#'     values and outputs a lisdt of aspreduction lists.
#'
#' @param mvalues a vector of natural mortality values to replace those
#'     inside the global object
#' @param inR0 the trial value of log(R0)
#' @param fish a data.frame containing the year and catch in each year
#' @param glb  the global variables defined in the data structures for
#'     datalowSA
#' @param props the biological properties of the species, including
#'     length-, weight-, maturity-, and selectivity-at-age
#' @param limitH a vector of two numbers denoting the lowest and
#'     highest values of the maximum harvest rate the stock is
#'     assumed to have experienced.
#'
#' @return a list of asmreduction lists
#' @export
#'
#' @examples
#' \dontrun{
#'   data(orhdat1)
#'   fish <- orhdat1$fish # 19 years of data
#'   glb <- orhdat1$glb
#'   props <- orhdat1$props
#'   inR0 <- seq(12.0,14.2,0.02)
#'   limitH <- c(0.25,0.5)
#'   glb$M <- c(0.03,0.32,0.36,0.38,0.04,0.042,0.44)
#'   glb$steep <- 0.6
#'   out <- calcout(mvalues,inR0,fish,glb,props,limitH=limitH)
#'   str(out,max.level=1)
#' }
calcout <- function(mvalues,inR0,fish,glb,props,limitH=limitH) {  #
   #   inout=out6; inR0=inR0; fish=fish;glb$steep=0.6;glb=glb;props=props;
   #   limitH=limitH; mvalues=mvalues
   numm <- length(mvalues)
   out <- vector("list",length(mvalues))
   names(out) <- mvalues
   for (i in 1:numm) {
      glb$M <- mvalues[i]
      out[[i]] <- asmreduction(inR0,fish,glb,props,limitH=limitH)
   }
   return(out=out)
} # end of calcout

#' projstats
#'
#' @param inreduct the list object generated by asmreduction,
#'     containing the result of the age-structured stock reduction
#'
#' @return a matrix of depletion values a tthe start and end of the
#'     projection period and their difference.
#' @export
#'
#' @examples
#' \dontrun{
#'   data(orhdat1)
#'   fish <- orhdat1$fish # 19 years of data
#'   glb <- orhdat1$glb
#'   props <- orhdat1$props
#'   inR0 <- seq(12.0,14.2,0.02)
#'   limitH <- c(0.25,0.5)
#'   glb$M <- 0.032
#'   glb$steep <- 0.6
#'   reduct <- asmreduction(inR0,fish,glb,props,limitH=limitH)
#'   projstats(reduct)
#'   reduct2 <- asmreduction(inR0,fish,glb,props,limitH=limitH,projyr=10,
#'                           constC=200)
#'   projstats(reduct2) # search for small differences and low gradients 
#' }
projstats <- function(inreduct) {  # inreduct=reduct
   yrs <- inreduct$yrs
   nyrs <- length(yrs)
   projyr <- inreduct$projyr
   if (inreduct$projyr > 0) {
      first <- nyrs - projyr
      gradyr <- nyrs - trunc(projyr/2)
   } else {
      stop("No years of projection present. \n")
   }
   pickR <- inreduct$pickR
   steps2 <- length(pickR)
   R0 <- inreduct$inR0[pickR]
   columns <- c(c(yrs[first],yrs[gradyr],yrs[nyrs]),"diff","grad")
   delta <- matrix(0,nrow=steps2,ncol=length(columns),
                   dimnames=list(R0,columns))
   depl2 <- inreduct$depl[,pickR]
   delta[,1:3] <- t(depl2[c(first,gradyr,nyrs),])
   delta[,"diff"] <- (delta[,3] - delta[,1])
   delta[,"grad"] <- (delta[,3] - delta[,2])/(yrs[nyrs] - yrs[gradyr])
   return(delta)
} # end of projstats

#' plotout
#'
#' @param out a list of asmreduction lists generated by calcout
#'
#' @return a matrix of logR0, DeplHH, logR0, DeplLH, and Steepness
#' @export
#'
#' @examples
#' \dontrun{
#'   data(orhdat1)
#'   fish <- orhdat1$fish # 19 years of data
#'   glb <- orhdat1$glb
#'   props <- orhdat1$props
#'   inR0 <- seq(12.0,14.2,0.02)
#'   limitH <- c(0.25,0.5)
#'   glb$M <- c(0.03,0.32,0.36,0.38,0.04,0.042,0.44)
#'   glb$steep <- 0.6
#'   out <- calcout(mvalues,inR0,fish,glb,props,limitH=limitH)
#'   plotprep(width=6, height=4)
#'   plotout(out)
#' }
plotout <- function(out) {
   R0 <- out[[1]]$answer[,"logR0"]
   numm <- length(out)
   ymax <- 0.0
   for (i in 1:numm) {
      reduct <- out[[i]]
      tmp <- max(reduct$answer[,"depl"])
      ymax <- max(tmp,ymax) * 1.025
   }
   columns <- c("logR0","DeplHH","logR0","DeplLH","Steepness")
   bounds <- matrix(NA,nrow=numm,ncol=5,dimnames=list(names(out),columns))
   plot(R0,out[[1]]$answer[,"depl"],type="n",ylim=c(0,ymax),yaxs="i",
        ylab='Depletion',panel.first=grid())
   abline(h=c(0.2,0.48),col=c(2,3),lty=2)
   for (i in 1:numm) { # i = 1
      ans <- out[[i]]$answer
      pickR <- out[[i]]$pickR
      lines(R0,ans[,"depl"],lwd=2,col=i)
      xval <- c(R0[pickR[1]],R0[tail(pickR,1)])
      yval <- c(ans[pickR[1],"depl"],ans[tail(pickR,1),"depl"])
      points(xval,yval,pch=16,cex=1.2)
      bounds[i,] <- c(R0[pickR[1]],ans[pickR[1],"depl"],
                      R0[tail(pickR,1)],ans[tail(pickR,1),"depl"],
                      out[[1]]$h)
   }
   mtext(paste0("Steepness = ",out[[1]]$h),side=3,outer=F,line=-1.1,font=7,
         cex=1.1)
   return(bounds)
} # end of plotout


#' plotdepletion
#'
#' @param inreduct the list object generated by asmreduction,
#'     containing the result of the age-structured stock reduction
#' @param defineplot a boolean determining whether to define the pars
#'     for a plot or not. defaults to TRUE
#' @param refpts a vector of two defining the limit and target
#'     reference points; defaults to 0.2 and 0.48
#'
#' @return nothing but it does generate a plot
#' @export
#'
#' @examples
#' \dontrun{
#'   data(orhdat1)
#'   fish <- orhdat1$fish # 19 years of data
#'   glb <- orhdat1$glb
#'   props <- orhdat1$props
#'   inR0 <- seq(12.0,14.2,0.02)
#'   limitH <- c(0.25,0.5)
#'   glb$M <- 0.032
#'   glb$steep <- 0.6
#'   reduct <- asmreduction(inR0,fish,glb,props,limitH=limitH)
#'   plotprep(width=6, height=4)
#'   plotdeletion(reduct)
#' }
plotdepletion <- function(inreduct,defineplot=TRUE,refpts=c(0.2,0.48)) {
   yrs <- inreduct$yrs
   nyrs <- length(yrs)
   pickR <- inreduct$pickR
   if (length(pickR) <= 1)
      stop("Lowest logR0 value not low enough to achieve lowest limH \n")
   steps2 <- length(pickR)
   projyr <- inreduct$projyr
   if (defineplot) {
      par(mfrow=c(1,1),mai=c(0.45,0.45,0.05,0.05),oma=c(0.0,0,0.0,0.0))
      par(cex=0.85, mgp=c(1.35,0.35,0), font.axis=7,font=7,font.lab=7)
   }
   depl2 <- inreduct$depl[,pickR]
   ymax <- getmaxy(depl2)
   plot(yrs,depl2[,1],type="l",ylim=c(0,ymax),lwd=1,col="grey",
        ylab="Depletion",panel.first=grid(),yaxs="i")
   for (i in 2:steps2) lines(yrs,depl2[,i],lwd=1,col="grey")
   abline(h=refpts,col=c(2,3))
   if (projyr > 0) abline(v=(inreduct$yrs[nyrs - projyr]+0.5),col=3)
   label <- paste0("H = ",inreduct$limitH[2])
   text(min(yrs)+1,0.05*ymax,label,pos=4,cex=1.1,font=7)
   mtext("Year",side=1,outer=T,line=0.0,font=7,cex=1.1)
} # end of plotdepletion

#' @title plotreduction generates a summary plot of a stock reduction
#'
#' @description plotreduction generates a summary plot of the output
#'     from an age-structured stock reduction produced by the
#'     asmreduction function, which in turn relies on the dynamics
#'     function from the aspm within the datalowSA package.
#'
#' @param inreduct the list object generates by asmreduction
#' @param defineplot boolean which determines whether a par statement
#'     is made or not. default = TRUE.
#' @param refpts a vector of two defining the limit and target
#'     reference points; defaults to 0.2 and 0.48
#'
#' @return outputs the stats from any projection period. also produces
#'     a 3,1 plot of FullH, spawning biomass, and depletion for the
#'     input stock reduction
#' @export
#'
#' @examples
#' \dontrun{
#'   data(orhdat1)
#'   fish <- orhdat1$fish # 19 years of data
#'   glb <- orhdat1$glb
#'   props <- orhdat1$props
#'   inR0 <- seq(12.0,14.2,0.02)
#'   limitH <- c(0.25,0.5)
#'   glb$M <- 0.032
#'   glb$steep <- 0.6
#'   reduct <- asmreduction(inR0,fish,glb,props,limitH=limitH)
#'   plotprep(width=6,height=5.5)
#'   plotreduction(reduct)
#' }     # inreduct=reduct; defineplot=TRUE
plotreduction <- function(inreduct,defineplot=TRUE,refpts=c(0.2,0.48)) {
   yrs <- inreduct$yrs
   nyrs <- length(yrs)
   pickR <- inreduct$pickR
   if (length(pickR) <= 1) {
      print(inreduct$answer)
      stop("logR0 values are failing to deplete or permit the limitH \n")
   }
   steps2 <- length(pickR)
   projyr <- inreduct$projyr
   if (defineplot) {
      par(mfrow=c(3,1),mai=c(0.25,0.45,0.05,0.05),oma=c(1.0,0,0.0,0.0))
      par(cex=0.85, mgp=c(1.35,0.35,0), font.axis=7,font=7,font.lab=7)
   }
   fullh2 <- inreduct$fullh[,pickR]
   ymax <- getmaxy(fullh2)
   plot(yrs,fullh2[,1],type="l",ylim=c(0,ymax),lwd=1,col="grey",
        ylab="FullH",panel.first=grid(),yaxs="i",xlab="")
   for (i in 2:steps2) lines(yrs,fullh2[,i],lwd=1,col="grey")
   if (projyr > 0) {
      xloc <- inreduct$yrs[nyrs - projyr]+0.5
      abline(v=xloc,col=3)
      text(xloc+0.5,0.95*ymax,paste0("ConstC = ",inreduct$constC),
           pos=4,cex=1.1)
   }
   catch <- inreduct$catches
   ymax <- getmaxy(catch)
   plot(yrs,c(NA,catch),type="l",lwd=2,ylim=c(0,ymax),yaxs="i",
        ylab="Catches (t)",xlab="",panel.first=grid())
   if (projyr > 0) abline(v=(inreduct$yrs[nyrs - projyr]+0.5),col=3)
   depl2 <- inreduct$depl[,pickR]
   ymax <- getmaxy(depl2)
   plot(yrs,depl2[,1],type="l",ylim=c(0,ymax),lwd=1,col="grey",
        ylab="Depletion",panel.first=grid(),yaxs="i")
   for (i in 2:steps2) lines(yrs,depl2[,i],lwd=1,col="grey")
   abline(h=refpts,col=c(2,3))
   if (projyr > 0) abline(v=(inreduct$yrs[nyrs - projyr]+0.5),col=3)
   label <- paste0("H = ",inreduct$limitH[1])
   text(max(yrs)-5,0.9*ymax,label,pos=4,cex=1.1,font=7)
   label <- paste0("H = ",inreduct$limitH[2])
   text(max(yrs)-5,0.05*ymax,label,pos=4,cex=1.1,font=7)
   mtext("Year",side=1,outer=T,line=0.0,font=7,cex=1.1)
   if (projyr > 0) {
      out <- projstats(inreduct)
      class(out) <- "reduction"
      return(out)
   }
} # end of plotreduction


#' @title summary.reduction S3 summary method for a reduction object
#'
#' @description summary.reduction an  S3 summary method for an asmreduction 
#'     object
#'
#' @param object matrix output from plotreduction
#' @param ... other arguments
#' @return a list containing the range of differences between the
#'     first and last year of projection and the gradient between the
#'     last year and half the projection years.
#'
#' @export
#' @examples 
#' \dontrun{
#'   data(orhdat1)
#'   fish <- orhdat1$fish # 19 years of data
#'   glb <- orhdat1$glb
#'   props <- orhdat1$props
#'   inR0 <- seq(12.0,14.2,0.02)
#'   limitH <- c(0.25,0.5)
#'   glb$M <- 0.032
#'   glb$steep <- 0.6
#'   reduct <- asmreduction(inR0,fish,glb,props,limitH=limitH,projyr=10,
#'                           constC=200)
#'   x <-  plotreduction(reduct,defineplot=FALSE)
#'   summary(x)
#' }
summary.reduction <- function(object, ...) {
  rgediff <- range(object[,"diff"])
  rgegrad <- range(object[,"grad"])
  columns <- as.numeric(colnames(object)[2:3])
  numyr <- columns[2] - columns[1]
  cat("\n",rgediff," last - first year of projection  \n")
  cat(rgegrad," gradient of last ", numyr,"projection years  \n")
  cat("\n")
  NextMethod("summary")
}

