
#' @title compyear plots a histogram of a variable for each year available
#' 
#' @description compyear plots a histogram of either ages or lengths for each 
#'     year available. It assumes the data are available as counts in columns,
#'     one for each age/length class with a row for each year. These can be 
#'     plotted as frequencies or proportions, with flexibility in colouring,
#'     column widths, and other details of the plots.
#'
#' @param x a matrix of years (rows) by counts (cols)
#' @param yrs the labels for each row of x
#' @param comps the ages or lengths to be plotted on the x-axis
#' @param plots number of rows and columns, defaults to c(length(x[,1]),1)
#' @param freq plot numbers 'freq' or proportions; default = TRUE
#' @param varlabel what label to use on x-axis, default = NA
#' @param width the width of each column in the histogram, default = 0.8
#' @param col colour of each cell; defaults to 2 (red)
#' @param border colour of the border of each cell, default = 2 (red)
#' @param lwd the width of the line around each polygon, default = 1
#' @param xmin starting value for xaxis, default = NA, so input range used
#' @param xmax end value for xaxis, default = NA, so input range used
#' @param inc the steps along xaxis, default = 1
#' @param xaxis if FALSE then a custom xaxis is plotted, default is TRUE
#' @param vline optionally plot a vertical on each plot, default = 0 = no line
#'
#' @return currently it returns nothing although it does generate a plot.
#' @export
#'
#' @examples
#' \dontrun{
#' indat <- c(95,2898,3017,1159,591,116,100,82,33,77,606,4385,1186,231,
#'            138,42,21,51,50,489,1121,4738,456,106,80,27,18)
#' naa <- matrix(indat,nrow=3,ncol=9,byrow=TRUE)
#' yrs <- c(1931,1932,1933)
#' ages <- 2:10
#' plotprep()
#' compyear(naa,yrs,ages,plots=c(2,2),freq=TRUE,border=3)
#' } # x=naa;yrs=yrs;comps=ages;plots=c(3,3);freq=TRUE;border=3
#'   # varlabel=NA;width=0.9;col=2;lwd=1;xmin=NA;xmax=NA;inc=1;xaxis=TRUE;vline=0
compyear <- function(x,yrs,comps,plots=c(length(x[,1]),1),freq=TRUE,
                     varlabel=NA,width=0.9,col=2,border=2,
                     lwd=1,xmin=NA,xmax=NA,inc=1,xaxis=TRUE,vline=0) {
   nyr <- length(yrs)
   ncomp <- length(comps)
   totfreq <- rowSums(x,na.rm=TRUE)
   if (!freq) for (i in 1:nyr) x[i,] <- x[i,]/totfreq[i]
   ymax <- max(x,na.rm=TRUE) * 1.05
   if (is.na(xmin)) xmin <- min(comps)
   if (is.na(xmax)) xmax <- max(comps)
   par(mfcol=plots,mai=c(0.225,0.25,0.025,0.05),oma=c(1.2,1.1,0.0,0.0)) 
   par(cex=0.75, mgp=c(1.35,0.35,0), font.axis=7,font=7,font.lab=7) 
   for (yr in 1:nyr) {
      values <- x[yr,]
      plot(comps,values,type="n",
           xlim=c((xmin-(width*0.75)),(xmax+(width*0.75))),
           xaxs="r",ylim=c(0,ymax),yaxs="i",xlab="",ylab="",xaxt="n")
      if (xaxis) axis(side=1,at=seq(xmin,xmax,inc),labels=seq(xmin,xmax,inc))
      for (i in 1:ncomp) {  # i <- 1
         x1 <- comps[i] - (width/2)
         x2 <- comps[i] + (width/2)
         xval <- c(x1,x1,x2,x2,x1)
         yval <- c(0,values[i],values[i],0,0)
         if (is.null(border)) border <- col
         polygon(xval,yval,col=col,border=border,lwd=lwd)
      }
      mtext(paste0(yrs[yr]," - ",totfreq[yr],"  "),side=3,outer=F,line=-1.0,font=7,cex=0.8,adj=1)       
   }
   if (is.na(varlabel)) {
      label <- "Numbers-at-Age"       
     } else {
      label <- varlabel  
   }
   if ((!freq) & (is.na(varlabel))) {
      label <- "Proportion-at-Age"      
   }
   mtext(label,side=2,outer=T,line=0.0,font=7,cex=1.0) 
} # end of compyear

#' @title ccyear plots a histogram of naa and plots log(naa) each year available
#' 
#' @description ccyear plots a histogram of either ages or lengths for each 
#'     year available. It assumes the data are available as counts in columns,
#'     one for each age/length class with a row for each year. These can be 
#'     plotted as frequencies or proportions, with flexibility in colouring,
#'     column widths, and other details of the plots.
#'
#' @param x a matrix of years (rows) by counts (cols)
#' @param yrs the labels for each row of x
#' @param comps the ages or lengths to be plotted on the x-axis
#' @param freq plot numbers 'freq' or proportions; default = TRUE
#' @param varlabel what label to use on x-axis, default = NA
#' @param width the width of each column in the histogram, default = 0.8
#' @param col colour of each cell; defaults to 2 (red)
#' @param border colour of the border of each cell, default = 2 (red)
#' @param lwd the width of the line around each polygon, default = 1
#' @param xmin starting value for xaxis, default = NA, so input range used
#' @param xmax end value for xaxis, default = NA, so input range used
#' @param inc the steps along xaxis, default = 1
#' @param xaxis if FALSE then a custom xaxis is plotted, default is TRUE
#' @param vline optionally plot a vertical on each plot, default = 0 = no line
#' @param title is literally a title for the plot. defaults to ""
#'
#' @return currently it returns nothing although it does generate a plot.
#' @export
#'
#' @examples
#' \dontrun{
#' indat <- c(95,2898,3017,1159,591,116,100,82,33,77,606,4385,1186,231,
#'            138,42,21,51,50,489,1121,4738,456,106,80,27,18)
#' naa <- matrix(indat,nrow=3,ncol=9,byrow=TRUE)
#' yrs <- c(1931,1932,1933)
#' ages <- 2:10
#' plotprep()
#' ccyear(naa,yrs,ages,plots=c(2,2),freq=TRUE,border=3,title="Example")
#' } # x=naa;yrs=yrs;comps=ages;freq=TRUE;border=3
#'   # varlabel=NA;width=0.9;col=2;lwd=1;xmin=NA;xmax=NA;inc=1;xaxis=TRUE;vline=0
#'   title="Example"
ccyear <- function(x,yrs,comps,freq=TRUE,varlabel=NA,width=0.9,
                   col=2,border=2,lwd=1,xmin=NA,xmax=NA,
                   inc=1,xaxis=TRUE,vline=0,title="") {
   plots <-c(length(x[,1]),2)
   nyr <- length(yrs)
   ncomp <- length(comps)
   totfreq <- rowSums(x,na.rm=TRUE)
   if (!freq) for (i in 1:nyr) x[i,] <- x[i,]/totfreq[i]
   ymax <- max(x,na.rm=TRUE) * 1.05
   if (is.na(xmin)) xmin <- min(comps)
   if (is.na(xmax)) xmax <- max(comps)
   par(mfrow=plots,mai=c(0.225,0.25,0.025,0.05)) 
   par(cex=0.75, mgp=c(1.35,0.35,0), font.axis=7,font=7,font.lab=7)
   if (nchar(title) > 0) { 
      par(oma=c(1.2,1.1,1.0,0.0))
   } else {
      par(,oma=c(1.2,1.1,0.0,0.0))
   }
   for (yr in 1:nyr) {  # yr=1
      values <- x[yr,]
      plot(comps,values,type="n",
           xlim=c((xmin-(width*0.75)),(xmax+(width*0.75))),
           xaxs="r",ylim=c(0,ymax),yaxs="i",xlab="",ylab="",xaxt="n")
      if (xaxis) axis(side=1,at=seq(xmin,xmax,inc),labels=seq(xmin,xmax,inc))
      for (i in 1:ncomp) {  # i <- 1
         x1 <- comps[i] - (width/2)
         x2 <- comps[i] + (width/2)
         xval <- c(x1,x1,x2,x2,x1)
         yval <- c(0,values[i],values[i],0,0)
         if (is.null(border)) border <- col
         polygon(xval,yval,col=col,border=border,lwd=lwd)
      }
      mtext(paste0(yrs[yr]," - ",totfreq[yr],"  "),side=3,outer=F,line=-1.0,
            font=7,cex=0.8,adj=1) 
      yvalues <- log(x[yr,])
      ymax2 <- getmaxy(yvalues)
      plot(comps,yvalues,type="p",pch=16,cex=1.0,col=2,ylim=c(0,ymax2),yaxs="i",
           ylab="",xlab="",panel.first = grid())
      lines(comps,yvalues,lwd=1,col=1) 
   }
   if (is.na(varlabel)) {
      label <- "Numbers-at-Age & Log(N)"       
   } else {
      label <- varlabel  
   }
   if ((!freq) & (is.na(varlabel))) {
      label <- "Proportion-at-Age & Log(N)"      
   }
   mtext(label,side=2,outer=T,line=0.0,font=7,cex=1.0) 
   mtext("Ages",side=1,outer=T,line=0.0,font=7,cex=1.0) 
   if (nchar(title) > 0) mtext(title,side=3,outer=T,line=0.0,font=7,cex=1.0) 
} # end of ccyear


#' @title classicCC conducts a naive catch curve on the input data
#' 
#' @description classicCC conducts a classical catch curve in which one selects 
#'     the first age to be included in the calculation, assuming it is the 
#'     first age to be fully selected in the fishery. It merely applies a linear
#'     regression to the log-transformed counts by age and subtracts the natural
#'     mortality from teh gradient (total mortality) to obtain an estimate of 
#'     the fishing mortality experienced on average across the age-classes 
#'     included in the analysis.
#'
#' @param NM an estimate of the natural mortality
#' @param ages the age classes encountered in the sample to be analysed
#' @param freqs the counts of each age-class in the sample
#' @param firstage the first age assumed to be fully selected by the fishing 
#'     gear. younger ages are not included in the analysis. 
#' @param plot a boolean determining whether to plot the catch curve, default
#'     is TRUE 
#'
#' @return a list containing the estimated fishing mortality, FF, the regression
#'     coefficients, and the model itself 
#' @export
#'
#' @examples
#' \dontrun{
#' age <- 0:9
#' counts <- c(3,4,501,2531,936,233,76,17,5,1)
#' M <- 0.6
#' pickage <- 3
#' out <- classicCC(M,age,counts,pickage,plot=TRUE)
#' print(out)
#' }           # NM=glb$M; ages=ages;freqs=freqs;firstage=7;plot=TRUE
classicCC <- function(NM,ages,freqs,firstage=0,plot=TRUE) {
   pickA <- which((ages >= firstage) & (freqs > 0))
   model <- lm(log(freqs[pickA]) ~ ages[pickA])
   Z <- -coef(model)[2]
   FF <- Z - NM
   coefs <- coef(model); names(coefs) <- c("Intercept","Z")
   if (plot) {
      if (names(dev.cur()) %in% c("null device", "RStudioGD"))
         dev.new(width = 6, height = 4, noRStudioGD = TRUE)
      par(mfrow=c(1,1),mai=c(0.45,0.5,0.1,0.05),oma=c(0.0,0,0.0,0.0)) 
      par(cex=1.0, mgp=c(1.35,0.35,0), font.axis=7,font=7,font.lab=7,tck=-0.02)  
      ymax <- getmaxy(log(freqs))
      plot(ages,log(freqs),type="p",pch=16,cex=1.0,
           xlab="Age",ylab="log(Numbers-at-Age",ylim=c(0,ymax),
           panel.first=c(grid(),abline(h=0,lwd=1,col="grey")))
      abline(model,col=4,lwd=2)
      text(0.9*max(ages),0.9*ymax,paste0("M = ",round(NM,3)))
      text(0.9*max(ages),0.8*ymax,paste0("F = ",round(FF,3)))
   }
   ans <- list(FF=FF,coef=coefs,regress=model)
   return(ans)
} # end of classicCC

#' @title popN calculates the proportional catch at age for catch curves
#' 
#' @description popN calculates the expected proportional catch-at-age for use
#'     when using a multinomial likeihood to fit a catch curve that has been
#'     enhanced through the addition of a logistic selectivity curve. It
#'     generates the expected equilibrium relative numbers-at-age in the 
#'     population and then uses the Baranov catch equation to generate the 
#'     expected numbers-at-age in the catch, and hence to be expected in the
#'     catch curve. This approach requires three parameters while the classicCC
#'     uses only two parameters. This function is 
#'     used within the function selectCC, that fits a catch curve enhanced with
#'     a selectivity curve so as to use the counts from all ages. It can also
#'     be used when plotting the outcome of a catch curve analysis.
#'
#' @param maxa the maximum age in the population and sample
#' @param M the natural mortality estimate
#' @param initF the fishing mortality to be tested
#' @param select the selectivity of the fishery
#'
#' @return a vector of the expected proportional distribution of numbers-at-age
#' @export
#' 
#' @examples 
#' \dontrun{
#' M <-  0.6
#' age <- 0:9
#' counts <- c(3,4,501,2531,936,233,76,17,5,1)
#' pars <- c(lm50=2.5,delta=0.5,fcur=0.6)
#' pp <- popN(max(age),M,pars[3],logist(pars[1],pars[2],age))
#' round(cbind(age,counts/sum(counts),pp),5)
#' }
popN <- function(maxa,M,initF,select) { # maxa=9;M=M;initF=pars[3];select=sel
   nages <- maxa + 1
   Nt <- numeric(nages)
   selF <- select * initF
   Nt[1] <- 1
   for (i in 2:maxa) Nt[i] <- Nt[i-1]*exp(-(selF[i] + M))
   Nt[nages] <- (Nt[maxa]*exp(-(selF[maxa] + M)))/
                (1 - exp(-(selF[maxa] + M)))
   Ncatch <- ((Nt * selF)/(selF + M))*(1 - exp(-(selF + M)))
   Np <- Ncatch/sum(Ncatch)
   return(Np)
}

#' @title multLL calculates the negative log-likelihood for the multinomial
#' 
#' @description multLL is used when calculating the -ve log-likelihood for the
#'     multinomial used to describe the age-composition in a selectivity
#'     enhanced catch curve (see selectCC). It assumes the existence of three
#'     variables in its environment: 'age' - a vector of ages for which counts are
#'     available, 'M' the natural mortality, and 'counts' the count of each 
#'     age-class in the sample. These variables need to have these specific 
#'     names.
#'
#' @param pars a vector containing starting estimates of the selectivity 
#'     parameters inL50 - the age at which 50% are selected, delta, which is
#'     twice the difference between the L50 and L95, and FF, the 
#'     fishing mortality being estimated by the catch curve.
#' @param ages the ages for which counts are available
#' @param counts the counts of each age-class - the numbers-at-age
#' @param M the natural mortality estimate
#' @param matchage which ages contain data to be compared; no NAs included.
#'
#' @return the negative multinomial log-likelihood
#' @export
#'
#' @examples
#' \dontrun{
#' M <-  0.6
#' age <- 0:9
#' counts <- c(3,4,501,2531,936,233,76,17,5,1)
#' pars <- c(lm50=2.5,delta=0.5,fcur=0.6)
#' pp <- popN(max(age),M,pars[3],logist(pars[1],pars[2],age))
#' # calculate the negative log-likelihood
#' -dmultinom(counts,sum(counts),pp,log=TRUE)
#' # calculate the negative log-likelihood in a manner suitable for optim
#' matchage <- 1:10 # a value for each age
#' multLL(pars,age,counts,M,matchage)  
#' }
multLL <- function(pars,ages,counts,M,matchage) { # pars=pars; ages=allage;counts=counts;M=M; matchage=matchage
   sel <- logist(pars[1],pars[2],ages)
   pp <- popN(max(ages),M,pars[3],sel)
   LL <- multinomLL(counts,pp[matchage]) 
   return(LL)
}


#' @title selectCC conducts a selectivity enhanced catch curve analysis
#' 
#' @description selectCC encapsulates the routines required to fit a selectivity
#'     enhanced catch curve to a catch-at-age composition sample. The method is 
#'     described in Wayte, S.E. and N.L. Klaer (2010) An effective harvest 
#'     strategy using improved catch-curves. Fisheries Research 106: 310-320. 
#'     Although the log-likelihood used here is based on the strict multinomial
#'     distribution.
#'
#' @param M the natural mortality estimate
#' @param maxage is the maximum age in the available data
#' @param counts is the numbers at age, a vector of counts with names that are the
#'     ages to which the counts relate
#' @param pars three values in a vector, these are the initial estimates of the 
#'     two selectivity parameters (A50, the age at 50% selection, and delta = 
#'     (A95 - A50), of course delta must be positive), and the initial 
#'     estimate (guess) of the fishing mortality. See the example below.
#' @param plot a logical parameter determining whether to plot the result or not,
#'     the default is FALSE
#'
#' @return a list containing the output from optim, 'best', with the parameter 
#'     estimates and the other diagnostics from the fit, and a matrix, 'result' 
#'     containing the observed and predicted values and counts
#' @export
#'
#' @examples
#' \dontrun{
#' M <-  0.6
#' ages <- 0:9
#' counts <- c(3,4,501,2531,936,233,76,17,5,1)
#' names(counts) <- 0:9
#' pars <- c(A50=2.5,delta=0.5,fcur=0.6)
#' selectCC(M,max(ages),counts,pars)
#' # now with a plot
#' selectCC(M,max(ages),counts,pars,plot=TRUE)
#' }  
selectCC <- function(M,maxage,counts,pars,plot=FALSE) { 
   # M=0.3; maxage=max(ages);counts=numaa;pars=pars;plot=TRUE
   countages <- as.numeric(names(counts))   
   allage <- 0:maxage
   matchage <- which(allage %in% countages)
   parscl <- magnitude(pars)
   bestL <- optim(pars,multLL,method="Nelder-Mead",ages=allage,
                  counts=counts,M=M,matchage=matchage,
                  control=list(maxit = 1000, parscale = parscl))
   optpar <- bestL$par
   sel <- logist(optpar[1],optpar[2],allage)
   pp <- popN(maxage,M,optpar[3],logist(optpar[1],optpar[2],allage))
   pcount <- pp[matchage] * sum(counts)
   logcount <- matrix(NA,nrow=length(counts),ncol=1)
   predcount <- logcount
   pickages <- which(counts > 0)
   logcount[pickages] <- log(counts[pickages])
   predcount[pickages] <- log(pcount[pickages])
   pCAA <- counts/sum(counts)  
   FaA <- sel[matchage]*bestL$par[3]
   if (plot) {
      if (names(dev.cur()) %in% c("null device", "RStudioGD"))
         dev.new(width = 6, height = 4, noRStudioGD = TRUE)    
      par(mfrow = c(2,1), mai = c(0.45, 0.45, 0.1, 0.05))
      par(cex = 0.85, mgp = c(1.35, 0.35, 0),font.axis = 7,font = 7,font.lab = 7)
      ages <- allage[matchage]
      ymax <- getmaxy(c(pCAA,pp))
      plot(ages,pCAA,type="p",pch=16,cex=1.0,col=1,ylim=c(0,ymax),yaxs="i",
           ylab="Proportion-at-Age",panel.first=c(grid(),abline(h=0,col="grey")))
      lines(ages,pp[matchage],lwd=2,col=2)
      lines(ages,sel[matchage]*(0.975*ymax),col=3,lwd=1)
      ymax <- getmaxy(logcount) 
      plot(ages,logcount,type="p",pch=16,cex=1.0,col=1,ylim=c(0,ymax),
           ylab="Log of Counts",panel.first=c(grid(),abline(h=0,col="grey")))
      points(ages,predcount,pch=16,cex=1,col=2)
      lines(ages,predcount,lwd=2,col=2)
      lines(ages,sel[matchage]*max(predcount),lwd=1,col=3)
      text(0.8*max(ages),0.9*ymax,paste0("Fully Selected F = ",round(optpar[3],4),cex=1.0,font=7))
      label <- paste0("Av naa weighted F = ",round(sum(counts * FaA)/sum(counts),4))
      text(0.8*max(ages),0.8*ymax,label,cex=1.0,font=7)
   }
   result <- cbind(counts,pcount,logcount,predcount,pCAA,pp,sel,FaA)
   colnames(result) <- c("counts","predcounts","logcount","logpredcount",
                         "pCAA","predpCAA","Selectivity","FaA")
   rownames(result) <- ages
   ans <- list(best=bestL,result=result)
   return(ans)
}




equilN <- function(maxa,M,initF,select) { # maxa=max(ages);M=M;initF=0.0;select=sel
   nages <- maxa + 1
   Nt <- numeric(nages)
   selF <- select * initF
   Nt[1] <- 1
   for (i in 2:maxa) Nt[i] <- Nt[i-1]*exp(-(selF[i] + M))
   Nt[nages] <- (Nt[maxa]*exp(-(selF[maxa] + M)))/
      (1 - exp(-(selF[maxa] + M)))
   return(Nt)
}
