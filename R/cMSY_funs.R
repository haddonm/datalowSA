#' @title central generates three estimates of central tendency
#'
#' @description central generates three estimates of central tendency and
#'     the quantiles about the distribution of the inut vector of values. The
#'     three measures are the arithmetic mean, the naive geometric mean, and
#'     the bias corrected geometric mean.
#'
#' @param x the vector of values whose central tendency is to be characertized
#' @param P the quantiles to be determined
#'
#' @return a 4 x 2 matrix containing the central tendency measures and the
#'    quantiles
#' @export
#'
#' @examples
#' \dontrun{
#' x <- rnorm(1000,mean=5,sd=1)
#' central(x,P=0.9)
#' x <- rlnorm(1000,meanlog=2,sdlog=0.2)
#' central(x,P=0.95)
#' exp(2)
#' }
central <- function(x,P=0.90) {
   avx <- mean(x,na.rm=TRUE)
   sdx <- sd(x,na.rm=TRUE)
   gav <- mean(log(x), na.rm = TRUE)
   gsd <- sd(log(x), na.rm = TRUE)
   gmean <- exp(gav + (gsd^2)/2)
   if ((is.numeric(P)) & (P < 1.0)) { ps <- c((1-P)/2,P + (1-P)/2)
   } else { ps <- c(0.05,0.95) }
   qs <- quantile(x,probs=ps)
   rows <- c("Arithmetic","Geometric","BCGeometric","90%Q 5%-95%")
   columns <- c("Mean","sd")
   ans <- matrix(0,nrow=4,ncol=2,dimnames=list(rows,columns))
   ans[,"Mean"] <- c(avx,exp(gav),gmean,qs[1])
   ans[,"sd"] <- c(sdx,gsd,gsd,qs[2])
   return(ans)
} # end of central

#' @title cMSYphaseplot plots the phase plot and catch and harvest rate plots
#' 
#' @description cMSYphaseplot extracts the necessary data to enable the 
#'     production of a phase plot of estimated average biomass against estimated
#'     average harvets rate. It plots the Bmsy = 0.5B0 for the Schaefer model as 
#'     well as 20%B0 = 20%K as a default limit reference point. The first year 
#'     of data is identified by a larger green point and the last year by a 
#'     larger red point. It also plots the expected harvest rate that should 
#'     lead to MSY, called Ftarg and that, which if continued for long enough,
#'     would drive the biomass below the limit reference point, Flim. Points 
#'     above the Flim line (or Ftarg depending on which management objectives are
#'     used) would be classed as over-fishing leading to a status of 'depleting'
#'     and points to the left of 0.2B0 would be over-fished or 'depleted'.
#'     In addition, the function plots the catch history and the implied harvest
#'     rates just below the phase plot to aid in its interpretation.
#'
#' @param answer the output from the run_cMSY function
#' @param fish the fishery data put into run_CMSY
#'
#' @return a list of meanB, meanH, msy, Bmsy, Hmsy, and Hlim, returned invisibly
#' @export
#'
#' @examples
#' \dontrun{
#'   data(invert)
#'   fish <- invert$fish
#'   glb <- invert$glb
#'   answer <- run_cMSY(fish,glb,n=1000,sigpR=0.025)
#'   plotprep(width=7,height=6)
#'   cMSYphaseplot(answer,fish)
#' }
cMSYphaseplot <- function(answer,fish) { 
   summcMSY <- summarycMSY(answer,fish,final=TRUE)
   avr <- summcMSY$meanr["Geometric","Mean"]
   avK <- summcMSY$meanK["Geometric","Mean"]
   biom <- seq(10,avK,length=200)
   prod <- avr*biom*(1-biom/avK)
   msy <- avr*avK/4
   pickY <- which.closest(msy,prod)
   Bmsy <- biom[pickY]
   Hmsy <- msy/Bmsy
   pickY2 <- which.closest(avK*0.2,biom)
   Bdepl <- biom[pickY2]
   Hlim <- prod[pickY2]/Bdepl
   outphase <- plottrajectory(answer$R1,fish$year,fish$catch,answer$parbound,
                              oneplot=TRUE,scalar=1.0,plotout=FALSE,plotall=7)
   numval <- length(outphase$medianB) - 1
   medianB <- outphase$medianB[1:numval]
   medianH <- outphase$medianH[1:numval]
   ymax <- getmaxy(medianH)
   par(mai=c(0.45,0.45,0.05,0.45),oma=c(0.0,0,0.0,0.0)) 
   par(cex=0.85, mgp=c(1.35,0.35,0), font.axis=7,font=7,font.lab=7)  
   layout(matrix(c(1,2)),heights=c(3,1))
   plot(medianB,medianH,type="l",lwd=2,col=2,ylim=c(0,ymax),xlim=c(0.1*avK,avK),
        ylab="Harvest Rate",xlab="Predicted Mean Biomass")
   points(medianB,medianH,pch=16,cex=1,col=1)
   points(medianB[1],medianH[1],pch=16,cex=1.5,col=3)
   points(medianB[numval],medianH[numval],pch=16,cex=1.5,col=2) # end point
   abline(v=c(avK*0.2,Bmsy,avK),col=c(2,3,3),lty=2)
   abline(h=c(Hmsy,Hlim),col=c(4,2),lty=2)
   text(Bmsy,0.0,"~Btarg",font=7,cex=1.0,pos=4)
   text(0.2*avK,0.0,"0.2B0",font=7,cex=1.0,pos=4)
   text(0.9*avK,1.075*Hmsy,"~Ftarg",font=7,cex=1.0,pos=4)
   text(0.9*avK,1.075*Hlim,"~Flim",font=7,cex=1.0,pos=4)
   cmax <- getmaxy(fish$catch)
   plot(fish$year,fish$catch,type="l",lwd=2,col=2,ylab="",xlab="",ylim=c(0,cmax),
        yaxs="i")
   par(new=TRUE)
   plot(fish$year,medianH,type="l",lwd=2,col=4,ylim=c(0,ymax),yaxt="n",ylab="",
        yaxs="i",xlab="",panel.first = grid(ny=0))
   abline(h=Hmsy,col=4,lty=2)
   ym2 <- round(ymax,2)
   axis(side=4,at=seq(0,ym2,length=3),labels = seq(0,ym2,length=3))
   mtext("Catch (t)",side=2,outer=F,line=1.2,font=7,cex=1.0,col=2) 
   mtext("Harvest Rate",side=4,outer=F,line=1.1,font=7,cex=1.0,col=4) 
   result <- list(medianB=medianB,medianH=medianH,msy=msy,Bmsy=Bmsy,
                  Hmsy=Hmsy,Hlim=Hlim,succB=outphase$succB,
                  succH=outphase$succH)
   return(invisible(result))
} # end of cMSYphaseplot



#' @title doconstC calculates and plots projections under a constant catch
#' 
#' @description doconstC merely combines the functions gettraject, doproject,
#'     makedeplet, and plotconstC to simplify the process of conducting 
#'     projections using constant catches. Compare this with separately using 
#'     the functions gettraject, doproject, makedeplet, and plotconstC in 
#'     sequence, which is all that dococnstC does.
#'
#' @param inR1 the R1 object that is within the list generated by run_cMSY.
#' @param projn the number of years of projection with a default of 5
#' @param constCatch the constant catch to be applied to each successful trajectory
#' @param lastyear the final year of the known catches and biomass trajectories
#' @param limit the depletion level acting as the limit referencepoint
#' @param target the depletion level used as a biomass target for the species.
#' @param console logical, should results be printed to the console. Default
#'     =TRUE
#' @param intensity the value that defines the density of trajectories required
#'     to give rise to full colour; default = NA which implies grey
#'
#' @return a matrix of the all years with the proportion < 20%, the 
#'     proportion > 48% (the input target), the mean and median depletion, and 
#'     the proportion of trajectories that were increasing relative to the 
#'     lastyear of data.
#' @export
#'
#' @examples
#' \dontrun{
#' data(invert)
#' fish <- invert$fish
#' glb <- invert$glb
#' reps <- 10000  # one would run at least 20000, preferably more
#' answer <- run_cMSY(fish,glb,n=reps,sigpR=1e-6)
#' out <- doconstC(answer$R1,projn=5,constCatch=150,lastyear=2017,
#'                 limit=0.2,target=0.4)
#' str(out)
#' }
doconstC <- function(inR1,projn=5,constCatch=100,lastyear=2017,limit=0.2,
                     target=0.48,console=TRUE,intensity=NA) {
   traject <- gettraject(inR1,projn=projn)
   newtraj <- doproject(traject,constC=constCatch)
   trajdepl <- makedeplet(newtraj)
   result <- plotconstC(trajdepl,endyear=lastyear,constC=constCatch,
                        limit=limit,target=target,intensity=intensity,
                        console=console)
   if (console) print(result)
   output <- list(result=result,trajdepl=trajdepl)
   return(output)
}


#' @title doproject after running the cMSY analysis the plausible trajectories
#'
#' @description after running the run_cMSY analysis this function projects each
#'     of the accepted biomass trajectories
#'
#' @param intraj the successful biomass trajectories obtained from gettraject
#' @param constC the constant catch that is to be applied as a projection
#' @param projn the number of years to be projected.
#' @param sigpR the process error, the same as used in run_cMSY
#'
#' @return the same biomass trajecotry matrix as input excpet the empty years
#'     will have been filled with projections of the surplus production
#'     dynamics made under the constant catch level.
#' @export
#'
#' @examples
#' \dontrun{
#' data(invert)
#' fish <- invert$fish
#' glb <- invert$glb
#' reps <- 5000  # one would run at least 20000, preferably more
#' answer <- run_cMSY(fish,glb,n=reps,sigpR=0.04)
#' traject <- gettraject(answer$R1,projn=5)
#' newtraj <- doproject(traject,constC=100)
#' head(newtraj)
#' trajdepl <- makedeplet(newtraj)
#' plotconstC(trajdepl,endyear=2017,constC=100,target=0.40)
#' }
doproject <- function(intraj,constC,projn=5,sigpR=0.025) { # intraj=traject; constC=50; projn=5; sigpR=0.025
   Ntraj <- dim(intraj)[1]
   pick <- which(is.na(intraj[1,]))
   Nproj <- length(pick)
   if (Nproj > 0) {
      yrs <- colnames(intraj)[pick]
      yrindex <- pick-1      
   } else {
      widtraj <- dim(intraj)[2]
      start <- widtraj - 3
      startyr <- as.numeric(colnames(intraj)[start])
      yrs <- startyr:(startyr + projn)
      yrindex <- start:(start + projn - 1)
      insertcol <- matrix(NA,nrow=Ntraj,ncol=projn)
      colnames(insertcol) <- yrs[2:(projn+1)]
      intraj <- cbind(intraj[,1:start],insertcol,intraj[,(widtraj-2):widtraj])
   }
   for (i in 1:Ntraj) {  # i = 1
      r <- intraj[i,"r"]
      K <- intraj[i,"K"]
      for (yr in yrindex) {
         Bt <- intraj[i,yr]
         intraj[i,(yr+1)] <- max(Bt + (r*Bt) * (1 - Bt/K) - constC,0.0)
      }
   }
   return(intraj)
} # end of doproject

#' @title fillell2 runs the criteria of success on each SRA
#'
#' @description fillell2 runs the acceptance criteria over the biomass
#'     trajectories obtained from the Stock Reduction Analysis for each
#'     combination of r, K, and starting depletion conducted in makebiomC. The
#'     criteria agsint which each trajectory are tested include that the
#'     final depletion be less than the maximum of the expected range of final
#'     depletions and greater than the minimum of final depletion levels. In
#'     addition I have included that the initial biomass level be greater
#'     than the final biomass level. This differs from the extra criteria used
#'     by Martell and Froese (2013), who selected the smallest K still able to
#'     generate the mean predicted MSY. 
#'
#' @param biot the matrix of biomass trajectories obtained from makebiomC,
#'     containing the SRA trajectories for the aprticular combination of
#'     r and K across the range of starting biomass trajectories.
#' @param intheta the array of parameters associated with the biomass
#'     trajectories. Includes r, K, the range of depletions and the process
#'     error added to productivity - sigR.
#' @param mult a multiplier for K to allow for stock biomasses to rise above K
#' @param ct  the vector of catches per year
#' @param Hmax upper limit of harvest rate included in the constraints;
#'     defaults to 1.0, which implies no upper limit.
#' @param Fyear is the index to the year in which a rnage of harvest rates
#'     is to be used to constrina the acceptable trajectories.
#'
#' @return a vector of 1 and 0 relating to an acceptable trajectory (one that
#'     met the criteria) and unacceptable (one that failed the criteria)
#'     
#' @export
#' @examples 
#' \dontrun{
#' print("This is the function that imposes constraints on the biomass")
#' print("trajectories. Add new ones to the end of the long if statement")
#' print("They need to define what is required to keep a trajectory")
#' }    #   biot=biot; intheta=itheta[tick,]; mult=mult; ct=ct; Hmax=maxH;Fyear=NA 
fillell2 <- function(biot, intheta, mult, ct, Hmax=1.0,Fyear) { 
   lenB0 <- dim(biot)[2]
   nyr1 <- dim(biot)[1]
   ell <- numeric(lenB0)
   K <- intheta["K"] * mult
   harvest <- c(0,ct) / biot
   for (i in 1:lenB0) {
      if ((biot[nyr1,i]/K >= intheta["findep1"]) &
          (biot[nyr1,i]/K <= intheta["findep2"]) &
          (sum((biot[,i] > 0)) == length(biot[,i])) &
          (sum((biot[,i] <= K)) == length(biot[,i])) &
          (all(harvest[Fyear,i] <= Hmax))) {
         ell[i] <- 1
      } else {
         ell[i] <- 0
      }
   }
   return(ell)
} # end of fillell2

#' @title getprop extract proportion of values between lim1 and lim2
#' 
#' @description getprop extracts the proportion of values in invect either
#'     below lim1, between lim1 and lim2, or above lim2. If lim2 has the 
#'     value 0.0 and lim1 > 0 then the proportion below lim2 is extracted. 
#'     If lim1 = 0.0 and lim2 > 0, then the proportion > lim2 is extracted.
#'     If both lim1 and lim2 > 0 then the proportion between the two is
#'     extracted. 
#'
#' @param invect the collection of values to be subdivided
#' @param lim1 the lower limit of values
#' @param lim2 the upper limit of values
#'
#' @return a scalar containing the proportion of records  
#' @export
#'
#' @examples
#' \dontrun{
#'   x <- 1:100
#'   getprop(x,20)
#'   getprop(x,20,50)
#'   getprop(x,lim2=80)
#' }
getprop <- function(invect,lim1=0.0,lim2=0.0) {
   totN <- length(invect)
   if ((lim1 > 0.0) & (lim2 == 0.0)) {
      tmp <- length(which(invect <= lim1))/totN
      prop <- c(lim1=lim1,lim2=lim2,prop=tmp)
   } 
   if ((lim1 == 0.0) & (lim2 > 0.0)) {
      tmp <- length(which(invect > lim2))/totN
      prop <- c(lim1=lim1,lim2=lim2,prop=tmp)
   }
   if ((lim1 > 0.0) & (lim2 > 0.0)) {
      tmp <- length(which((invect > lim1) & (invect <= lim2)))/totN
      prop <- c(lim1=lim1,lim2=lim2,prop=tmp) 
   }
   if ((lim1 == 0.0) & (lim2 == 0.0)) {
      warning("No lim1 or lim2 values input \n")
      prop <- -1
   }
   return(prop)
} # end of getprop

#' @title sraMSY sets up the input parameters and data for oneSRA
#'
#' @description sraMSY sets up the run parameters in terms of input parameter
#'     bounds, the number of replicates, and other parameters.
#'     Not exported but can be viewed using datalowSA:::sraMSY.
#'
#' @param theta a list containing the initial parameter values including
#'     the initial r range (2 numbers), the initial k range (2 numbers), the
#'     final depletion range (2 numbers), and the recruitment variability
#' @param N the number of replicate searches across the parameter space
#' @param startbd the set of initial starting depletion levels
#' @param nyr the number of years of catch data
#' @param ct the vector of catches per year
#' @param yr the vector of years
#' @param mult a multiplier for K to allow for stock biomasses to rise above K;
#'     default value = 1.0, so K is the default upper limit
#' @param maxH upper limit of harvest rate included in the constraints;
#'     defaults to 1.0, which implies no upper limit.
#' @param Fyear is the index to the year in which a rnage of harvest rates
#'     is to be used to constrina the acceptable trajectories.
#'
#' @return a list containing itheta (the vectors of parameters), elltot (a
#'     matrix of the yes/no vectors for each initial biomass depletion level),
#'     and biomass (the biomass trajectories for each of the N parameter
#'     vectors)  # theta=parbound;N=n;startbd=startbd;nyr=nyr;ct=ct;yr=yr;mult=multK; maxH=1.0;Fyear=NA
sraMSY <- function(theta, N, startbd, nyr, ct, yr,mult,maxH,Fyear) {
   ri <- runif(N, theta$r[1], theta$r[2])
   ki <- runif(N, theta$k[1], theta$k[2])
   lenB0 <- length(startbd)
   # itheta <- matrix(0,nrow=N,ncol=5,dimnames=list(1:N,c("r","K","findep1",
   #                                                      "findep2","sigR")))
   itheta <- cbind(r=ri,K=ki,
                   findep1=theta$finaldepl[1], findep2=theta$finaldepl[2],
                   sigR=theta$sigR)
   biomass <- array(0,dim=c(N,(nyr+1),lenB0),
                    dimnames=list(1:N,seq(yr[1],(yr[nyr]+1),1),startbd))
   elltot <- matrix(0,nrow=N,ncol=lenB0,dimnames=list(1:N,startbd))
   for (tick in 1:N) { # tick=1
      biot <- makebiomC(itheta[tick,],startbd,ct) #, nyr,numbd)
      biomass[tick,,] <- biot
      elltot[tick,] <- fillell2(biot, itheta[tick,], mult, ct, Hmax=maxH,Fyear) 
   }
   return(list(itheta=itheta,elltot=elltot,biomass=biomass))
}  # end of sraMSY

#' @title run_cMSY - sets up and runs the catch-MSY simulations
#'
#' @description run_cMSY  - sets up and runs the catch-MSY simulations
#'    The input data is merely a matrix with, as a minimum, a column of years, 
#'    and a column of catches, the input object 'glob' needs to contain the 
#'    resilience. Strictly it only needs
#'    the resilience. run_cMSY calls sraMSY, which, in turn calls oneSRA.
#'    Eventially it calls plottrajectory to plot out results. This code derives 
#'    from example code produced by Martell and Froese (2013) A simple method 
#'    for estimating MSY from catch and resilience Fish and Fisheries 14:504-514.
#'
#' @param indat - a data.frame, with at least a 'catch' column containing
#'     catch at time t, a 'year' column for year. The 'fish' object from the
#'     standard data file or readdata.
#' @param glob the globals list 'glb', from readdata or from one of the included
#'     data sets. It contains at least a 'resilience' object for
#'     resilience, which is either 'verylow', 'low', 'medium", or 'high', and 
#'     finally a 'spsname' object, which, not surprisingly, is the name of 
#'     the species concerned.
#' @param n - defaults to 10000; the number of random selections of r and K.
#'     Defines the number of replicate searches within the parameter space.
#' @param incB the increments between the bounds of the initial biomass
#'     depletion levels; defaults to 0.025, but have used 0.05 previously
#' @param sigpR the measure of process error added to the dynamics. If set to a 
#'     very small value, 1e-06, the model will act as deterministic.
#' @param multK a multiplier for K to allow for stock biomasses to rise above K,
#'     defaults to 1.0, ie. K is the upper limit.
#' @param finaldepl this allows the option of externally setting the final
#'     depletion where there have been major reductions in catch that have not
#'     been due to a reduction in the stock; defaults to NA, which sets the
#'     finaldepl to the pre-defined priors
#' @param start_k allows an option to alter the starting K values; for example
#'     in Orange Roughy, gigantic initial catches possibly make up a significant
#'     proportion of the initial biomass so multiplying by 60 or 100 will lead
#'     to ridiculous initial K values. A vector of two numbers is required.
#'     The default is NA, which means it will be c(maxcatch,60*maxcatch)
#' @param start_r allows for altering the default starting r values; for example
#'     in a species though to be of resilience verylow there may be uncertainty
#'     over how low and one might want a range from say 0.01 - 0.3 instead of 
#'     0.015 - 0.125. The default is NA, which implies the schedule of values 
#'     in to code will be used. A vector of two numbers is required.
#' @param initdepl this allows the option of externally setting the initial
#'     depletion. This may be useful where there is evidence that the stock
#'     really has been unfished and is expected to be much closer to 100%B0 
#'     than the default setting of c(0.5, 0.975); defaults to NA, which sets the
#'     initdepl to the pre-defined priors
#' @param maximumH upper limit of harvest rate included in the constraints;
#'     defaults to 1.0, which implies no upper limit.
#' @param Hyear is the index to the year in which a rnage of harvest rates
#'     is to be used to constrina the acceptable trajectories.
#'
#' @return plots up nine graphs summarizing catches, r, K, and MSY.
#'     returns a list containing the vector of relative counts of successful 
#'     trajectories for different combinations of r and K.
#' @export
#' @examples
#' \dontrun{
#' data(invert)
#' fish <- invert$fish
#' glb <- invert$glb
#' reps <- 5000  # one would run at least 20000, preferably more
#' answer <- run_cMSY(fish,glb,n=reps,sigpR=0.04)
#' summcMSY <- summarycMSY(answer,fish,final=TRUE)
#' str(summcMSY,max.level=1)
#' out <- plottrajectory(answer$R1,fish$year,fish$catch,answer$parbound,     
#'                       oneplot=FALSE,scalar=1.0,plotout=TRUE,plotall=7)
#' plotcMSY6(summcMSY,fish[,"catch"],label=glb$spsname)
#' ans <- pulloutStats(answer$R1,probabs=c(0.025,0.05,0.5,0.95,0.975))
#' out <- plotconstC(ans$deplet,endyear=2017,constC=0,console=FALSE,intensity=NA)
#' outC <- doconstC(answer$R1,constCatch=50,lastyear=2017,console=FALSE,intensity=NA)
#' }  
run_cMSY <- function(indat,glob,n=10000,incB=0.025,
                     sigpR=0.025,multK=1.0,finaldepl=NA,start_k=NA,
                     start_r=NA,initdepl=NA,maximumH=1.0,Hyear=NA) {
#  n=10000;incB=0.025;sigpR=0.025;multK=1.0;finaldepl=NA;start_k=NA 
#   start_r=NA;initdepl=NA;maximumH=1.0; indat=fish; glob=glb; Hyear = NA
   colnames(indat) <- tolower(colnames(indat))
   res <- tolower(glob$resilience)
   if ((iscol("catch",indat)) & (iscol("year",indat))) {
      ct <- indat$catch;   
      yr <- indat$year; 
      nyr <- length(yr)
   } else {
      stop("Input data needs to contain columns of 'catch' and 'year' \n")
   }
   if (is.na(Hyear)) Hyear <- 1:(nyr+1)   # ensure that maxH points to a year
   if (sigpR == 0) sigpR <- 1e-8
   # set up initial paramter ranges
   if (is.na(start_r[1])) {
      start_r  <- if(res == "verylow") { c(0.015, 0.125)
                    } else if(res == "low") { c(0.10,0.6)
                      } else if (res == "medium") { c(0.3,0.8)
                        } else if(res == "high") { c(0.6,1.5)
                          } else { c(0.2,1) # default if no res is found
                          }
   } else {
      if (length(start_r) != 2) {
         outtext <- "If start_r is input, it needs to have both lower and upper
         bounds on start_r defined; ie. two numbers. Defaulting to the usual 
         strategy for setting start_r"
         warning(cat(outtext,"\n"))
         start_r  <- if(res == "verylow") { c(0.015, 0.125)
            } else if(res == "low") { c(0.10,0.6)
               } else if (res == "medium") { c(0.3,0.8)
                  } else if(res == "high") { c(0.6,1.5)
                     } else { c(0.2,1) # default if no res is found
         }
      }
   }
   maxct <- max(ct,na.rm=TRUE)
   if (is.na(start_k[1])) {
       start_k <- c(maxct,60*maxct) ## default for k prior
     } else {
       if (length(start_k) != 2) {
         outtext <- "If start_k is input, it needs to have both lower and upper
                     bounds on start_k defined; ie. two numbers. Defaulting to
                     start_k=c(maxct,60*maxct)"
         warning(cat(outtext,"\n"))
         start_k <- c(maxct,60*maxct)
      }
     }
   if (is.na(initdepl[1])) {
      if (ct[1]/maxct < 0.25) {
       initdepl <- c(0.5,0.975)
        } else {
       initdepl <- c(0.15,0.7)
      }
   }
   if (is.na(finaldepl[1])) {
      if (ct[nyr]/maxct > 0.5) {
         finaldepl <- c(0.15,0.7)
        } else { 
         finaldepl <- c(0.05,0.5) 
      }
   }
   pick <- which(is.na(ct))
   if (length(pick) > 0) ct[pick] <- 0.001
   startbd <- seq(initdepl[1], initdepl[2], by = incB) ## init biomass inc 0.05
   parbound <- list(r=start_r,k=start_k,initdepl=initdepl,finaldepl=finaldepl,
                    sigR=sigpR)
   columns <- c("Initial","Final")
   rows <- c("Years","Resilience","ProcessErr","InitDepl","FinalDepl","Low r",
             "Hi  r","Low K","Hi  K","Trials","MSY")
   outtab <- as.data.frame(matrix(0,nrow=length(rows),ncol=length(columns),
                                  dimnames=list(rows,columns)))
   outtab["Years",1:2]  <- range(yr)
   outtab["Resilience",] <- c(res,res)
   outtab["ProcessErr",] <- c(sigpR,sigpR)
   outtab["InitDepl",] <- initdepl
   outtab["FinalDepl",] <- finaldepl
   outtab["Low r",1] <- start_r[1]
   outtab["Hi  r",1] <- start_r[2]
   outtab["Low K",1] <- start_k[1]
   outtab["Hi  K",1] <- start_k[2]
   outtab["Trials",] <- c(n,n)
   ## MAIN
   firstparbound <- parbound
   Rfirst <- sraMSY(parbound,n,startbd,nyr,ct,yr, mult=multK, 
                    maxH=maximumH,Fyear=Hyear)

   out <- plottrajectory(Rfirst,yr,ct,parbound,oneplot=FALSE,plotout=FALSE)
   ell <- out$ell
   rK <- out$rK
   r1 	<- rK[,1]  ## Get statistics on r, k, MSY and
   k1 	<- rK[,2]  ## determine new bounds for r and k
   outtab["MSY",1] <- round(exp(mean(log(rK[,4]))),3)
   max_k1a  <- min(k1[r1<(1.1*parbound$r[1])]) ## smallest k1 near initial lower bound of r
   max_k1b  <- max(k1[(r1*k1/4) < outtab["MSY",1]]) ## largest k1 that gives mean MSY
   max_k1 <- min(max_k1a,max_k1b)
   if(length(r1)<10) {
      stop(cat("Too few (", length(r1), ") possible r-k combinations, check input parameters","\n"))
   }
   ## set new upper bound of r to 1.2 max r1
   parbound$r[2] <- min(parbound$r[2],1.2*max(r1))
   ## set new lower bound for k to 0.9 min k1 and upper bound to max_k1
   parbound$k <- c(0.9 * min(k1), max_k1)
   outtab["Low r",2] <- round(parbound$r[1],3)
   outtab["Hi  r",2] <- round(parbound$r[2],3)
   outtab["Low K",2] <- round(parbound$k[1],3)
   outtab["Hi  K",2] <- round(parbound$k[2],3)
   ## Repeat analysis with new r-k bounds
   R1 <- sraMSY(parbound, n, startbd, nyr, ct, yr, mult=multK, 
                           maxH=maximumH,Fyear=Hyear)
   out <- plottrajectory(R1,yr,ct,parbound,oneplot=FALSE,plotout=FALSE)
   ell <- out$ell
   rK <- out$rK
   outtab["MSY",2] <- round(exp(mean(log(rK[,4]))),3)
   output <- pulloutStats(R1)
   B0 <- R1$biomass[,1,]/ startbd
   finalD <- R1$biomass[,nyr,] / B0
   initialD <- R1$biomass[,1,] / B0
   ans <- list(R1,ell,rK,parbound,output,Rfirst,firstparbound,startbd,outtab,
               initialD,finalD,B0,maximumH)
   names(ans) <- c("R1","ell","rK","parbound","Statistics","Rfirst",
                   "firstparbound","startbd","outtab",
                   "initialDepletion","finaldepletion","B0","MaximumH")
   return(ans)
} # end of run_cMSY


#' @title pulloutStats summaries results from the Catch-MSY analysis
#'
#' @description pulloutStats summaries the results from the Catch-MSY
#'     analysis by generating the mean, minimum, maximum, and quantiles of
#'     the resulting r, K, MSY, and last year depletion values.
#'
#' @param inR1 the input parameter vectors with their respective ok values
#' @param probabs the percentiles used in pulling out the quantiles of the
#'     r, K, and MSY values; default c(0.025, 0.05, 0.1,0.5,0.9, 0.95, 0.975)
#' @return a list containing a matrix of summary statistics regarding the r, K, 
#'     MSY, and last year depletion values, he biomass trajectories, and the 
#'     depletion trajectories
#' @export
#'
#' @examples
#' \dontrun{
#' data(invert)
#' fish <- invert$fish
#' glb <- invert$glb
#' nyr <- length(fish$year)
#' answer <- run_cMSY(fish,glb,n=5000,sigpR=0.04)
#' results <- pulloutStats(answer$R1)
#' str(results)
#' }           
pulloutStats <- function(inR1,probabs=c(0.025, 0.05, 0.5, 0.95, 0.975)) {
   columns <- c("2.5%Perc","Mean","97.5%Perc",paste0((100*probabs),"%"))
   rows <- c("r","K","MSY","CurrDepl")
   output <- matrix(0,nrow=length(rows),ncol=length(columns),
                    dimnames=list(rows,columns))
   rK <- inR1$itheta
   goodbad <- inR1$elltot
   biomass <- inR1$biomass
   ok <- rowSums(goodbad)
   pick <- which(ok > 0)
   good <- goodbad[pick,]
   rows <- as.numeric(rownames(good))
   biom <- biomass[pick,,]
   r <- rK[pick,1]
   K <- rK[pick,2]
   allmsy = (r * K)/4
   meanpCIr <- getLNCI(exp(mean(log(r))),sd(log(r)))
   meanpCIK <- getLNCI(exp(mean(log(K))),sd(log(K)))
   meanpCIMSY <- getLNCI(exp(mean(log(allmsy))),sd(log(allmsy)))
   output[1,] <- c(meanpCIr,quantile(r,probs=probabs))
   output[2,] <- c(meanpCIK,quantile(K,probs=probabs))
   output[3,] <- c(meanpCIMSY,quantile(allmsy,probs=probabs))
   # pull out all the accepted biomass trajectories
   yrs <- as.numeric(unlist(dimnames(biomass)[2]))
   numcol <- dim(biomass)[2]
   startbd <- as.numeric(dimnames(biomass)[[3]])
   rKok <- rK[pick,]
   nok <- length(pick)
   ntraject <- sum(ok)
   traject <- matrix(0,nrow=ntraject,ncol=(numcol+3))
   rownames(traject) <- 1:ntraject
   colnames(traject) <- c(yrs,"r","K","bd")
   count <- 0
   for (i in 1:nok) { 
      usetraj <- which(good[i,] > 0) # which bd successul
      ntraj <- length(usetraj)       # how many successful
      pickrow <- (count + 1):(count + ntraj)
      trajs <- as.matrix(biom[i,,usetraj]) # extract the bd
      depl <- startbd[usetraj]
      userK <- rKok[i,1:2]
      for (j in 1:ntraj) traject[pickrow[j],] <- c(trajs[,j],userK,depl[j])
      count <- count + ntraj
   }
   deplet <- makedeplet(traject)
   lastY <- deplet[,numcol]
   meanDepl <- mean(lastY)
   sdDepl <- sd(lastY)
   output[4,] <- c(meanDepl-1.96*sdDepl,meanDepl,meanDepl+1.96*sdDepl,
                   quantile(lastY,probs=probabs))
   return(list(output=output,traject=traject,deplet=deplet))   
} # end of pulloutStats



#' @title summarycMSY makes tables of msy,r,K,meanr,meanK,and all picks
#'
#' @description summarycMSY generates a list of countcolour, meanmsy, meanr,
#'     meanK, r, K, msy, pickC (a list of pickblk, pickblu, pickyel,pickmax,
#'     rnot, knot), and years
#'
#' @param ans the object output from run_cMSY
#' @param fish the input data often named fish
#' @param final whether or not to consider the first phase or the final phase,
#'     the default is TRUE, which considers the final phase
#'
#' @return a list of 12 objects used to summarize and plot the output. Returned
#'     invisibly; need to first allocate to an object.
#' @export
#'
#' @examples
#' \dontrun{
#' data(invert)
#' fish <- invert$fish
#' glb <- invert$glb
#' 
#' nyr <- length(fish$year)
#' reps <- 5000  # one would run at least 20000, preferably more
#' answer <- run_cMSY(fish,glb,n=reps,sigpR=0.04)
#' summcMSY <- summarycMSY(answer,fish,final=TRUE)
#' str(summcMSY,max.level=1)
#' }
summarycMSY <- function(ans,fish,final=TRUE) {
   if (final) {
       inR1 <- ans$R1
       inparbound <- ans$parbound
     } else {
       inR1 <- ans$Rfirst
       inparbound <- ans$firstparbound
   }
   years <- fish$year
   itheta <- inR1$itheta
   elltot <- inR1$elltot
   ok <- rowSums(elltot)
   pick <- which(ok > 0)
   ell <- elltot[pick,]
   r <- itheta[pick,1]
   K <- itheta[pick,2]
   meanr <- central(r,P=0.95)
   meanK <- central(K,P=0.95)
   maxok <- max(ok,na.rm=TRUE)
   boundsok <- trunc(c(0.25,0.5,0.75)*maxok)
   countcolour <- numeric(5)
   collab <- paste0(c("black_","blue_","yellow_","green_"),
                    c(trunc(c(0.25,0.5,0.75)*maxok),paste0(0.75*maxok,maxok)))
   names(countcolour) <- c("red_0",collab)
   numok <- length(pick)
   niter <- length(itheta[,1])
   countcolour[1] <- niter - numok
   count <- rowSums(ell,na.rm=T)
   pickblk <- which(count <= boundsok[1])
   countcolour[2] <- length(pickblk)
   pickblu <- which((count > boundsok[1]) & (count <= boundsok[2]))
   countcolour[3] <- length(pickblu)
   pickyel <- which((count > boundsok[2]) & (count <= boundsok[3]))
   countcolour[4] <- length(pickyel)
   pickmax <- which(count > boundsok[3])
   countcolour[5] <- length(pickmax)
   rnot <- itheta[-pick,1]       # red rK combination fails
   knot <- itheta[-pick,2]
   msy <- r * K / 4
   meanmsy = central(msy)
   pickC <- list(pickblk=pickblk,pickblu=pickblu,pickyel=pickyel,
                 pickmax=pickmax,rnot=rnot,knot=knot)
   result <- list(countcolour=countcolour,meanmsy=meanmsy,meanr=meanr,
                  meanK=meanK,r=r,K=K,msy=msy,pickC=pickC,years=years,
                  parbound=inparbound,fish=fish)
   return(invisible(result))
} # end of summarycMSY

#' @title halftable halves the height of a tall narrow data.frame
#'
#' @description halftable would be used when printing a table using kable
#'     from knitr where one of the columns was Year. The objective would be to
#'     split the table in half taking the bottom half and attaching it on
#'     the right hand side of the top half. The year column would act as the
#'     index.
#'
#' @param inmat the data.frame to be subdivided
#' @param yearcol the column name of the year field
#' @param subdiv the number of times the data.frame should be subdivided;
#'     the default is 3 but the numbers can only be 2 or 3.
#'
#' @return a data.frame half the height and double the width of the original
#' @export
#'
#' @examples
#' \dontrun{
#' x <- as.data.frame(matrix(runif(80),nrow=20,ncol=4))
#' x[,1] <- 1986:2005
#' x[,4] <- paste0("text",1:20)
#' halftable(x,yearcol="V1",subdiv=2)
#' halftable(x[,c(1,2,4)],yearcol="V1")
#' x1 <- rbind(x,x[1,])
#' x1[21,"V1"] <- 2006
#' halftable(x1,yearcol="V1",subdiv=3)
#' }
halftable <- function(inmat,yearcol="Year",subdiv=3) {
   if (!(subdiv %in% c(2,3))) stop("\n subdiv must be 2 or 3 \n")
   numrow <- dim(inmat)[1]
   numcol <- dim(inmat)[2]
   extra <- rep(NA,numcol)
   if ((numrow %% subdiv) == 0) {
      newnr <- numrow/subdiv
      incomplete <- FALSE
   } else {
      newnr <- trunc(numrow/subdiv) + 1
      incomplete <- TRUE
   }
   # years <- inmat[,yearcol]
   first <- inmat[1:newnr,]
   if (subdiv == 2) {
      second <- inmat[-c(1:newnr),]
      diff <- (nrow(first) - nrow(second))
      if (diff > 0) {
         numcol <- ncol(inmat)
         third <- rbind(second,extra)
      } else {
         third <- second
      }
   } else {
      second <- inmat[c(newnr+1):c(2*newnr),]
      first <- cbind(first,second)
      third <- inmat[-c(1:(2*newnr)),]
      diff <- nrow(first) - nrow(third)
      if (diff > 0) third <- rbind(third,extra)
      if (diff > 1) third <- rbind(third,extra)
   }
   outmat <- cbind(first,third)
   rownames(outmat) <- 1:newnr
   return(outmat)
} # end of halftable



#' @title makedeplet converts the biomass trajectories into a depletion matrix
#'
#' @description makedeplet converts the biomass trajectories into a deplletion
#'     matrix by dividing through each trajectory by its respective K value.
#'     Usually this would be done after the matrix had been projected forward.
#'
#' @param intraj the matrix derived from gettraject containing the successful
#'     biomass trajectories.
#'
#' @return a matrix of only the biomass trajectories once they have each been
#'     divided by their respective K values
#' @export
#'
#' @examples
#' \dontrun{
#' traject <- rbind(c(rnorm(10,mean=200,sd=10),0.5,300,0.65),
#'                  c(rnorm(10,mean=200,sd=10),0.5,300,0.65))
#' colnames(traject) <- c(1:10,"r","K","initD")
#' makedeplet(traject)
#' }
makedeplet <- function(intraj) {
   ntraject <- dim(intraj)[1]
   nyrs <- dim(intraj)[2] - 3
   deplet <- intraj
   for (i in 1:ntraject) deplet[i,1:nyrs] <- deplet[i,1:nyrs]/intraj[i,"K"]
   return(deplet)
} # end of deplet

#' @title gettraject extracts the plausible biomass trajectories from cMSY
#'
#' @description gettraject extracts the final plausible biomass trajectories
#'     from the R1 object that is part of the output from a run_cMSY analysis.
#'     The R1 object contains the table of biomass trajectories, the identifer
#'     of the  individual trajectories within each rK pair that succeeded,
#'     and the rK pairs that were trialed. The output is a matrix of only the
#'     successful biomass trajectories with the associated rK and starting
#'     depletion appending to each trajectory. If no projections are wanted
#'     then projn should be set to 0
#'
#' @param inR1 the R1 object that is within the list generated by run_cMSY.
#' @param projn the number of extra projection years to allow for in the
#'     biomass trajectories placed into the output matrix ready to be filled
#'     by the doproject function. Defaults to 0 which leaves out room
#'     set up for projections.
#'
#' @return a matrix of the accepted biomass trajectories extended by NAs
#'     of the length of projn, plus the rK pair and initial depletion that
#'     gave rise to the successful biomass trajectory.
#' @export
#'
#' @examples
#' data(invert)
#' fish <- invert$fish
#' glb <- invert$glb
#' reps <- 5000  # one would run at least 20000, preferably more
#' answer <- run_cMSY(fish,glb,n=reps,sigpR=0.04)
#' traject <- gettraject(answer$R1,projn=5)
#' newtraj <- doproject(traject,constC=150)
#' head(newtraj)
#' trajdepl <- makedeplet(newtraj)
#' plotconstC(trajdepl,endyear=2017,constC=150,target=0.40)
gettraject <- function(inR1,projn=0) {  # inR1=answer$R1; projn=0
   rK <- inR1$itheta
   goodbad <- inR1$elltot
   biomass <- inR1$biomass
   startbd <- as.numeric(dimnames(biomass)[[3]])
   ok <- rowSums(goodbad)
   pick <- which(ok > 0)
   biom <- biomass[pick,,]
   good <- goodbad[pick,]
   rKok <- rK[pick,]
   final <- dim(biom)[2]
   nok <- length(pick)
   ntraject <- sum(ok)
   numcols <- final + projn + 3
   yrs <- as.numeric(dimnames(biom)[[2]])
   if (projn > 0) {
      projyrs <- (yrs[final]+1):(yrs[final] + projn)
      yrsplus <- c(yrs,projyrs)
   } else {
      yrsplus <- yrs
   }
   traject <- matrix(0,nrow=ntraject,ncol=numcols)
   rownames(traject) <- 1:ntraject
   colnames(traject) <- c(yrsplus,"r","K","bd")
   count <- 0
   for (i in 1:nok) { # pull out all the accepted biomass trajectories
      usetraj <- which(good[i,] > 0)
      ntraj <- length(usetraj)
      pickrow <- (count + 1):(count + ntraj)
      trajs <- as.matrix(biom[i,,usetraj])
      depl <- startbd[usetraj]
      userK <- rKok[i,1:2]
      for (j in 1:ntraj) {
         if (projn > 0) {
            traject[pickrow[j],] <- c(trajs[,j],rep(NA,projn),userK,depl[j])
         } else {
            traject[pickrow[j],] <- c(trajs[,j],userK,depl[j])
         }
      }
      count <- count + ntraj
   }
   return(traject)
} # end of gettraject

#' @title plotcMSY6 plots out a summary of the Catch-MSY results in 6 graphs
#'
#' @description plotcMSY6 generates 6 graphs illustrating the array of rK
#'     parameter combinations and whether they were successful or not. That
#'     plot is coloured by how many trajectories across the initial depletion
#'     range were successful.
#'
#' @param cMSY the list from running summcMSY on the output from run_cMSY
#' @param catch the catch in each year
#' @param label simply a text label for the y-axes; default = NA
#'
#' @return nothing, but it does generate a plot to the screen
#' @export
#'
#' @examples
#' \dontrun{
#' data(invert)
#' fish <- invert$fish
#' glb <- invert$glb
#' reps <- 5000  # one would run at least 20000, preferably more
#' answer <- run_cMSY(fish,glb,n=reps,sigpR=0.04)
#' summcMSY <- summarycMSY(answer,fish,final=FALSE)
#' plotcMSY6(summcMSY,fish[,"catch"],label=glb$spsname)  
#' summcMSY <- summarycMSY(answer,fish,final=TRUE)
#' str(summcMSY,max.level=1)
#' plotcMSY6(summcMSY,fish[,"catch"],label=glb$spsname)
#' }
plotcMSY6 <- function(cMSY,catch,label=NA) {
   meanmsy <- cMSY$meanmsy
   meanr <- cMSY$meanr
   meanK <- cMSY$meanK
   r <- cMSY$r
   K <- cMSY$K
   years <- cMSY$years
   start_r <- cMSY$parbound$r
   start_k <- cMSY$parbound$k
   par(mfcol=c(2,3))
   if (is.na(label)) {
      par(mai=c(0.425,0.4,0.1,0.05),oma=c(0.0,0.0,0.0,0)) 
   } else {
      par(mai=c(0.425,0.4,0.1,0.05),oma=c(0.0,0.0,1.0,0))
   }
   par(cex=0.85, mgp=c(1.5,0.35,0), font.axis=7,font.lab=7,font=7)
   ##  Plot 1: catch by year
   ymax <- max(catch,meanmsy[4,],na.rm=TRUE)* 1.15
   plot(years, catch, type="l", ylim = c(0, ymax), xlab = "Year",
        ylab = "Catch",lwd=2)
   abline(h=meanmsy[2,"Mean"],col=2, lwd=2)
   abline(h=meanmsy[4,],col=2,lwd=1)
   mtext(round(meanmsy[2,"Mean"],3),side=3,outer=FALSE,line=-1.0,font=7,cex=0.85)
   ##  Plot 2: distribution of r
   hist(r, breaks=20, xlim=c(0, 1.1 * max(r)), main = "",col="grey",
        xlab="Successful r")
   abline(v=meanr[2,"Mean"],col=2,lwd=2)
   abline(v=meanr[4,],col="red")
   ##  Plot 3: plot unsuitable combinations
   plot(cMSY$pickC$rnot, cMSY$pickC$knot, pch=1,cex=0.5,col=2,xlim=start_r,ylim=start_k,
        xlab="r", ylab="K")
   ## plot plausible combinations
   points(r[cMSY$pickC$pickblk],K[cMSY$pickC$pickblk],pch=16,cex=0.75,col=1)
   points(r[cMSY$pickC$pickblu],K[cMSY$pickC$pickblu],pch=16,cex=0.75,col=4)
   points(r[cMSY$pickC$pickyel],K[cMSY$pickC$pickyel],pch=16,cex=0.75,col=7)
   points(r[cMSY$pickC$pickmax],K[cMSY$pickC$pickmax],pch=16,cex=1.25,col=3)
   abline(v=meanr["Geometric","Mean"],col=7,lwd=1)
   abline(h=meanK["Geometric","Mean"],col=7,lwd=1)
   ##  Plot 4: distribution of K
   # hist(K, breaks=20, xlim=c(0, 1.1 * max(K)), xlab="Successful K",
   #      main = "", col="grey")
   hist(K, breaks=20, xlab="Successful K",main = "", col="grey")
   abline(v=meanK[2,"Mean"],col=2,lwd=2)
   abline(v=meanK[4,],col="red")
   ## Plot 5: plot unsuitable combinations on log scale
   plot(log(cMSY$pickC$rnot), log(cMSY$pickC$knot),pch=1,cex=0.5,col=2,
        xlab="ln(r)",ylab="ln(k)")
   ## plot plausible combinations on log scale
   points(log(r),log(K),pch=16,cex=1.0,col=1)
   points(log(r[cMSY$pickC$pickblu]),log(K[cMSY$pickC$pickblu]),pch=16,cex=0.75,col=4)
   points(log(r[cMSY$pickC$pickyel]),log(K[cMSY$pickC$pickyel]),pch=16,cex=0.75,col=7)
   abline(v=mean(log(r)),col=7,lwd=1)
   abline(h=mean(log(K)),col=7,lwd=1)
   ##  Plot 6: distribution of MSY
   hist(cMSY$msy, breaks=20, xlim=c(0, 1.2 * max(cMSY$msy)), xlab="MSY",main = "",
        col="grey")
   abline(v=meanmsy[2,"Mean"],col=2, lwd=2)
   abline(v=meanmsy[4,],col="red",lwd=1)
   if (!is.na(label)) mtext(label,side=3,outer=T,line=0.0,font=7,cex=1.0)
} # end of plotcMSY6


#' @title plottrajectory literally plots predicted trajectories
#'
#' @description plottrajectory plots out the predicted trajectories from
#'     those paramter combinations that have been accepted. In addition, and
#'     more importantly, it identifies those trajectories that succeeded and
#'     puts them into a smaller matrix than the complete set of trialed rK
#'     combinations and each of the successful trajectories. This can project 
#'     a limited number of trajectories, determined by oneplot and plotall. If
#'     oneplot is true it does not matter what is in plotall, all trajectories
#'     are given in a single plot along with the median values in red. Similarly 
#'     for the harvest rate
#'
#' @param inR1 the output from run_cMST
#' @param years the vector of years
#' @param catch the vector of catches per year
#' @param inparbound the set of parameter vectors that were successful

#' @param scalar literally scales the catch to the same units as biomass;
#'     defaults to 1000 so as to convert Kg to tonnes.
#' @param Bmax Deprecated. No longer used. Remove from any code will be depleted
#'     from later iterations of datalowSA 
#' @param oneplot Plot all trajectories on top of each other rather than
#'     individually. defaults to TRUE.
#' @param plotout produce a plot or not? Defaults to TRUE
#' @param plotall when oneplot=FALSE how many plots to generate; defaults to
#'     7, which plots the successful trajectories from the first seven r-K pairs
#'     that had successful outcomes. Useful numbers are 7 and 15 as the total
#'     catch history is also illustrated along with the mean MSY to aid in 
#'     understandig the trajectories. To see all trajectories set plotall=TRUE.
#'
#' @return a list of the yes/no vector and of the accepted rK pairs. This is
#'     returned invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#'  data(invert)
#'  fish <- invert$fish 
#'  glb <- invert$glb 
#'  reps <- 5000  # one would run at least 20000, preferably more 
#'  answer <- run_cMSY(fish,glb,n=reps,sigpR=0.04) 
#'  summcMSY <- summarycMSY(answer,fish,final=TRUE) 
#'  # plotprep(width=8,height=5,newdev=FALSE)
#'  out <- plottrajectory(answer$R1,fish$year,fish$catch,answer$parbound,
#'                       oneplot=FALSE,scalar=1.0,plotout=TRUE,plotall=7)
#' }  #
plottrajectory <- function(inR1,years,catch,inparbound,scalar=1000.0,Bmax=2,
                           oneplot=TRUE,plotout=TRUE,plotall=7) {
#   inR1=answer$R1;years=fish$year;catch=fish$catch;inparbound=answer$parbound; Bmax=2
#   oneplot=TRUE;scalar=1.0;plotout=FALSE;plotall=7
   medianB <- NULL
   medianH <- NULL
   nyr <- length(years)
   itheta <- inR1$itheta
   elltot <- inR1$elltot
   biomass <- inR1$biomass/scalar
   yearsplus <- as.numeric(names(biomass[1,,1])) 
   nyrp <- length(yearsplus)
   ok <- rowSums(elltot)
   pick <- which(ok > 0)
   ell <- elltot[pick,]
   okP <- ok[pick]
   Bmax <- 2
   colour <- numeric(length(pick))
   colour[okP >= quantile(okP,probs=0.95)] <- 4
   colour[okP < quantile(okP,probs=0.95)] <- 3
   colour[okP <= quantile(okP,probs=0.50)] <- 2
   colour[okP <= quantile(okP,probs=0.25)] <- 1
   rows <- as.numeric(rownames(ell))
   rK <- itheta[pick,1:2]
   rK <- cbind(rK,ok[pick],rK[,1]*rK[,2]/4,colour) #(trunc(ok[pick]/5)+1))
   rownames(rK) <- rows
   colnames(rK) <- c("r","K","Freqok","MSY","Colour")
   succB <- NA; succH <- NA
   if (oneplot) {
      pickbt <- which(ell[1,] > 0)
      totrow <- sum(okP)
      succB <- matrix(NA,nrow=totrow,ncol=length(yearsplus),
                      dimnames=list(1:totrow,yearsplus))
      nell <- length(ell[,1])
      count <- 1
      for (i in 1:nell) {
         pickbt <- which(ell[i,] > 0)
         lenbt <- length(pickbt)
         for (j in 1:lenbt) {
            succB[count,] <- biomass[rows[i],,pickbt[j]]
            count <- count + 1
         }
      }
      count <- count - 1
      medianB <- apply(succB,2,median,na.rm=TRUE)
      ymax <- getmaxy(succB)
      if (plotout) {
         par(mfrow=c(2,1))
         par(mai=c(0.25,0.3,0.1,0.05),oma=c(0.0,1.0,0,0))
         par(cex=0.85, mgp=c(1.5,0.35,0), font.axis=7,font=7)
         plot(yearsplus,succB[1,],type="l",lwd=1,ylim=c(0,ymax),
              yaxs="i",xlab="",ylab="",col="grey",panel.first=grid())
         for (i in 1:count)  
                     lines(yearsplus,succB[i,],lwd=1,col="grey") 
         lines(yearsplus,medianB,lwd=2,col=2)
         mtext("Predicted Biomass",side=2,line=1.25,outer=F,font=7,cex=1.0)
      }
      # Second plot of harvest rates
      harvest <- biomass * 0.0
      for (i in 1:nell) {
         pickbt <- which(ell[i,] > 0)
         lenbt <- length(pickbt)
         for (j in 1:lenbt) {
            harvest[rows[i],1:nyr,pickbt[j]] <- 
               (catch/scalar)/biomass[rows[i],1:nyr,pickbt[j]]
         }
      }
      succH <- matrix(NA,nrow=totrow,ncol=nyrp,
                      dimnames=list(1:totrow,yearsplus))
      pickbt <- which(ell[1,] > 0)
      count <- 1         
      for (i in 1:nell) {
         pickbt <- which(ell[i,] > 0)
         lenbt <- length(pickbt)
         for (j in 1:lenbt) {
            succH[count,] <- c(harvest[rows[i],1:nyr,pickbt[j]],NA)
            count <- count + 1
         }
      } 
      if (plotout) {         
         nH <- dim(succH)[1]
         ymax <- max(getmaxy(succH),0.55)
         plot(yearsplus,succH[1,],type="l",lwd=1,
              ylim=c(0,ymax),yaxs="i",xlab="",ylab="",col="grey",
              panel.first=grid())
         for (i in 1:nH) lines(yearsplus,succH[i,],lwd=1,col="grey")  
      }
      medianH <- apply(succH,2,median,na.rm=TRUE)
      if (plotout) {
         lines(yearsplus,medianH,lwd=2,col=2)
         abline(h=0.5,col=3,lwd=1)
         mtext("Harvest Rate",side=2,line=1.25,outer=F,font=7,cex=1.0)
      }
   }  else if (plotout) {
      if (plotall == 15) { par(mfrow=c(4,4)) } else {  par(mfrow=c(2,4)) }
      par(mai=c(0.25,0.3,0.1,0.05),oma=c(0.0,1.0,0,0))
      par(cex=0.85, mgp=c(1.5,0.35,0), font.axis=7,font=7)
      plotot <- length(ell[,1])
      if (is.numeric(plotall)) plotot <- min(plotall,plotot)
      ymax <- max(biomass[rows[1:plotot],,],na.rm=T)*1.05
      for (i in 1:plotot) {  # i = 1
         pickbt <- which(ell[i,] > 0)
         lenbt <- length(pickbt)
         label <- paste(round(itheta[rows[i],1],4),round(itheta[rows[i],2],0),sep="_")
         yearsplus <- as.numeric(names(biomass[rows[1],,pickbt[1]]))
         plot(yearsplus,biomass[rows[i],,pickbt[1]],type="l",lwd=2,ylim=c(0,ymax),
              yaxs="i",xlab="",ylab="",col=1,panel.first=grid())
         if (lenbt > 1) {
            for (j in 2:lenbt) {
               lines(yearsplus,biomass[rows[i],,pickbt[j]],lwd=2,col=1)
            }
         }
         mtext(label,side=2,line=1.1,outer=F,font=7,cex=0.85)
         msy <- itheta[rows[i],1]*itheta[rows[i],2]/4.0
         mtext(round(msy,3),side=3,line=-1.0,outer=F,font=7,cex=0.85)
      }
      ymax <- max(catch/scalar)*1.15
      plot(years,catch/scalar,type="l",lwd=2,ylim=c(0,ymax),yaxs="i",xlab="",
           ylab="Catch",col=2,panel.first=grid())
      msy <- exp(mean(log(rK[,1]*rK[,2]/4)))
      abline(h=msy/scalar,col=4,lwd=2)
      mtext(round(msy,3),side=3,line=-1.0,outer=F,font=7,cex=0.85)
   }
   return(invisible(list(ell=ell,rK=rK,medianB=medianB,medianH=medianH,
                         succB=succB,succH=succH)))
} # end of plottrajectory


#' @title plotconstC summarizes constant catch projections in catch-MSY
#' 
#' @description plotconstC plots and summarizes the outcome of constant 
#'     catch projections made on each of the successful trajectories from the 
#'     catch-MSY analysis. The catch-MSY analysis provides an uncertain estimate 
#'     of MSY and of final depletion. But by conducting constant catch
#'     projectios the implications in terms of what proportion of trajectories 
#'     increase and what proportion decrease can aid decisions and could form 
#'     he basis for the development of formal harvest control rules. It will be
#'     noticed that a large proportion of trajectories can be below 20%, but
#'     notice also that at the same time a high proportion can be above 48%, 
#'     assuming that this Commonwealth default target is used. It is the case 
#'     that a target of 0.4 or 40% has been suggested for non-primary target
#'     species, as a means of preventing the management of such species from 
#'     prevent the primary economic drivers of a fishery from being caught.
#'
#' @param deplete the output of 'gettraject', a matrix of biomass trajectories
#' @param endyear the final year of the known catches and biomass trajectories
#' @param constC the constant catch applied to generate the proections
#' @param limit biomass depletion level used as a limit for the species
#' @param target the depletion level used as a target for the species concerned.
#' @param console a boolean determinng whether the projection years results are
#'     printed to the console
#' @param intensity defaults to NA but if contains a value this is the density
#'     of trajectories that lead to full colour
#' @param contours default to TRUE, should the 80% and 90% quantile contours 
#'     be plotted?
#'
#' @return a matrix of all years with the proportion < 20%, the proportion > 48% 
#'     (the input target), the mean and median depletion, and the proportion of 
#'     trajectories that were increasing relative to the endyear of data.
#' @export
#'
#' @examples
#' \dontrun{
#' data(invert)
#' fish <- invert$fish
#' glb <- invert$glb
#' reps <- 10000  # one would run at least 20000, preferably more
#' answer <- run_cMSY(fish,glb,n=reps,sigpR=1e-8)
#' traject <- gettraject(answer$R1,projn=5)
#' newtraj <- doproject(traject,constC=300)
#' trajdepl <- makedeplet(newtraj)
#' plotconstC(trajdepl,endyear=2017,constC=300,limit=0.2,target=0.40)
#' }
plotconstC <- function(deplete,endyear,constC=0.0,limit=0.2,target=0.48,
                       console=TRUE,intensity=NA,contours=TRUE) {  
   getltX <- function(invect,inlim) return(length(which(invect < inlim)))
 #  deplete=ans$deplet;endyear=2017;constC=0
 #  limit=0.2;target=0.48; console=TRUE;intensity=NA;contours=TRUE
   nyrs <- dim(deplete)[2] - 3
   onlytraj <- deplete[,1:nyrs]
   yrs <- as.numeric(colnames(onlytraj))
   pickyr <- which(yrs == endyear)
   Ntraj <- dim(onlytraj)[1]
   med <- apply(onlytraj,2,median,na.rm=TRUE)
   av <- apply(onlytraj,2,mean,na.rm=TRUE)   
   par(mfrow=c(1,1),mai=c(0.45,0.45,0.05,0.05),oma=c(0.0,0,0.0,0.0)) 
   par(cex=0.85, mgp=c(1.35,0.35,0), font.axis=7,font=7,font.lab=7) 
   ymax <- getmaxy(onlytraj)
   plot(yrs,onlytraj[1,],type="n",xlab="Year",ylab="Depletion",ylim=c(0,ymax),
        yaxs="i",panel.first=grid())
   if (is.na(intensity)) {
      for (i in 1:Ntraj) 
         lines(yrs,onlytraj[i,],col="grey",lwd=1)
   } else {
      for (i in 1:Ntraj) 
         lines(yrs,onlytraj[i,],col=rgb(0.3,1,0.5,1/intensity),lwd=1)      
   }
   contour <- apply(onlytraj,2,function(x) quantile(x,probs=c(0.05,0.1,0.9,0.95)))
   lines(yrs,med[1:nyrs],col=1,lwd=2)
   lines(yrs,av[1:nyrs],col=4,lwd=2)
   if (contours) for (i in 2:3) lines(yrs,contour[i,],col=2,lty=2)
   abline(h=c(limit,target),col=c(2,2))
   abline(v=endyear,col=3)
   legend(trunc(mean(yrs)),0.95*ymax,c("Mean","Median"),
          col=c(4,1),lwd=3,bty="n",cex=1.0) 
   text(trunc(mean(yrs)),0.8*ymax,paste0("ConstCatch = ",constC),
        cex=1.0,font=7,pos=4)

   if (pickyr < nyrs) {
      select <- pickyr:nyrs
   } else {
      select <- (pickyr - 4):pickyr
   }
   columns <- c("Year","PltLim%","PgtTarg%","Mean","Median","Pincrease")
   result <- matrix(0,nrow=nyrs,ncol=length(columns),dimnames=list(yrs,columns))
   for (i in 1:nyrs) {
      nlt02 <- getltX(onlytraj[,i],inlim=limit)
      nlt48 <- getltX(onlytraj[,i],inlim=target)
      compdepl <- onlytraj[,i]/onlytraj[,pickyr]
      pGT <- length(compdepl[compdepl > 1.0])/Ntraj 
      result[i,] <- c(yrs[i],nlt02/Ntraj,(1-nlt48/Ntraj),
                      av[i],med[i],pGT)
   }
   if (console) print(result[select,])
   return(invisible(result))
} # end of plotconstC


#' @title trendMSY calculates the  mean MSY per K class
#' 
#' @description trendMSY subdivides the range of the successful K values
#'     into a set of classes of width 'inc' and for each class calculates the
#'     mean (geometric mean) of the MSY for the subset of r and K values. This
#'     enables the central value of MSY to be plotted on the scatter of 
#'     successful points.
#'
#' @param inr the set of r values to be tested, derives from summarycMSY
#' @param inK the set of K values to be tested, derives from summarycMSY
#' @param inc the class width of the K classes
#'
#' @return a matrix of r, K and MSY values
#' @export
#'
#' @examples
#' \dontrun{
#'  data(invert)
#'  fish <- invert$fish 
#'  glb <- invert$glb 
#'  reps <- 5000  # one would run at least 20000, preferably more 
#'  answer <- run_cMSY(fish,glb,n=reps,sigpR=0.04) 
#'  summcMSY <- summarycMSY(answer,fish,final=TRUE) 
#'  trendMSY(summcMSY$r,summcMSY$K,inc=200) 
#' }
trendMSY <- function(inr,inK,inc=100){  # inr=r; inK=K
   bounds <- range(inK)
   begin <- trunc(bounds[1]/inc)*inc
   finish <- trunc((bounds[2]+inc)/inc)*inc
   Kclass <- seq(begin,finish,inc)
   Kcenter <- seq((begin+inc/2),finish,inc)
   nclass <- length(Kcenter)
   columns <- c("Kcenter","N","MSY","rcenter")
   meanmsy <- matrix(0,nrow=nclass,ncol=length(columns),
                     dimnames=list(Kcenter,columns))
   for (i in 1:nclass) {  #  i=1
      pick <- which((inK >= Kclass[i]) & (inK < Kclass[(i+1)]))
      num <- length(pick)
      if (num > 0) {
         pickK <- inK[pick]; pickr <- inr[pick]
         averagemsy <- exp(mean(log((pickr * pickK/4))))
         meanr <- mean(pickr)
         meanmsy[i,] <- c(Kcenter[i],num,averagemsy,meanr)
      } else {
         meanmsy[i,] <- c(0,0,0,0)
      }  
   }
   return(meanmsy)
} # end of trendMSY


#' @title makecMSYdat generates a cMSY dataset out of other data files
#'
#' @description makecMSYdat expects to find in the input
#'     data.frame or matrix, columns containing 'Year', and 'Total' 
#'     (meaning total removals = catch + discards), and 'Resilience' 
#'     being any of 'verylow', 'low', 'medium', or 'high'. 
#'
#' @param indat the tier 4 datafile for one species
#' @param spsname the name of the species to be analysed
#' @param yearcol the column name of the year data
#' @param catchcol the column namae of the total catch data
#' @param resil the value of resilience for the species as a character of
#'     either "verylow", "low", "medium", or "high". Note the lower case,
#'     defaults to "low"
#'
#' @return a list containing a matrix of year and catch, and a list of the
#'     resilience and spsname
#'
#' @export
#'
#' @examples
#' \dontrun{
#' print("read the csv file containing the general data into 'dat'")
#' print("then run  makecMSYdat(dat,'spsname')")
#' }
makecMSYdat <- function(indat,spsname="",yearcol="Year",catchcol="Catch",resil="low") { 
   glb <- list(resilience=resil[1],spsname=spsname)
   N <- dim(indat)[1]
   fish <- as.data.frame(cbind(indat[,yearcol],indat[catchcol]))
   rownames(fish) <- c(1:N)
   colnames(fish) <- c("year","catch")
   return(list(fish=fish,glb=glb,props=NULL,agedata=NULL,lendata=NULL))
}  # end of makecMSYdat


