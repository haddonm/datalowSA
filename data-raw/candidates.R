

library(rutilsMH)
library(datalowSA)

data(invert)
fish <- invert$fish
glb <- invert$glb



#  n=10000;incB=0.025;sigpR=0.025;multK=1.0;finaldepl=NA;start_k=NA 
#   start_r=NA;initdepl=NA;maximumH=1.0; indat=fish; glob=glb; Hyear = NA



run_cMSY <- function(indat,glob,n=10000,incB=0.025,
                     sigpR=0.025,multK=1.0,finaldepl=NA,start_k=NA,
                     start_r=NA,initdepl=NA,maximumH=1.0,Hyear=NA) {
  
  
    n=100000;incB=0.025;sigpR=0.025;multK=1.0;finaldepl=c(0.05,0.7);start_k=NA 
     start_r=NA;initdepl=NA;maximumH=1.0; indat=fish; glob=glb; Hyear = NA
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
  columns <- c("r","K","initdepl","initB","msy")
  numcol=length(columns)
  param <- matrix(0,nrow=n,ncol=numcol,dimnames=list(1:n,columns))
  rpar <- runif(n,min=parbound$r[1],max=parbound$r[2])
  Kpar <- runif(n,min=parbound$k[1],max=parbound$k[2])
  initdepl <- runif(n,min=parbound$initdepl[1],max=parbound$initdepl[2])
  initB <- Kpar * initdepl
  msy <- (rpar * Kpar)/4
  param <- cbind(rpar,Kpar,initdepl,initB,msy)
  years <- fish[,"year"]
  nyrs <- length(years)
  
  years <- c(years,(years[nyrs]+1))
  nyrs1 <- nyrs + 1
  
  Hmax <- maximumH # extreme value
  Kmult <- 1.1
  catch <- fish[,"catch"]
  biomass <- matrix(0,nrow=n,ncol=nyrs1,dimnames=list(1:n,years))
  biomass[,1] <- initB
  for (i in 1:nyrs) 
    biomass[,(i+1)] <- biomass[,i] + rpar*biomass[,i] * (1 - (biomass[,i]/Kpar)) - catch[i]
  biomass <- biomass[,1:nyrs]
  dim(biomass)
#  pickneg <- which(apply(biomass,1,countgtzero) != nyrs)
  b1 <- keepval(biomass,Kpar,catch,finaldepl,multK,Hmax)
  
} # end of run_cMSY 
  


keepval <- function(biomass,Kpar,catch,finaldepl,multK,Hmax) {
  nyrs <- ncol(biomass)
  pickneg <- which(biomass[,nyrs] < 0)
  if (length(pickneg) > 1) {
    biomass2 <- biomass[-pickneg,]
    Kpar2 <- Kpar[-pickneg]
  } else {
    biomass2 <- biomass
    Kpar2 <- Kpar
  }
  findepl <- biomass2[,nyrs]/Kpar2 
  pickfindep <- which((findepl < finaldepl[1]) | (findepl > finaldepl[2]))
  if (length(pickfindep) > 1) {
    biomass3 <- biomass2[-pickfindep,]
    Kpar3 <- Kpar2[-pickfindep]
  } else {
    biomass3 <- biomass2
    Kpar3 <- Kpar2
  }
  testK <- ((Kpar3 * multK) - biomass3)
  pickKM <- which(apply(testK,1,countgtzero) != nyrs)
  if (length(pickKM) > 1) {
    biomass4 <- biomass4[-pickKM,]
    Kpar4 <- Kpar3[-pickKM]
  } else {
    biomass4 <- biomass3
    Kpar4 <- Kpar3
  }
  harvestR <- biomass4
  newn <- nrow(biomass4)
  for (i in 1:nyrs) harvestR[,i] <- catch[i] / biomass4[,i]
  pickH <- which(apply(harvestR,2,max) > Hmax)
  if (length(pickH) > 0) {
    biomass5 <- biomass4[-pickH,]
  } else { 
    biomass5 <- biomass4
  }   
  rown <- 1:n
  row5 <- as.numeric(rownames(biomass5))
  pickkeep <- match(row5,rown)
  return(list(biomass=biomass5,pickkeep=pickkeep))
}





library(microbenchmark)

microbenchmark(
  b1 <- keepval(biomass,Kpar,catch,finaldepl,multK,Hmax),
  times=20
)







  
  
{    # the old run_cMSY  
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


n=100000
years <- fish[,"year"]
nyrs <- length(years)

years <- c(years,(years[nyrs]+1))
nyrs1 <- nyrs + 1










pickR <- which((biomass[,nyrs] > 1) & (biomass[,nyrs] < Kpar*Kmult))
reject <- biomass[-pickR,]
biomass2 <- biomass[pickR,]
dim(reject)
dim(biomass2)
rpar2 <- rpar[pickR]
Kpar2 <- Kpar[pickR]
initdepl2 <- initdepl[pickR]
msy2 <- msy[pickR]
param2 <- param[pickR,]

max_K1a <- min(Kpar2[(rpar2 <(1.1*parbound$r[1]))])
max_K1b <- max(Kpar2[(msy2 < mean(msy2))])
max_K1 <- min(max_K1a,max_K1b)
pickK <- which(Kpar2 <= max_K1)
Kpar3 <- Kpar2[pickK]
rpar3 <- rpar2[pickK]
initdepl3 <- initdepl2[pickK]
param3 <- param2[pickK,]
msy3 <- msy2[pickK]
biomass3 <- biomass2[pickK,]

pickU <- which(Kpar < 10000)

plotprep(width=8,height=6,newdev=FALSE)
parset()
plot1(rpar[pickU],Kpar[pickU],type="p",maxy=10000)
#points(rpar2,Kpar2,pch=16,col=2)
points(rpar3,Kpar3,pch=1,col=3)


plotprep(width=8,height=6,newdev=FALSE)
parset(plots=c(2,2))
hist(msy,xlim=c(0,6500),main="")
hist(rpar3,main="")
hist(msy3,xlim=c(0,6500),main="")
hist(Kpar3,main="")


sortorder <- order(param3[,"initdepl"],decreasing=TRUE)

hist(msy,breaks=20)



answer <- run_cMSY(indat,glob,n=10000,incB=0.025,
                   sigpR=0.025,multK=1.0,finaldepl=NA,start_k=NA,
                   start_r=NA,initdepl=NA,maximumH=1.0,Hyear=NA)













