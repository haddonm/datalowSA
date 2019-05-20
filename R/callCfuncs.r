

#' @title getProductionC estimates MSY from output of ASPM
#'
#' @description getProductionC takes the optimum estimate of R0 from ASPM and
#'     estimates the production curve, from which it gains the MSY, Bmsy, Hmsy,
#'     and Dmsy (depletion at MSY). It generate the production curve by stepping
#'     through the dynamics of the fishery over a series of constant harvest
#'     rates for however many iterations of nyr required. It is identical to
#'     getProduction but uses an Rcpp routine to speed up the iterative steps
#'     by about 26 times. That is very useful when conducting bootstraps
#'
#' @param inR0 the optimum estimate of R0 from ASPM
#' @param infish the fish object from a data set
#' @param inglb the glb object from a data set
#' @param inprops the props object from a data set
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
#' pop1 <- unfished(glb,props,exp(glb$R0))
#' B0 <-pop1$B0
#' M <- glb$M
#' nyrs <- 30
#' years <- 0:nyrs
#' pars <- c(13.6,0.3)
#' bestL <- optim(par,aspmLL,method="Nelder-Mead",infish=fish,inglb=glb,
#'                inprops=props,
#'                control=list(maxit = 1000, parscale = c(10,0.1)))
#' prod <- getProduction(exp(bestL$par[1])infish,inglb,inprops,
#'                       Hrg=c(0.0005,0.07,0.0005),nyr=50)
#' print(prod)
#' }
getProductionC <- function(inR0,infish,inglb,inprops,
                           Hrg=c(0.025,0.4,0.025),nyr=50,maxiter=3) {
   maxage <- inglb$maxage; M <- inglb$M;  steep <- inglb$steep;
   maa <- inprops$maa; waa <- inprops$waa
   sela <- inprops$sela
   surv <- exp(-M)
   hS <- exp(-M/2)
   B0 <- getB0(inR0,inglb,inprops)
   global <- c(steep,inR0,B0)   
   getyield <- function(NAA,hrate) {  #  NAA=Nt; hrate=hrange[hnum]
      for (iter in 1:maxiter) {  # iterate until equilibrium yield reached
         ans <- fishNAAC(nyr,maxage,hS,hrate,global,maa,waa,sela,NAA)
         NAA <- ans[[1]]
         Ct <- ans[[2]]
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
   for (hnum in 1:nH) # hnum=1
      production[(hnum+1),2:4] <- getyield(Nt,hrange[hnum])
   production[,"Depletion"] <- production[,"SpawnB"]/production[1,"SpawnB"]
   return(as.data.frame(production))
} # end of getProductionC

