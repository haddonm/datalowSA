% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/callCfuncs.r
\name{getProductionC}
\alias{getProductionC}
\title{getProductionC estimates MSY from output of ASPM}
\usage{
getProductionC(
  inR0,
  infish,
  inglb,
  inprops,
  Hrg = c(0.025, 0.4, 0.025),
  nyr = 50,
  maxiter = 3
)
}
\arguments{
\item{inR0}{the optimum estimate of R0 from ASPM}

\item{infish}{the fish object from a data set}

\item{inglb}{the glb object from a data set}

\item{inprops}{the props object from a data set}

\item{Hrg}{the desired sequence of harvest rates to be used to define the
production curve. The default is c(0.025,0.4,0.025), but it is a good
idea to plot teh harvest rate against yield and manually adjust the
sequence values to focus attention and detail on where the MSY falls.}

\item{nyr}{The number of years making up a single iteration while searching
for the equilibrium yield. default = 50.}

\item{maxiter}{defaults to 3. Determines how many runs through the nyr
steps are conducted to reach equilibrium. One can trial different values
but 3 is a reasonable compromise. It is best to test different maxiter
values to ensure equilibria are reached in each yield estimate.}
}
\value{
a data.frame containing the sequence of harvest rates, the related
    spawning biomass, the exploitable biomass, the yield, and depletion.
}
\description{
getProductionC takes the optimum estimate of R0 from ASPM and
    estimates the production curve, from which it gains the MSY, Bmsy, Hmsy,
    and Dmsy (depletion at MSY). It generate the production curve by stepping
    through the dynamics of the fishery over a series of constant harvest
    rates for however many iterations of nyr required. It is identical to
    getProduction but uses an Rcpp routine to speed up the iterative steps
    by about 26 times. That is very useful when conducting bootstraps
}
\examples{
\dontrun{
data(fishdat)
fish <- fishdat$fish
glb <- fishdat$glb
props <- fishdat$props
pop1 <- unfished(glb,props,exp(glb$R0))
B0 <-pop1$B0
M <- glb$M
nyrs <- 30
years <- 0:nyrs
pars <- c(13.6,0.3)
bestL <- optim(par,aspmLL,method="Nelder-Mead",infish=fish,inglb=glb,
               inprops=props,
               control=list(maxit = 1000, parscale = c(10,0.1)))
prod <- getProduction(exp(bestL$par[1])infish,inglb,inprops,
                      Hrg=c(0.0005,0.07,0.0005),nyr=50)
print(prod)
}
}
