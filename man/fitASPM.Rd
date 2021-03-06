% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aspm.r
\name{fitASPM}
\alias{fitASPM}
\title{fitASPM fits an age-structured production model}
\usage{
fitASPM(initpar, infish, inglb, inprops, callfun = aspmLL)
}
\arguments{
\item{initpar}{a vector of 2 or 3 numbers that are the initial parameter
values given to the estimate of logR0, and the estimate of the variation
around the CPUE data that the model is to be fitted to, and finally, the
initial depletion.}

\item{infish}{the fish data.frame from readdata or built in dataset}

\item{inglb}{the glb data.frame from readdata or built in dataset}

\item{inprops}{the props data.frame from readdata or built in dataset}

\item{callfun}{the negative log-likelihoood used; either aspmLL or aspmPENLL}
}
\value{
a list containing the optim output
}
\description{
fitASPM fits an age-structured produciton model that only has
    a single parameter - the unfished recruitment level R0.
}
\examples{
\dontrun{
data(dataspm)
fish <- dataspm$fish
glb <- dataspm$glb
props <- dataspm$props
pars <- c(14,0.3)
aspmLL(pars,fish,glb,props)      # should be -2.277029
bestspm <- fitASPM(pars,infish=fish,inglb=glb,inprops=props)
bestspm
fishery <- dynamics(bestspm$par,fish,glb,props)
round(fishery,4)
}
}
