% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/asmreduct.r
\name{projstats}
\alias{projstats}
\title{projstats}
\usage{
projstats(inreduct)
}
\arguments{
\item{inreduct}{the list object generated by asmreduction,
containing the result of the age-structured stock reduction}
}
\value{
a matrix of depletion values a tthe start and end of the
    projection period and their difference.
}
\description{
projstats
}
\examples{
\dontrun{
  data(orhdat1)
  fish <- orhdat1$fish # 19 years of data
  glb <- orhdat1$glb
  props <- orhdat1$props
  inR0 <- seq(12.0,14.2,0.02)
  limitH <- c(0.25,0.5)
  glb$M <- 0.032
  glb$steep <- 0.6
  reduct <- asmreduction(inR0,fish,glb,props,limitH=limitH)
  projstats(reduct)
  reduct2 <- asmreduction(inR0,fish,glb,props,limitH=limitH,projyr=10,
                          constC=200)
  projstats(reduct2) # search for small differences and low gradients 
}
}
