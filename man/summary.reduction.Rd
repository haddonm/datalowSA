% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/asmreduct.r
\name{summary.reduction}
\alias{summary.reduction}
\title{summary.reduction S3 summary method for a reduction object}
\usage{
\method{summary}{reduction}(object, ...)
}
\arguments{
\item{object}{matrix output from plotreduction}

\item{...}{other arguments}
}
\value{
a list containing the range of differences between the
    first and last year of projection and the gradient between the
    last year and half the projection years.
}
\description{
summary.reduction an  S3 summary method for an asmreduction 
    object
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
  reduct <- asmreduction(inR0,fish,glb,props,limitH=limitH,projyr=10,
                          constC=200)
  x <-  plotreduction(reduct,defineplot=FALSE)
  summary(x)
}
}
