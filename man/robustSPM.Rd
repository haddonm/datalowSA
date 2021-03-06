% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spm.r
\name{robustSPM}
\alias{robustSPM}
\title{robustSPM conducts a robustness test on the quality of fit of an SPM}
\usage{
robustSPM(
  inpar,
  fish,
  glb,
  N = 10,
  scaler = 15,
  console = TRUE,
  schaefer = TRUE
)
}
\arguments{
\item{inpar}{the parameter set to begin the trials with}

\item{fish}{the fisheries data: at least year, catch, and cpue}

\item{glb}{the global variables containing the spsname}

\item{N}{the number of random trials to run; defaults to 10, which is too few}

\item{scaler}{the divisor that sets the degree of normal random variation to 
add to the parameter values; default = 15 the smaller the value the more
variable the outcome}

\item{console}{print summary statistics to the screen? default = TRUE}

\item{schaefer}{default = TRUE, which sets the analysis to the Schaefer
model. setting it to FALSE applies the Fox model instead}
}
\value{
a list of results from each run, the range of values across runs, and
    the median values.
}
\description{
robustSPM conducts a robustness test on the quality of fit of 
    an SPM. This is done by using the original optimal model parameters or 
    the original guessed parameter values, add random variation to each of 
    them, and re-fit the model. This process needs to be repeated multiple 
    times. This should enable an analysis of the stability of the modelling 
    outcomes. If the optimum parameters are used then add more variation, if
    initial guesses are used you may need to select different starting points
    so that the random variation covers the parameter space reasonably well.
}
\examples{
\dontrun{
  data(dataspm)
  fish <- dataspm$fish
  glb <- dataspm$glb
  props <- dataspm$props
  pars <- c(14,0.19,0.6)
  out <- robustSPM(pars,fish,glb,props)
  str(out)
  print(out$results)
}
}
