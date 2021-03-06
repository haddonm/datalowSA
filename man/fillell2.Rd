% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cMSY_funs.R
\name{fillell2}
\alias{fillell2}
\title{fillell2 runs the criteria of success on each SRA}
\usage{
fillell2(biot, intheta, mult, ct, Hmax = 1, Fyear)
}
\arguments{
\item{biot}{the matrix of biomass trajectories obtained from makebiomC,
containing the SRA trajectories for the aprticular combination of
r and K across the range of starting biomass trajectories.}

\item{intheta}{the array of parameters associated with the biomass
trajectories. Includes r, K, the range of depletions and the process
error added to productivity - sigR.}

\item{mult}{a multiplier for K to allow for stock biomasses to rise above K}

\item{ct}{the vector of catches per year}

\item{Hmax}{upper limit of harvest rate included in the constraints;
defaults to 1.0, which implies no upper limit.}

\item{Fyear}{is the index to the year in which a rnage of harvest rates
is to be used to constrina the acceptable trajectories.}
}
\value{
a vector of 1 and 0 relating to an acceptable trajectory (one that
    met the criteria) and unacceptable (one that failed the criteria)
}
\description{
fillell2 runs the acceptance criteria over the biomass
    trajectories obtained from the Stock Reduction Analysis for each
    combination of r, K, and starting depletion conducted in makebiomC. The
    criteria agsint which each trajectory are tested include that the
    final depletion be less than the maximum of the expected range of final
    depletions and greater than the minimum of final depletion levels. In
    addition I have included that the initial biomass level be greater
    than the final biomass level. This differs from the extra criteria used
    by Martell and Froese (2013), who selected the smallest K still able to
    generate the mean predicted MSY.
}
\examples{
\dontrun{
print("This is the function that imposes constraints on the biomass")
print("trajectories. Add new ones to the end of the long if statement")
print("They need to define what is required to keep a trajectory")
}    #   biot=biot; intheta=itheta[tick,]; mult=mult; ct=ct; Hmax=maxH;Fyear=NA 
}
