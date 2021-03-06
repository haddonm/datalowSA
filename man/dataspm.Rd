% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datalowSA.r
\docType{data}
\name{dataspm}
\alias{dataspm}
\title{dataspm Three data objects suitable for use with datalowSA.}
\format{
A list of three objects
\describe{
  \item{fish}{ a data.frame containing Year, Catch, CPUE, SE, Records, and
      GeoM which is the unstandardized geometric mean CPUE }
  \item{glb}{ a list of global variables including maxage, M, parameters for
      growth, weight-at-age, maturity-at-age, steepness, R0, selectivity,
      resilience, number of ages, and the ages themselves. }
  \item{props}{ a data.frame of age, laa, waa, maa, sela, and feca}
}
}
\usage{
dataspm
}
\description{
A dataset containing the fish data.frame, the glb list, and the
    props data.frame set up ready for use with datalowSA. In particular it can
    be used with the SPM functions, as well as the ASPM functions.
}
\keyword{datasets}
