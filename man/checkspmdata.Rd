% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spm.r
\name{checkspmdata}
\alias{checkspmdata}
\title{checkspmdata ensures the input data contains the necessary columns}
\usage{
checkspmdata(infish)
}
\arguments{
\item{infish}{the data.frame containing columns of data}
}
\value{
a 3 x 2 matrix with vaiable and true or false for presence
}
\description{
checkspmdata ensures the input fishery data contains the
    year, catch, and cpue columns necessary for a SPM analysis.
}
\examples{
\dontrun{
data(fishdat)
fish <- fishdat$fish
checkspmdata(fish)
}
}
