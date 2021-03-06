% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datalow_utils.r
\name{histyear}
\alias{histyear}
\title{histyear plots a histogram of a variable for each year available}
\usage{
histyear(
  x,
  Lbound = -3.5,
  Rbound = 12.25,
  inc = 0.25,
  new = FALSE,
  plots = c(11, 3),
  pickvar = "LnCE",
  varlabel = "log(CPUE)",
  col = 2,
  border = 2,
  addnormal = TRUE,
  right = TRUE,
  vline = "mean"
)
}
\arguments{
\item{x}{the data.frame of data with at least a 'Year' and pickvar present}

\item{Lbound}{leftbound on all histograms, defaults to -3.5}

\item{Rbound}{right bound on all histograms, defaults to 12.25}

\item{inc}{the class width of the histogram, defaults to 0.25}

\item{new}{reuse a plotting device or output a new one}

\item{plots}{the number of rows and columns, defaults to c(11,3)}

\item{pickvar}{which variable in the input data.frame to plot each year 
default = 'LnCE'}

\item{varlabel}{what label to use on x-axis, default = 'log(CPUE)'}

\item{col}{colour of each cell; defaults to 2 (red)}

\item{border}{colour of the border of each cell, default = 2 (red)}

\item{addnormal}{include the fitting of a normal distribution}

\item{right}{put the legend on the right hand side, default = TRUE}

\item{vline}{allows for the placing of a vertical line on each histogram
if set to "" then nothing is added. defaults to the average.}
}
\value{
a matrix of the year, mean value, stdev, and N number of 
    observations. It also plots a histogram for each year and fits a
    normal distribution to each one.
}
\description{
histyear plots a histogram of a variable for each year 
    available.
}
\examples{
\dontrun{
print("still to be developed")
}
}
