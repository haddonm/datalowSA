

#' @title datalowSA a set of functions to assist with data-poor assessments
#'
#' @description The datalowSA package provides three categories of functions
#'     analytical functions that enable the production of data-poor
#'     model-assisted stock assessments, utility functions that assist
#'     with data manipulation and extracting informaiton from output objects,
#'     and plotting functions that
#'     facilitate the illustration of the results of the assessments.
#'     In addition there are example data sets with which to illustrate the
#'     methods.
#'
#' @section Analytical functions:
#' \describe{
#'   \item{central}{ generates three estimates of central tendency}
#'   \item{doproject}{ generates constant catch projections after running cMSY}
#'   \item{fillell}{ runs the checks on SRA to find success and failure}
#'   \item{fillell2}{ runs the checks on SRA to find success and failure, but
#'       but adds the criteria that the end depletion must be lower than the
#'       start}
#'   \item{Level4MSY}{ generates an MSY estimate from catches and F estimates}
#'   \item{run_cMSY}{ The main function for conducting a modified Catch-MSY
#'       analysis.}
#'   \item{sraMSY}{ Is called by run_cMSY and it calls oneSRA for as many
#'       iterations or replicates as entered. It produces large arrays of
#'       the biomass trajectories from each SRA along with whether or not
#'       each trajectory meets the acceptance criteria or not. Not exported
#'       but can be read using r4tier5:::sraMSY }
#'   \item{oneSRA}{ Is called by sraMSY. It takes in the vector of initial
#'       biomass depletions plus the randomly generated set of population
#'       model parameters and runs the SRA for each of the combinations of
#'       parameters and initial depletion levels. Not exported but can be
#'       read using r4tier5:::oneSRA }
#'   \item{pulloutStats}{ summaries the results from the Catch-MSY
#'       analysis by generating the mean, minimum, maximum, and quantiles of
#'       the resulting r, K, and MSY values.}
#' }
#' @section Utility functions:
#' \describe{
#'   \item{gettier5data}{ gets the columns of data required for Tier5, the
#'       input data.frame must contain at least year and catch, but can also
#'       contain species}
#'   \item{gettraject}{ extracts the plausible biomass trajectories from the
#'       output of cMSY}
#'   \item{halftable}{ halves the height of a tall narrow data.frame}
#'   \item{makedeplet}{ converts the biomass trajetories into a depletion matrix}
#'   \item{pulloutStats}{ summaries results from the Catch-MSY analysis}
#'   \item{datalowSA}{ A brief description of all functions in datalowSA}
#'   \item{summarycMSY}{ makes tables of msy, r, K, meanr, meanK, and all picks}
#'   \item{tier4to5}{ generates a Tier5 formatted dataset from a tier4 dataset}
#'   \item{whichsps}{ generates a listing of which species are in the tier4 data}
#' }
#' @section Plotting functions:
#' \describe{
#'   \item{plotMSY6}{ generates 6 graphs illustrating the array of rK
#'       parameter combinations and whether they were successful or not. That
#'       plot is coloured by how many trajectories across the initial
#'       depletion range were successful.}
#'   \item{plottrajectory}{ plots out the predicted biomass trajectories from
#'       those parameter combinations that have been accepted. It can either
#'       put all trajectories on one plot or generate a separate plot for each
#'       rK parameter set. Each individual biomass trajectory represents a set
#'       of population model parameters and a single initial depletion. It is
#'       possible to only print a specified number of parameter sets rather
#'       than all of them.}
#' }
#' @section Data sets:
#' \describe{
#'   \item{fishdat}{ A dataset containing the fish data.frame, the glb list, and the
#'     props data.frame set up ready for use with datalowSA. In particular it can
#'     be used with fitASPM, fitSPM, run_cMSY, and DBSRA. see ?fishdat}
#'   \item{dataspm}{ A dataset containing the fish data.frame, the glb list, and the
#'     props data.frame set up ready for use with datalowSA. In particular it can
#'     be used with the SPM functions, as well as the ASPM functions. see ?dataspm}
#'   \item{invert}{ A dataset containing the fish data.frame as a 31 x 7 matrix, 
#'     the glb and props data.frames are set to NULL. The fish data.frame has
#'     both the standardized cpue as well as the unstandardized geom, that is
#'     the geometric mean cpue.  This is particularly set up to
#'     be used with the SPM functions but also the Catch-MSY routines. see ?invert}
#'   \item{plaice}{ A dataset containing the fish, glb, props, agedata, and lendata
#'     for North sea plaice. Data taken from Beverton and Holt (1957). The primary 
#'     use of this data set is to illustrate the use of catch curves.} 
#'   \item{sps}{ A dataset containing 9 columns of typical scalefish fisheries data}   
#' }
#' @section Vignettes:
#' To learn more about datalowSA, start with the vignette:
#' \code{browseVignettes(package = "datalowSA")}
#'
#' @docType package
#' @name datalowSA
NULL

#' @title fishdat Three data objects suitable for use with datalowSA.
#'
#' @description A dataset containing the fish data.frame, the glb list, and the
#'     props data.frame set up ready for use with datalowSA. In particular it can
#'     be used with fitASPM, fitSPM, run_cMSY, and DBSRA.
#'
#' @format A list of three objects
#' \describe{
#'   \item{fish}{ a data.frame containing Year, Catch, CPUE, and SE, the standard
#'       error of the CPUE estimates, if present}
#'   \item{glb}{ a list of global variables including maxage, M, parameters for
#'       growth, weight-at-age, maturity-at-age, steepness, R0, selectivity,
#'       resilience, number of ages, and the ages themselves. }
#'   \item{props}{ a data.frame of age, laa, waa, maa, sela, and feca}
#' }
"fishdat"

#' @title dataspm Three data objects suitable for use with datalowSA.
#'
#' @description A dataset containing the fish data.frame, the glb list, and the
#'     props data.frame set up ready for use with datalowSA. In particular it can
#'     be used with the SPM functions, as well as the ASPM functions.
#'
#' @format A list of three objects
#' \describe{
#'   \item{fish}{ a data.frame containing Year, Catch, CPUE, SE, Records, and
#'       GeoM which is the unstandardized geometric mean CPUE }
#'   \item{glb}{ a list of global variables including maxage, M, parameters for
#'       growth, weight-at-age, maturity-at-age, steepness, R0, selectivity,
#'       resilience, number of ages, and the ages themselves. }
#'   \item{props}{ a data.frame of age, laa, waa, maa, sela, and feca}
#' }
"dataspm"

#' @title invert data derived from a trawl caught invertebrate fishery.
#'
#' @description A dataset containing the fish data.frame as a 31 x 7 matrix, 
#'     the glb and props data.frames are set to NULL. The fish data.frame has
#'     both the standardized cpue as well as the unstandardized geom, that is
#'     the geometric mean cpue.  This is particularly set up to
#'     be used with the SPM functions but also the Catch-MSY routines.
#'
#' @format A list of three objects only two of which contains data
#' \describe{
#'   \item{fish}{ a data.frame containing year, catch, cpue, SE of the cpue, 
#'       geom, which is the unstandardized geometric mean CPUE, vessel, which
#'       is the number of active vessels reporting catches, and records, which is
#'       the number of cpue records reported each year }
#'   \item{glb}{ contains the resilience and spsname }
#'   \item{props}{ set to NULL}
#' }
#' @examples 
#'  \dontrun{
#'  data(invert)
#'  str(invert)
#'  print(invert$fish)
#' }
"invert"

#' @title orhdat1 Three data objects suitable for use with asmreduct.
#'
#' @description A dataset containing a fish data.frame, the glb list, and 
#'     the props data.frame set up ready for use with asmreduct. 
#'
#' @format A list of three objects
#' \describe{
#'   \item{fish}{ a data.frame containing year, catch}
#'   \item{glb}{ a list of global variables including maxage, M, parameters 
#'       for growth, weight-at-age, maturity-at-age, steepness, R0, 
#'       selectivity, resilience, number of ages, the ages themselves, the
#'       number of years of catch data, and the species name}
#'   \item{props}{ a data.frame of age, laa, waa, maa, sela, and feca}
#' }
"orhdat1"


#' @title plaice data derived from Beverton and Holt, 1957 for European Plaice.
#' 
#' @description plaice data including fish, glb, props, agedata, and lendata
#'     for North sea plaice dervied from tables and the text of the classical
#'     Beverton and Holt, 1957, book. Includes age data that is useful for 
#'     illustratung the catch curves. Much of this data has also been included
#'     in the age-structured model described in Haddon, 2011.
#'
#' @format A list of five objects with only the first four containing data, the
#'     lendata only contains formatted data for illustrating that format, it is
#'     not real data. The other objects contain real data.
#' \describe{
#'   \item{fish}{ a data.frame containing year, catch, cpue, SE of the cpue }
#'   \item{glb}{biological parameters relating to growth, selectivity, 
#'     weight-at-age, steepness, and resilience and spsname }
#'   \item{props}{ contains six variables ages, laa, waa, maa, sela, and feca,
#'     which are all relative to age.}
#'   \item{agedata}{ a list of 5 objects, yrage - the years in which age data are
#'     available, ages - the observed ages, agemax - the maximum age, nage - 
#'     the number of observed ages, and naa - the numbers-at-age by year}
#'   \item{lendata}{ a list of 5 objects akin to the agedata object but for
#'     length data.}
#' }
#' @examples 
#'  \dontrun{
#'  data(plaice)
#'  str(plaice)
#'  print(plaice$fish)
#'  print(plaice$agedata)
#' }
"plaice"

#' @importFrom grDevices dev.new dev.cur dev.off graphics.off png rgb
#' @importFrom graphics abline hist lines mtext par plot points grid text title
#' @importFrom graphics arrows axis legend polygon segments layout
#' @importFrom stats qnorm rnorm dnorm runif sd quantile optim loess
#' @importFrom stats coef dmultinom lm median as.formula anova ccf
#' @importFrom utils tail read.table read.csv write.table head
#' @importFrom Rcpp evalCpp sourceCpp
#' @exportPattern("^[[:alpha:]]+")
#' @useDynLib datalowSA
NULL




