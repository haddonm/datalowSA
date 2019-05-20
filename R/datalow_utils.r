
#' @title addnorm - adds a normal distribution to a histogram of a data set.
#'
#' @description  addnorm - adds a normal distribution to a histogram of a data
#'    set. This is generally to be used to illustrate whether log-transformation
#'    normalizes a set of catch or cpue data.
#' @param inhist - is the output from a call to 'hist' (see examples)
#' @param xdata -  is the data that is being plotted in the histogram.
#' @param inc - defaults to a value of 0.01; is the fine grain increment used to
#'    define the normal curve. The histogram will be coarse grained relative to
#'    this.
#' @return a list with a vector of 'x' values and a vector of 'y' values (to be
#'    used to plot the fitted normal probability density function), and a vector
#'    used two called 'stats' containing the mean and sandard deviation of the
#'    input data
#' @export addnorm
#' @examples
#' x <- rnorm(1000,mean=5,sd=1)
#' dev.new(height=6,width=4,noRStudioGD = TRUE)
#' par(mfrow= c(1,1),mai=c(0.5,0.5,0.3,0.05))
#' par(cex=0.85, mgp=c(1.5,0.35,0), font.axis=7)
#' outH <- hist(x,breaks=25,col=3,main="")
#' nline <- addnorm(outH,x)
#' lines(nline$x,nline$y,lwd=3,col=2)
#' print(nline$stats)
addnorm <- function(inhist,xdata,inc=0.01) {
   lower <- inhist$breaks[1]
   upper <- tail(inhist$breaks,1)
   cw <- inhist$breaks[2]-inhist$breaks[1]
   x <- seq(lower,upper, inc) #+ (cw/2)
   avCE <- mean(xdata,na.rm=TRUE)
   sdCE <- sd(xdata,na.rm=TRUE)
   N <- length(xdata)
   ans <- list(x=x,y=(N*cw)*dnorm(x,avCE,sdCE),stats=c(avCE,sdCE,N))
   return(ans)
} # end of addnorm

#' @title calcrmse calcuates the root mean square error for two vectors
#' 
#' @description given a set of observations and predicted values, calcrmse
#'     calculates the root mean square error sqrt(resid^2/n) to provide a 
#'     measure of relative fit. An assumption of normal errors is made so
#'     if the observations and predicted values actually have a log-normal
#'     distribution one should log-transform the input values.
#'
#' @param obs the observed data
#' @param pred the predicted values whose fit to the observations is to 
#'     measured
#'
#' @return a scalar value which is the rmse.
#' @export
#'
#' @examples
#' \dontrun{
#' x <- rep(1,10)
#' y <- c(1.109,1.210,0.947,0.933,0.832,0.864,0.633,0.820,1.004,1.049)
#' calcrmse(log(x),log(y))  # should be 0.1899495
#' }
calcrmse <- function(obs,pred){ # obs=fishery[,"CPUE"]; pred=fishery[,"PredCE"]
   pickCE <- which(!is.na(obs))
   resid <- obs[pickCE] - pred[pickCE]
   rmse <- sqrt(sum(resid^2)/length(resid))
   return(rmse)
}

#' @title checkdata determines which methods match the input data
#'
#' @description checkdata ensures the input fishery data contains the
#'     data necessary for each of the analyses present and reports which
#'     analyses are possible. It checks for simple errors
#'
#' @param indata this can be either the complete list of data objects of just
#'     the matrix of fisheries data.
#' @param glob in case one inputs a data.frame of fishery data this allows one
#'     to enter the globals list containing as a minimum the spsname and
#'     resilience, so that a test can be made for the spm analysis
#'
#' @return a 3 x 2 matrix with vaiable and true or false for presence
#' @export
#'
#' @examples
#' \dontrun{
#' data(fishdat)
#' fish <- fishdat$fish
#' checkdata(fishdat)
#' checkdata(fish)
#' }
checkdata <- function(indata,glob=NA) { # indata=fish; glob=glb
   rown <- c("catch-MSY","spm","aspm","catch-curves")
   result <- as.data.frame(matrix(0,nrow=4,ncol=2,
             dimnames=list(rown,c("Method","Possible"))))
   result[,"Method"] <- rown
   result[,"Possible"] <- FALSE
   if (class(indata) == "data.frame") {
      if ((all(c("year","catch") %in% colnames(indata))) &
          (length(grep("cpue",colnames(indata))) > 0))
         result["spm",] <- TRUE  
      if (is.list(glob)) {
         if ((all(c("year","catch") %in% colnames(indata))) & 
             (glb$resilience %in% c("verylow","low","medium","high")))
               result["catch-MSY",] <- TRUE   
       } else {
         print("Only the fisheries data.frame checked for use with spm.")
      }   
      return(result)
   } else  if (class(indata) == "list") {
      fish <- indata$fish
      glb <- indata$glb
      glbnames <- c("maxage","M","Linf","K","t0","Waa","Wab","M50a","deltaM",
                    "steep","R0","sela50","deltaS","resilience","nages","ages",
                    "nyrs","spsname")
      props <- indata$props
      agedata <- indata$agedata
      if ((all(c("year","catch") %in% colnames(fish))) & 
         (glb$resilience %in% c("verylow","low","medium","high")))
          result["catch-MSY",] <- TRUE
      if (all(c("year","catch","cpue") %in% colnames(fish)))
          result["spm",] <- TRUE
      if ((all(c("year","catch","cpue") %in% colnames(fish))) &
          ((all(glbnames %in% names(glb))) | (class(props) == "data.frame")))
          result["aspm",] <- TRUE         
      if (class(agedata) != "NULL") result["catch-curves",] <- TRUE
      return(result)
   } else {
      return(result)
   }
} # end of checkspmdata


#' @title countgtzero used in apply to count the number of zeros in a vector
#'
#' @description countgtzero is designed to be used within the apply function to 
#'     count the number of zeros in a vector
#' @param invect vector of values
#' @return A single value of zero or the number of zeros
#' @export countgtzero
#' @examples
#' \dontrun{
#' x <- c(1,2,3,4,0,0,0,5,6,7,8) # 8 values > 0 
#' countgtzero(x)
#' }
countgtzero <- function(invect) {
   return(length(which(invect > 0)))
}


#' @title dataTemplate - generates a standard input datafile as a template
#'
#' @description dataTemplate - generates a standard input datafile to use as
#'     a template. It is possible to define the number of blocks and then, once
#'     the data file is created, go in and edit it appropriately to suit exactly
#'     the data you have for your own species. Note that in the example commas
#'     are used to separate individual data fields, this is required if you are
#'     going to use the function 'readdata' to get your data into datalowSA,
#'     which is recommended. But of course you are free to use it however you 
#'     wish! Each section in the data file, except the title, is identified 
#'     using CAPITAL letters, as in RESILIENCE, NYRS, YEARS, etc. These, again
#'     are needed by 'readdata' to identify the various sections and proceed
#'     to reading that data in appropriately. If you do not have data for a 
#'     section just delete it entirely, including the heading. DO NOT use a
#'     section heading name anywhere else in the document as 'readdata' will
#'     always try to use the first instance which could cause you trouble. The
#'     example AGE data comes from Table 13.1 in Beverton and Holt (1957) and
#'     consists of ageing data from North Sea plaice. Tw sexes have been given
#'     here to illustrate the use of age by sex (female first row for a year,
#'     males in the second row for a year), but zeros are given for males, The
#'     numbers at length are pure fiction and again are merely here to 
#'     illustrate the format.
#'     
#' @param filename - the name for the generated datafile, character string,
#'     defaults to tmpdat.csv
#' @param title the first line of the data file; identify its contents
#' @return a standard definition data file ready to be read by readdata
#'     and it also outputs the full address of the filename.
#' @export
#' @examples
#' \dontrun{
#' dataTemplate(filename="fishery1.csv")
#' ans <- readdata("fishery1.csv")
#' str(ans)
#' }                   # filename="C:/Users/had06a/Dropbox/Public/fishery1.csv"
dataTemplate <- function(filename="constants.csv",title="SpeciesName") {
   cat(title, "\n\n",file=filename,append=FALSE)
   cat("# The format of this file is such that the first line contains \n",
       file=filename, append=TRUE)
   cat("# the species name or title for the subsequent analyses \n",
       file=filename,append=TRUE)
   cat("# Each section contains a header fully in CAPITALS, which is used by \n",
       file=filename, append=TRUE)
   cat("# the function 'readdata' to identify the following data and read it \n",
       file=filename, append=TRUE)
   cat("# in appropriately. If you do not have data for a particular \n",
       file=filename, append=TRUE)
   cat("# section, just delete it and its header. \n",
       file=filename, append=TRUE)
   cat("# DO NOT confuse the software by having the same word used as \n",
       file=filename, append=TRUE)
   cat("# a section header occur more than once in the data file \n",
       file=filename, append=TRUE)
   cat("# Always save the file as a comma separated variable file as this \n",
       file=filename, append=TRUE)
   cat("# is the default format when using 'readdata'. \n\n",
       file=filename, append=TRUE)
   cat("RESILIENCE  \n",file=filename, append=TRUE)
   cat("verylow   \n\n",file=filename, append=TRUE)
   cat("NYRS  \n",file=filename, append=TRUE)
   cat("30,   \n\n",file=filename, append=TRUE)
   cat("YEARS,catch,cpue,SE  \n",file=filename, append=TRUE)
   cat("1986,3924.912,NA,NA  \n",file=filename, append=TRUE)
   cat("1987,5117.988,NA,NA  \n",file=filename, append=TRUE)
   cat("1988,4722.2,NA,NA  \n",file=filename, append=TRUE)
   cat("1989,1365.128,1.49,0.2  \n",file=filename, append=TRUE)
   cat("1990,801.567,1.849,0.2  \n",file=filename, append=TRUE)
   cat("1991,625.407,1.501,0.2  \n",file=filename, append=TRUE)
   cat("1992,1108.241,2.997,0.2  \n",file=filename, append=TRUE)
   cat("1993,964.409,1.415,0.2  \n",file=filename, append=TRUE)
   cat("1994,800.618,1.139,0.2  \n",file=filename, append=TRUE)
   cat("1995,962.399,0.7,0.2  \n",file=filename, append=TRUE)
   cat("1996,1180.349,0.469,0.2  \n",file=filename, append=TRUE)
   cat("1997,297.003,0.303,0.2  \n",file=filename, append=TRUE)
   cat("1998,316.131,0.356,0.2  \n",file=filename, append=TRUE)
   cat("1999,210.529,0.39,0.2  \n",file=filename, append=TRUE)
   cat("2000,169.337,0.439,0.2  \n",file=filename, append=TRUE)
   cat("2001,200.843,0.489,0.2  \n",file=filename, append=TRUE)
   cat("2002,255.735,0.431,0.2  \n",file=filename, append=TRUE)
   cat("2003,217.502,0.52,0.2  \n",file=filename, append=TRUE)
   cat("2004,283.11,0.777,0.2  \n",file=filename, append=TRUE)
   cat("2005,264.607,1.322,0.2  \n",file=filename, append=TRUE)
   cat("2006,139.316,1.412,0.2  \n",file=filename, append=TRUE)
   cat("2007,28.571,NA,NA  \n",file=filename, append=TRUE)
   cat("2008,3.331,NA,NA  \n",file=filename, append=TRUE)
   cat("2009,13.859,NA,NA  \n",file=filename, append=TRUE)
   cat("2010,21.44,NA,NA  \n",file=filename, append=TRUE)
   cat("2011,31.426,NA,NA  \n",file=filename, append=TRUE)
   cat("2012,17.253,NA,NA  \n",file=filename, append=TRUE)
   cat("2013,35.94,NA,NA  \n",file=filename, append=TRUE)
   cat("2014,22.087,NA,NA  \n",file=filename, append=TRUE)
   cat("2015,16.206,NA,NA  \n\n\n",file=filename, append=TRUE)
   cat("# Other variables can be added to the biology list below. The \n",
       file=filename, append=TRUE)
   cat("# standard format is to add the value followed by a comma, followed \n", 
       file=filename, append=TRUE)
   cat("# by a descriptive name. This will then be able to be included in \n",
       file=filename, append=TRUE)
   cat("# the standard input function 'readdata' once we know what data you \n",
       file=filename, append=TRUE)
   cat("# may have that we did not think of. \n\n",file=filename, append=TRUE)
   cat("BIOLOGY  \n",file=filename, append=TRUE)
   cat("80   , maxage  the maximum age, usually a plus group \n",
       file=filename, append=TRUE)
   cat("0.036  , M  natural mortality  \n",file=filename, append=TRUE)
   cat("39.6   , Linf vB asymptotic maximum length \n",file=filename, append=TRUE)
   cat("0.06   , K vB Brody growth coefficient \n",file=filename, append=TRUE)
   cat("-0.01  , t0 theoretical age at zero length\n",file=filename, append=TRUE)
   cat("0.0365 , Waa weight at age a parameter \n",file=filename, append=TRUE)
   cat("2.965  , Wab weight at age b parameter \n",file=filename, append=TRUE)
   cat("31.0   , M50a age at 50% maturity  \n",file=filename, append=TRUE)
   cat("5      , deltaM diff between ages at 50 and 95% maturity \n",
       file=filename, append=TRUE)
   cat("31    , sela50 age at 50% selectivity  \n",file=filename, append=TRUE)
   cat("5     , deltaS diff between 50 and 95% selectivity \n",file=filename, 
       append=TRUE)
   cat("0.6   , steep stock recruitment steepness \n",file=filename, append=TRUE)
   cat("14    , Ln(R0) initial value but this will be estimated  \n\n",
       file=filename, append=TRUE)
   cat("# The catch-at-age data derives from Table 13.1 in Beverton and \n",
       file=filename, append=TRUE)
   cat("# Holt, 1957; this version uses -99 to indicate missing date. \n\n",
       file=filename, append=TRUE)
   cat("AGE data \n",file=filename, append=TRUE)
   cat("9 , years of numbers-at-age data \n",file=filename, append=TRUE)
   cat("2 , sexes, females first=1 then males=2 \n",file=filename, append=TRUE)
   cat("2, 3, 4, 5, 6, 7, 8, 9, 10,  age classes \n",file=filename, append=TRUE)
   cat("1929,1,328,2120,2783,1128,370,768,237,112,48 \n",file=filename, append=TRUE)
   cat("1930,1,223,2246,1938,1620,302,106,181,58,18 \n",file=filename, append=TRUE)
   cat("1931,1,95,2898,3017,1159,591,116,100,82,33 \n",file=filename, append=TRUE)
   cat("1932,1,77,606,4385,1186,231,138,42,21,51 \n",file=filename, append=TRUE)
   cat("1933,1,50,489,1121,4738,456,106,80,27,18 \n",file=filename, append=TRUE)
   cat("1934,1,44,475,1666,1538,2510,160,50,43,14 \n",file=filename, append=TRUE)
   cat("1935,1,131,1373,1595,1587,1326,883,144,30,28 \n",file=filename, append=TRUE)
   cat("1936,1,38,691,2862,1094,864,382,436,27,15 \n",file=filename, append=TRUE)
   cat("1937,1,138,1293,1804,1810,426,390,163,228,26 \n",file=filename, append=TRUE)
   cat("1929,2,-99,-99,-99,-99,-99,-99,-99,-99,-99 \n",file=filename, append=TRUE)
   cat("1930,2,-99,-99,-99,-99,-99,-99,-99,-99,-99 \n",file=filename, append=TRUE)
   cat("1931,2,-99,-99,-99,-99,-99,-99,-99,-99,-99 \n",file=filename, append=TRUE)
   cat("1932,2,-99,-99,-99,-99,-99,-99,-99,-99,-99 \n",file=filename, append=TRUE)
   cat("1933,2,-99,-99,-99,-99,-99,-99,-99,-99,-99 \n",file=filename, append=TRUE)
   cat("1934,2,-99,-99,-99,-99,-99,-99,-99,-99,-99 \n",file=filename, append=TRUE)
   cat("1935,2,-99,-99,-99,-99,-99,-99,-99,-99,-99 \n",file=filename, append=TRUE)
   cat("1936,2,-99,-99,-99,-99,-99,-99,-99,-99,-99 \n",file=filename, append=TRUE)
   cat("1937,2,-99,-99,-99,-99,-99,-99,-99,-99,-99 \n\n\n",file=filename, append=TRUE)
   cat("LENGTH data \n",file=filename, append=TRUE)
   cat("2 , years of numbers-at-length data \n",file=filename, append=TRUE)
   cat("2 , sexes, females=1 first then males=2 \n",file=filename, append=TRUE)
   cat("15,18,21,24,27,30,33,36,39, length classes \n",file=filename, append=TRUE)
   cat("1990,1,15,100,200,300,400,350,300,220,100 \n",file=filename, append=TRUE)
   cat("1990,2,15,100,200,300,400,350,300,220,100 \n",file=filename, append=TRUE)
   cat("1998,1,15,100,200,300,400,350,300,220,100 \n",file=filename, append=TRUE)
   cat("1998,2,15,100,200,300,400,350,300,220,100 \n",file=filename, append=TRUE)
   return(filename)
}  # end of dataTemplate


#' @title facttonum converts a vector of numeric factors into numbers
#' 
#' @description facttonum converts a vector of numeric factors into numbers.
#'     If the factors are not numeric then the outcome will be a series of NA.
#'     It is up to you to apply this function only to numeric factors. A warning 
#'     will be thrown if the resulting output vector contains NAs
#'
#' @param invect the vector of numeric factors to be converted back to numbers
#'
#' @return an output vector of numbers instead of the input factors
#' @export
#'
#' @examples
#' \dontrun{
#'  DepCat <- as.factor(rep(seq(100,600,100),2)); DepCat
#'  5 * DepCat[3]
#'  as.numeric(levels(DepCat))  # #only converts the levels not the replicates
#'  DepCat <- facttonum(DepCat)
#'  5 * DepCat[3]
#'  x <- factor(letters)
#'  facttonum(x)
#' }
facttonum <- function(invect){
   if (class(invect) == "factor") {
      outvect <- suppressWarnings(as.numeric(levels(invect))[invect])
   }
   if (class(invect) == "numeric") outvect <- invect
   if (any(is.na(outvect))) 
      warning("NAs produced, your input vector may have non-numbers present \n")
   return(outvect)
} # end of facttonum


#' @title getLNCI gets the log-normal confidence intervals
#' 
#' @description getLNCI takes the mean and the standard deviation and produces
#'     the asymmetric log-normal confidence intervals around the mean values 
#'
#' @param av the mean value or a vector of mean values
#' @param se the standard deviation 
#' @param P the percent used for the CI, defaults to 95.
#'
#' @return a vector of three for a single input or a matrix of 3 columns for 
#'     input vectors
#' @export
#'
#' @examples
#' \dontrun{
#'   av <- c(4.0,2.15)
#'   se <- 0.33
#'   getLNCI(av,se,P=95)
#'   se <- c(0.33,0.4)
#'   getLNCI(av,se)
#' }
getLNCI <- function(av,se,P=95){  # av=fissp[,"CPUE"]; se=rmse;P=0.95
   Zmult <- -qnorm((1-(P/100))/2.0)
   lower <- av * exp(-Zmult*se)
   upper <- av * exp(Zmult*se)
   if (length(av) > 1) {
      result <- cbind(lower,av,upper)
      colnames(result) <- c("lower","mean","upper")
   } else {
      result <- c(lower=lower,mean=av,upper=upper)
   }
   return(result)
} # end of getLNCI   


#' @title getminy generates the lower bound for a plot
#'
#' @description getminy generates a lower bound for a plot where it is unknown
#'     whether the minumum is less than zero of not. If less than 0 then
#'     multiplying by the default mult of 1.05 works well but if the outcome if
#'     > 0 then the multiplier needs to be adjusted appropriately so the minimum
#'     is slightly lower than the minimum of the data
#'
#' @param x the vector of data to be tested for its minimum
#' @param mult the multiplier for both ends, defaults to 1.05 (=0.95 if >0)
#'
#' @return a suitable lower bound for a plot if required
#' @export
#'
#' @examples
#' vect <- rnorm(10,mean=0,sd=2)
#' sort(vect)
#' getminy(vect,mult=1.0)
getminy <- function(x,mult=1.05) {
   ymin <- min(x,na.rm=TRUE)
   if (ymin < 0) {
      ymin <- ymin * mult
   } else {
      ymin <- ymin * (2 - mult)
   }
   return(ymin)
} # end of getminy

#' @title getmaxy generates the upper bound for a plot
#'
#' @description getmaxy generates an upper bound for a plot where it is unknown
#'     whether the maximum is greater than zero of not. If > 0 then
#'     multiplying by the default mult of 1.05 works well but if the outcome if
#'     < 0 then the multiplier needs to be adjusted appropriately so the maximum
#'     is slightly higher than the maximum of the data
#'
#' @param x the vector of data to be tested for its maximum
#' @param mult the multiplier for both ends, defaults to 1.05 (=0.95 if < 0)
#'
#' @return a suitable upper bound for a plot if required
#' @export
#'
#' @examples
#' \dontrun{
#' vect <- rnorm(10,mean=0,sd=2)
#' sort(vect,decreasing=TRUE)
#' getmaxy(vect,mult=1.0)
#' vect <- rnorm(10,mean = -5,sd = 1.5)
#' sort(vect,decreasing=TRUE)
#' getmaxy(vect,mult=1.0)
#' }
getmaxy <- function(x,mult=1.05) {
   ymax <- max(x,na.rm=TRUE)
   if (ymax > 0) {
      ymax <- ymax * mult
   } else {
      ymax <- ymax * (2 - mult)
   }
   return(ymax)
} # end of getmaxy

#' @title getsingle extracts a single number from an input line of characters
#'
#' @description getsingle splits up a text line and translates the first non-
#'     empty character string into a number.
#'
#' @param inline the line of text, usually taken after using readLines
#' @param sep the separator used to divide the numbers from descriptive text.
#'     defaults to a comma.
#'
#' @return a single number
#' @export
#'
#' @examples
#' \dontrun{
#' x <- "12.3 , this is a number"
#' y <- "21.3 # 22.3 # here are two numbers"
#' getsingle(x)
#' getsingle(y,sep="#")
#' }
getsingle <- function(inline,sep=",") {  # inline=dat[41]
   tmp <- unlist(strsplit(inline,sep))
   tmp <- gsub(" ","",tmp)
   tmp <- tmp[nchar(tmp) > 0]
   return(as.numeric(tmp[1]))
}


#' @title getvector  extracts a vector of numbers from a line of characters
#' 
#' @description getvector when reading in a csv file using readLines, 
#'     getvector extarcts a line of numbers from a specified line within
#'     the readLine object.This function works out how many numbers there
#'     are. If you wish to add a comment at the end of a vector of numbers
#'     it must be separated from tehm by the separator. e.g. a comma
#' @param indat the readLines object
#' @param locate the line number from which to extract the numbers
#' @param sep the separator between numbers, defaults to ","
#'
#' @return a vector of numbers
#' @export
#'
#' @examples
#' \dontrun{
#' x <- "12.3, 15.1, 8.7,10.3,  # this is a vector of numbers"
#' y <- "21.3 # 22.3 # 8.7 # 10.3 # here are four numbers"
#' getvector(x)
#' getvector(y,sep="#")
#' }
getvector <- function(indat,locate,sep=",") { # indat=dat; locate=pick+2;sep=","
   vect <- indat[locate]
   if (length(grep("\t",vect) > 0)) vect <- gsub("\t",sep,vect)
   vect <- unlist(strsplit(vect,sep))
   vect <- gsub(" ","",vect)
   vect1 <- vect
   vect <- suppressWarnings(as.numeric(vect))
   vect <- vect[nchar(vect) > 0]   
   if (!any(vect1 == "NA")) {
      vect <- vect[!is.na(vect)]
   }
   return(vect)
}

#' @title greplow - uses tolower in the search for the pattern
#'
#' @description greplow - a grep implementation that ignores the case of either
#'    the search pattern or the object to be search. Both are converted to lower
#'    case before using grep.
#' @param pattern - the text to search for in x
#' @param x - the vector or object within which to search for 'pattern' once
#'    both have been converted to lowercase.
#'
#' @return the index location within x of 'pattern', if it is present, an empty
#'    integer if not
#' @export greplow
#'
#' @examples
#' txt <- c("Long","Lat","LongE","LatE","Depth","Zone","Effort","Method")
#' greplow("zone",txt)
#' greplow("Zone",txt)
#' greplow("long",txt)
greplow <- function(pattern,x) {
   return(grep(tolower(pattern),tolower(x)))
}


#' @title histyear plots a histogram of a variable for each year available
#' 
#' @description histyear plots a histogram of a variable for each year 
#'     available.
#'
#' @param x the data.frame of data with at least a 'Year' and pickvar present 
#' @param Lbound leftbound on all histograms, defaults to -3.5
#' @param Rbound right bound on all histograms, defaults to 12.25
#' @param inc  the class width of the histogram, defaults to 0.25
#' @param new reuse a plotting device or output a new one
#' @param plots the number of rows and columns, defaults to c(11,3)
#' @param pickvar which variable in the input data.frame to plot each year 
#'     default = 'LnCE'
#' @param varlabel what label to use on x-axis, default = 'log(CPUE)'
#' @param col colour of each cell; defaults to 2 (red)
#' @param border colour of the border of each cell, default = 2 (red)
#' @param addnormal include the fitting of a normal distribution
#' @param right put the legend on the right hand side, default = TRUE
#' @param vline allows for the placing of a vertical line on each histogram
#'     if set to "" then nothing is added. defaults to the average.
#'
#' @return a matrix of the year, mean value, stdev, and N number of 
#'     observations. It also plots a histogram for each year and fits a
#'     normal distribution to each one.
#' @export
#'
#' @examples
#' \dontrun{
#' print("still to be developed")
#' }
histyear <- function(x,Lbound=-3.5,Rbound=12.25,inc=0.25,new=FALSE,
                     plots=c(11,3),pickvar="LnCE",varlabel="log(CPUE)",
                     col=2,border=2,addnormal=TRUE,right=TRUE,vline="mean") {
   yrs <- sort(unique(x[,"Year"]))
   nyr <- length(yrs)
   columns <- c("Year","maxcount","Mean","StDev","N")
   results <- matrix(0,nrow=nyr,ncol=length(columns),dimnames=list(yrs,columns))
   rgeCE <- range(x[,pickvar],na.rm=TRUE)
   avCE <- mean(x[,pickvar],na.rm=TRUE)
   if (rgeCE[1] < Lbound) Lbound <- rgeCE[1] - inc
   if (rgeCE[2] > Rbound) Rbound <- rgeCE[2] + inc
   if (new) plotprep(width=6,height=9)
   par(mfcol=plots,mai=c(0.225,0.25,0.025,0.05),oma=c(1.2,1.0,0.0,0.0)) 
   par(cex=0.75, mgp=c(1.35,0.35,0), font.axis=7,font=7,font.lab=7) 
   for (yr in 1:nyr) {
      pick <- which(x[,"Year"] == yrs[yr])
      outh <- hist(x[pick,pickvar],breaks=seq(Lbound,Rbound,inc),col=col,
                   border=border,main="",xlab="",ylab="",panel.first=grid())
      if (vline == "mean") abline(v=avCE,col=4,lwd=2)
      if (is.numeric(vline)) abline(v=vline,col=4,lwd=2)
      adjust <- 0
      if (right) adjust <- 1
      mtext(paste0("  ",yrs[yr]),side=3,outer=F,line=-1.5,font=7,cex=0.8,adj=adjust) 
      mtext(paste0(" ",length(pick)),side=3,outer=F,line=-2.5,font=7,cex=0.8,adj=adjust)
      if (addnormal) {
         pickmax <- which.max(outh$counts)
         ans <- addnorm(outh,x[pick,pickvar])
         lines(ans$x,ans$y,col=4,lwd=2)
         results[yr,] <- c(yrs[yr],outh$mids[pickmax],ans$stats)
      }
   }
   mtext("Frequency",side=2,outer=T,line=0.0,font=7,cex=1.0) 
   mtext(varlabel,side=1,outer=T,line=0.0,font=7,cex=1.0) 
   return(invisible(results))
} # end of histyear


#' @title incol is a utility to determine is a column is present in a matrix
#'
#' @description incol is a utility to determine whether a names columns is
#'     present in a given matrix or data.frame.
#'
#' @param incol the name of the column; defaults to "year" as an example
#' @param inmat the matrix or data.frame within which to search for incol
#'
#' @return TRUE or FALSE
#' @export
#'
#' @examples
#' \dontrun{
#' test <- matrix(c(1,2,3,4),nrow=2,ncol=2,dimnames=list(1:2,c("year","Catch")))
#' print(test)
#' iscol("year",test)
#' iscol("Catch",test)
#' iscol("catch",test)
#' iscol("ages",test)
#' }
iscol <- function(incol="year",inmat) { # incol="ages"; inmat=dat
   if (length(grep(incol,colnames(inmat))) < 1) return(FALSE)
   else return(TRUE)
}

#' @title magnitude returns the magnitude of numbers
#' 
#' @description magnitude is useful when using an
#'     optimizer such as optim, which uses a parscale parameter.
#'     magnitude can determine the respective parscale value for each
#'     parameter value.
#'
#' @param x the vector of numbers (parameters) whose magnitudes are
#'     needed
#'
#' @return a vector of magnitudes 
#' @export
#'
#' @examples
#' \dontrun{
#' x <- c(0,0.03,0.3,3,30,300,3000)
#' magnitude(x)
#' }
magnitude <- function(x) {
   return(10^(floor(log10(abs(x)))))
}

#' @title makedataset combines year, catch, and optionally cpue into a data set
#' 
#' @description makedataset takes the components of a simplified data set and 
#'     combines them into a standard structure suitable for use with datalowSA.
#'     It requires a vector called year, one called catch, optionally one called
#'     cpue (all vectors need to be the same length with gaps filled with NA), 
#'     the species name, and its resilience. It returns a list containing
#'     all the required parts.
#'
#' @param year a vector of years that contained fisheries data
#' @param catch a vector of catches in each year
#' @param cpue either a single NA (default) or a vector of cpue. Any gaps in the 
#'     cpue time-series should be filled with NA
#' @param species the species name in quotes
#' @param resilience either verylow, low, medium, or high
#'
#' @return a list of fish, glb, props, agedata, and lendata, the last 3 NULL
#' @export
#'
#' @examples
#' \dontrun{
#' year <- 1986:2016
#' catch <- c(112.9,206.3,95.7,183.1,147.4,198.9,102.1,235.5,247.8,426.8,448,577.4,
#'            558.5,427.9,509.3,502.4,429.6,360.2,306.2,195.7,210,287.3,214.2,260.6,
#'            272.2,356.9,345,282.7,285.1,237.8,233.3)
#' cpue <- c(1.2006,1.3547,1.0585,1.0846,0.9738,1.0437,0.7759,1.0532,1.284,1.3327,
#'           1.4014,NA,NA,1.142,0.9957,0.8818,0.7635,0.7668,0.7198,0.5997,0.6336,
#'           0.6936,0.8894,0.8644,0.8442,0.8427,0.8849,0.9964,0.9804,0.957,1.0629)
#'  dat <- makedataset(year,catch,cpue,"dataspmbits","low")
#'  dat
#' }
makedataset <- function(year, catch, cpue=NA, species="species",resilience="low") {
   if (length(year) != length(catch)) 
      stop("year vector different length to catch vector \n")
   if (length(cpue) == 1) cpue <- rep(NA,length(year))
   fish <- as.data.frame(cbind(year,catch,cpue),row.names=year)
   glb <- list(spsname=species,resilience=resilience)
   return(list(fish=fish,glb=glb,props=NULL,agedata=NULL,lendata=NULL))
} # end of makedataset

#' @title multinomLL is the negative log-likelihood for multinomial values
#' 
#' @description multinomLL calculates the negative log-likelihood for the 
#'     multinomial distribution. Commonly used with length and age composition 
#'     data in assessment models. Zero observations in particular categories
#'     do not matter as the equation is - Sum(Obs x log(expected-proportion)). 
#'     Thus zeros in the observations just contribute zero to the sum.
#'
#' @param counts the observed counts in each category (age or length) that is to
#'     be fitted by the model
#' @param predprop the predicted proportions in each category. One needs a 
#'     separate function to calculate this.
#'
#' @return a single scalar value, the -veLL
#' @export
#'
#' @examples
#' \dontrun{
#'   obs <- c(0,5,11,19,12,3,0,0)
#'   expprop <- c(0.018,0.002,0.12,0.4,0.3,0.12,0.03,0.01)
#'   multinomLL(obs,expprop)  # should be 92.61393
#' }
multinomLL <- function(counts,predprop) {
   LL <- -sum(counts*log(predprop))  #
   return(LL)
}

#' @title outoptim provides a sucinct summary of the optim solution
#' 
#' @description outoptim provides a more succinct printout of the optim solution
#'     which only uses five lines on the screen rather than the standard print
#'     of the output list structure
#'
#' @param inopt an object into which the output from optim is directed.
#'
#' @return nothing, but it does print the optim result to the console in five 
#'     lines without the list structure
#' @export
#'
#' @examples
#' \dontrun{
#' data(fishdat)
#' bestL <- optim(c(14.0,0.3),aspmLL,method="Nelder-Mead",infish=fishdat$fish,
#'                 inglb=fishdat$glb,inprops=fishdat$props,
#'                 control=list(maxit=1000,parscale=c(10,0.1)))
#' outoptim(bestL)
#' }
outoptim <- function(inopt){
   cat("$par         : ",inopt$par,"\n")
   cat("$value       : ",inopt$value,"\n")
   cat("$counts      : ",inopt$counts,"  iterations, gradient\n")
   cat("$convergence : ",inopt$convergence,"\n")
   cat("$message     : ",inopt$message,"\n")
} # end of outoptim

#' @title parsyn types the standard syntax for the par command to the console
#'
#' @description parsyn types the standard syntax for the par command to the
#'     console so it can be copied and pasted into your own code and modified.
#'
#' @return it writes two lines of R code to the console
#' @export
#'
#' @examples
#' parsyn()
parsyn <- function() {
   cat("par(mfrow=c(1,1),mai=c(0.45,0.45,0.05,0.05),oma=c(0.0,0,0.0,0.0)) \n")
   cat("par(cex=0.85, mgp=c(1.35,0.35,0), font.axis=7,font=7,font.lab=7)  \n")
}

#' @title penalty0 enables the adding of a large penalty as one approaches 0.0
#'
#' @description penalty0 allows for the option of adding a large penalty as
#'     a parameter approaches 0.0 . See spmLL 
#'     for example code that contains such a parameter. For example, when 
#'     fitting an spm sometimes the optimal mathematical model fit can occur 
#'     by depressing the r value to 0 or even go negative. This is only
#'     used internally to datalowSA and so is not formally exported.
#'
#' @param x the parameter value that potentially incurs a penalty
#'
#' @return a single value as a penalty to be added to a Log-Likelihood or SSQ
#'
#' @examples
#' datalowSA:::penalty0(0.5)
#' datalowSA:::penalty0(0.1)
#' datalowSA:::penalty0(0.01)
#' datalowSA:::penalty0(0.005)
penalty0 <- function(x){
   ans <- 500*exp(-1000*x)
   return(ans)
} # end of penalty0

#' @title penalty1 enables the adding of a large penalty as one approaches 1.0
#'
#' @description penalty1 allows for the option of adding a large penalty as
#'     a parameter approaches 1.0 and moves to become larger than 1. See aspmLL 
#'     for example code that contains such a parameter. For example, when 
#'     fitting an ASPM sometimes the optimal mathematical model fit can occur 
#'     by depressing the R0 and having an initial depletion >>1. This is only
#'     used internally to datalowSA and so is not formally exported.
#'
#' @param x the parameter value that potentially incurs a penalty
#'
#' @return a single value as a penalty to be added to a Log-Likelihood or SSQ
#'
#' @examples
#' datalowSA:::penalty1(0.5)
#' datalowSA:::penalty1(0.9)
#' datalowSA:::penalty1(0.99)
penalty1 <- function(x){
   ans <- 50 * (abs((1 - abs(x) - 0.5))/0.5)^45
   return(ans)
} # end of penalty1


#' @title plot1 a simple way to plot an xy line plot
#'
#' @description plot1 provides a quick way to plot out a single xy
#'     line plot. It can be used with plotprep to generate a plot
#'     outside of Rstudio or by itself to generate one within Rstudio.
#'     It uses a standard par setup and permits custom labels, font,
#'     and font size (cex). It checks the spread of y and if a ymax is
#'     not given in the parameters finds the ymax and checks to see if
#'     y goes negative in which case it uses getmin, so the
#'     y-axis is set to 0 - ymax or ymin - ymax
#'
#' @param x The single vector of x data
#' @param y the single vector of y data. If more are required they can
#'     be added spearately after calling plot1.
#' @param xlabel the label fot the x-axis, defaults to empty
#' @param ylabel the label fot the y-axis, defaults to empty
#' @param type the type of plot "l" is for line, the default, "p" is
#'     points. If you want both plot a line and add points afterwards.
#' @param usefont which font to use, defaults to 7 which is Times bold
#' @param cex the size of the fonts used. defaults to 0.85
#' @param maxy defaults to 0, which does nothing. If a value is given
#'     then this value is used rather than estimating from the input y
#' @param defpar if TRUE then plot1 will declare a par statement. If false it
#'     will expect one outside the function. In this way plot1 can be
#'     used when plotting multiple graphs, perhaps as mfrow=c(2,2)
#' @param inpch the input character type if using type="p", default=16
#' @param incol the colour to use for the line or points, default = black
#'
#' @return nothing but it does plot a graph and changes the par setting
#' @export
#'
#' @examples
#' \dontrun{
#' x <- rnorm(20,mean=5,sd=1)
#' plot1(x,x,xlabel="x-values",ylabel="yvalues")
#' }
plot1 <- function(x,y,xlabel="",ylabel="",type="l",usefont=7,cex=0.85,
                  maxy=0,defpar=TRUE,inpch=16,incol=1){
  if (defpar) {
    par(mfrow = c(1,1), mai = c(0.45,0.45,0.1,0.05),oma = c(0,0,0,0))
    par(cex = cex, mgp = c(1.35, 0.35, 0), font.axis = usefont,
        font = usefont, font.lab = usefont)
  }
  if (maxy > 0) ymax <- maxy  else ymax <- getmaxy(y)
  if (min(y,na.rm=TRUE) < 0.0) ymin <- getminy(y) else ymin <- 0.0
  addline <- FALSE
  plot(x,y,type=type,pch=inpch,lwd=2,col=incol,ylim=c(ymin,ymax),yaxs="i",
       ylab=ylabel,xlab=xlabel,cex=cex,panel.first=grid())
} # end of plot1



#' @title plotfish plots the catch and optionally the cpue from fish
#' 
#' @description plotfish uses the matrix of fishery data used in the
#'     datalowSA standard data format. It requires the matrix or data.frame
#'     to contain the columns 'year', 'catch', and optionally 'cpue'.
#'
#' @param fish the matrix or data.frame containing year, catch, and cpue.
#' @param glb the list of biologicals potentially containing the spsname
#' @param ce a logical parameter determining whether to plot the cpue or not.
#'     the default = TRUE
#' @param title determines whether or not the spsname is printed at the top
#'     of the plot. Default = TRUE but for a more formal publication it 
#'     might need to be set to FALSE, which also reallocates the room 
#'     given to the title to the plot.
#' @param fnt the font used in the plot and axes.
#' @param filename default is empty. If a filename is put here a .png file
#'     with that name will be put into the working directory. 
#' @param resol the resolution of the png file, defaults to 200 dpi
#'
#' @return prints the location of the png file produced to the console
#' @export
#'
#' @examples
#' \dontrun{
#'   data(dataspm)
#'   plotfish(dataspm$fish,glb,ce=TRUE,filename="")
#' }
plotfish <- function(fish,glb,ce=TRUE,title=TRUE,fnt=7,filename="",
                        resol=200) {
   colnames(fish) <- tolower(colnames(fish))
   rows <- 1
   if (ce) rows <- 2
   lenfile <- nchar(filename)
   if (lenfile > 3) {
      end <- substr(filename,(lenfile-3),lenfile)
      if (end != ".png") filename <- paste0(filename,".png")
      ifelse(rows == 1,hgt <- 3.0,hgt <- 3.5)
      png(filename=filename,width=5.0,height=hgt,units="in",res=resol)
   } 
   yrs <- fish[,"year"]
   if (title) {
      par(mfrow=c(rows,1),mai=c(0.5,0.45,0.1,0.05),oma=c(0.0,0,1.0,0.0)) 
   } else {
      par(mfrow=c(rows,1),mai=c(0.5,0.45,0.1,0.05),oma=c(0.0,0,0.0,0.0))    
   }   
   par(cex=0.85, mgp=c(1.35,0.35,0), font.axis=fnt,font=fnt,font.lab=fnt)  
   ymax <- getmaxy(fish[,"catch"])
   plot(yrs,fish[,"catch"],type="l",lwd=2,ylab="Catch",xlab="Year",
        ylim=c(0,ymax),yaxs="i",panel.first = grid())
   if (title)
       mtext(glb$spsname,side=3,cex=1.0,line=0,font=fnt,outer=TRUE)
   if (ce) {
      ymax <- getmaxy(fish[,"cpue"])
      plot(yrs,fish[,"cpue"],type="l",lwd=2,ylab="CPUE",xlab="Year",
           ylim=c(0,ymax),yaxs="i",panel.first = grid())
   } 
   if (lenfile > 0) {
      outfile <- paste0(getwd(),"/",filename)
      print(outfile)
      dev.off()
   }
} # end of plotfish


#' @title plotfishery plots the catch and optionally the cpue from fish
#' 
#' @description plotfishery is now deprecated and only calls plotfish.
#'
#' @param fish the matrix or data.frame containing year, catch, and cpue.
#' @param glb the list of biologicals potentially containing the spsname
#' @param ce a logical parameter determining whether to plot the cpue 
#'     or not. the default = TRUE
#' @param title determines whether or not the spsname is printed at the top
#'     of the plot. Default = TRUE but for a more formal publication it 
#'     might need to be set to FALSE, which also reallocates the room 
#'     given to the title to the plot.
#' @param fnt the font used in the plot and axes.
#' @param filename default is empty. If a filename is put here a .png file
#'     with that name will be put into the working directory. 
#' @param resol the resolution of the png file, defaults to 200 dpi
#'
#' @return prints the location of the png file produced to the console
#' @export
plotfishery <- function(fish,glb,ce=TRUE,title=TRUE,fnt=7,filename="",
                     resol=200) {
   plotfish(fish,glb,ce,title,fnt,filename,resol)
}

#' @title plotprep: sets up a window and the par values for a single plot
#'
#' @description plotprep: sets up a window and the par values for a single plot.
#'   it checks to see if a graphics device is open and opens a new one if not.
#'   This is simply a utility function to save typing the standard syntax.
#'   Some of the defaults can be changed. Typing the name without () will
#'   provide a template for modification. If 'windows' is called repeatedly this
#'   will generate a new active graphics device each time leaving the older ones
#'   inactive but present. For quick exploratory plots this behaviour is not
#'   wanted, hence the check if an active device exists already or not.
#' @param width defaults to 6 inches = 15.24cm - width of plot
#' @param height defaults to 3 inches = 7.62cm - height of plot
#' @param plots defaults to c(1,1), but arranges multiple plots. If used it may
#'    be necessary to print out this code and adjust the mai and oma variables
#' @param usefont default is 7 (bold Times); 1 = sans serif, 2 = sans serif bold
#' @param cex default is 0.85, the size of font used for text within the plots
#' @param xmtext default is TRUE; if plots is not c(1,1) this alters the mai and
#'    oma variables for the x-axis to allow for mtexting and avoid the x title
#' @param ymtext default is TRUE; if plots is not c(1,1) this alters the mai and
#'    oma variables for the y-axis to allow for mtexting and avoid the y title
#' @param newdev reuse a previously defined graphics device or make a new one;
#'    defaults to TRUE
#' @param rows defaults to TRUE, determines whether to use mfrow or mfcol
#' @param filename defaults to "" = do not save to a filename. If a filename is
#' @return Checks for and sets up a graphics device and sets the default plotting
#'   par values. This changes the current plotting options!
#' @export plotprep
#' @examples
#' x <- rnorm(1000,mean=0,sd=1.0)
#' plotprep()
#' hist(x,breaks=30,main="",col=2)
plotprep <- function(width=6,height=3.6,plots=c(1,1),usefont=7,cex=0.85,
                     xmtext=TRUE,ymtext=TRUE,
                     newdev=TRUE,rows=TRUE,filename="") {
   if  ((names(dev.cur()) != "null device") & (newdev)) suppressWarnings(dev.off())
   lenfile <- nchar(filename)
   if (lenfile > 3) {
      end <- substr(filename,(lenfile-3),lenfile)
      if (end != ".png") filename <- paste0(filename,".png")
      png(filename=filename,width=width,height=height,units="in",res=300)
   } else {
      if (names(dev.cur()) %in% c("null device","RStudioGD"))
         dev.new(width=width,height=height,noRStudioGD = TRUE)
   }
   firstmai <- 0.45; secondmai <- 0.45; thirdmai <- 0.1
   firstoma <- 0.0; secondoma <- 0.0; thirdoma <- 0.0
   if (sum(plots) != 2) {
      if (xmtext) {
         firstmai <- 0.25
         thirdmai <- 0.05
         firstoma <- 1.0
         thirdoma <- 0.1
      }
      if (ymtext) {
         secondmai <- 0.25
         secondoma <- 1.0
      }
   }
   maival <- c(firstmai,secondmai,thirdmai,0.05)
   omaval <- c(firstoma,secondoma,thirdoma,0.0)
   if (rows) {
      par(mfrow = plots,mai=maival,oma=omaval)
   } else {
      par(mfcol = plots,mai=maival,oma=omaval)
   }
   par(cex=cex, mgp=c(1.35,0.35,0), font.axis=usefont,font=usefont,font.lab=usefont)
   if (lenfile > 0) cat("\n Remember to place 'graphics.off()' after the plot \n")
} # end of plotprep

#' @title quants used in apply to estimate quantiles across a vector
#'
#' @description quants used in 'apply' to estimate quantiles across a vector
#' @param invect vector of values
#' @return a vector of the c(0.025,0.05,0.1,0.5,0.9,0.95,0.975) quantiles
#' @export quants
#' @examples
#' \dontrun{
#'  x <- rnorm(100,mean=5,sd=1)
#'  quants(x)
#' }
quants <- function(invect) {
   ans <- quantile(invect,probs = c(0.025,0.05,0.5,0.9,0.95,0.975),na.rm=T)
   return(ans)
}

#' @title readdata reads in a standard format data file
#'
#' @description readdata reads in a standard format data file. An example of the
#'     standard format is generated by using the function dataTemplate, which
#'     generates an example datafile which can be used to demonstrate the
#'     methods present in datalowSA, or can be used as a template to edit and
#'     input a new or different dataset.
#'
#' @param filename the filename (including the full path if required) containing
#'     the data in the standard format.
#' @param property does the data include a table of length-at-age, maturity-at-
#'     age, weight-at-age, fecundity-at-age, and that kind of thing (see the
#'     standard format for a list of data.)
#' @param verbose Default = TRUE, which prints out details as data is read in.
#'
#' @return a list of three objects, fish includes the Year, Catch, CPUE, and SE
#'     of the CPUE, glb containing an array of biological properties that can
#'     be global, and props containing age, laa, waa, maa, sela.
#' @export
#'
#' @examples
#' \dontrun{
#' dataTemplate(filename="test.csv", title="A test of the functions")
#' ans <- readdata("test.csv",property=FALSE)
#' str(ans)
#' }             # filename="sardine.csv"; 
readdata <- function(filename,property=FALSE,verbose=TRUE) {  # property=FALSE; filename=paste0(datadir,"eastdeepwatershark.csv")
   dat <- readLines(filename)
   spsname <- scan(file=filename,skip=0,what=character(),nlines = 1,quiet=TRUE)
   spsname <- removeEmpty(gsub(",","",spsname))
   pick <- grep("RESILIENCE",dat)[1]
   resilience <- scan(file=filename,skip=pick,what=character(),nlines = 1,quiet=TRUE)
   resilience <- removeEmpty(gsub(",","",resilience))
   pick <- grep("NYRS",dat)[1] + 1
   nyrs <- getsingle(dat[pick])
   if (verbose) cat(spsname,"\n\n resilience = ",resilience,
                    "  Num Years = ",nyrs,"\n\n")
   # get the fishery data into as many columns as required
   pick <- grep("YEARS",dat)[1]
   columns <- tolower(removeEmpty(unlist(strsplit(dat[pick],","))))
   numcols <- length(columns)
   columns <- c("year",columns[2:numcols])
   skips <- pick:(pick+nyrs-1)
   fish <- as.data.frame(matrix(NA,nrow=nyrs,ncol=length(columns),
                                dimnames=list(1:nyrs,columns)))
   for (i in 1:nyrs) { # i = 1
      fish[i,] <- scan(file=filename,skip=skips[i],sep=",",nlines = 1,quiet=TRUE)[1:numcols]
   }
   if (verbose) {
      cat("fish \n\n")
      print(head(fish,10))
      cat("\n")
   }
   glb=list(resilience=resilience,spsname=spsname)
   pick <- grep("BIOLOGY",dat)[1] + 1
   if (is.na(pick)) {
      props <- NULL
   } else {
      maxage <- getsingle(dat[pick])
      M <- getsingle(dat[pick+1])
      Linf <- getsingle(dat[pick+2])
      K <- getsingle(dat[pick+3])
      t0 <- getsingle(dat[pick+4])
      Waa <- getsingle(dat[pick+5])
      Wab <- getsingle(dat[pick+6])
      M50a <- getsingle(dat[pick+7])
      deltaM <- getsingle(dat[pick+8])
      sela50 <- getsingle(dat[pick+9])
      deltaS <- getsingle(dat[pick+10])
      steep <- getsingle(dat[pick+11])
      R0 <- getsingle(dat[pick+12])
      ages <- 0:maxage
      nages <- length(ages)
      glb <- list(maxage=maxage,M=M,
                  Linf=Linf, K=K, t0=t0,
                  Waa=Waa, Wab=Wab,
                  M50a=M50a, deltaM=deltaM,
                  steep=steep,R0=R0,
                  sela50=sela50, deltaS=deltaS,
                  resilience=resilience,
                  nages=nages,ages=ages,nyrs=nyrs,spsname=spsname)
      columns <- c("age","laa","waa","maa","sela","feca")
      props <- as.data.frame(matrix(NA,nrow=nages,ncol=length(columns),
                                    dimnames=list(ages,columns)))   
      if (property) {
         pick <- grep("PROPERTY",dat)[1] + 1
         for (i in 1:nages) {
            props[i,] <- getvector(dat,pick,sep=",") 
            pick <- pick + 1
         }
      } else {
         # now calculate the properties
         laa <- vB(c(Linf,K,t0),ages)
         waa <- (Waa * laa ^ Wab)/1000
         maa <- logist(M50a,deltaM,ages)
         sela <- logist(sela50,deltaS,ages)
         feca <- sela * waa
         props <- as.data.frame(cbind(ages,laa,waa,maa,sela,feca))
      }
   }
   if (verbose) { 
      cat("biology \n")
      print(unlist(glb))
      cat("\n properties \n")
      print(head(props,10))
   }
   #   dat <- readLines(filename)
   pick <- grep("AGE",dat)[1] + 1
   if (is.na(pick)) {
      agedata <- NULL
   } else {
      yrage <- getsingle(dat[pick])
      numsex <- getsingle(dat[(pick+1)])
      ages <- getvector(dat,(pick+2),sep=",")
      agemax <- max(ages)
      nage <- length(ages)
      naa <- matrix(0,nrow=(yrage*numsex),ncol=(nage+2))
      colnames(naa) <- c("year","sex",ages)
      pick <- pick+2  # i = 1
      for (i in 1:(yrage*numsex)) naa[i,] <- getvector(dat,(pick+i),sep=",")
      rownames(naa) <- naa[,1]
      agedata <- list(yrage=yrage,ages=ages,agemax=agemax,nage=nage,naa=naa)
   }
   pick <- grep("LENGTH",dat)[1] + 1
   if (is.na(pick)) {
      lendata <- NULL
   } else {
      yrlen <- getsingle(dat[pick])
      numsexl <- getsingle(dat[(pick+1)])
      lengths <- getvector(dat,(pick+2),sep=",")
      maxlen <- max(lengths)
      nlength <- length(lengths)
      if (verbose) {
         cat("Number of years of Data: ",yrlen,"\n")
         cat("Number of sexes with Data: ",numsexl,"\n")
         cat("Length classes: ",lengths,"\n")
         cat("Number of length classes ",nlength,"\n")
      }
      nal <- matrix(0,nrow=(yrlen*numsexl),ncol=(nlength+2))
      colnames(nal) <- c("year","sex",lengths)
      pick <- pick+2
      for (i in 1:(yrlen*numsexl)) nal[i,] <- getvector(dat,(pick+i),sep=",")
      rownames(nal) <- nal[,1]
      
      lendata <- list(yrlen=yrlen,lengths=lengths,maxlen=maxlen,
                      nlength=nlength,nal=nal)
   }
   ans <- list(fish=fish,glb=glb,props=props,agedata=agedata,lendata=lendata)
   return(ans)
} # end of readdata

#' @title removeEmpty removes empty strings from a vector of strings
#'
#' @description removeEmpty removes empty strings from a vector of strings.
#'     Such spaces often created by spurious commas at the end of lines. It
#'     also removes strings made up only of spaces and removes spaces from
#'     inside of inidivdual chunks of text.
#'
#' @param invect a vector of input strings, possibly containing empty strings
#'
#' @return a possibly NULL vector of strings
#' @export
#'
#' @examples
#' x <- c("1","","2","","   ","3"," ","4","","a string","end")
#' x
#' length(x)
#' length(removeEmpty(x))
#' removeEmpty(x)
removeEmpty <- function(invect) {
   tmp <- gsub(" ","",invect)
   tmp <- tmp[nchar(tmp) > 0]
   return(tmp)
}



#' @title toXL copies a data.frame or matrix to the clipboard
#'
#' @description toXL copies a data.frame or matrix to the clipboard
#'    so one can then switch to Excel and just type <ctrl> + V to paste the
#'    data in place
#'
#' @param x a vector or matrix
#' @param output - a boolean determining whether to print the object to the
#'    screen as well as the clipboard; defaults to FALSE
#' @return Places the object 'x' into the clipboard ready for pasting
#' @export toXL
#' @examples
#' \dontrun{
#' datamatrix <- matrix(data=rnorm(100),nrow=10,ncol=10)
#' colnames(datamatrix) <- paste0("A",1:10)
#' rownames(datamatrix) <- paste0("B",1:10)
#' toXL(datamatrix,output=TRUE)
#' }
toXL <- function(x,output=FALSE) {
   write.table(x,"clipboard",sep="\t")
   if(output) print(x)
}


#' @title vB calculates the predicted von Bertalanffy length at age
#'
#' @description vB calculates length at age for the von Bertalanffy curve.
#'
#' @param par is a vector the first three cells of which are Linf, K, and t0
#'    for the VB curve; the fourth parameter will be sigma, the standard
#'    deviation of the normal likelihoods used with the residuals
#' @param ages is a vector of ages
#'
#' @return a vector of predicted lengths for the vector of ages in 'ages'
#' @export
#'
#' @examples
#' ages <- seq(0,20,1)
#' pars <- c(Linf=50,K=0.3,t0=-1.0,sigma=1.0) # Linf, K, t0, sigma
#' cbind(ages,vB(pars,ages))
vB <- function(par,ages) {
   return(par[1] * (1 - exp(-par[2]*(ages-par[3]))))
}

#' @title which.closest find the number closest to a given value
#'
#' @description which.closest finds either the number in a vector which is
#'     closest to the input value or its index value
#'
#' @param x the value to lookup
#' @param invect the vector in which to lookup the value x
#' @param index should the closest value be returned or its index; default=TRUE
#'
#' @return by default it returns the index in the vector of the value closest to
#'     the input  value
#' @export
#'
#' @examples
#' vals <- rnorm(100,mean=5,sd=2)
#' pick <- which.closest(5.0,vals,index=TRUE)
#' pick
#' vals[pick]
#' vals[c(pick-5):(pick+5)] 
#' which.closest(5.0,vals,index=FALSE)
which.closest <- function(x,invect,index=T) {
   pick <- which.min(abs(invect-x))
   if (index) {
      return(pick)
   } else {
      return(invect[pick])
   }
} # end of which.closest
