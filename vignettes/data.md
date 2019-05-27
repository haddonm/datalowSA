---
title: "dataTemplate"
author: "Malcolm Haddon"
date: "2019-05-21"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{"Using the dataTemplate and readdata functions"}
  %\VignetteEngine{knitr::knitr}
  %\VignetteEncoding{UTF-8}
---



## Introduction

There are functions relating to data input and formatting to facilitate the use of many of the functions inside __datalowSA__ and __cede__. Essentially there are five objects that can contain data to be used by __datalowSA__. Here we will list them and then later 

1. __fish__ is a data.frame (matrix) that contains at least the _year_ and the _catch_, but can also contain _cpue_ (note all column names are in lower case) along with many other variables with a value for each (or a subset) year.
2. __glb__ is an R list containing a selection of constants (see later).
3. __props__ is another data.frame (matrix) containing properties such as length-at-age, weight-at-age, maturity-at-age, and selectivity-at-age. Indeed, any variable that varies with age could be included in here.
4. __agedata__ is a list of five objects relating to the ageing data.
5. __lendata__ is a similar list of five objects relating to the length data.


To illustrate the standard format of this input data there is a function _dataTemplate_. As this function writes a csv file to a hard disk no example will be run in this vignette but the code would be _dataTemplate(paste0(path,"/","fishery1.csv"))_, which would write a fishery1.csv file into the given path. Such a file can be read by the function _readdata_. In the example below you would have had to run the _dataTemplate_ function first and then, of course, you would need to alter the file path and name from that in the example below. Note also that the data in this template is not internally consistent. The _fish_ and _glb_ objects relate to a deep water species while the age data derives from the English Plaice flatfish data taken from Beverton and Holt (1957).



```r
# Obviously you need to include your own path to where you have stored fishery1.csv  
data("plaice")
str(plaice)
```

```
## List of 5
##  $ fish   :'data.frame':	9 obs. of  4 variables:
##   ..$ year : num [1:9] 1929 1930 1931 1932 1933 ...
##   ..$ catch: num [1:9] 2386 1463 2128 2430 1255 ...
##   ..$ cpue : num [1:9] 0.41 0.257 0.445 0.373 0.277 ...
##   ..$ se   : num [1:9] 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2
##  $ glb    :List of 18
##   ..$ maxage    : num 10
##   ..$ M         : num 0.3
##   ..$ Linf      : num 60
##   ..$ K         : num 0.3
##   ..$ t0        : num -0.9
##   ..$ Waa       : num 1.5
##   ..$ Wab       : num 2.9
##   ..$ M50a      : num 2
##   ..$ deltaM    : num 1.5
##   ..$ steep     : num 0.8
##   ..$ R0        : num 14
##   ..$ sela50    : num 3.38
##   ..$ deltaS    : num 0.955
##   ..$ resilience: chr "medium"
##   ..$ nages     : int 11
##   ..$ ages      : int [1:11] 0 1 2 3 4 5 6 7 8 9 ...
##   ..$ nyrs      : num 9
##   ..$ spsname   : chr "European_Plaice"
##  $ props  :'data.frame':	11 obs. of  6 variables:
##   ..$ ages: num [1:11] 0 1 2 3 4 5 6 7 8 9 ...
##   ..$ laa : num [1:11] 14.2 26.1 34.9 41.4 46.2 ...
##   ..$ waa : num [1:11] 3.29 19.18 44.56 73.24 100.85 ...
##   ..$ maa : num [1:11] 0.0193 0.1231 0.5 0.8769 0.9807 ...
##   ..$ sela: num [1:11] 3.03e-05 6.60e-04 1.42e-02 2.39e-01 8.73e-01 ...
##   ..$ feca: num [1:11] 9.96e-05 1.27e-02 6.33e-01 1.75e+01 8.80e+01 ...
##  $ agedata:List of 5
##   ..$ yrage : num 9
##   ..$ ages  : num [1:9] 2 3 4 5 6 7 8 9 10
##   ..$ agemax: num 10
##   ..$ nage  : int 9
##   ..$ naa   : num [1:18, 1:11] 1929 1930 1931 1932 1933 ...
##   .. ..- attr(*, "dimnames")=List of 2
##   .. .. ..$ : chr [1:18] "1929" "1930" "1931" "1932" ...
##   .. .. ..$ : chr [1:11] "year" "sex" "2" "3" ...
##  $ lendata:List of 5
##   ..$ yrlen  : num 2
##   ..$ lengths: num [1:9] 15 18 21 24 27 30 33 36 39
##   ..$ maxlen : num 39
##   ..$ nlength: int 9
##   ..$ nal    : num [1:4, 1:11] 1990 1990 1998 1998 1 ...
##   .. ..- attr(*, "dimnames")=List of 2
##   .. .. ..$ : chr [1:4] "1990" "1990" "1998" "1998"
##   .. .. ..$ : chr [1:11] "year" "sex" "2" "3" ...
```


## The fish Object

```r
print(plaice$fish)
```

```
##   year    catch      cpue  se
## 1 1929 2385.802 0.4096944 0.2
## 2 1930 1462.544 0.2567071 0.2
## 3 1931 2128.012 0.4445481 0.2
## 4 1932 2429.704 0.3725270 0.2
## 5 1933 1254.771 0.2774982 0.2
## 6 1934 1435.447 0.2707523 0.2
## 7 1935 1808.107 0.3850153 0.2
## 8 1936 1806.873 0.3344746 0.2
## 9 1937 1611.695 0.3930758 0.2
```


Quite often it will occur that one has catch data for a series of years before we have CPUE data and, as you can see, we have replaced the empty cells with the __NA__ that R uses for missing data. This is more useful within R than using -99 or some other standard value. Currently the four columns present in the example are the only ones used within __datalowSA__ and __cede__. However, we are still learning what data the various jurisdictions in Australia actually possess. Once that is known it should be possible to include methods that can usefully integrate any other data types available with a value each year.

The two packages are designed to use the component objects within the data set, and some methods (for example catch-MSY) only use the _fish_ object and two parts of the _glb_ (_spsname_ and _resilience_). If that is all that is to be used (say where only catch data is available) then instead of filling in the whole data template the two objects can be created separately, which would be much more efficient.



```r
year <- 1986:2016
catch <- c(112.9,206.3,95.7,183.1,147.4,198.9,102.1,235.5,247.8,426.8,448,577.4,
           558.5,427.9,509.3,502.4,429.6,360.2,306.2,195.7,210,287.3,214.2,260.6,
           272.2,356.9,345,282.7,285.1,237.8,233.3)
cpue <- c(1.2006,1.3547,1.0585,1.0846,0.9738,1.0437,0.7759,1.0532,1.284,1.3327,
          1.4014,NA,NA,1.142,0.9957,0.8818,0.7635,0.7668,0.7198,0.5997,0.6336,
          0.6936,0.8894,0.8644,0.8442,0.8427,0.8849,0.9964,0.9804,0.957,1.0629)
dat <- makedataset(year,catch,cpue,"testdata","verylow")
dat
```

```
## $fish
##      year catch   cpue
## 1986 1986 112.9 1.2006
## 1987 1987 206.3 1.3547
## 1988 1988  95.7 1.0585
## 1989 1989 183.1 1.0846
## 1990 1990 147.4 0.9738
## 1991 1991 198.9 1.0437
## 1992 1992 102.1 0.7759
## 1993 1993 235.5 1.0532
## 1994 1994 247.8 1.2840
## 1995 1995 426.8 1.3327
## 1996 1996 448.0 1.4014
## 1997 1997 577.4     NA
## 1998 1998 558.5     NA
## 1999 1999 427.9 1.1420
## 2000 2000 509.3 0.9957
## 2001 2001 502.4 0.8818
## 2002 2002 429.6 0.7635
## 2003 2003 360.2 0.7668
## 2004 2004 306.2 0.7198
## 2005 2005 195.7 0.5997
## 2006 2006 210.0 0.6336
## 2007 2007 287.3 0.6936
## 2008 2008 214.2 0.8894
## 2009 2009 260.6 0.8644
## 2010 2010 272.2 0.8442
## 2011 2011 356.9 0.8427
## 2012 2012 345.0 0.8849
## 2013 2013 282.7 0.9964
## 2014 2014 285.1 0.9804
## 2015 2015 237.8 0.9570
## 2016 2016 233.3 1.0629
## 
## $glb
## $glb$spsname
## [1] "testdata"
## 
## $glb$resilience
## [1] "verylow"
## 
## 
## $props
## NULL
## 
## $agedata
## NULL
## 
## $lendata
## NULL
```


In summary, the minimum specification for the _fish_ object is a column of _year_ and a column of _catch_. _cpue_ is optional as are any other columns you wish to add. While __datalowSA__ does not yet use any extra columns they may prove useful to you when plotting out results. Missing data in the _cpue_ time-series should be filled with _NA_ values.


## The glb and props objects

The default _glb_ object only contains the _spsname_ and the _resilience_, the first of which is the first line of the .csv file and the second is identified under the RESILIENCE marker in the .csv file. The __BIOLOGY__ marker is always required in the .csv file which leads to a matrix being defined to contain columns of the ages, the length-at-age, the weight-at-age, the maturity-at-age, the selectivity-at-age, and the fecundity-at-age. Prior to reading in the properties a number of biological parameters are entered, the number depending on whether or not the _property_ parameter is TRUE or not. If TRUE then one first reads in four _glb_ properties, the maxage, the M, the steepness, and the R0 values. The properties will then be read in directly.  

An example file _fisheryprops.csv_ is provided that illustrates the format for a file where the age related properties are read in directly (as may be required is a different growth pattern to the von Bertalanffy curve or the selectivity of maturity are described using other than a logistic curve).

How the props matrix is filled is determined by the _property_ parameter in the _readdata_ function. If the _property_ parameter is left as the default of FALSE, then _readdata_ expects to find a series of constants as described in the "example.csv" file generated by the _dataTemplate_ function. These constant are:

BIOLOGY  

* 80   , maxage  the maximum age, usually a plus group 

* 0.036  , M  natural mortality  

* 39.6   , Linf vB asymptotic maximum length 

* 0.06   , K vB Brody growth coefficient 

* -0.01  , t0 theoretical age at zero length

* 0.0365 , Waa weight at age a parameter from the power growth equation W = waa * Age ^ wab

* 2.965  , Wab weight at age b parameter 

* 31.0   , M50a age at 50% maturity  a classical logistic cure as described in the appendix of the aspm vignette under Stock Dynamics

* 5      , deltaM diff between ages at 50 and 95% maturity 

* 31    , sela50 age at 50% selectivity  similarly a logistic curve as for the maturity-at-age. If selectivity is more likely to be dome shaped (as for example might be expected from a gill-net or hook fishery) this has issues for the assessment because small catches of the older age classes can imply large amounts of unseen biomass.

* 5     , deltaS diff between 50 and 95% selectivity 

* 0.6   , steep stock recruitment steepness. By definition the steepness is the gradient of the stock recruitment curve at a depletion level of 20%. In a model it is one indicator of total stock productivity and resilience.

* 14    , Ln(R0) initial value but this will be estimated; as this is estimated it may be deprecated in later versions of datalowSA.  

To apply the age-structured production model you really need to have estimates of these parameters, either from the fishery under question of from a meta analysis of very similar species.

As an alternative if you have empirically derived estimates of the length-at-age, the weight-at-age, the maturity-at-age, the selectivity-at-age, and the fecundity-at-age, these can be entered directly. This is especially useful if growth is not best described by the power equation, of selectivity is not logistic. Fecundity-at-age does not need to be filled in with anything other than NAs.




```r
# obviously you need to change this path to whereever you kept your copy of this file.
data("invert")
str(invert)
```

```
## List of 5
##  $ fish   :'data.frame':	31 obs. of  7 variables:
##   ..$ year   : num [1:31] 1986 1987 1988 1989 1990 ...
##   ..$ catch  : num [1:31] 283 186 286 235 330 ...
##   ..$ cpue   : num [1:31] 0.701 0.884 0.979 0.837 1.581 ...
##   ..$ se     : num [1:31] 0 0.038 0.041 0.043 0.049 0.05 0.058 0.05 0.05 0.044 ...
##   ..$ geom   : num [1:31] 71.7 93 124.6 139.3 174.5 ...
##   ..$ vessel : num [1:31] 47 47 41 39 25 29 19 21 26 25 ...
##   ..$ records: num [1:31] 1592 1764 1395 1143 727 ...
##  $ glb    :List of 2
##   ..$ resilience: chr "low"
##   ..$ spsname   : chr "TrawlCaught_Invertebrate"
##  $ props  : NULL
##  $ agedata: NULL
##  $ lendata: NULL
```

Note the greatly reduced list of biological properties inside _glb_.


## agedata

This requires the __AGE__ keyword followed immediately by the number of years of age composition data, the number of sexes for which data will be presented, and a vector of the age classes represented in the data. An example might be:

* AGE

* 9 , years of numbers-at-age data 

* 2 , sexes, females first=1 then males=2 

* 2, 3, 4, 5, 6, 7, 8, 9, 10,  age classes 

* 1929,1,328,2120,2783,1128,370,768,237,112,48 

* 1930,1,223,2246,1938,1620,302,106,181,58,18 

* 1931,1,95,2898,3017,1159,591,116,100,82,33 

* followed by more lines of year, sex, ages ...  data


While the fishery might act on two sexes either the sexes have not been distinguished or data is only available for females. In such a case the number of sexes is 1. If you do have age-composition data for both females and males then the number of sexes would be 2. The number of years of data and number of sexes are multiplied together to identify how many lines of age-composition data are expected. If you had five years of female age composition and only 4 years of male age composition data then you would need to add a row of year, 2, NAs .... to balance the observations within each year.

Currently, neither age-composition or length-composition data have methods implemented within __datalowSA__. In theory this R package is for data poor species although visits to different jurisdictions is making it clear that the array of data available is more complex than previously envisaged.

## lendata

The format used for age composition data is also used for length composition data.










