## ----setup, include=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(
	echo = TRUE,
	message = FALSE,
	warning = FALSE)
options(knitr.kable.NA = "",
        knitr.table.format = "pandoc")

options("show.signif.stars"=FALSE,"stringsAsFactors"=FALSE,
        "max.print"=50000,"width"=240)

library(datalowSA)
suppressPackageStartupMessages(library(knitr))


## ----plaicedata, echo=TRUE,fig.width=7,fig.height=6,fig.align="center"------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# library(datalowSA)
data(plaice)
str(plaice,max.level=1)
naa <- plaice$agedata$naa
pick <- which(naa[,"sex"] == 1)
yrs <- naa[pick,"year"]
naa <- naa[pick,3:11]
ages <- plaice$agedata$ages
#plotprep()
compyear(naa,yrs,ages,plots=c(3,3),freq=TRUE,border=3)


## ----fitclassicCC, echo=TRUE, fig.width=7,fig.height=4.5,fig.align="center"-------------------------------------------------------------------------------------------------------------------------------------------------------------------
M <- plaice$glb$M
pickage <- 4  # try repeating this but compare with pickage = 3
numaa <- naa[1,]   # repeat this using different years of data
out <- classicCC(M,ages,numaa,pickage,plot=TRUE)


## ----fitclassicCC2, echo=TRUE, fig.width=7,fig.height=4.5,fig.align="center"------------------------------------------------------------------------------------------------------------------------------------------------------------------
M <- plaice$glb$M
pickage <- 4   # try repeating this but compare with pickage = 4
numaa <- naa[5,]   # repeat this using different years of data
out <- classicCC(M,ages,numaa,pickage,plot=TRUE)


## ----printout, echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
print(out)

## ----selectCC, echo=TRUE, fig.width=7,fig.height=4.5,fig.align="center"-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
data(plaice)
glb <- plaice$glb
naa <- plaice$agedata$naa
pick <- which(naa[,"sex"] == 1)
naa <- naa[pick,3:11]
numaa <- naa[2,] 
pars <- c(A50=3.0,delta=0.5,fcur=0.6)
out <- selectCC(glb$M,glb$maxage,numaa,pars,plot=TRUE)


## ----selectCCout, echo=TRUE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
print(out)


