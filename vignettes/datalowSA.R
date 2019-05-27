<<<<<<< HEAD
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


## ----SAFSbyprod, echo=FALSE,fig.width=8,fig.height=5------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
addt <- function(txt,x,y,small=FALSE,col=1) {
   cexs <- 1.4
   if (small) cexs <- 0.9
   text(x,y,txt,cex=cexs,font=7,col=col)
}
c1 <- 17
c2 <- 48
c3 <- 75
par(mfrow=c(1,1),mai=c(0.05,0.05,0.05,0.05)) 
par(cex=0.85, mgp=c(1.35,0.35,0))  
plot(1:96,1:96,type="n",xaxt="n",yaxt="n",xlab="",ylab="",bty="n")
addt("Provision of Management Advice",x=70,y=94,col=4)
addt("Collect Data",x=c3,y=90,col=1)
addt("Fishery Dependent or",x=c3,y=87,col=1,small=TRUE)
addt("Fishery Independent",x=c3,y=84,col=1,small=TRUE)
addt("Estimate",x=c3,y=70,col=1)
addt("Performance Measures",x=c3,y=67,col=1)
addt("Do a Stock Assessment",x=c3,y=64,col=1,small=TRUE)
addt("Harvest Decision Rule",x=c3,y=52,col=1)
addt("Estimate a Sustainable Catch",x=c3,y=49,col=1,small=TRUE)
addt("Can be the same,",x=c2,y=52,col=1,small=TRUE)
addt("but not necessarily",x=c2,y=49,col=1,small=TRUE)
addt("Estimate Sustainable Catch",x=c3,y=37,col=1)
addt("Evidence and Assessment",x=c3,y=34,col=1,small=TRUE)
addt("Management Process",x=c3,y=19,col=1)
addt("Socio/Economic/Cultural Factors",x=c3,y=16,col=1,small=TRUE)
addt("Set TAC",x=c2,y=17.5,col=1)
x <- c((c2-10),(c2-10),94,94,(c2-10)); y <- c(10,98,98,10,10)
lines(x,y,lwd=1,lty=2)
addt("SAFS Process",x=c1,y=75,col=4)
addt("Determine Stock Status",x=c1,y=52,col=1)
addt("Compare PMs with Reference Points",x=c1,y=48,col=1,small=TRUE)
addt("Public Perception",x=c1,y=27,col=1)
inc <- 16
x <- c((c1-inc),(c1-inc),(c1+inc),(c1+inc),(c1-inc)); y <- c(21,80,80,21,21)
lines(x,y,lwd=1,lty=2)
x <- c(c3,c3,c3,c3); ya <- c(82,62,47,32); yb <- c(72,54,39,21)
arrows(x0=x,y0=ya,y1=yb,length=0.075,angle=30,code=2,lwd=2)
x <- c(c1,c1); ya <- c(66,46); yb <- c(54,29)
arrows(x0=x,y0=ya,y1=yb,length=0.075,angle=30,code=2,lwd=2)
lines(c(60,c1),c(66,66),lwd=2)
xa <- c(32.5,56); xb <- c(39,61) ; ya <- c(50,50);
arrows(x0=xa,x1=xb,y0=ya,length=0.075,angle=30,code=3,lwd=2)
arrows(x0=(c3-14),x1=(c3-21),y0=17,length=0.075,angle=30,code=2,lwd=2)



## ---- eval=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  browseVignettes("datalowSA")

## ---- eval=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  dataTemplate(filename="eg.csv",title="spsname")

## ----getinvert, echo=TRUE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
data(invert)
print(invert)


## ----getfishdat, echo=TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
data(fishdat)
print(fishdat)


## ----getdataspm, echo=TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
data(dataspm)
print(dataspm)


## ----getplaice, echo=TRUE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
data(plaice)
print(plaice)


=======
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


## ----SAFSbyprod, echo=FALSE,fig.width=8,fig.height=5------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
addt <- function(txt,x,y,small=FALSE,col=1) {
   cexs <- 1.4
   if (small) cexs <- 0.9
   text(x,y,txt,cex=cexs,font=7,col=col)
}
c1 <- 17
c2 <- 48
c3 <- 75
par(mfrow=c(1,1),mai=c(0.05,0.05,0.05,0.05)) 
par(cex=0.85, mgp=c(1.35,0.35,0))  
plot(1:96,1:96,type="n",xaxt="n",yaxt="n",xlab="",ylab="",bty="n")
addt("Provision of Management Advice",x=70,y=94,col=4)
addt("Collect Data",x=c3,y=90,col=1)
addt("Fishery Dependent or",x=c3,y=87,col=1,small=TRUE)
addt("Fishery Independent",x=c3,y=84,col=1,small=TRUE)
addt("Estimate",x=c3,y=70,col=1)
addt("Performance Measures",x=c3,y=67,col=1)
addt("Do a Stock Assessment",x=c3,y=64,col=1,small=TRUE)
addt("Harvest Decision Rule",x=c3,y=52,col=1)
addt("Estimate a Sustainable Catch",x=c3,y=49,col=1,small=TRUE)
addt("Can be the same,",x=c2,y=52,col=1,small=TRUE)
addt("but not necessarily",x=c2,y=49,col=1,small=TRUE)
addt("Estimate Sustainable Catch",x=c3,y=37,col=1)
addt("Evidence and Assessment",x=c3,y=34,col=1,small=TRUE)
addt("Management Process",x=c3,y=19,col=1)
addt("Socio/Economic/Cultural Factors",x=c3,y=16,col=1,small=TRUE)
addt("Set TAC",x=c2,y=17.5,col=1)
x <- c((c2-10),(c2-10),94,94,(c2-10)); y <- c(10,98,98,10,10)
lines(x,y,lwd=1,lty=2)
addt("SAFS Process",x=c1,y=75,col=4)
addt("Determine Stock Status",x=c1,y=52,col=1)
addt("Compare PMs with Reference Points",x=c1,y=48,col=1,small=TRUE)
addt("Public Perception",x=c1,y=27,col=1)
inc <- 16
x <- c((c1-inc),(c1-inc),(c1+inc),(c1+inc),(c1-inc)); y <- c(21,80,80,21,21)
lines(x,y,lwd=1,lty=2)
x <- c(c3,c3,c3,c3); ya <- c(82,62,47,32); yb <- c(72,54,39,21)
arrows(x0=x,y0=ya,y1=yb,length=0.075,angle=30,code=2,lwd=2)
x <- c(c1,c1); ya <- c(66,46); yb <- c(54,29)
arrows(x0=x,y0=ya,y1=yb,length=0.075,angle=30,code=2,lwd=2)
lines(c(60,c1),c(66,66),lwd=2)
xa <- c(32.5,56); xb <- c(39,61) ; ya <- c(50,50);
arrows(x0=xa,x1=xb,y0=ya,length=0.075,angle=30,code=3,lwd=2)
arrows(x0=(c3-14),x1=(c3-21),y0=17,length=0.075,angle=30,code=2,lwd=2)



## ---- eval=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  browseVignettes("datalowSA")

## ---- eval=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  dataTemplate(filename="eg.csv",title="spsname")

## ----getinvert, echo=TRUE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
data(invert)
print(invert)


## ----getfishdat, echo=TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
data(fishdat)
print(fishdat)


## ----getdataspm, echo=TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
data(dataspm)
print(dataspm)


## ----getplaice, echo=TRUE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
data(plaice)
print(plaice)


>>>>>>> 02950d79bbe34812ac9c4d09e13b418822ea40de
