library(codeutils)
library(hplot)
library(datalowSA)
library(knitr)


data(fishdat)
fish <- fishdat$fish
glb <- fishdat$glb
props <- fishdat$props
pars <- c(14.0,0.3)
ans <- fitASPM(pars,infish=fish,inglb=glb,inprops=props)
outoptim(ans)

fishery <- dynamics(ans$par,infish=fish,inglb=glb,inprops = props)
kable(fishery,digits=c(0,3,3,1,1,3,3,3,3))


ceCI <- getLNCI(fishery[,"PredCE"],ans$par[2])

plotprep(width=9, height=8)
plotASPM(fishery,CI=ceCI,defineplot=FALSE)



