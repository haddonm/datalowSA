library(MSEtool)

rs <- DLMtool::Red_snapper

fish <- cbind(rs@Year,as.vector(rs@Cat),as.vector(rs@Ind))
rownames(fish) <- fish[,1]
colnames(fish) <- c("year","catch","cpue")

constants <- c(h,rho,maxage)


param <- c(logR0,logh,logq,logsigma)
