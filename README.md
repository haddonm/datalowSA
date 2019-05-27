# datalowSA
Use devtools::install_github("https://github.com/haddonm/datalowSA",build_vignettes=TRUE)  to install from github. This will also require Rtools.exe to be installed (obtainable from CRAN).

A renaming of humbleSA, which was/is a branch from simpleSA.
simpleSA was written originally during a FRDC project based around training courses in data poor stock assessment methods (FRDC 2017-102.
After I left CSIRO in August 2018, it did not seem right to continue the development of simpleSA as that might have interfered with how CSIRO wanted to progress any development. So I took a complete branch and called it humbleSA. While this name is trying to remind potential users that such assessment methods need to be treated with compassion, given theri very limited nature, it could be considered a confusing name. Hence, here I will continue the development of the methods included using the more descriptive name of datalowSA.

datalowSA instead of datapoorSA because some of the included methods (surplus production models, both simple and age-structured) are more like data-moderate methods in tehir requirement for an index of relative abundance or a biomass estimate or two. 

This repository is a complete branch of simpleSA developed during the implementation of the FRDC project: Reducing the Number of Undefined Species in Future Status of Australian Fish Stocks Reports: Phase Two - training in the assessment of data-poor stocks. FRDC Project 2017/102 (see Haddon et al, 2019). Development of datalowSA will move beyond simpleSA and humbleSA, and it already contains code for conducting simple age-structured stock reduction analyses on truly data-poor fisheries as found in exploraory fisheries.

Reference:
Haddon, M. Burch, P., Dowling, N., and R. Little (2019) Reducing the Number of Un-defined Species in Future Status of Australian Fish Stocks Reports: Phase Two - training in the assessment of data-poor stocks. FRDC Final Report 2017/102. CSIRO Oceans and Atmosphere and Fisheries Research Development Corpora-tion. Hobart 125 p.
