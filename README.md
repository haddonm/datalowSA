# datalowSA

To install from github it is most simple from inside RStudio using:

if (!require(devtools)){install.packages("devtools")}
devtools::install_github("https://github.com/haddonm/datalowSA",build_vignettes=TRUE)

If using Windows this will also require Rtools.exe to be installed (obtainable from CRAN under the Download R for Windows link).

The steps above will install the vignettes as well (use browseVignettes("datalowSA"). But if you just want the vignettes they are listed above both as HTML and PDF versions and are to be found in the 'datalowSADocs' repository.

'datalowSA' is is a branch from simpleSA (the original branch called humbleSA has been discontinued).

'simpleSA' was written originally during a FRDC project aimed at providing training courses in data poor stock assessment methods (Reducing the Number of Undefined Species in Future Status of Australian Fish Stocks Reports: Phase Two - training in the assessment of data-poor stocks. FRDC Project 2017/102. See Haddon et al, 2019). After I left CSIRO in August 2018, it did not seem right to continue the development of 'simpleSA' as that might have interfered with how CSIRO wanted to progress any development. So I took a complete branch and called it 'humbleSA'. While this name is trying to remind potential users that such assessment methods need to be treated with both caution and compassion, given their very limited nature, it could be considered a confusing name. Hence, here I will continue the development of the methods included using the more descriptive name of 'datalowSA'. 'datalowSA' instead of 'datapoorSA' because some of the included methods (surplus production models, both simple and age-structured) are more like data-moderate methods in their requirement for an index of relative abundance or a biomass estimate or two. 

Development of 'datalowSA' has already moved beyond 'simpleSA', and it now contains code for conducting simple age-structured stock reduction analyses on truly data-poor fisheries such as are found in exploraory fisheries. However, those functions have yet to be completely documented inside a suitable vignette so especialy caution is urged should they be used (though each function contains worked examples).

* 2020-09-03 datalowSA 0.1.2 Modified the incomplete catch-curve vignette to correct an equation and the plotting code for selectCC. Thanks to Andre for pointing out that the multinomial -ve log-likelihood equation had errors. The R-code functions, _selectCC_, _multLL_, and _multinomLL_ are all present and correct. 

Reference:
Haddon, M. Burch, P., Dowling, N., and R. Little (2019) Reducing the Number of Un-defined Species in Future Status of Australian Fish Stocks Reports: Phase Two - training in the assessment of data-poor stocks. FRDC Final Report 2017/102. CSIRO Oceans and Atmosphere and Fisheries Research Development Corpora-tion. Hobart 125 p.
