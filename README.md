
<!-- README.md is generated from README.Rmd. Please edit that file -->

# datalowSA

## Installation

Installation of \_\_datalowSA is most easily done within RStudio.

To install datalowSA on a Windows computer will need to install the
latest **Rtools.exe** files, which contain the required C++ compiler and
other tools needed to develop packages. **Rtools** can be obtained from
<https://CRAN.R-project.org> under the *Download R for Windows*
installation link and subsequent window. The only tricky aspect is that
you must add the Rtools binary directory to your Windows path, but the
CRAN page provides clear instructions for doing that. On a Macintosh
computer you should not need Rtools.exe as the required software is
already installed. However, the **devtools** and **Rcpp** package will
be required.

Then the development version of datalowSA can be installed from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")  #  unhash these if you do not have devtools or
# install.packages("Rcpp")      #  Rcpp installed
devtools::install_github("haddonm/datalowSA",build_vignettes=TRUE)
```

You will need **devtools** to install from GitHub and **Rcpp** to
produce the code needed for the catch-MSY functions.

The steps above should install the vignettes as well (use
browseVignettes(“datalowSA”).

### The Vignettes

If you just want the vignettes and FRDC Report then they are to be found
in the ‘<https://www.github.com/haddonm/datalowSADocs>’ repository. Once
there in your browser, perhaps the best way forward is to press the
green “code” button and ask to ‘download ZIP’, which will download a
4.9Mb file containing all the files in the repository, which you can
open in the usual way.

## Development

**datalowSA** is a branch from **simpleSA** (the original branch called
humbleSA has been discontinued).

**simpleSA** was written originally, and rapidly, during a FRDC project
aimed at providing training courses in data poor stock assessment
methods (Reducing the Number of Undefined Species in Future Status of
Australian Fish Stocks Reports: Phase Two - training in the assessment
of data-poor stocks. FRDC Project 2017/102. See Haddon et al, 2019,
which is included in the
‘<https://www.github.com/haddonm/datalowSADocs>’ repository).

After I left CSIRO in August 2018, it would not have been correct to
continue the development of **simpleSA**, as that might have interfered
with how CSIRO wanted to progress any development. So I took a complete
branch and called it ‘humbleSA’. While this name is trying to remind
potential users that such assessment methods need to be treated with
both caution and compassion, given their very limited nature, it could
be considered a confusing name. Hence, here I will continue the
development of the methods included using the more descriptive name of
**datalowSA**. **datalowSA** instead of ‘datapoorSA’ because some of the
included methods (surplus production models, both simple and
age-structured) are more like data-moderate methods in their requirement
for an index of relative abundance or a biomass estimate or two.

Development of **datalowSA** has already moved beyond **simpleSA**, and
it now contains code for conducting simple age-structured stock
reduction analyses on truly data-poor fisheries such as are found in
exploraory fisheries. However, those functions have yet to be completely
documented inside a suitable vignette so caution is especiallyurged
should they be used (though each function contains worked examples).

  - 2020-09-03 datalowSA 0.1.2 Modified the draft and incomplete
    catch-curve vignette to correct an equation and the plotting code
    for selectCC. Thanks to Andre for pointing out that the multinomial
    -ve log-likelihood equation had errors. The R-code functions,
    *selectCC*, *multLL*, and *multinomLL* are all present and correct.

### Reference:

Haddon, M. Burch, P., Dowling, N., and R. Little (2019) Reducing the
Number of Un-defined Species in Future Status of Australian Fish Stocks
Reports: Phase Two - training in the assessment of data-poor stocks.
FRDC Final Report 2017/102. CSIRO Oceans and Atmosphere and Fisheries
Research Development Corpora-tion. Hobart 125 p.
