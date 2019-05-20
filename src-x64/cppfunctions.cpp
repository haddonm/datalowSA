#include <Rcpp.h>
 
using namespace Rcpp;

//' @title makebiomC runs the surplus production model inside catch-MSY.
//' 
//' @description runs the surplus production model inside catch-MSY. This is 
//'     written in C++ because it contains two for-loops the whole of which is
//'     run for however many iterations are used. Without using C++ the process
//'     can take 15 minutes rather than 20 seconds. This function is only called
//'     inside sraMSY, which is another function that did not strictly need to 
//'     be exported to the public gaze.
//'
//' @param intheta the vector of parameters for the spm and the biomass
//' @param bd the intial biomass depletion vector
//' @param ct the vector of catches leading to the stock reduction
//' @export
// [[Rcpp::export]]
NumericMatrix makebiomC(NumericVector intheta,NumericVector bd,NumericVector ct) {
   int nyr = ct.size();
   int lenbd = bd.size();
   NumericVector xt(nyr+1);
   NumericVector initB(lenbd);
   NumericMatrix biom(nyr+1,lenbd);
   initB = exp(rnorm(lenbd, 0, intheta(4)));
   for (int j = 0; j < lenbd; ++j) {
      biom(0,j) = bd(j) * intheta(1) * initB(j);
      xt = exp(rnorm(nyr,0,intheta(4)));
      for (int i = 0; i < nyr; ++i) {
         biom((i+1),j) = std::max(((biom(i,j) + intheta(0) * biom(i,j) * (1 - biom(i,j)/intheta(1))) - ct(i)) * xt(i),0.0);
      }
   }
   return biom;
}

