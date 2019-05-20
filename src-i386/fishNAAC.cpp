#include <Rcpp.h>
using namespace Rcpp;

//' @title fishNAAC numbers-at-age dynamics using Rcpp.
//' 
//' @description conducts the number-at-age dynamics for use within the
//'     the age-structured production model.
//'
//' @param nyr is literally the number of years of dynamics being run.
//' @param maxa is the maximum age
//' @param hS is half the natural survivorship exp(-M/2)
//' @param hrate is the fully selected annual harvest rate
//' @param glob is a vector of parameters of length at least 3:
//'     index 0 = steepness, 1=B0, 2=R0, 
//' @param maa is the maturity-at-age
//' @param waa is the weight-at-age
//' @param sela is the selectivity-at-age
//' @param Nt
//' 
//' @export
// [[Rcpp::export]]
List fishNAAC(int nyr, int maxa, double hS, double hrate, NumericVector glob,
              NumericVector maa, NumericVector waa, NumericVector sela, NumericMatrix Nt) {
   double spb = 0;
   double reca;
   double recb;
   NumericVector Ct(maxa+1);
   NumericVector mwa(maxa+1);

   for (int i=0; i <= maxa; i++) mwa(i) = maa(i) * waa(i);
   reca = (4.0 * glob(0) * glob(1))/(5 * glob(0) - 1);
   recb = (glob(2) * (1 - glob(0)))/(5 * glob(0) - 1);
   for (int j = 1; j < nyr; ++j) {
      spb = 0;
      for (int i=0; i <= maxa; i++) spb = spb + (maa(i) * waa(i) * Nt(i,j-1)/1000.0);
      //    printf("spb = %f\n",spb);
      Nt(0,j) = (reca * spb)/(recb + spb);
      for (int i=0; i <= maxa; i++) Ct(i) = (Nt(i,(j-1)) * hS) * (hrate * sela(i));
      for (int i = 1; i <= maxa; ++i) Nt(i,j) = ((Nt((i-1),(j-1)) * hS) - Ct(i-1)) * hS; //) * hS;
      Nt(maxa,j) = Nt(maxa,j) + ((Nt(maxa,(j-1)) * hS) - Ct(maxa)) * hS; //) * hS;
   }
   return List::create(Nt,Ct);
}


