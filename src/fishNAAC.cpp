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
//' @param Nt is thge numbers at age at time t
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

//' @title bh calculates the expected Beverton-Holt recruitment
//'
//' @description bh calculate the expected Beverton-Holt stock recruitment 
//'     level from the available spawning biomass, the steepness, R0 and B0.
//'     This would be used when fitting a model to data. 
//'
//' @param SPB the current spawning or mature biomass
//' @param steep the steepness of the Beverton-Holt stock recruitment curve
//' @param R0 the unfished average recruitment level
//' @param B0 the unfished spawning biomass.
//' @export
// [[Rcpp::export]]
double bhC(double SPB, double steep, double R0, double B0) {
  double a, b, recr;
  a = (4.0 * steep * R0)/(5 * steep - 1);
  b = (B0 * (1 - steep))/ (5 * steep - 1);
  recr = ((a * SPB)/(b + SPB));
  return recr;
}


//' @title SpBC calculate spawning biomass from a vector of numbers-at-age
//'
//' @description SpBC calculates the spawning biomass from a vector of
//'     numbers-at-age, Maturity-at-age, and Weight-at-age.
//'
//' @param Nt the numbers-at-age as a vector
//' @param MatA maturity at age vector
//' @param WghtA weight at age vector as kilograms
//' @export
// [[Rcpp::export]]  
double SpBC(NumericVector Nt, NumericVector MatA, NumericVector WghtA) {
  int n = Nt.size();
  double spb = 0.0;
  for (int i = 0; i < n; i++) {
    spb += Nt[i] * MatA[i] * WghtA[i];
  }
  spb = spb/1000.0;
  return spb;
}

//' @title ExBC calculate exploitable biomass 
//'
//' @description ExBC calculates the exploitable biomass from a vector of
//'     numbers-at-age, selectivity-at-age, and Weight-at-age.
//'
//' @param Nt the numbers-at-age as a vector
//' @param Sel the selectivity-at-age vector
//' @param WghtA weight at age vector as kilograms
//' @export
// [[Rcpp::export]]  
double ExBC(NumericVector Nt, NumericVector Sel, NumericVector WghtA) {
  int n = Nt.size();
  double exb = 0.0;
  for (int i = 0; i < n; i++) {
    exb += Nt[i] * Sel[i] * WghtA[i];
  }
  exb = exb/1000.0;
  return exb;
}

