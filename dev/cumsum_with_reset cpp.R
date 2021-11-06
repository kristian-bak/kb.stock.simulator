#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
  //
  //   http://www.rcpp.org/
  //   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
  //

  //' @title Cumulative sum with reset
//'
//' @param x binary vector
//' @description `cumsum_with_reset` calculates the cumulative sum of x with 0 as the reset value
//' @export
// [[Rcpp::export]]
NumericVector cumsum_with_reset(IntegerVector x) {
  int n = x.size();
  NumericVector y(n);

  if (x[0] == 1) {
    y[0] = 1;
  } else {
    y[0] = 0;
  }

  for (int i = 1; i < n; ++i) {

    if (x[i] == 1) {
      if (x[i - 1] == 1) {
        y[i] = y[i - 1] + 1;
      } else {
        y[i] = 1;
      }
    } else {
      y[i] = 0;
    }

  }

  return y;

}
