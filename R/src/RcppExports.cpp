// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// rank_diff_and_p
NumericVector rank_diff_and_p(NumericVector x);
RcppExport SEXP _best_friends_of_rank_diff_and_p(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(rank_diff_and_p(x));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_best_friends_of_rank_diff_and_p", (DL_FUNC) &_best_friends_of_rank_diff_and_p, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_best_friends_of(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
