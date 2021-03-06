// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// MoransI
double MoransI(SEXP mat1, SEXP r1);
RcppExport SEXP _ICvectorfields_MoransI(SEXP mat1SEXP, SEXP r1SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type mat1(mat1SEXP);
    Rcpp::traits::input_parameter< SEXP >::type r1(r1SEXP);
    rcpp_result_gen = Rcpp::wrap(MoransI(mat1, r1));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_ICvectorfields_MoransI", (DL_FUNC) &_ICvectorfields_MoransI, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_ICvectorfields(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
