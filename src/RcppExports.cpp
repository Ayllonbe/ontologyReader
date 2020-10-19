// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// computegoUIC
std::vector<long double> computegoUIC(List& ontology, std::vector<std::string>& ids);
RcppExport SEXP _ontologyReader_computegoUIC(SEXP ontologySEXP, SEXP idsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List& >::type ontology(ontologySEXP);
    Rcpp::traits::input_parameter< std::vector<std::string>& >::type ids(idsSEXP);
    rcpp_result_gen = Rcpp::wrap(computegoUIC(ontology, ids));
    return rcpp_result_gen;
END_RCPP
}
// readerString
List readerString(String go_url);
RcppExport SEXP _ontologyReader_readerString(SEXP go_urlSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< String >::type go_url(go_urlSEXP);
    rcpp_result_gen = Rcpp::wrap(readerString(go_url));
    return rcpp_result_gen;
END_RCPP
}
// reader
List reader(String go_file);
RcppExport SEXP _ontologyReader_reader(SEXP go_fileSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< String >::type go_file(go_fileSEXP);
    rcpp_result_gen = Rcpp::wrap(reader(go_file));
    return rcpp_result_gen;
END_RCPP
}
// gs2
double gs2(StringVector& setOfGenes, List& annotation, Environment& ontology);
RcppExport SEXP _ontologyReader_gs2(SEXP setOfGenesSEXP, SEXP annotationSEXP, SEXP ontologySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< StringVector& >::type setOfGenes(setOfGenesSEXP);
    Rcpp::traits::input_parameter< List& >::type annotation(annotationSEXP);
    Rcpp::traits::input_parameter< Environment& >::type ontology(ontologySEXP);
    rcpp_result_gen = Rcpp::wrap(gs2(setOfGenes, annotation, ontology));
    return rcpp_result_gen;
END_RCPP
}

RcppExport SEXP _rcpp_module_boot_ontology();

static const R_CallMethodDef CallEntries[] = {
    {"_ontologyReader_computegoUIC", (DL_FUNC) &_ontologyReader_computegoUIC, 2},
    {"_ontologyReader_readerString", (DL_FUNC) &_ontologyReader_readerString, 1},
    {"_ontologyReader_reader", (DL_FUNC) &_ontologyReader_reader, 1},
    {"_ontologyReader_gs2", (DL_FUNC) &_ontologyReader_gs2, 3},
    {"_rcpp_module_boot_ontology", (DL_FUNC) &_rcpp_module_boot_ontology, 0},
    {NULL, NULL, 0}
};

RcppExport void R_init_ontologyReader(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
