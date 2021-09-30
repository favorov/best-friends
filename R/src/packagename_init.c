#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
 Check these declarations against the C/Fortran source code.
 */
//https://stackoverflow.com/questions/42313373/r-cmd-check-note-found-no-calls-to-r-registerroutines-r-usedynamicsymbols

/* .Call calls */
extern SEXP _best_friends_rank_diff_and_p_for_the_best(SEXP);
extern SEXP _best_friends_rank_diff_and_p_for_the_best_n(SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"_best_friends_rank_diff_and_p_for_the_best",   (DL_FUNC) &_best_friends_rank_diff_and_p_for_the_best,   1},
  {"_best_friends_rank_diff_and_p_for_the_best_n", (DL_FUNC) &_best_friends_rank_diff_and_p_for_the_best_n, 2},
  {NULL, NULL, 0}
};

void R_init_best_friends(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}