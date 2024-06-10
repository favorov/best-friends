#'
#' best.friends: A package that describe whether a collection is friendly for a tag of not 
#'
#' We have a set T of tags a set C of collections. Clouds pay attention to tags.
#' The attention that each cloud pays to each tag is represented by a real value. 
#' The attention  can represent any type of relation measure, e.g. fuzzy membership. 
#' The attention values is a \eqn{|T| \times |C|} matrix \eqn{A}.
#'
#' For each tag, we want to identify the collection(s) that specifically prefer(s) the tag. 
#' We say that such a collection is a friend (or the best friend if it is the only) for the tag. 
#' The simplest example: imagine that only one collection pays attention to our tag.
#'
#' See our arxiv paper for details.
#' @keywords internal
"_PACKAGE"
#' @section best.friends functions:
#' [best.friends] finds the best friends for each tag, a wrapper for the the following functions.
#'
#' [unif.ks.test] tests uniformity of a integer vector, the uniformity correcponds to tte "no-friend" null model.
#'
#' [step.fit.ln.likelihoods] fits a integer vector with one-step model, it is for alternative, higher step contains friends. 
#'
#' [tag.int.ranks] is use by all above toe prepare the integer vector to test. They are ranks of attentian that a collection pays to different tags. The ranking happens inside different collections separately. The ties are resolved at random, to keep the ranks integer.
#'
#' @importFrom utils packageDescription
#' @importFrom data.table frankv
#'
NULL
