#'
#' friends.test: A package that checks if a partition element in a bipartite 
#' graph has friends in the other partition.
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
#' [friends.test] finds the best friends for each row, it is a wrapper for
#' the following functions.
#'
#' [unif.ks.test] tests uniformity of a integer vector, the uniformity corresponds 
#' to the "has-no-friends" uniform null model.
#'
#' [step.fit.ln.likelihoods] fits a integer vector with one-step model, 
#' step contains friends. 
#'
#' [tag.int.ranks] is use by all above toe prepare the integer vector to test. They are ranks of attentian that a collection pays to different tags. The ranking happens inside different collections separately. The ties are resolved at random, to keep the ranks integer.
#'
#' @importFrom utils packageDescription
#' @importFrom data.table frankv
#'
NULL
