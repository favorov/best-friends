#' fit possible bi-uniform step models for a set of ranks
#' @param ranks vector of ranks of a tag in different collections
#' @returns the weight on the left of the step and the likelihood of the ranks for each step position in \eqn{1 .. max(ranks)}
#'   