#' fit possible bi-uniform step models for a set of ranks
#' #' See [best.friends] documentation for details.
#'
#' @param ranks vector of ranks of a tag in different collections
#' @param tags.no number of tags, i.e. maximal rank 
#' @returns the ln of the likelihood of the ranks for each step position in \eqn{1 .. max(ranks)+1}; the last is for uniform, non-step case
#' example(tag.int.ranks)
#' steps<-step.ln.likelihoods(TF.ranks[42,],genes.no)
#' @export