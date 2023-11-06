#'
#' discrete.friends.test.p
#' 
#' Given the attention matrix for each tag and each possible slit of the collections into 
#' set of the friends for the tag and the remainder set, assess the p-values for the split.
#' 
#' If p-value is low and the null is rejected, the tag reliably separates the clouds: 
#' \eqn{m} more friendly to \eqn{t_i} clouds are real friends of the tag, others are not. 
#' The tag is the marker for all its friends.
#' 
#' See [best.friends] documentation for details.
#' 
#' discrete.friends.test.p calculates exact p-value, all the ranks in input are integers
#' 
#' p(q) = \frac{\binom{k}{l}}{n^k}\sum^{n-1-q}_{v_l = 0} \left[(v_l + 1)^{l} - {v_l}^l \right] \left[ (n-q-v_l)^{k-l} - (n-1-q-v_l)^{k-l}\right].
#' @param k description
#' @param l description
#' @param n description
#' @param l description
#' @param q description
#' @param v_l description
