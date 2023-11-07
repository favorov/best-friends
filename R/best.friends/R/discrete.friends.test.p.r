#'
#' discrete.friends.test
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
#' p(q) = \frac{\binom{k}{l}}{n^k}\sum^{n-1-q}_{v_l = 0} \left[(v_l + 1)^{l} - v^l_l \right] \left[ (n-q-v_l)^{k-l} - (n-1-q-v_l)^{k-l}\right].
#' @param k description
#' @param l description
#' @param n description
#' @param l description
#' @param q description
#' @param v_l description
# v это ранги этого тэга
# v_l - это l_й по порядку ранг, начиная с l=0 
# (v_0 это самый лучший/самый маленький)
# n - число тэгов
# l - это номер ранга тэга в лучшей из двух коллекций
# q == v_{l+1}-v_{l}
# k - число коллекций
# все v живут в диапазоне от 0 до n-1
