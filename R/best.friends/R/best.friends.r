#'
#' best.friends
#' 
#' Find Tags that are best friends to Collections
#' 
#' @param mat A matrix tags x collections of attention that a collection pays to tag.
#' @param threshold The adjusted p-value threshold for KS test for 
#' non-uniformity of ranks.
#' @param p.adjust.method Multiple testing correction method, see \link[stast]{p.adjust}.
#' @param best.no The maximum number of friends for a tag, the default is \code{1}, 
#' i.e. the best friend.
#' @return A data those of tags and collections that are markers and best friends 
#' friends.
#' @importFrom stats p.adjust
#' @examples WIP
#' @export
#' 
best.friends <- function(mat, threshold = 0.05, p.adjust.method = "BH", best.no = 1) {
  #find tags with non-uniform ranks
  all_ranks <- tag.int.ranks(mat)
  adj_nunif_pval <- p.adjust(
      apply(all_ranks, 1, unif.ks.test),
      method = p.adjust.method)

  marker_ranks <- all_ranks[adj_nunif_pval<=threshold, ]

  #find friends that make tag ranks non-uniform
  tag_count <- dim(mat)[1]

  all_friends <- apply(marker_ranks, 1,
                       function(x) best.step.fit(x, tags.no = tag_count))

  #best friends are cases where a tag is a marker in only best.no collections
  best_friends <- all_friends[sapply(all_friends,
                                     function(x) x$population.on.left <= best.no)]

  #format result as 2-column data frame
  #res_pre <- lapply(best_friends, function(x) colnames(marker_ranks)[x$col.on.left])
  
  res_pre <- lapply(seq_along(best_friends), function(x) {
     data.frame(
       tag=names(best_friends[x]),
       collection=colnames(marker_ranks)[best_friends[[x]]$collections.on.left]
     )
  })

  res <- do.call(rbind, res_pre)

  res
}
