#' best.friends.of: A package that provide the best-friend ranking
#'
#' Suppose We have some similarity of interaction measure for pairs of instances, e.q. genes and regulators.
#' We want to know what genes are the specifically best friends for selected regulators and vice versa.
#'
#' Main function: 
#' 
#' @section friends.of functions:
#' best.friends.of.features takes the features x ettities matrix, and for each feature provides the index of an entity that is the potential best friend of the featurefand the corresponding p-value. The p-value is about the null hypothesis that the difference between the best and the nest index is a result of randomess.  
#' friends.of.features does the same, but it considers n (possibly, all) the features as potential friends, and p-values is generated for each feature+entity pair. The p-values tests the null hyposthesis that claims that the differnce of the feature's index in this entity and in the next-by-friendship entity is by random. 
#'
#' @docType package
#' @name best.friends.of
#' @useDynLib best.friends.of
#' @importFrom Rcpp evalCpp
# these two are Rcpp - scecific ivocations 
NULL
