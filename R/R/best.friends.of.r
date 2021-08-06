#' best.friends: A package that describe wheteher a community is a best friend (or one of the best friends) for its member. 
#'
#' We have a list of intersecting communities, and the membership of all the elements is represened by real value. 
#' It can be a fuzzy inclusion or, maybe, different status of members in a  communities. The absence of a relation (E is not a member of C at all)
#' is supposed to be represented by the smallest value, naturally, it is 0 and all the memberships are positive. 
#' The membership is a ExC matrix.
#' 
#' We want to find the community that specifically prefers an element.
#' We say that the community are  friendly to the elements. The simplest: suppose an element that is mebmer of only one community. 
#' The member is a marker for the community (if we see him, all the community is around) and the community is friendly to him (all others do not like him).
#' We say that the community is a friend (or best friend) of an element if it prefers the element more than other commities do and the difference is unlikely to be at random.
#'
#' @section best.friends functions:
#' [best.friend.test] takes the elements x communities matrix, and for each element provides the community that is the potential best friend of the element and the corresponding p-value. The p-value is about the null hypothesis that the difference between the best and the next ranks of the element in communities a result of randomess. If p-value is low and the null is rejected, the community is the best friend of the element and the element is the community marker.
#'
#'
#' [friends.test] does the same, but it considers n (possibly, all) the communities as potential friends for each member, and p-values are generated for each element+community pair. The p-values tests the null hyposthesis that claims that the differnce of the elements's ranks in this community and in the next-by-member-rank-of-the-element community is by random. If p-value is low and the null is rejected, the element reliably separates the comumities.
#'
#' @docType package
#' @name best.friends
#' @useDynLib best.friends
#' @importFrom Rcpp evalCpp
# these two are Rcpp - scecific ivocations 
NULL
