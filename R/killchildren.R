#' @title Skeleton Prune
#'
#' @import data.tree
#'
#' @param nodo Node whose children will be cut from the tree.
#'
#' @description
#' Internal function to remove the children of a node. Doesn't return anything and shouldn't be used.
#'
#' @noRd
killchildren = function(nodo){
  nodo$children = NULL
}
