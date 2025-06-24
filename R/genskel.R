#' @title Recursive Tree Generation
#'
#' @inheritParams startskel
#' @param parent Node to generate from.
#' @param level Level of the node in the tree, number of symbols in its context.
#'
#' @description
#' Internal recursive function to generate nodes with context *uw* \eqn{\forall u \in A} from a node with context *w*, as long as
#' \eqn{n > N_{min}} and two or more transitions are possible.
#'
#' Should not be used, only made for \link{startskel}.
#'
#' @keywords internal

genskel = function(parent, level, sample, alphabet, Nmin, sep = '-', contextsep = sep){
  possible_index = parent$index[parent$index>level]

  for(u in alphabet){
    node = parent$AddChild(paste0(u, sep, parent$name))
    node$context = paste0(u, contextsep, parent$context)
    node$index = possible_index[which(sample[possible_index-level]==u)]
    node$counts = table(factor(sample[node$index], levels = alphabet))
    node$n = sum(node$counts)
    if(node$n >= Nmin) genskel(node, level+1, sample, alphabet, Nmin, sep)
  }
}
