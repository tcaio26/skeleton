#' @title Initializes the tree
#'
#' @inheritParams skeleton
#'
#'
#' @return Uncut suffix tree generated from the data as a *data.tree* object.
#'
#' @description
#' Function to initialize a probability tree. Is intended for \link{skeleton}, but can be used on its own to check the data.
#' Does not prune the tree at all. For manual usage, do \link{startskel} -> \link{sculptskeleton} -> \link{expand.transitions} -> \link{trans.to.matrix}.
#'
#' @examples
#' (t = startskel('01000110', Nmin = 1, TRUE))
#' ToDataFrameTree(t, 'context', 'p', 'dom', 'n')
#'
#'
#' @export

startskel = function(sample, alphabet, Nmin, sep = '-', contextsep = sep){
  root = Node$new('r')
  root$context = ''
  root$index = 1:length(sample)
  root$counts = table(factor(sample[root$index], levels = alphabet))
  root$n = length(sample)

  genskel(root, 1, sample, alphabet, Nmin, sep)

  lapply(Traverse(root), function(node){
    transitions = (node$counts>0)
    if(sum(transitions)==0){
      transitions = c(T,T,T,T)
      names(transitions) = alphabet
    } #não assumimos nada para nodos não observados.
    node$transitions = transitions
  })

  return(root)
}
