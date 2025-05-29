#' @title Initializes the tree
#'
#' @param sample_txt Data for the tree, in the format of a single string
#' @param Nmin Cutoff for node generation, passes onto \link{genskel}. See \link{generate_skeleton.R} for more details.
#' @param prob Logical, should suffix probabilities be computed? Defaults to false.
#'
#' @return Uncut suffix tree generated from the data as a *data.tree* object.
#'
#' @description
#' Function to initialize a probability tree. Is intended for \link{generate_skeleton}, but can be used on its own to check the data.
#' Does not prune the tree at all.
#'
#' @export

startskel = function(sample_txt, Nmin, prob = F){
  sample_vec = string_to_vec(sample_txt)
  raiz = Node$new("r")
  raiz$index = 1:length(sample_vec)
  raiz$n = length(sample_vec)
  raiz$context = ''
  raiz$dom = 0
  if(prob) raiz$p = sum(sample_vec)/raiz$n

  genskel(raiz, sample_vec, sample_txt, Nmin, prob = prob)
  return(raiz)
}
