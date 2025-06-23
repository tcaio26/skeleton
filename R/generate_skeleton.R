#' @title Generate skeleton tree
#'
#' @param sample Data in the form of an atomic vector. Can only contain symbols present in *alphabet*, and needs to be longer than **Nmin**.
#' @param alphabet Set \eqn{A} of possible symbols for the VLMC. Defaults to sort(unique(sample)).
#' @param Nmin A transition will only be considered "prohibited" (with probability 0) if the empirical probability is 0 after Nmin or more occurences.
#' @param sensibility,alpha Alternatives to Nmin. If provided, \eqn{N_{min}= \lceil\log_{1-s}\alpha - \log_{1-s}|A|\rceil} where \eqn{s} is the sensibility. Both must be provided, see details for more.
#' @param treeAsDataFrame Should the tree output be the result of ToDataFrameTree()? If false, returns a data.tree object. Defaults to TRUE.
#' @param silent If TRUE, no progress messages are printed.
#' @param sep Symbol separator for the node names.
#' @param contextsep Symbol separator for the node contexts.
#'
#' @return Skeleton of the VLMC adjusted to the sample provided, in two objects:
#'
#' A **Tree**, either in DataFrame format or tree object, with the columns/attributes for each node/context \eqn{w}:
#'  - *context* context.
#'  - *n* number of occurences in the sample. (Needs to have a symbols after the context, if the sample ends in w that ocurrence won't be counted.)
#'  - *transitions* vector of transitions, in TRUE (for possible) or FALSE format.
#'
#' A \eqn{|A|^k\times |A|^k}**Skeleton Matrix**, determining which transitions have a postive probability, where \eqn{k} is the order of the skeleton. See \link{skel_matrix} for more details.
#'
#' @details
#' Since it is impossible to determine empirically that a probability is 0, a cutoff needs to be specified. Nmin is a simple but arbitrary way of doing it.
#' Sensibility and alpha: Probabilities greater or equal to *sensibility* will be detected, at a *alpha* confidence level.
#' Requirements: \eqn{\alpha\in(0,0.5),\;\;s<1/|A|}
#'
#' Alternatively, \deqn{Y\sim Binom(N_{min},s)\Rightarrow P[Y=0]\leq \alpha}
#'
#' @export

generate_skeleton = function(sample, alphabet = sort(unique(sample)), Nmin, sensibility=0.05, alpha=0.05,
                             sep = '-', contextsep = sep,
                             asDataFrameTree = T, cleantree=T, silent = F){
  #checks

  if(!(missing(alpha)||(is.double(alpha) && interval_check(alpha,0,0.5, inc.U=T)))) stop("alpha must be a numeric value between 0 and 0.5")
  if(!(missing(sensibility)||(is.double(sensibility) && interval_check(sensibility,0,0.5, inc.U=T)))) stop("sensibility must be a numeric value between 0 and 0.5")
  if(!(missing(Nmin)||(is.numeric(Nmin) && Nmin>0))) stop("Nmin must be a positive integer")

  if(all(missing(sensibility), missing(alpha), missing(Nmin))){
    warning("No tolerance arguments provided, proceeding with default alpha = sensibility = 0.05")
  }
  if(!missing(Nmin)&&length(sample)<=Nmin) stop("Sample length must be larger than Nmin, preferably a lot larger")

  #generating function
  if(missing(Nmin)) Nmin = ceiling(log(alpha, 1-sensibility)-log(length(alphabet),1-sensibility))
  if(Nmin>=length(sample)) stop("sample too small for Nmin")

  if(!silent) print("Everything ok, building tree...")

  tree = startskel(sample, alphabet, Nmin, sep, contextsep)
  if(!silent) print(paste0('Tree built with depth ', tree$height-1))
  if(!silent) print('Pruning tree...')

  sculptskeleton(tree, Nmin, copy = F, declare = F)
  if(!silent) print(paste0("Tree pruned, skeleton has order ", tree$height-1))



  return(list(skel = tree, matrix = matrix))
}
