#' @title Generate skeleton tree
#'
#' @param sample Data in the form of an atomic vector. Can only contain symbols present in *alphabet*, and needs to be longer than **Nmin**.
#' @param alphabet Set \eqn{A=\{a_1,a_2,...,a_{|A|}\}} of possible symbols for the VLMC. Defaults to sort(unique(sample)).
#' @param Nmin A transition will only be considered "prohibited" (with probability 0) if the empirical probability is 0 after Nmin or more occurences.
#' @param sensibility,alpha Alternatives to Nmin. If provided, \eqn{N_{min}= \lceil\log_{1-s}\alpha - \log_{1-s}|A|\rceil} where \eqn{s} is the sensibility. Both must be provided, see details for more.
#' @param silent If TRUE, no progress messages are printed.
#' @param sep,contextsep Symbol separator for the node names. A separate symbol can be specified for the contexts, if wanted. Both symbols cannot be present in \eqn{A}.
#' @param matrix.only If TRUE, only the transition matrix \eqn{M} is returned. Defaults to false.
#' @param irreductible If TRUE, only the irreductibility is returned. Defaults to false.
#'
#' @return Skeleton of the VLMC adjusted to the sample provided, in three objects:
#'
#' A **Tree**, either in DataFrame format or tree object, with the columns/attributes for each node/context \eqn{w}:
#'  - *context* context.
#'  - *n* number of occurences in the sample. (Needs to have a symbol after the context, if the sample ends in w that ocurrence won't be counted.)
#'  - *transitions* vector of transitions, in TRUE (for possible) or FALSE format.
#'
#' A \eqn{|A|^k} **Transition list**, determining which next symbols have a positive transition probability for each past of order \eqn{k}.
#'
#' A \eqn{|A|^k\times |A|^k} **Skeleton Matrix**, determining which transitions have a postive probability, where \eqn{k} is the order of the skeleton. See details for more.
#'
#' @details
#' Since it is impossible to determine empirically that a probability is 0, a cutoff needs to be specified. Nmin is a simple but arbitrary way of doing it.
#' Sensibility and alpha: Probabilities greater or equal to *sensibility* will be detected, at a *alpha* confidence level.
#'
#' Requirements: \eqn{\alpha\in(0,0.5),\;\;s\in(0,1/|A|)}. Recommended: \eqn{s<0.01,\;s<<1/|A|, \alpha<0.05} due do the large ammount of nodes produced.
#'
#' Alternatively, \deqn{Y\sim Binom(N_{min},s)\Rightarrow P[Y=0]\leq \alpha}
#'
#'
#'
#' Matrix \eqn{M}:
#' The skeleton matrix has all possible k-sized sequences of symbols of \eqn{A}, where \eqn{k} is the order of the skeleton, as its rownames and colnames.
#' For every past (row) \eqn{w}, the entry in column (future) \eqn{w_{-k+1}^{-1}u} (the last \eqn{k-1} symbols of \eqn{w} followed by \eqn{u}) will be 1 if, and only if,
#' a transition from \eqn{w} to \eqn{w_{-k+1}^{-1}u} has probability greater than 0. Alternatively, let \eqn{A^k=\{w^{(i)}, i\in[1,|A|^k]\}} be the set of all sequences of \eqn{k} symbols of \eqn{A}, then
#' \deqn{w^{(j)}=w^{(i)-1}_{-k+1}u \Rightarrow M_{ij}=I\{P[u|w]>0\}}
#' For more information, see \[article in progress\].
#'
#' @export

skeleton = function(sample, alphabet = sort(unique(sample)), Nmin, sensibility=min(0.01,1/length(alphabet)), alpha=0.05,
                             sep = '-', contextsep = sep,
                             silent = F, matrix.only = F, irreductible.only = F){
  #checks
  if(!is.character(sample)) stop('Incorrect sample format. Provide an atomic character vector')
  if(!is.character(alphabet)) stop('Incorrect alphabet format. Provide an atomic character vector')
  if(!all(unique(sample)%in%alphabet)) stop(paste(
    paste(unique(sample)[!unique(sample)%in%alphabet], collapse = ', '), 'are not in the alphabet'))

  if(!silent && missing(alphabet)) print("No alphabet provided, following with unique elements of sample")
  a = length(alphabet)
  if(!identical(alphabet, unique(alphabet))) stop("Repeated elements in alphabet")
  if(any(c(sep,contextsep)%in%alphabet)) stop('sep and contextsep cannot be in alphabet')
  if(!interval.check(alpha, 0, 1/2)) stop('Alpha not between 0 and 1/2')
  if(!interval.check(sensibility, 0, 1/a)) stop('Sensibility not between 0 and 1/|A|')
  if(missing(Nmin)){
    Nmin = ceiling(log(alpha,1-sensibility)-log(a,1-sensibility))
  }
  if(length(sample)<=Nmin) stop("Sample too small for Nmin")
  #params
  A = alphabet
  if(!silent) print("All ok. Configurations:")
  if(!silent) print(paste("Sample size:",length(sample)))
  if(!silent) print(paste("Alphabet:",alphabet,"of size",length(alphabet)))
  if(!silent) print(paste("Nmin :",Nmin))
  #generating function
  skel = startskel(sample, A, Nmin, sep, contextsep)
  if(!silent) print(paste("Initial tree generated with",length(Traverse(skel)),"nodes"))
  sculptskeleton(skel, Nmin, declare = !silent)
  if(!silent) print(paste("Skeleton pruned, order",skel$height -1))
  transitions = expand.transitions(skel, A, sep)
  M = trans.to.matrix(transitions, A, sep)


  if(matrix.only) return(M)
  if(irreductible.only) return(irreductible(M))
  else return(list(skeleton = skel, transitions = transitions, matrix = M))
}
