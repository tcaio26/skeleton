#' @title Generate skeleton tree
#'
#' @param sample Data. Either a string or a numeric vector. Should be longer than Nmin and only contain 0 and 1 symbols.
#' @param Nmin A transition will only be considered "prohibited" (with probability 0) if the empirical probability is 0 after Nmin or more occurences.
#' @param sensibility,alpha Alternatives to Nmin. If provided, \eqn{N_{min}=\log_{1-s}\alpha}where \eqn{s} is the sensibility. Both must be provided, see details for more.
#' @param output "tree", "matrix", "both" or the first letter of each. Defaults to both.
#' @param cleantree If set to TRUE (default), non-leaf nodes' probability, dom, and context are emptied.
#'
#' @return Skeleton of the VLMC adjusted to the sample provided, either as the suffix tree (data.tree object) or the transition matrix.
#'
#' @details
#' Since it is impossible to determine empirically that a probability is 0, a cutoff needs to be specified. Nmin is a simple but arbitrary way of doing it.
#' Sensibility and alpha: Probabilities greater or equal to *sensibility* will be detected, at a *alpha* confidence level.
#'
#' @export

generate_skeleton = function(sample, Nmin=59, sensibility, alpha, output = 'both', cleantree=T){
  use_nmin = missing(sensibility)&missing(alpha)
  #checks
  if(!is.numeric(sample)&&!all(is.character(sample),length(sample)==1)){
    stop("Sample must be a numeric vector or a string")
  }
  if(is.character(sample)) sample = as.numeric(unlist(strsplit(sample,'')))
  if(!all(unique(sample)%in%c(0,1))) stop("Sample must be comprised of 0 and 1 values")
  if(length(sample)<2) stop("Sample must be at least 2 symbols long")

  if(!(missing(alpha)||(is.double(alpha) && interval_check(alpha,0,1)))) stop("alpha must be a numeric value between 0 and 1")
  if(!(missing(sensibility)||(is.double(sensibility) && interval_check(sensibility,0,0.5)))) stop("sensibility must be a numeric value between 0 and 0.5")
  if(!(missing(Nmin)||(is.numeric(Nmin) && Nmin>0))) stop("Nmin must be a positive integer")

  if(all(missing(sensibility), missing(alpha), missing(Nmin))){
    warning("No tolerance arguments provided, proceeding with default Nmin = 59")
    use_nmin=T
  }

  if(xor(missing(sensibility),missing(alpha))){
    warning("Provided only alpha or sensibility. Provide both or none. Proceeding with ", Nmin)
    use_nmin=T
  }
  if(!missing(Nmin)&&length(sample)<=Nmin) stop("Sample length must be larger than Nmin, preferably a lot larger")

  #generating function
  if(!use_nmin) Nmin = ceiling(log(alpha, 1-sensibility))
  if(Nmin>=length(sample)) stop("Nmin too large for sample")

  if(is.numeric(sample)) sample = vec_to_string(sample)

  tree = startskel(sample, Nmin, prob = T)

  sculptskeleton(tree, Nmin, copy = F, print = F)

  if(cleantree){
    nodes = Traverse(tree)
    notleaves = nodes[sapply(nodes, function(node) !isLeaf(node))]
    sapply(notleaves, function(node){
      node$context = ''
      node$p = ''
      node$dom = ''
    })
  }

  #output

  return(tree)
}
