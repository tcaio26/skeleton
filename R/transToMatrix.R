#' @title Converting a list of transitions to a matix
#'
#' @param transitions List of transitions, vectors \eqn{v} of length \eqn{|A|} and name \eqn{w} where \deqn{v_i = I[P[a_i|w]>0],\;\forall a_i\in A}
#' @param A Alphabet of the VLMC.
#' @param sep Character separator used in the generation of the skeleton.
#'
#' @returns A transition matrix. See \link{skel_matrix} for more details.
#'
#' @export
transToMatrix = function(transitions, A, sep){
  M = matrix(0, nrow = length(transitions), ncol = length(transitions),
             dimnames = list(names(transitions), names(transitions)))

  d = length(strsplit(names(transitions)[1], sep)[[1]]) #espaço vazio no início
  for(w in names(transitions)){
    possible_transitions = paste(
      paste(strsplit(w, sep)[[1]][2:d], collapse = sep), A, sep = sep
    )
    M[w,possible_transitions] = transitions[[w]]
  }
  return(M)
}
