#' @title Transition Skeleton Matrix
#'
#' @description
#' Generates the transition skeleton matrix from a pruned skeleton. Meant for \link{generate_skeleton}.
#'
#' @param tree tree resulting from \link{sculptskeleton}. Must have context and p attributes if done manually.
#'
#' @details
#' Shorter contexts are expanded by pasting all possible pasts to their beginning. \\n
#' For example, if context w has k-2 symbols, *00w, 10w, 01w,* and *11w* will all be generated, with the same probability as w.
#'
#' For a sequence *w* of length *k* \eqn{w=X^{-1}_{-k}}, its possible transitions are \eqn{w\_0=X^{-1}_{k+1}0, w\_1=X^{-1}_{k+1}1}
#'
#' \eqn{M[w,w\_u]=1} if the transition \eqn{w, w\_u} has probability greater than 0, for \eqn{u\in {0,1}}
#'
#' @return A \eqn{2^k\times 2^k} matrix **M**, where *k* is the maximum length of the **tree**'s contexts.
#'
#' @importFrom tibble tibble
#' @importFrom dplyr left_join
#'
#' @export
skel_matrix = function(tree){
  df = ToDataFrameTree(tree, 'context','p', filterFun = isLeaf) #only leaves

  max_char = max(nchar(df$context))
  matrix_df = tibble(exp_context = character(0), context = character(0))

  for(c in df$context){
    l = max_char-nchar(c)
    if(l==0) matrix_df = rbind(matrix_df, tibble(exp_context = c, context = c))
    fills = apply(expand.grid(replicate(l, c(0,1), simplify = FALSE)), 1, paste, collapse = '')
    contexts = paste0(fills, c)
    matrix_df = rbind(matrix_df, tibble(exp_context = contexts, context = replicate(length(fills),c)))
  }

  matrix_df = left_join(matrix_df, df, by = dplyr::join_by('context'))
  pasts = apply(expand.grid(replicate(max_char, c(0,1), simplify = FALSE)), 1, paste, collapse='')
  m = matrix(0, nrow = 2^max_char, ncol = 2^max_char, dimnames = list(pasts, sort(pasts))) #this combination of sorted and unsorted pasts creates "blocks" in the transitions that could have positive probabilities
  for(p in pasts){
    prob = matrix_df[matrix_df$exp_context==p,]$p
    p_cut = vec_to_string(string_to_vec(p)[2:nchar(p)])
    m[p, paste0(p_cut,'0')] = prob
    m[p, paste0(p_cut,'1')] = 1-prob
  }
  m = ifelse(m>0, 1, m)
  return(m)
}
