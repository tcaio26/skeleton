#' @title Tree generation
#'
#' @inheritParams startskel
#' @param node Node to generate from.
#' @param sample_vec,sample_txt Different formats for the sample.
#'
#' @description
#' Internal recursive function to generate two nodes with context **0w** and **1w** from a node with context **w**, as long as
#' \eqn{n > N_{min}} and the empirical probability of 1 is not 0 or 1.
#'
#' Should not be used, only made for \link{startskel}.
#'
#' @keywords internal

genskel = function(node, sample_vec, sample_txt, Nmin, prob = F){
  #resultado 0
  l = node$AddChild(paste0('0',node$name))
  w = paste0('0',node$context)
  i = match_str(sample_txt, w)
  if(any(i=='no matches')){
    l$context = w
    l$index = numeric(0)
    l$n = 0
    l$dom = 0
    if(prob) l$p = numeric(0)
  }
  else{
    n = length(i)
    l$context = w
    l$index = i
    l$n = length(i)
    l$dom = ifelse(sum(sample_vec[i])%in%c(0,n), (sample_vec[i][1] - (1-sample_vec[i][1])), 0) #0 vira -1, 1 continua
    if(prob) l$p = sum(sample_vec[i])/n
    if(n>=Nmin && l$dom==0){
      genskel(l, sample_vec, sample_txt, Nmin, prob)
    }
  }


  #resultado 1
  r = node$AddChild(paste0('1',node$name))
  w = paste0('1',node$context)
  i = match_str(sample_txt, w)
  if(any(i=='no matches')){
    r$context = w
    r$index = numeric(0)
    r$n = 0
    r$dom = 0
    if(prob) r$p = numeric(0)
  }
  else{
    n = length(i)
    r$context = w
    r$index = i
    r$n = length(i)
    r$dom = ifelse(sum(sample_vec[i])%in%c(0,n), (sample_vec[i][1] - (1-sample_vec[i][1])), 0)
    if(prob) r$p = sum(sample_vec[i])/n
    if(n>=Nmin && r$dom==0){
      genskel(r, sample_vec, sample_txt, Nmin, prob)
    }
  }
}
