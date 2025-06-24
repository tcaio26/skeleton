#' @title Get the biggest string that is a suffix of another
#'
#' @param contexts Candidates.
#' @param string Bigger string for which we want a suffix.
#'
#' @description
#' Finds the biggest string among **contexts** that matches exactly the end of **string**.
#'
#' @returns Biggest context matching the end of the string, or NULL if none match.
#'
#' @export
get.max.context = function(contexts, string){
  candidates = contexts[sapply(contexts, function(str) grepl(paste0(str,'$'), string))]
  if(length(candidates)>0) return(candidates[[which.max(nchar(candidates))]])
  return(NULL)
}
