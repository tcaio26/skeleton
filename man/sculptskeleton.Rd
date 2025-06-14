% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sculptskeleton.R
\name{sculptskeleton}
\alias{sculptskeleton}
\title{Skeleton pruning}
\usage{
sculptskeleton(t, Nmin, copy = F, print = F)
}
\arguments{
\item{t}{Tree, generated from \link{startskel}.}

\item{Nmin}{A transition will only be considered "prohibited" (with probability 0) if the empirical probability is 0 after Nmin or more occurences.}

\item{copy}{Set to TRUE to preserve the original tree. FALSE will alter the original tree and not return anything, the default.}

\item{print}{If TRUE prints how many nodes are cut by level of the tree.}
}
\description{
Prunes a probability tree generated by \link{startskel}. Made for \link{generate_skeleton}.
}
\details{
For a node w, whose children are leaves, removes the children if:
\itemize{
\item both don't have enough occurrences to be significant.
\item both have no prohibited transitions.
\item both have the same prohibited transitions (really should never happen).
}

Starts from the second to last level of the tree.
}
