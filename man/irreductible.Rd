% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/irreductible.R
\name{irreductible}
\alias{irreductible}
\title{Irreductibility of a matrix}
\usage{
irreductible(M)
}
\arguments{
\item{M}{A binary square matrix, such as the result of \link{skeleton}.}
}
\value{
Logical result for irreductibility.
}
\description{
Tests the irreductibility of a Matrix.
}
\details{
\eqn{M} is irreductible if
\deqn{\forall i,j \in [1,|A|^k], \exists n \in ℕ | M^n[i,j]>0}
In practice this can be tested with \eqn{n = 1,...,|A|^k-1}.\deqn{Q[i,j] = \mathcal{I}\{(\sum_{l=1}^{|A|^k}M^l)[i,j]>0\}}

\eqn{M} is irreductible \eqn{\Leftrightarrow Q[i,j]>0\forall (i,j)\in [1,|A|^k]\times[1,|A|^k]}.

If \eqn{M} has any null columns, it won't be irreductible.
}
