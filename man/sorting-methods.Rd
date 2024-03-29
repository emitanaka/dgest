% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/concurrence.R
\name{sort_none}
\alias{sort_none}
\alias{sort_freq}
\alias{sort_hclust}
\title{Sorting method for the concurrence matrix}
\usage{
sort_none()

sort_freq(order = "desc")

sort_hclust(method = "single", order = "rev")
}
\arguments{
\item{order}{The ordering method. "desc" or "asc" for \code{sort_freq} and "rev" or "none" for \code{sort_hclust}.}

\item{method}{The method for \code{hclust} function.}
}
\description{
Sorting method for the concurrence matrix
}
