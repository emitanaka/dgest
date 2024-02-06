#' Get the concurrence matrix
#'
#' Computes the concurrence matrix for a given GxE data. The connectivity matrix
#' is a synonym for the concurrence matrix. The diagonal elements return the number
#' of genotypes in the corresponding environment. The off-diagonal elements are the
#' number of common genotypes across environments.
#'
#' @param data A data.
#' @param gen The name of the genotype column.
#' @param env The name of the environment column.
#' @param sort The sorting method. "freq" sorts by descending diagonal frequency.
#'
#' @export
concurrence <- function(data, gen, env, sort = "none") {
  res <- data |>
    dplyr::distinct({{gen}}, {{env}}) |>
    #dplyr::select({{gen}}, {{env}}) |>
    table() |>
    crossprod()

  if(is.character(sort)) {
    sort_fn <- get(paste0("sort_", sort))()
  } else if(is.function(sort)) {
    sort_fn <- sort
  }

  res[sort_fn(res), sort_fn(res)]
}

#' @rdname concurrence
#' @export
connectivity <- concurrence

#' Sorting method for the concurrence matrix
#'
#' @param order The ordering method. "desc" or "asc" for `sort_freq` and "rev" or "none" for `sort_hclust`.
#' @param method The method for `hclust` function.
#' @rdname sorting-methods
#' @export
sort_none <- function() {
  function(M) seq(ncol(M))
}

#' @rdname sorting-methods
sort_freq <- function(order = "desc") {
  switch(order,
         "desc" = function(M) order(-diag(M)),
         "asc" = function(M) order(diag(M)),
         cli::cli_abort("The order {cli::col_red(order)} does not exist."))
}

#' @rdname sorting-methods
sort_hclust <- function(method = "single", order = "rev") {
  switch(order,
         "rev" = function(M) rev(hclust(as.dist(max(M) - M), method = method)$order),
         "none" = function(M) hclust(as.dist(max(M) - M), method = method)$order)
}

#' Adorn the concurrence table
#'
#' This function is similar to the adorn_ functions in the janitor package.
#' It adorns the a frequency table with relevant statistics. In this case,
#' it returns a data.frame with the number of genotypes or common genotypes across environment.
#' The percentage shows the proportion of common genotypes across environments where the denominator is
#' the number of genotypes in the corresponding row environment.
#'
#' @param M The concurrence/connectivity matrix.
#' @param envname The name of the environment column.
#' @param digits An integer specifying the number of decimal places.
#'
#' @export
adorn_common_percentages <- function(M, envname = "env", digits = 1L) {
  Ms <- diag(1/diag(M)) %*% M
  res <- matrix(paste(M, paste0("(", sprintf(paste0("%.", digits,"f"), 100 * Ms), "%)")), nrow(M), ncol(M))
  diag(res) <- as.character(diag(M))
  colnames(res) <- colnames(M)
  rownames(res) <- rownames(M)
  as.data.frame(res) |>
    tibble::rownames_to_column(envname)
}
