#' Get the concurrence table
#'
#' Write the concurrence table for a given GxE data.
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

sort_none <- function() {
  function(M) seq(ncol(M))
}

sort_freq <- function(order = "desc") {
  switch(order,
         "desc" = function(M) order(-diag(M)),
         "asc" = function(M) order(diag(M)),
         cli::cli_abort("The order {cli::col_red(order)} does not exist."))
}

sort_hclust <- function(method = "single", order = "rev") {
  switch(order,
         "rev" = function(M) rev(hclust(as.dist(max(M) - M), method = method)$order),
         "none" = function(M) hclust(as.dist(max(M) - M), method = method)$order)
}

