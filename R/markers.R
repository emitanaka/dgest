
#' Genotype mismatch
#'
#' @param x,y A pair of vectors that contain the genotype labels in a pair of datasets.
#' @rdname genotype-mismatch
#' @return A vector of genotype labels that are mismatched.
#' @seealso [genotype_fuzzy_match_left()]
#' @export
genotype_mismatch_left <- function(x, y) {
  x <- unique(x)
  y <- unique(y)
  x[!x %in% y]
}

#' @rdname genotype-mismatch
#' @export
genotype_mismatch_right <- function(x, y) {
  x <- unique(x)
  y <- unique(y)
  y[!y %in% x]
}

#' @rdname genotype-mismatch
#' @export
genotype_mismatch_full <- function(x, y) {
  unique(c(genotype_mismatch_left(x, y), genotype_mismatch_right(x, y)))
}

geno_comparisons <- function(x, y, ..., ignore_case = TRUE) {
  x_mismatch <- x_mismatch0 <- genotype_mismatch_left(x, y)
  y_mismatch <- y_mismatch0 <- genotype_mismatch_right(x, y)
  if(ignore_case) {
    x_mismatch <- tolower(x_mismatch)
    y_mismatch <- tolower(y_mismatch)
  }
  compares <- expand.grid(x = x_mismatch, y = y_mismatch)
  compares$dist <- stringdist::stringdist(compares$x, compares$y)
  list(dist = compares, x_mismatch = x_mismatch0, y_mismatch = y_mismatch0)
}

#' Attempts to find a closely matching genotype
#'
#' @param x,y A pair of vectors that contain the genotype labels in a pair of datasets.
#' @rdname genotype-fuzzy-match
#' @return A data.frame returning the genotype labels that are closely matched.
#' @export
genotype_fuzzy_match_left <- function(x, y, ...,
                                      name_x = "x", name_y = "y",
                                      ignore_case = TRUE) {
  compares <- geno_comparisons(x, y, ..., ignore_case = ignore_case)
  best_match <- tapply(compares$dist$dist, compares$dist$x, which.min)

  res <- data.frame(x = compares$x_mismatch, y = compares$y_mismatch[best_match])
  colnames(res) <- c(name_x, name_y)
  res
}


#' @rdname genotype-fuzzy-match
#' @export
genotype_fuzzy_match_right <- function(x, y, ...,
                                      name_x = "x", name_y = "y",
                                      ignore_case = TRUE) {
  compares <- geno_comparisons(x, y, ..., ignore_case = ignore_case)
  best_match <- tapply(compares$dist$dist, compares$dist$y, which.min)

  res <- data.frame(x = compares$x_mismatch[best_match], y = compares$y_mismatch)
  colnames(res) <- c(name_x, name_y)
  res
}

#' @rdname genotype-fuzzy-match
#' @export
genotype_fuzzy_match_full <- function(x, y, ...,
                                       name_x = "x", name_y = "y",
                                       ignore_case = TRUE) {
  res <- rbind(genotype_fuzzy_match_right(x, y, ..., name_x = name_x, name_y = name_y, ignore_case = ignore_case),
        genotype_fuzzy_match_left(x, y, ..., name_x = name_x, name_y = name_y, ignore_case = ignore_case))
  res[!duplicated(res), ]
}
