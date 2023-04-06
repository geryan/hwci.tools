#' @title  Combine HWCI data
#' @description Combines several `hwcidata` objects into a single one (to calculate a landscape mean)
#'
#' @param ... One or more `hwcidata` objects.
#'
#' @return A `hwcidata` object.
#' @author Gerard Ryan
#' @export
#'
#' @examples
combine_hwci_data <- function(...){

  data <- list(...)

  t <- add_list_data(data, "t")
  j <- data[[1]]$j
  v <- add_list_data(data, "v")
  m <- add_list_data(data, "m")
  b <- add_list_data(data, "b")
  l <- extend_list_data(data, "l")
  w <- data[[1]]$w
  r <- add_list_data(data, "r")
  s <- data[[1]]$s

  hwci_data <- list(t = t, j = j, v = v, m = m, b = b, l = l, w = w, r = r, s = s)

  hwci_data <- structure(hwci_data, class = c("hwcidata", class(hwci_data)))

  hwci_data

}


add_list_data <- function(x, y){
  library(rlang)
  library(tibble)
  z <- lapply(
    X = x,
    FUN = function(x, y){
      x %>%
        as_tibble %>%
        pull({{y}})
    },
    y = y
  )

  apply(
    X = matrix(
      unlist(z),
      ncol = length(z)
    ),
    MARGIN = 1,
    FUN =
  )
}

extend_list_data <- function(x, y){

  library(rlang)
  library(tibble)
  z <- lapply(
    X = x,
    FUN = function(x, y){
      x %>%
        as_tibble %>%
        pull({{y}})
    },
    y = y
  )

  zz <- vector("list", length(z[[1]]))

  for(i in 1:length(z[[1]])){
    zz[[i]] <- unlist(sapply(z, `[`, i))
  }

  zz
}
