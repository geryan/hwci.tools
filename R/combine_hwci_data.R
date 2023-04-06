#' @title  Combine HWCI data
#' @description Combines several `hwcidata` objects into a single one (to calculate a landscape mean)
#'
#' @param ... One or more `hwcidata` objects from the same landscape.
#'
#' @return A `hwcidata` object.
#' @details
#' This function assumes that objects passed to `...` are from the same landscape over the same time.
#'
#' In combining objects it takes `t`, `j`, `w`, and `s` from the first object passed, as it is assumed that these are the same over the landscape. `v`, `m`, `b`, and `r` are summed at each time point, and `l` are concatenated at each time point.
#'
#' @author Gerard Ryan
#' @export
#'
#' @examples
#' allspp_dat <- combine_hwci_data(sp1_data, sp2_data, sp3_data)
#' allspp_dat
combine_hwci_data <- function(...){

  data <- list(...)

  t <- data[[1]]$t
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
    FUN = sum
  )
}

extend_list_data <- function(x, y){

  library(rlang)
  library(tibble)
  z <- lapply(
    X = x,
    FUN = function(x, y){
      x %>%
        tibble::as_tibble %>%
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


