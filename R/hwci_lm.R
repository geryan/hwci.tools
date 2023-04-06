#' @title  HWC Landscape Mean Index
#' @description Calculate Landscape Mean HWC Index
#'
#' @param ... One or more `hwciresults` objects.
#'
#' @return A `hwci_lm_result` object containing landscape mean
#' @author Gerard Ryan
#' @export
#'
#' @examples
hwci_lm <- function(...){

  data <- list(...)

  if(FALSE %in% sapply(data, FUN = function(x){"hwciresult" %in% class(x)})){
    stop("All data must be a hwciresult class objects")
  }

  d <- lapply(data, FUN = function(x){x$X_s})

  kappa <- length(d)

  m <- matrix(unlist(d), ncol = kappa)

  X_l <- apply(m, MARGIN = 1, FUN = prod)^(1/kappa)

  #X_l <- as.lm_index(X_l)

  X_l <- structure(X_l, class = c("hwci_lm_result", class(X_l)))

  X_l

}

#' @export
print.hwci_lm_result <- function(x){
  cat("HWC landscape mean index:\n")
  cat(x)
}

# as.lm_index <- function(X_l){
#   as_class(X_l, "lm_index")
# }

