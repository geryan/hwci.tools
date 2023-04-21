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
#' allspp_lm <- hwci_lm(sp1_indices, sp2_indices, sp3_indices)
#' allspp_lm
hwci_lm <- function(...){

  data <- list(...)

  if(FALSE %in% sapply(data, FUN = function(x){"hwciresult" %in% class(x)})){
    stop("All data must be a hwciresult class objects")
  }

  d <- lapply(data, FUN = function(x){x$X_s})

  kappa <- length(d)

  m <- matrix(unlist(d), ncol = kappa)

  Psi_m <- apply(m, MARGIN = 1, FUN = prod)^(1/kappa)

  t <- data[[1]]$t

  hwcilm <- list(
    t = t,
    Psi_m = Psi_m
  )

 hwcilm <- structure(hwcilm, class = c("hwci_lm", class(hwcilm)))

 hwcilm

}

#' @export
print.hwci_lm <- function(x){
  cat("HWC landscape mean index:\n")
  str(x)
}


