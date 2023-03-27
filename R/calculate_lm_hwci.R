#' Title
#'
#' @param data
#'
#' @return
#' @author Gerard Ryan
#' @export
#'
#' @examples
calculate_lm_hwci <- function(data){
  d <- lapply(data, FUN = function(x){x$X_s})

  kappa <- length(d)

  m <- matrix(unlist(d), ncol = kappa)

  X_l <- apply(m, MARGIN = 1, FUN = prod)^(1/kappa)

  X_l
}
