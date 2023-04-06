#' @title  HWC Index suite
#' @description Calculate HWC Index suite for single species or landscape aggregate
#'
#' @param data
#'
#' @return A `hwciresults` object containing the HWC Index suite.
#' @author Gerard Ryan
#' @export
#'
#' @examples
hwci <- function(
    data # s, character vector of length 1 or length time_series
){

  if(!("hwcidata" %in% class(data))){
    stop("data must be a hwcidata class object")
  }

  t <- data$t
  j <- data$j
  v <- data$v
  m <- data$m
  b <- data$b
  l <- data$l
  w <- data$w
  r <- data$r
  s <- data$s


  h_f <- dplyr::case_when(
    v > j ~ 1,
    TRUE ~ v/j
  )

  h_s <- dplyr::case_when(
    m > v ~ 1,
    v > 0 ~ m/v,
    v == 0 ~ 0
  )

  h_m <- v

  x_h <- (h_f + h_s)/2

  e_f <- dplyr::case_when(
    b > j ~ 1,
    TRUE ~ 1 - b/j
  )

  e_s <- dplyr::case_when(
    b == 0 ~ 0,
    TRUE ~ sapply(X = l, FUN = mean)/w
  ) %>%
    dplyr::if_else(
      . > 1, 1, .
    )

  e_m <- sapply(l, sum)

  x_e <- (e_f + e_s)/2

  w_f <- dplyr::case_when(
    r > v + b ~ 1,
    v + b > 0 ~ r/(v + b),
    TRUE ~ 0
  )

  w_s <- dplyr::case_when(
    r == 0 ~ 0,
    s == "CR" ~ 1,
    s == "EN" ~ 0.8,
    s == "VU" ~ 0.6,
    s == "NT" ~ 0.8,
    TRUE ~ 0.2
  )

  w_m <- r

  x_w <- (w_f + w_s)/2

  X_s <- (x_h * x_e * x_w)^(1/3)

  hwci <- list(
    t = t,
    h_f = h_f,
    h_s = h_s,
    h_m = h_m,
    e_f = e_f,
    e_s = e_s,
    e_m = e_m,
    w_f = w_f,
    w_s = w_s,
    w_m = w_m,
    x_h = x_h,
    x_e = x_e,
    x_w = x_w,
    X_s = X_s
  )

  hwci <- structure(hwci, class = c("hwciresult", class(hwci)))

  hwci

}

#' @export
print.hwciresult <- function(x){
  cat("HWC index suite:\n")
  cat(str(x, 1))
}

