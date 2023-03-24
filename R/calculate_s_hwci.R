#' Title
#'
#' @param t
#' @param j
#' @param v
#' @param m
#' @param b
#' @param l
#' @param w
#' @param r
#' @param s
#'
#' @return
#' @author geryan
#' @export
#'
#' @examples
calculate_s_hwci <- function(
    t, #time_series, # integer vector of time steps, e.g. years
    j, #landscape_households, # j, single number or vector of length time_series
    #landscape_population = NULL, # single number or vector of length time_series
    #landscape_mean_hh_size = NULL,  # single number or vector of length time_series
    #u, # hv_individuals, # u, vector of length time_series
    v, # hv_incidents, # v, vector of length time_series
    m, # h_mortalities, # m, vector of length time_series
    b, # e_incidents, # b, vector of length time_series
    l, # e_losses, # l, list of length time_series, where each element i is a vector of length e_incidents[i]
    w, # household wealth
    r, #wv_incidents,
    s = c("LC", "NT", "VU", "EN", "CR", "NE", "DD") # s, character vector of length 1 or length time_series
){

  s <- match.arg(s)


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

  result <- list(
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

  return(result)

}

