#' @title  HWC Index data
#' @description Data object containing data to calculate simplified HWC Index
#'
#' @param t Time series. An integer vector of time steps, e.g. years.
#' @param j Number of households in the landscape. An integer vector of length 1 or `length(t)`.
#' @param v Number of human-victim incidents. An integer vector of `length(t)`.
#' @param m Number of human mortalities. An integer vector of `length(t)`.
#' @param b An integer vector of `length(t)`.
#' @param l Economic loss incidents. A list of `length(t)`, where each element `i` of that list is an integer vector of the value lost in a given incident in time `i`.
#' @param w An integer vector of 1 or `length(t)`.
#' @param r Number of wildlife-victim incidents. An integer vector of `length(t)`.
#' @param s IUCN Red List classification of the species, sub-species, or population of interest. Character vector of length 1, corresponding to one of `"LC"`, `"NT"`, `"VU"`, `"EN"`, `"CR"`, `"NE"`, or `"DD"`.
#'
#' @return A `hwcidata` object ready for analysis.
#' @author Gerard Ryan
#' @export
#'
#' @examples
#'
#' #Timeline
#' nyears <- 100
#' time_series <- 1:nyears
#' #landscape population
#' k <- 500
#' # average household size
#' ll <- 5
#' # number of households in landscape
#' j <- k/ll
#' #Human-victim incidents
#' v_mu <- 20
#' v_rising <- rnbinom(n = nyears, mu = v_mu + 0.2*time_series, size = v_mu/2)
#' #Human mortalities
#' m_mu <- 5
#' m_rising <- rnbinom(n = nyears, mu = m_mu + 0.05*time_series, size = m_mu/2) %>%
#'   if_else(v_rising < ., v_rising, .)
#' #Economic incidents
#' b_none <- rep(0, times = nyears)
#' #Value of economic incidents
#' l_none <- vector("list", length = nyears)
#' # Household wealth (mean)
#' w_bar <- 1000
#' #Wildlife-victim incidents
#' r_mu <- 5
#' r_rising <- rnbinom(n = nyears, mu = r_mu + 0.05*time_series, size = r_mu/2) %>%
#'   if_else(v_rising < ., v_rising, .)
#' # Put together into data object
#' sp1_data <- hwci_data(
#'   t = time_series,
#'   j = j,
#'   v = v_rising,
#'   m = m_rising,
#'   b = b_none,
#'   l = l_none,
#'   w = w_bar,
#'   r = r_rising,
#'   s = "CR"
#' )
#' sp1_data

hwci_data <- function(
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

  hwci_data <- list(t = t, j = j, v = v, m = m, b = b, l = l, w = w, r = r, s = s)

  hwci_data <- structure(hwci_data, class = c("hwcidata", class(hwci_data)))

  hwci_data


}

#' @export
print.hwcidata <- function(x){
  cat("hwcidata object:\n")
  cat(str(x, 1))
}

