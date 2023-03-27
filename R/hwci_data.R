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
#' @return
#' @export
#'
#' @examples
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

  as.hwcidata(hwci_data)

}



print.hwcidata <- function(x){
  cat("hwcidata object:\n")
  str(x)
}

as.hwcidata <- function(hwcidata){
  as_class(hwcidata, "hwcidata", "list")
}
