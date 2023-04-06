## code to prepare simulated data
library(hwci.tools)
set.seed(1969)

nyears <- 100

time_series <- 1:nyears
k <- 500
ll <- 5
j <- k/ll

v_mu <- 20
v_none   <- rep(0, times = nyears)
v_low    <- rnbinom(n = nyears, mu = v_mu,                   size = v_mu/2)
v_rising <- rnbinom(n = nyears, mu = v_mu + 0.2*time_series, size = v_mu/2)
v_high   <- rnbinom(n = nyears, mu = v_mu * 3,               size = v_mu/2)

m_mu <- 5
m_none <- rep(0, times = nyears)
m_low    <- rnbinom(n = nyears, mu = m_mu,                    size = m_mu/2) %>%
  if_else(v_low < ., v_low, .)
m_rising <- rnbinom(n = nyears, mu = m_mu + 0.05*time_series, size = m_mu/2) %>%
  if_else(v_rising < ., v_rising, .)
m_high   <- rnbinom(n = nyears, mu = m_mu * 3,                size = m_mu/2) %>%
  if_else(v_high < ., v_high, .)

b_mu <- 15
b_none <- rep(0, times = nyears)
b_low    <- rnbinom(n = nyears, mu = b_mu,                    size = b_mu/2)
b_rising <- rnbinom(n = nyears, mu = b_mu + 0.15*time_series, size = b_mu/2)
b_high   <- rnbinom(n = nyears, mu = b_mu * 4,                size = b_mu/2)

l_mu <- 100
l_none <- vector("list", length = nyears)
l_none[1:nyears] <- 0
l_flat_b_low <- lapply(b_low, FUN = function(x){
  rnbinom(n = x, mu = l_mu, size = l_mu/20)
})
l_rising_b_low <- lapply(b_low, FUN = function(x){
  rnbinom(n = x, mu = l_mu + time_series, size = l_mu/20)
})
l_rising_b_low <- mapply(
  FUN = function(x, y){
    rnbinom(n = x, mu = l_mu + y, size = l_mu/20)
  },
  x = b_low,
  y = time_series
)
l_flat_b_rising <- lapply(b_rising, FUN = function(x){
  rnbinom(n = x, mu = l_mu, size = l_mu/20)
})
l_rising_b_rising <- mapply(
  FUN = function(x, y){
    rnbinom(n = x, mu = l_mu + y, size = l_mu/20)
  },
  x = b_rising,
  y = time_series
)

w_bar <- 1000

r_mu <- 5
r_none <- rep(0, times = nyears)
r_low    <- rnbinom(n = nyears, mu = r_mu,                    size = r_mu/2) %>%
  if_else(v_low < ., v_low, .)
r_rising <- rnbinom(n = nyears, mu = r_mu + 0.05*time_series, size = r_mu/2) %>%
  if_else(v_rising < ., v_rising, .)
r_high   <- rnbinom(n = nyears, mu = r_mu * 3,                size = r_mu/2) %>%
  if_else(v_high < ., v_high, .)

s_cr <- "CR"
s_en <- "EN"
s_vu <- "VU"
s_nt <- "NT"
s_lc <- "LC"

#Species 1: large carnivore type - human victims, no economic damage, reprisals
sp1_data <- hwci_data(
  t = time_series,
  j = j,
  v = v_rising,
  m = m_rising,
  b = b_none,
  l = l_none,
  w = w_bar,
  r = r_rising,
  s = "CR"
)

#Species 2: crop pest type - no human incidents, only economic incidents
sp2_data <- hwci_data(
  t = time_series,
  j = j,
  v = v_none,
  m = m_none,
  b = b_rising,
  l = l_flat_b_rising,
  w = w_bar,
  r = r_none,
  s = "LC"
)

#Species 3: elephant type - human and economic incidents, incidents getting worse, few reprisals
sp3_data <- hwci_data(
  t = time_series,
  j = j,
  v = v_rising,
  m = m_rising,
  b = b_rising,
  l = l_rising_b_rising,
  w = w_bar,
  r = r_low,
  s = "EN"
)

## Calculate species indices
sp1_indices <- hwci(data = sp1_data)
sp2_indices <- hwci(data = sp2_data)
sp3_indices <- hwci(data = sp3_data)

## Calculate landscape metrics

#*Landscape mean*
# allspp_lm <- hwci_lm(sp1_indices, sp2_indices, sp3_indices)
#*Landscape aggregate indicators*
# allspp_dat <- combine_hwci_data(sp1_data, sp2_data, sp3_data)

usethis::use_data(sp1_data, overwrite = TRUE)
usethis::use_data(sp2_data, overwrite = TRUE)
usethis::use_data(sp3_data, overwrite = TRUE)
usethis::use_data(sp1_indices, overwrite = TRUE)
usethis::use_data(sp2_indices, overwrite = TRUE)
usethis::use_data(sp3_indices, overwrite = TRUE)

