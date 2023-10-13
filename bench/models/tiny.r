library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

dogs <- stan_demo("dogs")
dogs