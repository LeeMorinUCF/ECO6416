

# Sample figures for bias/variance calculations.


num_obs <- 100
num_reps <- 1000

beta_0 <- 0.5
beta_hat_mean <- 0.75
beta_hat_sd <- 0.5

beta_distn <- rnorm(n = num_reps, mean = beta_hat_mean, sd = beta_hat_sd)
hist(x = beta_distn,
     main = 'Estimates of Slope Coefficients from Many Samples',
     xlab = 'beta_hat',
     breaks = 40)
# abline(v = beta_0, col = 'black', lwd = 2, lty = 'dashed')
lines(x = c(beta_0, beta_0),
      y = c(0, 100),
      col = 'black', lwd = 3, lty = 'dashed')




beta_0 <- 2.5
beta_hat_mean <- 2.5
beta_hat_sd <- 1.0

beta_distn <- rnorm(n = num_reps, mean = beta_hat_mean, sd = beta_hat_sd)
hist(x = beta_distn,
     main = 'Estimates of Slope Coefficients from Many Samples',
     xlab = 'beta_hat',
     breaks = 40)
# abline(v = beta_0, col = 'black', lwd = 2, lty = 'dashed')
lines(x = c(beta_0, beta_0),
      y = c(0, 100),
      col = 'black', lwd = 3, lty = 'dashed')





beta_0 <- 0
beta_hat_mean <- -1
beta_hat_sd <- 0.75

beta_distn <- rnorm(n = num_reps, mean = beta_hat_mean, sd = beta_hat_sd)
hist(x = beta_distn,
     main = 'Estimates of Slope Coefficients from Many Samples',
     xlab = 'beta_hat',
     breaks = 40)
# abline(v = beta_0, col = 'black', lwd = 2, lty = 'dashed')
lines(x = c(beta_0, beta_0),
      y = c(0, 100),
      col = 'black', lwd = 3, lty = 'dashed')




beta_0 <- 3
beta_hat_mean <- 7
beta_hat_sd <- 2

beta_distn <- rnorm(n = num_reps, mean = beta_hat_mean, sd = beta_hat_sd)
hist(x = beta_distn,
     main = 'Estimates of Slope Coefficients from Many Samples',
     xlab = 'beta_hat',
     breaks = 40)
# abline(v = beta_0, col = 'black', lwd = 2, lty = 'dashed')
lines(x = c(beta_0, beta_0),
      y = c(0, 200),
      col = 'black', lwd = 3, lty = 'dashed')