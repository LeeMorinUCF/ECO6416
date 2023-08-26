# Newey-West example:

install.packages('AER')
library(AER)
# fit investment equation
data(Investment)
summary(Investment)

fm <- lm(RealInv ~ RealGNP + RealInt, data = Investment)

summary(fm)


# Newey & West (1994) compute this type of estimator
NeweyWest(fm)

# Translate into standard errors:
sqrt(diag(NeweyWest(fm)))


# The Newey & West (1987) estimator requires specification
# of the lag and suppression of prewhitening
NeweyWest(fm, lag = 4, prewhite = FALSE)

# bwNeweyWest() can also be passed to kernHAC(), e.g.
# for the quadratic spectral kernel
kernHAC(fm, bw = bwNeweyWest)

