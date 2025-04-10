
# Recover influence function for event study estimates
es_inf_func <- es$inf.function$dynamic.inf.func.e

# Recover variance-covariance matrix
n <- nrow(es_inf_func)
V <- t(es_inf_func) %*% es_inf_func / n / n

# Remove the coefficient normalized to zero
referencePeriodIndex <- which(es$egt == -1)
V    <- V[-referencePeriodIndex,-referencePeriodIndex]
beta <- es$att.egt[-referencePeriodIndex]

nperiods <- nrow(V)
npre     <- sum(1*(es$egt < -1))
npost    <- nperiods - npre
baseVec1 <- basisVector(index=(e+1),size=npost)
baseVecAll <- rep(1/npost, npost)
orig_ci  <- constructOriginalCS(betahat        = beta,
                                sigma          = V,
                                numPrePeriods  = npre,
                                numPostPeriods = npost,
                                l_vec          = baseVecAll)


robust_ci <- createSensitivityResults_relativeMagnitudes(betahat        = beta,
                                                         sigma          = V,
                                                         numPrePeriods  = npre,
                                                         numPostPeriods = npost,
                                                         l_vec          = baseVecAll,
                                                         gridPoints     = 100)
