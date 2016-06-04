# Basic data for testing
data <- as.matrix(data.frame(x = c(1, 0, 1), 
                             y = c(0, 1, 1)))
priors <-  c(Normal(1, 0.5), 
             Normal(2, 0.5))

p <- prefEl(data = data, priors = priors)

expect_error(p$infer(estimate = "foo"))

expect_equal(p$infer(estimate = "MAP"), c(1, 2))

p$priors <- c(Normal(1, 1), 
              Exp(0.5))

expect_equal(p$infer(estimate = "MAP"), c(1, 0))

p$priors <- c(Normal(1, 1),
              Exp(-0.5))

expect_equal(p$infer(estimate = "MAP"), c(1,0))

# Want to throw errors at indecisive people
p$priors <- c(Flat(), Flat())
expect_error(p$infer())

# Begin testing actual preferneces with MAP estimate
p$priors <- c(Normal(0,1), 
              Normal(0,1))
p$addPref(BayesPref::`%>%`(1,3))
est <- p$infer()
expect_gt(est[1], est[2])

p$addPref(BayesPref::`%>%`(2,3))
est <- p$infer()
expect_equal(est[1], est[2])

p$addPref(BayesPref::`%>%`(1,2))
est <- p$infer()
expect_gt(est[1], est[2])

# Test posterior mean estimator
est <- p$infer(estimate = "mean")
expect_gt(est[1], est[2])
