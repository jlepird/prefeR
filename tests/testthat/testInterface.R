# Validate basic extraction
expect_equal(prefEl(data = c(0,1))$data, c(0,1))

# Validate preference constructors
expect_equal(BayesPref::`%>%`(1,2)[[1]], 1) # No comparison op for strict class
expect_equal(BayesPref::`%>%`(1,2)[[2]], 2)
expect_equal(BayesPref::`%<%`(1,2)[[1]], 2)
expect_equal(BayesPref::`%<%`(1,2)[[2]], 1)
expect_equal(BayesPref::`%=%`(1,2)[[1]], 1) 
expect_equal(BayesPref::`%=%`(1,2)[[2]], 2)
expect_is(BayesPref::`%<%`(1,2), c("list","strict"))
expect_is(BayesPref::`%<%`(1,2), c("list","strict"))
expect_is(BayesPref::`%=%`(1,2), c("list","indif"))

# Preference adders
s <- BayesPref::`%>%`(1,2)
p <- prefEl()
p$addPref(s)
expect_equal(p$strict[[1]], s)
s <- BayesPref::`%=%`(1,2)
p$addPref(s)
expect_equal(p$indif[[1]], s)

# Prior helper functions
expect_equal(Exp(2.0)(1.0), dexp(1.0, 1.0/2, log = TRUE))
expect_equal(Normal(0.0,1.0)(1.0), dnorm(1.0, log = TRUE))
expect_equal(Flat()(1), 0.0)
expect_is(Exp(2.0), c("prior", "Exp"))
expect_is(Normal(0,1), c("prior", "Normal"))
expect_is(Flat(), c("prior", "Flat"))