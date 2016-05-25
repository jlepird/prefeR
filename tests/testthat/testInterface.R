# Validate basic extraction
expect_that(prefEl(data = c(0,1))$data, 
            equals(c(0,1)))