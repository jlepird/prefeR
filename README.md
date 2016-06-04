### Package Status
[![Build Status](https://travis-ci.org/jlepird/BayesPref.png)](https://travis-ci.org/jlepird/BayesPref)
[![codecov.io](https://codecov.io/gh/jlepird/BayesPref/coverage.svg?branch=master)](https://codecov.io/gh/jlepird/BayesPref?branch=master)

### What is preference elicitation?
Most real-world decisions must reconcile multiple, competing objectives. In buying a car, you might be concerned about cost, reliability, and performance, but before you can make a decision, you must establish the relative importance of these goals. A common mathematical approach to this problem is to define weights for each of these objectives. Although you might have a ballpark intuition for the weights, it is difficult to set them in a repeatable and defendable manner. 

Preference elicitation relieves some of this burden. Instead of determining the weights directly, you make a series of pairwise comparisons between alternatives: do you prefer car A, car B, or are you indifferent? Research has shown that these pairwise comparisons are far easier to make and much easier to *justify* than explicitly setting the weights directly.  This package implements a preference elicitation algorithm then takes your decisions and uses them to calculate an optimal set of weights based on your choices. It can even suggest which comparisons you should make to get the most accurate weights with the fewest number of queries. 

### Installation
This package is not yet added to CRAN, but you can install it directly from Github with the ```devtools``` package. 
```R
library(devtools)
install_github("jlepird/BayesPref")
```

### Examples
#### Hello, World
```R
library(BayesPref)

# Each column of data is a variable, i.e. objective, and each row
# is an alternative.
p <- prefEl(data = data.frame(x = c(1, 0, 1), 
                              y = c(0, 1, 1)))
p$addPref(1 %>% 3)
p$addPref(2 %>% 3)
p$infer()   # returns that x and y are of equal importance
p$suggest() # suggest compare 1 to 2
p$addPref(1 %=% 2)
p$infer()  # maintains belief that 1 and 2 are equal
```
