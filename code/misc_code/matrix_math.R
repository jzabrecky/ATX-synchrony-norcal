#### Making sure I understand matrix math in R !!!
### Jordan Zabrecky
## last edited: 07.03.2025

# This code is to make sure I understand matrix math in R in order
# to check predictive model code! :)

#### (1) Making matrices ####

# covariates matrix (observed values)
# rows of each field observations and columns for each covariate type
covariates <- matrix(c(1,2,3,4,5,
                       5,4,3,2,1,
                       2,2,2,2,2), nrow = 5, ncol = 3)
covariates

# parameter estimate (mean parameter estimate)
# the parameter estimate matrix retrieved from the STAN model 
# rows of parameters and number of chains
# for simplicity let's just say we are using thdata:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAACgAAAAkCAYAAAD7PHgWAAABBklEQVR4Xu2XMQrCQBBFBQvR6wgJHsEDpHVjBDvvoBhbI3bWCkZbFUyhFrYiEat0WgmC6AVkdQqbIVmWZAOi82C64b+/bDWZDEEQP4phTLMaa9d003bTGMgu1psF7JVGNzuWPdzs18GDz443rgrIcndXbvW8g1axGfZKo7P2eBXc+WB74a3FGXtiA1kwzfnpqTF7hL3SwDfAaz+BqvjkwYADe6WhglQwJlQwKVQwKakVTGOoYNL5z4JxwBlUMEwqAu9SwTCpCLxLBcOkIvCusoKT9/WFQ6OkIvCukoJwt5rO0sehUVIReBem6ng+OLBXmnKjn4PbGM5PeKnqgXIlo5vHXoL4Nl4ZYqbbEGA7+wAAAABJRU5ErkJggg==e mean of chains
params <- matrix(c(0.5, 2, 1), ncol = 3, nrow = 1)
params

# so, in essence:
# one value for each, cov1 * b1 + cov2 * b2 + cov3 * b3
# for each time step

# we will have predictions the same length as covariates
preds <- matrix(NA, nrow = nrow(covariates), ncol = nrow(params))

# make predictions
for(i in 1:nrow(covariates)) {
  for(j in 1:nrow(params)) {
    preds[i,j] <- covariates[i,]%*%params[j,]
  }
}

# expected:
# 12.5
# 10
# 9.5
# 8
# 6.5

view(preds)
# success!
