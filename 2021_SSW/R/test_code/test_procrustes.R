# short unit-test for procrustes analysis

# source function
Rcpp::sourceCpp(here("src", "procrustes.cpp"))

# rotation
theta = runif(1L, -2*pi, 2*pi)
Rstar = matrix(
    c(cos(theta), sin(theta), -1.0 * sin(theta), cos(theta)), 
    nrow = 2, ncol = 2
)

# translation
tstar = c(runif(2L, -1, 1))

# scaling
sstar = runif(1L, .2, 2)

# target
n = 20; k = 2
Xstar = matrix(
    rnorm(n*k), 
    n,
    k
)

# distorted matrix
X = sstar * sweep(Xstar, 2L, tstar, "+") %*% Rstar


# get procrustes transform
res = procrustes(X, Xstar, T, T)

# check results
stopifnot(
    all.equal(res$Xstar, Xstar),
    all.equal(abs(as.numeric(res$t_vec)), abs(tstar)),
    all.equal(abs(Rstar), abs(res$R)),
    all.equal(sstar, 1/res$sigma),
    all.equal(sweep(res$sigma * X %*% res$R, 2L, res$t_vec, "+"), Xstar)
)

message("All checks for procrustes transfrom function passed!")

