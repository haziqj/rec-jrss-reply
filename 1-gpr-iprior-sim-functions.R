# Require iprior v0.6.4.9002 or higher for GPR support
library(iprior)
library(RPEnsemble)

# Function to create test and train set
testTrain <- function(n = 50, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  train.index <- sample(1:length(y), size = n, replace = FALSE)
  test.index <- (1:length(y))[-train.index]

  X.train <- X[train.index, ]
  y.train <- y[train.index]
  X.test <- X[test.index, ]
  y.test <- y[test.index]

  list(X.train = X.train, y.train = y.train, X.test = X.test, y.test = y.test)
}

# Function to tabulate mean and se
tabRes <- function(...) {
  this <- list(...)
  K <- length(this)
  tab <- NULL

  for (k in 1:K) {
    tab.mean <- apply(this[[k]], 2, mean)
    tab.se <- apply(this[[k]], 2, sd) / 10
    tab.mean.and.se <- paste0(round(tab.mean, 2), " (",
                              round(tab.se, 2), ")")
    tab <- rbind(tab, tab.mean.and.se)
  }

  rownames(tab) <- names(this)
  colnames(tab) <- colnames(this[[1]])
  as.data.frame(tab)
}

# Function for inner simulations
innerSim <- function(n, kernel, ipriorfunction, gpr, fbmoptim = FALSE) {
  dat <- testTrain(n = n)
  mod <- kernL(dat$y.train, dat$X.train,
               model = list(kernel = kernel, rootkern = gpr))
  if (fbmoptim) {
    mod <- ipriorfunction(mod, silent = TRUE)
  } else {
    mod <- ipriorfunction(mod, control = list(silent = TRUE))
  }
  y.test <- round(predict(mod, newdata = list(dat$X.test)))
  sum(y.test != dat$y.test) / (N - n) * 100
}
# innerSim(50, "FBM", fbmOptim, gpr = FALSE, fbmoptim = TRUE)

# Function for GPR/I-prior simulations
mySim <- function(y = y, X = X.orig, nsim = 100, n = c(50, 100, 200),
                  type = c("linear", "fbm", "fbmoptim"), gpr = FALSE,
                  starting = 1) {
  type <- match.arg(type, c("linear", "fbm", "fbmoptim"))
  res.tmp <- matrix(NA, ncol = length(n), nrow = nsim)
  colnames(res.tmp) <- paste0(c("n = "), n)
  pb <- txtProgressBar(min = 0, max = nsim * length(n), style = 3)

  ipriorfn <- ipriorOptim
  fbmoptim <- FALSE
  if (type == "linear") {
    kernel <- "Canonical"
  }
  if (type == "fbm") {
    kernel <- "FBM"
  }
  if (type == "fbmoptim") {
    kernel <- "FBM"
    ipriorfn <- fbmOptim
    fbmoptim <- TRUE
  }

  count <- 0
  for (i in staring:nsim) {
    for (j in 1:length(n)) {
      res.tmp[i, j] <- innerSim(n[j], kernel = kernel, ipriorfunction = ipriorfn,
                                gpr = gpr, fbmoptim = fbmoptim)
      count <- count + 1
      setTxtProgressBar(pb, count)
    }
  }
  close(pb)

  res.tmp
}
# mySim(nsim = 1)

