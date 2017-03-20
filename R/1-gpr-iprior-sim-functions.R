# Require iprior v0.6.4.9002 or higher for GPR support
library(iprior)
library(RPEnsemble)
library(ggplot2)

# For push notifications
userID <- "uyq2g37vnityt1b3yvpyicv6o9h456"
appToken <- "avxnrig1qppsgsw9woghwwmxsobo4a"

# Function to specify decimal places
decPlac <- function(x, k = 2) format(round(x, k), nsmall = k)

# Function to combine mean and se
meanAndSE <- function(x, y) paste0(decPlac(x, 2), " (", decPlac(y, 2), ")")

# Function to return 0 or 1 from linear fit
classLin <- function(x) {
  tmp <- rep(0, length(x))
  tmp[x >= 0.5] <- 1
  tmp
}

# Function to calculate ranks
tabRank <- function(x) {
  tmp <- lapply(x, FUN = rank, ties.method = "min")
  tmp.mat <- matrix(unlist(tmp), ncol = length(tmp), byrow = FALSE)
  res <- rank(apply(tmp.mat, 1, sum), ties.method = "min")
  names(res) <- rownames(x)
  res
}

# Function to create test and train set
testTrain <- function(n, seed = NULL) {
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
  tab.mean <- tab.se <- tab <- NULL

  for (k in 1:K) {
    tab.mean.tmp <- apply(this[[k]], 2, mean)
    tab.se.tmp <- apply(this[[k]], 2, sd) / 10
    tab.mean.and.se <- meanAndSE(tab.mean.tmp, tab.se.tmp)
    tab.mean <- rbind(tab.mean, tab.mean.tmp)
    tab.se <- rbind(tab.se, tab.se.tmp)
    tab <- rbind(tab, tab.mean.and.se)
  }

  rownames(tab.mean) <- rownames(tab.se) <- rownames(tab) <- names(this)
  colnames(tab.mean) <- colnames(tab.se) <- colnames(tab) <- colnames(this[[1]])
  list(
    tab = as.data.frame(tab),
    tab.mean = as.data.frame(tab.mean),
    tab.se = as.data.frame(tab.se)
  )
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
  y.test <- classLin(predict(mod, newdata = list(dat$X.test)))
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
  for (i in starting:nsim) {
    for (j in 1:length(n)) {
      res.tmp[i, j] <- innerSim(n[j], kernel = kernel, ipriorfunction = ipriorfn,
                                gpr = gpr, fbmoptim = fbmoptim)
      count <- count + 1
      setTxtProgressBar(pb, count)
    }
  }
  close(pb)

  push.message <- paste0(
    experiment.name, ": ", type, ifelse(gpr, " GPR", " I-prior"), " COMPLETED."
  )
  pushoverr::pushover(message = push.message, user = userID, app = appToken)
  res.tmp
}
# mySim(nsim = 1)

# Function to plot
plotRes <- function() {
  suppressMessages(
    plot.df <- reshape2::melt(cbind(tab.mean, id = rownames(tab.mean)))
  )
  id2 <- plot.df$id
  id2.GPR <- grep("GPR", id2)
  id2.Iprior <- grep("I-prior", id2)
  id2 <- rep(1, length(id2))
  id2[id2.GPR] <- 2
  id2[id2.Iprior] <- 3
  id2 <- as.factor(id2)
  suppressMessages(
    plot.se <- reshape2::melt(tab.se)
  )
  plot.df <- cbind(plot.df, se = plot.se[, 2], id2 = id2)
  ggplot(plot.df, aes(x = value, y = id, col = id2)) +
    geom_point() +
    facet_grid(. ~ variable) +
    geom_errorbarh(aes(xmin = value - 1.96 * se, xmax = value + 1.96 * se,
                       height = 0)) +
    labs(x = "Misclassification rate", y = NULL) + guides(col = FALSE)
}
