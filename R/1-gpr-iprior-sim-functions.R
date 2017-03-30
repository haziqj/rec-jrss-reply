# Require iprior v0.6.4.9002 or higher for GPR support
library(iprior)
library(RPEnsemble)
library(ggplot2)
library(foreach)
library(doSNOW)
no.cores <- detectCores() / 2  # or set number of cores

# For push notifications using pushoverR
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
tabRank <- function(x, y) {
  tmp <- lapply(x + y, FUN = rank, ties.method = "min")
  tmp.mat <- matrix(unlist(tmp), ncol = length(tmp), byrow = FALSE)
  res <- rank(apply(tmp.mat, 1, sum), ties.method = "min")
  names(res) <- rownames(x)
  res
}

# Function to create test and train set
testTrain <- function(n.testTrain, y.testTrain, X.testTrain, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  train.index <- sample(1:length(y.testTrain), size = n.testTrain,
                        replace = FALSE)
  test.index <- (1:length(y.testTrain))[-train.index]
  if (length(test.index) > 1000) {
    test.index <- sample(test.index, size = 1000, replace = FALSE)
  }

  X.train <- X.testTrain[train.index, ]
  y.train <- y.testTrain[train.index]
  X.test <- X.testTrain[test.index, ]
  y.test <- y.testTrain[test.index]

  list(X.train = X.train, y.train = y.train, X.test = X.test, y.test = y.test,
       N = length(y.testTrain))
}

# Function to tabulate mean and se
tabRes <- function(...) {
  this <- list(...)
  K <- length(this)
  tab.mean <- tab.se <- tab <- NULL

  for (k in 1:K) {
    if (any(is.na(this[[k]]))) {
      tab.mean <- rbind(tab.mean, NA)
      tab.se <- rbind(tab.se, NA)
      tab <- rbind(tab, NA)
    } else {
      tab.mean.tmp <- apply(this[[k]], 2, mean)
      tab.se.tmp <- apply(this[[k]], 2, sd) / sqrt(nrow(this[[k]]))
      tab.mean.and.se <- meanAndSE(tab.mean.tmp, tab.se.tmp)
      tab.mean <- rbind(tab.mean, tab.mean.tmp)
      tab.se <- rbind(tab.se, tab.se.tmp)
      tab <- rbind(tab, tab.mean.and.se)
    }
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
innerSim <- function(y.innerSim, X.innerSim, n.innerSim, kernel,
                     ipriorfunction, gpr, fbmoptim = FALSE) {
  dat <- testTrain(n.innerSim, y.innerSim, X.innerSim)
  mod <- kernL(dat$y.train, dat$X.train,
               model = list(kernel = kernel, rootkern = gpr))
  if (fbmoptim) {
    mod <- ipriorfunction(mod, silent = TRUE)
  } else {
    mod <- ipriorfunction(mod, control = list(silent = TRUE))
  }
  y.test <- classLin(predict(mod, newdata = list(dat$X.test)))
  mean(y.test != dat$y.test) * 100
}


# Function for GPR/I-prior simulations (parallelised)
mySim <- function(y.mySim = y, X.mySim = X.orig, nsim = 100, n.mySim = n,
                  type = c("linear", "fbm", "fbmoptim"), gpr = FALSE) {
  type <- match.arg(type, c("linear", "fbm", "fbmoptim"))
  pb <- txtProgressBar(min = 0, max = nsim, style = 3)
  progress <- function(i) setTxtProgressBar(pb, i)

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

  this.exp <- paste0(experiment.name, "- ", type,
                     ifelse(gpr, " GPR", " I-prior"))
  suppressWarnings(file.remove(paste0("save/", this.exp, ".csv")))

  cl <- makeCluster(no.cores)
  registerDoSNOW(cl)
  res <- foreach(i = 1:nsim, .combine = rbind,
                 .packages = c("iprior"),
                 .export = c("innerSim", "classLin", "testTrain"),
                 .options.snow = list(progress = progress)) %dopar%
    {
    res.tmp <- rep(NA, length(n.mySim))
    for (j in 1:length(n.mySim)) {
      res.tmp[j]  <- innerSim(y.mySim, X.mySim, n.mySim[j], kernel = kernel,
                              ipriorfunction = ipriorfn, gpr = gpr,
                              fbmoptim = fbmoptim)
    }
    write.table(res.tmp, file = paste0("save/", this.exp, ".csv"),
                append = TRUE, sep = ",", row.names = TRUE, col.names = TRUE)
    res.tmp
  }
  close(pb)
  stopCluster(cl)
  save.image(experiment.name)

  pushoverr::pushover(message = paste0(this.exp, " COMPLETED."),
                      user = userID, app = appToken)

  colnames(res) <- paste0(c("n = "), n.mySim)
  res
}

# Functions for simulations with random projections
innerSimRP <- function(y.innerSim, X.innerSim, n.innerSim, kernel,
                       ipriorfunction, gpr, fbmoptim = FALSE, B1.innerSim, B2.innerSim) {
  dat <- testTrain(n.innerSim, y.innerSim, X.innerSim)
  small.d <- 5
  A <- RPGenerate(p = ncol(X.innerSim), d = small.d, B2 = B2.innerSim,
                  method = "Gaussian")
  XRP <- lapply(1:B2.innerSim,
                function(x, X = dat$X.train) X %*% A[, small.d * (x - 1) + 1:small.d])
  XRP.error <- rep(NA, B2.innerSim)
  y.test.lot <- matrix(NA, nrow = B1.innerSim, ncol = length(dat$y.test))
  for (i in 1:B1.innerSim) {
    for (j in 1:B2.innerSim) {
      mod <- kernL(dat$y.train, XRP[[j]],
                   model = list(kernel = kernel, rootkern = gpr))
      if (fbmoptim) {
        mod <- ipriorfunction(mod, silent = TRUE)
      } else {
        mod <- ipriorfunction(mod, control = list(silent = TRUE))
      }
      y.train <- classLin(fitted(mod))
      XRP.error[j] <- mean(y.train != y.innerSim) * 100
    }
    minj <- which.min(XRP.error)
    cols <- small.d * (minj - 1) + 1:small.d
    mod <- kernL(dat$y.train, XRP[[minj]],
                 model = list(kernel = kernel, rootkern = gpr))
    if (fbmoptim) {
      mod <- ipriorfunction(mod, silent = TRUE)
    } else {
      mod <- ipriorfunction(mod, control = list(silent = TRUE))
    }
    XRP.test <- dat$X.test %*% A[, cols]
    y.test.lot[i, ] <- classLin(predict(mod, newdata = list(XRP.test)))
  }
  y.test <- classLin(apply(y.test.lot, 1, mean))
  mean(y.test != dat$y.test) * 100
}
# innerSimRP(y, X.orig, 50, "Canonical", ipriorOptim, TRUE, B2 = 5, B1 = 2)

# Functions for simulations with random projections
mySimRP <- function(y.mySim = y, X.mySim = X.orig, nsim = 100, n.mySim = n,
                    B1 = 30, B2 = 5, type = c("linear", "fbm", "fbmoptim"),
                    gpr = FALSE) {
  type <- match.arg(type, c("linear", "fbm", "fbmoptim"))
  pb <- txtProgressBar(min = 0, max = nsim, style = 3)
  progress <- function(i) setTxtProgressBar(pb, i)

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

  this.exp <- paste0(experiment.name, "- ", type,
                     ifelse(gpr, " RP-GPR", " RP-I-prior"))
  suppressWarnings(file.remove(paste0("save/", this.exp, ".csv")))

  cl <- makeCluster(no.cores)
  registerDoSNOW(cl)
  res <- foreach(i = 1:nsim, .combine = rbind,
                 .packages = c("iprior", "RPEnsemble"),
                 .export = c("innerSimRP", "classLin", "testTrain"),
                 .options.snow = list(progress = progress)) %dopar% {
    res.tmp <- rep(NA, length(n.mySim))
    for (k in 1:length(n.mySim)) {
      res.tmp[k]  <- innerSimRP(y.mySim, X.mySim, n.mySim[k], kernel = kernel,
                                ipriorfunction = ipriorfn, gpr = gpr, B1.innerSim = B1,
                                B2.innerSim = B2, fbmoptim = fbmoptim)
    }

    write.table(res.tmp, file = paste0("save/", this.exp, ".csv"),
                append = TRUE, sep = ",", row.names = TRUE, col.names = TRUE)
    res.tmp
  }
  close(pb)
  stopCluster(cl)
  save.image(experiment.name)

  pushoverr::pushover(message = paste0(this.exp, " COMPLETED."),
                      user = userID, app = appToken)

  colnames(res) <- paste0(c("n = "), n.mySim)
  res
}
# mySimRP(y, X.orig, nsim = 4, n.mySim = n, B1 = 2, B2 = 5, "linear", gpr = TRUE)

# Function to plot
plotRes <- function() {
  suppressMessages(
    plot.df <- reshape2::melt(cbind(tab.mean, id = rownames(tab.mean)))
  )
  id2 <- plot.df$id
  id2.GPR <- grep("GPR", id2)
  id2.Iprior <- grep("I-prior \\(", id2)
  id2.IpriorProbit <- grep("I-prior probit", id2)
  id2.RPGPR <- grep("RP5-GPR", id2)
  id2.RPIprior <- grep("RP5-I-prior \\(", id2)
  id2.RPIpriorProbit <- grep("RP5-I-prior probit", id2)
  id2 <- rep(1, length(id2))
  id2[id2.GPR] <- 5
  id2[id2.Iprior] <- 3
  id2[id2.IpriorProbit] <- 2
  id2[id2.RPGPR] <- 4
  # id2[id2.RPIprior] <- 6
  # id2[id2.RPIpriorProbit] <- 7
  id2 <- as.factor(id2)
  suppressMessages(
    plot.se <- reshape2::melt(tab.se)
  )
  plot.df <- cbind(plot.df, se = plot.se[, 2], id2 = id2)
  plot.df$id <- factor(plot.df$id, levels = rev(rownames(tab.all)))
  # plot.df
  ggplot(plot.df, aes(x = value, y = id, col = id2)) +
    geom_point() +
    geom_errorbarh(aes(xmin = value - 1.96 * se, xmax = value + 1.96 * se,
                       height = 0)) +
    facet_grid(. ~ variable) +
    labs(x = "Misclassification rate", y = NULL) + guides(col = FALSE)
}
