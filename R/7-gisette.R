# Load functions
source("R/1-gpr-iprior-sim-functions.R")

experiment.name <- "Gisette data"
# The Gisette data set (https://archive.ics.uci.edu/ml/datasets/Gisette) con-
# sists of 6000 observations of handwritten digits, namely 3000 ‘4’s and 3000
# ‘9’s. Each obser- vation represents the original 28 × 28 pixel image, with
# added noise variables resulting in a 5000-dimensional vector. We first
# subsampled 1500 of the 6000 observations, giving 760 ‘4’s and 740 ‘9’s—this
# data set was then kept fixed through the subsequent 100 repeats of the
# experiment. The observations are sparse with a large number of 0-entries.
# Binary classification task: "recognise 4's (0)" or "recognise 9's (1)" p =
# 5000 (pixel images), N = 1500
load("data/Gis.RData")
summary(as.factor(GisData$y))
X.orig <- GisData$x
y <- GisData$y
y <- y - 1  # convert to 0 and 1.
N <- length(y)
n <- c(50, 200, 1000)  # subsamples

# Simulations
res.gprlin <- mySim(type = "linear", gpr = TRUE, nsim = 12)  # linear GPR
res.gprfbm <- mySim(type = "fbm", gpr = TRUE, nsim = 12)  # FBM GPR
res.gprfbmoptim <- mySim(type = "fbmoptim", gpr = TRUE, nsim = 8)  # FBM optim GPR

res.iplin <- mySim(type = "linear", nsim = 12)  # Canonical I-prior
res.ipfbm <- mySim(type = "fbm", nsim = 12)  # FBM I-prior
res.ipfbmoptim <- mySim(type = "fbmoptim", nsim = 8)  # FBM optim I-prior

tab <- tabRes("GPR (linear)"      = res.gprlin,
              "GPR (FBM)"         = res.gprfbm,
              "GPR (FBM MLE)"     = res.gprfbmoptim,
              "I-prior (linear)"  = res.iplin,
              "I-prior (FBM)"     = res.ipfbm,
              "I-prior (FBM MLE)" = NA)

# Results from REC
rp.lda5.mean <- c(15.75, 10.58, 9.39)
rp.lda5.se   <- c(0.41, 0.17, 0.15)
rp.lda5      <- meanAndSE(rp.lda5.mean, rp.lda5.se)
rp.qda5.mean <- c(15.53, 10.53, 9.37)
rp.qda5.se   <- c(0.40, 0.19, 0.16)
rp.qda5      <- meanAndSE(rp.qda5.mean, rp.qda5.se)
rp.knn5.mean <- c(15.95, 11.09, 9.57)
rp.knn5.se   <- c(0.46, 0.17, 0.16)
rp.knn5      <- meanAndSE(rp.knn5.mean, rp.knn5.se)
rp.tab <- rbind("RP-LDA5" = rp.lda5, "RP-QDA5" = rp.qda5, "RP-knn5" = rp.knn5)
colnames(rp.tab) <- colnames(tab$tab)

# Calculate ranks
tab.mean <- rbind(tab$tab.mean, "RP-LDA5" = rp.lda5.mean,
                  "RP-QDA5" = rp.qda5.mean, "RP-knn5" = rp.knn5.mean)
tab.se <- rbind(tab$tab.se, "RP-LDA5" = rp.lda5.se,
                "RP-QDA5" = rp.qda5.se, "RP-knn5" = rp.knn5.se)
tab.ranks <- tabRank(tab.mean, tab.se)

# Tabulate results
tab.all <- cbind(rbind(tab$tab, rp.tab), Rank = tab.ranks)
knitr::kable(tab.all, align = "r")

# Plot
plotRes()
