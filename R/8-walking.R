# Load functions
source("R/1-gpr-iprior-sim-functions.R")

experiment.name <- "Human activity recognition data"
# The human activity recognition data set
# (http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones)
# consists of p = 561 accelerometer measurements, recorded from a smartphone
# while a subject is performing an activity. We subsampled the data to include
# only the walking and laying activities. In the resulting data set, there are
# 1226 ‘walking’ observations (class 0), and 1407 ‘laying’ observations (class
# 1). Binary classification task: "walking (0)" or "laying (1)" p =
# 561 (accelerometer measurements), N = 2633
load("data/HARWalkLay.RData")
summary(as.factor(HAR2$y))
X.orig <- HAR2$x
y <- HAR2$y
y <- y - 1  # convert to 0 and 1.
N <- length(y)
n <- c(50, 200, 1000)  # subsamples

# Simulations
res.gprlin <- mySim(type = "linear", gpr = TRUE)  # linear GPR
res.gprfbm <- mySim(type = "fbm", gpr = TRUE)  # FBM GPR
# res.gprfbmoptim <- mySim(type = "fbmoptim", gpr = TRUE)  # FBM optim GPR

res.iplin <- mySim(type = "linear")  # Canonical I-prior
res.ipfbm <- mySim(type = "fbm")  # FBM I-prior
# res.ipfbmoptim <- mySim(type = "fbmoptim")  # FBM optim I-prior

tab <- tabRes("GPR (linear)"      = res.gprlin,
              "GPR (FBM)"         = res.gprfbm,
              # "GPR (FBM MLE)"     = res.gprfbmoptim,
              "I-prior (linear)"  = res.iplin,
              "I-prior (FBM)"     = res.ipfbm)#,
              # "I-prior (FBM MLE)" = res.ipfbmoptim)

# Results from REC
rp.lda5.mean <- c(36.84, 36.45, 32.57)
rp.lda5.se   <- c(0.84, 0.85, 1.06)
rp.lda5      <- meanAndSE(rp.lda5.mean, rp.lda5.se)
rp.qda5.mean <- c(44.43, 43.56, 41.10)
rp.qda5.se   <- c(0.34, 0.31, 0.33)
rp.qda5      <- meanAndSE(rp.qda5.mean, rp.qda5.se)
rp.knn5.mean <- c(49.08, 47.27, 36.39)
rp.knn5.se   <- c(0.24, 0.26, 0.29)
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
