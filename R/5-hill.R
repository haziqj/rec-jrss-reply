# Load functions
source("R/1-gpr-iprior-sim-functions.R")

experiment.name <- "Hill-valley data"
# The hill–valley data set (http://archive.ics.uci.edu/ml/datasets/Hill-Valley)
# consists of 1212 observations of a terrain, each when plotted in sequence
# represents either a hill (class 0; size 600) or a valley (class 1; size 612).
# The task is to classify the terrain on the basis of a vector of dimension p =
# 100.
load("data/Hill.RData")
summary(as.factor(HillData$y))
X.orig <- HillData$x
y <- HillData$y
y <- y - 1  # convert to 0 and 1
N <- length(y)
n <- c(100, 200, 500)  # subsamples

# Simulations
res.gprlin <- mySim(type = "linear", gpr = TRUE)  # linear GPR
res.gprfbm <- mySim(type = "fbm", gpr = TRUE)  # FBM GPR
res.gprfbmoptim <- mySim(type = "fbmoptim", gpr = TRUE)  # FBM optim GPR

res.iplin <- mySim(type = "linear")  # Canonical I-prior
res.ipfbm <- mySim(type = "fbm")  # FBM I-prior
res.ipfbmoptim <- mySim(type = "fbmoptim")  # FBM optim I-prior

tab <- tabRes("GPR (linear)"            = res.gprlin,
              "GPR (FBM)"               = res.gprfbm,
              "GPR (FBM MLE)"           = res.gprfbmoptim,
              "I-prior (linear)"        = res.iplin,
              "I-prior (FBM)"           = res.ipfbm,
              "I-prior (FBM MLE)"       = res.ipfbmoptim)

# Results from REC
rp.lda5.mean <- c(36.84, 36.45, 32.57)
rp.lda5.se   <- c(0.84, 0.85, 1.06)
rp.lda5      <- meanAndSE(rp.lda5.mean, rp.lda5.se)
rp.qda5.mean <- c(44.43, 43.56, 41.10)
rp.qda5.se   <- c(0.34, 0.31, 0.33)
rp.qda5      <- meanAndSE(rp.qda5.mean, rp.qda5.se)
rp.knn5.mean <- c(49.08, 47.27, 366.39)
rp.knn5.se   <- c(0.24, 0.26, 0.29)
rp.knn5      <- meanAndSE(rp.knn5.mean, rp.knn5.se)
rp.tab <- rbind("RP5-LDA" = rp.lda5, "RP5-QDA" = rp.qda5, "RP5-knn" = rp.knn5)
colnames(rp.tab) <- colnames(tab$tab)

# Calculate ranks
tab.mean <- rbind(tab$tab.mean, "RP5-LDA" = rp.lda5.mean,
                  "RP5-QDA" = rp.qda5.mean, "RP5-knn" = rp.knn5.mean)
tab.se <- rbind(tab$tab.se, "RP5-LDA" = rp.lda5.se,
                "RP5-QDA" = rp.qda5.se, "RP5-knn" = rp.knn5.se)
tab.ranks <- tabRank(tab.mean, tab.se)

# Tabulate results
tab.all <- cbind(rbind(tab$tab, rp.tab), Rank = tab.ranks)
knitr::kable(tab.all, align = "r")

# Plot
plotRes()
