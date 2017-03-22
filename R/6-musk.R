# Load functions
source("R/1-gpr-iprior-sim-functions.R")

experiment.name <- "Musk data"
# The musk data set (http://archive.ics.uci.edu/ml/datasets/Musk+\%28Ver
# sion+2\%29) consists of 1016 musk (class 0) and 5581 non-musk (class 1)
# molecules. The task is to classify a molecule on the basis of p = 166 shape
# measurements. N = 6597
load("data/Musk.RData")
summary(as.factor(Musk$y))
X.orig <- Musk$x
y <- Musk$y
y[y == 2] <- 0  # convert to 0 and 1
N <- length(y)
n <- c(100, 200, 500)  # subsamples

# Simulations
res.gprlin <- mySim(type = "linear", gpr = TRUE)  # linear GPR
res.gprfbm <- mySim(type = "fbm", gpr = TRUE)  # FBM GPR
res.gprfbmoptim <- mySim(type = "fbmoptim", gpr = TRUE)  # FBM optim GPR

res.iplin <- mySim(type = "linear")  # Canonical I-prior
res.ipfbm <- mySim(type = "fbm")  # FBM I-prior
res.ipfbmoptim <- mySim(type = "fbmoptim")  # FBM optim I-prior

tab <- tabRes("GPR (linear)"      = res.gprlin,
              "GPR (FBM)"         = res.gprfbm,
              "GPR (FBM MLE)"     = res.gprfbmoptim,
              "I-prior (linear)"  = res.iplin,
              "I-prior (FBM)"     = res.ipfbm,
              "I-prior (FBM MLE)" = res.ipfbmoptim)

# Results from REC
rp.lda5.mean <- c(33.24, 30.19, 27.49)
rp.lda5.se   <- c(0.42, 0.35, 0.30)
rp.lda5      <- meanAndSE(rp.lda5.mean, rp.lda5.se)
rp.qda5.mean <- c(30.47, 28.28, 26.31)
rp.qda5.se   <- c(0.33, 0.26, 0.28)
rp.qda5      <- meanAndSE(rp.qda5.mean, rp.qda5.se)
rp.knn5.mean <- c(33.49, 30.18, 27.09)
rp.knn5.se   <- c(0.40, 0.33, 0.31)
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
