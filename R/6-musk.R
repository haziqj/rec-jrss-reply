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
res.gprlin <- mySim(type = "linear", gpr = TRUE, nsim = 76)  # linear GPR
res.gprfbm <- mySim(type = "fbm", gpr = TRUE, nsim = 76)  # FBM GPR
res.gprfbmoptim <- mySim(type = "fbmoptim", gpr = TRUE)  # FBM optim GPR

res.iplin <- mySim(type = "linear")  # Canonical I-prior
res.ipfbm <- mySim(type = "fbm")  # FBM I-prior
res.ipfbmoptim <- mySim(type = "fbmoptim")  # FBM optim I-prior

# res.gprlinRP <- mySimRP(type = "linear", gpr = TRUE)  # linear GPR with RP
# res.gprfbmRP <- mySimRP(type = "fbm", gpr = TRUE)  # FBM GPR with RP
# res.iplinRP <- mySimRP(type = "linear")  # Canonical I-prior with RP
# res.ipfbmRP <- mySimRP(type = "fbm")  # FBM I-prior with RP

tab <- tabRes("RP5-GPR (linear)"        = res.gprlinRP,
              "RP5-GPR (FBM)"           = res.gprfbmRP,
              "GPR (linear)"            = res.gprlin,
              "GPR (FBM-0.5)"           = res.gprfbm,
              "GPR (FBM-MLE)"           = res.gprfbmoptim,
              "I-prior (linear)"        = res.iplin,
              "I-prior (FBM-0.5)"       = res.ipfbm,
              "I-prior (FBM-MLE)"       = res.ipfbmoptim)
              # "RP5-I-prior (linear)"    = res.iplinRP,
              # "RP5-I-prior (FBM)"       = res.ipfbmRP)

# Results from REC
rp.lda5.mean <- c(14.63, 12.18, 10.15)
rp.lda5.se   <- c(0.31, 0.23, 0.15)
rp.lda5      <- meanAndSE(rp.lda5.mean, rp.lda5.se)
rp.qda5.mean <- c(12.08, 9.92, 8.64)
rp.qda5.se   <- c(0.27, 0.18, 0.13)
rp.qda5      <- meanAndSE(rp.qda5.mean, rp.qda5.se)
rp.knn5.mean <- c(11.81, 9.65, 8.04)
rp.knn5.se   <- c(0.27, 0.21, 0.15)
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
