# Load functions
source("R/1-gpr-iprior-sim-functions.R")

experiment.name <- "Hill-valley data"
# (https://archive.ics.uci.edu/ml/datasets/Arrhythmia) This database contains
# 279 attributes, 206 of which are linear valued and the rest are nominal.
# Concerning the study of H. Altay Guvenir: "The aim is to distinguish between
# the presence and absence of cardiac arrhythmia and to classify it in one of
# the 16 groups. Class 01 refers to 'normal' ECG classes 02 to 15 refers to
# different classes of arrhythmia and class 16 refers to the rest of
# unclassified ones. For the time being, there exists a computer program that
# makes such a classification. However there are differences between the
# cardiolog's and the programs classification. Taking the cardiolog's as a gold
# standard we aim to minimise this difference by means of machine learning
# tools."
# Binary classification task: "normal (0)" or "arrhythmia (1)" p = 100 (ECG
# measurements), N = 451
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

tab <- tabRes("GPR (linear)"      = res.gprlin,
              "GPR (FBM)"         = res.gprfbm,
              "GPR (FBM MLE)"     = res.gprfbmoptim,
              "I-prior (linear)"  = res.iplin,
              "I-prior (FBM)"     = res.ipfbm,
              "I-prior (FBM MLE)" = res.ipfbmoptim)

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
