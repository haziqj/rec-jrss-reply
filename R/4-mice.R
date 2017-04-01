# Load functions
source("R/1-gpr-iprior-sim-functions.R")

experiment.name <- "Mice protein expression"
# (https://archive.ics.uci.edu/ml/datasets/Mice+Protein+Expression) The data set
# consists of the expression levels of 77 proteins/protein modifications that
# produced detectable signals in the nuclear fraction of cortex. There are 38
# control mice and 34 trisomic mice (Down syndrome), for a total of 72 mice. In
# the experiments, 15 measurements were registered of each protein per
# sample/mouse. Therefore, for control mice, there are 38x15, or 570
# measurements, and for trisomic mice, there are 34x15, or 510 measurements. The
# dataset contains a total of 1080 measurements per protein. Each measurement
# can be considered as an independent sample/mouse. Binary classification task:
# "control (0)" or "trisomy (1)" p = 77 (gene expressions), N = 1080
mice <- read.table("data/Data_Cortex_Nuclear.csv", sep = ",", header = TRUE)

# Remove missing data
missing.index <- apply(mice, 1, function(x) any(is.na(x)))
sum(missing.index)  # how many?
mice <- mice[-which(missing.index), ]

# Data
summary(mice$Genotype)
X <- X.orig <- as.matrix(mice[, 2:78])
y <- as.numeric(mice$Genotype) - 1
N <- length(y)
n <- c(100, 200, 500)  # subsamples

# Simulations
res.gprlin <- mySim(type = "linear", gpr = TRUE, n.mySim = n)  # linear GPR
res.gprfbm <- mySim(type = "fbm", gpr = TRUE, n.mySim = n)  # FBM GPR
res.gprfbmoptim <- mySim(type = "fbmoptim", gpr = TRUE, n.mySim = n)  # FBM optim GPR

res.iplin <- mySim(type = "linear", n.mySim = n)  # Canonical I-prior
res.ipfbm <- mySim(type = "fbm", n.mySim = n)  # FBM I-prior
res.ipfbmoptim <- mySim(type = "fbmoptim", n.mySim = n)  # FBM optim I-prior

tab <- tabRes("GPR (linear)"      = res.gprlin,
              "GPR (FBM-0.5)"     = res.gprfbm,
              "GPR (FBM-MLE)"     = res.gprfbmoptim,
              "I-prior (linear)"  = res.iplin,
              "I-prior (FBM-0.5)" = res.ipfbm,
              "I-prior (FBM-MLE)" = res.ipfbmoptim)

# Results from REC
rp.lda5.mean <- c(NA, 25.17, 23.56)
rp.lda5.se   <- c(NA, 0.30, 0.26)
rp.lda5      <- meanAndSE(rp.lda5.mean, rp.lda5.se)
rp.qda5.mean <- c(NA, 18.24, 16.05)
rp.qda5.se   <- c(NA, 0.29, 0.24)
rp.qda5      <- meanAndSE(rp.qda5.mean, rp.qda5.se)
rp.knn5.mean <- c(NA, 11.24, 2.24)
rp.knn5.se   <- c(NA, 0.29, 0.10)
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
