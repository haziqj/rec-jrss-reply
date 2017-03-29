# Load functions
source("R/1-gpr-iprior-sim-functions.R")

experiment.name <- "Eye state detection data"
# EEG Eye State Data Set (https://archive.ics.uci.edu/ml/datasets/EEG+Eye+State)
# consists of p=14 electroencephalogram measurements on 14 980 observations. The
# task is to use the electroencephalogram reading to determine the state of the
# eye. There are 8256 observations for which the eye is open (class 0), and 6723
# for which the eye is closed (class 1). Binary classification task: "eye open
# (0)" or "eye closed (1)" p = 14 (accelerometer measurements), N = 14980
eye <- read.table("data/EEG Eye State.arff.txt", sep = ",", skip = 19)
summary(as.factor(eye[, 15]))
X.orig <- eye[, -15]
y <- eye[, 15]
N <- length(y)
n <- c(50, 200, 1000)  # subsamples

# Simulations
res.gprlin <- mySim(type = "linear", gpr = TRUE, nsim = 16)  # linear GPR
res.gprfbm <- mySim(type = "fbm", gpr = TRUE, nsim = 16)  # FBM GPR
res.gprfbmoptim <- mySim(type = "fbmoptim", gpr = TRUE, nsim = 16)  # FBM optim GPR

res.iplin <- mySim(type = "linear", nsim = 16)  # Canonical I-prior
res.ipfbm <- mySim(type = "fbm", nsim = 16)  # FBM I-prior
res.ipfbmoptim <- mySim(type = "fbmoptim", nsim = 16)  # FBM optim I-prior

tab <- tabRes("GPR (linear)"      = res.gprlin,
              "GPR (FBM-0.5)"     = res.gprfbm,
              "GPR (FBM-MLE)"     = res.gprfbmoptim,
              "I-prior (linear)"  = res.iplin,
              "I-prior (FBM-0.5)" = res.ipfbm,
              "I-prior (FBM-MLE)" = res.ipfbmoptim)

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
