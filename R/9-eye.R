# Load functions
source("R/1-gpr-iprior-sim-functions.R")

experiment.name <- "Eye state detection data"
# EEG Eye State Data Set (https://archive.ics.uci.edu/ml/datasets/EEG+Eye+State)
# consists of p=14 electroencephalogram measurements on 14 980 observations. The
# task is to use the electroencephalogram reading to determine the state of the
# eye. There are 8257 observations for which the eye is open (class 0), and 6723
# for which the eye is closed (class 1). Binary classification task: "eye open
# (0)" or "eye closed (1)" p = 14 (accelerometer measurements), N = 14980
eye <- read.table("data/EEG Eye State.arff.txt", sep = ",", skip = 19)
summary(as.factor(eye[, 15]))
X.orig <- eye[, -15]
y <- eye[, 15]
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
              "GPR (FBM-0.5)"     = res.gprfbm,
              "GPR (FBM-MLE)"     = NA,
              "I-prior (linear)"  = res.iplin,
              "I-prior (FBM-0.5)" = res.ipfbm,
              "I-prior (FBM-MLE)" = NA)

# Results from REC
rp.lda5.mean <- c(42.06, 38.61, 36.30)
rp.lda5.se   <- c(0.38, 0.29, 0.21)
rp.lda5      <- meanAndSE(rp.lda5.mean, rp.lda5.se)
rp.qda5.mean <- c(38.97, 32.44, 30.91)
rp.qda5.se   <- c(0.39, 0.42, 0.87)
rp.qda5      <- meanAndSE(rp.qda5.mean, rp.qda5.se)
rp.knn5.mean <- c(39.37, 26.91, 13.54)
rp.knn5.se   <- c(0.39, 0.27, 0.19)
rp.knn5      <- meanAndSE(rp.knn5.mean, rp.knn5.se)
rp.tab <- rbind("RP5-LDA" = rp.lda5, "RP5-QDA" = rp.qda5, "RP5-knn" = rp.knn5)
colnames(rp.tab) <- colnames(tab$tab)

# Calculate ranks
tab.mean <- rbind(tab$tab.mean, "RP5-LDA" = rp.lda5.mean,
                  "RP5-QDA" = rp.qda5.mean, "RP5-knn" = rp.knn5.mean)
tab.se <- rbind(tab$tab.se, "RP-LDA5" = rp.lda5.se,
                "RP5-QDA" = rp.qda5.se, "RP5-knn" = rp.knn5.se)
tab.ranks <- tabRank(tab.mean, tab.se)

# Tabulate results
tab.all <- cbind(rbind(tab$tab, rp.tab), Rank = tab.ranks)
knitr::kable(tab.all, align = "r")

# Plot
plotRes()
