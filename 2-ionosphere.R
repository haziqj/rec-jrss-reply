# Load functions
source("1-gpr-iprior-sim-functions.R")

# Ionosphere data (https://archive.ics.uci.edu/ml/datasets/Ionosphere) This
# radar data was collected by a system in Goose Bay, Labrador. This system
# consists of a phased array of 16 high-frequency antennas with a total
# transmitted power on the order of 6.4 kilowatts. See the paper for more
# details. The targets were free electrons in the ionosphere. "Good" radar
# returns are those showing evidence of some type of structure in the
# ionosphere. "Bad" returns are those that do not; their signals pass through
# the ionosphere. Received signals were processed using an autocorrelation
# function whose arguments are the time of a pulse and the pulse number. There
# were 17 pulse numbers for the Goose Bay system. Instances in this databse are
# described by 2 attributes per pulse number, corresponding to the complex
# values returned by the function resulting from the complex electromagnetic
# signal.
# Binary classification task: "good (0)" or "bad (1)"
# p = 34 (radar data), N = 350
ion <- read.table("ionosphere.data.txt", sep = ",", header = TRUE)
summary(ion$g)
X.orig <- as.matrix(ion[, -35])
y <- as.numeric(ion$g)
y[y == 2] <- 0  # convert good = 0
N <- length(y)

# Simulations
res.gprlin <- mySim(type = "linear", gpr = TRUE)  # linear GPR
res.gprfbm <- mySim(type = "fbm", gpr = TRUE)  # FBM GPR
res.gprfbmoptim <- mySim(type = "fbmoptim", gpr = TRUE)  # FBM optim GPR

res.iplin <- mySim(type = "linear")  # Canonical I-prior
res.ipfbm <- mySim(type = "fbm")  # FBM I-prior
res.ipfbmoptim <- mySim(type = "fbmoptim")  # FBM optim I-prior

# Tabulate results
tab <- tabRes("GPR (linear)"      = res.gprlin,
              "GPR (FBM)"         = res.gprfbm,
              "GPR (FBM MLE)"     = res.gprfbmoptim,
              "I-prior (linear)"  = res.iplin,
              "I-prior (FBM)"     = res.ipfbm,
              "I-prior (FBM MLE)" = res.ipfbmoptim)
rp.lda5 <- c("13.05 (0.38)", "10.75 (0.25)", "9.78 (0.26)")
rp.qda5 <- c("8.14 (0.37)", "6.15 (0.37)", "5.21 (0.20)")
rp.knn5 <- c("13.05 (0.46)", "7.43 (0.25)", "5.43 (0.19)")
rp.tab <- rbind("RP-LDA5" = rp.lda5, "RP-QDA5" = rp.qda5, "RP-knn5" = rp.knn5)
colnames(rp.tab) <- colnames(tab)
tab <- rbind(tab, rp.tab)
tab
