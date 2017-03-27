# Load functions
source("R/1-gpr-iprior-sim-functions.R")

experiment.name <- "Ionosphere data"
# (https://archive.ics.uci.edu/ml/datasets/Ionosphere) This
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
ion <- read.table("data/ionosphere.data.txt", sep = ",", header = TRUE)
summary(ion$g)
X.orig <- as.matrix(ion[, -35])
y <- as.numeric(ion$g)
y[y == 2] <- 0  # convert good = 0
N <- length(y)
n <- c(50, 100, 200)

# Simulations
res.gprlin <- mySim(type = "linear", gpr = TRUE)  # linear GPR
res.gprfbm <- mySim(type = "fbm", gpr = TRUE)  # FBM GPR
res.gprfbmoptim <- mySim(type = "fbmoptim", gpr = TRUE)  # FBM optim GPR

res.iplin <- mySim(type = "linear")  # Canonical I-prior
res.ipfbm <- mySim(type = "fbm")  # FBM I-prior
res.ipfbmoptim <- mySim(type = "fbmoptim")  # FBM optim I-prior

res.iprobitlin <- ipmySim(nsim = 4)
res.iprobitfbm <- ipmySim(nsim = 4, type = "fbm")

res.gprlinRP <- mySimRP(type = "linear", gpr = TRUE)  # linear GPR with RP
res.gprfbmRP <- mySimRP(type = "fbm", gpr = TRUE)  # FBM GPR with RP
# res.iplinRP <- mySimRP(type = "linear")  # Canonical I-prior with RP
# res.ipfbmRP <- mySimRP(type = "fbm")  # FBM I-prior with RP

tab <- tabRes("RP5-GPR (linear)"        = res.gprlinRP,
              "RP5-GPR (FBM)"           = res.gprfbmRP,
              "GPR (linear)"            = res.gprlin,
              "GPR (FBM)"               = res.gprfbm,
              "GPR (FBM MLE)"           = res.gprfbmoptim,
              "I-prior (linear)"        = res.iplin,
              "I-prior (FBM)"           = res.ipfbm,
              "I-prior (FBM MLE)"       = res.ipfbmoptim,
              "I-prior probit (linear)" = res.iprobitlin,
              "I-prior probit (FBM)"    = res.iprobitfbm)
              # "RP5-I-prior (linear)"    = res.iplinRP,
              # "RP5-I-prior (FBM)"       = res.ipfbmRP)

# Results from REC
rp.lda5.mean <- c(13.05, 10.75, 9.78)
rp.lda5.se   <- c(0.38, 0.25, 0.26)
rp.lda5      <- meanAndSE(rp.lda5.mean, rp.lda5.se)
rp.qda5.mean <- c(8.14, 6.15, 5.21)
rp.qda5.se   <- c(0.37, 0.37, 0.20)
rp.qda5      <- meanAndSE(rp.qda5.mean, rp.qda5.se)
rp.knn5.mean <- c(13.05, 7.43, 5.43)
rp.knn5.se   <- c(0.46, 0.25, 0.19)
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


