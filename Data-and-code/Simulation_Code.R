### Random projection ensemble classifier

#setwd("~/RPEnsembleSims")

#Load Library
library("RPEnsemble")

library("class")
library("mvtnorm")
library("MASS")
library("Matrix")
library("parallel")
library("distr")
library("pamr")
library("penalizedLDA")
library("randomForest")
library("e1071")
library("kernlab")
library("tools")
library("penalized")



##MainSimNEW: Generates training a test set and estimates error for different base classifiers, repeats n.reps times using parLapply. 
MainSim <- function(Model.No, n.train = 50, p = 50, s0 = 1, prior = 1/2, n.test = 1000, n.reps = 100, d = 5, B1 = 100, B2=100,  k = c(3,5))
{
  psnice(value = 19)
  for (i in 1:n.reps)
  {
    print(i)
    set.seed(100 + i)
    data.train <- RPModel(Model.No, n.train, p, prior)
    data.test <- RPModel(Model.No, n.test, p, prior)
    
    OutLDA <-  RPParallel(XTrain =  data.train$x, YTrain = data.train$y , XTest = data.test$x, d = d, B1 = B1, B2 = B2, base = "LDA", projmethod = "Gaussian", estmethod = "training", clustertype = "Fork", cores  = 3, seed = 123 + i)
    errLDA <- 100*mean(RPEnsembleClass(OutLDA, n = n.train, n.test = 1000, p1 = sum(data.train$y == 1)/n.train, samplesplit = FALSE, alpha = RPalpha(OutLDA, Y = data.train$y, p1 = sum(data.train$y == 1)/n.train)) != data.test$y)
    
    OutQDA <-  RPParallel(XTrain =  data.train$x, YTrain = data.train$y , XTest = data.test$x, d = d, B1 = B1, B2 = B2, base = "QDA", projmethod = "Gaussian", estmethod = "loo", clustertype = "Fork", cores  = 3, seed = 123 + i)
    errQDA <-  100*mean(RPEnsembleClass(OutQDA, n = n.train, n.test = 1000, p1 = sum(data.train$y == 1)/n.train, samplesplit = FALSE, alpha = RPalpha(OutQDA, Y = data.train$y, p1 = sum(data.train$y == 1)/n.train)) != data.test$y)
    
    Outknn <-  RPParallel(XTrain =  data.train$x, YTrain = data.train$y , XTest = data.test$x, d = d, B1 = B1, B2 = B2, base = "knn", projmethod = "Gaussian", estmethod = "loo", k = c(3,5,7,9,11), clustertype = "Fork", cores  = 3, seed = 123 + i)
    errknn <- 100*mean(RPEnsembleClass(Outknn, n = n.train, n.test = 1000, p1 = sum(data.train$y == 1)/n.train, samplesplit = FALSE, alpha = RPalpha(Outknn, Y = data.train$y, p1 = sum(data.train$y == 1)/n.train)) != data.test$y)
    
    if (i==1) Risk <- c(errLDA, errQDA, errknn)
    else Risk <- rbind(Risk, c(errLDA, errQDA, errknn))
    save(Risk,file=paste("Risk-",d,"-",Model.No,"-",n.train,"-",prior,".RData",sep=""))
  }
}
  

##CompSimOTE: Comparison with the OTE and ESKNN methods
CompSimOTE <- function(Model.No, n.train = 50, p = 50, s0 = 1, prior = 1/2, n.test = 1000, n.reps = 100, k = c(3,5))
{
  for (i in 1:n.reps)
  {
    #setseed
    set.seed(100 + i)
    
    print(i)
    
    #Generate data
    data.train <- Model(Model.No, n.train, p, s0, prior)
    data.test <- Model(Model.No, n.test, p, s0, prior)
    
    n.min <- min(table(data.train$y))
    n1 <- table(data.train$y)[[1]]
    n2 <- table(data.train$y)[[2]]
    
    errOTE <- NA ; tOTE <- NULL
    errESKNN <- NA ; tESKNN <- NULL
    
    tOTE <- system.time(Out.ote <- OTClass(XTraining = data.train$x, YTraining = data.train$y - 1, t.initial = 1000))[1:3] + system.time(errOTE <- 100*Predict.OTClass(Out.ote, XTesting = data.test$x, YTesting =  data.test$y - 1 )$Error.Rate)[1:3]
    
    tESKNN <- system.time(Out.esknn <- esknnClass(xtrain = data.train$x, ytrain = factor(data.train$y - 1)))[1:3] + system.time(errESKNN <- 100*Predict.esknnClass(Out.esknn, xtest = data.test$x, ytest =  factor(data.test$y - 1) )$ClassError)[1:3]
    
    if (i==1) Risk <-  c(errOTE,errESKNN)
    else Risk <- rbind(Risk, c(errOTE,errESKNN))
    if (i==1) Time <-  rbind(tOTE, tESKNN) 
    else Time <- Time + rbind(tOTE, tESKNN)  
  }
  return(list(Risk = Risk, Time = Time))
}

##CompSim: Generates training a test set and estimates error for different comparison classifiers, repeats n.reps times. 
CompSim <- function(Model.No, n.train = 50, p = 50, s0 = 1, prior = 1/2, n.test = 1000, n.reps = 100, k = c(3,5))
    {
        for (i in 1:n.reps)
            {
                #setseed
                set.seed(100 + i)
          
                print(i)
                
                #Generate data
                data.train <- Model(Model.No, n.train, p, s0, prior)
                data.test <- Model(Model.No, n.test, p, s0, prior)

                n.min <- min(table(data.train$y))
                n1 <- table(data.train$y)[[1]]
                n2 <- table(data.train$y)[[2]]
                
                errLDA <- NA ; tLDA <- NULL
                if (n.train > p) tLDA <- system.time(errLDA <- 100*mean(predict(lda(x = data.train$x, grouping = data.train$y), data.test$x)$class != data.test$y, na.rm = TRUE))[1:3]

                errQDA <- NA ; tQDA <- NULL
                if (n.min > p) tQDA <- system.time(errQDA <- 100*mean(predict(qda(x = data.train$x, grouping = data.train$y), data.test$x)$class != data.test$y, na.rm = TRUE))[1:3]

                errknn <- NA ; tknn <- NA
                tknn <- system.time(kcv <- sapply(k,function(x){sum(knn.cv(data.train$x, data.train$y, x) != data.train$y)}))[1:3] + system.time(errknn <-100*mean(knn(data.train$x, data.test$x, data.train$y, k = k[which.min(kcv)]) != data.test$y, na.rm = TRUE))[1:3]

                errPENlog <- NA ; tPENlog <- NA
                tPENlog <- system.time(cv.outL1 <- optL1(response = factor(data.train$y), penalized = data.train$x, minlambda1 = 0.1, maxlambda1 = 10, model = "logistic", fold = 5)$lambda)[1:3] + system.time(errPENlog <- 100*mean((predict(penalized(response = factor(data.train$y), penalized = data.train$x, lambda1 = cv.outL1, lambda2 = 0, model = "logistic"), data.test$x) > 0.5) == (2-data.test$y) ))[1:3]
                
                err1PEN <- NA ; tPEN <- NA
                tPEN <- system.time(cv.out1 <- PenalizedLDA.cv(data.train$x,data.train$y,lambdas=c(0.055,0.06,0.065,.07,.075,0.08,.085,0.09,0.095)))[1:3] + system.time(out1  <- PenalizedLDA(data.train$x,data.train$y,data.test$x,lambda = cv.out1$bestlambda , K = cv.out1$bestK))[1:3] + system.time(err1PEN <- 100*mean(out1$ypred[,1]-data.test$y != 0, na.rm = TRUE))[1:3]

                err1NSC <- NA ; tNSC <- NA
                tNSC <- system.time(Trainout1 <- pamr.train(list(x = t(data.train$x), y = data.train$y)))[1:3] + system.time(CV.out1NSC <- pamr.cv(Trainout1, list(x = t(data.train$x), y = data.train$y), nfold = 10))[1:3] + system.time(out1NSC <- pamr.predict(Trainout1,t(data.test$x), threshold = CV.out1NSC$threshold[which.min(CV.out1NSC$error)],type = c("class")))[1:3] + system.time(err1NSC <- 100*mean(out1NSC != data.test$y, na.rm = TRUE))[1:3]
                
                err1SCRDA <- NA ; tSCRDA <- NA
             #   tSCRDA <- system.time(out1SCRDA <- ascrda(data.train$x,data.train$y,data.test$x,data.test$y,SCRDAmethod = "SCRDA"))[1:3] + system.time(err1SCRDA <- as.numeric(out1SCRDA$SCRDA)*100)[1:3]

                err1IR <- NA; tIR <- NA
            #    tIR <- system.time(err1IR <- FitDLDA(data.train$x,data.train$y,data.test$x,data.test$y)$Err*100)[1:3]

                errRandForest <- NA ; tRF <- NA
                tRF <- system.time(RandForest <- randomForest(x = data.train$x, y =  factor(data.train$y), xtest = data.test$x, ytest = factor(data.test$y), ntree=1000, mtry=sqrt(p), replace=TRUE, classwt=c(n1/n.train, n2/n.train), cutoff = c(0.5,0.5), sampsize = n.train, nodesize = 1, keep.forest= TRUE))[1:3] + system.time(errRandForest <- 100*mean(as.numeric(predict(RandForest, newdata = data.test$x)) != data.test$y, na.rm = TRUE))[1:3]
                
                errSVMRad <- NA; tSVMR <- NA
                tSVMR <- system.time(SVMRad <- svm(x = data.train$x, y = factor(data.train$y) , kernel = "radial", gamma = 1/p, cost = 1, class.weights = list("1" = n1/n.train, "2" =  n2/n.train), cachesize = 40, tolerance = 0.001, epsilon = 0.1, shrinking = TRUE, cross = 0, probability = FALSE, fitted = TRUE, na.action = na.omit))[1:3]  + system.time(errSVMRad <- 100*mean(as.numeric(predict(SVMRad, newdata = data.test$x)) != data.test$y, na.rm = TRUE))[1:3]
                        
                errSVMLin <- NA;  tSVML <- NA
               tSVML <- system.time(SVMLin <- svm(x = data.train$x, y = factor(data.train$y) , kernel = "linear", gamma = 1/p, cost = 1, class.weights = list("1" = n1/n.train, "2" =  n2/n.train), cachesize = 40, tolerance = 0.001, epsilon = 0.1, shrinking = TRUE, cross = 0, probability = FALSE, fitted = TRUE, na.action = na.omit))[1:3]  + system.time(errSVMLin <- 100*mean(as.numeric(predict(SVMLin, newdata = data.test$x)) != data.test$y, na.rm = TRUE))[1:3]

                errGPRad <- NA ; tGPR <- NA
                tGPR <- system.time(GPRad <- gausspr(x = data.train$x, y = factor(data.train$y), scaled = FALSE, type= "classification", kernel="rbfdot", kpar="automatic", var=1, variance.model = FALSE, tol=0.0005, cross=0, fit=FALSE, na.action = na.omit))[1:3] + system.time(errGPRad <- 100*mean(1 + (predict(GPRad, newdata = data.test$x,  type = "probabilities", coupler = "pkpd")[,2]>0.5) != data.test$y))[1:3]
                
                if (i==1) Risk <-  c(errLDA, errQDA, errknn,errRandForest, errSVMRad, errSVMLin, errGPRad, err1PEN, err1NSC, err1SCRDA, err1IR, errPENlog)
                else Risk <- rbind(Risk,  c(errLDA, errQDA, errknn,errRandForest, errSVMRad, errSVMLin, errGPRad, err1PEN, err1NSC, err1SCRDA, err1IR, errPENlog))
                if (i==1) Time <-  rbind(tLDA, tQDA, tknn,tRF, tSVMR, tSVML, tGPR, tPEN, tNSC, tSCRDA, tIR, tPENlog)
                else Time <- Time + rbind(tLDA, tQDA, tknn,tRF, tSVMR, tSVML, tGPR, tPEN, tNSC, tSCRDA, tIR, tPENlog)
          }
       return(list(Risk = Risk, Time = Time))
}


##Settings:  reads settings c(Model, n, p, s, prior, ntest, nreps, d, B1, B2) 
#Settings <- c(1, 50, 100, 1, 0.5, 1000, 100, 5, 50, 500)
Settings <- read.csv("Settings.txt", header = T)

## Specify which jobs in settings to run
JobsC = NULL
JobsRP = NULL
JobsO <- NULL
JobsRP <- 1

#nice processes
psnice(value = 19)

for (Job in JobsC)
{
  outcompall <- CompSim(Model.No = Settings[Job,1], n.train = Settings[Job,2], p = Settings[Job,3], s0 = Settings[Job,4], prior = Settings[Job,5], n.test = Settings[Job,6], n.reps = Settings[Job,7], k = c(3,5,7,9,11))
  outcomprisk <- outcompall$Risk
  save(outcomprisk,file=paste("RiskComp-",Job,".RData",sep=""))
}

for (Job in JobsO)
{
  outcompall <- CompSimOTE(Model.No = Settings[Job,1], n.train = Settings[Job,2], p = Settings[Job,3], s0 = Settings[Job,4], prior = Settings[Job,5], n.test = Settings[Job,6], n.reps = Settings[Job,7], k = c(3,5,7,9,11))
  outcomprisk <- outcompall$Risk
  save(outcomprisk,file=paste("RiskCompOTE-",Job,".RData",sep=""))
}

for (Job in JobsRP)
  {
  print(Job)
  psnice(value = 19)
  MainSim(Model.No = Settings[Job,1], n.train = Settings[Job,2], p = Settings[Job,3], s0 = Settings[Job,4], prior = Settings[Job,5], n.test = Settings[Job,6], n.reps = Settings[Job,7] , d = Settings[Job,8], B1 = Settings[Job,9], B2 = Settings[Job,10], k = c(3,5,7,9,11))
}







