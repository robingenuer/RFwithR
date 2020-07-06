#' # 3 Random Forests
#' 
#' ## 3.1 General principle
#' 
#' ### 3.1.1 Instability of a tree
#' 
## ----rfTreeBoot1, fig.cap="Classification tree obtained with the default values of `rpart()` on the bootstrap sample `spamBoot1`; `spam` data.", fig.width=8.5, fig.height=6----
set.seed(368910)
spamBoot1 <- spamApp[sample(1:nrow(spamApp), nrow(spamApp), replace = TRUE), ]
treeBoot1 <- rpart(type ~ ., data = spamBoot1)
plot(treeBoot1)
text(treeBoot1, xpd = TRUE)

#' 
## ----rfTreeBoot2, fig.cap="Classification tree obtained with the default values of `rpart()` on the bootstrap sample `spamBoot1`; `spam` data.", fig.width=8.5, fig.height=6----
set.seed(368915)
spamBoot2 <- spamApp[sample(1:nrow(spamApp), nrow(spamApp), replace = TRUE), ]
treeBoot2 <- rpart(type ~ ., data = spamBoot2)
plot(treeBoot2)
text(treeBoot2, xpd = TRUE)

#' 
## ----rfCompTreeBoot------------------------------
mean(predict(treeBoot1, spamTest, type = "class") !=
     predict(treeBoot2, spamTest, type = "class"))

#' 
#' ### 3.1.2 From a tree to an ensemble: the Bagging
#' 
## ----rfLibrary-----------------------------------
library(randomForest)

#' 
## ----rfBagging-----------------------------------
bagging <- randomForest(type ~ ., data = spamApp, mtry = ncol(spamApp) - 1)
bagging

#' 
## ----rfBaggingErrors-----------------------------
errTestBagging <- mean(predict(bagging, spamTest) != spamTest$type)
errEmpBagging <- mean(predict(bagging, spamApp) != spamApp$type)

#' 
#' ## 3.3 The `randomForest` package
#' 
## ----rfRFDef-------------------------------------
RFDef <- randomForest(type ~ ., data = spamApp)
RFDef

#' 
## ----rfRFDefAlter--------------------------------
RFDef <- randomForest(spamApp[, -58], spamApp[, 58])

#' 
## ----rfRFRIErrorsPrint---------------------------
errTestRFDef <- mean(predict(RFDef, spamTest) != spamTest$type)
errEmpRFDef <- mean(predict(RFDef, spamApp) != spamApp$type)

#' 
#' ## 3.5 Parameters setting for prediction
#' 
#' ### 3.5.1 The number of trees: `ntree`
#' 
## ----rfPlotRFDefEcho, fig.cap= "Evolution of the global OOB error and for each class, according to the number of trees, `spam` data."----
plot(RFDef)

#' 
## ----rfRFDoTrace---------------------------------
RFDoTrace <- randomForest(type~., data=spamApp, ntree = 250, do.trace=25)

#' 
#' ### 3.5.2 The number of variables chosen at each node: `mtry`
#' 
## ----rfTuneMtryEcho------------------------------
nbvars <- 1:(ncol(spamApp) - 1)
oobsMtry <- sapply(nbvars, function(nbv) {
  RF <- randomForest(type~., spamApp, ntree = 250, mtry = nbv)
  return(RF$err.rate[RF$ntree, "OOB"])})

#' 
## ----rf10runs------------------------------------
mean(replicate(n = 25,
  randomForest(type ~ ., spamApp, ntree = 250)$err.rate[250, "OOB"]))

#' 
## ----rfBagStump----------------------------------
bagStump <- randomForest(type~., spamApp, ntree = 100,
                         mtry = ncol(spamApp) - 1, maxnodes = 2)

#' 
## ----rfBagStumpRes-------------------------------
bagStumpbestvar <- table(bagStump$forest$bestvar[1,])
names(bagStumpbestvar) <- colnames(spamApp)[as.numeric(names(bagStumpbestvar))]
sort(bagStumpbestvar, decreasing = TRUE)

#' 
## ----rfRFStump-----------------------------------
RFStump <- randomForest(type~., spamApp, ntree = 100, maxnodes = 2)
RFStumpbestvar <- table(RFStump$forest$bestvar[1,])
names(RFStumpbestvar) <- colnames(spamApp)[as.numeric(names(RFStumpbestvar))]
sort(RFStumpbestvar, decreasing = TRUE)

#' 
#' ## 3.6 Examples
#' 
#' ### 3.6.1 Predicting ozone concentration
#' 
## ----rfOzoneLoad---------------------------------
library("randomForest")
data("Ozone", package = "mlbench")

#' 
## ----rfOzoneRFDef--------------------------------
OzRFDef <- randomForest(V4 ~ ., Ozone, na.action = na.omit)

#' 
## ----rfOzoneRFDefRes, fig.cap="OOB error as a function of the number of trees, `Ozone` data."----
OzRFDef
plot(OzRFDef)

#' 
## ----rfOzoneMtryPlot, fig.cap="OOB error as a function of `mtry` parameter value, `Ozone` data."----
plot(nbvars, oobsMtrys, type ="l", xlab = "mtry", ylab = "Erreur OOB")

#' 
## ----rfOzoneRFDefStrat---------------------------
bins <- c(0, 10, 20, 40)
V4bin <- cut(Ozone$V4, bins, include.lowest = TRUE, right = FALSE)
OzoneBin <- data.frame(Ozone, V4bin)
OzRFDefStrat <- randomForest(V4 ~ . - V9 - V4bin, OzoneBin, strata = V4bin,
                             sampsize = 200, na.action = na.omit)
OzRFDefStrat

#' 
#' ### 3.6.2 Analyzing genomic data
#' 
## ----rfVac18Load---------------------------------
library(randomForest)
data("vac18", package = "mixOmics")

#' 
## ----rfVac18DataManage---------------------------
geneExpr <- vac18$genes
stimu <- vac18$stimulation

#' 
## ----rfVac18RFpsur3, fig.cap="OOB error evolution according to the number of trees in the forest (OOB errors per class have been hidden to facilitate the reading of the graph), `Vac18` data."----
VacRFpsur3 <- randomForest(x = geneExpr, y = stimu, mtry = ncol(geneExpr)/3)
VacRFpsur3
plot(VacRFpsur3)

#' 
## ----rfVac18RFCompMtry---------------------------
nFor <- 25
VacOOBsqrtp <- replicate(nFor,
  randomForest(geneExpr, stimu)$err.rate[500, "OOB"])
VacOOBpsur3 <- replicate(nFor,
  randomForest(geneExpr, stimu, mtry = ncol(geneExpr)/3)$err.rate[500, "OOB"])

#' 
#' ### 3.6.3 Analyzing dust pollution
#' 
## ----rfPM10load----------------------------------
library(randomForest)
data("jus", package = "VSURF")
jusComp <- na.omit(jus)

#' 
## ----rfPM10jusRF---------------------------------
jusRF <- randomForest(PM10 ~ ., data = jusComp)

#' 
## ----rfPM10partialNO, fig.cap="Marginal effect of NO variable on PM10 concentration, associated with a random forest trained using station JUS data."----
partialPlot(jusRF, pred.data = jusComp, x.var = "NO",
            main = "Effet marginal - NO")

#' 
