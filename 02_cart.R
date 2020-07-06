#' # 2 CART
#' 
#' ## 2.4 The `rpart` package
#' 
## ----cartTreeDef, fig.cap="Classification tree obtained with the default values of `rpart()`, `spam` data.", fig.width=8.5, fig.height=6----
library(rpart)
treeDef <- rpart(type ~ ., data = spamApp)
print(treeDef, digits = 2)
plot(treeDef)
text(treeDef, xpd = TRUE)

#' 
## ----cartTreeMaxCode, fig.show='hide', results='hide'----
set.seed(601334)
treeMax <- rpart(type ~ ., data = spamApp, minsplit = 2, cp = 0)
plot(treeMax)
treeMax$cptable

#' 
## ----cartplotCp, fig.cap="Errors estimated by cross-validation of the sequence of sub-trees pruned from the maximal tree, `spam` data."----
plotcp(treeMax)

#' 
## ----cartTreeOpt, fig.width=8.5, fig.height=7, fig.cap="Optimal pruned tree, `spam` data."----
cpOpt <- treeMax$cptable[ which.min(treeMax$cptable[, 4]), 1 ]
treeOpt <- prune(treeMax, cp = cpOpt)
plot(treeOpt)
text(treeOpt, xpd = TRUE, cex = 0.8)

#' 
## ----cartTree1SE, fig.width=8.5, fig.height=7, fig.cap="Tree \"1-SE\" pruned, `spam` data."----
thres1SE <- sum(treeMax$cptable[ which.min(treeMax$cptable[, 4]), 4:5])
cp1SE <- treeMax$cptable[ min(which(treeMax$cptable[, 4] <= thres1SE)), 1]
tree1SE <- prune(treeMax, cp = cp1SE)
plot(tree1SE)
text(tree1SE, xpd = TRUE, cex = 0.8)

#' 
## ----cartErrorsMax----------
errTestTreeMax <- mean(predict(treeMax, spamTest, type = "class") !=
                         spamTest$type)
errEmpTreeMax <- mean(predict(treeMax, spamApp, type = "class") !=
                        spamApp$type)

#' 
#' ## 2.5 Competing and surrogate splits
#' 
#' ### 2.5.2 Surrogate splits
#' 
## ----cartTreeStump----------
treeStump <- rpart(type ~ ., data = spamApp, maxdepth = 1)
summary(treeStump)

#' 
#' ### 2.5.3 Interpretability
#' 
## ----cartVarImp, fig.width=8, fig.cap="Importance of variables in the sense of CART for the maximal tree, `spam` data."----
par(mar=c(7, 3, 1, 1) + 0.1)
barplot(treeMax$variable.importance, las = 2, cex.names = 0.8)

#' 
#' ## 2.6 Examples
#' 
#' ### 2.6.1 Predicting ozone concentration
#' 
## ----cartOzoneLoad----------
library("rpart")
data("Ozone", package = "mlbench")

#' 
## ----cartOzoneTreeDef, fig.width=8.5, fig.height=6, fig.cap="Default tree, `Ozone` data."----
OzTreeDef <- rpart(V4 ~ ., data = Ozone)
print(OzTreeDef, digits = 3)
plot(OzTreeDef)
text(OzTreeDef, xpd = TRUE, cex = 0.9)

#' 
## ----cartOzoneTreeMax-------
set.seed(727325)
OzTreeMax <- rpart(V4 ~ ., data = Ozone, minsplit = 2, cp = 0)

#' 
## ----cartOzoneTreePlotcp, fig.cap="Errors estimated by cross-validation of the sequence of sub-trees pruned from the maximal tree, `Ozone` data."----
plotcp(OzTreeMax)

#' 
## ----cartOzoneTreeOpt, fig.cap="Optimal pruned tree, `Ozone` data."----
OzIndcpOpt <- which.min(OzTreeMax$cptable[, 4])
OzcpOpt <- OzTreeMax$cptable[OzIndcpOpt, 1]
OzTreeOpt <- prune(OzTreeMax, cp = OzcpOpt)
plot(OzTreeOpt)
text(OzTreeOpt, xpd = TRUE)

#' 
#' ### 2.6.2 Analyzing genomic data
#' 
## ----cartVac18Load----------
library(rpart)
data("vac18", package = "mixOmics")
VAC18 <- data.frame(vac18$genes, "stimu" = vac18$stimulation)

#' 
## ----cartVac18TreeDef, fig.cap="Default tree obtained with `rpart()` on the `Vac18` data."----
VacTreeDef <- rpart(stimu ~ ., data = VAC18)
print(VacTreeDef)
plot(VacTreeDef)
text(VacTreeDef, use.n = TRUE, xpd = TRUE)

#' 
## ----cartVac18TreeMax, fig.cap="Maximal tree on `Vac18` data."----
set.seed(788182)
VacTreeMax <- rpart(stimu ~ ., data = VAC18, minsplit = 2, cp = 0)
plot(VacTreeMax)
text(VacTreeMax, use.n = TRUE, xpd = TRUE)

#' 
## ----cartVac18TreeMaxLoo, fig.cap="Errors estimated by 10-fold cross-validation (left) and leave-one-out (right) of the sequence of sub-trees pruned from the maximal tree, `Vac18` data."----
set.seed(413745)
VacTreeMaxLoo <- rpart(stimu ~ ., data = VAC18, minsplit = 2, cp = 0, xval = nrow(VAC18))
par(mfrow=c(1,2))
plotcp(VacTreeMax)
plotcp(VacTreeMaxLoo)

#' 
## ----cartVac18TreeOpt, fig.cap="Optimal pruned tree, `Vac18` data."----
VacIndcpOpt <- which.min(VacTreeMaxLoo$cptable[, 4])
VaccpOpt <- VacTreeMaxLoo$cptable[VacIndcpOpt, 1]
VacTreeOpt <- prune(VacTreeMaxLoo, cp = VaccpOpt)
plot(VacTreeOpt)
text(VacTreeOpt, use.n = TRUE, xpd = TRUE)

