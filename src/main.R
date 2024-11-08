install.packages(c("FactoMineR", "factoextra"))
install.packages("lattice")
install.packages("dplyr")
install.packages("lattice")
install.packages("rpart")
install.packages("rattle")
install.packages("rpart.plot")
install.packages("RColorBrewer")
install.packages("pROC")
install.packages("caret")
install.packages("ROCR")

library(dplyr)
library(FactoMineR)
library(factoextra)
library(lattice)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(pROC)
library(caret)
library(ROCR)


dataset = read.csv("diabetes.csv", header = TRUE)
str(dataset)

df = data.frame(colSums(dataset==0))
df

dataset = filter(dataset, Glucose > 0 & BMI > 0)

df = data.frame(colSums(dataset==0))
df

df = data.frame(colSums(dataset==0))
meanGlucose = (mean(dataset$Glucose) * nrow(dataset)) / 
  (nrow(dataset) - df['Glucose',])
meanBloodPressure = (mean(dataset$BloodPressure) * 
                       nrow(dataset)) / (nrow(dataset) - 
                                           df['BloodPressure',])
meanSkinThickness = (mean(dataset$SkinThickness) * nrow(dataset))/
  (nrow(dataset) - df['SkinThickness',])
meanInsulin = (mean(dataset$Insulin) * nrow(dataset)) / 
  (nrow(dataset)-df['Insulin',])
meanBMI = (mean(dataset$BMI) * nrow(dataset)) / 
  (nrow(dataset)-df['BMI',])

dataset$Glucose[dataset$Glucose == 0] = meanGlucose
dataset$BloodPressure[dataset$BloodPressure == 0] = 
  meanBloodPressure
dataset$SkinThickness[dataset$SkinThickness == 0] = 
  as.integer(meanSkinThickness)
dataset$Insulin[dataset$Insulin == 0] = meanInsulin
dataset$BMI[dataset$BMI == 0] = meanBMI
summary(dataset)

df = data.frame(colSums(dataset == 0))
df

dataset$Outcome = ifelse(dataset$Outcome == "1", "Yes", "No")


dataset$Outcome = factor(dataset$Outcome)
str(dataset)

split.data = function(data, p = 0.7, s = 1){
  set.seed(s)
  index = sample(1:dim(data)[1])
  train = data[index[1:floor(dim(data)[1] * p)], ]
  test = data[index[((ceiling(dim(data)[1] * p)) + 1):
                      dim(data)[1]], ] 
  return(list(train=train, test=test)) 
}


allset = split.data(dataset, p = 0.7, s = 1)
trainset = allset$train
testset = allset$test

summary(trainset)

barplot(table(trainset$Outcome), 
        main = "Diabetici", 
        names = c("No", "Sì"))

hist(trainset$Pregnancies, 
     main = "Gravidanze", 
     xlab="Numero di gravidanze")

hist(trainset$Glucose, 
     main = "Glucosio", 
     xlab="Livello di glucosio")

hist(trainset$BloodPressure, 
     main = "Pressione del sangue", 
     xlab="valore della pressione del sangue")


hist(trainset$SkinThickness, 
     main = "Spessore della pelle", 
     xlab="Spessore")

hist(trainset$Insulin, 
     main = "Insulina", 
     xlab="Livello di insulina")

hist(trainset$BMI, 
     main = "BMI", 
     xlab="Indice BMI")

hist(trainset$Age, 
     main = "Età",
     xlab="Età")

boxplot(trainset$Pregnancies,
        col = "lightgreen",
        border = "darkgreen",
        horizontal = TRUE,
        main = "Distribuzione gravidanze nel trainset",
        xlab = "Numero di gravidanze"
)

barplot(table(trainset$Outcome, trainset$Age),
        col=c("darkblue","red"),
        legend = c("Non diabetici", "Diabetici"),
        main = "Individui diabetici per età",
        ylab = "Frequency",
        xlab = "Età")

barplot(table(trainset$Outcome, trainset$Pregnancies),
        col=c("darkblue","red"),
        legend = c("Non diabetici", "Diabetici"),
        main = "Individui diabetici per gravidanza",
        ylab = "Frequency",
        xlab = "Numero di gravidanze")

barplot(table(trainset$Outcome, trainset$Glucose),
        col=c("darkblue","red"),
        legend = c("Non diabetici", "Diabetici"),
        main = "Individui diabetici per glucosio",
        ylab = "Frequency",
        xlab = "Individui diabetici per glucosio")

counts = table(trainset$Outcome, trainset$BloodPressure)
barplot(counts, 
        col=c("darkblue","red"), 
        legend = c("Non diabetici", "Diabetici"), 
        main = "Pazienti con diabete per pressione sanguigna")

xyplot(trainset$Insulin ~ trainset$Glucose, 
       data = trainset, 
       group = Outcome,
       col=c("darkblue","red"),
       auto.key = TRUE,
       ylab = "Livello di insulina",
       xlab = "Livello di glucosio")

trainset.active = trainset[, -9]

res.pca = PCA(trainset.active, graph = FALSE)
eig.val = get_eigenvalue(res.pca)
eig.val

var = get_pca_var(res.pca)
fviz_pca_var(res.pca, col.var = "black")

fviz_pca_var(res.pca, col.var = "black", axes = c(1,3))

fviz_pca_var(res.pca, col.var = "black", axes = c(1,4))

ind = get_pca_ind(res.pca)
fviz_pca_ind(res.pca,
             axes = c(1, 2),
             col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

ind = get_pca_ind(res.pca)
fviz_pca_ind(res.pca,
             axes = c(1, 3),
             col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

ind = get_pca_ind(res.pca)
fviz_pca_ind(res.pca,
             axes = c(1, 4),
             col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

fviz_pca_biplot(res.pca,
                geom.ind = "point",
                col.ind = trainset$Outcome,
                addEllipses = TRUE,
                legend.title = "Groups")

set.seed(1)
dt = rpart(Outcome ~ ., 
           data = trainset, 
           method = "class", 
           control = rpart.control(xval=10))
fancyRpartPlot(dt)

plotcp(dt)

pdt = rpart(Outcome ~ ., 
            data = trainset, 
            method = "class", 
            control = rpart.control(xval=10,
                                    cp = 0.057))
fancyRpartPlot(pdt)

dtp = predict(dt, testset, type = "class")
rdt = confusionMatrix(dtp,
                      testset$Outcome,
                      mode = "prec_recall")
rdt

pdtp = predict(pdt, testset, type = "class")
rpdt = confusionMatrix(pdtp,
                       testset$Outcome,
                       mode = "prec_recall")
rpdt

dt.pred.cart = predict(dt, 
                       newdata = testset, 
                       type = "prob")[, 2]
dt.pred.rocr = prediction(dt.pred.cart, testset$Outcome)
dt.perf.tpr.rocr = performance(dt.pred.rocr, "tpr", "fpr")
dt.perf.rocr = performance(dt.pred.rocr, 
                           measure = "auc", 
                           x.measure = "cutoff")
plot(dt.perf.tpr.rocr, 
     main=paste("DT AUC:",(dt.perf.rocr@y.values)))
abline(0, 1, lty = 2)

opt.cut = function(perf, pred){
  cut.ind = mapply(FUN=function(x, y, p){
    d = (x - 0)^2 + (y-1)^2
    ind = which(d == min(d))
    c(sensitivity = y[[ind]], 
      specificity = 1-x[[ind]],
      cutoff = p[[ind]])
  }, perf@x.values, perf@y.values, pred@cutoffs)
}

print("DT")
print(opt.cut(dt.perf.tpr.rocr, dt.pred.rocr))

dt.acc.perf = performance(dt.pred.rocr, measure = "acc")
plot(dt.acc.perf)

ind = which.max(slot(dt.acc.perf, "y.values")[[1]] ) 
dt.acc = slot(dt.acc.perf, "y.values")[[1]][ind]
cutoff = slot(dt.acc.perf, "x.values")[[1]][ind] 
print(c(accuracy = dt.acc, cutoff = cutoff))

pdt.pred.cart = predict(pdt, 
                        newdata = testset, 
                        type = "prob")[, 2]
pdt.pred.rocr = prediction(pdt.pred.cart, testset$Outcome)
pdt.perf.tpr.rocr = performance(pdt.pred.rocr, "tpr", "fpr")
pdt.perf.rocr = performance(pdt.pred.rocr, 
                            measure = "auc", 
                            x.measure = "cutoff")
plot(pdt.perf.tpr.rocr, 
     main=paste("PDT AUC:",(pdt.perf.rocr@y.values)))
abline(0, 1, lty = 2)

print("PDT")
print(opt.cut(pdt.perf.tpr.rocr, pdt.pred.rocr))

pdt.acc.perf = performance(pdt.pred.rocr, measure = "acc")
plot(pdt.acc.perf)

ind = which.max(slot(pdt.acc.perf, "y.values")[[1]]) 
pdt.acc = slot(pdt.acc.perf, "y.values")[[1]][ind]
cutoff = slot(pdt.acc.perf, "x.values")[[1]][ind] 
print(c(accuracy = pdt.acc, cutoff = cutoff))

print("Albero normale: ")
rdt$table
print('-----------------')
print("Albero pruned: ")
rpdt$table

print("Albero normale: ")
rdt$overall['Accuracy']
print('-----------------')
print("Albero pruned: ")
rpdt$overall['Accuracy']

print("Albero normale: ")
rdt$byClass['Precision']
print('-----------------')
print("Albero pruned: ")
rpdt$byClass['Precision']

print("Albero normale: ")
rdt$byClass['Recall']
print('-----------------')
print("Albero pruned: ")
rpdt$byClass['Recall']

print("Albero normale: ")
rdt$byClass['F1']
print('-----------------')
print("Albero pruned: ")
rpdt$byClass['F1']

plot(dt.perf.tpr.rocr,
     col = "darkblue",
     main=paste("DT AUC:", 
                round((dt.perf.rocr@y.values[[1]]), 4),
                "  |  ",
                "PDT AUC:", 
                round(pdt.perf.rocr@y.values[[1]], 4)))
plot(pdt.perf.tpr.rocr,
     add = TRUE,
     col = "red")
abline(0, 1, lty = 2)
legend("bottomright", legend=c("Albero normale", "Albero pruned"),
       col=c("darkblue", "red"), lty=1, cex=0.8)




control = trainControl(method = "cv", 
                       number = 10, 
                       classProbs = TRUE, 
                       summaryFunction = twoClassSummary)

bayes.model = train(Outcome ~ .,
                    data = trainset, 
                    method = "naive_bayes", 
                    metric = "ROC",
                    trControl = control)

nbp = predict(bayes.model, testset)
resultBayes = confusionMatrix(nbp, 
                              testset$Outcome, 
                              mode = "prec_recall")
resultBayes

nb = bayes.model
nb.pred.nb = predict(nb, newdata = testset, type = "prob")[,2]
nb.pred.rocr = prediction(nb.pred.nb, testset$Outcome) 
nb.perf.tpr.rocr = performance(nb.pred.rocr, "tpr", "fpr")
nb.perf.rocr = performance(nb.pred.rocr, measure = "auc", x.measure = "cutoff")
plot(nb.perf.tpr.rocr, main=paste("AUC:",(nb.perf.rocr@y.values)))
abline(0, 1, lty = 2)

print("Bayes cut-off")
print(opt.cut(nb.perf.tpr.rocr, nb.pred.rocr))

nb.acc.perf = performance(nb.pred.rocr, measure = "acc")
plot(nb.acc.perf)

ind = which.max(slot(nb.acc.perf, "y.values")[[1]]) 
nb.acc = slot(nb.acc.perf, "y.values")[[1]][ind]
cutoff = slot(nb.acc.perf, "x.values")[[1]][ind] 
print(c(accuracy = nb.acc, cutoff = cutoff))

print("Albero: ")
rdt$table
print('-----------------')
print("Naive Bayes: ")
resultBayes$table

print("Albero: ")
rdt$overall['Accuracy']
print('-----------------')
print("Naive Bayes: ")
resultBayes$overall['Accuracy']

print("Albero: ")
rdt$byClass['Precision']
print('-----------------')
print("Naive Bayes: ")
resultBayes$byClass['Precision']

print("Albero: ")
rdt$byClass['Recall']
print('-----------------')
print("Naive Bayes: ")
resultBayes$byClass['Recall']

print("Albero: ")
rdt$byClass['F1']
print('-----------------')
print("Naive Bayes: ")
resultBayes$byClass['F1']

plot(dt.perf.tpr.rocr,
     col = "darkblue",
     main=paste("Tree AUC:", 
                round((dt.perf.rocr@y.values[[1]]), 4),
                "  |  ",
                "Bayes AUC:", 
                round(nb.perf.rocr@y.values[[1]], 4)))
plot(nb.perf.tpr.rocr,
     add = TRUE,
     col = "red")
abline(0, 1, lty = 2)
legend("bottomright", legend=c("Albero di decisione", 
                               "Naive Bayes"),
       col=c("darkblue", "red"), 
       lty=1, cex=0.8)
