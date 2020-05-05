#### importing packages ----
library(dplyr) # A Grammar of Data Manipulation
library(tidyverse) # Easily Install and Load the 'Tidyverse'
library(mice) # Multivariate Imputation by Chained Equations
library(VIM) # Visualization and Imputation of Missing Values
library(NbClust) # Determining the Best Number of Clusters in a Data Set
library(fpc) # Flexible Procedures for Clustering # Flexible Procedures for Clustering
library(yarrr) # A Companion to the e-Book "YaRrr!: The Pirate's Guide to R"
library(sysfonts) # Loading Fonts into R
library(showtext) # Using Fonts More Easily in R Graphs
library(Cairo) # R Graphics Device using Cairo Graphics Library for Creating
               # High-Quality Bitmap (PNG, JPEG, TIFF), Vector (PDF, SVG,
               # PostScript) and Display (X11 and Win32) Output
library(bruceR) # BRoadly Useful Collections and Extensions of R functions
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics
library(nortest) # Tests for Normality
library(lmerTest) # Tests in Linear Mixed Effects Models
library(fpc) # Flexible Procedures for Clustering # Flexible Procedures for Clustering
library(pracma) # Practical Numerical Math Functions
library(e1071) # Misc Functions of the Department of Statistics, Probability
               # Theory Group (Formerly: E1071), TU Wien
library(caret) # Classification and Regression Training

#### loading and reshaping the row data ----
### loading the row data
drug.ori <- readr::read_csv("abuserR.csv")
head(drug.ori)
dim(drug.ori)
view(drug.ori)

### dealing with the missing data
## missing patterns visualization
md.pattern(drug.ori)
matrixplot(drug.ori)
aggr(drug.ori, sortVars = TRUE, 
     sortCombs = TRUE,
     border = NA)
# the missing-case proportion in variables including Frequency, Religion, Family, Time_Span, Relapse 
# are over 11%, so these vars. should be deleted in further analysis.
aggr(t(drug.ori), sortVars = TRUE, 
     sortCombs = TRUE,
     border = NA)
# the missing-vars proportion in subjects including No.33, 34, 172, 173, 44, 53, 45, 156, 65, 91, 71, 87, 113 and 147 
# are over 11%, so these vars. should be deleted in further analysis.

## fliping out the vars and subjects with high missing proportion
drug.denoise <- drug.ori[-c(33, 34, 172, 173, 44, 53, 45, 156, 65, 91, 71, 87, 113, 147), -c(85, 89:92)]
dim(drug.denoise)
view(drug.denoise)
matrixplot(drug.denoise)

## missing value interpolation by 'mice'
# cleaning up PMP data
drug.denoise %>%
  select(PMP1, PMP5, PMP16) %>%
  mice() %>%
  complete() -> drug.complete
drug.denoise %>%
  select(PMP7, PMP8, PMP13) %>%
  mice() %>%
  complete() %>% cbind(drug.complete, .) -> drug.complete
drug.denoise %>%
  select(PMP4, PMP9, PMP18) %>%
  mice() %>%
  complete() %>% cbind(drug.complete, .) -> drug.complete
drug.denoise %>%
  select(PMP11, PMP15, PMP21) %>%
  mice() %>%
  complete() %>% cbind(drug.complete, .) -> drug.complete
drug.denoise %>%
  select(PMP1, PMP3, PMP17) %>%
  mice() %>%
  complete() %>% cbind(drug.complete, .) -> drug.complete
drug.denoise %>%
  select(PMP2, PMP12, PMP14) %>%
  mice() %>%
  complete() %>% cbind(drug.complete, .) -> drug.complete
drug.denoise %>%
  select(PMP10, PMP19, PMP20) %>%
  mice() %>%
  complete() %>% cbind(drug.complete, .) -> drug.complete
# cleaning up MLQ data
drug.denoise %>%
  select(paste0('MLQ', 1:5)) %>%
  mice() %>%
  complete() %>% cbind(drug.complete, .) -> drug.complete
# cleaning up ZTPI data
drug.denoise %>%
  select(ZIPI4, ZIPI5, ZIPI16, ZIPI22, ZIPI27,
         ZIPI33, ZIPI34, ZIPI36, ZIPI50, ZIPI54) %>%
  mice() %>%
  complete() %>% cbind(drug.complete, .) -> drug.complete
drug.denoise %>%
  select(ZIPI2, ZIPI7, ZIPI11, ZIPI15, ZIPI20, 
         ZIPI25, ZIPI29, ZIPI41, ZIPI49) %>%
  mice() %>%
  complete() %>% cbind(drug.complete, .) -> drug.complete
drug.denoise %>%
  select(ZIPI1, ZIPI8, ZIPI12, ZIPI17, ZIPI19, 
         ZIPI23, ZIPI26, ZIPI28, ZIPI31, ZIPI32, 
         ZIPI42, ZIPI44, ZIPI46, ZIPI48, ZIPI55) %>%
  mice() %>%
  complete() %>% cbind(drug.complete, .) -> drug.complete
drug.denoise %>%
  select(ZIPI3, ZIPI14, ZIPI35, ZIPI37, ZIPI38, ZIPI39, 
         ZIPI47, ZIPI52, ZIPI53) %>%
  mice() %>%
  complete() %>% cbind(drug.complete, .) -> drug.complete
drug.denoise %>%
  select(ZIPI6, ZIPI9, ZIPI10, ZIPI13, ZIPI18, ZIPI21, 
         ZIPI24, ZIPI30, ZIPI40, ZIPI43, ZIPI45, ZIPI51, ZIPI56) %>%
  mice() %>%
  complete() %>% cbind(drug.complete, .) -> drug.complete

matrixplot(drug.complete)
drug.complete <- cbind(drug.complete, drug.denoise[c('Age','Marriage','Education','Job')])
drug.complete['ID'] <- c(1:nrow(drug.complete))
dim(drug.complete)
View(drug.complete)
matrixplot(drug.complete)

#### estimating the reliability of each questionnaire ----
drug.complete %>%
  select(starts_with('PMP')) %>%
  alpha() # row-alpha = 0.82, std-alpha = 0.83
drug.complete %>%
  select(starts_with('MLQ')) %>%
  alpha() # row-alpha = 0.80, std-alpha = 0.80

### cauz ZTPI is a combating scale with different dimensions, the reliability should be estimation in separation
drug.complete %>%
  select(starts_with('ZIPI')) %>%
  alpha() # alpha of whole ZTPI = 0.83
drug.complete %>%
  select(ZIPI4, ZIPI5, ZIPI16, ZIPI22, ZIPI27,
         ZIPI33, ZIPI34, ZIPI36, ZIPI50, ZIPI54) %>%
  alpha() # alpha of negative past = 0.67
drug.complete %>%
  select(ZIPI2, ZIPI7, ZIPI11, ZIPI15, ZIPI20, 
         ZIPI25, ZIPI29, ZIPI41, ZIPI49) %>%
  alpha() # alpha of positive past = 0.45
drug.complete %>%
  select(ZIPI1, ZIPI8, ZIPI12, ZIPI17, ZIPI19, 
         ZIPI23, ZIPI26, ZIPI28, ZIPI31, ZIPI32, 
         ZIPI42, ZIPI44, ZIPI46, ZIPI48, ZIPI55) %>%
  alpha() # alpha of present = 0.68
drug.complete %>%
  select(ZIPI3, ZIPI14, ZIPI35, ZIPI37, ZIPI38, ZIPI39, 
         ZIPI47, ZIPI52, ZIPI53) %>%
  alpha() # alpha of fateful present = 0.69
drug.complete %>%
  select(ZIPI6, ZIPI9, ZIPI10, ZIPI13, ZIPI18, ZIPI21, 
         ZIPI24, ZIPI30, ZIPI40, ZIPI43, ZIPI45, ZIPI51, ZIPI56) %>%
  alpha() # alpha of future = 0.61

#### calculating the score of each questionnaire ----
### PMP scores
drug.complete %>%
  select(PMP1, PMP5, PMP16) %>%
  rowSums()/3 -> drug.complete['pmp.achi']
drug.complete %>%
  select(PMP7, PMP8, PMP13) %>%
  rowSums()/3 -> drug.complete['pmp.relation']
drug.complete %>%
  select(PMP4, PMP9, PMP18) %>%
  rowSums()/3 -> drug.complete['pmp.relig']
drug.complete %>%
  select(PMP11, PMP15, PMP21) %>%
  rowSums()/3 -> drug.complete['pmp.selfacc']
drug.complete %>%
  select(PMP1, PMP3, PMP17) %>%
  rowSums()/3 -> drug.complete['pmp.selftrans']
drug.complete %>%
  select(PMP2, PMP12, PMP14) %>%
  rowSums()/3 -> drug.complete['pmp.intim']
drug.complete %>%
  select(PMP10, PMP19, PMP20) %>%
  rowSums()/3 -> drug.complete['pmp.fair']
drug.complete %>%
  select(starts_with('PMP')) %>%
  rowSums()/21 -> drug.complete['pmp.total']

### MLQ scores
drug.complete %>%
  select(starts_with('MLQ')) %>%
  rowSums() -> drug.complete['mlq.total']

### ZTPI scores
drug.complete %>%
  select(ZIPI4, ZIPI5, ZIPI16, ZIPI22, ZIPI27,
         ZIPI33, ZIPI34, ZIPI36, ZIPI50, ZIPI54) %>%
  rowSums()/10 -> drug.complete['ztpi.negpast']
drug.complete %>%
  select(ZIPI2, ZIPI7, ZIPI11, ZIPI15, ZIPI20, 
         ZIPI25, ZIPI29, ZIPI41, ZIPI49) %>%
  rowSums()/9 -> drug.complete['ztpi.pospast']
drug.complete %>%
  select(ZIPI1, ZIPI8, ZIPI12, ZIPI17, ZIPI19, 
         ZIPI23, ZIPI26, ZIPI28, ZIPI31, ZIPI32, 
         ZIPI42, ZIPI44, ZIPI46, ZIPI48, ZIPI55) %>%
  rowSums()/15 -> drug.complete['ztpi.hepresent']
drug.complete %>%
  select(ZIPI3, ZIPI14, ZIPI35, ZIPI37, ZIPI38, ZIPI39, 
         ZIPI47, ZIPI52, ZIPI53) %>%
  rowSums()/9 -> drug.complete['ztpi.fatepresent']
drug.complete %>%
  select(ZIPI6, ZIPI9, ZIPI10, ZIPI13, ZIPI18, ZIPI21, 
         ZIPI24, ZIPI30, ZIPI40, ZIPI43, ZIPI45, ZIPI51, ZIPI56) %>%
  rowSums()/13 -> drug.complete['ztpi.future']

### sweap out the items' score
drug.data <- data.frame(drug.complete[,83:86], drug.complete[,88:101])
drug.data['id'] <- c(1:nrow(drug.data))
matrixplot(drug.data)

#### descriptive statistics ----
### output N, Mean, SD and range
Describe(drug.data[,5:18])
Describe(drug.data[,c(12,14:18)], plot = T, smooth = 'lm',
         save.file = 'cor-matrix.png', save.size = "15:8",save.dpi = 300)
### normalization test
lillie.test(drug.data$pmp.total) # D = 0.06, p = 0.18
lillie.test(drug.data$ztpi.negpast) # D = 0.06, p = 0.12
lillie.test(drug.data$ztpi.pospast) # D = 0.07, p = 0.04*
lillie.test(drug.data$ztpi.hepresent) # D = 0.4, p = 0.59
lillie.test(drug.data$ztpi.fatepresent) # D = 0.07, p = 0.05*
lillie.test(drug.data$ztpi.future) # D = 0.07, p = 0.03*

#### cluster analysis ----
drug.scale <- as.data.frame(sapply(drug.data[,5:18], scale))
ztpi.clust <- NbClust(drug.scale[,10:14],method = "kmeans")
drug.scale['cluster'] <- ztpi.clust[["Best.partition"]]
drug.kmean <- kmeans(drug.scale[,10:14], centers = 2)
plotcluster(drug.scale, drug.kmean$cluster) 
drug.data['cluster'] <- ztpi.clust[["Best.partition"]]

### Descriptive statistics of clustering result
drug.cluster.describe <- data.frame(as.factor(drug.data$Marriage), as.factor(drug.data$Education), as.factor(drug.data$Job), drug.data[,20])
colnames(drug.cluster.describe) <- c('marriage', 'education', 'job', 'cluster')
drug.cluster.describe %>%
  subset(cluster == '1') %>%
  summary()
drug.cluster.describe %>%
  subset(cluster == '2') %>%
  summary()
# we find no difference among demographic variables
# given the magnitude of two clusters are different, we just focused on the proportion of values

### Comparing the differences between categories
## Levene's test for variance equality
leveneTest(pmp.total ~ as.factor(cluster), data = drug.data) # F = 5.52, p = 0.02, varance unequal
## t test shows no difference in the MIL score between categories
t.test(drug.data$pmp.total~drug.data$cluster, conf = 0.95, var.equal = F) # t = -1.27, df = 142.07, p = 0.21

## group visualization
CairoPDF("clusterPlot.pdf")
pirateplot(formula = pmp.total ~ cluster, data = drug.data,
           pal = "southpark",
           theme = 0,
           point.o = 0.6,
           avg.line.o = 1,
           avg.line.lwd = 2,
           avg.line.col = "black",
           bar.b.o = 0,
           bean.b.o = 1,
           inf.f.o = 0.5,
           inf.method = "sd",
           ylab = "Meaning in Life",
           xlab = "Time style category")
dev.off()

#### representation similarity analysis ----
drug.cate1 <- drug.data[which(drug.data$cluster == 1), ]
drug.cate2 <- drug.data[which(drug.data$cluster == 2), ]
cor.dis1 <- 1 - cor(drug.cate1[,5:11], method = 'spearman')
cor.dis2 <- 1 - cor(drug.cate2[,5:11], method = 'spearman') 

CairoPDF("cluster1_RDM.pdf")
lattice::levelplot(cor.dis1, main = "cor.matrix of cate 1")
dev.off()

CairoPDF("cluster2_RDM.pdf")
lattice::levelplot(cor.dis2, main = "cor.matrix of cate 2")
dev.off()

rsa.cor <- cor(cor.dis1[upper.tri(cor.dis1, diag = F)], cor.dis2[upper.tri(cor.dis2, diag = F)], method = 'spearman')

# set random seed for reproducible results
set.seed(1)
nperm <- 5000 # set permutation count
nppl <- dim(cor.dis1)[1] # number of target people
permcor <- rep(NA,nperm) # preallocate results
diff.cor <- matrix(NA, nrow = nperm, ncol = length(cor.dis1[upper.tri(cor.dis1, diag = F)]))
for (i in 1:nperm){
  sel1 <- sample(nppl) # permutation vector
  sel2 <- sample(nppl) # permutation vector
  dis1.sim <- squareform(cor.dis1[sel1,sel1]) # permute matrix and re-vectorize neural similarity
  dis2.sim <- squareform(cor.dis2[sel2,sel2]) 
  permcor[i] <- cor(dis1.sim,dis2.sim, method = 'spearman') # calculate permuted correlation
}
# calculate p-value
mean(abs(permcor) > abs(cor(cor.dis1[upper.tri(cor.dis1, diag = F)], cor.dis2[upper.tri(cor.dis2, diag = F)], method = 'spearman')))
CairoPDF("permutation-test.pdf")
hist(abs(permcor),xlim=c(0,.8),main="Permuted null versus actual correlation")
abline(v=abs(cor(cor.dis1[upper.tri(cor.dis1, diag = F)], cor.dis2[upper.tri(cor.dis2, diag = F)], method = 'spearman')),col="red",lwd=2)
dev.off()

CairoPDF("RDM_plot.pdf")
lattice::levelplot(cor.dis1 - cor.dis2, main = "cor.matrix difference")
dev.off()

# bootstrap 95% CI
bootres <- replicate(5000,mean(sample(rsa.cor,length(rsa.cor),T)))
quantile(bootres,c(.025,.975))

plot(rowMeans(t(drug.cate1[,5:11])), rowMeans(t(drug.cate2[,5:11])))
cor(cor.dis1[upper.tri(cor.dis1, diag = F)], cor.dis2[upper.tri(cor.dis2, diag = F)])

#### support vector machine classification ----
### classification argrit
drug.svm.data <- drug.data[,c(5:11,20)]
drug.svm.data$cluster <- as.factor(drug.svm.data$cluster)

### cross validation - 10 folds corss validation
folds = createFolds(drug.svm.data$cluster, k = 10)
### start working!
cv = lapply(folds, function(x) { 
  # separating apart the training & testing set
  train.fold = drug.svm.data[-x, ]
  test.fold = drug.svm.data[x, ]
  # applying the classifer on the train.fold
  svm.models = svm(formula = cluster ~ .,
                   data = train.fold,
                   type = 'C-classification',
                   kernel = 'radial')
  # using svm.model to predict the test.fold
  pred.model = predict(svm.models, newdata = test.fold[-8])
  confus.mat <- confusionMatrix(pred.model, reference = test.fold[,8])
  return(matrix(data = c(confus.mat$overall[1], confus.mat$byClass[1], confus.mat$byClass[2])))
})
### turn the list into vector
svm.pre.result <- unlist(cv)
svm.pre.acc <- sum(svm.pre.result[seq(1, 30, by = 3)])/10 # 0.6917
svm.pre.sensi <- sum(svm.pre.result[seq(2, 30, by = 3)])/10 # 0.7444
svm.pre.speci <- sum(svm.pre.result[seq(3, 30, by = 3)])/10 # 0.6357

# what if sweap religion scores out of the predictive features
feature.weight <- matrix(data = NA, nrow = 7, ncol = 3)
colnames(feature.weight) <- c('accuracy', 'sensitive', 'specific')
for (i in 1:7) {
  drug.svm.data2 <- drug.svm.data[,-i]
  folds = createFolds(drug.svm.data2$cluster, k = 10)
  cv = lapply(folds, function(x) { 
    # separating apart the training & testing set
    train.fold = drug.svm.data[-x, ]
    test.fold = drug.svm.data[x, ]
    # applying the classifer on the train.fold
    svm.models = svm(formula = cluster ~ .,
                     data = train.fold,
                     type = 'C-classification',
                     kernel = 'radial')
    # using svm.model to predict the test.fold
    pred.model = predict(svm.models, newdata = test.fold[-8])
    confus.mat <- confusionMatrix(pred.model, reference = test.fold[,8])
    return(matrix(data = c(confus.mat$overall[1], confus.mat$byClass[1], confus.mat$byClass[2])))
  })
  # turn the list into vector
  svm.pre.result <- unlist(cv)
  feature.weight[i, 1] <- sum(svm.pre.result[seq(1, 30, by = 3)])/10
  feature.weight[i, 2] <- sum(svm.pre.result[seq(2, 30, by = 3)])/10
  feature.weight[i, 3] <- sum(svm.pre.result[seq(3, 30, by = 3)])/10
}
# checking results
feature.weight
  
