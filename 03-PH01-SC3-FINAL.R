#-----------------
# TRAIN, CV & TEST
rm(list=ls())
load('./data/sc3.data.RData')
#-------------------------------
# SPLIT DATA INTO TRAIN AND TEST
set.seed(907)
train.idx <- caret::createDataPartition(
  y = sc3.data$outcome$SURVIVAL_STATUS,
  times = 1,
  p = 0.8
)[[1]]
tr.samp <- sc3.data$outcome$PATIENTID[train.idx]
table(sc3.data$outcome$SURVIVAL_STATUS[train.idx])
#------------------
# 80% dataset
# 0   1 
# 31 102
#------------------
sc3.train <- list(
  phenotype = sc3.data$phenotype[sc3.data$phenotype$PATIENTID %in% tr.samp,],
  y = sc3.data$outcome[sc3.data$outcome$PATIENTID %in% tr.samp,'SURVIVAL_STATUS'],
  x = sc3.data$feature[sc3.data$feature$PATIENTID %in% tr.samp,-1]
)
sc3.test <- list(
  phenotype = sc3.data$phenotype[!(sc3.data$phenotype$PATIENTID %in% tr.samp),],
  y = sc3.data$outcome[!(sc3.data$outcome$PATIENTID %in% tr.samp),'SURVIVAL_STATUS'],
  x = sc3.data$feature[!(sc3.data$feature$PATIENTID %in% tr.samp),-1]
)
save(sc3.train,sc3.test,file = './data/PH01-SC3-model-data.RData')
#----------------
# Feature reduction
#----------------
rm(list = ls())
load('./data/PH01-SC3-model-data.RData')
source('func-room.R')
idx.0 <- sc3.train$y == 0
idx.1 <- sc3.train$y == 1
feature.pvals <- apply(sc3.train$x,2,performTEST)
sc3.feature.pvals <- feature.pvals[order(feature.pvals)]
save(sc3.feature.pvals,
     file='./data/PH01-SC3-feature-pvals.RData')
#-----------------------
# TRAINING PHASE, 5-FOLD CV 
rm(list=ls())
load('./data/PH01-SC3-feature-pvals.RData')
load('./data/PH01-SC3-model-data.RData')
source('func-room.R')
p.cutoffs <- c(1.0e-06,1.0e-05,1.0e-04,1.0e-03,1.0e-02)
for(i in 1:length(p.cutoffs)){
  message('Threshold pval <= ',p.cutoffs[i])
  p.cutoff <- p.cutoffs[i]
  sc3.features <- sc3.feature.pvals[sc3.feature.pvals <= p.cutoff]
  #---------------
  model.feature <- names(sc3.features)
  tr.x <- sc3.train$x[,model.feature]
  tr.y <- as.factor(sc3.train$y)
  tt.x <- sc3.test$x[,model.feature]
  tt.y <- as.factor(sc3.test$y)
  
  ph01.sc3.SVM.mods <- performSVM(
    tr.x = tr.x,
    tr.y = tr.y,
    tt.x = tt.x,
    tt.y = tt.y,
    SEED = 768
  )
  perf.PH01.SC3 <- getPerfMetrics(x = ph01.sc3.SVM.mods)
  outname <- paste0(
    './data/MODEL-PERF-PH01/perf.PH01.SC3',
    p.cutoff,'.RData')
  save(perf.PH01.SC3,
       file = outname)
  p.train <- get.TPR.FPR.plot(x = ph01.sc3.SVM.mods,type = 'train')
  p.test <- get.TPR.FPR.plot(x = ph01.sc3.SVM.mods,type = 'test')
  outfile <- paste0('./figures/PH01-SC3-SVM-',p.cutoff,'.png')
  png(outfile,
      width = 1000,height = 1000,
      res = 200)
  gridExtra::grid.arrange(
    p.train + ggtitle(paste('Train | pval <= ',p.cutoff)),
    p.test + ggtitle(paste('Test | pval <= ',p.cutoff)))
  dev.off()
}
#-----------------------
# FINAL MODEL
rm(list=ls())
load('./data/PH01-SC3-feature-pvals.RData')
load('./data/PH01-SC3-model-data.RData')
sc3.features <- sc3.feature.pvals[sc3.feature.pvals <= 1.0e-06]
#---------------
model.feature <- names(sc3.features)
message('Features used!')
message(paste(model.feature,collapse = ', '))
tr.x <- sc3.train$x[,model.feature]
tr.y <- as.factor(sc3.train$y)
tt.x <- sc3.test$x[,model.feature]
tt.y <- as.factor(sc3.test$y)
sc3.mod.data <- list(
  tr.x = tr.x,
  tr.y = tr.y,
  tt.x = tt.x,
  tt.y = tt.y
)
save(sc3.mod.data,
     file = './data/MODEL-DATA-PH01/sc3.mod.data.RData')
#-------------------------------------------------------
#-------------------------------------------------------
rm(list=ls())
load('./data/MODEL-DATA-PH01/sc3.mod.data.RData')
source('func-room.R')
ph01.sc3.SVM.mods <- performSVM(
  tr.x = sc3.mod.data$tr.x,
  tr.y = sc3.mod.data$tr.y,
  tt.x = sc3.mod.data$tt.x,
  tt.y = sc3.mod.data$tt.y,
  SEED = 768
)
perf.PH01.SC3 <- getPerfMetrics(x = ph01.sc3.SVM.mods)
sink('./submission/Summary-Phase1-SC3-TRAIN.txt')
print(perf.PH01.SC3$tr)
sink()
outname <- './data/MODEL-PERF-FINAL-PH01/perf.PH01.SC3.FINAL.RData'
save(perf.PH01.SC3,
     file = outname)