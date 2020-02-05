#-----------------
# TRAIN, CV & TEST
rm(list=ls())
load('./data/sc1.data.RData')
#-------------------------------
# SPLIT DATA INTO TRAIN AND TEST
set.seed(907)
train.idx <- caret::createDataPartition(
  y = sc1.data$outcome$SURVIVAL_STATUS,
  times = 1,
  p = 0.8
)[[1]]
tr.samp <- sc1.data$outcome$PATIENTID[train.idx]
table(sc1.data$outcome$SURVIVAL_STATUS[train.idx])
#------------------
# 80% dataset
# 0   1 
# 40 262
#------------------
sc1.train <- list(
  phenotype = sc1.data$phenotype[sc1.data$phenotype$PATIENTID %in% tr.samp,],
  y = sc1.data$outcome[sc1.data$outcome$PATIENTID %in% tr.samp,'SURVIVAL_STATUS'],
  x = sc1.data$feature[sc1.data$feature$PATIENTID %in% tr.samp,-1]
)
sc1.test <- list(
  phenotype = sc1.data$phenotype[!(sc1.data$phenotype$PATIENTID %in% tr.samp),],
  y = sc1.data$outcome[!(sc1.data$outcome$PATIENTID %in% tr.samp),'SURVIVAL_STATUS'],
  x = sc1.data$feature[!(sc1.data$feature$PATIENTID %in% tr.samp),-1]
)
save(sc1.train,sc1.test,file = './data/PH01-SC1-model-data.RData')
#----------------
# Feature reduction
#----------------
rm(list = ls())
load('./data/PH01-SC1-model-data.RData')
source('func-room.R')
idx.0 <- sc1.train$y == 0
idx.1 <- sc1.train$y == 1
feature.pvals <- apply(sc1.train$x,2,performTEST)
sc1.feature.pvals <- feature.pvals[order(feature.pvals)]
save(sc1.feature.pvals,
     file='./data/PH01-SC1-feature-pvals.RData')
#-----------------------
# TRAINING PHASE, 5-FOLD CV 
rm(list=ls())
load('./data/PH01-SC1-feature-pvals.RData')
load('./data/PH01-SC1-model-data.RData')
source('func-room.R')
p.cutoffs <- c(1.0e-05,1.0e-04,1.0e-03,1.0e-02)
for(i in 1:length(p.cutoffs)){
  message('Threshold pval <= ',p.cutoffs[i])
  p.cutoff <- p.cutoffs[i]
  sc1.features <- sc1.feature.pvals[sc1.feature.pvals <= p.cutoff]
  #---------------
  model.feature <- names(sc1.features)
  tr.x <- sc1.train$x[,model.feature]
  tr.y <- as.factor(sc1.train$y)
  tt.x <- sc1.test$x[,model.feature]
  tt.y <- as.factor(sc1.test$y)
  
  ph01.sc1.SVM.mods <- performSVM(
    tr.x = tr.x,
    tr.y = tr.y,
    tt.x = tt.x,
    tt.y = tt.y,
    SEED = 768
  )
  perf.PH01.SC1 <- getPerfMetrics(x = ph01.sc1.SVM.mods)
  dir.create('./data/MODEL-PERF-PH01/',showWarnings = FALSE)
  outname <- paste0(
    './data/MODEL-PERF-PH01/perf.PH01.SC1',
    p.cutoff,'.RData')
  save(perf.PH01.SC1,
       file = outname)
  
  p.train <- get.TPR.FPR.plot(x = ph01.sc1.SVM.mods,type = 'train')
  p.test <- get.TPR.FPR.plot(x = ph01.sc1.SVM.mods,type = 'test')
  dir.create('./figures',showWarnings = FALSE)
  outfile <- paste0('./figures/PH01-SC1-SVM-',p.cutoff,'.png')
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
load('./data/PH01-SC1-feature-pvals.RData')
load('./data/PH01-SC1-model-data.RData')
source('func-room.R')
sc1.features <- sc1.feature.pvals[sc1.feature.pvals <= 1.0e-05]
#---------------
model.feature <- names(sc1.features)
message('Features used!')
message(paste(model.feature,collapse = ', '))
tr.x <- sc1.train$x[,model.feature]
tr.y <- as.factor(sc1.train$y)
tt.x <- sc1.test$x[,model.feature]
tt.y <- as.factor(sc1.test$y)

ph01.sc1.SVM.mods <- performSVM(
  tr.x = tr.x,
  tr.y = tr.y,
  tt.x = tt.x,
  tt.y = tt.y,
  SEED = 768
)
perf.PH01.SC1 <- getPerfMetrics(x = ph01.sc1.SVM.mods)
sink('./submission/Summary-Phase1-SC1-TRAIN.txt')
print(perf.PH01.SC1$tr)
sink()
dir.create('./data/MODEL-PERF-FINAL-PH01/',showWarnings = FALSE)
outname <- './data/MODEL-PERF-FINAL-PH01/perf.PH01.SC1.FINAL.RData'
save(perf.PH01.SC1,
     file = outname)
