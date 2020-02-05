#-----------------
# TRAIN, CV & TEST
rm(list=ls())
load('./data/sc2.data.RData')
#-------------------------------
# SPLIT DATA INTO TRAIN AND TEST
set.seed(907)
train.idx <- caret::createDataPartition(
  y = sc2.data$outcome$SURVIVAL_STATUS,
  times = 1,
  p = 0.8
)[[1]]
tr.samp <- sc2.data$outcome$PATIENTID[train.idx]
table(sc2.data$outcome$SURVIVAL_STATUS[train.idx])
#------------------
# 80% dataset
# 0   1 
# 36 104
#------------------
sc2.train <- list(
  phenotype = sc2.data$phenotype[sc2.data$phenotype$PATIENTID %in% tr.samp,],
  y = sc2.data$outcome[sc2.data$outcome$PATIENTID %in% tr.samp,'SURVIVAL_STATUS'],
  x = sc2.data$feature[sc2.data$feature$PATIENTID %in% tr.samp,-1]
)
sc2.test <- list(
  phenotype = sc2.data$phenotype[!(sc2.data$phenotype$PATIENTID %in% tr.samp),],
  y = sc2.data$outcome[!(sc2.data$outcome$PATIENTID %in% tr.samp),'SURVIVAL_STATUS'],
  x = sc2.data$feature[!(sc2.data$feature$PATIENTID %in% tr.samp),-1]
)
save(sc2.train,sc2.test,file = './data/PH01-SC2-model-data.RData')
#----------------
# Feature reduction
#----------------
rm(list = ls())
load('./data/PH01-SC2-model-data.RData')
source('func-room.R')
idx.0 <- sc2.train$y == 0
idx.1 <- sc2.train$y == 1
feature.pvals <- apply(sc2.train$x,2,performTEST)
sc2.feature.pvals <- feature.pvals[order(feature.pvals)]
save(sc2.feature.pvals,
     file='./data/PH01-SC2-feature-pvals.RData')
#-----------------------
# TRAINING PHASE, 5-FOLD CV 
rm(list=ls())
load('./data/PH01-SC2-feature-pvals.RData')
load('./data/PH01-SC2-model-data.RData')
source('func-room.R')
p.cutoffs <- c(1.0e-02,0.05)
for(i in 1:length(p.cutoffs)){
  message('Threshold pval <= ',p.cutoffs[i])
  p.cutoff <- p.cutoffs[i]
  sc2.features <- sc2.feature.pvals[sc2.feature.pvals <= p.cutoff]
  #---------------
  model.feature <- names(sc2.features)
  tr.x <- sc2.train$x[,model.feature]
  tr.y <- as.factor(sc2.train$y)
  tt.x <- sc2.test$x[,model.feature]
  tt.y <- as.factor(sc2.test$y)
  
  ph01.sc2.SVM.mods <- performSVM(
    tr.x = tr.x,
    tr.y = tr.y,
    tt.x = tt.x,
    tt.y = tt.y,
    SEED = 768
  )
  perf.PH01.SC2 <- getPerfMetrics(x = ph01.sc2.SVM.mods)
  outname <- paste0(
    './data/MODEL-PERF-PH01/perf.PH01.SC2',
    p.cutoff,'.RData')
  save(perf.PH01.SC2,
       file = outname)
  
  p.train <- get.TPR.FPR.plot(x = ph01.sc2.SVM.mods,type = 'train')
  p.test <- get.TPR.FPR.plot(x = ph01.sc2.SVM.mods,type = 'test')
  outfile <- paste0('./figures/PH01-SC2-SVM-',p.cutoff,'.png')
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
load('./data/PH01-SC2-feature-pvals.RData')
load('./data/PH01-SC2-model-data.RData')
source('func-room.R')
sc2.features <- sc2.feature.pvals[sc2.feature.pvals <= 1.0e-02]
#---------------
model.feature <- names(sc2.features)
message('Features used!')
message(paste(gsub('X','',model.feature),collapse = ', '))
tr.x <- sc2.train$x[,model.feature]
tr.y <- as.factor(sc2.train$y)
tt.x <- sc2.test$x[,model.feature]
tt.y <- as.factor(sc2.test$y)

ph01.sc2.SVM.mods <- performSVM(
  tr.x = tr.x,
  tr.y = tr.y,
  tt.x = tt.x,
  tt.y = tt.y,
  SEED = 768
)
perf.PH01.SC2 <- getPerfMetrics(x = ph01.sc2.SVM.mods)
sink('./submission/Summary-Phase1-SC2-TRAIN.txt')
print(perf.PH01.SC2$tr)
sink()
outname <- './data/MODEL-PERF-FINAL-PH01/perf.PH01.SC2.FINAL.RData'
save(perf.PH01.SC2,
     file = outname)