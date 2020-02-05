performTEST <- function(x){
  if(sd(x) != 0){
  x.0 <- x[idx.0]
  x.1 <- x[idx.1]
  df_ <- data.frame(
    class = rep(
      c('0','1'),
      c(length(x.0),
        length(x.1))),
    value = c(
      x.0,
      x.1
    )
  )
  normal <- shapiro.test(df_$value)$p.value > 0.05
  if(normal){
    pval <- t.test(value~class,data=df_)$p.value
  }else{
    pval <- wilcox.test(value~class,data=df_)$p.value
  }
  }else{
    pval <- 1
  }
  return(pval)
}

performSVM <- function(tr.x,tr.y,tt.x,tt.y,SEED=7860){
  tr.y <- as.factor(tr.y)
  library(e1071)
  library(caret)
  library(foreach)
  set.seed(SEED)
  cv.5 <- createFolds(tr.y,k = 5)
  out <- list()
  message('Performing SVM!')
  for(i in 1:length(cv.5)){
    message(paste0('CV = ',i))
    idx.tr <- unlist(cv.5[-i])
    idx.tt <- unlist(cv.5[i])
    mini.tr.x <- tr.x[idx.tr,]
    mini.tr.y <- tr.y[idx.tr]
    
    mini.tt.x <- tr.x[idx.tt,]
    mini.tt.y <- tr.y[idx.tt]
    
    #--------------------
    # SVM MODELS
    mod.linear <- svm(
      x = mini.tr.x,
      y= mini.tr.y,
      kernel = 'linear',
      type = 'C-classification',
      probability = TRUE
    )
    pred.linear <- predict(
      mod.linear,
      mini.tt.x,
      probability = FALSE
    )
    pred.linear.prob <- predict(
      mod.linear,
      mini.tt.x,
      probability = TRUE
    )
    mod.radial <- svm(
      x = mini.tr.x,
      y= mini.tr.y,
      kernel = 'radial',
      type = 'C-classification',
      probability = TRUE
    )
    pred.radial <- predict(
      mod.radial,
      mini.tt.x
    )
    pred.radial.prob <- predict(
      mod.radial,
      mini.tt.x,
      probability = TRUE
    )
    mod.sigmoid <- svm(
      x = mini.tr.x,
      y= mini.tr.y,
      kernel = 'sigmoid',
      type = 'C-classification',
      probability = TRUE
    )
    pred.sigmoid <- predict(
      mod.sigmoid,
      mini.tt.x
    )
    pred.sigmoid.prob <- predict(
      mod.sigmoid,
      mini.tt.x,
      probability = TRUE
    )
    tmpOUT.r <- getCVperf(
      pred = pred.radial,
      orig = mini.tt.y,
      mod.method = 'SVM.radial'
    )
    tmpOUT.s <- getCVperf(
      pred = pred.sigmoid,
      orig = mini.tt.y,
      mod.method = 'SVM.sigmoid'
    )
    tmpOUT.l <- getCVperf(
      pred = pred.linear,
      orig = mini.tt.y,
      mod.method = 'SVM.linear'
    )
    perf.svm <- list(
      'linear' = tmpOUT.l,
      'sigmoid'=tmpOUT.s,
      'radial'=tmpOUT.r
      
    )
    auc.perf <- list(
      'linear' = getAUC(
        pred.prob = pred.linear.prob,
        orig = mini.tt.y
      ),
      'sigmoid'= getAUC(
        pred.prob = pred.sigmoid.prob,
        orig = mini.tt.y),
      'radial'= getAUC(
        pred.prob = pred.radial.prob,
        orig = mini.tt.y)
    )
    #---------
    # PREDICT TT
    tt.linear <- getCVperf(
      pred = predict(mod.linear,tt.x),
      orig = tt.y,
      mod.method = 'SVM.linear'
    )
    tt.sigmoid <- getCVperf(
      pred = predict(mod.sigmoid,tt.x),
      orig = tt.y,mod.method = 'SVM.sigmoid'
    )
    tt.radial <- getCVperf(
      pred = predict(mod.radial,tt.x),
      orig = tt.y,mod.method = 'SVM.radial'
    )
    tt.out <- list(
      'linear' = tt.linear,
      'sigmoid' = tt.sigmoid,
      'radial' = tt.radial
    )
    #-------------------------
    # tt AUC
    tt.auc <- list(
      'linear' = getAUC(
        pred.prob = predict(
          mod.linear,
          tt.x,
          probability = TRUE
        ),
        orig = tt.y
      ),
      'sigmoid'= getAUC(
        pred.prob = predict(
          mod.sigmoid,
          tt.x,
          probability = TRUE
        ),
        orig = tt.y),
      'radial'= getAUC(
        pred.prob = predict(
          mod.radial,
          tt.x,
          probability = TRUE
        ),
        orig = tt.y)
    )
    OUT <- list(
      tr.perf = list(
        AUC = sapply(auc.perf,function(x){x$AUC}),
        TRP.FPR =  lapply(auc.perf,function(x){x$TPR.FPR}),
        summary = perf.svm),
      tt.perf = list(
        AUC = sapply(tt.auc,function(x){x$AUC}),
        TRP.FPR =  lapply(tt.auc,function(x){x$TPR.FPR}),
        summary = tt.out
      )
    )
    out[[i]] <- OUT
  }
  names(out) <- paste0('CV',1:length(cv.5))
  return(out)
}
getCVperf <- function(pred,
                      orig,
                      mod.method){
  xout <- caret::confusionMatrix(
    data = pred,
    reference=orig
  )
  return(xout)
}
getAUC <- function(pred.prob,orig){
  preds <- attr(pred.prob,'probabilities')[,2]
  pred <- ROCR::prediction(
    predictions = preds,
    labels = as.factor(as.character(orig))
  )
  AUC <- ROCR::performance(pred,"auc")@y.values[[1]]
  perf.tpr.fpr <- ROCR::performance(pred,"tpr","fpr")
  return(
    list(
      AUC = AUC,
      TPR.FPR =  perf.tpr.fpr
    )
  )
}
getPerfMetrics <- function(x){
  # x <- ph01.sc1.SVM.mods
  #' ------------------------------------------------
  #' Overall accuracy 
  #' ------------------------------------------------
  tr.Accuracy <- do.call(what = 'rbind',lapply(x,function(x){
    sapply(x$tr.perf$summary,function(x){
      x$overall['Accuracy']
    })}
  ))
  tt.Accuracy <- do.call(what = 'rbind',lapply(x,function(x){
    sapply(x$tt.perf$summary,function(x){
      x$overall['Accuracy']
    })}
  ))
  
  #' ------------------------------------------------
  #' Confusion matrix indicating number of predictions, 
  #' true positives, 
  #' false positives, 
  #' true negatives, and 
  #' false negatives 
  #' ------------------------------------------------
  cm.tr <- lapply(x,function(x){
    lapply(x$tr.perf$summary,function(x){
      x$table
    })}
  )
  cm.tt <- lapply(x,function(x){
    lapply(x$tt.perf$summary,function(x){
      x$table
    })}
  )
  #' ------------------------------------------------
  #' Specificity
  #' Sensitivity
  #' ------------------------------------------------
  tr.spc <- do.call(what = 'rbind',lapply(x,function(x){
    sapply(x$tr.perf$summary,function(x){
      x$byClass[c('Specificity')]
    })}
    ))
  tt.spc <- do.call(what = 'rbind',lapply(x,function(x){
    sapply(x$tt.perf$summary,function(x){
      x$byClass[c('Specificity')]
    })}
  ))
  tr.sen <- do.call(what = 'rbind',lapply(x,function(x){
    sapply(x$tr.perf$summary,function(x){
      x$byClass[c('Sensitivity')]
    })}
  ))
  tt.sen <- do.call(what = 'rbind',lapply(x,function(x){
    sapply(x$tt.perf$summary,function(x){
      x$byClass[c('Sensitivity')]
    })}
  ))
  #' ------------------------------------------------
  #' Area under the curve (AUC)
  #' ------------------------------------------------
  tr.AUC <- do.call(what = 'rbind',
                    lapply(x,function(x)x$tr.perf$AUC))
  tt.AUC <- do.call(what = 'rbind',
                    lapply(x,function(x)x$tt.perf$AUC))
  OUT <- list(
    tr = list(
      Accuracy = tr.Accuracy,
      ConfusionMatrix = cm.tr,
      Sensitivity = tr.sen,
      Specificity = tr.spc,
      AUC = tr.AUC
    ),
    tt = list(
      Accuracy = tt.Accuracy,
      ConfusionMatrix = cm.tt,
      Sensitivity = tt.sen,
      Specificity = tt.spc,
      AUC = tt.AUC
    )
  )
  return(OUT)
}

get.TPR.FPR.plot <- function(x,type='train'){
  if(type=='train'){
    xx <- plyr::ldply(lapply(
      x,
      function(x){
        xy <- plyr::ldply(lapply(
          x$tr.perf$TRP.FPR,
          function(x){
            df_ <- data.frame(x.values = x@x.values,
                              y.values = x@y.values)
            colnames(df_) <- c(x@x.name,x@y.name)
            return(df_)
          }
        ))
        colnames(xy)[1] <- 'MOD'
        xy
      }
    ))
    colnames(xx)[1] <- 'CV'
  }else{
    xx <- plyr::ldply(lapply(
      x,
      function(x){
        xy <- plyr::ldply(lapply(
          x$tt.perf$TRP.FPR,
          function(x){
            df_ <- data.frame(x.values = x@x.values,
                              y.values = x@y.values)
            colnames(df_) <- c(x@x.name,x@y.name)
            return(df_)
          }
        ))
        colnames(xy)[1] <- 'MOD'
        xy
      }
    ))
    colnames(xx)[1] <- 'CV'
  }
  
  library(ggplot2)
  colnames(xx)[3:4] <- c('FPR','TPR')
  p <- ggplot(xx,aes(x=FPR,y=TPR,group=CV, col=CV)) +
    geom_line() +
    facet_wrap(facets = 'MOD') +
    ggtitle(type) +
    theme(axis.text.x = element_text(angle = 45,vjust = 1,hjust = 1))
  return(p)
}