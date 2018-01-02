# cv.roc.curve
#
# This function produces an average ROC-curve for 
# cross-validation (CV). The function outputs the average 
# ROC-curve across k CV-folds as well as the min and max,
# and the summary for AUC across the folds. 
#
# INPUT:   predictions  A matrix of predictions, one column
#                       for each CV-fold.
#          test.labels  A matrix of test labels.
#          plot       Should the result be plotted?
#                     (default = TRUE)
#
# (c) Lasse Ruokolainen -- December 2017
#         last modified -- January 2018
##############################################################

cv.roc.curve = function(predictions,test.labels,plot = TRUE){
  
  args = as.character(match.call())[-1]
  if(length(args)<2) stop('Too few input arguments')
  if(!is.matrix(predictions)) stop('A matrix of predictions is required')
  if(!is.matrix(test.labels)) stop('A matrix of labels is required')  

  # Load required packages
  .require.package('foreach')
  .require.package('parallel')
  .require.package('ROCR')
  .require.package('ggplot2')	
  
  # Generate prediction objects:
  preds = lapply(colnames(predictions),function(namex) prediction(predictions[,namex],test.labels[,namex]))
  
  # Calculate performance metrics:	
  perf = lapply(preds,function(x) performance(x,'tpr','fpr'))
  auc = do.call(c,mclapply(preds,function(x) unlist(performance(x,'auc')@y.values)))
  
  # Interpolate ROC-curves fro compatibility:
  x.out = seq(0,1,len=100)
  interp = mclapply(perf,function(x) approx(x=unlist(x@x.values),
                                            y=unlist(x@y.values),xout=x.out))
  interp.y = mclapply(interp,function(x) do.call(cbind,x)[,'y'])
  interp.y = do.call(cbind,interp.y)
  
  # Build data frame:
  mu.y = rowMeans(interp.y)
  rng.y = apply(interp.y,1,function(x) c(min(x),max(x)))
  df = data.frame(cbind(x.out,mu.y,t(rng.y)))
  names(df) = c('x','mean','min','max')
  
  # Plotting:
  if(plot == TRUE){
    p = ggplot(df,aes(x,mean))+
      geom_abline(intercept=0,slope=1,col='gray50',linetype=2)+
      geom_ribbon(aes(x=x, ymax=max, ymin=min), fill="red3", alpha=.25)+
      geom_line(col='red3',size=1)+
      xlab('True negative rate') + ylab('True positive rate')+
      theme_classic()
    print(p)
  }
  
  
  # Output:
  cat(paste('AUC summary:','\n'))
  print(summary(auc))
  return(list(roc=df,auc=auc))
}

.require.package = function(package, ...) {
  if(suppressWarnings(!require(package, character.only=T, quietly=T))) { 
    fun = sys.calls()[[1]][1]
    message(paste("Function ", fun, " needs: ", package,
                  "; not found, installing...", sep=""))
    install.packages(package, ...)
    require(package, character.only=T)
  }
}
