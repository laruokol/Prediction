# roc.curve
# 
# This function plots the receiver-operator (ROC) curve
# for a prediction model and returns the area undert the
# curve, which can be used as an index of model perfor-
# mance. 
#
# INPUT:  model             A model object generated 
#                           with training data.
#         test.data         A data.frame containing 
#                           the test data.
#         response.variable Either the name or column 
#                           index of the response in 
#                           the test data.
#         plot              Plot or not? (default = TRUE)
#         ...               Additional arguments passed 
#                           to plot.
#
# (c) Lasse Ruokolainen -- October 2017
##########################################################

roc.curve = function(model, test.data, response.variable, plot = T,...){
  library(ROCR)
  
  
  # Make sure response variable is binary (0/1):
  y = as.numeric(factor(test.data[,response.variable])) - 1

  # Output model prediction as probabilities:
  pred1 = predict(model,test.data,type = 'prob')
  
  # Create prediction object:
  pred2 = prediction(pred1[,2],y)
  
  # Calculate receiver-operator metrics:
  perf = performance(pred2,'tpr','fpr')
  
  # Plot ROC-curve
  plot(perf,col='dodgerblue',lwd=1.5,...)
    lines(c(0,1),c(0,1),lty=2)
    
  # Calculate Area Under the roc-Curve (AUC):  
  auc = unlist(performance(pred2,'auc')@y.values)
  
  print('AUC:')
  return(auc)
}