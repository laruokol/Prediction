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
#                           If yes, will also returbn a 
#                           ggplot object.
#
# (c) Lasse Ruokolainen -- October 2017
#         last modified -- January 2018
##########################################################

roc.curve = function(model, test.data, response.variable, 
					 pred1 = NULL,plot = T,...){
  .require.package('ROCR')
  .require.package('ggplot2')	
  
  # Make sure response variable is binary (0/1):
  y = as.numeric(factor(test.data[,response.variable])) - 1

  # Output model prediction as probabilities:
  if(is.null(pred1)){
	  pred1 = predict(model,test.data,type = 'prob')
	  # Create prediction object:
	  pred2 = prediction(pred1[,2],y)
  } else {
	  # Create prediction object:
	  pred2 = prediction(pred1,y)  	
  }
    
  # Calculate receiver-operator metrics:
  perf = performance(pred2,'tpr','fpr')
  x.p = unlist(perf@x.values)
  y.p = unlist(perf@y.values)  
  
  # Output cutoff:
  perf.c = performance(pred2,'fpr',x.measure='cutoff')
  x.c = unlist(perf.c@y.values)
  y.c = unlist(perf.c@x.values); y.c[1] = 1
  
  df = data.frame(x=rep(x.c,2),y=c(y.c,y.p),type=rep(c('Cut-off','ROC'),each=length(x.c)))
  
  # Calculate Area Under the roc-Curve (AUC):  
  auc = unlist(performance(pred2,'auc')@y.values)  
  cat(paste('AUC:',as.character(signif(auc,3))))
  
  # Plot ROC-curve  
  if(plot==TRUE){
	p = ggplot(df,aes(x,y,col=type)) + 
    	geom_line(aes(x,y),linetype=2,inherit.aes=F,data=data.frame(x=c(0,1),y=c(0,1)))+
	  	geom_line(size=1.0) + 
    	xlab('False positive rate')+ylab('True positive rate') + 
	    theme_light() + theme(axis.text = element_text(size=10),axis.title = element_text(size=12)) + 
    	scale_color_manual(values=c('blue2','red2'),name='')  	
  	print(p)	
  }  
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
