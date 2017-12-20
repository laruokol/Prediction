# cv.roc.curve
#
# This function produces an average ROC-curve for 
# cross-validation (CV). The function outputs the average 
# ROC-curve across k CV-folds as well as the min and max,
# and the summary for AUC across the folds. 
#
# INPUT:   models     A list of models, one for each CV-fold.
#          test.data  A data.frame used for model testing.
#          respvar    The name of the response variable.
#          plot       Should the result be plotted?
#                     (default = TRUE)
#
# (c) Lasse Ruokolainen -- December 2017
##############################################################

cv.roc.curve = function(models,test.data,respvar,plot = TRUE){

	args = as.character(match.call())[-1]
	if(length(args)<3) stop('Too few input arguments')
	if(!is.list(models)) stop('A list of models is required')
	if(!(args%in%c('models'))) stop('Missing model list')
	if(!(args%in%c('test.data'))) stop('Missing test data')	
	if(!(args%in%c('respvar'))) stop('Missing response variable name')	

	# Load required packages
	.require.package('foreach')
	.require.package('parallel')
	.require.package('ROCR')
	.require.package('ggplot2')	

	# Generate prediction probabilities for each model:
	pred1 = foreach(ii = 1:length(models)) %dopar% {
		pred1 = predict(models[[ii]],d.test,type = 'prob')[,2]
	}

	# Make sure response variable is numeric (binary):
	y = as.numeric(factor(d.test[,respvar])) - 1

	# Generate prediction objects:
	pred2 = mclapply(pred1,function(x) prediction(x,y))
	# Calculate performance metrics:	
	perf = mclapply(pred2,function(x) performance(x,'tpr','fpr'))
	auc = do.call(c,mclapply(pred2,function(x) unlist(performance(x,'auc')@y.values)))

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
		ggplot(df,aes(x,mean))+
			geom_abline(intercept=0,slope=1,col='gray50',linetype=2)+
			geom_ribbon(aes(x=x, ymax=max, ymin=min), fill="red3", alpha=.25)+
			geom_line(col='red3',size=1)+
			xlab('True negative rate') + ylab('True positive rate')+
			theme_classic()
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
