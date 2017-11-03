# confusion.matrix
# 
# This function calculates the confusion matrix for a binary
# prediction, as well as the class recall rates (aka. sensitivity
# and specificity) and precisions.
#
# INPUT:   prediction     model prediction of class labels
#          true.values    true labels
#
# (c) Lasse Ruokolainen -- November 2017
#################################################################

confusion.matrix = function(prediction,true.values){
	conf = table(true.values, prediction,dnn=c('Observed','Predicted'))
	recall = diag(conf)/rowSums(conf)
	precision = diag(conf)/colSums(conf)
	return(list(conf,recall,precision))
}