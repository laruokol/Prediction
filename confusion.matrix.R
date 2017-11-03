# confusion.matrix
# 
# This function calculates the confusion matrix for a binary
# prediction, as well as the class recall rates (aka. sensitivity
# and specificity) and precisions. In addition, the function re-
# turns classification accuracy, the kappa statistic (accuracy 
# scaled by obtaining correct label just by chance), and the F-
# measure (harmonic mean of precision and recall).
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

	accuracy = sum(diag(conf)) / sum(conf)
	
	tab = prop.table(conf)
	pr_a = sum(diag(tab))
	pr_e = sum(rowSums(tab) * colSums(tab))
	kappa = (pr_a - pr_e) / (1 - pr_e)
	
	F.measure = 2*precision*recall / (precision + recall)
	
	return(list(conf, recall, precision, accuracy, kappa, F.measure))
}
