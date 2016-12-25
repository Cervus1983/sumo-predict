library(ROCR)

# interface to http://sumodb.sumogames.de/Query_bout.aspx
source("sumodb.R")

# common functions used in models
source("functions.R")


# load models
if(TRUE) {
	# random guess: picks a number from a beta distribution
	source("models/randomGuess.R")
	
	# simple approach: historical win rate for given difference in rank
	source("models/histWinRate.R")
	
	# random forest
	source("models/randomForest.R")
}


# removes forfeits & play-offs
clean <- function(df) {
	df %>% filter(
		day <= 15,
		kimarite != "fusen"
	)
}


# train & test data sets
if(TRUE) {
	train.data <- clean(
		rbind(
			sumodbBoutQuery(basho = "2016.01", division = "m"),
			sumodbBoutQuery(basho = "2016.03", division = "m"),
			sumodbBoutQuery(basho = "2016.05", division = "m"),
			sumodbBoutQuery(basho = "2016.07", division = "m"),
			sumodbBoutQuery(basho = "2016.09", division = "m")
		)
	)

	test.data <- clean(sumodbBoutQuery(basho = "2016.11", division = "m"))
}


# http://mlwiki.org/index.php/ROC_Analysis
evaluate <- function(model) {
	graphics.off()
	
	pred <- prediction(
		predictions = do.call(model, list(train.data, test.data)),
		labels = test.data$win1
	)

	# ROC curve	(AUC in the title)
	windows()

	plot(
		performance(pred, measure = "tpr", x.measure = "fpr"),
		colorize = TRUE,
		main = sprintf("AUC = %.1f%%", unlist(performance(pred, "auc")@y.values) * 100)
	)

	lines(x = c(0, 1), y = c(0, 1))

	# cutoff plot
	windows()

	acc = performance(pred, "acc")

	ac.val = max(unlist(acc@y.values))
	th = unlist(acc@x.values)[unlist(acc@y.values) == ac.val]
	
	plot(
		acc,
		main = paste("Cutoff:", paste(sprintf("%.1f%%", th[order(th)] * 100), collapse = ", ")),
		xlab = ""
	)

	abline(v = th, col = "grey", lty = 2)
}


# evaluate models one by one
evaluate("randomGuess")
evaluate("histWinRate") # AUC = 61.6%
evaluate("randomForest") # 62.3%
