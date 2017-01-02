library(dplyr)
library(ROCR)
library(stringr)
library(tidyr)
library(XML)

options(stringsAsFactors = FALSE)


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

	# Bayesian approach
	source("models/bayesian.R")
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
		do.call(
			rbind,
			lapply(
				c("2016.01", "2016.03", "2016.05", "2016.07", "2016.09"),
				function(x) read.csv(
					paste0("https://raw.githubusercontent.com/Cervus1983/sumodb/master/CSV/", x, ".results.csv")
				)
			)
		)
	)

	test.data <- clean(read.csv("https://raw.githubusercontent.com/Cervus1983/sumodb/master/CSV/2016.11.results.csv"))
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
evaluate("histWinRate") # AUC = 61.8%
evaluate("randomForest") # AUC ~ 58%-60%
evaluate("bayesian") # 68.2%
