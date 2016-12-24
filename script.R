library(ROCR)

source("sumodb.R")


# converts "Rank" string to <level>.<rank>.<group> (example: rank2str("Y1e") -> "1.01.1")
rank2str <- function(s) {
	sprintf(
		"%s.%02d.%s",
		match(substr(s, 1, 1), c("Y", "O", "S", "K", "M", "J")),
		as.integer(str_match(s, "^\\D([0-9]+)\\D")[, 2]),
		match(substr(s, nchar(s), nchar(s)), c("e", "w"))
	)
}


# maps "rank1" and "rank2" to "ordinal1" and "ordinal2" (NB: shuffles rows)
withOrdinals <- function(df) {
	map <- unique(df %>% mutate(s = rank2str(rank1)) %>% select(basho, rank = rank1, s)) %>%
		arrange(basho, s) %>%
		group_by(basho) %>%
		mutate(ordinal = row_number()) %>%
		select(-s)

	merge(
		merge(
			df %>% mutate(rank = rank1),
			map %>% rename(ordinal1 = ordinal)
		) %>% select(-rank) %>% mutate(rank = rank2),
		map %>% rename(ordinal2 = ordinal)
	) %>% select(-rank)
}


# prepares data set
prepare <- function(df) withOrdinals(df %>% filter(day <= 15, kimarite != "fusen"))


# historical data (win rate ~ difference in rank) for "simple" approach
train.data <- prepare(
	rbind(
		sumodbBoutQuery(basho = "2016.01", division = "m"),
		sumodbBoutQuery(basho = "2016.03", division = "m"),
		sumodbBoutQuery(basho = "2016.05", division = "m"),
		sumodbBoutQuery(basho = "2016.07", division = "m"),
		sumodbBoutQuery(basho = "2016.09", division = "m")
	)
)

train.data.summary <- train.data %>%
	mutate(below = ordinal1 - ordinal2) %>%
	group_by(below) %>%
	summarise(total = n(), wins = sum(win1)) %>%
	mutate(winRate = wins / total) %>%
	arrange(below)


# test data
test.data <- prepare(sumodbBoutQuery(basho = "2016.11", division = "m"))


# different approaches to predicting a bout's outcome
pWin1 <- list(
	# baseline: random pick from a beta distribution
	random = function(basho, day, shikona1, ordinal1, shikona2, ordinal2) rbeta(1, 1, 1),

	# simplistic approach: historical win rate for given difference in rank
	#   61.2% with one year's worth of historical data
	#   60,2% with three years?!
	simple = function(basho, day, shikona1, ordinal1, shikona2, ordinal2) {
		x <- train.data.summary[which(train.data.summary$below == ordinal1 - ordinal2), ]$winRate
		ifelse(length(x) == 1, x, 0.5)
	}
)


# generates predictions using pWin1[[name]]
approach <- function(name) {
	if(name %in% names(pWin1)) {
		mapply(
			pWin1[[name]],
			test.data$basho,
			test.data$day,
			test.data$shikona1,
			test.data$ordinal1,
			test.data$shikona2,
			test.data$ordinal2,
			USE.NAMES = FALSE
		)
	}
}
		

# http://mlwiki.org/index.php/ROC_Analysis
analyse <- function(name) {
	graphics.off()
	
	pred <- prediction(
		predictions = prob[, 2], #approach(name),
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
