# simple approach: historical win rate for given difference in rank
histWinRate <- function(train.data, test.data) {
	# historical win rate ~ difference in rank
	train.data.summary <- withOrdinals(train.data) %>%
		mutate(below = ordinal1 - ordinal2) %>%
		group_by(below) %>%
		summarise(total = n(), wins = sum(win1)) %>%
		mutate(winRate = wins / total) %>%
		arrange(below)

	# look up historical win rate for test data
	(
		left_join(
			withOrdinals(test.data) %>% mutate(below = ordinal1 - ordinal2),
			train.data.summary
		) %>% mutate(winRate = ifelse(is.na(winRate), 0.5, winRate)) # replace NA with 0.5 (rare, shouldn't matter)
	)$winRate
}
