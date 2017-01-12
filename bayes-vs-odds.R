# trainBayes()
source("bayes.R")

# getBanzuke() & getResults()
source("sumo.R")


# tests model using historical betting odds
testModel <- function(basho) {
	# banzuke (rankings) for makuuchi (top division)
	banzuke <- getBanzuke(basho) %>% filter(rankOrder(rank) < "06")
	
	# historical betting odds
	odds <- read.csv(paste0("odds/", basho, ".odds.csv"))


	# most recent {n} basho prior to {basho}
	prevBasho <- function(basho, n) {
		all.basho <- getBashoList()
		tail(all.basho[all.basho < basho], n)
	}
	
	# three years' worth of historical results
	train.data <- getResults(
		prevBasho(basho, 18)
	) %>% filter(
		(shikona1 %in% banzuke$rikishi | shikona2 %in% banzuke$rikishi) & # at least one of the wrestlers (rikishi) took part in the bout
		kimarite != "fusen" # discard forfeits
	)

	# fit model
	skill <- trainBayes(train.data)

	# sanity check: who is the top dog?
	print(table(colnames(skill[, banzuke$rikishi])[apply(skill[, banzuke$rikishi], 1, which.max)]))


	# returns vector of probabilities for rikishi1 winning
	predict <- function(rikishi1, rikishi2) quantile(plogis(skill[[rikishi1]] - skill[[rikishi2]]), c(0.025, 0.5, 0.975))
	
	# calculate EV given odds & outcome probabilities (from the model)
	odds <- odds %>% mutate(EV1 = NA, EV2 = NA)
	
	for(i in 1:nrow(odds)) {
		pWin1 <- predict(odds$rikishi1[i], odds$rikishi2[i])[2]
		odds$EV1[i] <- pWin1 * odds$odds1[i]
		odds$EV2[i] <- (1 - pWin1) * odds$odds2[i]
	}
	

	# How much would I've won if I'd betted on bouts with positive EV (according to the model)?
	df <- merge(
		odds %>% filter(EV1 > 1 | EV2 > 1),
		getResults(basho) %>% select(rikishi1 = shikona1, rikishi2 = shikona2, win1, win2, kimarite)
	) %>%
		mutate(
			profit = ifelse(
				kimarite == "fusen",
				NA, # cancel bet in case of forfeit
				ifelse(EV1 > EV2, odds1 * win1 - 1, odds2 * win2 - 1)
			)
		) %>%
		summarise(
			basho = basho,
			bets.offered = nrow(odds),
			bets.placed = n(),
			bets.settled = sum(!is.na(profit)),
			profit = sum(profit, na.rm = TRUE),
			per.settled.bet = profit / bets.settled,
			per.offered.bet = profit / bets.offered
		)
	
	print.data.frame(df)
	
	df
}


# test model on all basho with odds available
do.call(
	rbind,
	lapply(
		c("2012.07", "2012.09", "2012.11", "2013.01", "2013.03", "2013.05"),
		testModel
	)
)

# basho 	bets.offered bets.placed bets.settled profit per.settled.bet per.offered.bet
# 2012.07          126          86           85  24.75      0.29117647      0.19642857
# 2012.09           61          37           37   3.06      0.08270270      0.05016393
# 2012.11           99          69           68   5.35      0.07867647      0.05404040
# 2013.01          139          97           97  -7.54     -0.07773196     -0.05424460
# 2013.03          140          96           96  15.81      0.16468750      0.11292857
# 2013.05          123          79           79   5.09      0.06443038      0.04138211

# ROI
sprintf("%.1f%%", (24.75 + 3.06 + 5.35 - 7.54 + 15.81 + 5.09) / (86 + 37 + 69 + 97 + 96 + 79) * 100)
# [1] "10.0%"
