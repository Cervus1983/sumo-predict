# trainBayes()
source("bayes.R")


# getOdds_2013_Natsu()
source("odds.R")


# getBanzuke() & getResults()
source("sumo.R")


# basho (tournament) under consideration: 2013.05 (Natsu)
basho <- "2013.05"


# banzuke (rankings) for 2013.05 (Natsu)
banzuke <- getBanzuke(basho) %>% filter(rankOrder(rank) < "06")


# odds for some of the bouts
odds <- getOdds_2013_Natsu()


# most recent {n} basho prior to {basho}
prevBasho <- function(basho, n) {
	all.basho <- getBasho()
	tail(all.basho[all.basho < basho], n)
}


# three years' worth of historical results
train.data <- getResults(
	prevBasho(basho, 18)
) %>% filter(
	(shikona1 %in% banzuke$rikishi | shikona2 %in% banzuke$rikishi) & # at least one of the wrestlers (rikishi) took part in the bout
	kimarite != "fusen" # discard forfeits
)


# historical head-to-head: bouts, wins (for rikishi1)
vs <- function(rikishi1, rikishi2) unlist(
	rbind(
		train.data %>%
			filter(shikona1 == rikishi1 & shikona2 == rikishi2) %>%
			summarise(bouts = n(), wins = sum(win1)),
		train.data %>%
			filter(shikona1 == rikishi2 & shikona2 == rikishi1) %>%
			summarise(bouts = n(), wins = sum(win2))
	) %>% summarise_each(funs(sum))
)


# fitted result of a Stan model
skill <- trainBayes(train.data)


# Who's the top dog (out of 2013.05 banzuke)?
table(colnames(skill[, banzuke$rikishi])[apply(skill[, banzuke$rikishi], 1, which.max)])


# returns vector of probabilities for rikishi1 winning
predict <- function(rikishi1, rikishi2) quantile(plogis(skill[[rikishi1]] - skill[[rikishi2]]), c(0.025, 0.5, 0.975))


# calculate EV given odds & outcome probabilities (from Stan model)
odds <- odds %>% mutate(EV1 = NA, EV2 = NA)

for(i in 1:nrow(odds)) {
	pWin1 <- predict(odds$rikishi1[i], odds$rikishi2[i])[2]
	odds$EV1[i] <- pWin1 * odds$odds1[i]
	odds$EV2[i] <- (1 - pWin1) * odds$odds2[i]
}

