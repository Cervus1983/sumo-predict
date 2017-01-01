# trainBayes()
source("bayes.R")


# getBanzuke() & getResults
source("sumo.R")


# banzuke for upcoming basho
banzuke <- getBanzuke("2017.01") %>% filter(rankOrder(rank) < "06")
banzuke[banzuke$rikishi == "Takakeisho", ]$rikishi <- "Sato" # pre-2017 shikona: http://sumodb.sumogames.de/Rikishi.aspx?r=12191


# three years' worth of historical results
train.data <- getResults(
	apply(
		expand.grid(
			2014:2016,
			sprintf("%02d", seq(1, 11, 2))
		),
		1,
		paste, collapse = "."
	)
) %>% filter(
	shikona1 %in% banzuke$rikishi &
	shikona2 %in% banzuke$rikishi &
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
skill <- trainBayes(train.data)	%>%
	rename(Takakeisho = Sato) # revert to new name

banzuke[banzuke$rikishi == "Sato", ]$rikishi <- "Takakeisho"


# returns vector of probabilities for rikishi1 winning
predict <- function(rikishi1, rikishi2) plogis(skill[[rikishi1]] - skill[[rikishi2]])


# probability of {row} defeating {column}
myPrediction <- as.data.frame(
	matrix(
		mapply(
			function(rikishi1, rikishi2) ifelse(rikishi1 == rikishi2, NA, mean(predict(rikishi1, rikishi2))),
			expand.grid(banzuke$rikishi, banzuke$rikishi, stringsAsFactors = FALSE)[, 1],
			expand.grid(banzuke$rikishi, banzuke$rikishi, stringsAsFactors = FALSE)[, 2],
			USE.NAMES = FALSE
		),
		nrow = nrow(banzuke)
	),
	row.names = banzuke$rikishi
)

colnames(myPrediction) <- banzuke$rikishi
