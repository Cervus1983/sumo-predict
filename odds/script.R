library(dplyr)
library(stringr)
library(XML)

options(stringsAsFactors = FALSE)


# http://uk.wettportal.com/oddsarchive/Combat_Sports/World/
root <- "http://uk.wettportal.com/oddsarchive/Combat_Sports/World/Sumo_-"

page <- list(
	"2012.07" = "Nagoya", # first table is actually from 2012.09 basho (tournament)
	"2012.09" = "Aki",
	"2012.11" = "Kyushu",
	"2013.01" = "Hatsu",
	"2013.03" = "Haru",
	"2013.05" = "Natsu"
)


# corrects typos in shikona (wrestler's name)
correct <- Vectorize(
	function(shikona) switch(
		shikona,
		"Aokiyama" = "Aoiyama",
		"Harumahuji" = "Harumafuji",
		"Shohouzan" = "Shohozan",
		"Tochihozan" = "Tochiozan",
		"Tomasho" = "Homasho",
		"Wakakouyu" = "Wakakoyu",
		shikona
	)
)


# returns odds with shikona not found in banzuke (rankings)
QA <- function(basho) anti_join(
	# odds
	do.call(
		rbind,
		readHTMLTable(paste(root, page[[basho]], "Basho", sep = "_"))[-(1:2)]
	) %>%
		transmute(
			rikishi1 = str_match(V2, "(.+) - (.+)")[, 2],
			odds1 = as.numeric(str_match(V5, "^([0-9.]+)")[, 1]),
			rikishi2 = str_match(V2, "(.+) - (.+)")[, 3],
			odds2 = as.numeric(str_match(V6, "^([0-9.]+)")[, 1])
		) %>%
		mutate(
			rikishi1 = correct(rikishi1),
			rikishi2 = correct(rikishi2)
		),

	# odds filtered using banzuke
	inner_join(
		inner_join(
			do.call(
				rbind,
				readHTMLTable(paste(root, page[[basho]], "Basho", sep = "_"))[-(1:2)]
			) %>%
				transmute(
					rikishi1 = str_match(V2, "(.+) - (.+)")[, 2],
					odds1 = as.numeric(str_match(V5, "^([0-9.]+)")[, 1]),
					rikishi2 = str_match(V2, "(.+) - (.+)")[, 3],
					odds2 = as.numeric(str_match(V6, "^([0-9.]+)")[, 1])
				) %>%
				mutate(
					rikishi1 = correct(rikishi1),
					rikishi2 = correct(rikishi2)
				),
			getBanzuke(basho) %>% filter(rankOrder(rank) < "06") %>% select(rikishi1 = rikishi)
		),
		getBanzuke(basho) %>% filter(rankOrder(rank) < "06") %>% select(rikishi2 = rikishi)
	)
)


# QA all pages (must return an empty data frame)
do.call(
	rbind,
	lapply(
		names(page),
		QA
	)
)


# create CSV file from each page
sapply(
	names(page),
	function(basho) write.csv(
			do.call(
				rbind,
				readHTMLTable(paste(root, page[[basho]], "Basho", sep = "_"))[-(1:2)]
			) %>%
				transmute(
					rikishi1 = str_match(V2, "(.+) - (.+)")[, 2],
					odds1 = as.numeric(str_match(V5, "^([0-9.]+)")[, 1]),
					rikishi2 = str_match(V2, "(.+) - (.+)")[, 3],
					odds2 = as.numeric(str_match(V6, "^([0-9.]+)")[, 1])
				) %>%
				mutate(
					rikishi1 = correct(rikishi1),
					rikishi2 = correct(rikishi2)
				),
		file = paste0(basho, ".odds.csv"),
		quote = FALSE,
		row.names = FALSE
	)
)


# manually move first 10 records from 2012.07 to 2012.09
