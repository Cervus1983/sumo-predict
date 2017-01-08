library(dplyr)
library(stringr)
library(XML)

options(stringsAsFactors = FALSE)


root <- "http://uk.wettportal.com/oddsarchive/Combat_Sports/World/Sumo_-"

page <- list(
	"2012.07" = "Nagoya", # first table - from 2012.09
	"2012.09" = "Aki",
	"2012.11" = "Kyushu",
	"2013.01" = "Hatsu",
	"2013.03" = "Haru",
	"2013.05" = "Natsu"
)




getOdds <- function(basho) {
	odds <- do.call(
		rbind,
		readHTMLTable(paste(root, page[[basho]], "Basho", sep = "_"))[-(1:2)]
	) %>%
		transmute(
			rikishi1 = str_match(V2, "(.+) - (.+)")[, 2],
			odds1 = as.numeric(str_match(V5, "^([0-9.]+)")[, 1]),
			rikishi2 = str_match(V2, "(.+) - (.+)")[, 3],
			odds2 = as.numeric(str_match(V6, "^([0-9.]+)")[, 1])
		)
	
	banzuke <- getBanzuke(basho)

	merge(
		merge(
			odds,
			banzuke %>% select(rikishi1 = rikishi, rank1 = rank),
			all.x = TRUE
		),
		banzuke %>% select(rikishi2 = rikishi, rank2 = rank),
		all.x = TRUE
	)




getOdds_2013_Natsu <- function() do.call(
	rbind,
	lapply(
		1:13,
		function(x) readHTMLTable(
			doc = "http://uk.wettportal.com/oddsarchive/Combat_Sports/World/Sumo_-_Natsu_Basho",
			which = 16 - x
		) %>%
			transmute(
				basho = "2013.05",
				day = x,
				rikishi1 = str_match(V2, "(.+) - (.+)")[, 2],
				rikishi2 = str_match(V2, "(.+) - (.+)")[, 3],
				odds1 = as.numeric(str_match(V5, "^([0-9.]+)")[, 1]),
				odds2 = as.numeric(str_match(V6, "^([0-9.]+)")[, 1])
			)
	)
) %>% mutate(
	rikishi1 = replace(rikishi1, rikishi1 == "Tochihozan", "Tochiozan"),
	rikishi2 = replace(rikishi2, rikishi2 == "Tochihozan", "Tochiozan"),
	rikishi2 = replace(rikishi2, rikishi2 == "Harumahuji", "Harumafuji")
)
