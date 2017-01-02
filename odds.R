library(dplyr)
library(stringr)
library(XML)

options(stringsAsFactors = FALSE)


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
