library(dplyr)
library(httr)
library(stringr)


options(stringsAsFactors = FALSE)


# formats rank as order-able string
rankOrder <- function(rank){
	s <- str_match(rank, "^(\\D+)([0-9]+)(\\D+)$")

	sprintf(
		"%02d.%03d.%s",
		match(s[, 2], c("Y", "O", "S", "K", "M", "J", "Ms", "Sd", "Jd", "Jk")), # title/division (yokozuna, ozeki, etc.)
		as.integer(s[, 3]), # ordinal (can go above 100 in some divisions)
		match(s[, 4], c("e", "w")) # east/west
	)
}


# reads CSV files from Cervus1983/sumodb repository (branch "all-divisions")
getCSV <- function(smth) do.call(
	rbind,
	lapply(
		smth,
		function(x) tryCatch(
			read.csv(
				paste0("https://raw.githubusercontent.com/Cervus1983/sumodb/all-divisions/CSV/", x, ".csv")
			),
			error = function(e) {},
			warning = function(w) {}
		)
	)
)


# fetches banzuke (rankings) for one or more basho (tournament)
getBanzuke <- function(basho) getCSV(paste(basho, "banzuke", sep = "."))


# fetches bout results for one or more basho (tournament)
getResults <- function(basho) getCSV(paste(basho, "results", sep = "."))
