# converts "rank" string to <level>.<rank>.<group> (example: rankToStr("Y1e") -> "1.01.1")
rankToStr <- function(s) {
	sprintf(
		"%s.%02d.%s",
		match(substr(s, 1, 1), c("Y", "O", "S", "K", "M", "J")),
		as.integer(str_match(s, "^\\D([0-9]+)\\D")[, 2]),
		match(substr(s, nchar(s), nchar(s)), c("e", "w"))
	)
}


# adds columns "ordinal1" & "ordinal2" (deduced from "rank1" & "rank2")
withOrdinals <- function(df) {
	map <- unique(df %>% mutate(s = rankToStr(rank1)) %>% select(basho, rank = rank1, s)) %>%
		arrange(basho, s) %>%
		group_by(basho) %>%
		mutate(ordinal = row_number()) %>%
		select(-s)
	
	inner_join(
		inner_join(
			df %>% mutate(rank = rank1),
			map %>% rename(ordinal1 = ordinal)
		) %>% select(-rank) %>% mutate(rank = rank2),
		map %>% rename(ordinal2 = ordinal)
	) %>% select(-rank)
}
