# random forest
randomForest <- function(train.data, test.data) {
	# enriches data set
	addFeatures <- function(df) {
		withOrdinals(df) %>% mutate(
			below = ordinal1 - ordinal2,

			level1 = factor(substr(rank1, 1, 1), levels = c("J", "M", "K", "S", "O", "Y")),
			level2 = factor(substr(rank2, 1, 1), levels = c("J", "M", "K", "S", "O", "Y")),

			wins.before1 = as.integer(str_match(result1, "^([0-9]+)-([0-9]+)")[, 2]) - win1,
			wins.before2 = as.integer(str_match(result2, "^([0-9]+)-([0-9]+)")[, 2]) - win2,
			defeats.before1 = as.integer(str_match(result1, "^([0-9]+)-([0-9]+)")[, 3]) - win2,
			defeats.before2 = as.integer(str_match(result2, "^([0-9]+)-([0-9]+)")[, 3]) - win1,
			
			win.rate.before1 = ifelse(wins.before1 + defeats.before1 == 0, 0.5, wins.before1 / (wins.before1 + defeats.before1)),
			win.rate.before2 = ifelse(wins.before2 + defeats.before2 == 0, 0.5, wins.before2 / (wins.before2 + defeats.before2)),

			win1 = as.factor(win1),
			win2 = as.factor(win2)
		)
	}

	# train model on enriched train.data 
	fit <- randomForest::randomForest(
		formula = win1 ~ ordinal1 + ordinal2 + below + level1 + level2, # + win.rate.before1 + win.rate.before2,
		data = addFeatures(train.data),
		importance = TRUE
	)

	# predict outcomes for test.data
	predict(
		fit,
		addFeatures(test.data),
		type = "prob"
	)[, 2] # select probability of 1 (first rikishi wins)
}
