library(randomForest)


addFeatures <- function(df) {
	df %>% mutate(
		win1 = as.factor(win1),
		win2 = as.factor(win2),
		below = ordinal1 - ordinal2,
		level1 = as.ordered(substr(rank1, 1, 1)),
		level2 = as.ordered(substr(rank2, 1, 1))
	)
}

model <- randomForest(
	formula = win1 ~ ordinal1 + ordinal2 + below + level1 + level2,
	data = addFeatures(train.data),
	importance = TRUE
)

model


prob = predict(model, addFeatures(test.data), type = "prob")
