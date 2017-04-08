library(dplyr)
library(plotly)
library(ROCR)
library(stringr)
library(tidyr)


options(stringsAsFactors = FALSE)


# odds ----
dirs <- list.dirs("C:/Users/Spel/Google Drive/Sumo")

odds <- do.call(
	rbind,
	lapply(
		dirs[grepl("odds", dirs)],
		function(dir) {
			files <- list.files(dir)
			do.call(
				rbind,			
				lapply(
					files,
					function(file) read.csv(paste(dir, file, sep = "/")) %>% 
						mutate(
							basho = str_match(dir, "/([0-9]{4}\\.[0-9]{2})/")[1, 2],
							day = str_match(file, "^(.*)\\.csv$")[1, 2]
						)
				)
			)
		}
	)
)


# results ----
results <- do.call(
	rbind,
	lapply(
		unique(odds$basho),
		function(x) read.csv(paste0("C:/Users/Spel/Desktop/GitHub/sumodb/CSV/", x, ".results.csv"))
	)
)


# odds + results ----
df <- merge(
	rbind(
		odds %>% mutate(
			shikona1 = rikishi1, shikona2 = rikishi2,
			p.win1 = odds2 / (odds1 + odds2)
		),
		odds %>% mutate(
			shikona1 = rikishi2, shikona2 = rikishi1,
			p.win1 = odds1 / (odds1 + odds2)
		)
	),
	results %>% filter(basho == "2017.03", kimarite != "fusen")
)


# sanity check ----
df %>% 
	plot_ly() %>% 
	add_boxplot(
		x = ~win1,
		y = ~p.win1
	)


# ROC curve with AUC in the title (http://mlwiki.org/index.php/ROC_Analysis) ----
graphics.off()
pred <- prediction(
	predictions = df$p.win1,
	labels = df$win1
)

windows()

plot(
	performance(pred, measure = "tpr", x.measure = "fpr"),
	colorize = TRUE,
	main = sprintf("AUC = %.1f%%", unlist(performance(pred, "auc")@y.values) * 100)
)

lines(x = c(0, 1), y = c(0, 1))
