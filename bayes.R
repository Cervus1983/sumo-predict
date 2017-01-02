library(rstan)
	options(mc.cores = parallel::detectCores())
	rstan_options(auto_write = TRUE)


# returns fitted result of a Stan model
trainBayes <- function(data) {
	# rikishi sorted by number of wins
	rikishi <- rbind(
		data %>% group_by(shikona1) %>% summarise(bouts = n(), wins = sum(win1)) %>% rename(shikona = shikona1),
		data %>% group_by(shikona2) %>% summarise(bouts = n(), wins = sum(win2)) %>% rename(shikona = shikona2)
	) %>%
		group_by(shikona) %>%
		summarise(
			bouts = sum(bouts),
			wins = sum(wins)
		) %>% arrange(-wins/bouts)

	# map rikishi names (shikona) to their position in the table above
	data <- data %>% mutate(
		rikishi1 = match(shikona1, rikishi$shikona),
		rikishi2 = match(shikona2, rikishi$shikona)
	)

	# train model
	model <- stan(
		model_code = "
			data {
				int n_rows;
				int n_rikishi;
				int rikishi1[n_rows];
				int rikishi2[n_rows];
				int win1[n_rows];
			}
			
			parameters {
				vector[n_rikishi] skill;
			}
			
			model {
				skill ~ normal(0, 1);
				
				for(i in 1:n_rows) {
					win1[i] ~ bernoulli_logit(skill[rikishi1[i]] - skill[rikishi2[i]]);
				}
			}
		",
		data = list(
			n_rows = nrow(data),
			n_rikishi = nrow(rikishi),
			rikishi1 = data$rikishi1,
			rikishi2 = data$rikishi2,
			win1 = data$win1
		)
	)

	# extract rikishi skill
	output <- as.data.frame(extract(model)$skill)
	colnames(output) <- rikishi$shikona
	
	output
}
