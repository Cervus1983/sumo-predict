# Bayesian approach
library(rstan)
	options(mc.cores = parallel::detectCores())
	rstan_options(auto_write = TRUE)

bayesian <- function(train.data, test.data) {
	# rikishi sorted by number of wins
	rikishi <- rbind(
		train.data %>% group_by(shikona1) %>% summarise(bouts = n(), wins = sum(win1)) %>% rename(shikona = shikona1),
		train.data %>% group_by(shikona2) %>% summarise(bouts = n(), wins = sum(win2)) %>% rename(shikona = shikona2)
	) %>%
		group_by(shikona) %>%
		summarise(
			bouts = sum(bouts),
			wins = sum(wins)
		) %>% arrange(-wins/bouts)

	# map rikishi names (shikona) to their position in the table above
	train.data <- train.data %>% mutate(
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
			n_rows = nrow(train.data),
			n_rikishi = nrow(rikishi),
			rikishi1 = train.data$rikishi1,
			rikishi2 = train.data$rikishi2,
			win1 = train.data$win1
		)
	)

	# extract rikishi skill
	skill <- as.data.frame(rstan::extract(model)$skill)
	colnames(skill) <- rikishi$shikona
	
	# predict outcomes for test.data
	mapply(
		function(shikona1, shikona2) {
			skill1 <- skill[[shikona1]]
			skill2 <- skill[[shikona2]]
			mean(plogis(skill1 - skill2))
		},
		test.data$shikona1,
		test.data$shikona2,
		USE.NAMES = FALSE
	)
}
