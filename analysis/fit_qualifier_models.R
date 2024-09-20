library(tidyverse)
library(brms)

d <- read_csv("../data/citations_and_descriptors.csv")

null_formula <- "outcome ~ 1 + mo(N_countries) + (1 + mo(N_countries) | journal)"

singpred_formula <- "outcome ~ 1 + mo(N_countries) + mo(predictor) +
	                   (1 + mo(N_countries) + mo(predictor) | journal)"

weird_formula <- "outcome ~ 1 + any_nonweird + mo(N_countries) +
	                    mo(education) + mo(industry) + mo(gdp) + mo(democracy) +
	                   (1 + any_nonweird + mo(N_countries) +
			    mo(education) + mo(industry) + mo(gdp) + mo(democracy) | journal)"

hungri_formula <- "outcome ~ 1 + mo(N_countries) +
 	                     mo(household_size) + mo(urbanism) + mo(gini) + mo(religiosity) +
	                    (1 + any_nonweird + mo(N_countries) +
			     mo(household_size) + mo(urbanism) + mo(gini) + mo(religiosity) | journal)"

outcomes <- c("group_identifier", "crosscultural", "culture_word", "countries_info_given")
predictors <- c("education", "industry", "gdp", "democracy", "household_size", "urbanism", "gini", "religiosity", "ethfrac")
	
shared_prior =prior(normal(0, 1.5), class="Intercept") +
	      prior(normal(0, 0.75), class="b") +
	      prior(exponential(10), class="sd")

for(outcome in outcomes) {
	formul <- as.formula(str_replace(null_formula, "outcome", outcome))
	m <- brm(formul, data=d, family="bernoulli", cores=4, prior=shared_prior)
	saveRDS(m, paste("m", outcome, "null.rds", sep="_"))
}

for(outcome in outcomes) {

	formul <- as.formula(str_replace(weird_formula, "outcome", outcome))
	m <- brm(formul, data=d, family="bernoulli", cores=4, prior=shared_prior)
	saveRDS(m, paste("m", outcome, "weird.rds", sep="_"))
	formul <- as.formula(str_replace(hungri_formula, "outcome", outcome))
	m <- brm(formul, data=d, family="bernoulli", cores=4, prior=shared_prior)
	saveRDS(m, paste("m", outcome, "hungri.rds", sep="_"))

	for(predictor in predictors) {
		form_str <- str_replace(singpred_formula, "outcome", outcome)
		form_str <- str_replace_all(form_str, "predictor", predictor)
		formul <- as.formula(form_str)
		m <- brm(formul, data=d, family="bernoulli", cores=4, 
		prior = shared_prior)
		saveRDS(m, paste("m_", outcome, "_singpred_", predictor, ".rds", sep=""))
	}

}
