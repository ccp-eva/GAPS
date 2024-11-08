#!/usr/bin/env Rscript
library(tidyverse)
library(brms)

d <- read_csv("../data/citations_and_descriptors.csv")
d$multi_country <- d$N_countries > 1

# New prior for negative binomials
shared_prior =prior(normal(5, 2.5), class="Intercept") +
	      prior(normal(0, 2.5), class="b") +
	      prior(exponential(5), class="sd")

# First citation models with offset
null_formula <- "citations ~ 1 + offset(log(exposure)) + multi_country +
	                   (1 + multi_country | journal)"

any_nonweird_formula <- "citations ~ 1 + offset(log(exposure)) + multi_country*any_nonweird +
	                   (1 + multi_country*any_nonweird | journal)"


m_null <- brm(bf(as.formula(null_formula), shape~multi_country), data=d, family="negbinomial", cores=4, prior=shared_prior, control=list(adapt_delta=0.9))
saveRDS(m, "m_citations_null_shape.rds")

singpred_formula <- "citations ~ 1 + offset(log(exposure)) +
			multi_country + mo(predictor) +
	           (1 + multi_country + mo(predictor) | journal)"
predictors <- c("education", "industry", "gdp", "democracy", "household_size", "urbanism", "gini", "religiosity", "ethfrac")

for(predictor in predictors) {
		form_str <- str_replace_all(singpred_formula, "predictor", predictor)
		formul <- as.formula(form_str)
		m <- brm(formul, data=d, family="negbinomial", cores=4, prior = shared_prior)
		saveRDS(m, paste("m_citations_singpred_", predictor, "_quick.rds", sep=""))
}
