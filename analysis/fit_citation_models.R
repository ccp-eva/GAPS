#!/usr/bin/env Rscript
library(tidyverse)
library(brms)

d <- read_csv("../data/citations_and_descriptors.csv")
d$multi_country <- if_else(d$N_countries == 1, -0.5, 0.5)

# New prior for negative binomials
shared_prior =prior(normal(5, 2.5), class="Intercept") +
	      prior(normal(0, 2.5), class="b") +
	      prior(exponential(5), class="sd") +
	      prior(normal(0.25, 1.5), class="Intercept", dpar="shape") +
	      prior(normal(0.25, 0.75), class="b", dpar="shape")

# First citation models with offset
null_formula <- "citations ~ offset(log(exposure)) + 1 + multi_country +
                                                    (1 + multi_country | journal)"

m <- brm(bf(null_formula, shape~multi_country), data=d, family="negbinomial", cores=4, prior=shared_prior, control=list(adapt_delta=0.9))
saveRDS(m, "m_citations_null.rds")

singpred_formula <- "citations ~ offset(log(exposure)) + 1 + multi_country * mo(predictor, id='foo') +
	                                                (1 + multi_country * mo(predictor, id='foo') | journal)"
predictors <- c("education", "industry", "gdp", "democracy", "household_size", "urbanism", "gini", "religiosity", "ethfrac")

for(predictor in predictors) {
		form_str <- str_replace_all(singpred_formula, "predictor", predictor)
		formul <- as.formula(form_str)
		int_term <- paste("mo", predictor, "idEQfoo:multi_country", sep="")
		m <- brm(bf(formul, shape~multi_country), data=d, family="negbinomial", cores=4, prior = shared_prior + prior_string("normal(0, 1.25)", class="b", coef=int_term))
		saveRDS(m, paste("m_citations_singpred_", predictor, "_shape.rds", sep=""))
}
