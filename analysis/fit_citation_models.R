#!/usr/bin/env Rscript
library(tidyverse)
library(brms)

d <- read_csv("../data/citations_and_descriptors.csv")
d$multi_country <- d$N_countries > 1

# New prior for negative binomials
shared_prior =prior(normal(5, 2.5), class="Intercept") +
	      prior(normal(2.5, 1.25), class="b") +
	      prior(exponential(5), class="sd")

# First citation models with offset
null_formula <- "citations ~ 1 + offset(log(exposure)) + mo(N_countries) +
	                   (1 + mo(N_countries) | journal)"

any_nonweird_formula <- "citations ~ 1 + offset(log(exposure)) + multi_country*any_nonweird +
	                   (1 + multi_country*any_nonweird | journal)"

m <- brm(null_formula, data=d, family="negbinomial", cores=4, prior=shared_prior, control=list(adapt_delta=0.9))
saveRDS(m, "m_citations_null.rds")
m <- brm(any_nonweird_formula, data=d, family="negbinomial", cores=4, prior=shared_prior, control=list(adapt_delta=0.9))
saveRDS(m, "m_citations_any_nonweird.rds")
