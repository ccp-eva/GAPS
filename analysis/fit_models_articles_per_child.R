library(tidyverse)
library(brms)

d <- read_csv("../data/articles_per_child.csv")

fit_and_save_single_predictor_model <- function(d, predictor) {
  if(predictor %in% c("western", "weird")) {
	  form <- paste("log_articles_per_child ~ 0 +", predictor)
  } else {
    form <- paste("log_articles_per_child ~ mo(", predictor, ")", sep="")
  }
  m <- brm(form, data=d, cores=4, save_pars = save_pars(all = TRUE))
  m <- m %>% add_criterion("loo", moment_match=TRUE)
  filename <- paste("m_singpred_", predictor, ".rds", sep="")
  saveRDS(m, filename)
  return(m)
}

# Null model
m_null <- brm(log_articles_per_child ~ 1,
	  data=d, cores=4) %>%
          add_criterion("loo")
saveRDS(m_null, "m_null.rds")

# Nielsen WEIRD
m_nielsen <- fit_and_save_single_predictor_model(d, "weird")

# Separated WEIRD components
m_w <- fit_and_save_single_predictor_model(d, "western")
m_e <- fit_and_save_single_predictor_model(d, "education")
m_i <- fit_and_save_single_predictor_model(d, "industry")
m_r <- fit_and_save_single_predictor_model(d, "gdp")
m_d <- fit_and_save_single_predictor_model(d, "democracy")

# Alternative components
M_h <- fit_and_save_single_predictor_model(d, "household_size")
M_u <- fit_and_save_single_predictor_model(d, "urbanism")
M_g <- fit_and_save_single_predictor_model(d, "gini")
M_r <- fit_and_save_single_predictor_model(d, "religiosity")
M_f <- fit_and_save_single_predictor_model(d, "ethfrac")

# Combined WEIRD components
m_weird <- brm(log_articles_per_child ~ western +
	                          mo(education) +
				  mo(industry) +
				  mo(gdp) +
				  mo(democracy),
	  data=d, cores=4) %>%
          add_criterion("loo")
saveRDS(m_weird, "m_multipred_weird.rds")

# Combined alternative components
m_alt <- brm(log_articles_per_child ~ mo(household_size) +
            mo(urbanism) +
            mo(gini) +
            mo(religiosity) +
            mo(ethfrac),
	  data=d, cores=4) %>%
          add_criterion("loo")
saveRDS(m_alt, "m_multipred_alt.rds")

m_mega <- brm(log_articles_per_child ~ western + mo(education) + mo(industry) + mo(gdp) + mo(democracy) +
                                       mo(household_size) + mo(urbanism) + mo(gini) + mo(religiosity) + mo(ethfrac),
	  data=d, cores=4) %>%
          add_criterion("loo")
saveRDS(m_mega, "m_multipred_all.rds")
