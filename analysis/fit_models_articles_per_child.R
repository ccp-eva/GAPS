library(tidyverse)
library(brms)
library(mice)

d <- read_csv("../data/articles_per_child.csv")

fit_and_save_single_predictor_model <- function(d, predictor) {
  if(predictor == "western") {
	  form <- "log_articles_per_child ~ 0 + western"
  } else {
    form <- paste("log_articles_per_child ~ mo(", predictor, ")", sep="")
  }
  m <- brm(form, data=d, cores=4, save_pars = save_pars(all = TRUE))
  m <- m %>% add_criterion("loo", moment_match=TRUE)
  filename <- paste("m_singpred_", predictor, ".rds", sep="")
  saveRDS(m, filename)
  return(m)
}

# Nielsen WEIRD
m_null <- brm(log_articles_per_child ~ 1,
	  data=d, cores=4) %>%
          add_criterion("loo")

m_nielsen <- brm(log_articles_per_child ~ weird,
	  data=d, cores=4) %>%
          add_criterion("loo")
saveRDS(m_nielsen, "m_nielsen.rds")

loo_compare(m_null, m_nielsen)
model_weights(m_null, m_nielsen)

# Cultural distance from US
d_cfst <- d %>%
	filter(!is.na(distUS))
m_cfst <- brm(log_articles_per_child ~ mo(distUS),
	  data=d_cfst, cores=4) %>%
          add_criterion("loo")
saveRDS(m_cfst, "m_cfst.rds")

m_nielsen_sub <- brm(log_articles_per_child ~ weird,
	  data=d_cfst, cores=4) %>%
          add_criterion("loo")
loo_compare(m_nielsen_sub, m_cfst)
model_weights(m_nielsen_sub, m_cfst)

# WEIRD shoot out
d_weird <- d %>%
	filter(!is.na(western),
	       !is.na(education),
	       !is.na(industry),
	       !is.na(gdp),
	       !is.na(democracy))

m_w <- fit_and_save_single_predictor_model(d_weird, "western")
m_e <- fit_and_save_single_predictor_model(d_weird, "education")
m_i <- fit_and_save_single_predictor_model(d_weird, "industry")
m_r <- fit_and_save_single_predictor_model(d_weird, "gdp")
m_d <- fit_and_save_single_predictor_model(d_weird, "democracy")

loo_compare(m_w, m_e, m_i, m_r, m_d)
model_weights(m_w, m_e, m_i, m_r, m_d)

# Theory shoot out
d_theory <- d %>%
	filter(!is.na(individualism),
	       !is.na(urbanism),
	       !is.na(household_size),
	       !is.na(religiosity),
	       !is.na(ethfrac),
	       !is.na(gini))

M_h <- fit_and_save_single_predictor_model(d_theory, "household_size")
M_u <- fit_and_save_single_predictor_model(d_theory, "urbanism")
M_g <- fit_and_save_single_predictor_model(d_theory, "gini")
M_r <- fit_and_save_single_predictor_model(d_theory, "religiosity")
M_i <- fit_and_save_single_predictor_model(d_theory, "individualism")

loo_compare(M_h, M_u, M_g, M_r, M_i)
model_weights(M_h, M_u, M_g, M_r, M_i)

d_weird_imp <- d %>%
	select(log_articles_per_child,
	       western,
	       education,
	       industry,
	       gdp,
	       democracy) %>%
	mice(m = 5, print = FALSE)

m_weird <- brm_multiple(log_articles_per_child ~ western +
	                          mo(education) +
				  mo(industry) +
				  mo(gdp) +
				  mo(democracy),
	  data=d_weird_imp, cores=4) %>%
          add_criterion("loo")
saveRDS(m_weird, "m_multipred_weird.rds")

d_theory_imp <- d %>%
	select(log_articles_per_child,
	       household_size,
	       urbanism,
	       gini,
	       religiosity,
	       ethfrac,
	       individualism) %>%
	mice(m = 5, print = FALSE)

m_theory <- brm_multiple(log_articles_per_child ~ mo(individualism) +
            mo(urbanism) +
            mo(household_size) +
            mo(religiosity) +
            mo(ethfrac) +
            mo(gini),
	  data=d_theory_imp, cores=4) %>%
          add_criterion("loo")
saveRDS(m_theory, "m_multipred_weird.rds")

loo_compare(m_weird, m_theory)
model_weights(m_weird, m_theory)
