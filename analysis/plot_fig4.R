#!/usr/bin/env Rscript
library(brms)
library(matrixStats)
library(patchwork)
library(tidyverse)

set.seed(1234)
dir.create("../plots/", showWarnings=FALSE)

pretty_varname <- function(varname) {
	if(varname == "education") {
		return("Educated")
	} else if(varname == "industry") {
		return("Industrialized")
	} else if(varname == "gdp") {
		return("Rich")
	} else if(varname == "democracy") {
		return("Democratic")
	} else if(varname == "gini") {
		return("Economically unequal")
	} else if(varname == "household_size") {
		return("Household size")
	} else if(varname == "urbanism") {
		return("Urbanized")
	} else if(varname == "ethfrac") {
		return("Ethnically diverse")
	} else if(varname == "religiosity") {
		return("Religious")
	} else {
		return(str_to_title(varname))
	}
}

ranked_predictors <- c("Urbanized", "Household size", "Industrialized", "Educated")

d <- read_csv("../data/articles_per_child.csv") %>%
	rename(WEIRD=weird) %>%
	mutate(Country = if_else(WEIRD, "WEIRD", "Other"),
	       `Article count` = factor(if_else(no_articles, "Assumed", "Actual"), c("Assumed", "Actual"))
	      )

predictors <- c("education", "industry", "urbanism", "household_size")
ccp_colours = rev(c("#006c66", "#c6d325", "#00b1ea", "#29485d"))

plot_t <- tibble(predictor=character(), decile=integer(),
		 articles=numeric())
for(i in 1:length(predictors)) {
	predictor <- predictors[i]
	m <- read_rds(paste("m_singpred_", predictor, ".rds", sep=""))
	nd <- tibble(foo=1:10)
	colnames(nd) <- predictor
	d_pred <- tibble(predictor=pretty_varname(predictor),
			 decile=1:10,
	                 articles=1e6*colMeans(exp(posterior_epred(m, newdata=nd, re_formula=NA))))
	plot_t <- add_row(plot_t, d_pred)
}
plot_t$predictor <- factor(plot_t$predictor, ranked_predictors, ordered=TRUE)
fig4a <- ggplot(plot_t) +
	geom_point(aes(x=decile, y=predictor, colour=predictor, size=articles)) +
	labs(size="Articles per\nmillion children") +
	xlab("Decile") +
	ylab("Country-level Cultural Metric") +
	scale_colour_manual(values=ccp_colours) +
	scale_x_continuous(breaks=1:10) +
	guides(colour="none") +
	scale_size_area(max_size = 15, breaks = 1:5) +
	theme_bw()

plot_t <- tibble(predictor=character(), decile=integer(),
		 `Group identifier`=numeric(),
		 `Cross-cultural`=numeric(),
		 `Culture word`=numeric(),
		 `Country information`=numeric())
qualifiers <- c("group_identifier", "crosscultural", "culture_word", "countries_info_given")
pretty_qualifiers <- c("Group identifier", "Cross-cultural", "Culture word", "Country information")
for(i in 1:length(predictors)) {
	predictor <- predictors[i]
	d_pred <- tibble(predictor=pretty_varname(predictor),
			 decile=1:10)
	for(j in 1:length(qualifiers)) {
		qualifier <- qualifiers[j]
		m <- read_rds(paste("m_", qualifier, "_singpred_", predictor, ".rds", sep=""))
		nd <- tibble(N_countries=1, foo=1:10)
		colnames(nd) <- c("N_countries", predictor)
		d_pred[[pretty_qualifiers[j]]] <- colMeans(posterior_epred(m, newdata=nd, re_formula=NA))

	}
	plot_t <- add_row(plot_t, d_pred)
}
plot_t$predictor <- factor(plot_t$predictor, rev(ranked_predictors), ordered=TRUE)
fig4b <-  plot_t %>%
	pivot_longer(cols= c("Group identifier", "Cross-cultural", "Culture word", "Country information"),
		     names_to="Qualifier",
		     values_to="proportion") %>%
	ggplot() +
	geom_line(aes(x=decile, y=proportion, colour=predictor), linewidth=1.5) +
	scale_colour_manual(values=rev(ccp_colours)) +
	scale_x_continuous(breaks=1:10) +
	labs(colour="Country-level\nCultural Metric") +
	xlab("Decile") +
	ylab("Proportion of papers") +
	facet_wrap(~Qualifier) +
	theme_bw()

plotlist <- list(fig4a, fig4b)
fig4 <- wrap_plots(plotlist, nrow=1, ncol=2)
ggsave("../plots/fig4.png", fig4, width=420, height=420/2.4, units="mm")
