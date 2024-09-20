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
		return("Democractic")
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

d <- read_csv("../data/articles_per_child.csv") %>%
	rename(WEIRD=weird) %>%
	mutate(Country = if_else(WEIRD, "WEIRD", "Other"),
	       `Article count` = factor(if_else(no_articles, "Assumed", "Actual"), c("Assumed", "Actual"))
	      )
fair_target <- log(sum(d$articles) / sum(d$children))
predictors <- c("education", "industry", "gdp", "democracy",
	"household_size", "urbanism", "gini", "religiosity", "ethfrac")

# Initialise model comparison table and add null model
model_comparison_table <- tibble(predictor = character(), looic=numeric(), looic_se=numeric())
nullmodel_filename <- paste("m_null.rds", sep="")
m <- readRDS(nullmodel_filename)
l <- loo(m, moment_match=FALSE)$estimates["looic",]
	model_comparison_table <- add_row(model_comparison_table, predictor = "Null model", looic=l[1], looic_se=l[2])

# Initialise plot list
plotlist <- list()
plot_index <- 1

# Iterate through predictors,
# updating model comparison table each time and
# adding plots to plotlist
for(j in 1:length(predictors)) {
	predictor <- predictors[j]
	model_filename <- paste("m_singpred_", predictor, ".rds", sep="")
	if(file.exists(model_filename)) {
		print(paste("found", model_filename))
		m <- readRDS(model_filename)

		# Update model comparison table
		l <- loo(m, moment_match=FALSE)$estimates["looic",]
		model_comparison_table <- add_row(model_comparison_table, predictor = pretty_varname(predictor), looic=l[1], looic_se=l[2])

		# Build plot table
		plot_table <- tibble(value=numeric(), multi_country=logical(), mean_outcome=numeric(), low_outcome=numeric(), high_outcome=numeric())

		# Create plot
		nd <- tibble(x=1:10)
		names(nd) <- predictor
		preds <- posterior_predict(m, newdata=nd)
		nd$pred_mean <- colMeans(preds)
		nd$pred_lower <- colQuantiles(preds, probs=0.025)
		nd$pred_upper <- colQuantiles(preds, probs=0.975)
		predictor <- ensym(predictor)
		p <- ggplot(nd) +
			geom_hline(yintercept=fair_target, linetype="longdash", alpha=0.5) +
			geom_line(aes(x=!!predictor, y=pred_mean)) +
			geom_ribbon(aes(x=!!predictor, ymin=pred_lower, ymax=pred_upper), alpha=0.25) +
			geom_jitter(aes(x=!!predictor, y=log_articles_per_child, colour=Country, alpha=`Article count`), width=0.1, data=d, show.legend = j == length(predictors)) +
			ylab("Articles per million children") +
			ggtitle(pretty_varname(predictor)) +
			xlab("Decile") +
			scale_x_continuous(breaks=1:10) +
			scale_y_continuous(breaks=log(1e-6*c(0.001, 0.01, 0.1, 1, 10, 100, 1000)),
					   labels=c("0.001", "0.01", "0.1", "1", "10", "100", "1000"),
					   limits=c(-24, -6.5), position=if_else(j < 5, "right", "left")) +
			scale_alpha_discrete(range=c(0.3, 1.0)) +
			theme_bw() +
			theme(aspect.ratio = 1, legend.box.margin=margin(0, 0, 0, -50))
		if(j != 4 & j != 5) {
			p <- p + theme(axis.text.y=element_blank(), axis.title.y=element_blank())
		}
		plotlist[[plot_index]] <- p
		plot_index <- plot_index + 1
	} else {
		print(paste("Ruh roh on ", predictor))
	}
}

# Make mod comparison plot
model_comparison_table <- model_comparison_table %>%
	mutate(looic_1se_down = looic - 1*looic_se,
	       looic_1se_up   = looic + 1*looic_se,
	       looic_2se_down = looic - 2*looic_se,
	       looic_2se_up   = looic + 2*looic_se)

ranked_predictors <- model_comparison_table %>%
	arrange(-looic) %>%
	pull(predictor)
model_comparison_table$predictor = factor(model_comparison_table$predictor, ranked_predictors)

p <- ggplot(model_comparison_table) +
	geom_segment(aes(x=looic_2se_down, xend=looic_2se_up, y=predictor, yend=predictor), size=2, alpha=0.5) +
	geom_segment(aes(x=looic_1se_down, xend=looic_1se_up, y=predictor, yend=predictor), size=2, alpha=0.75) +
	geom_point(aes(x=looic, y=predictor), size=5, alpha=1.0)  +
	ggtitle("Model comparisons") + 
	xlab("LOOIC (lower better)") +
	ylab("") +
	theme_bw() +
	theme(aspect.ratio = 1)

# Make the final combined plot
plotlist <- c(list(p), plotlist)
p <- wrap_plots(plotlist, nrow=2, ncol=5)
ggsave("../plots/final_singlepred_comparison.png", p, width=420, height=420/2.4, units="mm")
