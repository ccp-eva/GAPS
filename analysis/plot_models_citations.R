#!/usr/bin/env Rscript
library(tidyverse)
library(brms)
library(patchwork)

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
		return("Econ. Unequal")
	} else if(varname == "household_size") {
		return("Household Size")
	} else if(varname == "urbanism") {
		return("Urbanized")
	} else if(varname == "ethfrac") {
		return("Ethnically Diverse")
	} else if(varname == "religiosity") {
		return("Religious")
	} else {
		return(str_to_title(varname))
	}
}

predictors <- c("education", "industry", "gdp", "democracy",
	"household_size", "urbanism", "gini", "religiosity", "ethfrac")

# Initialise model comparison table and add null model
model_comparison_table <- tibble(predictor = character(), looic=numeric(), looic_se=numeric())
nullmodel_filename <- paste("m_citations_null_shape.rds", sep="")
m <- readRDS(nullmodel_filename)
l <- loo(m, moment_match=FALSE)$estimates["looic",]
model_comparison_table <- add_row(model_comparison_table, predictor = "Null model", looic=l[1], looic_se=l[2])

legend =TRUE

# Initialise plot list
plotlist <- list()
plot_index <- 1

nice_title <- "(e) Citations"
yaxis_label <- "Expected citations after 10 years"

# Iterate through predictors,
# updating model comparison table each time and
# adding plots to plotlist
for(j in 1:length(predictors)) {
	predictor <- predictors[j]
	model_filename <- paste("m_citations_singpred_", predictor, "_shape.rds", sep="")
	if(file.exists(model_filename)) {
		print(paste("found", model_filename))
		m <- readRDS(model_filename)

		# Update model comparison table
		l <- loo(m, moment_match=FALSE)$estimates["looic",]
		model_comparison_table <- add_row(model_comparison_table, predictor = pretty_varname(predictor), looic=l[1], looic_se=l[2])

		# Build plot table
		plot_table <- tibble(value=numeric(), multi_country=logical(), mean_outcome=numeric(), low_outcome=numeric(), high_outcome=numeric())
		nd <- expand_grid(foo=1:10, multi_country=c(F, T), exposure=10)
		names(nd) <- c(predictor, "multi_country", "exposure")
		preds <- posterior_linpred(m, newdata=nd, transform=TRUE, re_formula=NA)
		for(k in 1:nrow(nd)) {
			plot_table <- add_row(plot_table, value=nd[[predictor]][k], multi_country=nd$multi_country[k], mean_outcome=mean(preds[,k]), low_outcome=quantile(preds[,k], p=0.025), high_outcome=quantile(preds[,k], p=0.975))
		}
		foonames <- colnames(plot_table)
		foonames[1] <- predictor
		colnames(plot_table) <- foonames

		# Create plot, append to plotlist
		predictor <- ensym(predictor)
		plot_table$`Countries sampled` <- if_else(plot_table$multi_country, "Multiple", "Single")
		plot_table$`Countries sampled` <- factor(plot_table$`Countries sampled`, c("Single", "Multiple"), ordered=TRUE)
		p <- ggplot(plot_table) +
			geom_line(aes(x=!!predictor, y=mean_outcome, group=`Countries sampled`, colour=`Countries sampled`), show.legend = legend && j == length(predictors)) +
			geom_ribbon(aes(x=!!predictor, ymin=low_outcome, ymax=high_outcome, fill=`Countries sampled`, group=`Countries sampled`), alpha=0.25, show.legend = legend && j == length(predictors)) +
#			geom_hline(yintercept=0.5, linetype="dotted") +

			ggtitle(pretty_varname(predictor)) +
			labs(fill="Countries\nsampled", colour="Countries\nsampled") +
			xlab("Decile") +
			ylab(yaxis_label) +

			scale_x_continuous(breaks=1:10) +
			scale_y_continuous(limits=c(0, 2000), position="right") +
			scale_colour_manual(values=c("#F8766D", "#00BFC4")) +
			scale_fill_manual(values=c("#F8766D", "#00BFC4")) +
			theme_bw() +
			coord_cartesian(ylim = c(0, 300)) +
			theme(aspect.ratio = 1)
		if(j != 9) {
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
	ggtitle("Model Comparisons") +
	xlab("LOOIC (lower better)") +
	ylab("") +
	theme_bw() +
	theme(aspect.ratio = 1)

# Make the final combined plot
plotlist <- c(list(p), plotlist)
p <- wrap_plots(plotlist, nrow=1, ncol=10) +
	plot_annotation(title=nice_title, theme = theme(plot.title = element_text(size = 18)))
ggsave(paste("../plots/final_longer_citations_singlepred_interaction_comparison.png", sep=""), p, width=297*2, height=0.95*(2*210/5), units="mm")
