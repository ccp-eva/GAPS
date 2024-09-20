#!/usr/bin/env Rscript
library(tidyverse)
library(brms)

m <- readRDS("m_citations_any_nonweird.rds")

nd <- expand_grid(multi_country=c(T,F), any_nonweird=c(T,F), exposure=10)
preds <- posterior_predict(m, newdata=nd, re_formula=NA)
plot_table <- tibble(multi_country=logical(), any_nonweird=logical(), citations=integer())
for(j in 1:10) {
	for(i in 1:nrow(nd)) {
		plot_table <- add_row(plot_table, multi_country=nd$multi_country[i], any_nonweird=nd$any_nonweird[i], citations=preds[,i])
	}
}

plot_table <- plot_table %>%
	mutate(type=case_when(!multi_country & !any_nonweird ~ "Single WEIRD",
			      !multi_country & any_nonweird ~ "Single non-WEIRD",
			      multi_country & !any_nonweird ~ "Multiple, all WEIRD",
			      multi_country & any_nonweird ~ "Multiple, one or more non-WEIRD")) %>%
	mutate(`Article type`=factor(type, c(
			      "Single WEIRD",
			      "Single non-WEIRD",
			      "Multiple, all WEIRD",
			      "Multiple, one or more non-WEIRD"
			      )))

p <- ggplot(plot_table) +
	geom_density(aes(x=citations, group=`Article type`), fill="Grey", bw=7.5) +
	geom_vline(aes(xintercept=mean, ), linetype="dotted", size=2, show.legend=FALSE, data=summarise(group_by(plot_table, `Article type`), mean=mean(citations))) +
	facet_wrap(~`Article type`, dir="v", ncol=1) +
	xlab("Citations 10 years after publication") +
	ylab("") +
	theme(axis.text.y=element_blank(),
	      axis.ticks.y=element_blank()) +
	xlim(0, 100)

ggsave("../plots/citations.png", plot=p, width=210, height=210*1.618, units="mm") 
