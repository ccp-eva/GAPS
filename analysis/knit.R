#!/usr/bin/Rscript
library(rmarkdown)
rmarkdown::render("articles_per_child_summary.Rmd", "pdf_document", clean=FALSE)
