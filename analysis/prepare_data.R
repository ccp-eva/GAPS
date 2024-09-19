library(tidyverse)
source("imputation.R")

# Read article data in original one-row-per-article format
d_articles <- read_csv("../data/all_articles.csv")

# Transform article data into a one-row-per-article-and-country format
d_articles_long <- tibble()
for(i in 1:nrow(d_articles)) {
	r <- d_articles[i,] 
	split_countries <- str_split_1(r$countries, ",")
	N_countries <- length(split_countries)
	if(N_countries == 1) {
		d_articles_long <- rbind(d_articles_long, r)
		next
	}
	for(j in 1:N_countries) {
		if(split_countries[j] == "") {	# Caused by stray commas
			next
		}
		r$countries <- split_countries[j]
		d_articles_long <- rbind(d_articles_long, r)
	}
}
stopifnot(length(unique(d_articles_long$doi)) == length(unique(d_articles$doi)))

# Standardise country names to match the country data 
d_articles_long <- d_articles_long %>%
	rename(country = countries) %>%
	mutate(country = str_to_title(country)) %>%
	filter(country != "Congo") %>% # This is 10.1111/cdev.12857, miscoded
	mutate(country = case_when(country == "Usa" ~ "United States of America",
                                   country == "(Usa From Other Study)" ~ "United States of America",
                                   country == "United States" ~ "United States of America",
                                   country == "Uk" ~ "United Kingdom",
                                   country == "(Uk From Other Study)" ~ "United Kingdom",
                                   country == "Iran" ~ "Iran (Islamic Republic of)",
                                   country == "Tanzania" ~ "United Rep. of Tanzania",
                                   country == "Vietnam" ~ "Viet Nam",
                                   country == "Korea" ~ "Republic of Korea",
                                   country == "South Korea" ~ "Republic of Korea",
                                   country == "Bolivia" ~ "Bolivia (Plurin. State of)",
                                   country == "Palestine" ~ "State of Palestine",
                                   country == "Phillipines" ~ "Philippines",
                                   country == "Phillippines" ~ "Philippines",
                                   country == "German" ~ "Germany",
                                   country == "Russia" ~ "Russian Federation",
                                   country == "Bosnia" ~ "Bosnia and Herzegovina",
				   country == "Brunei" ~ "Brunei Darussalam",
				   country == "Democratic Republic Of Congo" ~ "Dem. Rep. of the Congo",
				   doi == "10.1111/cdev.12487" ~ "Dem. Rep. of the Congo",
				   doi == "10.1111/cdev.13306" ~ "Congo-Brazzaville",
                                   TRUE ~ country)
	)

# Load country data
d_countries <- read_csv("../data/country_scales.csv")

# Calculate estimated number of children and select/rename demographic variables
d_countries <- d_countries %>%
	select(country, population, percent_children,
	       NielsenWEIRD,
	       western, education_index, cip, gdp_per_capita, democracy_index,
	       individualism, urban_pc, household_size, religiosity_index, ethnic_fractionalization, GINI_index, cfst) %>%
	mutate(population=as.numeric(population),
	       children=population*1e6*percent_children/100,
	       NielsenWEIRD = NielsenWEIRD == "yes") %>%
	rename(weird=NielsenWEIRD,
	       education=education_index,
	       industry=cip,
	       gdp=gdp_per_capita,
	       democracy=democracy_index,
	       urbanism=urban_pc,
	       religiosity=religiosity_index,
	       ethfrac=ethnic_fractionalization,
	       gini=GINI_index,
	       distUS=cfst)

# Store quantiles for later
quantiles <- tibble(foo=1:11)
for(metric in c("distUS",
		"education", "industry", "gdp", "democracy",
		"household_size", "urbanism", "religiosity", "ethfrac", "gini")) {
	quantiles[[metric]] <- quantile(d_countries[[metric]], probs=seq(0,1,1/10), na.rm=TRUE)
}
# Function tonvert demographic variables to quantile codes
convert_to_scale <- function(x, y) {
	 return(ordered(factor(
		cut(x, breaks=y, include.lowest=TRUE),
	    labels=1:10)))
}

d_countries %>%
	select(-country, -population, -percent_children, -children, -weird, -western,
	       -individualism) %>%
	is.na() %>%
	apply(FUN=as.integer, MARGIN=2) %>%
	rowSums %>%
	table

# Impute missing 24% of data points
d_countries <- impute_missing_data(d_countries)

# Before converting country level predictors to ordinal format for part 1
# analyses, calculate the means for multi-country papers
d_articles_mean_preds <- d_articles_long %>%
	left_join(d_countries, by="country") %>%
	group_by(doi) %>%
	summarise(education = mean(education, na.rm=T),
	          industry = mean(industry, na.rm=T),
	          gdp = mean(gdp, na.rm=T),
	          democracy = mean(democracy, na.rm=T),
#	          individualism = mean(individualism, na.rm=T),
	          household_size=mean(household_size),
	          urbanism=mean(urbanism),
	          religiosity=mean(religiosity),
	          ethfrac=mean(ethfrac),
	          gini=mean(gini),
		  distUS=mean(distUS),
		  N_countries = length(unique(country)),
	          any_nonweird = any(!weird)
		  ) %>%
	mutate(education=convert_to_scale(education, quantiles$education),
	       industry=convert_to_scale(industry, quantiles$industry),
	       gdp=convert_to_scale(gdp, quantiles$gdp),
	       democracy=convert_to_scale(democracy, quantiles$democracy),
	       household_size=convert_to_scale(household_size, quantiles$household_size),
	       urbanism=convert_to_scale(urbanism, quantiles$urbanism),
	       # NO IDEA why this + 0.00001 hack is necessary to make cut() work...
	       religiosity=convert_to_scale(religiosity + 0.00001, quantiles$religiosity),
	       ethfrac=convert_to_scale(ethfrac, quantiles$ethfrac),
	       gini=convert_to_scale(gini, quantiles$gini),
	       distUS=convert_to_scale(distUS, quantiles$distUS),
	       ) %>%
#	       individualism=convert_to_scale(individualism, quantiles$individualism)) %>%
	left_join(d_articles, by="doi") %>%
	mutate(year = as.integer(str_sub(year_issue, 1, 4)),
	       exposure = 2022 - year,
	       countries_info_given = countries_info_given == "yes",
	       group_identifier = group_identifier == "yes",
	       evolution_word = evolution_word == "yes",
	       culture_word = culture_word == "yes",
	       crosscultural = crosscultural == "yes",
	       universal_word = universal_word == "yes")
write_csv(d_articles_mean_preds, "../data/citations_and_descriptors.csv")

# Now convert country level predictors to ordinal format
d_countries <- d_countries %>%
	mutate(education=convert_to_scale(education, quantiles$education),
	       industry=convert_to_scale(industry+1e-17, quantiles$industry),
	       gdp=convert_to_scale(gdp+1e-17, quantiles$gdp),
	       democracy=convert_to_scale(democracy+1e-17, quantiles$democracy),
#	       individualism=convert_to_scale(individualism, quantiles$education),
	       urbanism=convert_to_scale(urbanism+1e-17, quantiles$urbanism),
	       household_size=convert_to_scale(household_size+1e-17, quantiles$household_size),
	       religiosity=convert_to_scale(religiosity+1e-15, quantiles$religiosity),
	       ethfrac=convert_to_scale(ethfrac+1e-17, quantiles$ethfrac),
	       gini=convert_to_scale(gini+1e-17, quantiles$gini),
	       distUS=convert_to_scale(distUS, quantiles$distUS)
	)

# Inexplicably, the cut() function assigns Niger an education value of NA
# after the above, even though we specify include.lowest=TRUE.  No idea
# why.  Maybe some kind of floating point threshold nonsense?  Patch it
# up, carefully.
na_edu_countries <- d_countries %>%
	filter(is.na(education)) %>%
	pull(country)
stopifnot(length(na_edu_countries) == 1)
stopifnot(na_edu_countries == "Niger")
d_countries$education <- replace_na(d_countries$education, sort(d_countries$education)[1])
stopifnot(sum(is.na(d_countries$western)) == 0)
stopifnot(sum(is.na(d_countries$education)) == 0)
stopifnot(sum(is.na(d_countries$industry)) == 0)
stopifnot(sum(is.na(d_countries$gdp)) == 0)
stopifnot(sum(is.na(d_countries$democracy)) == 0)
stopifnot(sum(is.na(d_countries$household_size)) == 0)
stopifnot(sum(is.na(d_countries$urbanism)) == 0)
stopifnot(sum(is.na(d_countries$gini)) == 0)
stopifnot(sum(is.na(d_countries$religiosity)) == 0)
stopifnot(sum(is.na(d_countries$ethfrac)) == 0)
write_csv(d_countries, "../data/processed_countries.csv")

# Prepare combined data file for "articles per child" analysis
# Get article counts
d_article_counts <- d_articles_long %>%
	group_by(country) %>%
	summarise(articles = sum(!is.na(unique(doi)))) %>%
	arrange(-articles)

# Combine article counts and country data, calculate articles per child
d <- left_join(d_countries, d_article_counts, by="country") %>%
	mutate(no_articles = is.na(articles),
	       articles = as.double(articles),
	       articles = replace_na(articles, 0.01)) %>%
	filter(!is.na(children)) %>%
	mutate(articles_per_child = articles / children,
	       log_articles_per_child = log(articles_per_child))

write_csv(d, "../data/articles_per_child.csv")
