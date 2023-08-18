library(tidyverse)

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
	       children=population*1e6*percent_children/100) %>%
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

# Convert demographic variables to quantile codes
convert_to_scale <- function(x) {
	 return(ordered(factor(
		cut(x,
		    breaks=quantile(x, probs=seq(0,1,1/10), na.rm=TRUE),
		    include.lowest=TRUE),
	    labels=1:10)))
}

d_countries <- d_countries %>%
	mutate(education=convert_to_scale(education),
	       industry=convert_to_scale(industry),
	       gdp=convert_to_scale(gdp),
	       democracy=convert_to_scale(democracy),
	       individualism=convert_to_scale(individualism),
	       urbanism=convert_to_scale(urbanism),
	       household_size=convert_to_scale(household_size),
	       religiosity=convert_to_scale(religiosity),
	       ethfrac=convert_to_scale(ethfrac),
	       gini=convert_to_scale(gini),
	       distUS=convert_to_scale(distUS)
	)

# Prepare combined data file for "articles per child" analysis
# Get article counts
d_article_counts <- d_articles_long %>%
	group_by(country) %>%
	summarise(articles = sum(!is.na(unique(doi)))) %>%
	arrange(-articles)

# Combine article counts and country data, calculate articles per child
d <- left_join(d_countries, d_article_counts, by="country") %>%
	mutate(articles = as.double(articles),
	       articles = replace_na(articles, 0.01)) %>%
	filter(!is.na(children)) %>%
	mutate(articles_per_child = articles / children,
	       log_articles_per_child = log(articles_per_child))

write_csv(d, "../data/articles_per_child.csv")
