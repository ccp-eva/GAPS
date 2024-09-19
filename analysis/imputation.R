impute_missing_data <- function(d) {
	d_vestig <- d %>%
		select(country, population, percent_children, children, weird, western, individualism)
	d <- d %>%
		select(-population, -percent_children, -children, -weird, -western, 
		       -individualism)
	d$missing_features <- d %>%
		is.na() %>%
		apply(FUN=as.integer, MARGIN=2) %>%
		rowSums()

	d_scaled <- d %>%
		mutate(education=scale(education),
		       industry=scale(industry),
		       gdp=scale(gdp),
		       democracy=scale(democracy),
		       urbanism=scale(urbanism),
		       household_size=scale(household_size),
		       religiosity=scale(religiosity),
		       ethfrac=scale(ethfrac),
		       gini=scale(gini),
		       distUS=scale(distUS),
		       )

	for(missing_no in 1:10) {
		d_complete <- filter(d_scaled, missing_features == 0)
		complete_matrix <- d_complete %>%
			select(-country, -missing_features) %>%
			as.matrix()
		incomplete_countries <- filter(d_scaled, missing_features == missing_no) %>% pull(country)
	
		for(inc in incomplete_countries) {
			print(paste("Finding NNs for: ", inc))
			incomp <- d_scaled %>%
				filter(country == inc) %>%
				select(-country, -missing_features)
			dm <- dist(rbind(incomp, complete_matrix))
			d_complete$dist_to_target <- as.vector(as.matrix(dm)[1,])[2:(nrow(d_complete)+1)]
			neighbours <- d_complete %>% arrange(dist_to_target) %>%
				head(n=10) 
			print(neighbours$country)
			mean_vals <- neighbours %>%
				select(-country, -missing_features) %>%
				colMeans
			print(mean_vals)
			country_index <- which(d_scaled$country == inc)
			na_indices <- which(is.na(incomp))
			for(x in na_indices) {
				d_scaled[country_index, x+1] <- mean_vals[x]
			}
			d_scaled$missing_features[country_index] <- 0
			print(d_scaled[country_index,])
		}
	}
	for(metric in colnames(select(d_scaled, -country, -missing_features))) {
		scaled <- d_scaled[[metric]]
		center <- attr(scaled, "scaled:center")
		scal <- attr(scaled, "scaled:scale")
		scaled <- as.vector(scaled)
		d[[metric]] <- center + scal*scaled
	}
	return(left_join(d, d_vestig))
}
