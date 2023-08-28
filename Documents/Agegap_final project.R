install.packages("gtsummary")
install.packages(tidyverse)
install.packages("here")
install.packages("broom")

library(gtsummary)
library(tidyverse)
library(broom)

agegap_cols <- c("movie_name", "release_year", "director",
								 "age_difference", "couple_number", "actor_1_name",
								 "actor_2_name", "character_1_gender", "character_2_gender",
								 "actor_1_birthdate", "actor_2_birthdate", "actor_1_age",
								 "actor_2_age")

agegap <- read_csv(here::here("age_gaps.csv"),
col_names = agegap_cols)

agegap = subset(agegap, select = -c(couple_number, actor_1_birthdate,
																		actor_2_birthdate, age_difference))

agegap$character_1_gender <- ifelse(agegap$character_1_gender
																		%in% c("man"),1,0)
agegap$character_2_gender <- ifelse(agegap$character_2_gender
																		%in% c("man"),1,0)

#Characters 1 and 2 are both coded as 1 for "man" and 0 for "woman"

agegap$actor_1_age <- as.numeric(agegap$actor_1_age)

agegap$actor_2_age <- as.numeric(agegap$actor_2_age)

agegap$age_difference <- (agegap$actor_1_age - agegap$actor_2_age)

tbl_summary(
	agegap,
	by = character_1_gender,
	include = c(actor_2_age, age_difference, actor_1_age,
							character_2_gender),
	label = list(
		actor_2_age ~ "Age of Actor 2",
		actor_1_age ~ "Age of Actor 1",
		age_difference ~ "Age Difference Between Actor 1 and Actor 2",
		character_1_gender ~ "Gender Identity of Character 1",
		character_2_gender ~ "Gender Identity of Character 2"
		),
	missing = "no")

actor1age_table <- tbl_uvregression(
	agegap,
	y = actor_1_age,
	include = c(actor_2_age, age_difference, character_1_gender,
							character_2_gender),
method = lm)

inline_text(actor1age_table, variable = "actor_2_age")

agediffvalue <- inline_text(actor1age_table, variable = "age_difference")

avgact1age <- mean(agegap$actor_1_age, na.rm = TRUE)

avgact2age <- mean(agegap$actor_2_age, na.rm = TRUE)

hist(agegap$age_difference)

#function: average difference in age

mean_age <- function(age_difference) {
	t <- length(age_difference)
	mean <- sum(age_difference, na.rm = TRUE)/t
	return(mean)
}

avgagediff <- mean_age(agegap$age_difference)


usethis::use_readme_rmd

