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
		character_1_gender ~ "Gender of Character 1",
		character_2_gender ~ "Gender of Character 2"
		),
	missing_text = "Missing")


tbl_summary(
	agegap,
	by = age_difference,
	include = c(actor_1_age, actor_2_age))

linear_model <- lm(age_difference ~ character_1_gender + character_2_gender +
									 actor_2_age, actor_1_age,
									 data = agegap)

tbl_regression(
	linear_model)

actor1age_table <- tbl_uvregression(
	agegap,
	y = actor_1_age,
	include = c(actor_2_age, age_difference, character_1_gender,
							character_2_gender),
method = lm)

tbl_uvregression(
	agegap,
	y = age_difference,
	include = c(actor_1_age, actor_2_age, character_1_gender,
							character_2_gender),
	method = lm)

inline_text(actor1age_table, variable = "age_difference")

hist(agegap$age_difference)

#function: mean age difference between actor 1 and actor 2
#add all packages into quarto doc and run code to create output



