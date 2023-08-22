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
age1[!is.na(age1)]

agegap$actor_2_age <- as.numeric(agegap$actor_2_age)
age2[!is.na(age2)]

agegap$age_difference <- (agegap$actor_1_age - agegap$actor_2_age)

tbl_summary(
	agegap,
	by = actor_1_age,
	include = c(actor_2_age, age_difference))

tbl_summary(
	agegap,
	by = age_difference,
	include = c(actor_1_age, actor_2_age))

tbl_uvregression(
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


hist(agegap$actor_1_age)
