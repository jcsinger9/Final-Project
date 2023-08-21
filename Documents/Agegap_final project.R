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

agegap$character_1_gender <- ifelse(agegap$character_1_gender %in% c("man"),1,0)
agegap$character_1_gender <- ifelse(agegap$character_1_gender %in% c("man"),1,0)



table(agegap$character_1_gender, useNA="always")

as.numeric(agegap$actor_1_age)
as.numeric(agegap$actor_2_age)

agegap$age_difference <- (actor_1_age - actor_2_age)

tbl_summary(
	agegap,
	by = movie_name,
	include = c(movie_name, release_year, director, age_difference,
							actor_1_age, actor_2_age))

tbl_uvregression(
	agegap,
	y = age_difference,
	include = c(release_year, director, age_difference),
method = lm)

hist(agegap$actor_1_age)
