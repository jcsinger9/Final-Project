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

agegap = subset(agegap, select = -c(couple_number, character_1_gender,
																		character_2_gender, actor_1_birthdate,
																		actor_2_birthdate))

tbl_summary(
	agegap,
	by = age_difference,
	include = c(movie_name, release_year, director, age_difference,
							actor_1_name, actor_2_name, actor_1_age, actor_2_age))

tbl_uvregression(
	alone,
	x = Season,
	include = c(Version, Season, Name, Item_number, Item),
method = lm)




