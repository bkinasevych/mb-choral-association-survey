library(surveymonkey)
library(tidyverse)


# get survey monkey API token
options(sm_oauth_token = Sys.getenv("SM_TOKEN"))

# get survey id
surveys <- browse_surveys(10)

# fetch audience survey data
survey_df <- 526773876 |>
  fetch_survey_obj() |>
  parse_survey() |>
  mutate(respondent_id = as.character(respondent_id))


# set audience survey column headers
col_names <- c(
  "x_survey_id",
  "x_collector_id",
  "id",
  "date_created",
  "x_date_modified",
  "x_status",
  "x_ip_address",
  "num_concerts_attended",
  "choirs_none",
  "attended_Canzona",
  "attended_DoW",
  "attended_Polycoro",
  "attended_Proximus5",
  "attended_Wpg_Singers",
  "attended_other",
  "frequency_now",
  "motivation_regular_patron",
  "motivation_program",
  "motivation_enjoy_choral_music",
  "motivation_support_friend",
  "motivation_venue",
  "motivation_overall_experience",
  "motivation_recommendation",
  "motivation_ad",
  "motivation_other",
  "barriers_price",
  "barriers_too_many_events",
  "barriers_inconvenient_location",
  "barriers_unaware",
  "barriers_programs_not_interesting",
  "barriers_attend_as_many_as_I_want",
  "barriers_other",
  "repetoire",
  "what_would_increase_attendance",
  "awareness",
  "distinct_choirs",
  "txt_distinct_choirs",
  "rate_value",
  "reasonable_price_range",
  "other_symphony",
  "other_opera",
  "other_theatre",
  "other_jazz",
  "other_festivals",
  "other_visual_arts",
  "other_live_performances",
  "other_noa",
  "age_group",
  "live",
  "received_from",
  "received_from_other"
)

# rename audience survey columns
names(survey_df) <- col_names

# clean and export survey data
survey_df <- survey_df |>
  select(-starts_with("x")) |>
  mutate(date_created = date(date_created))

write_rds(survey_df, "data/audience_survey.rds")

# fetch singer survey data
survey_df <- 421836094 |>
  fetch_survey_obj() |>
  parse_survey() |>
  mutate(respondent_id = as.character(respondent_id))

col_names <- c(
  "x_survey_id",
  "x_collector_id",
  "id",
  "date_created",
  "x_date_modified",
  "x_status",
  "x_ip_address",
  "num_choirs_now",
  "num_choirs_past",
  "txt_involved",
  "sing_canzona",
  "sing_dead_of_winter",
  "sing_polycoro",
  "sing_proximus_5",
  "sing_winnipeg_singers",
  "num_rehearsals",
  "motivation_artistic_challenge",
  "motivation_love_choral",
  "motivation_programming",
  "motivation_commitment",
  "motivation_performing",
  "motivation_compensation",
  "motivation_pd",
  "motivation_other",
  "sustainability",
  "txt_sustainability",
  "barriers_lack_of_paid_work",
  "barriers_balancing_choirs",
  "barriers_scheduling_conflicts",
  "barriers_travel",
  "barriers_low_pay",
  "barriers_burnout",
  "barriers_technically_challenging",
  "barriers_repertoire",
  "barriers_noa",
  "barriers_other",
  "importance_compensation",
  "fairness_compensation",
  "improve_more_coordination",
  "improve_higher_pay",
  "improve_larger_audiences",
  "improve_marketing",
  "improve_more_performances",
  "improve_different_programming",
  "improve_other",
  "influence_reduction",
  "txt_influence_reduction_other",
  "singers_support_choirs",
  "txt_other_comments",
  "age_group",
  "time_performaing"
)

# rename singer survey columns
names(survey_df) <- col_names

# clean and export survey data
survey_df <- survey_df |>
  select(-starts_with("x")) |>
  mutate(date_created = date(date_created))

write_rds(survey_df, "data/singer_survey.rds")

rm(list = ls())
