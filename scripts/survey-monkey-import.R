library(surveymonkey)
library(tidyverse)
library(bit64)


# get survey monkey API token
options(sm_oauth_token = Sys.getenv("SM_TOKEN"))

# get survey data

survey_df <- 526773876 |>
  fetch_survey_obj() |>
  parse_survey() |>
  mutate(respondent_id = as.character(respondent_id))


# set column headers
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

# rename columns
names(survey_df) <- col_names

# clean and export survey data
survey_df <- survey_df |>
  select(-starts_with("x")) |>
  mutate(date_created = date(date_created))

write_rds(survey_df, "data/audience_survey.rds")

rm(list = ls())
