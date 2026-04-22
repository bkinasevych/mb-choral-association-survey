library(tidyverse)

audience_survey_txt <- read_rds("data/audience_survey.rds") |> 
  select(id, motivation_other, barriers_other, repetoire, what_would_increase_attendance,
txt_distinct_choirs, received_from_other)

write_csv(audience_survey_txt, "data/audience_survey_qualitative.csv")

singer_survey_txt <- read_rds("data/singer_survey.rds") |> 
  select(id, starts_with("txt"), motivation_other, barriers_other, improve_other)

write_csv(singer_survey_txt, "data/singer_survey_qualitative.csv")

