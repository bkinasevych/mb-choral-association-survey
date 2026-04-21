#
#
#
#
#
#
#
#

#| include: false
#| label: setup

library(tidyverse)
library(gt)
library(scales)

#source("scripts/survey-monkey-import.R")


audience_data_full <- read_rds("data/audience_survey.rds") |> 
    mutate(age_group_collapsed = case_when(
        age_group == "65+" ~ age_group,
        TRUE ~ "Under 65"
    ))

singer_data_full <- read_rds("data/singer_survey.rds") |> 
    mutate(age_group_collapsed = case_when(
        age_group %in% c("Under 25", "25–34", "35–44") ~ "Under 45",
        age_group %in% c("45–54", "55–64", "65+") ~ "45 plus",
        TRUE ~ age_group)) |> 
    mutate(time_performing_collapsed = case_when(
        time_performing == "more than 10 years" ~ "More than 10 yrs",
        time_performing %in% c("1-2 years", "3-5 years", "6-10 years") ~ "Less than 10 yrs",
        TRUE ~ time_performing)) |> 
    mutate(age_group_collapsed = factor(age_group_collapsed,
        levels = c("45 plus", "Under 45")),
            time_performing_collapsed = factor(time_performing_collapsed,
        levels = c("More than 10 yrs", "Less than 10 yrs")))


organization_data_full <- read_rds("data/organization_survey.rds")

# set colours
teal <- "#007f97"
light_teal <- "#98b8b8"
purple <- "#4d4076"
light_purple <- "#8e7291"

# set ggplot theme
plot_theme <- function(){
    theme_minimal(base_size = 14) +
    theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        plot.title.position = "plot"
    )
}

source("scripts/functions.R")


#
#
#
#
#
#
#
n_respondents <- audience_data_full |> 
    count() |> 
    pull(n)

n_under65 <- audience_data_full |> 
    filter(age_group_collapsed == "Under 65") |> 
    count() |> 
    pull()

n_over65 <- audience_data_full |> 
    filter(age_group_collapsed == "65+") |> 
    count() |> 
    pull()
#
#
#
#
#
#
#
#
#
#
#| title: "Age groups"

df <- audience_data_full |> 
    count(age_group) |> 
    drop_na(age_group) |> 
    mutate(pct = n/sum(n)) |> 
    mutate(pct_label = paste0(round(pct * 100, 0), "%"))

simple_bar_chart(df, age_group)

#
#
#
#| title: "Location"

df <- audience_data_full |> 
    count(live) |> 
    drop_na(live) |> 
    mutate(pct = n/sum(n)) |> 
    mutate(pct_label = paste0(round(pct*100, 0), "%"))

simple_bar_chart(df, live) +
    labs(title = "Where do you currently live?")

#
#
#
#| title: "Origin"

df <- audience_data_full |> 
    count(received_from) |> 
    drop_na() |> 
    mutate(
        pct = n/sum(n),
        pct_label = paste0(round(pct*100, 1), "%"),
        received_from = fct_reorder(received_from, pct, .desc = TRUE))
        
simple_bar_chart(df, received_from) +
    labs(title = "How did you receive this survey?") +
    scale_x_continuous(expand = expansion(mult = c(0, 0.2)))
#
#
#
#
#| title: "Attendance"

df <- audience_data_full |> 
    count(num_concerts_attended) |> 
    drop_na(num_concerts_attended) |> 
    mutate(
        pct = n/sum(n),
        pct_label = paste0(round(pct * 100, 0)))
    
simple_bar_chart(df, num_concerts_attended) +
    labs(title = "Approximately how many choral concerts do you attend in a year?")


#
#
#
#
#
#| title: "Attendance by age"

df <- audience_data_full |> 
    select(age_group_collapsed, num_concerts_attended) |> 
    drop_na() |> 
    group_by(age_group_collapsed) |> 
    count(num_concerts_attended) |> 
    mutate(pct = n/sum(n),
    pct_label = paste0(round(pct * 100, 0), "%"))

grouped_bar_chart(df, num_concerts_attended, age_group_collapsed) +
    labs(
        title = "Approximately how many choral concerts do you attend in a year?",
        subtitle = "By age group"
    )


#
#
#
#
#
#
#| title: "Choirs"

df <- audience_data_full |> 
    select(starts_with("attended")) |> 
    pivot_longer(cols = everything()) |> 
    filter(!is.na(value)) |> 
    count(value) |> 
    mutate(pct = n/n_respondents,
        pct_label = paste0(round(pct * 100, 0), "%"),
        value = fct_reorder(value, pct, .desc = TRUE))
        
simple_bar_chart(df, value) +
    labs(title = "Which of the following choirs have you attended in the past 3 years?")
#
#
#
#| title: "Choirs by age"

df <- audience_data_full |> 
    select(starts_with("attended"), age_group_collapsed) |> 
    pivot_longer(cols = -age_group_collapsed) |> 
    filter(!is.na(value)) |> 
    group_by(age_group_collapsed) |> 
    count(value) |> 
    mutate(
        pct = n/n_respondents,
        pct_label = paste0(round(pct * 100, 0), "%"),
        value = fct_reorder(value, pct, .desc = TRUE))

grouped_bar_chart(df, value, age_group_collapsed) +
    labs(
        title = "Which of the following choirs have you attended in the past 3 years?",
        subtitle = "By age group"
    )


#
#
#
#| title: "Post-Covid attendance"

df <- audience_data_full |> 
    count(frequency_now) |> 
    mutate(
        pct = n/sum(n),
        pct_label = paste0(round(pct * 100, 0), "%")
        )

simple_bar_chart(df, frequency_now) + 
    labs(title = "Compared to before the pandemic, how often do you attend choral concerts?")




#
#
#
#| title: "Post-Covid attendance by age"

df <- audience_data_full |>
    group_by(age_group_collapsed) |> 
    count(frequency_now) |> 
    mutate(
        pct = n/sum(n),
        pct_label = paste0(round(pct * 100, 0), "%")
        )

grouped_bar_chart(df, frequency_now, age_group_collapsed) +
    labs(
        title = "Compared to before the pandemic, how often do you attend choral concerts?",
        subtitle = "By age group"
    )     

#
#
#
#
#
#
#
#| title: "Motivation"

df <- audience_data_full |> 
    select(starts_with("motivation")) |> 
    mutate(motivation_other = case_when(
        !is.na(motivation_other) ~ "Other",
        TRUE ~ NA
    )) |> 
    pivot_longer(cols = everything()) |> 
    select(value) |> 
    mutate(value = str_remove(value, "<span>"),
    value = str_remove(value, "</span>")) |> 
        filter(!is.na(value)) |> 
        count(value) |> 
        arrange(desc(n)) |> 
        mutate(
            pct = n/n_respondents,
            pct_label = paste0(round(pct * 100, 0), "%"),
            value = fct_reorder(value, pct, .desc = TRUE))
            
simple_bar_chart(df, value) +
    labs(
        title = "What usually motivates you to attend a choral concert?"
    )

#
#
#
#| title: "Motivation by age"

df <- audience_data_full |> 
    select(starts_with("motivation"), age_group_collapsed) |> 
    mutate(motivation_other = case_when(
        !is.na(motivation_other) ~ "Other",
        TRUE ~ NA
    )) |> 
    pivot_longer(cols = -age_group_collapsed) |> 
    mutate(value = str_remove(value, "<span>"),
    value = str_remove(value, "</span>")) |> 
        filter(!is.na(value)) |> 
    group_by(age_group_collapsed) |> 
        count(value) |> 
        mutate(
            pct = n/n_respondents,
            pct_label = paste0(round(pct * 100, 0), "%"),
            value = fct_reorder(value, pct, .desc = TRUE))
            
grouped_bar_chart(df, value, age_group_collapsed) +
    labs(
        title = "What usually motivates you to attend a choral concert?",
        subtitle = "By age group"
    )


#
#
#
#| title: "Barriers"

df <- audience_data_full |> 
    select(starts_with("barriers")) |> 
    mutate(barriers_other = case_when(
        !is.na(barriers_other) ~ "Other",
        TRUE ~ NA
    )) |> 
    pivot_longer(cols = everything()) |> 
    select(value) |> 
        filter(!is.na(value)) |> 
        count(value) |> 
        arrange(desc(n)) |> 
        mutate(
            pct = n/n_respondents,
            pct_label = paste0(round(pct * 100, 0), "%"),
            value = fct_reorder(value, pct, .desc = TRUE))

simple_bar_chart(df, value) +
    labs(
        title = "What factors might prevent you from attending a choral concert more often?"
    )
#
#
#
#| title: "Barriers by age"

df <- audience_data_full |> 
    select(starts_with("barriers"), age_group_collapsed) |> 
    mutate(barriers_other = case_when(
        !is.na(barriers_other) ~ "Other",
        TRUE ~ NA
    )) |> 
    pivot_longer(cols = -age_group_collapsed) |> 
    filter(!is.na(value)) |> 
    group_by(age_group_collapsed) |> 
    count(value) |> 
    mutate(pct = n/sum(n),
        pct_label = paste0(round(pct * 100, 0), "%"),
        value = fct_reorder(value, pct, .desc = TRUE))

grouped_bar_chart(df, value, age_group_collapsed) +
    labs(
        title = "What factors might prevent you from attending a choral concert more often?",
        subtitle = "By age group"
    )
#
#
#
#| title: "Other events"

df <- audience_data_full |> 
    select(starts_with("other")) |> 
    pivot_longer(cols = everything()) |> 
    select(value) |> 
    drop_na(value) |> 
    count(value) |> 
    arrange(desc(n)) |> 
    mutate(pct = n/n_respondents,
        pct_label = paste0(round(pct * 100, 0), "%"),
        value = fct_reorder(value, pct, .desc = TRUE))
        
simple_bar_chart(df, value) +
    labs(
        title = "What other types of arts or cultural event do you attend?"
    )

#
#
#
#| title: "Other events by age"

df <- audience_data_full |> 
    select(starts_with("other"), age_group_collapsed) |> 
    pivot_longer(cols = -age_group_collapsed) |> 
    drop_na(value) |> 
    group_by(age_group_collapsed) |> 
    count(value) |> 
    mutate(pct = n/n_respondents,
        pct_label = paste0(round(pct * 100, 0), "%"),
        value = fct_reorder(value, pct, .desc = TRUE))
        
grouped_bar_chart(df, value, age_group_collapsed) +
    labs(
        title = "What other types of arts or cultural event do you attend?",
        subtitle = "By age group",
    )
#
#
#
#
#
#| title: "Awareness"

df <- audience_data_full |> 
    count(awareness) |> 
    drop_na(awareness) |> 
    mutate(pct = n/sum(n),
        pct_label = paste0(round(pct * 100, 0), "%"),)
        
simple_bar_chart(df, awareness) +
    labs(
        title = "Before receiving this survey, how aware were you of the range of professional choral organizations in Manitoba?"
    )
    
#
#
#
#| title: "Awareness by age"

df <- audience_data_full |> 
    group_by(age_group_collapsed) |> 
    count(awareness) |> 
    drop_na(awareness) |> 
    mutate(pct = n/sum(n),
        pct_label = paste0(round(pct * 100, 0), "%"))
        
grouped_bar_chart(df, awareness, age_group_collapsed) +
    labs(
        title = "Before receiving this survey, how aware were you of the range of professional choral organizations in Manitoba?",
        subtitle = "By age group"
    )

#
#
#
#| title: "Distinction"

df <- audience_data_full |> 
    count(distinct_choirs) |> 
    drop_na(distinct_choirs) |> 
    mutate(pct = n/sum(n),
        pct_label = paste0(round(pct * 100, 0), "%"))

simple_bar_chart(df, distinct_choirs) +
    labs(
        title = "In your experience, do the different professional choirs in Manitoba have clear and distinct identities and styles?"
    )

#
#
#
#| title: "Distinction by age"

df <- audience_data_full |> 
    group_by(age_group_collapsed) |> 
    count(distinct_choirs) |> 
    drop_na(distinct_choirs) |> 
    mutate(pct = n/sum(n),
        pct_label = paste0(round(pct * 100, 0), "%"))
        
grouped_bar_chart(df, distinct_choirs, age_group_collapsed) +
    labs(
        title = "In your experience, do the different professional choirs in Manitoba have clear and distinct identities and styles?",
        subtitle = "By age group"
    )

#
#
#
#| title: "Value"

df <- audience_data_full |> 
    count(rate_value) |> 
    drop_na(rate_value) |> 
    mutate(pct = n/sum(n),
        pct_label = paste0(round(pct * 100, 0), "%"))
        
simple_bar_chart(df, rate_value) +
    labs(
        title = "How would you rate the value of choral concert tickets compared with other arts events you attend?"
    )

#
#
#
#| title: "Value by age"

df <- audience_data_full |> 
    group_by(age_group_collapsed) |> 
    count(rate_value) |> 
    drop_na(rate_value) |> 
    mutate(pct = n/sum(n),
        pct_label = paste0(round(pct * 100, 0), "%"))
        
grouped_bar_chart(df, rate_value, age_group_collapsed) +
    labs(
        title = "In your experience, do the different professional choirs in Manitoba have clear and distinct identities and styles?",
        subtitle = "By age group"
    )
#
#
#
#| title: "Price range"

df <- audience_data_full |> 
    count(reasonable_price_range) |> 
    drop_na(reasonable_price_range) |> 
        mutate(pct = n/sum(n),
        pct_label = paste0(round(pct * 100, 0), "%"))

simple_bar_chart(df, reasonable_price_range) +
    labs(
        title = "What ticket price range feels reasonable to you for a professional choral concert?"
    )
#
#
#
#| title: "Price range by age"

df <- audience_data_full |> 
    group_by(age_group_collapsed) |> 
    count(reasonable_price_range) |> 
    drop_na(reasonable_price_range) |> 
        mutate(pct = n/sum(n),
        pct_label = paste0(round(pct * 100, 0), "%"))

grouped_bar_chart(df, reasonable_price_range, age_group_collapsed) +
    labs(
        title = "What ticket price range feels reasonable to you for a professional choral concert?",
        subtitle = "By age group"
    )
#
#
#
#
#
#
#
n_respondents <- count(singer_data_full) |> 
    pull(n)
#
#
#
#
#
#
#
#
#
#| title: "Age"

df <- singer_data_full |> 
    count(age_group) |> 
    drop_na(age_group) |> 
    mutate(pct = n/sum(n),
    pct_label = paste0(round(pct * 100, 0), "%"))

simple_bar_chart(df, age_group)


#
#
#
#| title: "Years performing"

df <- singer_data_full |> 
    count(time_performing) |> 
    drop_na(time_performing) |> 
    mutate(pct = n/sum(n),
    pct_label = paste0(round(pct * 100, 0), "%"))

simple_bar_chart(df, time_performing)


#
#
#
#| title: "Num choirs"

df <- singer_data_full |> 
    count(num_choirs_now) |> 
    calc_percent()

simple_bar_chart(df, num_choirs_now) +
    labs(title = "How many choirs do you currently sing with?")

#
#
#
#
#| title: "Num past choirs"

df <- singer_data_full |> 
    count(num_choirs_past) |> 
    calc_percent()

simple_bar_chart(df, num_choirs_past) +
    labs(title = "In the past 5 years, how many different choirs have\nyou sung with in Manitoba?")

#
#
#
#| title: "All choirs"

df <- singer_data_full |> 
    select(starts_with("sing_")) |> 
    pivot_longer(cols = everything()) |> 
    drop_na(value) |> 
    count(value) |> 
    mutate(pct = n/n_respondents,
        pct_label = paste0(round(pct * 100, 0), "%")) |> 
    mutate(value = fct_reorder(value, pct, .desc = TRUE))

simple_bar_chart(df, value) +
    labs(title = "Which organizations do you currently sing with?")
#
#
#
#| title: "Num rehearsals"

df <- singer_data_full |> 
    count(num_rehearsals) |> 
    drop_na(num_rehearsals) |> 
    calc_percent()

simple_bar_chart(df, num_rehearsals) +
    labs(
        title = "How many choral rehearsals per week do you typically attend during the busiest part of the season?"
    )
#
#
#
#
#
#
#| title: "Motivation"

df <- singer_data_full |> 
    select(starts_with("motivation")) |> 
    mutate(motivation_other = case_when(
        !is.na(motivation_other) ~ "Other",
        TRUE ~ NA
    )) |> 
    pivot_longer(cols = everything()) |> 
    drop_na(value) |> 
    count(value) |> 
    mutate(pct = n/n_respondents,
        pct_label = paste0(round(pct * 100, 0), "%")) |> 
    mutate(value = fct_reorder(value, pct, .desc = TRUE))

simple_bar_chart(df, value) +
    labs(
        title = "What motivates you to sing in a professional choir?"
    )
#
#
#
#| title: "Motivation by age"

n_under45 <- singer_data_full |> 
    filter(age_group_collapsed == "Under 45") |> 
    count() |> 
    pull()

n_over45 <- singer_data_full |> 
    filter(age_group_collapsed == "45 plus") |> 
    count() |> 
    pull()

df <- singer_data_full |> 
    select(age_group_collapsed, starts_with("motivation")) |> 
    mutate(motivation_other = case_when(
        !is.na(motivation_other) ~ "Other",
        TRUE ~ NA
    )) |> 
    pivot_longer(cols = -age_group_collapsed) |> 
    group_by(age_group_collapsed) |> 
    count(value) |> 
    drop_na() |> 
    mutate(denom = if_else(age_group_collapsed == "Under 45",
        n_under45, n_over45)) |> 
    mutate(pct = n/denom,
        pct_label = paste0(round(pct * 100, 0), "%"))

grouped_bar_chart(df, value, age_group_collapsed) +
    labs(
        title = "What motivates you to sing in a professional choir?",
        subtitle = "By age group"
    )
#
#
#
#| title: "Motivation by years"

n_under_ten <- singer_data_full |> 
    filter(time_performing_collapsed == "Less than 10 yrs") |> 
    count() |> 
    pull()

n_over_ten <- singer_data_full |> 
    filter(time_performing_collapsed == "More than 10 yrs") |> 
    count() |> 
    pull()

df <- singer_data_full |> 
    select(time_performing_collapsed, starts_with("motivation")) |>
    mutate(motivation_other = case_when(
        !is.na(motivation_other) ~ "Other",
        TRUE ~ NA
    )) |> 
    pivot_longer(cols = -time_performing_collapsed) |> 
    group_by(time_performing_collapsed) |> 
    count(value) |> 
    drop_na() |> 
    mutate(denom = if_else(time_performing_collapsed == "Less than 10 yrs",
        n_under_ten, n_over_ten)) |> 
    mutate(pct = n/denom,
        pct_label = paste0(round(pct * 100, 0), "%"))

grouped_bar_chart(df, value, time_performing_collapsed) +
    labs(
        title = "What motivates you to sing in a professional choir?",
        subtitle = "By number of years performing"
    )

```
#
#| title: "Compensation"

df <- singer_data_full |> 
    count(importance_compensation) |> 
    drop_na() |> 
    calc_percent()

simple_bar_chart(df, importance_compensation) +
    labs(
        title = "How important is compensation in your decision\nto participate in a choir?"
    )
#
#
#
#| title: "Compensation by age"

df <- singer_data_full |> 
    group_by(age_group_collapsed) |> 
    count(importance_compensation) |> 
    drop_na() |> 
    calc_percent()

grouped_bar_chart(df, importance_compensation, age_group_collapsed) +
    labs(
        title = "How important is compensation in your decision\nto participate in a choir?",
        subtitle = "By age group"
    )
#
#
#
#| title: "Compensation by years"

df <- singer_data_full |> 
    group_by(time_performing_collapsed) |> 
    count(importance_compensation) |> 
    drop_na() |> 
    calc_percent()

grouped_bar_chart(df, importance_compensation, time_performing_collapsed) +
    labs(
        title = "How important is compensation in your decision\nto participate in a choir?",
        subtitle = "By number of years performing"
    )
#
#
#
#
#
#
#
#
#| title: "Satisfaction w/ pay"

df <- singer_data_full |> 
    count(fairness_compensation) |> 
    drop_na() |> 
    calc_percent()

simple_bar_chart(df, fairness_compensation) +
    labs(
        title = "In your view, how fair is the compensation singers\nreceive from professional choirs in Manitoba?"
    )

#
#
#
#| title: "Satisfaction by age"

df <- singer_data_full |> 
    group_by(age_group_collapsed) |> 
    count(fairness_compensation) |> 
    drop_na() |> 
    calc_percent()

grouped_bar_chart(df, fairness_compensation, age_group_collapsed) +
    labs(
        title = "In your view, how fair is the compensation singers\nreceive from professional choirs in Manitoba?",
        subtitle = "By age group"
    )

#
#
#
#
#| title: "Satisfaction by years"

df <- singer_data_full |> 
    group_by(time_performing_collapsed) |> 
    count(fairness_compensation) |> 
    drop_na() |> 
    calc_percent()

grouped_bar_chart(df, fairness_compensation, time_performing_collapsed) +
    labs(
        title = "In your view, how fair is the compensation singers\nreceive from professional choirs in Manitoba?",
        subtitle = "By number of years performing"
    )
#
#
#
#| title: "Sustainability"

df <- singer_data_full |> 
    count(sustainability) |> 
    calc_percent()

simple_bar_chart(df, sustainability) +
    labs(
        title = "How sustainable does your current level\nof professional choral involvement feel for you personally?"
    )
#
#
#
#| title: "Sustainability by age"

df <- singer_data_full |> 
    group_by(age_group_collapsed) |> 
    count(sustainability) |> 
    calc_percent() |> 
    drop_na()

grouped_bar_chart(df, sustainability, age_group_collapsed) +
    labs(
        title = "How sustainable does your current level\nof professional choral involvement feel for you personally?",
        subtitle = "By age group"
    )

#
#
#
#| title: "Sustainability by years"

df <- singer_data_full |> 
    group_by(time_performing_collapsed) |> 
    count(sustainability) |> 
    calc_percent() |> 
    drop_na()

grouped_bar_chart(df, sustainability, time_performing_collapsed) +
    labs(
        title = "How sustainable does your current level\nof professional choral involvement feel for you personally?",
        subtitle = "By number of years performing"
    )

#
#
#
#| title: "Barriers"

df <- singer_data_full |> 
    select(starts_with("barriers")) |> 
    mutate(barriers_other = case_when(
        !is.na(barriers_other) ~ "Other",
        TRUE ~ NA
    )) |> 
    pivot_longer(cols = everything()) |> 
    count(value) |> 
    drop_na() |> 
    mutate(pct = n/n_respondents,
        pct_label = paste0(round(pct * 100, 0), "%")) |> 
    mutate(value = fct_reorder(value, pct, .desc = TRUE))

simple_bar_chart(df, value) +
    labs(
        title = "What challenges do you experience related to singing in professional choirs?"
    )
#
#
#
#| title: "Barriers by age"

df <- singer_data_full |> 
    select(starts_with("barriers"), age_group_collapsed) |> 
    mutate(barriers_other = case_when(
        !is.na(barriers_other) ~ "Other",
        TRUE ~ NA
    )) |>    
    pivot_longer(cols = -age_group_collapsed) |> 
    group_by(age_group_collapsed) |> 
    count(value) |> 
    drop_na() |> 
    mutate(denom = if_else(age_group_collapsed == "Under 45",
        n_under45, n_over45),
        pct = n/denom,
        pct_label = paste0(round(pct * 100, 0), "%"),
        ) |> 
    mutate(value = fct_reorder(value, pct, .desc = TRUE))

grouped_bar_chart(df, value, age_group_collapsed) +
    labs(
        title = "What challenges do you experienc related to singing in professional choirs?",
        subtitle = "By age group"
    )

#
#
#
#| title: "Barriers by years"

df <- singer_data_full |> 
    select(starts_with("barriers"), time_performing_collapsed) |> 
    mutate(barriers_other = case_when(
        !is.na(barriers_other) ~ "Other",
        TRUE ~ NA
    )) |>    
    pivot_longer(cols = -time_performing_collapsed) |> 
    group_by(time_performing_collapsed) |> 
    count(value) |> 
    drop_na() |> 
    mutate(denom = if_else(time_performing_collapsed == "Less than 10 yrs",
        n_under_ten, n_over_ten),
        pct = n/denom,
        pct_label = paste0(round(pct * 100, 0), "%"),
        ) |> 
    mutate(value = fct_reorder(value, pct, .desc = TRUE))

grouped_bar_chart(df, value, time_performing_collapsed) +
    labs(
        title = "What challenges do you experienc related to singing in professional choirs?",
        subtitle = "By number of years performing"
    )

#
#
#
#
#
#| title: "Reduced involvement"

df <- singer_data_full |> 
    count(influence_reduction) |> 
    drop_na() |> 
    calc_percent() |> 
    mutate(influence_reduction = fct_reorder(influence_reduction, pct, .desc = TRUE))

simple_bar_chart(df, influence_reduction) +
    labs(
        title = "If you had to reduce your choir involvement, what would most influence that decision?"
    )
#
#
#
#| title: "Reduce involvement by age"

df <- singer_data_full |> 
    group_by(age_group_collapsed) |> 
    count(influence_reduction) |> 
    drop_na() |> 
    calc_percent() |> 
    mutate(influence_reduction = fct_reorder(influence_reduction, pct, .desc = TRUE))

grouped_bar_chart(df, influence_reduction, age_group_collapsed) +
    labs(
        title = "If you had to reduce your choir involvement, what would most influence that decision?",
        subtitle = "By age group"
    )

#
#
#
#| title: "Reduce involvement by years"

df <- singer_data_full |> 
    group_by(time_performing_collapsed) |> 
    count(influence_reduction) |> 
    drop_na() |> 
    calc_percent() |> 
    mutate(influence_reduction = fct_reorder(influence_reduction, pct, .desc = TRUE))

grouped_bar_chart(df, influence_reduction, time_performing_collapsed) +
    labs(
        title = "If you had to reduce your choir involvement, what would most influence that decision?",
        subtitle = "By number of years performing"
    )
#
#
#
#
#| title: "Improvements"

df <- singer_data_full |> 
    select(starts_with("improve")) |> 
    mutate(improve_other = case_when(
        !is.na(improve_other) ~ "Other",
        TRUE ~ NA)) |> 
    pivot_longer(cols = everything()) |> 
    drop_na(value) |> 
    count(value) |> 
    mutate(pct = n/n_respondents,
        pct_label = paste0(round(pct * 100, 0), "%")) |> 
    mutate(value = fct_reorder(value, pct, .desc = TRUE))

simple_bar_chart(df, value) +
    labs(
        title = "From a singer's perspective, what changes would most improve the sustainability of professional choral singing in Manitoba?"
    )

#
#
#
#| title: "Improvements by age"

df <- singer_data_full |> 
    select(age_group_collapsed, starts_with("improve")) |> 
    mutate(improve_other = case_when(
        !is.na(improve_other) ~ "Other",
        TRUE ~ NA)) |> 
    pivot_longer(cols = -age_group_collapsed) |> 
    drop_na(value) |> 
    group_by(age_group_collapsed) |> 
    count(value) |> 
    mutate(denom = if_else(
        age_group_collapsed == "Under 45", n_under45, n_over45
    )) |> 
    mutate(pct = n/denom,
        pct_label = paste0(round(pct * 100, 0), "%")) |> 
    mutate(value = fct_reorder(value, pct, .desc = TRUE))

grouped_bar_chart(df, value, age_group_collapsed) +
    labs(
        title = "From a singer's perspective, what changes would most improve the sustainability of professional choral singing in Manitoba?",
        subtitle = "By age group"
    )
#
#
#
#| title: "Improvement by years"

df <- singer_data_full |> 
    select(time_performing_collapsed, starts_with("improve")) |> 
    mutate(improve_other = case_when(
        !is.na(improve_other) ~ "Other",
        TRUE ~ NA)) |> 
    pivot_longer(cols = -time_performing_collapsed) |> 
    drop_na(value) |> 
    group_by(time_performing_collapsed) |> 
    count(value) |> 
    mutate(denom = if_else(
        time_performing_collapsed == "Less than 10 yrs", n_under_ten, n_over_ten
    )) |> 
    mutate(pct = n/denom,
        pct_label = paste0(round(pct * 100, 0), "%")) |> 
    mutate(value = fct_reorder(value, pct, .desc = TRUE))

grouped_bar_chart(df, value, time_performing_collapsed) +
    labs(
        title = "From a singer's perspective, what changes would most improve the sustainability of professional choral singing in Manitoba?",
        subtitle = "By number of years performing"
    )

#
#
#
#| title: "Singers fundraising"

df <- singer_data_full |> 
    count(singers_support_choirs) |> 
    drop_na() |> 
    calc_percent()

simple_bar_chart(df, singers_support_choirs) +
    labs(
        title = "How reasonable is it to expect singers to support the marketing and fundraising efforts of the choirs they sing in?"
    )
#
#
#
#| title: "Singers fundraising by age"

df <- singer_data_full |> 
    group_by(age_group_collapsed) |> 
    count(singers_support_choirs) |> 
    drop_na() |> 
    calc_percent()

grouped_bar_chart(df, singers_support_choirs, age_group_collapsed) +
    labs(
        title = "How reasonable is it to expect singers to support the marketing and fundraising efforts of the choirs they sing in?",
        subtitle = "By age group"
    )
#
#
#
#| title: "Singers fundraising by years"

df <- singer_data_full |> 
    group_by(time_performing_collapsed) |> 
    count(singers_support_choirs) |> 
    drop_na() |> 
    calc_percent()

grouped_bar_chart(df, singers_support_choirs, time_performing_collapsed) +
    labs(
        title = "How reasonable is it to expect singers to support the marketing and fundraising efforts of the choirs they sing in?",
        subtitle = "By number of years performing"
    )

#
#
#
#
#
#
#
#
#
