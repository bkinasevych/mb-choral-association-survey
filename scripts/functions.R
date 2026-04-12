simple_bar_chart <- function(
  data,
  variable
) {

data |> 
  ggplot(aes(
    x = pct,
    y = fct_rev( {{variable}} ))) +
    geom_col(fill = teal) +
    geom_text(aes(
      label = pct_label),
      hjust = -0.2) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(
      x = "",
      y = ""
    ) +
    plot_theme()

}

grouped_bar_chart <- function(
  data,
  variable,
  grouping
){

data |> 
  ggplot(
    aes(
      x = pct,
      y = fct_rev( {{variable}} ),
      fill = {{grouping}})) +
  geom_col(
    colour = "white",
    position = "dodge",
    width = 0.7) +
  geom_text(
    aes(
      label = pct_label
    ),
      position = position_dodge(width = 0.7),
      hjust = -0.2) +
  guides(
    fill = guide_legend(reverse = TRUE)) +
  scale_fill_manual(
    values = c(teal, purple)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    x = "",
    y = "",
    fill = "",
    ) +
  plot_theme()

}