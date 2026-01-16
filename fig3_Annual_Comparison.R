library(ggplot2)
library(dplyr)
library(readr)
library(scales)
library(treemapify)

setwd("/Users/kamilkashif/Documents/University/Masters - DSiBA/Advanced Visualisation in R/Project")

df_ls <- read_csv("Data/df_EC_LS_MAIN.csv", show_col_types = FALSE) %>%
  mutate(Date = as.Date(Date))

df_perf <- df_ls %>%
  filter(!is.na(strat_return), !is.na(Date)) %>%
  mutate(Year = as.integer(format(Date, "%Y")))

# Calculate annual compounded returns
annual_returns <- df_perf %>%
  group_by(Year) %>%
  summarize(
    ann_ret = prod(1 + strat_return, na.rm = TRUE) - 1,
    .groups = "drop"
  ) %>%
  arrange(Year) %>%
  mutate(
    parent_group = if_else(ann_ret >= 0, "Positive Returns", "Negative Returns"),
    area = sqrt(abs(ann_ret)),
    abs_ret = abs(ann_ret),
    label_color = if_else(abs_ret > quantile(abs_ret, 0.5, na.rm = TRUE), "white", "gray30"),
    text_size = 6 + (area / max(area, na.rm = TRUE)) * 8,
    year_label = as.character(Year),
    return_label = if_else(
      abs_ret >= 0.02,
      paste0("(", if_else(ann_ret >= 0, "+", ""), percent(ann_ret, accuracy = 0.1), ")"),
      ""
    ),
    label = if_else(
      return_label != "",
      paste0(year_label, "\n", return_label),
      year_label
    )
  )

# Set up color scale to be symmetric around zero
ret_range <- range(annual_returns$ann_ret, na.rm = TRUE)
max_abs <- max(abs(ret_range))
legend_breaks <- pretty(c(-max_abs, max_abs), n = 5)
if (!0 %in% legend_breaks) {
  legend_breaks <- sort(c(legend_breaks, 0))
}

annual_returns <- annual_returns %>% 
  arrange(parent_group, Year)

fig3 <- ggplot(annual_returns, aes(area = area, fill = ann_ret, subgroup = parent_group, label = label)) +
  geom_treemap_subgroup_border(color = "white", size = 3) +
  geom_treemap(color = "white", size = 1.5) +
  geom_treemap_text(
    aes(color = label_color, size = text_size),
    place = "centre",
    fontface = "bold",
    min.size = 4,
    padding.x = grid::unit(3, "mm"),
    padding.y = grid::unit(3, "mm"),
    reflow = TRUE
  ) +
  scale_color_identity(guide = "none") +
  scale_size_identity(guide = "none") +
  scale_fill_gradient2(
    low = "#DC143C",
    mid = "#7F8C8D",
    high = "#228B22",
    midpoint = 0,
    limits = c(-max_abs, max_abs),
    breaks = legend_breaks,
    labels = percent_format(accuracy = 0.1),
    guide = "none"
  ) +
  labs(title = "Annual Contributions to Strategy Performance") +
  theme_void(base_size = 12) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5, margin = margin(b = 8)),
    plot.margin = margin(20, 20, 20, 20)
  )

ggsave("fig3.png", fig3, width = 12, height = 8, dpi = 300)