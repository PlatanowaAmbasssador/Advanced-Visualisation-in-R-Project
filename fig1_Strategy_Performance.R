library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(scales)
library(patchwork)
library(zoo)

setwd("/Users/kamilkashif/Documents/University/Masters - DSiBA/Advanced Visualisation in R/Project")

df_ls <- read_csv("Data/df_EC_LS_MAIN.csv", show_col_types = FALSE) %>%
  mutate(
    Date = as.Date(Date),
    strategy = strategy * 1000,
    buy_n_hold = buy_n_hold * 1000
  )

pos_labs <- c(`-1` = "Short Entry", `0` = "Flat", `1` = "Long Entry")

df_perf <- df_ls %>%
  arrange(Date) %>%
  mutate(
    position_cat = factor(position, levels = c(-1, 0, 1), labels = pos_labs),
    cummax_strategy = cummax(strategy),
    drawdown = strategy / cummax_strategy - 1
  )

# Create regime periods for shading
regimes <- df_perf %>%
  mutate(run_id = cumsum(position_cat != lag(position_cat, default = first(position_cat)))) %>%
  group_by(run_id, position_cat) %>%
  summarize(start = first(Date), end = last(Date), .groups = "drop")

max_dd_date <- df_perf$Date[which.min(df_perf$drawdown)]
max_dd_value <- min(df_perf$drawdown, na.rm = TRUE)

# Reshape for plotting both series together
perf_long <- df_perf %>%
  select(Date, strategy, buy_n_hold) %>%
  pivot_longer(-Date, names_to = "series", values_to = "value") %>%
  mutate(series = recode(series, strategy = "Strategy", buy_n_hold = "Buy & Hold"))

max_strategy <- max(df_perf$strategy, na.rm = TRUE)
end_of_year <- as.Date("2023-12-31")
pos_palette <- c("Short Entry" = "#C0392B", "Flat" = "#7F8C8D", "Long Entry" = "#1E8449")
line_palette <- c("Strategy" = "#1B4F72", "Buy & Hold" = "#7D3C98")

p_perf <- ggplot() +
  geom_rect(
    data = regimes,
    aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = position_cat),
    alpha = 0.12,
    inherit.aes = FALSE
  ) +
  geom_line(
    data = perf_long,
    aes(Date, value, color = series),
    linewidth = 0.9
  ) +
  geom_smooth(
    data = perf_long %>% filter(series == "Strategy"),
    aes(Date, value),
    method = "loess",
    se = FALSE,
    linewidth = 0.7,
    color = "#0E6251"
  ) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray45") +
  geom_hline(yintercept = max_strategy, linetype = "dotted", color = "#1B4F72") +
  geom_vline(xintercept = max_dd_date, linetype = "dotted", color = "gray40") +
  scale_color_manual(values = line_palette, breaks = c("Buy & Hold", "Strategy")) +
  scale_fill_manual(values = pos_palette, breaks = c("Short Entry", "Long Entry", "Flat")) +
  scale_y_continuous(
    limits = c(0, 8000),
    labels = dollar_format(accuracy = 1),
    expand = expansion(mult = c(0.06, 0.02))
  ) +
  scale_x_date(
    limits = c(as.Date("2005-01-01"), end_of_year),
    breaks = seq(from = as.Date("2005-01-01"), to = end_of_year, by = "2 years"),
    date_labels = "%Y",
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title = "Performance Divergence Across Market Regimes",
    subtitle = paste0("Final Strategy Equity: ", dollar_format(accuracy = 1)(max_strategy)),
    x = NULL, y = "Cumulative Equity ($)",
    color = NULL, fill = NULL
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    legend.box = "horizontal",
    axis.title.y = element_text(margin = margin(r = 6)),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.margin = margin(5.5, 5.5, 5.5, 12)
  ) +
  guides(
    color = guide_legend(order = 1),
    fill = guide_legend(order = 2, title = NULL)
  )

p_dd <- ggplot(df_perf, aes(Date)) +
  geom_rect(
    data = regimes,
    aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = position_cat),
    alpha = 0.12,
    inherit.aes = FALSE
  ) +
  geom_area(aes(y = drawdown), fill = "#2E86C1", alpha = 0.25) +
  geom_hline(yintercept = max_dd_value, linetype = "dashed", color = "gray40") +
  geom_vline(xintercept = max_dd_date, linetype = "dotted", color = "gray40") +
  scale_fill_manual(values = pos_palette) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_date(
    limits = c(as.Date("2005-01-01"), end_of_year),
    breaks = seq(from = as.Date("2005-01-01"), to = end_of_year, by = "2 years"),
    date_labels = "%Y",
    expand = expansion(mult = c(0, 0))
  ) +
  labs(x = "Date", y = "Drawdown") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

fig1 <- p_perf / p_dd + plot_layout(heights = c(2, 1))

ggsave("fig1.png", fig1, width = 10, height = 7, dpi = 300)