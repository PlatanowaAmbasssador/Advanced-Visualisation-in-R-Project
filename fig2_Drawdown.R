library(ggplot2)
library(dplyr)
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

df_perf <- df_ls %>%
  arrange(Date) %>%
  mutate(
    cummax_strategy = cummax(strategy),
    drawdown = strategy / cummax_strategy - 1,
    vol_21d = zoo::rollapply(strat_return, width = 21, FUN = sd, fill = NA, align = "right") * sqrt(252)
  )

max_dd_date <- df_perf$Date[which.min(df_perf$drawdown)]
max_dd_value <- min(df_perf$drawdown, na.rm = TRUE)
# Zoom window around max drawdown
zoom_start <- max_dd_date - 365
zoom_end <- max_dd_date + 365

trough_window_start <- max_dd_date - 30
trough_window_end <- max_dd_date + 30

df_zoom <- df_perf %>%
  filter(Date >= zoom_start & Date <= zoom_end)

# Calculate rectangle bounds for trough highlighting
trough_data <- df_zoom %>%
  filter(Date >= trough_window_start & Date <= trough_window_end)
trough_center <- trough_data$strategy[trough_data$Date == max_dd_date][1]
trough_range <- (max(trough_data$strategy, na.rm = TRUE) - min(trough_data$strategy, na.rm = TRUE)) * 0.5
trough_y_min <- trough_center - trough_range * 0.3
trough_y_max <- trough_center + trough_range * 1.7

p_equity_trades <- ggplot(df_zoom, aes(Date, strategy)) +
  geom_rect(
    aes(xmin = trough_window_start, xmax = trough_window_end, ymin = trough_y_min, ymax = trough_y_max),
    fill = "#FFB6C1",
    alpha = 0.3,
    inherit.aes = FALSE
  ) +
  geom_line(color = "#1B4F72", linewidth = 0.85) +
  geom_vline(xintercept = max_dd_date, linetype = "dotted", color = "#C0392B", linewidth = 0.7) +
  annotate(
    "text",
    x = max_dd_date + 10,
    y = max(df_zoom$strategy, na.rm = TRUE),
    label = paste0("MD: ", percent(max_dd_value, accuracy = 0.1)),
    hjust = 0,
    vjust = -0.5,
    size = 3.2,
    color = "#C0392B",
    fontface = "bold"
  ) +
  scale_y_continuous(
    labels = dollar_format(accuracy = 1),
    expand = expansion(mult = c(0.05, 0.05))
  ) +
  scale_x_date(
    date_breaks = "3 months",
    date_labels = "%b %Y",
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  labs(
    title = "Zoom on the Worst Drawdown: Equity and Risk Context",
    x = NULL,
    y = "Cumulative Equity ($)"
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5, margin = margin(b = 10)),
    axis.title.y = element_text(margin = margin(r = 6)),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.margin = margin(b = -5, t = 5.5, l = 5.5, r = 5.5)
  )

p_vol <- ggplot(df_zoom, aes(Date, vol_21d)) +
  geom_line(color = "#D35400", linewidth = 0.75) +
  geom_area(fill = "#D35400", alpha = 0.15) +
  geom_vline(xintercept = max_dd_date, linetype = "dotted", color = "#C0392B", linewidth = 0.7) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    expand = expansion(mult = c(0.05, 0.05))
  ) +
  scale_x_date(
    date_breaks = "3 months",
    date_labels = "%b %Y",
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  labs(x = "Date", y = "Annualized Volatility") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    plot.margin = margin(t = -5, b = 5.5, l = 5.5, r = 5.5)
  )

fig2 <- p_equity_trades / p_vol + plot_layout(heights = c(1.3, 1))

ggsave("fig2.png", fig2, width = 10, height = 7, dpi = 300)