library(here)
library(readr)
library(dplyr)
library(ggplot2)

scores <- read_csv(here("baltic2015", "scores.csv"))
scores <- read_csv("/Users/eleanorecampbell/Desktop/scoresJan28.csv") %>%
  tidyr::pivot_longer(
    cols = c(status, trend, future, pressures, resilience, score),
    names_to = "dimension"
  ) %>%
  select(-Name) %>%
  rename(score = value)

goals_pal <-  tibble::tibble(
  goal = c(
    "MAR","FIS","FP","CW","CON","EUT","TRA",
    "SP","LSP","ICO","LE","ECO","LIV",
    "AO","TR","CS","NP", "BD"
  ),
  color = c(
    "#549dad","#4ead9d","#53b3ac","#89b181","#60a777","#7ead6d","#9aad7e",
    "#97a4ba","#9fb3d4","#7c96b4","#9a97ba","#7d7ca3","#9990b4",
    "#ddca36","#b6859a","#d0a6a1","#ccb4be","#88b1a6"
  )
)

plotdf <- scores %>%
  filter(dimension %in% c("future", "status"), goal != "Index") %>%
  tidyr::pivot_wider(names_from = dimension, values_from = score) %>%
  mutate(future_minus_current = future - status) %>%
  mutate(future_minus_current = ifelse(is.nan(future_minus_current), NA, future_minus_current)) %>%
  left_join(read_csv(here("baltic2015", "conf", "goals.csv")) %>% select(goal, name))

plotdf$goal <- factor(
  plotdf$goal,
  c(
    "MAR","FIS","FP","CW","CON","EUT","TRA",
    "SP","LSP","ICO","LE","ECO","LIV",
    "AO","TR","CS","NP", "BD"
  )
)
plotdf$name <- factor(
  plotdf$name,
  arrange(distinct(select(plotdf, goal, name)), goal)$name
)

rangeplotdf <- left_join(
  plotdf %>%
    filter(region_id %in% 501:517) %>%
    group_by(goal) %>%
    summarize(
      minscore = min(future_minus_current, na.rm = TRUE),
      maxscore = max(future_minus_current, na.rm = TRUE)
    ),
  plotdf %>%
    filter(region_id == 0) %>%
    select(status, future_minus_current, goal, name),
  by = "goal"
) %>% mutate(name_label = sprintf("%s (%s)", name, goal))

## rangeplot of status vs future minus status, with ranges
rangeplot <- ggplot(rangeplotdf) +
  geom_point(aes(status, future_minus_current, color = goal)) +
  geom_hline(yintercept = 0) +
  geom_errorbar(
    aes(x = status, ymin = minscore, ymax = maxscore, color = goal),
    width = 0.8, alpha = 0.6
  ) +
  ggrepel::geom_text_repel(aes(status, future_minus_current, label = goal), segment.alpha = 0) +
  labs(x = "\nStatus", y = "Likely Future minus Status\n") +
  scale_color_manual(
    labels = unique(rangeplotdf$name_label),
    values = goals_pal$color
  ) +
  scale_y_continuous(limits = c(-15, 42), breaks = seq(-15, 42, 5)) +
  theme_bw() +
  guides(color = guide_legend(
    title = "Goal or Sub-goal", title.position = "top"
  )) +
  theme(legend.background = element_rect(fill = "ghostwhite"))

renaming <- rangeplotdf %>%
  mutate(rename = sprintf("%s (%s)", name, round(status))) %>%
  select(name, rename)

## violin plot with distributionss of projected changes by goal
violinplot <- ggplot(mutate(rangeplotdf, name = sprintf("%s (%s)", name, round(status)))) +
  geom_violin(
    data = plotdf %>%
      filter(region_id %in% 501:517) %>%
      left_join(renaming, by = "name") %>%
      mutate(name = rename),
    aes(x = name, y = future_minus_current, fill = goal),
    trim = FALSE,
    color = NA, alpha = 0.4, width = 1.8, show.legend = FALSE
  ) +
  geom_point(
    aes(name, future_minus_current, color = goal),
    size = 1.7, show.legend = FALSE
  ) +
  geom_errorbar(
    aes(x = name, ymin = minscore, ymax = maxscore, color = goal),
    width = 0.4, size = 0.4, alpha = 0.8, show.legend = FALSE
  ) +
  geom_hline(yintercept = 0) +
  # ggrepel::geom_text_repel(
  #   data = rangeplotdf %>%
  #     mutate(status = sprintf(
  #       "(Status: %s)",
  #       round(status)
  #     )),
  #   aes(name, future_minus_current, label = status, color = goal),
  #   # family = "Times",
  #   segment.alpha = 0, size = 5, alpha = 0.9, show.legend = FALSE
  # ) +
  labs(x = NULL, y = "\nLikely Future minus Status\n(Current status shown in parentheses)") +
  scale_y_continuous(limits = c(-15, 42), breaks = seq(-15, 42, 5)) +
  scale_fill_manual(values = goals_pal$color) +
  scale_color_manual(values = goals_pal$color) +
  coord_flip() +
  theme_bw() +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 15)
    # axis.text = element_text(family = "Times", size = 12),
    # axis.title = element_text(family = "Times", size = 14)
  )
## saved with approximate dimensions 1220 x 860
