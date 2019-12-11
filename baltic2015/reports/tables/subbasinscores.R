library(here)
scores <- readr::read_csv(here("baltic2015", "scores.csv"))

basin_scores_wide <- scores %>%
  filter(region_id %in% 1:42) %>%
  left_join(
    readr::read_csv(here("baltic2015", "regions_lookup_complete_wide.csv")) %>%
      select(region_id, area_km2_rgn, subbasin_name, subbasin_id),
    by =  "region_id"
  ) %>%
  group_by(goal, dimension, subbasin_name, subbasin_id) %>%
  summarize(basinmean = weighted.mean(score, area_km2_rgn, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(score = ifelse(is.nan(basinmean), NA, round(basinmean, 2))) %>%
  select(goal, dimension, region_id = subbasin_id,  score) %>%
  spread(dimension, score)


chk <- scores %>%
  filter(region_id %in% 501:517, dimension == "score") %>%
  rename(score0 = score) %>%
  select(-dimension) %>%
  left_join(
    basin_scores_wide %>%
      mutate(score = round(score, 2)),
    by = c("goal", "region_id")
  ) %>%
  mutate(diffs = score-score0)
# hist(chk$diffs)

write_csv(basin_scores_wide, here("baltic2015", "reports", "tables", "subbasin_dim_scores.csv"))
