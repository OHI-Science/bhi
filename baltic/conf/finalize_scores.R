
## need region scores and region areas
# rgn_scores <- read_csv(here::here("baltic2015", "scores.csv"))
# areas <- read_csv(here::here("baltic2015", "regions_lookup_complete_wide.csv"))

cat(sprintf("Calculating scores for EEZ and SUBBASIN AREAS by area weighting...\n"))

## aggregate function
aggregate_scores <- function(rgn_scores, areas_df){
  aggregated_scores <- rgn_scores %>%
    dplyr::filter(region_id %in% 1:42) %>%
    dplyr::left_join(areas_df, by = "region_id") %>%
    dplyr::group_by(goal, dimension, areas_name, areas_id) %>%
    dplyr::summarise(score = weighted.mean(score, area_km2_rgn, na.rm = TRUE)) %>%
    ungroup() %>%
    select(goal, dimension, score, areas_id) %>%
    dplyr::mutate(score = ifelse(is.nan(score), NA, score))

}


## aggregate for eezs
areas_eez <- select(areas, region_id, area_km2_rgn, areas_name = eez_name, areas_id = eez_id)
scores_eez <- aggregate_scores(rgn_scores, areas_df = areas_eez)

## aggregate for subbasins
areas_subbasin <- select(areas, region_id, area_km2_rgn, areas_name = subbasin_name, areas_id = subbasin_id)
scores_subbasin <- aggregate_scores(rgn_scores, areas_df = areas_subbasin)

areas_baltic <- areas %>%
  select(region_id, area_km2_rgn) %>%
  mutate(areas_name = "Baltic Sea", areas_id = 0)
scores_baltic <- aggregate_scores(rgn_scores, areas_df = areas_baltic)


## create table with dimensions columns per subbasin, and full baltic
Basinscores_wBaltic <- dplyr::bind_rows(
  scores_subbasin %>%
    dplyr::mutate(score = round(score, 2)) %>%
    dplyr::left_join(
      areas %>%
        select(region_id = subbasin_id, Name = subbasin_name) %>%
        dplyr::distinct(),
      by = "region_id"
    ),
  scores_baltic %>%
    dplyr::mutate(score = round(score, 2)) %>%
    mutate(Name = "Baltic")
)
scores_subbasin_wide <- Basinscores_wBaltic %>%
  tidyr::pivot_wider(names_from = dimension, values_from = score) %>%
  dplyr::select(goal, Name, region_id, status, future, trend, resilience, pressures, score) %>%
  dplyr::arrange(goal, Name)


## combine scores with EEZ and SUBBASIN scores
scores <- rgn_scores %>%
  dplyr::filter(region_id %in% 1:42) %>%
  dplyr::bind_rows(scores_eez, scores_subbasin) %>%
  mutate(score = ifelse(is.nan(score), NA, score)) %>%
  mutate(score = round(score, 2)) %>%
  distinct()
