# Creating Reports

### Introduction

### Our Audiences

### Why use RMarkdown?

### Recycled Text & Canned Content

### Standardized Figures & Tables

**Flower Plots**

Flower plots are the primary way we visually communicate OHI assessement results. These plots can be created using the `make_flower_plot` function written in `R/visuzliation.R`. Details about the function are written as roxygen documentation accompanying the function and also recorded in the `R` folder Readme. An example illustrating the function's usage:

```{r}
rgn_scores <- readr::read_csv(file.path(dir_baltic, "scores.csv")) %>% 
                      dplyr::filter(region_id == 1)
flower_plot <- make_flower_plot(rgn_scores, plot_year = 2014, dim = "score", 
                      color_pal = NA, color_by = "goal", gradient = TRUE, 
                      legend_tab = TRUE, update_legend = TRUE, 
                      labels = "curved", center_val = TRUE, critical_value = 5, save = NA)
```

Flower plots can be generated with or without an accompanying table [created with `formattable`](https://www.littlemissdata.com/blog/prettytables), which acts as a legend. This table includes columns for goal or subgoal name columns, color-key, and score value. The specific chunck of code creating this table is the second half of the `LABELING AND LEGENDS` section of the function's code. A static version of this legend/table is saved by [screenshotting the table-as-htmlwidget](https://github.com/renkun-ken/formattable/issues/26). This requires htmltools and webshot packages, and requires phantomjs to be intalled which can be done with `webshot::install_phantomjs()`.


**Trend Bar Plots**

Trend bar plots created via the `make_trends_barplot` function illustrate the magnitude and direction of the 'trend' dimension of the scores. Positive trends mean we expect to see higher scores (improvement) associated with those goals, in the near future.

```{r}
rgn_scores <- readr::read_csv(file.path(dir_baltic, "scores.csv")) %>% 
                      dplyr::filter(region_id == 1)
thm <- apply_bhi_theme("trends_barplot") # another R\visualization.R function
pal <- c(thm$bhi_palettes$blues, thm$bhi_palettes$reds)

trend_plot <- make_trends_barplot(rgn_scores, color_pal = pal, plot_year = 2014)
```


### Additional References
