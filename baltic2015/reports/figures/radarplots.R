library(here)
library(ggplot2)
library(fmsb)
library(circlize)
library(magick)

scores <- read_csv(here("baltic2015", "scores.csv"))


temp_labels <- file.path(here("baltic2015", "reports", "figures"), "radarplotlabs.png")
circ_df <- data.frame(
  labels = c(
    "Bothnian Bay", "The Quark (East)", "Bothnian Sea (East)", "Åland Sea (East)",
    "Gulf of Finland", "Northern Baltic Proper (East)", "Gulf of Riga",
    "Eastern Gotland Basin (North)","Eastern Gotland Basin (South)","Gdansk Bay",
    "Bornholm Basin (South)","Arkona Basin (South)",
    "Bay of Mecklenburg","Kiel Bay","Great Belt","Kattegat","The Sound",
    "Arkona Basin (North)","Bornholm Basin (North)", "Western Gotland Basin","Northern Baltic Proper (West)",
    "Åland Sea (West)","Bothnian Sea (West)","The Quark (West)"
  ),
  width = c(2, 1, 1, 1, 3, 2, 2, 3, 3, 2, 2, 1, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1),
  order = as.factor(1:24)) %>%
  dplyr::mutate(x = sum(width)-(cumsum(width)-width), x_end = sum(width)-(cumsum(width))) %>%
  tidyr::gather("start_end", "range", -labels, -width, -order) %>%
  dplyr::select(labels, range, order) %>%
  arrange(order)

png(temp_labels, width = 1200, height = 1200, bg = "transparent")
circos.clear()
circos.par(
  "track.height" = 0.64,
  cell.padding = c(0.02, 0, 0.02, 0),
  "clock.wise" = FALSE,
  start.degree = 94.3,
  gap.degree = 0.2
)
circos.initialize(
  factors = circ_df$order,
  x = circ_df$range
)
circos.track(
  factors = circ_df$order,
  y = circ_df$range,
  panel.fun = function(x, y){
    circos.text(
      CELL_META$xcenter,
      CELL_META$ycenter,
      circ_df$labels[circ_df$order == as.numeric(CELL_META$sector.index)][1],
      cex = 2,
      col = "slategrey",
      facing = "clockwise",
      niceFacing = TRUE
    )
  },
  bg.col = NA,
  bg.border = 1
)
dev.off()
img_text <- image_read(temp_labels) %>% image_scale(920)

for(g in setdiff(unique(scores$goal), "Index")){

  ## with Subbasins
  # plot_df <- scores %>%
  #   filter(region_id %in% 501:517, dimension == "score", goal == g) %>%
  #   mutate(score = ifelse(is.nan(score), 0, score), subbasin = as.factor(paste0("B", region_id))) %>%
  #   select(subbasin, score)
  # plot_df$subbasin <- factor(
  #   plot_df$subbasin,
  #   levels(plot_df$subbasin)[rev(c(17,16,15,14,13,12,11,9,8,7,5,4,2,1,3,6,10))]
  # )
  # plot_df <-  plot_df %>%
  #   tidyr::spread(subbasin, score) %>%
  #   data.frame()

  ## with BHI regions instead
  plot_df <- scores %>%
    filter(region_id %in% 1:42, dimension == "score", goal == g) %>%
    mutate(
      scoreNA = ifelse(is.na(score)|is.nan(score), "wNA", "woNA"),
      score = ifelse(is.na(score)|is.nan(score), 2, score),
      rgn = as.factor(paste0("R", region_id))
    )
  plot_df$rgn_reorder = factor(
    plot_df$rgn,
    levels(plot_df$rgn)[rev(c(
      37,35,32,30,26,27,28,24,25,21,20,18,17,13,16,15,14,11,10,9,8,5,2,42:40,
      34,23,12,1,38,39,3,4,7,6,19,22,29,31,33,36
    ))]
  )
  plot_df <- plot_df %>%
    select(rgn_reorder, score, scoreNA)
  if(length(unique(plot_df$scoreNA)) == 2){
    plot_df <- plot_df %>%
      pivot_wider(names_from = scoreNA, values_from = score) %>%
      # mutate(wNA = ifelse(is.na(wNA), -10, wNA), woNA = ifelse(is.na(woNA), 0, woNA)) %>%
      # mutate(wNA = ifelse(is.na(wNA), -10, wNA)) %>%
      pivot_longer(names_to = "scoreNA", values_to = "score", -rgn_reorder)
  }
  plot_df <- plot_df %>%
    mutate(score = ifelse(
      is.na(score) & scoreNA == "wNA", -10, ifelse(
        is.na(score) & scoreNA == "woNA", 0, score
      ))) %>%
    pivot_wider(names_from = rgn_reorder, values_from = score) %>%
    arrange(desc(scoreNA)) %>%
    select(-scoreNA) %>%
    data.frame()
  plot_df <- rbind(rep(100, 18), rep(-50, 18), plot_df, rep(0, 18))

  temp_plot <- here::here("baltic2015", "reports", "figures", sprintf("%s_radar.png", str_to_lower(g)))
  png(temp_plot, width = 1200, height = 1200)
  if(nrow(plot_df) == 4){
    fmsb::radarchart(
      plot_df,
      pty = c(32, 32),
      cglwd = 0.2,
      plwd = 0.7,
      plty = 1,
      cglty = 1,
      seg = 50,
      vlcex = 0.6,
      vlabels = NA,
      pfcol = c("#9EBCDAB3", "white"),
      pcol = c("#1c3548FF", "maroon"),
      cglcol = "#1c354AB3"
    )
  } else if(nrow(plot_df) == 5){
    fmsb::radarchart(
      plot_df,
      pty = c(32, 9, 32),
      cglwd = 0.2,
      plwd = 0.7,
      plty = 1,
      cglty = 1,
      seg = 50,
      vlcex = 0.6,
      vlabels = NA,
      pfcol = c("#9EBCDAB3", "white", "white"),
      pcol = c("#1c3548FF", "maroon", "maroon"),
      cglcol = "#1c354AB3"
    )
  }
  dev.off()

  img_plot <- image_read(temp_plot) %>% image_crop(geometry_area(920, 920, 154, 132))
  magick::image_composite(img_plot, img_text)
  image_write(image_composite(img_plot, img_text), path = temp_plot, format = "png")
}
