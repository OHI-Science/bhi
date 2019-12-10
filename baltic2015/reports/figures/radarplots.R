library(here)
library(ggplot2)
library(fmsb)
library(circlize)
library(magick)
library(dplyr)

scores <- readr::read_csv(here("baltic2015", "scores.csv"))

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
  order = as.factor(24:1)) %>%
  dplyr::mutate(x = sum(width)-(cumsum(width)-width), x_end = sum(width)-(cumsum(width))) %>%
  tidyr::gather("start_end", "range", -labels, -width, -order) %>%
  dplyr::select(labels, range, order)

## make two versions, one with labels, one with none
for(i in c("goals", "index")){

  temp_labels <- file.path(here("baltic2015", "reports", "figures"), sprintf("radarplotlabs_%s.png", i))
  cols <- c(goals = "transparent", index = "slategrey")

  png(temp_labels, width = 1100, height = 1100, bg = "transparent")
  circos.clear()
  circos.par(
    "track.height" = 0.64,
    cell.padding = c(0.02, 0, 0.02, 0),
    "clock.wise" = FALSE,
    start.degree = 94.27,
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
        col = cols[[i]],
        facing = "clockwise",
        niceFacing = TRUE
      )
    },
    bg.col = NA,
    bg.border = 1
  )
  dev.off()
  # image_read(temp_labels) %>% image_scale(920)
}


for(g in unique(scores$goal)){ # setdiff(unique(scores$goal), "Index")

  ## with BHI regions
  plot_df <- scores %>%
    filter(region_id %in% 1:42, dimension == "score", goal == g) %>%
    mutate(
      scoreNA = ifelse(is.na(score)|is.nan(score), "wNA", "woNA"),
      score = ifelse(is.na(score)|is.nan(score), 2, score)
    )
  ## reorder regions
  ## for plotting clockwise from north around baltic
  plot_df$region_id = factor(
    plot_df$region_id,
    levels(as.factor(plot_df$region_id))[c(
      42,40,38,36,32:34,30,31,28,27,25,24,20,
      23:21,19,18,17,16,13,10:7,4:1,5,6,
      11,12,15,14,26,29,35,37,39,41
    )]
  )
  plot_df <- plot_df %>%
    select(region_id, score, scoreNA) %>%
    arrange(desc(region_id))

  ## want to add points where have NA values...
  if(any(plot_df$scoreNA == "wNA")){
    plot_df <- plot_df %>%
      tidyr::pivot_wider(names_from = scoreNA, values_from = score) %>%
      tidyr::pivot_longer(names_to = "scoreNA", values_to = "score", -region_id)
  }
  ## hack for NAs: add points for all but move under center when don't want them shown
  plot_df <- plot_df %>%
    mutate(
      score = ifelse(
        is.na(score) & scoreNA == "wNA", -10, ifelse(
          is.na(score) & scoreNA == "woNA", 0, score
        )),
      region_id = paste("R", as.character(region_id), sep =  "-")
    ) %>%
    tidyr::pivot_wider(names_from = region_id, values_from = score) %>%
    arrange(desc(scoreNA)) %>%
    select(-scoreNA) %>%
    data.frame()
  ## include another row with -50 y-value, to scale y axiss
  plot_df <- rbind(rep(100, 18), rep(-50, 18), plot_df, rep(0, 18))

  ## create the radar plots
  temp_plot <- here::here("baltic2015", "reports", "figures", sprintf("%s_radar.png", stringr::str_to_lower(g)))
  png(temp_plot, width = 1100, height = 1100)
    if(nrow(plot_df) == 4){
    fmsb::radarchart(
      plot_df,
      pty = c(32, 32),
      cglwd = 0.5,
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
      pty = c(32, 19, 32),
      cglwd = 0.5,
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

  temp_labels <- ifelse(
    g == "Index",
    file.path(here("baltic2015", "reports", "figures"), "radarplotlabs_index.png"),
    file.path(here("baltic2015", "reports", "figures"), "radarplotlabs_goals.png")
  )

  ## center number for plots
  balticscore <- filter(scores, goal == g, dimension == "score", region_id == 0)$score
  img_text <- temp_labels %>%
    image_read() %>%
    image_scale(841)

  plotobj <- image_read(temp_plot) %>%
    image_crop(geometry_area(820, 820, 144, 122))

  magick::image_composite(plotobj, img_text) %>%
    image_annotate(
      formatC(round(balticscore, 1), format = "f", digits = 1),
      size = 85,
      color = "#1c354AB3",
      location = "+338+378"
    ) %>%
    image_write(path = temp_plot, format = "png")

  ## create a version also without the subbasin dividing lines
  if(g != "Index"){
    plotobj %>%
      image_annotate(
        formatC(round(balticscore, 1), format = "f", digits = 1),
        size = 85,
        color = "#1c354AB3",
        location = "+338+378"
      ) %>%
      image_write(path = stringr::str_replace(temp_plot, "radar", "no_basins"), format = "png")
  }
}
