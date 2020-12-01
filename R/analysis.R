library(doParallel)
library(parallel)
library(foreach)
library(readr)
library(dplyr)
library(ggplot2)

sim_elast_of_sub <- function(scorescsv, calcyear = 2014, elast_of_sub = c(0, 1), niter = 10000, goal_weights = NULL){

  scorescsv <- filter(scorescsv, region_id %in% 1:42)
  goalscores <- filter(scorescsv, goal != "Index", dimension %in% c("status", "future"))
  if("year" %in% names(goalscores)){
    goalscores <- filter(goalscores, year == calcyear)
  }
  ## regions with one or more goal scores of zero will obtain an index value of zero
  ## to prevent this collapse to zero (looses some useful information), can set zero scores to small value
  ## no other scores have such small scores, so doesnt confuse signal (?)
  goalscores <- mutate(goalscores, score = ifelse(score == 0, 0.1, score))

  goalscores <- goalscores %>%
    left_join(goal_weights, by = "goal") %>%
    mutate(weight = ifelse(is.na(score), NA, weight)) %>%
    group_by(dimension, region_id) %>%
    mutate(rgn_weight = weight/sum(weight, na.rm = TRUE)) %>%
    ungroup()

  set.seed(10)
  nsample_uniform <- runif(niter, min = min(elast_of_sub), max = max(elast_of_sub))
  ## issues with calculations when sigma gets very small...
  ## remove any sigma values less than 0.0051
  nsample_uniform <- nsample_uniform[nsample_uniform >= 0.0051]
  nsample_uniform <- c(modelrun = nsample_uniform)

  cl <- makeCluster(3)
  registerDoParallel(cl)

  t0 <- Sys.time()
  simulated_scores <- foreach(i = 1:length(nsample_uniform), .packages= c("tidyverse"), .combine = rbind) %dopar% {

    s <- nsample_uniform[[i]]

    simulated_scores <- goalscores %>%
      group_by(dimension, region_id) %>%
      mutate(weightedscores = rgn_weight*score^((s-1)/s)) %>%
      summarize(score = sum(weightedscores, na.rm = TRUE)^(s/(s-1))) %>%
      group_by(region_id) %>%
      summarize(score = mean(score), dimension = "score") %>%
      arrange(desc(score)) %>%
      mutate(rank = rank(100-score)) %>%
      ungroup() %>%
      mutate(elasticity = paste("sigma", round(s, 7)), modelrun = names(nsample_uniform[i]))
  }
  Sys.time() - t0
  stopCluster(cl)
  closeAllConnections()

  ## result with original (arithmetic) scores included
  scores_arithmetic <- scorescsv %>%
    filter(goal == "Index", dimension %in% c("score")) %>%
    arrange(desc(score)) %>%
    mutate(rank = rank(100-score)) %>%
    ungroup() %>%
    mutate(elasticity = "infinite", modelrun = "modelrun0") %>%
    select(-goal)

  ## quick plots to check results
  plotdf <- simulated_scores %>%
    mutate(region = as.factor(paste("region", region_id))) %>%
    left_join(
      read_csv("https://raw.githubusercontent.com/OHI-Science/bhi-prep/master/supplement/lookup_tabs/rgns_complete.csv") %>%
        select(region_id, region_name, region_order),
      by = "region_id"
    )
  plotdf$region_name <- factor(
    plotdf$region_name,
    levels = arrange(distinct(plotdf, region_name, region_order), region_order)$region_name
  )
  scoreplot <- ggplot(plotdf) +
    geom_point(aes(region_name, score), size = 1.2, alpha = 0.01, color = "steelblue") +
    coord_flip() +
    labs(
      x = NULL,
      y = expression(paste("\nAggregated scores, with limited substitution posibilities (", sigma %~% U, "(0, 1))"))
    )
  rankplot <- ggplot(plotdf, aes(region_name, rank)) +
    geom_boxplot(outlier.size = 0.2, size = 0.2, alpha = 0.2, fill = "steelblue", color = "royalblue") +
    coord_flip() +
    labs(x = NULL, y =  NULL)

  ## ranking comparison plot
  ## comparing ranks from arithmetic calculation, and range of possible ranks w sigma ~ U(0, 1)
  rank_mode_fun <- function(x){
    ux <- unique(x)
    tab <- tabulate(match(x, ux))
    data.frame(rank_mode = ux[tab == max(tab)])
  }
  rank_modes <- simulated_scores %>%
    group_by(region_id) %>%
    group_modify(~ rank_mode_fun(.x$rank)) %>%
    ungroup()

  plotdf <- simulated_scores %>%
    group_by(region_id) %>%
    summarise(minrank = min(rank), maxrank = max(rank)) %>%
    ungroup() %>%
    left_join(rank_modes, by = "region_id") %>%
    left_join(select(scores_arithmetic, region_id, rank_arithmetic = rank), by = "region_id") %>%
    left_join(
      read_csv("https://raw.githubusercontent.com/OHI-Science/bhi-prep/master/supplement/lookup_tabs/rgns_complete.csv") %>%
        select(region_id, region_name),
      by = "region_id"
    ) %>%
    mutate(region_name = stringr::str_replace(region_name, ", ", ",\n"))

  compareplot <- ggplot(plotdf) +
    geom_abline(color = "lightgrey") +
    geom_errorbar(
      aes(x = rank_arithmetic, ymin = minrank, ymax = maxrank),
      width = 0.1, color = "lightsteelblue"
    ) +
    geom_point(aes(x = rank_arithmetic, y = rank_mode)) +
    labs(
      x = expression(paste("\nRanking with unlimited substitution posibilities (", sigma, " = ", infinity, ")")),
      y = expression(paste("\nRanking with limited substitution posibilities (", sigma %~% U, "(0, 1))")),
      title = "Position of point along vertical axis represents the rank mode \nand the bar indicates min-max range"
    ) +
    ggrepel::geom_text_repel(
      aes(x = rank_arithmetic, y = rank_mode, label = region_name),
      size = 3.5, color = "dimgrey", segment.colour = NA,
      hjust = 0, vjust = 0
    )




  result <- rbind(scores_arithmetic, simulated_scores)

  return(result)
}
